"""Exception and warning classes, and catalog message text (SPEC A-10, A-11).

Every message a user reads comes from ``remedies.json`` by key through
:func:`msg`; no raise or warn site types its own text (A-10).
"""

from __future__ import annotations

import functools
import json
from importlib.resources import files
from typing import Any


class HydroloomError(ValueError):
    """Base class for errors hydroloom raises on a condition a user can meet."""


class RepresentationError(HydroloomError):
    """The network's representation lacks the pattern or columns a function needs."""


class CRSError(HydroloomError):
    """The input has a geographic or missing coordinate reference system."""


class HydroloomWarning(UserWarning):
    """Warning class for every warning hydroloom issues (A-11)."""


@functools.cache
def _remedies() -> dict[str, dict[str, Any]]:
    resource = files("hydroloom.data").joinpath("remedies.json")
    data = json.loads(resource.read_text(encoding="utf-8"))
    catalog: dict[str, dict[str, Any]] = data["remedies"]

    return catalog


def msg(key: str, /, **fields: Any) -> str:
    """Return the message text for the catalog entry ``key``.

    The text is the entry's ``message_python`` where it has one, its
    ``message`` otherwise, with ``fields`` filled into its ``str.format``
    placeholders.

    Parameters
    ----------
    key : str
        A key of ``remedies.json``.
    **fields : Any
        A value for each placeholder the entry declares.

    Returns
    -------
    str
        The formatted message.

    Raises
    ------
    KeyError
        If ``key`` is not in the catalog, naming the key, or if a placeholder
        has no value, naming the key and the placeholder.

    Examples
    --------
    >>> msg("start_not_in_network")[:31]
    'Every start must be an id in th'
    """
    catalog = _remedies()
    if key not in catalog:
        raise KeyError(key)
    entry = catalog[key]
    template: str = entry.get("message_python") or entry["message"]

    try:
        return template.format(**fields)
    except KeyError as err:
        raise KeyError(f"{key}.{err.args[0]}") from None
