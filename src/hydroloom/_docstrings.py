"""Compose numpydoc docstrings from the R documentation (SPEC A-13).

roxygen is the one authored source for argument meanings. ``dev/docstrings.R``
writes ``docstrings.json`` from ``man/``; :func:`doc` reads it at import and
writes the decorated function's ``__doc__``. Every composed field passes
through :data:`SUBSTITUTIONS`, the R-phrasing table, in order.

The composed docstring holds, in numpydoc order: the summary from ``title``;
the extended summary from ``description`` then ``details``; Parameters from
``arguments``, restricted to and ordered by the Python signature; Returns from
``value``; a See Also entry for the R function; Notes holding each transferred
Rd section as a paragraph led by its title; and the Examples block of the
function's own hand-written ``__doc__``, the only part of that ``__doc__`` kept.
"""

from __future__ import annotations

import functools
import inspect
import json
import re
from collections.abc import Callable
from importlib.resources import files
from typing import Any, TypeVar

F = TypeVar("F", bound=Callable[..., Any])

# R phrasing (a regular expression) -> Python phrasing, applied in order to
# every composed field. `\A` anchors a pattern to the start of a field, where
# roxygen puts an argument's R type; the Python type goes in the numpydoc type
# slot instead. tests/python/test_docstrings.py counts the fires of each pair
# over the whole of docstrings.json; a pair that never fires is not kept.
SUBSTITUTIONS: tuple[tuple[str, str], ...] = (
    # The A-13 examples.
    (r"data\.frame network compatible with hydroloom_names", "a table"),
    (r"logical if TRUE", "if true"),
    (r"logical if `TRUE`", "if true"),
    # R type words opening an argument or value.
    (r"\Alogical\. ", ""),
    (r"\Alogical ", ""),
    (r"\Aboolean ", ""),
    (r"\Acharacter or numeric ", "text or number "),
    (r"\Acharacter or factor vector, or NULL\. ", ""),
    (r"\Acharacter or NULL\. ", ""),
    (r"\Acharacter\. ", ""),
    (r"\Acharacter ", ""),
    (r"\Anumeric\. ", ""),
    (r"\Anumeric ", ""),
    (r"\Ainteger ", ""),
    (r"\Aunits distance ", "distance "),
    (r"\Acrs\. ", ""),
    (r"\Adata\.frame\. ", ""),
    (
        r"\Avector of the same length as `nrow\(x\)` containing ",
        "one value per row of `x`, containing ",
    ),
    (r"\Avector ", "sequence "),
    # Spatial classes.
    (
        r"sfc_POINT simple feature geometry list of length `nrow\(indexes\)`",
        "point geometry column with one point per row of `indexes`",
    ),
    (r"sf sf data\.frame", "table with geometry"),
    (r"sf data\.frame or sfc geometry", "table with geometry or geometry column"),
    (r"sf data\.frame or sfc", "table with geometry or geometry column"),
    (r"sf data\.frame", "table with geometry"),
    (r"sf data\.table", "table with geometry"),
    (r"sf or sfc", "table with geometry or geometry column"),
    (r"an sf point geometry column", "a point geometry column"),
    (r"a sf object", "a table with geometry"),
    (r"\bsf geometry", "geometry"),
    (r"When sfc is provided", "When a geometry column is provided"),
    (r"the return value is sfc", "the return value is a geometry column"),
    (r"\bsfc geometry\b", "geometry column"),
    (r"\bsfc\b", "geometry column"),
    # Tables.
    (r"data,frame", "table"),
    (r"\bdataframe\b", "table"),
    (r"data\.frame", "table"),
    (
        r"or list as returned by make_index_ids",
        "or index ids as returned by make_index_ids",
    ),
    # Column types, in the vocabulary's type names.
    (r"\(character\)", "(text)"),
    (r"\(character:", "(text:"),
    (r"\(logical\)", "(flag)"),
    (r"a logical flag", "a flag"),
    (r"a logical value", "a flag"),
    (r"logical columns", "flag columns"),
    # Logical, missing, and null values. `arg = TRUE` code spans are left alone.
    (r"`TRUE`", "true"),
    (r"`FALSE`", "false"),
    (r"(?<!= )\bTRUE\b", "true"),
    (r"(?<!= )\bFALSE\b", "false"),
    (r"an `NA`", "a missing"),
    (r"`NA`", "missing"),
    (r"If missing or NULL", "If not given"),
    (r"When non-NULL", "When given"),
    (r"When NULL", "When not given"),
    (r"is NULL", "is not given"),
    (r"Default NULL", "Default none"),
    (r"or NULL", "or none"),
    (r"`Inf`", "infinity"),
    # R call syntax, and R functions with a differently named Python counterpart.
    (r"`unqiue\(x\$id\)`", "the unique `id` values of `x`"),
    (
        r"c\(UM, DM, UT, DD\) or equivalently c\(upmain, downmain, up, down\)",
        "UM, DM, UT, DD or equivalently upmain, downmain, up, down",
    ),
    (r"`hy\(\)`", "`as_hy()`"),
    (r"`hy_capabilities\(\)`", "`capabilities()`"),
    (r"`is\.hy\(\)`", "`is_hy()`"),
    (r"See hydroloom_names for", "See register_names for"),
)

_COMPILED = tuple((re.compile(pattern), repl) for pattern, repl in SUBSTITUTIONS)

# A-9: the first positional argument is `network` wherever R's `x` is a network.
_DEFAULT_R_NAMES = {"network": "x"}

_EXAMPLES = re.compile(r"^Examples\n-{8,}\n", re.MULTILINE)


def substitute(text: str) -> tuple[str, list[int]]:
    """Apply the substitution table to one field.

    Returns the substituted text and the number of fires of each pair, in
    table order.
    """
    counts = []
    for pattern, repl in _COMPILED:
        text, n = pattern.subn(repl, text)
        counts.append(n)

    return text, counts


@functools.cache
def r_docs() -> dict[str, dict[str, Any]]:
    """Read ``docstrings.json``, keyed by Rd alias."""
    resource = files("hydroloom.data").joinpath("docstrings.json")
    docs: dict[str, dict[str, Any]] = json.loads(resource.read_text(encoding="utf-8"))

    return docs


def _field(text: str, *, period: bool = False) -> str:
    # Substitute, then capitalize the first letter unless the first word is an
    # identifier; numpydoc wants summary, parameter, and return text to end
    # with a period.
    text = substitute(text)[0].strip()
    first = text.split(maxsplit=1)[0] if text else ""
    if text[:1].isalpha() and text[0].islower() and "_" not in first:
        text = text[0].upper() + text[1:]
    if period and text and text[-1] not in ".?!:":
        text += "."

    return text


def _indent(text: str) -> str:
    return "\n".join(f"    {line}" if line else "" for line in text.splitlines())


def _annotation(annotation: Any) -> str:
    if annotation is inspect.Parameter.empty:
        return ""
    if isinstance(annotation, str):
        return annotation

    return inspect.formatannotation(annotation)


def _parameter_line(param: inspect.Parameter) -> str:
    kind = _annotation(param.annotation)
    if param.default is not inspect.Parameter.empty:
        kind = (
            f"{kind}, default {param.default!r}"
            if kind
            else f"default {param.default!r}"
        )

    return f"{param.name} : {kind}" if kind else param.name


def _section(name: str, body: str) -> str:
    return f"{name}\n{'-' * len(name)}\n{body}"


def compose(
    alias: str, func: Callable[..., Any], r_names: dict[str, str] | None = None
) -> str:
    """Compose the numpydoc docstring for ``func`` from the Rd entry ``alias``.

    ``r_names`` maps a Python argument name to the R argument it documents
    where the two differ (A-9); ``network`` maps to ``x`` unless the R entry has
    its own ``network``. A Python argument with no R text raises ``KeyError``.
    """
    entry = r_docs()[alias]
    names = {**_DEFAULT_R_NAMES, **(r_names or {})}
    r_args = entry.get("arguments") or {}
    signature = inspect.signature(func)

    parts = [_field(entry["title"], period=True)]
    for key in ("description", "details"):
        if entry.get(key):
            parts.append(_field(entry[key]))

    params = []
    for param in signature.parameters.values():
        r_name = (
            param.name if param.name in r_args else names.get(param.name, param.name)
        )
        if r_name not in r_args:
            raise KeyError(f"{alias}.{param.name}")
        params.append(
            f"{_parameter_line(param)}\n{_indent(_field(r_args[r_name], period=True))}"
        )
    if params:
        parts.append(_section("Parameters", "\n".join(params)))

    if entry.get("value"):
        returns = _annotation(signature.return_annotation)
        parts.append(
            _section(
                "Returns", f"{returns}\n{_indent(_field(entry['value'], period=True))}"
            )
        )

    parts.append(
        _section(
            "See Also",
            f"{alias} : R function `hydroloom::{alias}()`, the source of this documentation.",
        )
    )

    notes = [
        f"{_field(title)}:\n\n{_field(text)}"
        for title, text in (entry.get("sections") or {}).items()
    ]
    if notes:
        parts.append(_section("Notes", "\n\n".join(notes)))

    own = inspect.cleandoc(func.__doc__ or "")
    match = _EXAMPLES.search(own)
    if match:
        parts.append(own[match.start() :].strip())

    return "\n" + "\n\n".join(parts) + "\n"


def doc(alias: str, /, **r_names: str) -> Callable[[F], F]:
    """Compose the decorated function's docstring from the Rd entry ``alias``.

    Keyword arguments map a Python argument name to the R argument it
    documents where A-9 renames it, as ``polygons="polys"``.
    """

    def decorate(func: F) -> F:
        func.__doc__ = compose(alias, func, r_names)

        return func

    return decorate
