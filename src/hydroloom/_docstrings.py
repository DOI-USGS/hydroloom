"""Compose numpydoc docstrings from the R documentation (SPEC A-13).

roxygen is the one authored source for argument meanings. ``dev/docstrings.R``
writes ``docstrings.json`` from ``man/``; :func:`doc` reads it at import and
writes the decorated function's ``__doc__``. Every composed field passes
through :data:`SUBSTITUTIONS`, the R-phrasing table, in order.

The composed docstring holds, in numpydoc order: the summary from ``title``;
the extended summary from ``description`` then ``details``; Parameters from
``arguments``, restricted to and ordered by the Python signature; Returns from
``value``; Notes holding each transferred Rd section as a paragraph led by its
title, then a line that identifies the R function the text comes from; and
the Examples block of the function's own hand-written ``__doc__``, the only
part of that ``__doc__`` kept. No See Also is written: the R function is not a
Python object numpydoc can link.
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
    # R-only facts (A-13, P-4). The roxygen states each one in a sentence or
    # paragraph of its own that starts "In R,"; these two pairs drop them, with
    # the space or blank line before, and run first so no phrasing pair
    # rewrites R text. A paragraph that opens with "In R," is R-only through
    # its end, with any list that follows it, bullet (`- `) or numbered
    # (`1. `). An "In R," sentence inside a paragraph ends at its first `.`,
    # `?` or `!` followed by whitespace or the end of the field, so it must
    # not contain an abbreviation such as "e.g. " or "i.e. ";
    # tests/python/test_docstrings.py enforces this over docstrings.json.
    (r"(?s)(?:\A|\n\n)In R,.*?(?=\n\n(?!- |\d+\. )|\Z)", ""),
    (r" In R,[^\n]*?[.?!](?=\s|\Z)", ""),
    # The A-13 examples.
    (r"logical if TRUE", "if `True`"),
    (r"logical if `TRUE`", "if `True`"),
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
        "one value per row of `network`, containing ",
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
    (r"an `hy` object", "a network table"),
    (r"`hy` object", "network table"),
    (r"\bhy object", "network table"),
    (r"an `hy`", "a network table"),
    (r"required attributes", "required columns"),
    (r"requires attributes", "requires columns"),
    # Column types, in the vocabulary's type names.
    (r"\(character\)", "(text)"),
    (r"\(character:", "(text:"),
    (r"\(logical\)", "(flag)"),
    (r"a logical flag", "a flag"),
    (r"a logical value", "a flag"),
    (r"logical columns", "flag columns"),
    # Logical, missing, and null values, as Python writes them.
    (r"`(\w+) = TRUE`", r"`\1=True`"),
    (r"`(\w+) = FALSE`", r"`\1=False`"),
    (r"`TRUE`", "`True`"),
    (r"`FALSE`", "`False`"),
    (r"(?<!= )\bTRUE\b", "`True`"),
    (r"(?<!= )\bFALSE\b", "`False`"),
    (r"an `NA`", "a missing"),
    (r"`NA`", "missing"),
    (r"If missing or NULL", "If not given"),
    (r"When non-NULL", "When given"),
    (r"When NULL", "When not given"),
    (r"is NULL", "is `None`"),
    (r"Default NULL", "Default `None`"),
    (r"or NULL", "or `None`"),
    (r"`Inf`", "infinity"),
    # R call syntax, and R functions with a differently named Python counterpart.
    # Backticked `x` is the network everywhere but check_valid, whose `x` is
    # the lines operand (A-9, phase 3).
    (r"(?<!CRS of )`x`", "`network`"),
    (
        r"c\(UM, DM, UT, DD\) or equivalently c\(upmain, downmain, up, down\)",
        "UM, DM, UT, DD or equivalently upmain, downmain, up, down",
    ),
    (r"`hy\(\)`", "`as_hy()`"),
    (r"`hy_network_type\(\)`", "`representation()`"),
    (r"`hy_capabilities\(\)`", "`capabilities()`"),
    (r"`is\.hy\(\)`", "`is_hy()`"),
    (r"\bhydroloom_name_definitions\b", "`name_definitions()`"),
    # Details follow the description in the extended summary, so a "See
    # details" inside the description (accumulate_downstream) is left alone.
    (r"(?i)\b(s)ee details\b(?! for required)", r"\1ee the description above"),
)

_COMPILED = tuple((re.compile(pattern), repl) for pattern, repl in SUBSTITUTIONS)

# Python-only arguments (A-9) have no roxygen text; theirs comes from the
# Signature section of the function's description. Keyed by alias.
PYTHON_ARGUMENTS: dict[str, dict[str, str]] = {
    "hy": {
        "id_col": "the input column to take as `id`, in place of any column "
        "hydroloom would choose by its known source names; `None` lets "
        "hydroloom choose.",
        "toid_col": "the input column to take as `toid`, in place of any "
        "column hydroloom would choose by its known source names; `None` lets "
        "hydroloom choose.",
    },
    "hy_reverse": {
        "source": "the dataset whose column names to rename to: `nhdplus`, "
        "`nhdplushr`, `3dhp`, or `hydrofabric`. `None` gives each hydroloom "
        "name the alias most recently registered for it, in lowercase, and "
        "leaves a name with no registered alias as it is.",
    },
}


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
    where the two differ (A-9). An argument resolves to its ``r_names`` entry,
    then to the R argument of the same name or its `PYTHON_ARGUMENTS` text,
    then to ``x`` for ``network``. A Python argument with no text raises
    ``KeyError``.
    """
    entry = r_docs()[alias]
    r_names = r_names or {}
    r_args = dict(entry.get("arguments") or {})
    r_args.update(PYTHON_ARGUMENTS.get(alias, {}))
    signature = inspect.signature(func)

    # A field is appended only if text is left after substitution, so a field
    # that is R-only from start to end adds nothing.
    parts = [_field(entry["title"], period=True)]
    for key in ("description", "details"):
        text = _field(entry.get(key) or "")
        if text:
            parts.append(text)

    params = []
    for param in signature.parameters.values():
        if param.name in r_names:
            r_name = r_names[param.name]
        elif param.name in r_args:
            r_name = param.name
        else:
            r_name = _DEFAULT_R_NAMES.get(param.name, param.name)
        if r_name not in r_args:
            raise KeyError(f"{alias}.{param.name}")
        # PYTHON_ARGUMENTS text already replaces the R text; an R-only
        # argument with no Python text keeps its line and no description.
        text = _field(r_args[r_name], period=True)
        line = _parameter_line(param)
        params.append(f"{line}\n{_indent(text)}" if text else line)
    if params:
        parts.append(_section("Parameters", "\n".join(params)))

    value = _field(entry.get("value") or "", period=True)
    if value:
        returns = _annotation(signature.return_annotation)
        parts.append(_section("Returns", f"{returns}\n{_indent(value)}"))

    notes = [
        f"{_field(title)}:\n\n{_field(text)}"
        for title, text in (entry.get("sections") or {}).items()
        if _field(text)
    ]
    notes.append(f"Documentation adapted from `{alias}()` in the hydroloom R package.")
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
