"""The hy frame layer: vocabulary names and representation checks.

`_as_hy` aligns a frame's column names with the vocabulary and types its
identifier and flag columns (I-2, I-3, I-5, I-12, M-18, G-14); it returns the
rename map as a local value, never as frame state (I-14), and `_hy_reverse`
takes it back. `representation` names the pattern a frame follows (A-2).
`requires` fills the registry `_route` reads to dispatch a public function on
that pattern (A-3, A-17).
"""

from __future__ import annotations

import functools
import json
import math
import warnings
from collections.abc import Callable, Mapping, Sequence
from dataclasses import dataclass
from enum import Enum
from importlib.resources import files
from typing import Any, TypeVar

import geopandas as gpd  # type: ignore[import-untyped]
import numpy as np
import pandas as pd  # type: ignore[import-untyped]

from hydroloom._docstrings import doc
from hydroloom.errors import (
    HydroloomError,
    HydroloomWarning,
    RepresentationError,
    msg,
)

F = TypeVar("F", bound=Callable[..., Any])


class Representation(str, Enum):
    """The representation patterns of a network table (A-2)."""

    HY = "hy"
    TOPO = "hy_topo"
    LEVELED = "hy_leveled"
    NODE = "hy_node"
    FLOWNETWORK = "hy_flownetwork"


# Vocabulary ----------------------------------------------------------------


@functools.cache
def _terms() -> dict[str, dict[str, Any]]:
    # Concept entries are not columns (S-2).
    resource = files("hydroloom.data").joinpath("hy_vocab.json")
    terms: dict[str, dict[str, Any]]
    terms = json.loads(resource.read_text(encoding="utf-8"))["terms"]

    return {name: term for name, term in terms.items() if not term.get("concept")}


@functools.cache
def _alias_map() -> tuple[tuple[str, str], ...]:
    # (alias, hy name) in term order, then alias order (align_names.md).
    return tuple(
        (alias["name"].lower(), name)
        for name, term in _terms().items()
        for alias in term.get("aliases") or []
    )


def _terms_of_type(kind: str) -> tuple[str, ...]:
    return tuple(name for name, term in _terms().items() if term.get("type") == kind)


# Name alignment (hy.md step 2, align_names.md steps 2 to 7) ------------------


def _join(values: Sequence[Any]) -> str:
    return ", ".join(str(v) for v in values)


def _align(columns: Sequence[Any], forced: Mapping[str, Any]) -> dict[Any, str]:
    """Return ``{input column: hy name}`` for every column whose name changes.

    ``forced`` maps a hy name to the input column given for it (``id_col``,
    ``toid_col``); that column takes the name ahead of the alias lookup and
    the other candidates for the name keep their input names.
    """
    terms = _terms()
    lower = {}
    for position, column in enumerate(columns, start=1):
        name = str(column)
        if name == "":
            raise KeyError(msg("column_name_empty", position=position))
        lower[column] = name.lower()

    by_lower: dict[str, list[Any]] = {}
    for column, name in lower.items():
        by_lower.setdefault(name, []).append(column)
    for group in by_lower.values():
        if len(group) > 1:
            raise KeyError(msg("column_name_duplicate", columns=_join(group)))

    for column in forced.values():
        if column not in lower:
            raise KeyError(column)
    forced_columns = set(forced.values())

    # Candidates per hy name, in alias-map order (step 2).
    candidates: dict[str, list[tuple[str, Any]]] = {}
    for alias, name in _alias_map():
        if name in forced:
            continue
        for column in by_lower.get(alias, []):
            if column not in forced_columns:
                candidates.setdefault(name, []).append((alias, column))

    # permanent_identifier drops out wherever any hy name has several
    # candidates, a column already holding the name counted among them, so a
    # second pass over aligned names gives the first pass's names.
    def count(name: str, found: list[tuple[str, Any]]) -> int:
        holder = name in by_lower and all(a != name for a, _ in found)
        return len(found) + holder

    if any(count(name, found) > 1 for name, found in candidates.items()):
        candidates = {
            name: [(a, c) for a, c in found if a != "permanent_identifier"]
            for name, found in candidates.items()
        }

    # Conflict: a column renames to a hy name another column already has.
    conflicts = [
        (column, name)
        for name, found in candidates.items()
        for alias, column in found
        if alias != name and name in by_lower
    ]
    conflicts += [
        (column, name)
        for name, column in forced.items()
        if name in by_lower and by_lower[name] != [column]
    ]
    if conflicts:
        raise KeyError(
            msg(
                "name_alias_conflict",
                columns=_join([c for c, _ in conflicts]),
                names=_join([n for _, n in conflicts]),
            )
        )

    for name, found in candidates.items():
        if len(found) > 2:
            raise KeyError(
                msg(
                    "name_alias_ambiguous",
                    columns=_join([c for _, c in found]),
                    name=name,
                )
            )

    renamed = {column: name for name, column in forced.items()}
    for name, found in candidates.items():
        if not found:
            continue
        if len(found) == 2:
            # The later alias in map order wins (step 5).
            warnings.warn(
                msg(
                    "name_alias_duplicate",
                    alias=found[-1][1],
                    aliases=_join([c for _, c in found]),
                ),
                HydroloomWarning,
                stacklevel=2,
            )
        renamed[found[-1][1]] = name

    # A column whose lowercased name is a hy name keeps the lowercase form; any
    # other column keeps its input name (step 7).
    names = {}
    for column in columns:
        new = renamed.get(column)
        if new is None and lower[column] in terms:
            new = lower[column]
        if new is not None and new != column:
            names[column] = new

    return names


# Identifier and flag ingest (hy.md step 5, I-2, I-3, M-18) -------------------


def _missing(value: Any) -> bool:
    if value is None or value is pd.NA:
        return True

    return isinstance(value, float | np.floating) and math.isnan(value)


def _identifier_values(values: pd.Series, column: Any) -> list[str | None]:
    """Return each value as its exact decimal string, missing as ``None`` (I-2).

    A string passes unchanged. A column mixing strings and numbers raises
    ``TypeError`` (I-3); a float with a fractional part or an infinite float
    raises, naming the input column and the 1-based rows (D3-07, I2-3).
    """
    items = values.tolist()
    out: list[str | None] = []
    text_rows, number_rows, fraction_rows, infinite_rows = [], [], [], []
    for row, value in enumerate(items, start=1):
        if _missing(value):
            out.append(None)
        elif isinstance(value, str):
            text_rows.append(row)
            out.append(value)
        else:
            number_rows.append(row)
            is_float = isinstance(value, float | np.floating)
            if is_float and math.isinf(value):
                infinite_rows.append(row)
                out.append(None)
            elif is_float and not float(value).is_integer():
                fraction_rows.append(row)
                out.append(None)
            else:
                out.append(str(int(value)))

    if text_rows and number_rows:
        first = min(text_rows[0], number_rows[0])
        other = number_rows if first == text_rows[0] else text_rows
        raise TypeError(msg("identifier_mixed_type", column=column, rows=_join(other)))
    if fraction_rows:
        raise HydroloomError(
            msg("identifier_not_whole", column=column, rows=_join(fraction_rows))
        )
    if infinite_rows:
        raise HydroloomError(
            msg("identifier_not_finite", column=column, rows=_join(infinite_rows))
        )

    return out


def _flag_values(values: pd.Series, column: Any, ids: pd.Series) -> pd.Series:
    """Return a numeric 0/1 flag column as boolean (M-18).

    ``bool`` when no value is missing, nullable ``boolean`` otherwise. A
    numeric column holding another value warns and is returned as it arrives;
    a non-numeric column is returned as it arrives.
    """
    dtype = values.dtype
    types = pd.api.types
    numeric = types.is_integer_dtype(dtype) or types.is_float_dtype(dtype)
    if types.is_bool_dtype(dtype) or not numeric:
        return values

    number = values.to_numpy(dtype=np.float64, na_value=np.nan)
    missing = np.isnan(number)
    other = ~missing & (number != 0) & (number != 1)
    if other.any():
        warnings.warn(
            msg(
                "flag_value_not_binary",
                column=column,
                rows=_join(ids[other].tolist()),
            ),
            HydroloomWarning,
            stacklevel=2,
        )
        return values

    flags = pd.Series(number == 1, index=values.index)
    if missing.any():
        flags = flags.astype("boolean").mask(missing)

    return flags


def _true(values: pd.Series) -> np.ndarray:
    return np.asarray(values.eq(True).fillna(False), dtype=bool)


def _as_hy(
    network: pd.DataFrame, *, id_col: Any = None, toid_col: Any = None
) -> tuple[pd.DataFrame, dict[Any, str]]:
    """Align names and ingest types; return the frame and the rename map.

    The map is ``{input column: hy name}`` for every column whose name
    changed, the value `_hy_reverse` takes back (A-17, I-14).
    """
    geometry = None
    if isinstance(network, gpd.GeoDataFrame):
        geometry = network.geometry.name
    attrs = pd.DataFrame(network, copy=True)
    if geometry is not None:
        attrs = attrs.drop(columns=geometry)

    forced = {
        name: column
        for name, column in (("id", id_col), ("toid", toid_col))
        if column is not None
    }
    names = _align(list(attrs.columns), forced)
    attrs = attrs.rename(columns=names).reset_index(drop=True)
    if "id" not in attrs.columns:
        raise RepresentationError(msg("no_id_column"))

    inverse = {hy: column for column, hy in names.items()}
    for term in _terms_of_type("id"):
        if term in attrs.columns:
            values = _identifier_values(attrs[term], inverse.get(term, term))
            if term == "toid":
                # I-5: a missing toid is the reserved outlet value.
                values = ["" if v is None else v for v in values]
            attrs[term] = pd.Series(values, index=attrs.index)

    for term in _terms_of_type("flag"):
        if term in attrs.columns:
            attrs[term] = _flag_values(
                attrs[term], inverse.get(term, term), attrs["id"]
            )

    if "toid" in attrs.columns:
        repeated = attrs.duplicated(["id", "toid"], keep=False)
        if repeated.any():
            rows = pd.unique(attrs.loc[repeated, "id"]).tolist()
            raise HydroloomError(msg("id_toid_pair_repeated", rows=_join(rows)))

        main = {"upmain", "downmain"} <= set(attrs.columns)
        if main and not attrs["id"].is_unique:
            if attrs.loc[_true(attrs["downmain"]), "id"].duplicated().any():
                raise HydroloomError(msg("flownetwork_multiple_downmain"))
            if attrs.loc[_true(attrs["upmain"]), "toid"].duplicated().any():
                raise HydroloomError(msg("flownetwork_multiple_upmain"))

    if geometry is None:
        return attrs, names

    # G-14: the active geometry column keeps its name and goes last.
    attrs[geometry] = network.geometry.array

    return gpd.GeoDataFrame(attrs, geometry=geometry), names


def _hy_reverse(out: pd.DataFrame, names: Mapping[Any, str]) -> pd.DataFrame:
    """Give the columns `_as_hy` renamed their input names back (I-14).

    A column `_as_hy` did not rename, such as one a function added, keeps its
    name.
    """
    inverse = {hy: column for column, hy in names.items()}

    return out.rename(columns={c: inverse[c] for c in out.columns if c in inverse})


# Representation and routing (A-2, A-3, A-17) ---------------------------------


def _classify(df: pd.DataFrame) -> Representation:
    """Return the pattern of an aligned frame; first match wins (hy.md step 9)."""
    columns = set(df.columns)
    if {"id", "toid"} <= columns:
        if not df["id"].is_unique:
            return Representation.FLOWNETWORK
        if {"topo_sort", "levelpath", "levelpath_outlet_id"} <= columns:
            return Representation.LEVELED
        return Representation.TOPO
    if {"id", "fromnode", "tonode"} <= columns and df["id"].is_unique:
        return Representation.NODE

    return Representation.HY


@dataclass(frozen=True)
class _Requirement:
    needs: tuple[Representation, ...]
    convert: Mapping[Representation, Callable[..., Any]]


# Public function name -> what it needs; filled by `requires` (A-3).
_REGISTRY: dict[str, _Requirement] = {}

# The remedies key raised when the first need is not met.
_NEEDS_KEY = {Representation.TOPO: "needs_topo", Representation.NODE: "needs_node"}


def requires(
    *needs: Representation,
    convert: Mapping[Representation, Callable[..., Any]] | None = None,
) -> Callable[[F], F]:
    """Register the patterns a public function needs (A-3).

    No ``needs`` means the function takes any table. ``convert`` maps a
    pattern to the call that turns it into one the function needs.
    """

    def register(fn: F) -> F:
        _REGISTRY[fn.__name__] = _Requirement(tuple(needs), dict(convert or {}))

        return fn

    return register


def _route(
    df: pd.DataFrame,
    fn: Callable[..., Any],
    native: Mapping[Representation, Callable[..., Any]],
    **kw: Any,
) -> Any:
    """Call the body of ``fn`` for the pattern of ``df`` (A-3).

    Applies the registered conversion first; a leveled frame takes the topo
    body when it has none of its own; a pattern with no body raises
    `RepresentationError`.
    """
    entry = _REGISTRY[fn.__name__]
    current = _classify(df)
    if current in entry.convert:
        df = entry.convert[current](df)
        current = _classify(df)
    label = current
    if label is Representation.LEVELED and label not in native:
        label = Representation.TOPO
    if label not in entry.needs or label not in native:
        key = _NEEDS_KEY[entry.needs[0]]
        text = msg(key, function=fn.__name__, current=current.value)
        raise RepresentationError(text)

    return native[label](df, **kw)


# Public functions -----------------------------------------------------------


@requires()
@doc("hy")
def as_hy(
    network: pd.DataFrame, *, id_col: Any = None, toid_col: Any = None
) -> pd.DataFrame:
    """Align a table with hydroloom names and types.

    Examples
    --------
    >>> import pandas as pd
    >>> from hydroloom import as_hy
    >>> net = as_hy(pd.DataFrame({"COMID": [1, 2], "ToCOMID": [2, None]}))
    >>> net.columns.tolist()
    ['id', 'toid']
    >>> net["toid"].tolist()
    ['2', '']
    """
    return _as_hy(network, id_col=id_col, toid_col=toid_col)[0]


@requires()
@doc("hy_network_type")
def representation(network: pd.DataFrame) -> str:
    """Name the representation pattern of a network.

    Examples
    --------
    >>> import pandas as pd
    >>> from hydroloom import representation
    >>> representation(pd.DataFrame({"id": [1, 2], "toid": [2, 0]}))
    'hy_topo'
    """
    df, _ = _as_hy(network)

    return _classify(df).value
