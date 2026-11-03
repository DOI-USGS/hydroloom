"""Conformance harness: manifest, vocabulary, converters, and layer comparison.

The manifest (SPEC S-4) drives one pytest item per case in ``test_conformance.py``.
This module holds what those items share: reading the manifest, the vocabulary, the
remedies catalog, fixtures and expectation layers; one converter per ``returns`` kind
(SPEC S-7); and the comparison after vocabulary-typed coercion with the D-13
tolerances and the D-14 order rule (VERIFICATION §2).
"""

from __future__ import annotations

import functools
import importlib.resources
import json
import os
import re
from collections.abc import Callable, Mapping, Sequence
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import geopandas as gpd
import numpy as np
import pandas as pd
import pyogrio
import pytest
import yaml

CONFORMANCE_DIR = (
    Path(__file__).resolve().parents[2] / "inst" / "extdata" / "conformance"
)
FIXTURES_DIR = CONFORMANCE_DIR / "fixtures"
EXPECTED_DIR = CONFORMANCE_DIR / "expected"
MANIFEST_PATH = CONFORMANCE_DIR / "manifest.yaml"

WRITE_ENV = "HYDROLOOM_WRITE_EXPECTED"

# Python names that differ from the manifest's R export name (SPEC S-4); each is
# taken from the Signature section of the function's description.
PYTHON_NAMES = {
    "hy": "as_hy",
    "is.hy": "is_hy",
    "hy_network_type": "representation",
    "hy_capabilities": "capabilities",
    "hydroloom_names": "register_names",
    "hydroloom_name_definitions": "name_definitions",
}

# Column types of the long-table layers (SPEC S-7), in vocabulary type names.
GRAPH_TYPES = {
    "id": "id",
    "position": "count",
    "neighbor": "id",
    "main": "flag",
    "edge_id": "id",
    "mode": "text",
}
TRAVERSAL_TYPES = {"start": "id", "path": "count", "order": "count", "id": "id"}

TEXT_TYPES = {"id", "text"}
INTEGER_TYPES = {"count", "code"}
FLOAT_TYPES = {"measure", "length", "area", "fraction"}

# D-13: atol from meter-scale source precision, by vocabulary unit.
ATOL_BY_UNIT = {"km": 1e-3, "m": 1.0, "km2": 1e-6, "m2": 1.0}
# D-13: compared exactly after rounding to four decimals.
ROUNDED_MEASURES = {"aggregate_id_measure": 4}

INT32 = np.iinfo(np.int32)


# Shared artifacts ----------------------------------------------------------


@functools.cache
def load_manifest() -> dict[str, Any]:
    """Read ``manifest.yaml`` (SPEC S-4)."""
    with MANIFEST_PATH.open(encoding="utf-8") as f:
        return yaml.safe_load(f)


def _read_data_json(name: str) -> dict[str, Any]:
    text = importlib.resources.files("hydroloom.data").joinpath(name)
    return json.loads(text.read_text(encoding="utf-8"))


@functools.cache
def load_vocab() -> dict[str, dict[str, Any]]:
    """Read the vocabulary terms from the package copy (SPEC S-3)."""
    return _read_data_json("hy_vocab.json")["terms"]


@functools.cache
def load_remedies() -> dict[str, dict[str, Any]]:
    """Read the remedies catalog from the package copy (SPEC A-10)."""
    return _read_data_json("remedies.json")["remedies"]


def python_name(function: str) -> str:
    """Return the Python name of a manifest function."""
    return PYTHON_NAMES.get(function, function)


def python_functions(manifest: Mapping[str, Any]) -> set[str]:
    """Return the Python names of functions with a case supported by Python (A-18)."""
    return {
        python_name(case["function"])
        for case in manifest["cases"]
        if "python" in case["supported_by"]
    }


def skip_reason(case: Mapping[str, Any], exported: Sequence[str]) -> str | None:
    """Return why a case cannot run in Python, or ``None`` when it can."""
    function = case["function"]
    name = python_name(function)
    if "python" not in case["supported_by"]:
        return f"{function}: python is not in supported_by"
    if name not in exported:
        return f"{function}: no Python implementation ({name} not in __all__)"

    return None


# Reading and writing layers ------------------------------------------------


def expected_path(case: Mapping[str, Any]) -> Path:
    """Return the expectation GeoPackage of a case (SPEC S-4 ``expected``)."""
    return EXPECTED_DIR / case.get("expected", f"{case['function']}.gpkg")


def read_layer(path: Path, layer: str, *, geometry: bool = False) -> pd.DataFrame:
    """Read one layer; attributes only unless ``geometry`` (SPEC S-5)."""
    return pyogrio.read_dataframe(path, layer=layer, read_geometry=geometry)


def read_expected(case: Mapping[str, Any]) -> pd.DataFrame:
    """Read the expectation layer of a case."""
    return read_layer(
        expected_path(case), case["id"], geometry=case.get("geometry", False)
    )


def read_fixture(fixture: str) -> pd.DataFrame:
    """Read a fixture GeoPackage, geometry included where it has one."""
    return pyogrio.read_dataframe(FIXTURES_DIR / fixture)


def write_layer(layer: pd.DataFrame, path: Path, name: str) -> None:
    """Write or replace one expectation layer (S-7, Python co-generator only)."""
    pyogrio.write_dataframe(
        layer, path, layer=name, driver="GPKG", layer_options={"OVERWRITE": "YES"}
    )


# Converters, one per returns kind (SPEC S-7) --------------------------------


@dataclass
class Graph:
    """A CSR graph with the fields of ``IndexIds`` (make_index_ids.md, SPEC K-3)."""

    indptr: np.ndarray
    indices: np.ndarray
    uniques: Sequence[str]
    mode: str | None = None
    main: np.ndarray | None = None
    edge_id: np.ndarray | None = None


def frame_to_layer(
    result: Any, *, columns: Sequence[str] | None = None, geometry: bool = False
) -> pd.DataFrame:
    """Convert a frame return to its layer; ``True`` becomes a scalar layer (D3-53)."""
    if isinstance(result, bool | np.bool_):
        return scalar_to_layer(bool(result))

    df = result
    keep = list(columns) if columns else None
    if isinstance(df, gpd.GeoDataFrame):
        if geometry:
            if df.geometry.name != "geometry":
                df = df.rename_geometry("geometry")
            if keep is not None:
                keep.append("geometry")
        else:
            df = pd.DataFrame(df.drop(columns=df.geometry.name))
    if keep is not None:
        df = df[keep]

    return df.reset_index(drop=True)


def layer_to_frame(layer: pd.DataFrame) -> pd.DataFrame:
    """Return a frame layer as the frame it stores."""
    return layer


def _graph_part(graph: Any) -> pd.DataFrame:
    indptr = np.asarray(graph.indptr, dtype=np.int64)
    indices = np.asarray(graph.indices, dtype=np.int64)
    uniques = np.asarray([str(u) for u in graph.uniques], dtype=object)
    counts = np.diff(indptr)
    rows = np.repeat(np.arange(len(uniques)), counts)
    position = np.arange(len(indices)) - np.repeat(indptr[:-1], counts)

    cols: dict[str, Any] = {
        "id": uniques[rows],
        "position": position.astype(np.int32),
        "neighbor": uniques[indices],
    }
    if graph.main is not None:
        cols["main"] = np.asarray(graph.main, dtype=bool)
    if graph.edge_id is not None:
        cols["edge_id"] = np.asarray([str(e) for e in graph.edge_id], dtype=object)

    return pd.DataFrame(cols)


def graph_to_layer(graph: Any) -> pd.DataFrame:
    """Convert a graph, or a ``(to, from)`` pair for mode both, to its layer."""
    if isinstance(graph, tuple):
        parts = [
            _graph_part(g).assign(mode=m)
            for g, m in zip(graph, ("to", "from"), strict=True)
        ]
        return pd.concat(parts, ignore_index=True)

    return _graph_part(graph)


def _as_text(series: pd.Series) -> np.ndarray:
    return series.astype(str).to_numpy(dtype=object)


def _layer_graph(part: pd.DataFrame, mode: str | None) -> Graph:
    # K-2: dense positions by factorize, then argsort, bincount, cumsum.
    n = len(part)
    both = np.concatenate([_as_text(part["id"]), _as_text(part["neighbor"])])
    codes, uniques = pd.factorize(both, sort=True)
    rank = codes[:n].astype(np.int64)
    sort = np.lexsort((part["position"].to_numpy(), rank))

    counts = np.bincount(rank, minlength=len(uniques))
    indptr = np.concatenate([[0], np.cumsum(counts)]).astype(np.int64)
    indices = codes[n:].astype(np.int64)[sort]
    main = part["main"].to_numpy(dtype=bool)[sort] if "main" in part else None
    edge_id = _as_text(part["edge_id"])[sort] if "edge_id" in part else None

    return Graph(indptr, indices, list(uniques), mode, main, edge_id)


def layer_to_graph(layer: pd.DataFrame) -> Graph | tuple[Graph, Graph]:
    """Convert a graph layer to a graph, or the ``(to, from)`` pair if it has ``mode``.

    The node set is the identifiers the layer holds; a terminal with no rows is
    absent, and dense positions follow identifier byte order (S-7: not contract).
    """
    if "mode" in layer.columns:
        return tuple(
            _layer_graph(layer[layer["mode"] == m].drop(columns="mode"), m)
            for m in ("to", "from")
        )  # type: ignore[return-value]

    return _layer_graph(layer, None)


def _unique_starts(starts: Any) -> list[str]:
    if isinstance(starts, str | int | np.integer):
        starts = [starts]
    return list(dict.fromkeys(str(s) for s in starts))


def traversal_to_layer(
    result: Sequence[Sequence[Sequence[Any]]], starts: Any
) -> pd.DataFrame:
    """Convert a traversal (one element per unique start, a list of paths) to a layer."""
    unique = _unique_starts(starts)
    if len(unique) != len(result):
        raise AssertionError(
            f"traversal has {len(result)} elements for {len(unique)} unique starts"
        )

    # One row per path, then one row per visited flowline; empty paths drop out.
    paths = pd.DataFrame({"start": unique, "id": list(result)}).explode("id")
    paths["path"] = paths.groupby(level=0).cumcount()
    visits = paths.explode("id").dropna(subset=["id"])
    visits["order"] = visits.groupby(level=0).cumcount()
    layer = visits[["start", "path", "order", "id"]].reset_index(drop=True)

    return layer.astype(
        {"start": object, "path": np.int32, "order": np.int32, "id": str}
    ).astype({"id": object})


def layer_to_traversal(layer: pd.DataFrame) -> tuple[list[str], list[list[list[str]]]]:
    """Convert a traversal layer to ``(starts, result)``.

    Starts with no visited flowline have no rows and are absent (S-7).
    """
    visits = pd.DataFrame(
        {
            "start": _as_text(layer["start"]),
            "path": layer["path"].to_numpy(np.int64),
            "order": layer["order"].to_numpy(np.int64),
            "id": _as_text(layer["id"]),
        }
    )
    starts = list(pd.unique(visits["start"]))
    by_path = (
        visits.sort_values("order", kind="stable")
        .groupby(["start", "path"], sort=True)["id"]
        .agg(list)
    )
    by_start = by_path.groupby(level="start", sort=False).agg(list)

    return starts, [by_start[s] for s in starts]


def scalar_to_layer(value: Any) -> pd.DataFrame:
    """Convert a scalar return to its one-row, one-column ``value`` layer."""
    return pd.DataFrame({"value": [value]})


def layer_to_scalar(layer: pd.DataFrame) -> Any:
    """Return the value a scalar layer stores."""
    if list(layer.columns) != ["value"] or len(layer) != 1:
        raise AssertionError(
            f"scalar layer must be one row of 'value', got {list(layer.columns)} "
            f"with {len(layer)} rows"
        )
    value = layer["value"].iloc[0]

    return value.item() if isinstance(value, np.generic) else value


def result_to_layer(result: Any, case: Mapping[str, Any], returns: str) -> pd.DataFrame:
    """Convert a return value to its expectation layer by ``returns`` kind."""
    if returns == "frame":
        return frame_to_layer(
            result, columns=case.get("columns"), geometry=case.get("geometry", False)
        )
    if returns == "graph":
        layer = graph_to_layer(result)
    elif returns == "traversal":
        layer = traversal_to_layer(result, (case.get("args") or {})["starts"])
    elif returns == "scalar":
        layer = scalar_to_layer(result)
    else:
        raise ValueError(f"unknown returns kind {returns!r}")
    if case.get("columns"):
        layer = layer[list(case["columns"])]

    return layer


# Comparison (SPEC S-7, D-13, D-14) -----------------------------------------


def column_types(columns: Sequence[str], returns: str = "frame") -> dict[str, str]:
    """Return the vocabulary type of each column that has one."""
    vocab = load_vocab()
    types = {c: vocab[c]["type"] for c in columns if vocab.get(c, {}).get("type")}
    if returns == "graph":
        types.update({c: t for c, t in GRAPH_TYPES.items() if c in columns})
    elif returns == "traversal":
        types.update({c: t for c, t in TRAVERSAL_TYPES.items() if c in columns})

    return types


def _decimal(value: Any) -> str:
    if isinstance(value, float | np.floating) and float(value).is_integer():
        return str(int(value))

    return str(value)


def _text(series: pd.Series, column: str, how: str) -> pd.Series:
    """Text as object dtype with ``None`` missing (I-13).

    ``how``: ``strict`` fails on a non-``str`` value (I-1), ``convert`` writes a
    number as its decimal string, ``keep`` leaves values as they are.
    """
    s = series.astype(object)
    na = s.isna()
    other = ~na & ~s.map(type).eq(str)
    if how == "strict" and other.any():
        value = s[other].iloc[0]
        raise AssertionError(
            f"column {column!r}: value {value!r} is {type(value).__name__}, "
            "not str (SPEC I-1)"
        )
    if how == "convert" and other.any():
        s = s.where(~other, s[other].map(_decimal))

    return s.where(~na, None)


def _is_texty(series: pd.Series) -> bool:
    return series.dtype == object or isinstance(series.dtype, pd.StringDtype)


def _coerce(
    actual: pd.DataFrame, expected: pd.DataFrame, types: Mapping[str, str]
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Coerce the expectation to the vocabulary dtypes; text on both sides (S-7)."""
    actual = actual.copy()
    expected = expected.copy()
    for col in expected.columns:
        kind = types.get(col)
        if col == "geometry" or kind == "flag":
            continue
        if kind in TEXT_TYPES:
            expected[col] = _text(expected[col], col, "convert")
            actual[col] = _text(actual[col], col, "strict")
        elif kind in INTEGER_TYPES:
            values = expected[col].astype(np.float64)
            outside = ((values < INT32.min) | (values > INT32.max)).any()
            if outside:
                # I-15: a count or code outside the 32-bit range compares as double.
                expected[col] = values
                actual[col] = actual[col].astype(np.float64)
            elif values.isna().any():
                expected[col] = values
            else:
                expected[col] = expected[col].astype(np.int32)
        elif kind in FLOAT_TYPES:
            expected[col] = expected[col].astype(np.float64)
        else:
            if _is_texty(expected[col]):
                expected[col] = _text(expected[col], col, "keep")
            if _is_texty(actual[col]):
                actual[col] = _text(actual[col], col, "keep")

    return actual, expected


def _atol(
    col: str, kind: str | None, expected: pd.DataFrame, atol: float | None
) -> float | np.ndarray:
    """Return the D-13 absolute tolerance of a column, per row for a measure."""
    if atol is not None:
        return atol
    unit = load_vocab().get(col, {}).get("unit")
    if kind in {"length", "area"} and unit in ATOL_BY_UNIT:
        return ATOL_BY_UNIT[unit]
    if kind == "measure":
        if "length_km" not in expected.columns:
            raise AssertionError(
                f"column {col!r}: measure tolerance needs length_km, which the "
                "layer does not have (SPEC D-13)"
            )
        # D-13: one meter along the flowline, 100 / length_m percent, per row.
        with np.errstate(divide="ignore"):
            return 100.0 / (expected["length_km"].to_numpy(np.float64) * 1000.0)

    return 0.0


def _assert_close(a: pd.Series, e: pd.Series, col: str, atol: Any, rtol: float) -> None:
    obj = f"column {col!r}"
    if np.ndim(atol) == 0:
        pd.testing.assert_series_equal(
            a, e, check_names=False, check_exact=False, rtol=rtol, atol=atol, obj=obj
        )
        return

    # Per-row atol (D-13 measure): pandas takes a scalar atol only.
    pd.testing.assert_series_equal(a.isna(), e.isna(), check_names=False, obj=obj)
    if a.dtype != e.dtype:
        raise AssertionError(f"{obj}: dtype {a.dtype}, expected {e.dtype}")
    av, ev = a.to_numpy(np.float64), e.to_numpy(np.float64)
    bad = np.abs(av - ev) > atol + rtol * np.abs(ev)
    if bad.any():
        i = int(np.flatnonzero(bad)[0])
        raise AssertionError(
            f"{obj}: {int(bad.sum())} of {len(bad)} rows differ beyond "
            f"tolerance; first at row {i}: {av[i]!r}, expected {ev[i]!r}"
        )


def _frame_sort(expected: pd.DataFrame, key: Sequence[str] | None) -> list[str]:
    """Return the sort columns of an ``order: ignore`` frame.

    ``id`` first (the D-7 sort), then ``key``, then the other non-float columns as
    tie breakers; every non-geometry column only when neither ``id`` nor ``key``
    is there.
    """
    cols = [c for c in expected.columns if c != "geometry"]
    head = ["id"] if "id" in cols else []
    head += [k for k in key or [] if k in cols and k not in head]
    if not head:
        return cols
    rest = [
        c
        for c in cols
        if c not in head and not pd.api.types.is_float_dtype(expected[c])
    ]

    return head + rest


def assert_layer_equal(
    actual: pd.DataFrame,
    expected: pd.DataFrame,
    *,
    returns: str = "frame",
    key: Sequence[str] | None = None,
    compare: str = "exact",
    order: str = "exact",
    atol: float | None = None,
    rtol: float | None = None,
) -> None:
    """Assert a result layer equals its expectation layer.

    Columns compare by name (D-14), after coercion to the vocabulary dtypes (S-7).
    ``compare="tolerance"`` applies the D-13 ``atol`` per type, or the case's
    ``atol``, with ``rtol`` 0 unless the case gives one. Rows are sorted before
    comparison on ``key`` for a graph or traversal layer always, and for a frame
    when ``order="ignore"`` on ``id``, then ``key``, then the other non-float
    columns (all columns where there is neither ``id`` nor ``key``).
    """
    missing = [c for c in expected.columns if c not in actual.columns]
    extra = [c for c in actual.columns if c not in expected.columns]
    if missing or extra:
        raise AssertionError(f"columns missing: {missing}; columns extra: {extra}")
    if len(actual) != len(expected):
        raise AssertionError(f"{len(actual)} rows, expected {len(expected)}")

    actual = actual[list(expected.columns)].reset_index(drop=True)
    expected = expected.reset_index(drop=True)
    types = column_types(list(expected.columns), returns)
    actual, expected = _coerce(actual, expected, types)

    if returns in {"graph", "traversal"}:
        by = list(key) if key else _frame_sort(expected, None)
    elif order == "ignore":
        by = _frame_sort(expected, key)
    else:
        by = []
    if by:
        actual = actual.sort_values(by, kind="stable").reset_index(drop=True)
        expected = expected.sort_values(by, kind="stable").reset_index(drop=True)

    tolerance = compare == "tolerance"
    for col in expected.columns:
        a, e = actual[col], expected[col]
        kind = types.get(col)
        if col == "geometry":
            from geopandas.testing import assert_geoseries_equal

            assert_geoseries_equal(gpd.GeoSeries(a), gpd.GeoSeries(e))
        elif col in ROUNDED_MEASURES and pd.api.types.is_float_dtype(e):
            pd.testing.assert_series_equal(
                a.round(ROUNDED_MEASURES[col]),
                e.round(ROUNDED_MEASURES[col]),
                check_names=False,
                check_exact=True,
                obj=f"column {col!r}",
            )
        elif tolerance and pd.api.types.is_float_dtype(e):
            _assert_close(a, e, col, _atol(col, kind, expected, atol), rtol or 0.0)
        else:
            pd.testing.assert_series_equal(
                a, e, check_names=False, check_exact=True, obj=f"column {col!r}"
            )


# Conditions (SPEC A-10) ----------------------------------------------------


def remedy_pattern(key: str) -> re.Pattern[str]:
    """Return a pattern matching the Python message of a remedies key.

    The template is ``message_python``, or ``message`` where the entry has no
    Python text; each ``str.format`` placeholder matches any text.
    """
    entry = load_remedies()[key]
    template = entry.get("message_python", entry["message"])
    parts = re.split(r"\{[A-Za-z_][^{}]*\}", template)
    literal = [re.escape(p.replace("{{", "{").replace("}}", "}")) for p in parts]

    return re.compile(".*?".join(literal), re.DOTALL)


def _message(condition: BaseException | Warning) -> str:
    args = getattr(condition, "args", ())
    return args[0] if args and isinstance(args[0], str) else str(condition)


def assert_raises(call: Callable[[], Any], case: Mapping[str, Any]) -> None:
    """Assert a call signals the case's condition with its remedies text (A-10)."""
    pattern = remedy_pattern(case["raises_key"]) if "raises_key" in case else None
    if case["raises"] == "error":
        with pytest.raises(Exception) as info:
            call()
        messages = [_message(info.value)]
    else:
        with pytest.warns(Warning) as record:
            call()
        messages = [_message(w.message) for w in record]
    if pattern is not None and not any(pattern.fullmatch(m) for m in messages):
        raise AssertionError(
            f"no condition matched remedies key {case['raises_key']!r}: {messages}"
        )


# One case ------------------------------------------------------------------


def _prepare_is_the_target(case: Mapping[str, Any]) -> bool:
    """Whether the case's ``prepare`` is a step the Python target runs itself.

    R's `hy_network_type` reads the class `hy()` stamps, so its cases prepare
    with `hy`. Python's `representation` aligns names and types `id` on its
    own and validates no edge (hy_network_type.md Algorithm step 1,
    Preconditions); running `as_hy` first would raise on edges it does not
    check, such as a repeated (id, toid) pair.
    """
    prepare = case["prepare"]

    return (
        case["function"] == "hy_network_type"
        and prepare["function"] == "hy"
        and not prepare.get("args")
    )


def run_case(case: Mapping[str, Any], manifest: Mapping[str, Any]) -> None:
    """Run one manifest case through the Python package and compare."""
    import hydroloom

    spec = manifest["functions"][case["function"]]
    target = getattr(hydroloom, python_name(case["function"]))
    args = dict(case.get("args") or {})

    if "fixture" in case:
        network = read_fixture(case["fixture"])
        if "prepare" in case and not _prepare_is_the_target(case):
            prepare = case["prepare"]
            fn = getattr(hydroloom, python_name(prepare["function"]))
            network = fn(network, **(prepare.get("args") or {}))

        def call() -> Any:
            return target(network, **args)

    else:

        def call() -> Any:
            return target(**args) if callable(target) else target

    if "raises" in case:
        assert_raises(call, case)
        return

    layer = result_to_layer(call(), case, spec["returns"])
    if os.environ.get(WRITE_ENV) == "1" and "python" in case["generator"]:
        write_layer(layer, expected_path(case), case["id"])
        return

    assert_layer_equal(
        layer,
        read_expected(case),
        returns=spec["returns"],
        key=spec.get("key"),
        compare=case["compare"],
        order=case["order"],
        atol=case.get("atol"),
        rtol=case.get("rtol"),
    )
