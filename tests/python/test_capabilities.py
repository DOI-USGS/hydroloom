"""capabilities: Postconditions of hy_capabilities.md as properties and interface tests.

Every shared hy_capabilities case stays ``[r]``: each R expectation layer has one
row per entry of R's 24-entry code table, and Python's code table is the
``@requires`` registry (hy_capabilities.md Algorithm, A-4). These tests check the
Postconditions against the registry; no computed value is written inline (S-7).
"""

from __future__ import annotations

import doctest

import geopandas as gpd
import pandas as pd
import pytest
import yaml
from hypothesis import given, settings
from hypothesis import strategies as st
from numpydoc.docscrape import get_doc_object
from numpydoc.validate import validate
from shapely.geometry import Point

from conformance import CONFORMANCE_DIR
from hydroloom import representation
from hydroloom._hy import _REGISTRY, capabilities, requires

PROPERTY = settings(derandomize=True, max_examples=100, deadline=None)

COLUMNS = (
    "id",
    "toid",
    "fromnode",
    "tonode",
    "topo_sort",
    "levelpath",
    "levelpath_outlet_id",
    "divergence",
    "aggregate_id",
    "length_km",
    "notes",
)

# hy_capabilities.md Postconditions: the rows available on a flownetwork.
FLOWNETWORK_AVAILABLE = {
    "sort_network",
    "add_topo_sort",
    "check_hy_graph",
    "navigate_network_dfs",
    "make_index_ids",
}

# hy_capabilities.md Postconditions: the calls a via may name.
VIA_CALLS = {"add_toids", "make_node_topology"}

values = st.one_of(st.integers(0, 4), st.none())


@st.composite
def tables(draw, *, columns=COLUMNS, geometry=None):
    """Any table over the network columns, ids small so they repeat."""
    names = draw(st.lists(st.sampled_from(columns), unique=True, max_size=6))
    n = draw(st.integers(0, 6))
    data = {c: draw(st.lists(values, min_size=n, max_size=n)) for c in names}
    table = pd.DataFrame(data, index=range(n))
    if geometry is None:
        geometry = draw(st.booleans())
    if geometry:
        return gpd.GeoDataFrame(table, geometry=[Point(i, 0) for i in range(n)])

    return table


def _any(name):
    return not _REGISTRY[name].needs


def _needs(name):
    return {need.value for need in _REGISTRY[name].needs}


# Rows and columns ----------------------------------------------------------


@PROPERTY
@given(tables())
def test_one_row_per_registry_entry(table):
    """The rows are the registry's entries, each once, by name (A-4)."""
    out = capabilities(table)

    assert out.columns.tolist() == ["function", "available", "via"]
    assert out["function"].is_unique
    assert out["function"].tolist() == sorted(_REGISTRY)


@PROPERTY
@given(tables())
def test_available_and_via_never_missing(table):
    """``available`` is bool and ``via`` is text, neither missing."""
    out = capabilities(table)

    assert out["available"].dtype == bool
    assert out["via"].map(type).eq(str).all()
    assert out["function"].map(type).eq(str).all()


@PROPERTY
@given(tables())
def test_via_only_where_unavailable(table):
    """A non-empty ``via`` implies ``available`` is false."""
    out = capabilities(table)

    assert not (out["available"] & (out["via"] != "")).any()


@PROPERTY
@given(tables())
def test_via_names_a_conversion(table):
    """Every non-empty ``via`` is ``add_toids`` or ``make_node_topology``."""
    via = capabilities(table)["via"]

    assert set(via[via != ""]) <= VIA_CALLS


@PROPERTY
@given(tables(columns=tuple(c for c in COLUMNS if c != "id")))
def test_no_representation_answers(table):
    """With no ``id``, only ``any`` rows can be available; no via (D3-167)."""
    out = capabilities(table)

    assert all(_any(f) for f in out.loc[out["available"], "function"])
    assert (out["via"] == "").all()


@st.composite
def flownetworks(draw):
    """Draw a table with a repeated id and distinct toids, plus other columns."""
    extra = draw(tables(columns=COLUMNS[2:], geometry=False))
    n = len(extra)
    ids = [1, 1, *draw(st.lists(st.integers(1, 4), min_size=n, max_size=n))]
    toids = [2, 3, *draw(st.lists(st.integers(0, 4), min_size=n, max_size=n))]
    table = pd.DataFrame({"id": ids, "toid": toids})
    for column in extra.columns:
        table[column] = [*extra[column], None, None]

    return table


@PROPERTY
@given(flownetworks())
def test_flownetwork_answer(table):
    """On a flownetwork the five named rows are available, no via (D3-42).

    A table with a repeated (id, toid) pair returns the same answer and does not
    raise (D3-196).
    """
    assert representation(table) == "hy_flownetwork"
    out = capabilities(table)
    available = set(out.loc[out["available"], "function"])
    patterned = {f for f in out["function"] if not _any(f)}

    assert available & patterned == FLOWNETWORK_AVAILABLE & set(_REGISTRY)
    assert (out["via"] == "").all()


@PROPERTY
@given(tables(), st.randoms(use_true_random=False))
def test_row_permutation_invariant(table, rng):
    """The output is unchanged by any permutation of the input rows (D-7)."""
    order = list(range(len(table)))
    rng.shuffle(order)

    pd.testing.assert_frame_equal(capabilities(table.iloc[order]), capabilities(table))


@PROPERTY
@given(tables())
def test_input_not_modified(table):
    """The input is not modified (A-8)."""
    before = table.copy()
    capabilities(table)

    pd.testing.assert_frame_equal(table, before)


# Interface -----------------------------------------------------------------


def test_repeated_pair_returns_the_flownetwork_answer():
    """A repeated (id, toid) pair is a flownetwork answer, not a raise (D3-196)."""
    divergence = pd.DataFrame({"id": [1, 1, 2, 3], "toid": [2, 3, 0, 0]})
    repeated = pd.concat([divergence, divergence.iloc[[0]]], ignore_index=True)

    pd.testing.assert_frame_equal(capabilities(repeated), capabilities(divergence))


def test_leveled_satisfies_topo():
    """A leveled table answers every hy_topo row as a topo table does (A-3)."""
    topo = pd.DataFrame({"id": [1, 2, 3], "toid": [2, 3, 0]})
    leveled = topo.assign(topo_sort=[3, 2, 1], levelpath=1, levelpath_outlet_id=3)
    a, b = capabilities(leveled), capabilities(topo)
    rows = [f for f in b["function"] if "hy_topo" in _needs(f)]

    assert rows
    a, b = a.set_index("function").loc[rows], b.set_index("function").loc[rows]
    pd.testing.assert_frame_equal(a, b)


@pytest.mark.parametrize("value", [None, [1, 2], "id", pd.Series([1, 2])])
def test_answers_on_anything(value):
    """Any object gets an answer with no representation (Preconditions, B-1)."""
    out = capabilities(value)

    assert all(_any(f) for f in out.loc[out["available"], "function"])
    assert (out["via"] == "").all()


def test_no_include_conversions():
    """Python takes no ``include_conversions`` argument (B-5, D3-31)."""
    with pytest.raises(TypeError):
        capabilities(pd.DataFrame({"id": [1]}), include_conversions=True)


def test_geometry_column_gate(monkeypatch):
    """A ``geom`` column need is met by an active geometry, with no id (G-14)."""
    monkeypatch.setitem(_REGISTRY, "geom_fn", None)

    @requires(columns=["geom"])
    def geom_fn(network):
        pass

    plain = pd.DataFrame({"notes": [1, 2]})
    spatial = gpd.GeoDataFrame(plain, geometry=[Point(0, 0), Point(1, 0)])
    rows = {
        name: frame.set_index("function").loc["geom_fn"]
        for name, frame in (
            ("plain", capabilities(plain)),
            ("spatial", capabilities(spatial)),
        )
    }

    assert not rows["plain"]["available"]
    assert rows["spatial"]["available"]


def test_registry_entry_matches_requires():
    """The capabilities entry equals its description's front matter (A-4)."""
    path = CONFORMANCE_DIR / "algorithms" / "hy_capabilities.md"
    front = path.read_text(encoding="utf-8").split("---\n", maxsplit=2)[1]
    requires_ = yaml.safe_load(front)["requires"]
    entry = _REGISTRY["capabilities"]

    assert requires_["pattern"] == ["any"]
    assert entry.needs == ()
    assert list(entry.columns) == list(requires_["columns"])
    assert dict(entry.via) == (requires_["via"] or {})


def test_doctest():
    """The hand-written example runs (A-13)."""
    tests = doctest.DocTestFinder().find(capabilities, "capabilities", globs={})
    runner = doctest.DocTestRunner()
    for test in tests:
        runner.run(test)

    assert tests and tests[0].examples
    assert runner.summarize(verbose=False).failed == 0


def test_docstring_validates():
    """The composed docstring validates; the R title is a question (A-13)."""
    result = validate(get_doc_object(capabilities))
    codes = {code for code, _ in result["errors"]}

    assert codes <= {"SA01", "SS03"}, result["errors"]
