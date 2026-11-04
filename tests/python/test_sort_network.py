"""sort_network and add_topo_sort: outlet-fill layers, interface, properties.

The manifest marks 20 sort_network and add_topo_sort cases ``r_only`` because
R writes the outlet ``toid`` of an integer ``id`` as ``0`` where Python writes
``""``: 15 through the add_toids node route (add_toids.md Algorithm step 6) and
5 through the ``hy()`` fill of a missing ``toid`` (hy.md Algorithm, I-5). The
sort does not read the outlet value (I-4), so row order and every other column
match. Each layer is run here against its stored layer with ``"0"`` in the
``toid`` column read as ``""``, as test_add_toids.py does for add_toids (P-3);
the expectation stays in the GeoPackage (S-7). Properties follow the
Postconditions of sort_network.md and add_topo_sort.md.
"""

from __future__ import annotations

import re
import warnings
from collections import defaultdict

import geopandas as gpd
import numpy as np
import pandas as pd
import pytest
from hypothesis import given, settings
from hypothesis import strategies as st
from shapely.geometry import Point

import hydroloom
from conformance import (
    assert_layer_equal,
    load_manifest,
    python_name,
    read_expected,
    read_fixture,
    result_to_layer,
)
from hydroloom import add_topo_sort, align_names, sort_network
from hydroloom._kernels import walk
from hydroloom.errors import HydroloomError, RepresentationError, msg

PROPERTY = settings(derandomize=True, max_examples=100, deadline=None)

MANIFEST = load_manifest()
OUTLET_FILL = {"add_toids.md §Algorithm step 6", "hy.md §Algorithm"}
SORT_FUNCTIONS = {"sort_network", "add_topo_sort"}
FILL_CASES = [
    c
    for c in MANIFEST["cases"]
    if c["function"] in SORT_FUNCTIONS and c.get("r_only") in OUTLET_FILL
]


def _outlet_as_text(layer: pd.DataFrame) -> pd.DataFrame:
    """Read R's integer outlet ``"0"`` in the toid column as ``""`` (I-5)."""
    out = layer.copy()
    names = dict(zip(align_names(out).columns, out.columns, strict=True))
    ids = set(out[names["id"]].astype(str))
    assert "0" not in ids, "an id 0 would make the outlet replacement ambiguous"
    toid = out[names["toid"]].astype(str)
    out[names["toid"]] = toid.where(toid != "0", "")

    return out


def test_fill_cases_are_the_marked_twenty():
    """The parametrized set below is every case the outlet fill keeps R-only."""
    assert len(FILL_CASES) == 20


@pytest.mark.parametrize("case", FILL_CASES, ids=[c["id"] for c in FILL_CASES])
def test_outlet_fill_layers(case):
    """Each R-only layer matches once its outlet is written ``""``."""
    network = read_fixture(case["fixture"])
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", FutureWarning)
        if "prepare" in case:
            prepare = getattr(hydroloom, python_name(case["prepare"]["function"]))
            network = prepare(network, **(case["prepare"].get("args") or {}))
        function = getattr(hydroloom, case["function"])
        result = function(network, **(case.get("args") or {}))

    layer = result_to_layer(result, case, "frame")
    assert_layer_equal(
        layer,
        _outlet_as_text(read_expected(case)),
        compare=case["compare"],
        order=case["order"],
    )


# Interface -----------------------------------------------------------------


def _worked_example() -> pd.DataFrame:
    return read_fixture("add_topo_sort_worked_example.gpkg")


def test_geodataframe_in_geodataframe_out():
    """Geometry follows its row and stays last; the CRS is kept (A-8, G-14)."""
    table = _worked_example()
    net = gpd.GeoDataFrame(
        table,
        geometry=[Point(int(i), 0) for i in table["id"]],
        crs=5070,
    )

    for out in (sort_network(net, split=True), add_topo_sort(net)):
        assert isinstance(out, gpd.GeoDataFrame)
        assert out.crs == net.crs
        assert out.columns[-1] == "geometry"
        assert [p.x for p in out.geometry] == [float(i) for i in out["id"]]


def test_input_names_return():
    """Source names come back; the computed columns keep hydroloom names."""
    table = _worked_example().rename(columns={"id": "COMID", "toid": "ToCOMID"})

    out = sort_network(table, split=True)

    assert list(out.columns) == ["COMID", "ToCOMID", "terminal_id"]


def test_topo_sort_replaces_input_column_under_its_name():
    """A Hydroseq column is replaced by the computed one, as Hydroseq (B-4)."""
    table = _worked_example().assign(Hydroseq=[1.5] * 6)

    out = add_topo_sort(table)

    assert list(out.columns) == ["id", "toid", "Hydroseq"]
    assert out["Hydroseq"].dtype == np.int32
    assert out["Hydroseq"].tolist() == [6, 5, 4, 3, 2, 1]


def test_outlets_as_scalar_or_integers():
    """One outlet, or integer outlets, read as identifiers (I-2)."""
    table = _worked_example()
    want = sort_network(table, outlets=["3"])

    pd.testing.assert_frame_equal(sort_network(table, outlets="3"), want)
    pd.testing.assert_frame_equal(sort_network(table, outlets=[3]), want)
    pd.testing.assert_frame_equal(sort_network(table, outlets=3.0), want)
    pd.testing.assert_frame_equal(sort_network(table, outlets=np.float64(3)), want)


def test_absent_outlets_named_once():
    """Every absent outlet is named, each once (D3-154)."""
    text = msg("outlet_not_in_network", outlets="99, 98")

    with pytest.raises(HydroloomError, match=re.escape(text)):
        sort_network(_worked_example(), outlets=["3", "99", "98", "99"])


def test_cycle_stops_before_outlet_check():
    """The cycle check runs before the outlets check (Algorithm steps 2, 3)."""
    table = pd.DataFrame({"id": [1, 2, 3], "toid": [2, 1, 0]})

    with pytest.raises(HydroloomError, match=re.escape(msg("network_cycle"))):
        sort_network(table, outlets=["99"])


def test_no_topology_raises_representation_error():
    """A table with neither toid nor node columns raises (A-10)."""
    with pytest.raises(RepresentationError):
        add_topo_sort(pd.DataFrame({"id": [1, 2]}))


def test_empty_network_typed():
    """Zero rows return every column typed (I-11)."""
    table = pd.DataFrame({"id": pd.Series([], dtype="int64"), "toid": []})

    out = sort_network(table, split=True)
    assert list(out.columns) == ["id", "toid", "terminal_id"] and out.empty
    topo = add_topo_sort(table)
    assert topo["topo_sort"].dtype == np.int32 and topo.empty


# Properties ----------------------------------------------------------------


@st.composite
def networks(draw, *, divergent: bool):
    """Acyclic id/toid tables with text ids that sort as "10" < "9" (D-1).

    Each flowline drains to one or, with ``divergent``, two flowlines listed
    after it, or to an outlet value: ``""`` or an id outside the table. Only a
    flowline with one downstream connection has an outlet value, so no block
    lands below its in-network downstream flowline (sort_network.md
    Postconditions exception).
    """
    n = draw(st.integers(0, 12))
    labels = draw(
        st.lists(st.integers(1, 120).map(str), min_size=n, max_size=n, unique=True)
    )
    rows = []
    for i, label in enumerate(labels):
        later = labels[i + 1 :]
        k = draw(st.integers(0, min(2 if divergent else 1, len(later))))
        if k == 0:
            rows.append((label, draw(st.sampled_from(["", "x"]))))
            continue
        targets = draw(st.lists(st.sampled_from(later), min_size=k, max_size=k))
        rows.extend((label, t) for t in dict.fromkeys(targets))
    rows = draw(st.permutations(rows))

    return pd.DataFrame(
        {
            "id": pd.Series([r[0] for r in rows], dtype=object),
            "toid": pd.Series([r[1] for r in rows], dtype=object),
            "v": np.arange(len(rows), dtype=np.int64),
        }
    )


def _reference(table: pd.DataFrame, outlets=None) -> list[str]:
    """Return the D-4 order of a network without repeated id (Definition)."""
    ids = set(table["id"])
    up = defaultdict(list)
    for i, t in zip(table["id"], table["toid"], strict=True):
        if t in ids:
            up[t].append(i)
    if outlets is None:
        pairs = zip(table["id"], table["toid"], strict=True)
        outlets = [i for i, t in pairs if t not in ids]
    order: list[str] = []

    def block(f):
        for u in sorted(up[f]):
            block(u)
        order.append(f)

    for outlet in sorted(set(outlets)):
        block(outlet)

    return order


def _terminal(table: pd.DataFrame) -> dict[str, str]:
    toid = dict(zip(table["id"], table["toid"], strict=True))
    out = {}
    for i in toid:
        f = i
        while toid[f] in toid:
            f = toid[f]
        out[i] = f

    return out


def _rows(frame: pd.DataFrame) -> list[tuple]:
    return sorted(map(tuple, frame[["id", "toid", "v"]].to_numpy().tolist()))


def _check_common(table: pd.DataFrame, out: pd.DataFrame) -> None:
    ids = out["id"].tolist()
    # Every output row is an input row; with outlets None, every row returns.
    assert _rows(out) == _rows(table)
    # Rows sharing an id are contiguous and in toid byte order.
    first = {i: ids.index(i) for i in ids}
    for i, group in out.groupby("id", sort=False):
        positions = group.index.to_numpy()
        assert positions.tolist() == list(range(first[i], first[i] + len(group)))
        assert group["toid"].tolist() == sorted(group["toid"])
    # Every flowline comes after every flowline upstream of it.
    for i, t in zip(out["id"], out["toid"], strict=True):
        if t in first:
            assert first[i] < first[t]


@PROPERTY
@given(networks(divergent=False), st.randoms(use_true_random=False))
def test_sort_network_dendritic(table, rng):
    """D-4 order, split, outlets, idempotence, and D-7 without re-sorting."""
    out = sort_network(table)
    _check_common(table, out)
    assert out["id"].tolist() == _reference(table)

    split = sort_network(table, split=True)
    pd.testing.assert_frame_equal(split.drop(columns="terminal_id"), out)
    terminal = _terminal(table)
    assert split["terminal_id"].tolist() == [terminal[i] for i in split["id"]]

    pd.testing.assert_frame_equal(sort_network(out), out)
    shuffled = table.sample(frac=1, random_state=rng.randint(0, 2**31))
    pd.testing.assert_frame_equal(sort_network(shuffled), out)

    roots = sorted(set(terminal.values()))
    chosen = [r for r in roots if rng.random() < 0.5]
    part = sort_network(table, outlets=chosen[::-1])
    assert part["id"].tolist() == _reference(table, chosen)


@PROPERTY
@given(networks(divergent=True), st.randoms(use_true_random=False))
def test_sort_network_flownetwork(table, rng):
    """Upstream before downstream across divergences; D-7 without re-sorting."""
    out = sort_network(table)
    _check_common(table, out)

    outlets = set(table.loc[~table["toid"].isin(table["id"]), "id"])
    split = sort_network(table, split=True)
    pd.testing.assert_frame_equal(split.drop(columns="terminal_id"), out)
    assert set(split["terminal_id"]) <= outlets
    assert not split[["id", "terminal_id"]].drop_duplicates()["id"].duplicated().any()

    pd.testing.assert_frame_equal(sort_network(out), out)
    shuffled = table.sample(frac=1, random_state=rng.randint(0, 2**31))
    pd.testing.assert_frame_equal(sort_network(shuffled), out)


@PROPERTY
@given(networks(divergent=True), st.randoms(use_true_random=False))
def test_add_topo_sort_postconditions(table, rng):
    """Values k down to 1 in output order, one per id, above each downstream."""
    out = add_topo_sort(table)
    k = table["id"].nunique()

    assert out["topo_sort"].dtype == np.int32
    pd.testing.assert_frame_equal(out.drop(columns="topo_sort"), sort_network(table))
    assert sorted(set(out["topo_sort"])) == list(range(1, k + 1))
    assert (np.diff(out["topo_sort"].to_numpy()) <= 0).all()
    assert not out[["id", "topo_sort"]].drop_duplicates()["id"].duplicated().any()
    value = dict(zip(out["id"], out["topo_sort"], strict=True))
    for i, t in zip(out["id"], out["toid"], strict=True):
        if t in value:
            assert value[i] > value[t]
    if k:
        assert out["topo_sort"].iloc[-1] == 1
    pd.testing.assert_frame_equal(add_topo_sort(out), out)

    # With outlets given (add_topo_sort.md Postconditions).
    ids = set(table["id"])
    outlets = [i for i in sorted(ids) if rng.random() < 0.3]
    if not outlets:
        return
    part = add_topo_sort(table, outlets=outlets)
    present = set(part["id"])
    down = defaultdict(set)
    up = defaultdict(set)
    for i, t in zip(table["id"], table["toid"], strict=True):
        if t in ids:
            down[i].add(t)
            up[t].add(i)
    # Every output id drains to one of outlets along toid.
    reach, frontier = set(outlets), list(outlets)
    while frontier:
        for u in up[frontier.pop()] - reach:
            reach.add(u)
            frontier.append(u)
    assert set(outlets) <= present <= reach
    # Present only when every downstream flowline is present; a named
    # flowline is a start and is exempt (sort_network.md Edge cases, 9
    # diverging to 2 and 3 with outlets 9 and 1).
    for i in present - set(outlets):
        assert down[i] <= present
    last = part.iloc[-1]
    assert last["id"] in outlets or last["toid"] not in ids
    assert last["topo_sort"] == 1
    m = len(present)
    assert sorted(set(part["topo_sort"])) == list(range(1, m + 1))


def _upstream_csr(downstream: list[list[int]]):
    """Upstream CSR of a dense graph; neighbors ascending (D-5)."""
    n = len(downstream)
    up: list[list[int]] = [[] for _ in range(n)]
    for u, targets in enumerate(downstream):
        for d in targets:
            up[d].append(u)
    indptr = np.zeros(n + 1, dtype=np.int64)
    indptr[1:] = np.cumsum([len(x) for x in up])
    indices = np.asarray([u for x in up for u in sorted(x)], dtype=np.int64)

    return indptr, indices


def _check_walk(downstream, extra, starts) -> None:
    """Each walk visits a node once, after its downstream nodes in that walk."""
    indptr, indices = _upstream_csr(downstream)
    countdown = np.asarray(
        [len(d) + e for d, e in zip(downstream, extra, strict=True)], dtype=np.int64
    )
    visits, walks = walk.topo_walk(
        indptr, indices, countdown, np.asarray(starts, dtype=np.int64)
    )

    for w in range(len(starts)):
        seen = visits[walks == w].tolist()
        assert seen[0] == starts[w]
        assert len(seen) == len(set(seen))
        position = {v: p for p, v in enumerate(seen)}
        for v in seen[1:]:
            assert any(d in position for d in downstream[v])
            for d in downstream[v]:
                if d in position:
                    assert position[d] < position[v]


def test_topo_walk_braid_above_nested_start():
    """A braid above an inner start is visited once in the later walk (step 5).

    Node 2 diverges to 5 and 1, 5 and 4 drain to 3, 3 to 1, and 1 to 0.
    The walk from 0 spends 2's countdown; the walk from 1 reaches 2 from both
    1 and 5 and must visit it once, after both. Before the step 5 rule the
    walk visited 2 twice, the second time after 4.
    """
    downstream = [[], [0], [5, 1], [1], [3], [3]]
    _check_walk(downstream, [0] * 6, [0, 1])


@PROPERTY
@given(st.data())
def test_topo_walk_visit_rule(data):
    """sort_network.md step 5 visit rule on random acyclic graphs."""
    n = data.draw(st.integers(1, 12))
    downstream = [
        sorted(
            data.draw(st.sets(st.integers(i + 1, n - 1), max_size=2))
            if i < n - 1
            else set()
        )
        for i in range(n)
    ]
    extra = [data.draw(st.integers(0, 1)) for _ in range(n)]
    starts = data.draw(st.lists(st.integers(0, n - 1), min_size=1, unique=True))
    _check_walk(downstream, extra, starts)
