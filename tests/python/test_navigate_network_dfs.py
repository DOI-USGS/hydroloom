"""navigate_network_dfs: interface and Postcondition properties.

Semantics live in the manifest (S-7); the tests here check the interface the
description's Signature and Preconditions give and the Postconditions of
navigate_network_dfs.md as properties over random acyclic networks.
"""

from __future__ import annotations

import re
import warnings
from collections import defaultdict

import numpy as np
import pandas as pd
import pytest
from hypothesis import given, settings
from hypothesis import strategies as st

from conformance import read_fixture, traversal_to_layer
from hydroloom import add_toids, make_index_ids, navigate_network_dfs
from hydroloom._kernels import sequential
from hydroloom.errors import HydroloomError, RepresentationError, msg

PROPERTY = settings(derandomize=True, max_examples=100, deadline=None)


def _tie() -> pd.DataFrame:
    return read_fixture("dfs_tie.gpkg")


# Interface -----------------------------------------------------------------


def test_returns_lists_of_text_indexes():
    """A list per start, a list of paths each, paths an Index of str (A-16)."""
    result = navigate_network_dfs(_tie(), ["1"])

    assert isinstance(result, list) and len(result) == 1
    assert isinstance(result[0], list)
    for path in result[0]:
        assert isinstance(path, pd.Index)
        assert path.dtype == object
        assert all(isinstance(v, str) for v in path)


def test_starts_compare_as_text():
    """A scalar or whole-number start reads as its decimal string (I-1)."""
    want = navigate_network_dfs(_tie(), ["1"])

    for starts in ("1", 1, [1], [1.0], np.array([1])):
        got = navigate_network_dfs(_tie(), starts)
        assert [[p.tolist() for p in e] for e in got] == [
            [p.tolist() for p in e] for e in want
        ]


@pytest.mark.parametrize("starts", [["99"], [None], [1.5], ["1", "99"]])
def test_start_not_in_network(starts):
    """A start that is missing or not an id raises (Preconditions)."""
    with pytest.raises(HydroloomError, match=re.escape(msg("start_not_in_network"))):
        navigate_network_dfs(_tie(), starts)


def test_direction_invalid():
    """A direction outside the four raises ValueError (Preconditions)."""
    text = msg("direction_invalid", direction="sideways")

    with pytest.raises(ValueError, match=re.escape(text)):
        navigate_network_dfs(_tie(), ["1"], direction="sideways")


@pytest.mark.parametrize("network", [[1, 2], None, {"id": [1]}])
def test_network_not_frame_or_index_ids(network):
    """Input neither a frame nor an IndexIds raises (Preconditions)."""
    text = msg("network_not_frame_or_index_ids")

    with pytest.raises(HydroloomError, match=re.escape(text)):
        navigate_network_dfs(network, ["1"])


def test_index_pair_is_not_an_index():
    """The mode "both" pair is not an IndexIds input."""
    pair = make_index_ids(_tie(), mode="both")

    with pytest.raises(HydroloomError):
        navigate_network_dfs(pair, ["1"])


def test_index_direction_checked_before_starts():
    """The index direction check runs before the start check (Preconditions)."""
    index = make_index_ids(_tie(), mode="to")
    text = msg("index_ids_direction_mismatch", needed="down", mode="to")

    with pytest.raises(HydroloomError, match=re.escape(text)):
        navigate_network_dfs(index, ["99"], direction="up")


def _node_table() -> pd.DataFrame:
    return pd.DataFrame(
        {
            "id": [1, 2, 3, 4],
            "fromnode": [10, 20, 20, 30],
            "tonode": [20, 30, 40, 50],
        }
    )


@pytest.mark.parametrize("direction", ["down", "up"])
def test_node_table_converts_silently(direction):
    """A node table walked down or up converts with no message (Preconditions)."""
    node = _node_table()
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", FutureWarning)
        topo = add_toids(node, return_dendritic=False)
    start = ["1"] if direction == "down" else ["4"]

    with warnings.catch_warnings():
        warnings.simplefilter("error")
        got = navigate_network_dfs(node, start, direction=direction)
    want = navigate_network_dfs(topo, start, direction=direction)

    assert [[p.tolist() for p in e] for e in got] == [
        [p.tolist() for p in e] for e in want
    ]


@pytest.mark.parametrize("direction", ["downmain", "upmain"])
def test_node_table_main_direction_needs_flownetwork(direction):
    """A node table in a main direction raises node_main_needs_flownetwork.

    Its route is to_flownetwork, which Python does not have yet (Remedies).
    """
    text = msg(
        "node_main_needs_flownetwork",
        function="navigate_network_dfs",
        direction=direction,
    )

    with pytest.raises(RepresentationError, match=re.escape(text)):
        navigate_network_dfs(_node_table(), ["1"], direction=direction)


def test_no_topology_raises_representation_error():
    """A table with neither toid nor node columns raises (A-10)."""
    with pytest.raises(RepresentationError):
        navigate_network_dfs(pd.DataFrame({"id": [1, 2]}), ["1"])


def test_cycle_stops_before_start_check():
    """make_index_ids raises on a cycle before the starts are read."""
    table = pd.DataFrame({"id": [1, 2, 3], "toid": [2, 1, 0]})

    with pytest.raises(HydroloomError, match=re.escape(msg("network_cycle"))):
        navigate_network_dfs(table, ["99"])


def test_main_path_required_before_start_check():
    """A main direction with no main columns raises before the start check."""
    table = pd.DataFrame({"id": [1, 2], "toid": [2, 0]})
    text = msg("main_path_required", direction="downmain")

    with pytest.raises(HydroloomError, match=re.escape(text)):
        navigate_network_dfs(table, ["99"], direction="downmain")


# Properties ----------------------------------------------------------------


@st.composite
def networks(draw):
    """Acyclic id/toid tables with text ids that sort as "10" < "9" (D-1).

    Each flowline drains to up to two flowlines listed after it, or to an
    outlet value (``""`` or an id outside the table); a flowline with an
    in-network toid may also carry an outside one (D3-76). ``downmain`` flags
    at most one row per ``id`` and ``upmain`` at most one row per ``toid``.
    """
    n = draw(st.integers(1, 12))
    labels = draw(
        st.lists(st.integers(1, 120).map(str), min_size=n, max_size=n, unique=True)
    )
    rows = []
    for i, label in enumerate(labels):
        later = labels[i + 1 :]
        k = draw(st.integers(0, min(2, len(later))))
        if k == 0:
            rows.append((label, draw(st.sampled_from(["", "x"]))))
            continue
        targets = draw(st.lists(st.sampled_from(later), min_size=k, max_size=k))
        rows.extend((label, t) for t in dict.fromkeys(targets))
        if draw(st.booleans()) and draw(st.booleans()):
            rows.append((label, "x"))
    rows = draw(st.permutations(rows))
    # At most one main downstream connection per flowline, the condition
    # as_hy raises flownetwork_multiple_downmain on.
    downmain = []
    flagged: set[str] = set()
    for label, _ in rows:
        flag = label not in flagged and draw(st.booleans())
        if flag:
            flagged.add(label)
        downmain.append(flag)
    # At most one main upstream connection per toid, the matching invariant
    # for upmain.
    upmain = []
    flagged_to: set[str] = set()
    for _, toid in rows:
        flag = toid not in flagged_to and draw(st.booleans())
        if flag:
            flagged_to.add(toid)
        upmain.append(flag)

    return pd.DataFrame(
        {
            "id": pd.Series([r[0] for r in rows], dtype=object),
            "toid": pd.Series([r[1] for r in rows], dtype=object),
            "downmain": downmain,
            "upmain": upmain,
        }
    )


def _neighbors(table: pd.DataFrame, direction: str) -> dict[str, set[str]]:
    """Neighbors in the walk direction; main connections only when main."""
    ids = set(table["id"])
    out: dict[str, set[str]] = defaultdict(set)
    for i, t, dm, um in zip(
        table["id"], table["toid"], table["downmain"], table["upmain"], strict=True
    ):
        if t not in ids:
            continue
        if direction == "up" or (direction == "upmain" and um):
            out[t].add(i)
        elif direction == "down" or (direction == "downmain" and dm):
            out[i].add(t)

    return out


def _reach(nbr: dict[str, set[str]], start: str, blocked: set[str]) -> set[str]:
    found, frontier = {start}, [start]
    while frontier:
        for v in nbr[frontier.pop()] - found - blocked:
            found.add(v)
            frontier.append(v)

    return found


def _check(table, starts, direction, reset) -> list[list[list[str]]]:
    result = navigate_network_dfs(table, starts, direction=direction, reset=reset)
    elements = [[p.tolist() for p in e] for e in result]
    nbr = _neighbors(table, direction)
    unique = list(dict.fromkeys(starts))

    # One element per unique start, in first-appearance order.
    assert len(elements) == len(unique)
    earlier: set[str] = set()
    for k, (start, element) in enumerate(zip(unique, elements, strict=True)):
        flat = [v for p in element for v in p]
        # No path is empty; empty exactly when the start was already returned.
        assert all(element)
        assert (not element) == (not reset and start in earlier)
        if element:
            assert flat[0] == start
        assert len(flat) == len(set(flat))
        if not reset:
            assert not set(flat) & earlier
            want = set() if start in earlier else _reach(nbr, start, earlier)
        else:
            want = _reach(nbr, start, set())
        assert set(flat) == want

        seen = set() if reset else set(earlier)
        position = {v: i for i, v in enumerate(flat)}
        for i, v in enumerate(flat):
            # Each identifier after the first is a neighbor of an earlier one.
            if i:
                assert any(position.get(u, i) < i for u in nbr if v in nbr[u])
            # D-6: the smallest neighbor, when unvisited, is visited next.
            if nbr[v]:
                first = min(nbr[v])
                if first not in seen:
                    assert flat[i + 1] == first
            seen.add(v)
        for p in element:
            # Within a path, each identifier is a neighbor of the one before.
            for a, b in zip(p, p[1:], strict=False):
                assert b in nbr[a]
            # A path ends at a terminal or where the walk turns to a visited
            # flowline (the smallest neighbor, D-6).
            if not direction.endswith("main"):
                last = p[-1]
                visited_before = set(flat[: position[last]]) | (
                    set() if reset else earlier
                )
                assert not nbr[last] or min(nbr[last]) in visited_before
        if direction.endswith("main"):
            assert len(element) <= 1
        elif k == 0 or reset:
            if all(len(nbr[v]) <= 1 for v in want):
                assert len(element) == 1
        earlier |= set(flat)

    # order and path as the traversal layer stores them (S-7).
    layer = traversal_to_layer(result, unique)
    for _, rows in layer.groupby("start", sort=False):
        assert rows["order"].tolist() == list(range(len(rows)))
        steps = np.diff(rows["path"].to_numpy())
        assert rows["path"].iloc[0] == 0 and set(steps) <= {0, 1}

    return elements


@PROPERTY
@given(
    networks(),
    st.sampled_from(["down", "up", "downmain", "upmain"]),
    st.booleans(),
    st.data(),
)
def test_postconditions(table, direction, reset, data):
    """navigate_network_dfs.md Postconditions, every direction."""
    ids = sorted(set(table["id"]))
    starts = data.draw(st.lists(st.sampled_from(ids), min_size=1, max_size=4))
    elements = _check(table, starts, direction, reset)

    # D-7, un-sorted: a row permutation gives the same result.
    shuffled = table.sample(frac=1, random_state=data.draw(st.integers(0, 2**31)))
    again = navigate_network_dfs(shuffled, starts, direction=direction, reset=reset)
    assert [[p.tolist() for p in e] for e in again] == elements

    # Frame and IndexIds input walk the same graph (Signature).
    mode = "from" if direction.startswith("up") else "to"
    index = make_index_ids(table, mode=mode)
    by_index = navigate_network_dfs(index, starts, direction=direction, reset=reset)
    assert [[p.tolist() for p in e] for e in by_index] == elements


@PROPERTY
@given(st.data())
def test_dfs_paths_shape(data):
    """The kernel's outputs align: path with visit, start_ptr per start (K-6)."""
    n = data.draw(st.integers(1, 10))
    targets = [
        sorted(data.draw(st.sets(st.integers(i + 1, n - 1), max_size=2)))
        if i < n - 1
        else []
        for i in range(n)
    ]
    indptr = np.zeros(n + 1, dtype=np.int64)
    indptr[1:] = np.cumsum([len(t) for t in targets])
    indices = np.asarray([v for t in targets for v in t], dtype=np.int64)
    starts = np.asarray(
        data.draw(st.lists(st.integers(0, n - 1), min_size=1, unique=True)),
        dtype=np.int64,
    )
    use_main = data.draw(st.booleans())
    main = (
        np.asarray(
            data.draw(
                st.lists(st.booleans(), min_size=len(indices), max_size=len(indices))
            ),
            dtype=bool,
        )
        if use_main
        else None
    )
    reset = data.draw(st.booleans())

    visit, path, start_ptr = sequential.dfs_paths(indptr, indices, starts, main, reset)

    for array in (visit, path, start_ptr):
        assert array.dtype == np.int64
    assert len(path) == len(visit)
    assert len(start_ptr) == len(starts) + 1
    assert start_ptr[0] == 0 and start_ptr[-1] == len(visit)
    assert (np.diff(start_ptr) >= 0).all()
    for lo, hi in zip(start_ptr[:-1], start_ptr[1:], strict=True):
        if hi > lo:
            assert path[lo] == 0
            assert set(np.diff(path[lo:hi])) <= {0, 1}
            if main is not None:
                assert (path[lo:hi] == 0).all()
    if not reset:
        assert len(np.unique(visit)) == len(visit)
