"""make_index_ids and IndexIds: interface tests and properties.

Properties follow the Postconditions of make_index_ids.md and K-3; the
semantics the manifest compares are not repeated here (S-7).
"""

from __future__ import annotations

import re

import numpy as np
import pandas as pd
import pytest
from hypothesis import given, settings
from hypothesis import strategies as st

from conformance import graph_to_layer
from hydroloom import make_index_ids
from hydroloom.errors import HydroloomError, RepresentationError, msg
from hydroloom.index_ids import IndexIds

PROPERTY = settings(derandomize=True, max_examples=60, deadline=None)


@st.composite
def networks(draw):
    """Acyclic networks; each flowline flows to later ones, some to two.

    The outlet value 0 is never an id; with ``downmain`` and ``upmain`` the
    first downstream row of a flowline is its main path.
    """
    n = draw(st.integers(0, 10))
    ids = draw(st.lists(st.integers(1, 10**6), min_size=n, max_size=n, unique=True))
    rows = []
    for i, ident in enumerate(ids):
        later = ids[i + 1 :]
        k = draw(st.integers(0, min(2, len(later))))
        targets = draw(st.permutations(later))[:k] if k else [0]
        rows += [(ident, t, j == 0) for j, t in enumerate(targets)]
    table = pd.DataFrame(rows, columns=["id", "toid", "downmain"]).astype(
        {"id": "int64", "toid": "int64"}
    )
    if draw(st.booleans()):
        table["upmain"] = ~table.duplicated("toid")
    else:
        table = table.drop(columns="downmain")

    return table


def _edges(index):
    owner = np.repeat(np.arange(len(index.uniques)), np.diff(index.indptr))
    return sorted(zip(index.uniques[owner], index.uniques[index.indices], strict=True))


def _check_index(index, table, mode):
    n = len(index.uniques)

    assert isinstance(index, IndexIds)
    assert index.mode == mode
    assert index.indptr.dtype == np.int64 and index.indices.dtype == np.int64
    assert len(index.indptr) == n + 1 and index.indptr[0] == 0
    assert (np.diff(index.indptr) >= 0).all()
    assert index.indptr[-1] == len(index.indices)
    assert ((index.indices >= 0) & (index.indices < n)).all()
    assert index.uniques.is_unique
    assert set(index.uniques) == {str(v) for v in table["id"]}
    assert index.edge_id is None
    assert index.main is None or len(index.main) == len(index.indices)
    for i in range(n):
        span = index.indices[index.indptr[i] : index.indptr[i + 1]]
        neighbors = list(index.uniques[span])
        assert neighbors == sorted(neighbors)


@PROPERTY
@given(networks(), st.randoms(use_true_random=False))
def test_make_index_ids_postconditions(table, rng):
    """Index invariants, the edge set, both modes, transpose, and row order."""
    to, up = make_index_ids(table, mode="both")
    ids = {str(v) for v in table["id"]}
    pairs = {(str(a), str(b)) for a, b in zip(table["id"], table["toid"], strict=True)}

    _check_index(to, table, "to")
    _check_index(up, table, "from")
    assert _edges(to) == sorted(p for p in pairs if p[1] in ids)
    assert _edges(up) == sorted((b, a) for a, b in _edges(to))
    assert (to.main is None) == ("downmain" not in table.columns)
    assert (up.main is None) == ("upmain" not in table.columns)

    alone = make_index_ids(table, mode="to"), make_index_ids(table, mode="from")
    for got, want in zip((to, up), alone, strict=True):
        np.testing.assert_array_equal(got.indptr, want.indptr)
        np.testing.assert_array_equal(got.indices, want.indices)
    flipped = to.transpose()
    assert flipped.mode == "from"
    np.testing.assert_array_equal(flipped.indptr, up.indptr)
    np.testing.assert_array_equal(flipped.indices, up.indices)

    shuffled = table.sample(frac=1, random_state=rng.randint(0, 2**31))
    key = ["mode", "id", "position"]
    pd.testing.assert_frame_equal(
        graph_to_layer(make_index_ids(shuffled, mode="both"))
        .sort_values(key)
        .reset_index(drop=True),
        graph_to_layer((to, up)).sort_values(key).reset_index(drop=True),
    )


def test_main_follows_downmain_and_upmain():
    """The "to" main is downmain and the "from" main upmain, per edge (D3-43)."""
    table = pd.DataFrame(
        {
            "id": [1, 1, 2, 3, 4],
            "toid": [2, 3, 4, 4, 0],
            "downmain": [True, False, True, True, True],
            "upmain": [True, True, True, False, True],
        }
    )

    to, up = make_index_ids(table, mode="both")
    to_layer = graph_to_layer(to).set_index(["id", "neighbor"])["main"]
    up_layer = graph_to_layer(up).set_index(["id", "neighbor"])["main"]

    assert to_layer.to_dict() == {
        ("1", "2"): True,
        ("1", "3"): False,
        ("2", "4"): True,
        ("3", "4"): True,
    }
    assert up_layer.to_dict() == {
        ("2", "1"): True,
        ("3", "1"): True,
        ("4", "2"): True,
        ("4", "3"): False,
    }


def test_empty_network():
    """Zero rows return an index with no nodes and no edges (I-11)."""
    index = make_index_ids(pd.DataFrame({"id": [], "toid": []}))

    assert index.indptr.tolist() == [0]
    assert len(index.indices) == 0 and len(index.uniques) == 0


def test_mode_invalid_raises_value_error():
    """An unknown mode is a ValueError (make_index_ids.md Preconditions)."""
    with pytest.raises(ValueError, match="mode must be one of"):
        make_index_ids(pd.DataFrame({"id": [1], "toid": [0]}), mode="down")


def test_node_pattern_raises_representation_error():
    """No conversion is registered: a node table raises (needs_topo)."""
    table = pd.DataFrame({"id": [1, 2], "fromnode": [1, 2], "tonode": [2, 3]})

    with pytest.raises(RepresentationError, match=r"representation hy_node"):
        make_index_ids(table)


def test_cycle_raises_hydroloom_error():
    """A cycle stops on either route (D-8, D3-41)."""
    with pytest.raises(HydroloomError, match="cycle"):
        make_index_ids(pd.DataFrame({"id": [1, 2], "toid": [2, 1]}))


def test_ell_and_to_scipy():
    """The padded view pads with -1 (K-4); scipy holds the same graph (K-3)."""
    table = pd.DataFrame({"id": [1, 2, 3], "toid": [3, 3, 0]})

    up = make_index_ids(table, mode="from")
    matrix, lengths = up.ell()
    graph = up.to_scipy()

    assert matrix.shape == (3, 2)
    assert lengths.tolist() == [0, 0, 2]
    assert matrix[0].tolist() == [-1, -1]
    assert graph.shape == (3, 3) and graph.nnz == 2
    assert graph.indices.tolist() == up.indices.tolist()


def test_mode_checked_before_routing():
    """A bad mode is a ValueError on any table, a node table included (S-18)."""
    node = pd.DataFrame({"id": [1, 2], "fromnode": [1, 2], "tonode": [2, 3]})

    for table in (node, pd.DataFrame({"name": ["a"]})):
        with pytest.raises(
            ValueError, match=re.escape(msg("index_mode_invalid"))
        ) as info:
            make_index_ids(table, mode="down")
        assert not isinstance(info.value, RepresentationError)


@pytest.mark.parametrize(
    ("ids", "rows"),
    [([1.0, np.nan, 3.0], "2"), ([1.0, np.nan, np.nan], "2, 3")],
    ids=["one-missing", "two-missing"],
)
def test_missing_id_raises_before_the_index(ids, rows):
    """A missing id raises id_missing at ingest; no row is dropped (B-1, SP-07)."""
    table = pd.DataFrame({"id": ids, "toid": [3, 3, 0]})
    text = msg("id_missing", column="id", rows=rows)

    with pytest.raises(HydroloomError, match=re.escape(text)):
        make_index_ids(table)


def test_index_ids_compare_by_identity():
    """``==``, ``hash()``, and ``in`` are object identity (S-17)."""
    table = pd.DataFrame({"id": [1, 2], "toid": [2, 0]})
    first, second = make_index_ids(table), make_index_ids(table)

    assert first == first
    assert first != second
    assert first in [second, first]
    assert second not in [first]
    assert len({first, second}) == 2
