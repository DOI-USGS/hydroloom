"""Properties and fixture checks of the CSR kernel (make_index_ids.md, SPEC K-2)."""

import numpy as np
import pandas as pd
import pytest
from hypothesis import given, settings
from hypothesis import strategies as st

from conformance import (
    Graph,
    assert_layer_equal,
    graph_to_layer,
    load_manifest,
    read_expected,
    read_fixture,
)
from hydroloom._kernels import csr

MANIFEST = load_manifest()
KEY = MANIFEST["functions"]["make_index_ids"].get("key")


# Strategies ---------------------------------------------------------------


@st.composite
def graphs(draw, max_n=10, injective=True):
    """Return ``(n, source, target, rank)``.

    ``rank`` is a permutation of n when ``injective``; otherwise any int64 key
    per node, ties and values outside ``[0, n)`` included.
    """
    n = draw(st.integers(0, max_n))
    if n == 0:
        edges = []
    else:
        node = st.integers(0, n - 1)
        edges = draw(st.lists(st.tuples(node, node), max_size=3 * n))
    if injective:
        keys = draw(st.permutations(range(n)))
    else:
        keys = draw(st.lists(st.integers(-(2**31), 2**31), min_size=n, max_size=n))
    rank = np.array(keys, dtype=np.int64)
    source = np.array([e[0] for e in edges], dtype=np.int64)
    target = np.array([e[1] for e in edges], dtype=np.int64)

    return n, source, target, rank


def _owner(indptr):
    return np.repeat(np.arange(len(indptr) - 1), np.diff(indptr))


def _pairs(source, target):
    return sorted(zip(source.tolist(), target.tolist(), strict=True))


PROPERTY = settings(max_examples=200, derandomize=True)

# Properties that hold for any int64 key draw from both; edge-permutation
# invariance and the transpose identities need an injective key, because edges
# whose neighbors tie on rank keep input order (D-3).
ANY_RANK = st.one_of(graphs(), graphs(injective=False))


# Properties from the Postconditions ---------------------------------------


@PROPERTY
@given(ANY_RANK)
def test_indptr_shape(graph):
    """``indptr`` is n + 1 long, starts at 0, never decreases, and ends at m."""
    n, source, target, rank = graph
    indptr, indices, order = csr.from_edges(source, target, n, rank)

    assert indptr.dtype == np.int64 and indices.dtype == np.int64
    assert order.dtype == np.int64
    assert len(indptr) == n + 1
    assert indptr[0] == 0
    assert np.all(np.diff(indptr) >= 0)
    assert indptr[-1] == len(indices) == len(source)
    assert np.all((indices >= 0) & (indices < max(n, 1)))


@PROPERTY
@given(ANY_RANK)
def test_edge_multiset_kept(graph):
    """The CSR holds each input edge once, duplicates included."""
    n, source, target, rank = graph
    indptr, indices, _ = csr.from_edges(source, target, n, rank)

    assert _pairs(_owner(indptr), indices) == _pairs(source, target)


@PROPERTY
@given(ANY_RANK)
def test_neighbors_in_rank_order(graph):
    """Each neighbor list is in rank order (D-5)."""
    n, source, target, rank = graph
    indptr, indices, _ = csr.from_edges(source, target, n, rank)
    owner = _owner(indptr)
    same_row = owner[1:] == owner[:-1]

    assert np.all(np.diff(rank[indices])[same_row] >= 0)


@PROPERTY
@given(ANY_RANK)
def test_node_without_edges_is_empty(graph):
    """A node with no out-edge has an empty range; no sentinel appears (K-4)."""
    n, source, target, rank = graph
    indptr, _, _ = csr.from_edges(source, target, n, rank)
    lonely = np.setdiff1d(np.arange(n), source)

    assert np.all(indptr[lonely] == indptr[lonely + 1])


@PROPERTY
@given(ANY_RANK)
def test_payload_follows_edge(graph):
    """``payload[order]`` lines up each input edge's payload with its CSR slot."""
    n, source, target, rank = graph
    indptr, indices, order = csr.from_edges(source, target, n, rank)

    assert np.array_equal(np.sort(order), np.arange(len(source)))
    assert np.array_equal(source[order], _owner(indptr))
    assert np.array_equal(target[order], indices)


@PROPERTY
@given(graphs(), st.randoms(use_true_random=False))
def test_edge_permutation_invariance(graph, rnd):
    """Shuffling the edges leaves the CSR unchanged (D-7)."""
    n, source, target, rank = graph
    shuffle = np.arange(len(source))
    rnd.shuffle(shuffle)
    payload = source * 1000 + target

    indptr, indices, order = csr.from_edges(source, target, n, rank)
    p_indptr, p_indices, p_order = csr.from_edges(
        source[shuffle], target[shuffle], n, rank
    )

    assert np.array_equal(indptr, p_indptr)
    assert np.array_equal(indices, p_indices)
    assert np.array_equal(payload[order], payload[shuffle][p_order])


@PROPERTY
@given(graphs())
def test_transpose_reverses_edges(graph):
    """The transpose equals the build from reversed edges, payload included (K-3)."""
    n, source, target, rank = graph
    payload = np.arange(len(source))
    indptr, indices, order = csr.from_edges(source, target, n, rank)
    t_indptr, t_indices, t_order = csr.transpose(indptr, indices, rank)
    r_indptr, r_indices, r_order = csr.from_edges(target, source, n, rank)

    assert np.array_equal(t_indptr, r_indptr)
    assert np.array_equal(t_indices, r_indices)
    assert np.array_equal(payload[order][t_order], payload[r_order])


@PROPERTY
@given(graphs())
def test_transpose_twice_is_identity(graph):
    """Transposing twice returns the CSR and the slot order unchanged (K-3)."""
    n, source, target, rank = graph
    indptr, indices, _ = csr.from_edges(source, target, n, rank)
    t_indptr, t_indices, t_order = csr.transpose(indptr, indices, rank)
    tt_indptr, tt_indices, tt_order = csr.transpose(t_indptr, t_indices, rank)

    assert np.array_equal(tt_indptr, indptr)
    assert np.array_equal(tt_indices, indices)
    assert np.array_equal(t_order[tt_order], np.arange(len(indices)))


@PROPERTY
@given(ANY_RANK)
def test_ell_round_trip(graph):
    """The ELL view, padded with -1, converts back to the same CSR (K-2, K-4)."""
    n, source, target, rank = graph
    indptr, indices, _ = csr.from_edges(source, target, n, rank)
    matrix, lengths = csr.ell(indptr, indices)
    width = int(np.diff(indptr).max()) if n else 0

    assert matrix.shape == (n, width) and matrix.dtype == np.int64
    assert np.array_equal(lengths, np.diff(indptr))
    slot = np.arange(width) < lengths[:, None]
    assert np.all(matrix[~slot] == -1)

    back_indptr = np.concatenate([[0], np.cumsum(lengths)])
    assert np.array_equal(back_indptr, indptr)
    assert np.array_equal(matrix[slot], indices)


def test_empty_graph():
    """No nodes and no edges: ``indptr == [0]`` (I-11)."""
    empty = np.array([], dtype=np.int64)
    indptr, indices, order = csr.from_edges(empty, empty, 0, empty)
    t_indptr, t_indices, _ = csr.transpose(indptr, indices, empty)
    matrix, lengths = csr.ell(indptr, indices)

    assert indptr.tolist() == [0] and t_indptr.tolist() == [0]
    assert len(indices) == len(order) == len(t_indices) == 0
    assert matrix.shape == (0, 0) and len(lengths) == 0


def test_one_node():
    """One node and no edges: ``indptr == [0, 0]``."""
    empty = np.array([], dtype=np.int64)
    indptr, indices, _ = csr.from_edges(empty, empty, 1, np.array([0]))

    assert indptr.tolist() == [0, 0] and len(indices) == 0
    assert csr.ell(indptr, indices)[0].shape == (1, 0)


def test_rank_outside_node_range():
    """A rank key outside ``[0, n)`` still groups by owning node (K-2)."""
    indptr, indices, _ = csr.from_edges(
        np.array([0, 1]), np.array([1, 0]), 2, np.array([0, 5])
    )

    assert indptr.tolist() == [0, 1, 2]
    assert indices.tolist() == [1, 0]


def test_worked_example_by_hand():
    """The description's worked example: from 10 lists 100, 30, 4 (D-5)."""
    ids = ["30", "4", "100", "10", "7"]
    rank = np.argsort(
        np.argsort(np.array(ids, dtype=object), kind="stable"), kind="stable"
    )
    source = np.array([0, 1, 2, 3], dtype=np.int64)
    target = np.array([3, 3, 3, 4], dtype=np.int64)

    indptr, indices, _ = csr.from_edges(source, target, 5, rank)
    f_indptr, f_indices, _ = csr.transpose(indptr, indices, rank)

    assert indices.tolist() == [3, 3, 3, 4]
    assert [ids[i] for i in f_indices[f_indptr[3] : f_indptr[4]]] == ["100", "30", "4"]
    matrix, _ = csr.ell(f_indptr, f_indices)
    assert matrix[3].tolist() == [2, 0, 1]
    assert matrix[0].tolist() == [-1, -1, -1]


# Fixture checks against the stored make_index_ids layers ------------------
#
# The caller's work above the boundary (Algorithm steps 4 to 7) is done here in
# the test, so the kernel is checked against the oracle without the public
# function: drop repeated rows, factorize id, map toid, drop outlets, rank the
# identifiers by byte order.


def _case(case_id):
    return next(c for c in MANIFEST["cases"] if c["id"] == case_id)


def _text(series):
    # Whole floats are written as integers, as the harness compares (S-7).
    def one(v):
        if isinstance(v, float | np.floating) and float(v).is_integer():
            return str(int(v))
        return str(v)

    return np.array([one(v) for v in series], dtype=object)


def _index(network, mode):
    cols = ["id", "toid"] + [c for c in ("downmain", "upmain") if c in network]
    rows = pd.DataFrame(network[cols]).drop_duplicates(keep="first")
    ids = _text(rows["id"])
    codes, uniques = pd.factorize(ids)
    uniques = np.asarray(uniques, dtype=object)
    n = len(uniques)
    target = pd.Index(uniques).get_indexer(_text(rows["toid"]))
    keep = target >= 0
    source = codes[keep].astype(np.int64)
    target = target[keep].astype(np.int64)

    rank = np.empty(n, dtype=np.int64)
    rank[np.argsort(uniques, kind="stable")] = np.arange(n)

    def flag(col):
        return rows[col].to_numpy(dtype=bool)[keep] if col in rows else None

    downmain, upmain = flag("downmain"), flag("upmain")
    indptr, indices, order = csr.from_edges(source, target, n, rank)
    to_main = None if downmain is None else downmain[order]
    to = Graph(indptr, indices, uniques, "to", to_main)
    if mode == "to":
        return to

    # "from" by transpose of "to"; upmain follows its edge (D3-43).
    t_indptr, t_indices, t_order = csr.transpose(indptr, indices, rank)
    frm = Graph(t_indptr, t_indices, uniques, "from")
    if upmain is not None:
        frm.main = upmain[order][t_order]

    # The same "from" built directly from the reversed edges.
    r_indptr, r_indices, r_order = csr.from_edges(target, source, n, rank)
    assert np.array_equal(r_indptr, t_indptr)
    assert np.array_equal(r_indices, t_indices)
    if upmain is not None:
        assert np.array_equal(upmain[r_order], frm.main)
    if mode == "from":
        return frm

    return to, frm


FIXTURE_CASES = [
    "make_index_ids_worked_example",
    "make_index_ids_worked_example_permuted",
    "make_index_ids_divergence_order",
    "make_index_ids_return_divergence_main",
    "make_index_ids_isolated_flowline",
    "make_index_ids_single_flowline",
    "make_index_ids_all_terminals",
    "make_index_ids_conflicting_downmain",
    "make_index_ids_conflicting_downmain_from",
    "make_index_ids_type_mismatch",
    "make_index_ids_empty_network",
]


@pytest.mark.parametrize("case_id", FIXTURE_CASES)
def test_fixture_matches_expected_layer(case_id):
    """The kernel's CSR, given the caller's ranking, gives the stored layer."""
    case = _case(case_id)
    network = read_fixture(case["fixture"])
    mode = (case.get("args") or {}).get("mode", "to")

    layer = graph_to_layer(_index(network, mode))

    assert_layer_equal(
        layer,
        read_expected(case),
        returns="graph",
        key=KEY,
        compare=case["compare"],
        order=case["order"],
    )
