"""Permutation invariance over the conformance fixtures (SPEC D-7, VERIFICATION §4).

No computed value depends on input row order. Each manifest case that Python runs,
reads a fixture, and is not a raise case is run twice: on the prepared fixture as
the harness reads it, and on the same rows shuffled by a hypothesis-drawn
permutation (index reset, so only row order changes). Both returns go through the
harness converter (``result_to_layer``) and are compared with
``assert_layer_equal``, exactly, so no tolerance hides a difference.

- Frames from functions that do not sort: both layers sorted by ``id``, the
  manifest ``key``, then the other non-float columns (``toid`` where ``id``
  repeats), the D-7 sort. Where the case is ``order: exact`` and the return has
  one row per input row, the shuffled layer must also equal the unshuffled layer
  taken in the permutation's order, since a non-sorting function keeps input row
  order (D-14).
- ``sort_network``, ``add_topo_sort``: output order is contract (D-4), so the
  layers compare unsorted.
- ``navigate_network_dfs``: the traversal layer holds identifiers with their
  ``start``, ``path`` and ``order`` (S-7, D-6); it compares on those columns, so
  visit order binds.
- ``make_index_ids``: the graph layer maps CSR positions back to identifiers
  (S-7); dense positions are not contract, neighbor order is (D-5), so the
  layers compare on the manifest key ``id, position, neighbor``.
- Scalars compare as their one-row layer.
"""

from __future__ import annotations

import functools
from typing import Any

import numpy as np
import pandas as pd
import pytest
from hypothesis import find, given, settings
from hypothesis import strategies as st

import hydroloom
from conformance import (
    assert_layer_equal,
    load_manifest,
    python_name,
    read_fixture,
    result_to_layer,
    skip_reason,
)

PROPERTY = settings(derandomize=True, max_examples=10, deadline=None)

MANIFEST = load_manifest()
ORDERED = {"sort_network", "add_topo_sort", "navigate_network_dfs"}

# Cases where the property finds a D-7 violation: case id -> reason. Each is a
# BACKLOG row; the function is not changed here.
VIOLATIONS: dict[str, str] = {}

CASES = [
    c
    for c in MANIFEST["cases"]
    if "fixture" in c
    and "raises" not in c
    and skip_reason(c, hydroloom.__all__) is None
]


@st.composite
def _permutations(draw: Any, n: int) -> list[int]:
    """Draw a permutation of ``range(n)``: an optional full shuffle, then swaps.

    ``st.permutations`` draws one choice per row, too large a draw for the
    thousand-row fixtures. A seeded shuffle reaches every order; the swaps let a
    failure shrink to no shuffle and the fewest transpositions that still fail.
    """
    order = np.arange(n)
    seed = draw(st.none() | st.integers(0, 2**32 - 1), label="shuffle seed")
    if seed is not None:
        order = np.random.default_rng(seed).permutation(n)
    if n > 1:
        row = st.integers(0, n - 1)
        swaps = draw(st.lists(st.tuples(row, row), max_size=8), label="swaps")
        for i, j in swaps:
            order[[i, j]] = order[[j, i]]

    return order.tolist()


def _param(case: dict[str, Any]) -> Any:
    marks = []
    if case["id"] in VIOLATIONS:
        marks.append(pytest.mark.xfail(strict=True, reason=VIOLATIONS[case["id"]]))

    return pytest.param(case, id=case["id"], marks=marks)


@functools.cache
def _fixture(case_id: str) -> pd.DataFrame:
    case = next(c for c in CASES if c["id"] == case_id)

    return read_fixture(case["fixture"])


def _prepare(case: dict[str, Any], network: pd.DataFrame) -> Any:
    """Apply the case's ``prepare`` as the harness does."""
    if "prepare" in case:
        prepare = case["prepare"]
        fn = getattr(hydroloom, python_name(prepare["function"]))
        network = fn(network, **(prepare.get("args") or {}))

    return network


@functools.cache
def _network(case_id: str) -> Any:
    """Return the function's input for a case: the fixture after ``prepare``."""
    case = next(c for c in CASES if c["id"] == case_id)

    return _prepare(case, _fixture(case_id).copy())


def _shuffled(case: dict[str, Any], perm: list[int]) -> Any:
    """Return the function's input with its rows in ``perm`` order.

    A frame input is shuffled after ``prepare``. An ``IndexIds`` input has no rows
    of its own, so the fixture rows are shuffled before ``prepare`` builds it.
    """
    network = _network(case["id"])
    if isinstance(network, pd.DataFrame):
        return network.iloc[perm].reset_index(drop=True)
    fixture = _fixture(case["id"]).iloc[perm].reset_index(drop=True)

    return _prepare(case, fixture)


def _rows(case: dict[str, Any]) -> int:
    network = _network(case["id"])
    if isinstance(network, pd.DataFrame):
        return len(network)

    return len(_fixture(case["id"]))


def _layer(case: dict[str, Any], network: Any) -> pd.DataFrame:
    spec = MANIFEST["functions"][case["function"]]
    target = getattr(hydroloom, python_name(case["function"]))
    result = target(network, **(case.get("args") or {}))

    return result_to_layer(result, case, spec["returns"])


@functools.cache
def _base_layer(case_id: str) -> pd.DataFrame:
    case = next(c for c in CASES if c["id"] == case_id)
    network = _network(case_id)
    if isinstance(network, pd.DataFrame):
        network = network.copy()

    return _layer(case, network)


def test_cases_cover_every_python_function_with_a_fixture():
    """Every exported function with a fixture case is in the property."""
    with_fixture = {
        python_name(c["function"])
        for c in MANIFEST["cases"]
        if "fixture" in c and skip_reason(c, hydroloom.__all__) is None
    }
    assert {python_name(c["function"]) for c in CASES} == with_fixture


def test_property_detects_an_order_dependent_value():
    """A column that records input row position fails the D-7 comparison."""
    frame = pd.DataFrame({"id": ["a", "b", "c"], "toid": ["b", "c", ""]})
    rows = find(_permutations(3), lambda p: p != sorted(p))
    base = frame.assign(topo_sort=np.arange(3, dtype=np.int32))
    shuffled = frame.iloc[rows].reset_index(drop=True)
    actual = shuffled.assign(topo_sort=np.arange(3, dtype=np.int32))
    with pytest.raises(AssertionError):
        assert_layer_equal(actual, base, order="ignore")


@pytest.mark.parametrize("case", [_param(c) for c in CASES])
@PROPERTY
@given(data=st.data())
def test_permutation_invariance(case, data):
    """Shuffled input rows give the same values as the fixture order (D-7)."""
    n = _rows(case)
    perm = data.draw(_permutations(n), label="permutation")
    shuffled = _shuffled(case, perm)

    actual = _layer(case, shuffled)
    base = _base_layer(case["id"])
    spec = MANIFEST["functions"][case["function"]]
    returns = spec["returns"]
    compare = "values" if case["compare"] == "values" else "exact"

    if case["function"] in ORDERED:
        key = ["start", "path", "order"] if returns == "traversal" else None
        assert_layer_equal(
            actual, base, returns=returns, key=key, compare=compare, order="exact"
        )
        return

    assert_layer_equal(
        actual,
        base,
        returns=returns,
        key=spec.get("key"),
        compare=compare,
        order="ignore",
    )
    row_mapped = isinstance(_network(case["id"]), pd.DataFrame) and len(base) == n
    if returns == "frame" and case["order"] == "exact" and row_mapped:
        # D-14: a non-sorting function keeps input row order.
        expected = base.iloc[perm].reset_index(drop=True)
        assert_layer_equal(
            actual, expected, returns=returns, compare=compare, order="exact"
        )
