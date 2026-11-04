"""Permutation invariance over the conformance fixtures (SPEC D-7, VERIFICATION §4).

No computed value depends on input row order. Each manifest case that Python runs,
reads a fixture, and is not a raise case is run on the prepared fixture as the
harness reads it, and on the same rows reordered (index reset, so only row order
changes): once reversed, and by hypothesis-drawn permutations. Both returns go
through the harness converter (``result_to_layer``) and are compared with
``assert_layer_equal``, exactly, so no tolerance hides a difference. A violation
fails the test; per the harness rule it becomes a fixture and a manifest case.

- Frames from functions that do not sort: both layers sorted by ``id``, the
  manifest ``key``, then the other non-float columns (``toid`` where ``id``
  repeats), the D-7 sort. Where the return has one row per input row, the
  shuffled layer must also equal the unshuffled layer taken in the permutation's
  order, since a non-sorting function keeps input row order (D-14). Every such
  function reaches that check for at least one case, asserted at collection.
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
from collections.abc import Callable
from typing import Any

import numpy as np
import pandas as pd
import pytest
from hypothesis import given, settings
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

CASES = [
    c
    for c in MANIFEST["cases"]
    if "fixture" in c
    and "raises" not in c
    and skip_reason(c, hydroloom.__all__) is None
]
CASES_BY_ID = {c["id"]: c for c in CASES}

# Non-sorting frame functions that never reach the D-14 row-order check, with the
# reason their frames are not row-mapped. Empty: every one reaches it.
NOT_ROW_MAPPED: dict[str, str] = {}


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


@functools.cache
def _fixture(case_id: str) -> pd.DataFrame:
    return read_fixture(CASES_BY_ID[case_id]["fixture"])


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
    return _prepare(CASES_BY_ID[case_id], _fixture(case_id).copy())


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


def _target(case: dict[str, Any]) -> Callable[..., Any]:
    return getattr(hydroloom, python_name(case["function"]))


def _layer(
    case: dict[str, Any], network: Any, target: Callable[..., Any] | None = None
) -> pd.DataFrame:
    spec = MANIFEST["functions"][case["function"]]
    target = target or _target(case)
    result = target(network, **(case.get("args") or {}))

    return result_to_layer(result, case, spec["returns"])


def _unshuffled_input(case_id: str) -> Any:
    network = _network(case_id)
    if isinstance(network, pd.DataFrame):
        network = network.copy()

    return network


@functools.cache
def _base_layer(case_id: str) -> pd.DataFrame:
    return _layer(CASES_BY_ID[case_id], _unshuffled_input(case_id))


def _row_mapped(case: dict[str, Any], base: pd.DataFrame) -> bool:
    """Whether a non-sorting frame return has one row per input row (D-14)."""
    spec = MANIFEST["functions"][case["function"]]

    return (
        case["function"] not in ORDERED
        and spec["returns"] == "frame"
        and isinstance(_network(case["id"]), pd.DataFrame)
        and len(base) == _rows(case)
    )


def _check(
    case: dict[str, Any], perm: list[int], target: Callable[..., Any] | None = None
) -> None:
    """Assert the case's return is invariant to input rows in ``perm`` order (D-7).

    ``target`` replaces the case's function, for the self-check.
    """
    actual = _layer(case, _shuffled(case, perm), target)
    if target is None:
        base = _base_layer(case["id"])
    else:
        base = _layer(case, _unshuffled_input(case["id"]), target)
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
    if _row_mapped(case, base):
        # D-14: a non-sorting function keeps input row order.
        expected = base.iloc[perm].reset_index(drop=True)
        assert_layer_equal(
            actual, expected, returns=returns, compare=compare, order="exact"
        )


def _check_row_order_reach() -> None:
    """Every non-sorting frame function reaches the D-14 check for some case."""
    frame_functions = {
        c["function"]
        for c in CASES
        if c["function"] not in ORDERED
        and MANIFEST["functions"][c["function"]]["returns"] == "frame"
    }
    reached = {
        c["function"]
        for c in CASES
        if c["function"] in frame_functions and _row_mapped(c, _base_layer(c["id"]))
    }
    unreached = frame_functions - reached - set(NOT_ROW_MAPPED)
    if unreached:
        raise RuntimeError(
            f"no case reaches the D-14 row-order check for {sorted(unreached)}"
        )


_check_row_order_reach()


def _reversed(n: int) -> list[int]:
    return list(range(n - 1, -1, -1))


def _first_case(predicate: Callable[[dict[str, Any]], bool]) -> dict[str, Any]:
    return next(c for c in CASES if predicate(c))


def test_cases_cover_every_python_function_with_a_fixture():
    """Every exported function with a fixture case is in the property."""
    with_fixture = {
        python_name(c["function"])
        for c in MANIFEST["cases"]
        if "fixture" in c and skip_reason(c, hydroloom.__all__) is None
    }
    assert {python_name(c["function"]) for c in CASES} == with_fixture


def test_shuffled_reorders_rows():
    """``_shuffled`` puts the input rows in a non-identity permutation's order."""
    case = _first_case(
        lambda c: isinstance(_network(c["id"]), pd.DataFrame) and _rows(c) >= 3
    )
    network = _network(case["id"])
    perm = _reversed(len(network))
    shuffled = _shuffled(case, perm)

    assert not shuffled.equals(network.reset_index(drop=True))
    pd.testing.assert_frame_equal(shuffled, network.iloc[perm].reset_index(drop=True))


def _frame_case() -> dict[str, Any]:
    """Return a non-sorting frame case with all columns and one row per input row."""
    return _first_case(
        lambda c: (
            _row_mapped(c, _base_layer(c["id"]))
            and "columns" not in c
            and _rows(c) >= 3
        )
    )


def test_self_check_fails_a_position_column():
    """A column recording input row position fails the D-7 sorted comparison."""
    case = _frame_case()
    real = _target(case)

    def fake(network: Any, **kwargs: Any) -> Any:
        result = real(network, **kwargs)
        return result.assign(topo_sort=np.arange(len(result), dtype=np.int32))

    n = _rows(case)
    _check(case, list(range(n)), fake)
    with pytest.raises(AssertionError):
        _check(case, _reversed(n), fake)


def test_self_check_fails_a_row_order_change():
    """Correct rows in an order not following the input fail the D-14 check."""
    case = _frame_case()
    real = _target(case)

    def fake(network: Any, **kwargs: Any) -> Any:
        result = real(network, **kwargs)
        return result.iloc[::-1].reset_index(drop=True)

    n = _rows(case)
    # A rotation: reversing the input would undo the fake's own reversal.
    perm = [*range(1, n), 0]
    _check(case, list(range(n)), fake)
    # The D-7 sorted comparison alone passes: the rows are right.
    assert_layer_equal(
        _layer(case, _shuffled(case, perm), fake),
        _layer(case, _unshuffled_input(case["id"]), fake),
        key=MANIFEST["functions"][case["function"]].get("key"),
        order="ignore",
    )
    with pytest.raises(AssertionError):
        _check(case, perm, fake)


def test_self_check_fails_input_order_in_a_sorting_function():
    """A sort that follows input row order fails the unsorted D-4 comparison."""
    tag = "_input_row"

    def eligible(c: dict[str, Any]) -> bool:
        network = _network(c["id"])
        if (
            c["function"] != "sort_network"
            or "columns" in c
            or not isinstance(network, pd.DataFrame)
            or len(network) < 3
        ):
            return False
        tagged = network.assign(**{tag: np.arange(len(network))})
        result = _target(c)(tagged, **(c.get("args") or {}))
        return tag in result.columns and len(result) == len(network)

    case = _first_case(eligible)
    real = _target(case)

    def fake(network: Any, **kwargs: Any) -> Any:
        tagged = network.assign(**{tag: np.arange(len(network))})
        result = real(tagged, **kwargs)
        return (
            result.sort_values(tag, kind="stable")
            .drop(columns=tag)
            .reset_index(drop=True)
        )

    n = _rows(case)
    _check(case, list(range(n)), fake)
    with pytest.raises(AssertionError):
        _check(case, _reversed(n), fake)


PARAMS = [pytest.param(c, id=c["id"]) for c in CASES]


@pytest.mark.parametrize("case", PARAMS)
def test_permutation_invariance_reversed(case):
    """Input rows in reverse order give the same values as the fixture order (D-7)."""
    _check(case, _reversed(_rows(case)))


@pytest.mark.parametrize("case", PARAMS)
@PROPERTY
@given(data=st.data())
def test_permutation_invariance(case, data):
    """Shuffled input rows give the same values as the fixture order (D-7)."""
    perm = data.draw(_permutations(_rows(case)), label="permutation")
    _check(case, perm)
