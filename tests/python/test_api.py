"""The public API equals the manifest's Python-supported functions (SPEC A-18)."""

import pytest

import hydroloom
from conformance import (
    PYTHON_NAMES,
    load_manifest,
    load_requires,
    python_functions,
    run_doctest,
)
from hydroloom._hy import _REGISTRY

R_NAMES = {python: r for r, python in PYTHON_NAMES.items()}

# Functions whose input is process state, exported with every manifest case
# R-only by design once the Python suite covers each Postcondition (RULES P-3,
# SPEC S-4); tests/python/test_register_names.py covers hydroloom_names.md.
PROCESS_STATE = {"register_names"}


def test_all_matches_manifest():
    """``hydroloom.__all__`` is the manifest's Python functions (A-18, S-4).

    A process-state function (P-3) is exported though its cases are R-only.
    """
    exported = list(hydroloom.__all__)
    manifest = load_manifest()

    assert len(exported) == len(set(exported))
    assert set(exported) == python_functions(manifest) | PROCESS_STATE
    for name in PROCESS_STATE:
        cases = [c for c in manifest["cases"] if c["function"] == R_NAMES[name]]
        assert cases and all("r_only" in c for c in cases), name


@pytest.mark.parametrize("name", sorted(hydroloom.__all__))
def test_registry_matches_requires(name):
    """Each public function's registered needs equal its ``requires`` (A-4).

    ``pattern``, ``columns``, and ``via`` are compared. ``via`` is the
    registry field `capabilities()` will name, distinct from ``convert``:
    `make_index_ids` stores ``{hy_node: add_toids}`` and registers no
    ``convert``. A function whose pattern is ``[none]`` takes no network, so
    it has no capability to report and is not registered.
    """
    requires = load_requires(R_NAMES.get(name, name))
    pattern = requires["pattern"]
    if pattern == ["none"]:
        assert name not in _REGISTRY
        return
    entry = _REGISTRY[name]
    needs = {need.value for need in entry.needs}
    via = {need.value: call for need, call in entry.via.items()}

    assert needs == (set() if pattern == ["any"] else set(pattern))
    assert list(entry.columns) == list(requires["columns"])
    assert via == (requires["via"] or {})


@pytest.mark.parametrize("name", sorted(hydroloom.__all__))
def test_doctest(name):
    """Every public function's hand-written example runs (A-13)."""
    assert run_doctest(getattr(hydroloom, name), name).failed == 0
