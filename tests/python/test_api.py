"""The public API equals the manifest's Python-supported functions (SPEC A-18)."""

import doctest

import pytest
import yaml

import hydroloom
from conformance import CONFORMANCE_DIR, PYTHON_NAMES, load_manifest, python_functions
from hydroloom._hy import _REGISTRY

R_NAMES = {python: r for r, python in PYTHON_NAMES.items()}


def test_all_matches_manifest():
    """``hydroloom.__all__`` is the set of manifest functions Python supports."""
    exported = list(hydroloom.__all__)

    assert len(exported) == len(set(exported))
    assert set(exported) == python_functions(load_manifest())


def _requires(function):
    path = CONFORMANCE_DIR / "algorithms" / f"{function}.md"
    front = path.read_text(encoding="utf-8").split("---\n", maxsplit=2)[1]

    return yaml.safe_load(front)["requires"]


@pytest.mark.parametrize("name", sorted(hydroloom.__all__))
def test_registry_matches_requires(name):
    """Each public function's registered needs equal its ``requires`` (A-4).

    ``pattern``, ``columns``, and ``via`` are compared. ``via`` is the
    registry field `capabilities()` will name, distinct from ``convert``:
    `make_index_ids` stores ``{hy_node: add_toids}`` and registers no
    ``convert``. A function whose pattern is ``[none]`` takes no network, so
    it has no capability to report and is not registered.
    """
    requires = _requires(R_NAMES.get(name, name))
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
    func = getattr(hydroloom, name)
    tests = doctest.DocTestFinder().find(func, name, globs={})
    runner = doctest.DocTestRunner()
    for test in tests:
        runner.run(test)

    assert tests and tests[0].examples
    assert runner.summarize(verbose=False).failed == 0
