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
    front = path.read_text(encoding="utf-8").split("---")[1]

    return yaml.safe_load(front)["requires"]


@pytest.mark.parametrize("name", sorted(hydroloom.__all__))
def test_registry_matches_requires(name):
    """Each public function's registered needs equal its ``requires`` (A-4)."""
    pattern = _requires(R_NAMES.get(name, name))["pattern"]
    needs = {need.value for need in _REGISTRY[name].needs}

    assert needs == (set() if pattern == ["any"] else set(pattern))


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
