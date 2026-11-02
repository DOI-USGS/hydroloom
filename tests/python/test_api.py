"""The public API equals the manifest's Python-supported functions (SPEC A-18)."""

import hydroloom
from conformance import load_manifest, python_functions


def test_all_matches_manifest():
    """``hydroloom.__all__`` is the set of manifest functions Python supports."""
    exported = list(hydroloom.__all__)

    assert len(exported) == len(set(exported))
    assert set(exported) == python_functions(load_manifest())
