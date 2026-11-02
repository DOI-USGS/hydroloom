"""Conformance cases, one pytest item per manifest case (SPEC S-4, S-7)."""

import pytest

import hydroloom
from conformance import load_manifest, run_case, skip_reason

MANIFEST = load_manifest()
CASES = MANIFEST["cases"]


@pytest.mark.parametrize("case", CASES, ids=[c["id"] for c in CASES])
def test_case(case):
    """Run a case, or skip it when Python does not implement its function."""
    reason = skip_reason(case, hydroloom.__all__)
    if reason is not None:
        pytest.skip(reason)

    run_case(case, MANIFEST)
