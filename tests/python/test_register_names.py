"""register_names: the alias registry, its raises, and its readers.

Properties follow the Postconditions of hydroloom_names.md; align_names.md step 2
(REVIEW_11032026 item 4) and hy_reverse.md (``source=None``) give the readers' behavior. No manifest
case can register a mapping (SPEC S-4), so these tests stand in for one.
"""

from __future__ import annotations

import doctest
import string
import warnings

import pandas as pd
import pytest
from hypothesis import given
from hypothesis import strategies as st

from conformance import remedy_pattern
from hydroloom import _hy as hy_module
from hydroloom import align_names, as_hy, hy_reverse, register_names
from hydroloom._hy import _alias_map, _terms, _vocab_alias_map
from hydroloom.errors import HydroloomWarning
from test_hy import PROPERTY

VOCAB = _vocab_alias_map()


@pytest.fixture(autouse=True)
def registry():
    """Restore the process-wide registry after each test."""
    before = list(hy_module._REGISTERED)
    yield hy_module._REGISTERED
    hy_module._REGISTERED[:] = before


def _raises(key, mapping, **kw):
    before = _alias_map()
    with pytest.raises(KeyError) as info:
        register_names(mapping, **kw)
    assert remedy_pattern(key).fullmatch(info.value.args[0])
    assert _alias_map() == before

    return info.value.args[0]


# Postconditions ---------------------------------------------------------------


def test_registry_at_load():
    """No alias repeats, every alias is lowercase, every value is a term."""
    aliases = [alias for alias, _ in VOCAB]

    assert hy_module._REGISTERED == []
    assert _alias_map() == VOCAB
    assert len(aliases) == len(set(aliases))
    assert all(alias == alias.lower() for alias in aliases)
    assert all(value in _terms() for _, value in VOCAB)


def test_register_appends_lowercased_in_order():
    """The prior map, then the mapping with its aliases lowercased."""
    register_names({"FlowlineID": "id"})
    prior = _alias_map()
    register_names({"Next_ID": "toid", "flowlineid": "id"})

    after = _alias_map()
    assert after[: len(prior)] == prior
    assert after[len(prior) :] == (("next_id", "toid"), ("flowlineid", "id"))


def test_replace_drops_registered_and_keeps_vocabulary():
    """replace=True is the map at load followed by the mapping (D3-20)."""
    register_names({"flowline_id": "id"})
    register_names({"next_id": "toid"}, replace=True)

    assert _alias_map() == (*VOCAB, ("next_id", "toid"))


def test_replace_with_empty_mapping_is_the_registry_at_load():
    """replace=True with no entries drops every registration (D3-20)."""
    register_names({"flowline_id": "id"})
    register_names({}, replace=True)

    assert _alias_map() == VOCAB


def test_unknown_value_raises_and_registers_nothing():
    """A value that is not a hydroloom name raises; the registry is unchanged."""
    register_names({"flowline_id": "id"})

    text = _raises("register_name_unknown", {"a": "id", "B": "not_a_name"})
    _raises("register_name_unknown", {"b": "not_a_name"}, replace=True)

    assert "B = not_a_name" in text
    assert "a = id" not in text


def test_alias_that_is_another_name_raises_and_registers_nothing():
    """An alias that, lowercased, is a hydroloom name other than its value raises."""
    register_names({"flowline_id": "id"})

    text = _raises("register_alias_is_name", {"TOID": "id", "x": "toid"})
    _raises("register_alias_is_name", {"id": "toid"}, replace=True)

    assert "TOID = id" in text
    assert "x = toid" not in text


def test_unknown_value_is_checked_first():
    """register_name_unknown raises ahead of register_alias_is_name (step 3)."""
    _raises("register_name_unknown", {"toid": "id", "a": "nope"})


def test_identity_entry_is_accepted():
    """An alias equal to its value renames nothing and registers."""
    register_names({"ID": "id"})

    assert _alias_map()[-1] == ("id", "id")
    assert align_names(pd.DataFrame({"id": [1]})).columns.tolist() == ["id"]


@PROPERTY
@given(
    st.dictionaries(
        st.text(string.ascii_letters + "_", min_size=1, max_size=6),
        st.sampled_from([*list(_terms())[:6], "nope"]),
        max_size=4,
    ),
    st.booleans(),
)
def test_register_postconditions(mapping, replace):
    """The map after a call, or unchanged after a raise; restored on exit."""
    terms = _terms()
    before = _alias_map()
    unknown = any(v not in terms for v in mapping.values())
    clash = any(a.lower() in terms and a.lower() != v for a, v in mapping.items())

    if unknown or clash:
        _raises(
            "register_name_unknown" if unknown else "register_alias_is_name",
            mapping,
            replace=replace,
        )
        return

    with register_names(mapping, replace=replace) as handle:
        assert handle is None
        added = tuple((a.lower(), v) for a, v in mapping.items())
        assert _alias_map() == (VOCAB if replace else before) + added
        assert all(v in terms for _, v in _alias_map())
        assert all(a == v for a, v in _alias_map() if a in terms)
    assert _alias_map() == before


# Context manager --------------------------------------------------------------


def test_context_manager_restores_on_exit_and_on_error():
    """Leaving ``with`` by any route restores the registry before the call."""
    register_names({"flowline_id": "id"})
    before = _alias_map()

    with register_names({"next_id": "toid"}):
        assert _alias_map()[-1] == ("next_id", "toid")
    assert _alias_map() == before

    with pytest.raises(RuntimeError), register_names({"next_id": "toid"}):
        raise RuntimeError
    assert _alias_map() == before


def test_context_manager_undoes_replace_and_inner_calls():
    """replace=True and calls made inside the block are undone with it."""
    register_names({"flowline_id": "id"})
    before = _alias_map()

    with register_names({"next_id": "toid"}, replace=True):
        assert ("flowline_id", "id") not in _alias_map()
        register_names({"other_id": "id"})
    assert _alias_map() == before


def test_nested_blocks_restore_in_reverse_order():
    """Each block restores the registry as it was on its own entry."""
    before = _alias_map()

    with register_names({"a_id": "id"}):
        middle = _alias_map()
        with register_names({"b_id": "id"}):
            assert len(_alias_map()) == len(before) + 2
        assert _alias_map() == middle
    assert _alias_map() == before


def test_registration_outside_with_stays():
    """Called outside ``with``, the registration stays."""
    register_names({"flowline_id": "id"})

    out = as_hy(pd.DataFrame({"Flowline_ID": [1, 2]}))
    assert out.columns.tolist() == ["id"]


def test_register_names_doctest():
    """The hand-written example runs (A-13)."""
    tests = doctest.DocTestFinder().find(register_names, "register_names", globs={})
    runner = doctest.DocTestRunner()
    for test in tests:
        runner.run(test)

    assert tests and runner.failures == 0


# align_names and as_hy (align_names.md step 2, REVIEW_11032026 item 4) ----------------------------


def test_registered_alias_wins_over_another_terms_alias():
    """After registering comid as toid, a COMID column becomes toid only."""
    table = pd.DataFrame({"COMID": [1, 2], "id": [5, 6]})
    register_names({"comid": "toid"})

    with warnings.catch_warnings():
        warnings.simplefilter("error")
        assert align_names(table).columns.tolist() == ["toid", "id"]
        out = as_hy(table)

    assert out.columns.tolist() == ["toid", "id"]
    assert out["toid"].tolist() == ["1", "2"]


def test_latest_pair_wins_among_different_names():
    """Only the latest pair of an alias mapped to two names is a candidate."""
    register_names({"myid": "toid"})
    register_names({"MyID": "id"})

    out = align_names(pd.DataFrame({"MyID": [1], "other": [2]}))

    assert out.columns.tolist() == ["id", "other"]


def test_registered_alias_wins_a_two_way_tie():
    """A registered alias comes after the vocabulary's and takes the name."""
    register_names({"flowline_id": "id"})

    with pytest.warns(HydroloomWarning) as caught:
        out = align_names(pd.DataFrame({"comid": [1], "flowline_id": [2]}))

    assert remedy_pattern("name_alias_duplicate").fullmatch(str(caught[0].message))
    assert out.columns.tolist() == ["comid", "id"]


def test_alias_registered_twice_warns_and_renames():
    """The same pair registered twice is two candidates on one column."""
    register_names({"flowline_id": "id"})
    register_names({"Flowline_ID": "id"})

    with pytest.warns(HydroloomWarning):
        out = align_names(pd.DataFrame({"flowline_id": [1]}))

    assert out.columns.tolist() == ["id"]


# hy_reverse(source=None) --------------------------------------------------------


def test_hy_reverse_restores_latest_registered_alias():
    """Each name takes its latest registered alias, lowercase; others stay."""
    register_names({"Flowline_ID": "id", "Next_ID": "toid"})
    out = as_hy(pd.DataFrame({"Flowline_ID": [1, 2], "Next_ID": [2, 0], "notes": 1}))
    register_names({"FlowlineID": "id"})

    back = hy_reverse(out)

    assert back.columns.tolist() == ["flowlineid", "next_id", "notes"]
    pd.testing.assert_frame_equal(back.set_axis(out.columns, axis=1), out)


def test_hy_reverse_skips_vocabulary_and_identity_entries():
    """The vocabulary aliases and an identity entry restore nothing."""
    register_names({"toid": "toid"})
    out = as_hy(pd.DataFrame({"COMID": [1, 2], "ToCOMID": [2, 0]}))

    assert hy_reverse(out).columns.tolist() == ["id", "toid"]


def test_hy_reverse_identity_entry_registered_last_wins():
    """An identity entry registered last is the latest alias: the name stays."""
    register_names({"flowline_id": "id", "": "toid"})
    register_names({"id": "id"})
    out = as_hy(pd.DataFrame({"Flowline_ID": [1, 2], "toid": [2, 0]}))

    assert hy_reverse(out).columns.tolist() == ["id", "toid"]


def test_hy_reverse_registered_alias_already_a_column_raises():
    """A restored alias that is already a column raises column_name_duplicate."""
    register_names({"flowline_id": "id"})
    table = pd.DataFrame({"id": ["1"], "Flowline_ID": [5]})

    with pytest.raises(KeyError) as info:
        hy_reverse(table)

    assert remedy_pattern("column_name_duplicate").fullmatch(info.value.args[0])
