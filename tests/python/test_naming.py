"""hy_reverse, is_hy, align_names, name_definitions: interface tests and properties.

Properties follow the Postconditions of hy_reverse.md, is.hy.md, align_names.md,
and hydroloom_name_definitions.md; the semantics the manifest compares are not
repeated here (S-7).
"""

from __future__ import annotations

import json
import re
import warnings

import geopandas as gpd
import pandas as pd
import pytest
from hypothesis import given
from hypothesis import strategies as st
from shapely.geometry import LineString

from conformance import CONFORMANCE_DIR, remedy_pattern
from hydroloom import align_names, as_hy, hy_reverse, is_hy, name_definitions
from hydroloom._hy import _SOURCES, _alias_map, _source_names, _terms
from hydroloom.errors import HydroloomWarning
from test_hy import PROPERTY, alias_tables, source_tables

SCHEMA = CONFORMANCE_DIR.parent / "hy_vocab.schema.json"
VOCAB = CONFORMANCE_DIR.parent / "hy_vocab.json"


def _quiet(call, *args, **kw):
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        return call(*args, **kw)


def _same_values(out, table):
    """Column i of ``out`` holds column i of ``table``, type included."""
    pd.testing.assert_frame_equal(out.set_axis(table.columns, axis=1), table)


# hy_reverse -------------------------------------------------------------------


@PROPERTY
@given(alias_tables(), st.sampled_from(_SOURCES))
def test_hy_reverse_postconditions(table, source):
    """Counts, order, values, the renamed set, and the as_hy round trip.

    A term with no alias under ``source`` keeps its hydroloom name.
    """
    out = _quiet(as_hy, table)
    before = out.copy()
    try:
        back = hy_reverse(out, source=source)
    except KeyError as error:
        assert remedy_pattern("column_name_duplicate").fullmatch(error.args[0])
        return

    pd.testing.assert_frame_equal(out, before)
    _same_values(back, out)
    inverse = _source_names(source)
    assert list(back.columns) == [inverse.get(c, c) for c in out.columns]
    pd.testing.assert_frame_equal(_quiet(as_hy, back), out)


@PROPERTY
@given(source_tables(), st.sampled_from(_SOURCES), st.randoms(use_true_random=False))
def test_hy_reverse_rows_follow_input(table, source, rng):
    """A permutation of the rows permutes the output the same way (D-7)."""
    out = as_hy(table)
    shuffled = out.sample(frac=1, random_state=rng.randint(0, 2**31))

    pd.testing.assert_frame_equal(
        hy_reverse(shuffled, source=source),
        hy_reverse(out, source=source).loc[shuffled.index],
    )


def test_hy_reverse_without_source_is_the_identity():
    """With no registry in Python, source=None restores nothing (D3-15)."""
    out = as_hy(pd.DataFrame({"COMID": [1, 2], "ToCOMID": [2, None]}))
    back = hy_reverse(out)

    pd.testing.assert_frame_equal(back, out)
    assert back is not out


def test_hy_reverse_unknown_source_raises_key_error():
    """An unknown source is a vocabulary problem (A-10)."""
    out = as_hy(pd.DataFrame({"id": [1]}))

    with pytest.raises(KeyError) as info:
        hy_reverse(out, source="nhd")

    assert remedy_pattern("unknown_alias_source").fullmatch(info.value.args[0])
    assert "'nhd'" in info.value.args[0]


@pytest.mark.parametrize("column", ["nhdplusid", "NHDPlusID"])
def test_hy_reverse_restored_name_already_a_column_raises(column):
    """Restoring an alias the table already has raises (hy_reverse.md H-03)."""
    table = pd.DataFrame({"id": ["1"], column: [5]})

    with pytest.raises(KeyError) as info:
        hy_reverse(table, source="nhdplushr")

    assert remedy_pattern("column_name_duplicate").fullmatch(info.value.args[0])
    assert column in info.value.args[0]


def test_hy_reverse_keeps_active_geometry():
    """A GeoDataFrame comes back with the same active geometry (A-8)."""
    lines = [LineString([(0, 0), (1, 1)]), LineString([(1, 1), (2, 2)])]
    table = gpd.GeoDataFrame(
        {"COMID": [1, 2], "ToCOMID": [2, 0], "shape": lines},
        geometry="shape",
        crs=5070,
    )

    back = hy_reverse(as_hy(table), source="nhdplus")

    assert isinstance(back, gpd.GeoDataFrame)
    assert back.geometry.name == "shape"
    assert back.columns.tolist() == ["comid", "tocomid", "shape"]


def test_sources_are_the_schema_sources():
    """The accepted sources are the vocabulary schema's `source` enum (S-2)."""
    text = SCHEMA.read_text(encoding="utf-8")
    found = re.findall(r'"source": \{[^}]*"enum": (\[[^\]]*\])', text)

    assert [tuple(json.loads(body)) for body in found] == [_SOURCES]


@pytest.mark.parametrize("source", _SOURCES)
def test_one_alias_per_term_and_source(source):
    """Each term has at most one alias per source, and no two terms share one.

    hy_reverse.md Algorithm, tie rules: the inverse map is one to one.
    """
    tagged = [
        (name, alias["name"].lower())
        for name, term in _terms().items()
        for alias in term.get("aliases") or []
        if alias.get("source") == source
    ]
    names = [name for name, _ in tagged]
    aliases = [alias for _, alias in tagged]

    assert len(names) == len(set(names))
    assert len(aliases) == len(set(aliases))


# is_hy ------------------------------------------------------------------------


@PROPERTY
@given(alias_tables(), st.randoms(use_true_random=False))
def test_is_hy_postconditions(table, rng):
    """A bool, True on as_hy output, row-order free, the input unchanged."""
    before = table.copy()
    value = is_hy(table)
    shuffled = table.sample(frac=1, random_state=rng.randint(0, 2**31))

    assert type(value) is bool
    assert is_hy(shuffled) is value
    pd.testing.assert_frame_equal(table, before)
    assert is_hy(_quiet(as_hy, table)) is True


@PROPERTY
@given(source_tables())
def test_is_hy_false_in_source_names(table):
    """A table in NHDPlusV2 names is False by condition 1; as_hy makes it True."""
    assert is_hy(table) is False
    assert is_hy(as_hy(table)) is True


@pytest.mark.parametrize("value", [None, [1, 2], {"id": ["1"]}, pd.Series(["1"])])
def test_is_hy_not_a_table(value):
    """Any object is accepted and gives False when it is not a table."""
    assert is_hy(value) is False


@pytest.mark.parametrize(
    "columns",
    [
        ["id", "ID"],
        ["", "id"],
        ["comid", "nhdplusid", "featureid"],
        ["comid", "id"],
        ["id", "id"],
    ],
    ids=["case-duplicate", "empty", "three-aliases", "conflict", "repeated-label"],
)
def test_is_hy_never_raises(columns):
    """A frame on which as_hy raises on its names is False, silently (G-6)."""
    table = pd.DataFrame([["1"] * len(columns)], columns=columns)

    with warnings.catch_warnings():
        warnings.simplefilter("error")
        assert is_hy(table) is False


@pytest.mark.parametrize(
    ("table", "expected"),
    [
        ({"id": ["1", "2"], "toid": ["2", None]}, True),
        ({"id": ["1", "2"], "fromnode": ["a", None], "tonode": ["b", "c"]}, True),
        ({"id": ["1", None], "toid": ["2", ""]}, False),
        ({"id": [1, 2], "toid": ["2", ""]}, False),
        ({"id": ["1", "2"], "toid": [2.0, 0.0]}, False),
        ({"id": ["1", "2"], "tonode": [1, 2]}, False),
        ({"name": ["a"]}, False),
        ({"id": ["1"], "Notes": [1]}, True),
    ],
    ids=[
        "missing-toid",
        "missing-node",
        "missing-id",
        "integer-id",
        "float-toid",
        "integer-node",
        "no-id",
        "other-column-any-type",
    ],
)
def test_is_hy_definition(table, expected):
    """Identifier dtypes and missing values on ingest (is.hy.md condition 2).

    Source names (condition 1) are in the manifest cases and the properties.
    """
    assert is_hy(pd.DataFrame(table)) is expected


def test_is_hy_string_dtype_and_zero_rows():
    """String dtype is text; a zero-row table is True with text identifiers (I-11)."""
    text = pd.DataFrame({"id": pd.Series(["1"], dtype=pd.StringDtype())})
    empty = pd.DataFrame({"id": pd.Series([], dtype=object)})
    numbers = pd.DataFrame({"id": pd.Series([], dtype="int64")})

    assert is_hy(text) is True
    assert is_hy(empty) is True
    assert is_hy(numbers) is False


def test_is_hy_geometry_name_is_not_read():
    """The active geometry column is set aside, as as_hy sets it aside."""
    table = gpd.GeoDataFrame(
        {"id": ["1"], "SHAPE": [LineString([(0, 0), (1, 1)])]}, geometry="SHAPE"
    )

    assert is_hy(table) is True


# align_names ------------------------------------------------------------------


@PROPERTY
@given(alias_tables(), st.randoms(use_true_random=False))
def test_align_names_postconditions(table, rng):
    """Values and positions kept; names hy or as input; idempotent; order free."""
    hy_names = set(_terms())
    before = table.copy()
    out = _quiet(align_names, table)

    pd.testing.assert_frame_equal(table, before)
    _same_values(out, table)
    assert out.columns.is_unique
    for old, new in zip(table.columns, out.columns, strict=True):
        assert new == old or new in hy_names
        if new != old:
            assert old.lower() == new or (old.lower(), new) in _alias_map()
        if old.lower() in hy_names:
            assert new == old.lower()
    pd.testing.assert_frame_equal(_quiet(align_names, out), out)

    names = dict(zip(table.columns, out.columns, strict=True))
    rows = table.sample(frac=1, random_state=rng.randint(0, 2**31))
    pd.testing.assert_frame_equal(_quiet(align_names, rows), out.loc[rows.index])
    order = list(table.columns)
    rng.shuffle(order)
    moved = _quiet(align_names, table[order])
    assert list(moved.columns) == [names[c] for c in order]


def test_align_names_types_nothing():
    """No identifier typing (I-2) and no toid fill (I-5); the index is kept."""
    table = pd.DataFrame({"COMID": [101, 102], "ToCOMID": [102.0, None]}, index=[5, 6])

    out = align_names(table)

    assert out.columns.tolist() == ["id", "toid"]
    assert out["id"].dtype == "int64"
    assert out["toid"].isna().tolist() == [False, True]
    assert out.index.tolist() == [5, 6]


def test_align_names_geometry_name_unchanged():
    """An uppercase geometry name stays as it is in Python (align_names.md)."""
    table = gpd.GeoDataFrame(
        {"COMID": [1], "SHAPE": [LineString([(0, 0), (1, 1)])]},
        geometry="SHAPE",
        crs=5070,
    )

    out = align_names(table)

    assert isinstance(out, gpd.GeoDataFrame)
    assert out.columns.tolist() == ["id", "SHAPE"]
    assert out.geometry.name == "SHAPE"
    assert out.crs == table.crs


def test_align_names_tie_warns_at_the_caller():
    """Two aliases of one name warn, pointing at this file (A-11)."""
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        out = align_names(pd.DataFrame({"nhdplusid": [1], "featureid": [2]}))

    assert [w.category for w in caught] == [HydroloomWarning]
    assert caught[0].filename == __file__
    assert out.columns.tolist() == ["nhdplusid", "id"]


# name_definitions -------------------------------------------------------------


def test_name_definitions_postconditions():
    """One row per column term, in file order, each with its definition."""
    raw = json.loads(VOCAB.read_text(encoding="utf-8"))["terms"]
    columns = {k: t for k, t in raw.items() if not t.get("concept")}

    defs = name_definitions()

    assert defs.columns.tolist() == ["name", "definition"]
    assert defs["name"].tolist() == list(columns)
    assert defs["definition"].tolist() == [t["definition"] for t in columns.values()]
    assert defs["name"].is_unique
    assert all(re.fullmatch(r"[a-z][a-z0-9]*(_[a-z0-9]+)*", n) for n in defs["name"])
    assert all(isinstance(d, str) and d for d in defs["definition"])
    assert "mainstem" not in set(defs["name"])


def test_name_definitions_is_a_new_frame():
    """Changing a returned frame leaves the next call unchanged."""
    defs = name_definitions()
    defs.loc[0, "definition"] = "changed"

    assert name_definitions().loc[0, "definition"] != "changed"
