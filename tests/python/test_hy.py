"""as_hy, representation, and _route: interface tests and properties.

Properties follow the Postconditions of hy.md and hy_network_type.md; the
semantics the manifest compares are not repeated here (S-7).
"""

from __future__ import annotations

import re
import warnings

import geopandas as gpd
import numpy as np
import pandas as pd
import pytest
from hypothesis import given, settings
from hypothesis import strategies as st
from shapely.geometry import LineString

from hydroloom import _hy as hy_module
from hydroloom import as_hy, representation
from hydroloom._hy import (
    Representation,
    _alias_map,
    _as_hy,
    _hy_reverse,
    _route,
    _terms,
    _terms_of_type,
    requires,
)
from hydroloom.errors import (
    HydroloomError,
    HydroloomWarning,
    RepresentationError,
    msg,
)

PROPERTY = settings(derandomize=True, max_examples=60, deadline=None)

# The registry the package filled at import, before any fixture swaps it.
_PUBLIC_REGISTRY = hy_module._REGISTRY


@st.composite
def source_tables(draw):
    """NHDPlusV2-style tables: COMID, ToCOMID with missing outlets, a name."""
    n = draw(st.integers(0, 8))
    ids = draw(st.lists(st.integers(1, 10**12), min_size=n, max_size=n, unique=True))
    toids = []
    for i in range(n):
        later = ids[i + 1 :]
        toid = draw(st.one_of(st.none(), st.sampled_from(later))) if later else None
        toids.append(toid)
    names = draw(st.lists(st.text(max_size=3), min_size=n, max_size=n))
    toid = pd.Series([np.nan if t is None else float(t) for t in toids], dtype=float)

    return pd.DataFrame(
        {"COMID": pd.Series(ids, dtype="int64"), "ToCOMID": toid, "Name": names}
    )


# as_hy ---------------------------------------------------------------------


@PROPERTY
@given(source_tables())
def test_as_hy_postconditions(table):
    """Row count, names, identifier text, the toid fill, and pass-through."""
    before = table.copy()
    out, names = _as_hy(table)

    pd.testing.assert_frame_equal(table, before)
    assert len(out) == len(table)
    assert list(out.columns) == ["id", "toid", "Name"]
    assert out.columns.is_unique
    assert names == {"COMID": "id", "ToCOMID": "toid"}
    assert all(isinstance(v, str) for v in out["id"])
    assert all(isinstance(v, str) for v in out["toid"])
    assert out["id"].tolist() == [str(v) for v in table["COMID"]]
    expected_toid = ["" if np.isnan(v) else str(int(v)) for v in table["ToCOMID"]]
    assert out["toid"].tolist() == expected_toid
    assert out["Name"].tolist() == table["Name"].tolist()


@PROPERTY
@given(source_tables())
def test_as_hy_idempotent_and_reversible(table):
    """A second pass changes nothing; _hy_reverse restores the input names."""
    out, names = _as_hy(table)

    pd.testing.assert_frame_equal(as_hy(out), out)
    assert list(_hy_reverse(out, names).columns) == list(table.columns)


def _alias_choices():
    """Per hy name, the column names that align to it alone.

    An alias listed under two hy names, or equal to another term's hy name,
    is left out: a column with it is a candidate for two names.
    """
    terms = _terms()
    owners: dict[str, set[str]] = {}
    for alias, name in _alias_map():
        owners.setdefault(alias, set()).add(name)
    choices: dict[str, list[str]] = {name: [name] for name in terms}
    for alias, name in _alias_map():
        if owners[alias] == {name} and (alias == name or alias not in terms):
            if alias not in choices[name]:
                choices[name].append(alias)

    return choices


_CHOICES = _alias_choices()


@st.composite
def alias_tables(draw):
    """Tables whose column names come from the alias map, in any case.

    Each hy name gets at most one column, so no draw holds a two-way alias
    tie: idempotence after a tie is open with the maintainer (SP-01), and a
    tie would fail this property until that is settled. The one candidate
    pair drawn is `permanent_identifier` beside another id column, which
    drops out by the step-4 rule.
    """
    n = draw(st.integers(0, 5))
    others = sorted(set(_CHOICES) - {"id"})
    hy_names = ["id", *draw(st.lists(st.sampled_from(others), unique=True, max_size=6))]
    columns = {name: draw(st.sampled_from(_CHOICES[name])) for name in hy_names}
    if columns["id"] != "permanent_identifier" and draw(st.booleans()):
        columns["permanent_identifier"] = "permanent_identifier"
    ids = list(range(1, n + 1))
    flags = set(_terms_of_type("flag"))
    data = {}
    for name, column in columns.items():
        column = "".join(c.upper() if draw(st.booleans()) else c for c in column)
        if name == "id":
            values = pd.Series(ids, dtype="int64")
        elif name == "toid":
            downs = [draw(st.one_of(st.none(), st.sampled_from(ids))) for _ in ids]
            values = pd.Series(
                [np.nan if d is None else float(d) for d in downs], dtype=float
            )
        elif name in flags:
            draws = st.lists(st.sampled_from([0, 1]), min_size=n, max_size=n)
            values = pd.Series(draw(draws), dtype="int64")
        elif name == "permanent_identifier":
            values = pd.Series([f"p{i}" for i in ids], dtype=object)
        else:
            draws = st.lists(st.integers(0, 9), min_size=n, max_size=n)
            values = pd.Series(draw(draws), dtype="int64")
        data[column] = values
    order = draw(st.permutations(list(data)))

    return pd.DataFrame({column: data[column] for column in order})


@PROPERTY
@given(alias_tables())
def test_as_hy_idempotent_and_reversible_over_aliases(table):
    """Idempotence and the name reverse over names drawn from the alias map."""
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        out, names = _as_hy(table)

    pd.testing.assert_frame_equal(as_hy(out), out)
    assert list(_hy_reverse(out, names).columns) == list(table.columns)


def test_as_hy_resets_index():
    """The row index is reset on ingest (I-12)."""
    table = pd.DataFrame({"id": [1, 2], "toid": [2, 0]}, index=[10, 20])

    assert as_hy(table).index.tolist() == [0, 1]


def test_as_hy_geometry_keeps_name_and_goes_last():
    """A GeoDataFrame returns with its active geometry name, last (G-14)."""
    lines = [LineString([(0, 0), (1, 1)]), LineString([(1, 1), (2, 2)])]
    table = gpd.GeoDataFrame(
        {"shape": lines, "COMID": [1, 2], "ToCOMID": [2, 0]},
        geometry="shape",
        crs=5070,
    )

    out = as_hy(table)

    assert isinstance(out, gpd.GeoDataFrame)
    assert out.geometry.name == "shape"
    assert list(out.columns) == ["id", "toid", "shape"]
    assert out.crs == table.crs
    assert out.geometry.geom_equals(table.geometry.reset_index(drop=True)).all()


def test_id_col_takes_the_name_ahead_of_the_aliases():
    """id_col names the id column; the other id alias keeps its name, silently."""
    table = pd.DataFrame({"nhdplusid": [1, 2], "featureid": [5, 6], "toid": [2, 0]})

    with warnings.catch_warnings():
        warnings.simplefilter("error")
        out = as_hy(table, id_col="nhdplusid")

    assert list(out.columns) == ["id", "featureid", "toid"]


def test_toid_col_takes_the_name():
    """toid_col names the toid column."""
    out = as_hy(pd.DataFrame({"id": [1, 2], "down": [2, None]}), toid_col="down")

    assert out["toid"].tolist() == ["2", ""]


def test_id_col_not_a_column_raises_key_error():
    """A column name that is not in the table is a KeyError naming it."""
    with pytest.raises(KeyError, match="nope"):
        as_hy(pd.DataFrame({"id": [1]}), id_col="nope")


def test_no_id_column_raises_representation_error():
    """Python raises where R warns (D3-130)."""
    with pytest.raises(RepresentationError):
        as_hy(pd.DataFrame({"name": ["a"]}))


def test_mixed_identifier_raises_type_error():
    """Strings and numbers in one identifier column raise TypeError (I-3)."""
    with pytest.raises(TypeError, match="Rows whose type differs"):
        as_hy(pd.DataFrame({"id": pd.Series(["a", 2], dtype=object)}))


@pytest.mark.parametrize(
    ("values", "expected"),
    [
        (pd.Series([1, None, 3], dtype="Int64"), ["1", None, "3"]),
        (pd.Series([2**64 - 1], dtype="uint64"), [str(2**64 - 1)]),
        (pd.Series([1.0, np.nan], dtype="float32"), ["1", None]),
        (pd.Series([1e19, 5.0]), ["10000000000000000000", "5"]),
        (pd.Series([True, False]), ["1", "0"]),
        (pd.Series(["a", None], dtype=object), ["a", None]),
    ],
    ids=["Int64", "uint64", "float32", "float-beyond-int64", "bool", "object"],
)
def test_identifier_ingest_by_dtype(values, expected):
    """Each identifier dtype becomes its exact decimal string, missing None (I-2)."""
    out = as_hy(pd.DataFrame({"id": values}))
    got = out["id"].tolist()

    assert [None if pd.isna(v) else v for v in got] == expected
    assert all(type(v) is str for v in got if not pd.isna(v))


@pytest.mark.parametrize(
    ("values", "key"),
    [
        ([1.0, 2.5, np.nan, 3.5], "identifier_not_whole"),
        ([1.0, np.inf, 2.0, -np.inf], "identifier_not_finite"),
    ],
)
def test_float_identifier_raises_with_rows(values, key):
    """A fractional or infinite float raises with the column and 1-based rows."""
    text = msg(key, column="COMID", rows="2, 4")

    with pytest.raises(HydroloomError, match=re.escape(text)):
        as_hy(pd.DataFrame({"COMID": values}))


def test_repeated_pair_raises():
    """A repeated (id, toid) pair raises at ingest (S-6, D3-57)."""
    with pytest.raises(HydroloomError, match="1"):
        as_hy(pd.DataFrame({"id": [1, 1], "toid": [2, 2]}))


@pytest.mark.parametrize(
    ("columns", "key"),
    [
        (["ID", "id"], "same name ignoring case"),
        (["", "id"], "position 1 has no name"),
        (["comid", "nhdplusid", "featureid"], "all aliases of the hydroloom name id"),
    ],
)
def test_name_errors_are_key_errors(columns, key):
    """Name problems are vocabulary problems, KeyError (A-10)."""
    table = pd.DataFrame([[1] * len(columns)], columns=columns)

    with pytest.raises(KeyError, match=key):
        as_hy(table)


def test_permanent_identifier_beside_id_is_not_a_conflict():
    """A second pass over aligned names keeps Permanent_Identifier as it is."""
    table = pd.DataFrame({"id": [1], "Permanent_Identifier": ["a"]})

    assert list(as_hy(table).columns) == ["id", "Permanent_Identifier"]


def test_flag_coding():
    """1/0 flags become bool, nullable boolean with a missing value (M-18)."""
    table = pd.DataFrame(
        {"id": [1, 2, 3], "StartFlag": [1, 0, 1], "TerminalFl": [0, np.nan, 1]}
    )

    out = as_hy(table)

    assert out["start_flag"].dtype == bool
    assert out["terminal_flag"].dtype == "boolean"
    assert out["terminal_flag"].isna().tolist() == [False, True, False]


def test_flag_not_binary_warns_and_passes_through():
    """A flag coded other than 0/1 warns and stays as it arrives (D3-152)."""
    table = pd.DataFrame({"id": [1, 2], "TerminalFl": [0, 2]})

    with pytest.warns(HydroloomWarning, match="Flag column TerminalFl"):
        out = as_hy(table)

    assert out["terminal_flag"].tolist() == [0, 2]


# representation --------------------------------------------------------------


def _toids(draw, ids):
    """Draw toid values, missing included, with distinct (id, toid) pairs (S-6).

    A drawn value whose pair is taken becomes ``100 + row``, so every draw is
    one `as_hy` accepts. Missing values are NaN, which `as_hy` fills with "".
    """
    seen = set()
    toids = []
    for row, i in enumerate(ids):
        toid = draw(st.one_of(st.none(), st.integers(0, 6)))
        if (i, toid) in seen:
            toid = 100 + row
        seen.add((i, toid))
        toids.append(np.nan if toid is None else float(toid))

    return pd.Series(toids, dtype=float)


@st.composite
def pattern_tables(draw):
    """Tables over the columns that decide the pattern, ids possibly repeated.

    Returns the table and a second toid column for the same ids (None when
    the table has no toid), for the toid-value independence postcondition.
    """
    n = draw(st.integers(0, 6))
    ids = draw(st.lists(st.integers(1, 4), min_size=n, max_size=n))
    table = pd.DataFrame({"id": pd.Series(ids, dtype="int64")})
    retoid = None
    if draw(st.booleans()):
        table["toid"] = _toids(draw, ids)
        retoid = _toids(draw, ids)
    if draw(st.booleans()):
        table["fromnode"] = range(n)
        table["tonode"] = range(1, n + 1)
    for column in ("topo_sort", "levelpath", "levelpath_outlet_id"):
        if draw(st.booleans()):
            table[column] = range(n)

    return table, retoid


@PROPERTY
@given(pattern_tables(), st.randoms(use_true_random=False))
def test_representation_postconditions(drawn, rng):
    """One of five names, row-order free, unchanged by as_hy and by values.

    Values vary within the inputs `as_hy` accepts: toid with distinct
    (id, toid) pairs and missing values, node values all missing (SP-03).
    """
    table, retoid = drawn
    value = representation(table)
    shuffled = table.sample(frac=1, random_state=rng.randint(0, 2**31))

    assert value in {r.value for r in Representation}
    assert representation(shuffled) == value
    assert representation(as_hy(table)) == value
    columns = set(table.columns)
    assert (value == "hy_flownetwork") == (
        {"id", "toid"} <= columns and not table["id"].is_unique
    )
    if value == "hy_leveled":
        assert {"toid", "topo_sort", "levelpath", "levelpath_outlet_id"} <= columns
    if value == "hy_node":
        assert "toid" not in columns
    if "fromnode" in columns:
        renodes = table.assign(fromnode=np.nan, tonode=np.nan)
        assert representation(renodes) == value
    if retoid is not None:
        assert representation(table.assign(toid=retoid)) == value


def test_representation_returns_plain_str():
    """The value is a str, not the enum member."""
    value = representation(pd.DataFrame({"id": [1], "toid": [0]}))

    assert type(value) is str


# _route and the registry --------------------------------------------------------


def _body(df, **kw):
    return "topo", kw


@pytest.fixture
def registry(monkeypatch):
    """Swap in a copy of the registry, so test registrations leave it alone."""
    copy = dict(hy_module._REGISTRY)
    monkeypatch.setattr(hy_module, "_REGISTRY", copy)

    return copy


@pytest.fixture
def routed(registry):
    """Register a TOPO-only function into the copied registry."""

    @requires(Representation.TOPO)
    def _routed(network):
        return _route(network, _routed, {Representation.TOPO: _body}, flag=1)

    return _routed


def test_test_registrations_stay_out_of_the_registry(routed):
    """The fixture registers into a copy (A-4 capabilities reads the registry)."""
    assert "_routed" in hy_module._REGISTRY
    assert "_routed" not in _PUBLIC_REGISTRY


def test_route_leveled_takes_the_topo_body(routed):
    """LEVELED satisfies TOPO by subsumption (A-3)."""
    df = as_hy(
        pd.DataFrame(
            {
                "id": [1],
                "toid": [0],
                "topo_sort": [1],
                "levelpath": [1],
                "levelpath_outlet_id": [1],
            }
        )
    )

    assert routed(df) == ("topo", {"flag": 1})


def test_route_raises_with_the_function_and_pattern(routed):
    """A pattern with no body raises the guidance text (A-3, A-10)."""
    df = as_hy(pd.DataFrame({"id": [1]}))

    with pytest.raises(RepresentationError, match=r"_routed\(\).*representation hy\)"):
        routed(df)


def test_route_converts_first(registry):
    """A registered conversion runs before the body is chosen (A-3)."""

    def to_topo(df):
        return df.assign(toid="")

    @requires(Representation.TOPO, convert={Representation.HY: to_topo})
    def converted(network):
        return _route(network, converted, {Representation.TOPO: _body})

    assert converted(as_hy(pd.DataFrame({"id": [1]})))[0] == "topo"


def test_route_with_no_needs_takes_any_table(registry):
    """``requires()`` with no needs routes every pattern to its body (A-3)."""

    @requires()
    def anything(network):
        return _route(network, anything, {Representation.HY: _body})

    assert anything(as_hy(pd.DataFrame({"id": [1]})))[0] == "topo"


def test_first_need_without_guidance_fails_at_registration(registry):
    """A first need with no remedies key raises when registered, not when called."""
    with pytest.raises(KeyError):
        requires(Representation.FLOWNETWORK)
