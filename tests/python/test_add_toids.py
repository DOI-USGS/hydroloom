"""add_toids: the outlet-fill layers, interface tests, and properties.

The manifest marks 18 add_toids cases ``r_only`` because R writes the outlet of
an integer ``id`` as ``0`` where Python writes ``""`` (add_toids.md Algorithm
step 6). Those layers are R's output with the outlet ``"0"`` for ``""``, the
output R gives for character identifiers, so each is run here against its
stored layer with that one value replaced (P-3); the expectation stays in the
GeoPackage (S-7). Properties follow the description's Postconditions.
"""

from __future__ import annotations

import re
import warnings

import geopandas as gpd
import pandas as pd
import pytest
from hypothesis import given, settings
from hypothesis import strategies as st
from shapely.geometry import Point

import hydroloom
from conformance import (
    assert_layer_equal,
    load_manifest,
    python_name,
    read_expected,
    read_fixture,
    result_to_layer,
)
from hydroloom import add_toids, align_names
from hydroloom.errors import RepresentationError, msg

PROPERTY = settings(derandomize=True, max_examples=80, deadline=None)

MANIFEST = load_manifest()
OUTLET_FILL = "add_toids.md §Algorithm step 6"
FILL_CASES = [c for c in MANIFEST["cases"] if c.get("r_only") == OUTLET_FILL]

# The id-typed columns add_toids reads, by hy name.
NODE_IDS = {"id", "fromnode", "tonode"}


def _as_text(values: pd.Series) -> pd.Series:
    return values.map(lambda v: None if pd.isna(v) else str(int(v))).astype(object)


def _text_ids(network: pd.DataFrame) -> pd.DataFrame:
    """Return the network with its id, fromnode, and tonode columns as text."""
    out = network.copy()
    aligned = align_names(network).columns
    for column, name in zip(network.columns, aligned, strict=True):
        if name in NODE_IDS:
            out[column] = _as_text(network[column])

    return out


def _outlet_as_text(layer: pd.DataFrame) -> pd.DataFrame:
    """Replace R's integer outlet ``"0"`` with Python's ``""`` (step 6, I-5)."""
    out = layer.copy()
    toid = out["toid"].astype(str)
    ids = set(out[[c for c in out.columns if c != "toid"][0]].astype(str))
    assert "0" not in ids, "an id 0 would make the outlet replacement ambiguous"
    out["toid"] = toid.where(toid != "0", "")

    return out


def test_fill_cases_are_the_marked_eighteen():
    """The parametrized set below is every case the outlet fill keeps R-only."""
    assert len(FILL_CASES) == 18


@pytest.mark.parametrize("text_input", [False, True], ids=["integer", "text"])
@pytest.mark.parametrize("case", FILL_CASES, ids=[c["id"] for c in FILL_CASES])
def test_outlet_fill_layers(case, text_input):
    """Each R-only layer matches once its outlet is written ``""``."""
    network = read_fixture(case["fixture"])
    if "prepare" in case:
        prepare = getattr(hydroloom, python_name(case["prepare"]["function"]))
        network = prepare(network, **(case["prepare"].get("args") or {}))
    elif text_input:
        network = _text_ids(network)

    with warnings.catch_warnings():
        warnings.simplefilter("ignore", FutureWarning)
        result = add_toids(network, **(case.get("args") or {}))

    layer = result_to_layer(result, case, "frame")
    assert_layer_equal(
        layer,
        _outlet_as_text(read_expected(case)),
        compare=case["compare"],
        order=case["order"],
    )


# Interface -----------------------------------------------------------------


def _worked_example() -> pd.DataFrame:
    return read_fixture("add_toids_worked_example.gpkg")


def test_geodataframe_in_geodataframe_out():
    """A GeoDataFrame returns one, each repeated row with its own geometry."""
    table = _worked_example()
    net = gpd.GeoDataFrame(
        table, geometry=[Point(i, i) for i in range(len(table))], crs=5070
    )

    with pytest.warns(FutureWarning):
        out = add_toids(net, return_dendritic=False)

    assert isinstance(out, gpd.GeoDataFrame)
    assert out.crs == net.crs
    assert out.columns[-1] == "geometry"
    first = out[out["id"] == "1"].geometry
    assert len(first) == 2 and (first == Point(0, 0)).all()


def test_input_names_return():
    """Source names come back; toid, with no input name, stays toid (step 1)."""
    table = _worked_example().rename(
        columns={"id": "COMID", "fromnode": "FromNode", "tonode": "ToNode"}
    )

    out = add_toids(table)

    assert set(out.columns) == {"COMID", "FromNode", "ToNode", "divergence", "toid"}


def test_dendritic_does_not_warn():
    """Only return_dendritic=False warns (A-11)."""
    with warnings.catch_warnings():
        warnings.simplefilter("error")
        add_toids(_worked_example())


def test_flownetwork_input_raises_toid_already_present():
    """A repeated id with toid raises RepresentationError (D3-38)."""
    table = pd.DataFrame({"id": [1, 1, 2], "toid": [2, 3, 0]})
    text = msg("toid_already_present", current="hy_flownetwork")

    with pytest.raises(RepresentationError, match=re.escape(text)):
        add_toids(table)


def test_divergence_messages_name_ids():
    """Raise and warning text names the flowlines by id (Preconditions)."""
    table = _worked_example()
    table["id"] = table["id"] + 100

    broken = table.assign(divergence=[0, 1, 1, 0, 0, 0])
    with pytest.raises(
        ValueError,
        match=re.escape(msg("divergence_coding_invalid", rows="102, 103")),
    ):
        add_toids(broken)

    zero = table.assign(divergence=[0, 0, 2, 0, 0, 0])
    with pytest.warns(UserWarning) as record:
        add_toids(zero)
    assert [str(w.message) for w in record] == [
        msg("divergence_zero_main_path", rows="102")
    ]


# Properties ----------------------------------------------------------------


@st.composite
def node_networks(draw):
    """Node tables with text ids; at most one main path at each fromnode.

    Some tonodes are missing or reach no fromnode; a fromnode with one flowline
    may carry any code, so lone minor paths occur.
    """
    n = draw(st.integers(0, 12))
    labels = [f"n{k}" for k in range(8)]
    fromnode = draw(st.lists(st.sampled_from(labels), min_size=n, max_size=n))
    tonode = draw(
        st.lists(st.one_of(st.none(), st.sampled_from(labels)), min_size=n, max_size=n)
    )
    ids = [f"f{i}" for i in draw(st.permutations(range(n)))]

    divergence = [0] * n
    for node in set(fromnode):
        rows = [i for i, f in enumerate(fromnode) if f == node]
        if len(rows) == 1:
            divergence[rows[0]] = draw(st.sampled_from([0, 1, 2]))
            continue
        main = draw(st.sampled_from(rows))
        for i in rows:
            divergence[i] = draw(st.sampled_from([0, 1])) if i == main else 2

    return pd.DataFrame(
        {
            "id": pd.Series(ids, dtype=object),
            "fromnode": pd.Series(fromnode, dtype=object),
            "tonode": pd.Series(tonode, dtype=object),
            "divergence": pd.Series(divergence, dtype="int64"),
            "name": pd.Series([f"x{i}" for i in range(n)], dtype=object),
        }
    )


def _targets(table: pd.DataFrame, dendritic: bool) -> dict[str, list[str]]:
    """Each row's eligible downstream ids in input order (Definition)."""
    reached = set(table["tonode"].dropna())
    counts = table["fromnode"].value_counts()
    out = {}
    for row in table.itertuples():
        down = table[table["fromnode"] == row.tonode] if row.tonode else table[:0]
        if dendritic:
            split = row.tonode in reached and counts.get(row.tonode, 0) >= 2
            down = down[~((down["divergence"] == 2) & split)]
        out[row.id] = down["id"].tolist()

    return out


def _add(table: pd.DataFrame, dendritic: bool) -> pd.DataFrame:
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        return add_toids(table, return_dendritic=dendritic)


@PROPERTY
@given(node_networks(), st.booleans(), st.randoms(use_true_random=False))
def test_add_toids_postconditions(table, dendritic, rng):
    """Postconditions in both modes, and D-7 under a row permutation."""
    out = _add(table, dendritic)
    ids = table["id"].tolist()
    by_id = table.set_index("id")
    targets = _targets(table, dendritic)

    columns = ["id", "toid", "fromnode", "tonode", "divergence", "name"]
    assert list(out.columns) == columns
    assert out["toid"].map(type).eq(str).all()
    assert set(out["toid"]) <= set(ids) | {""}
    # Rows follow input order; one row per target, or one outlet row.
    want = [(i, t) for i in ids for t in (targets[i] or [""])]
    assert list(zip(out["id"], out["toid"], strict=True)) == want

    def values(series):
        return [None if pd.isna(v) else v for v in series]

    for column in ("fromnode", "tonode", "divergence", "name"):
        # Values, not dtypes: as_hy types the identifier columns (I-2).
        assert values(out[column]) == values(by_id.loc[out["id"], column])
    linked = out[out["toid"] != ""]
    assert by_id.loc[linked["toid"], "fromnode"].tolist() == linked["tonode"].tolist()
    assert out.loc[out["tonode"].isna(), "toid"].eq("").all()
    if dendritic:
        assert len(out) == len(table) and out["id"].is_unique

    shuffled = table.sample(frac=1, random_state=rng.randint(0, 2**31))
    again = _add(shuffled.reset_index(drop=True), dendritic)
    assert sorted(zip(again["id"], again["toid"], strict=True)) == sorted(want)
    assert list(pd.unique(again["id"])) == shuffled["id"].tolist()
