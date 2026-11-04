"""is_dendritic: interface tests and properties.

Properties follow the Postconditions of is_dendritic.md; the values the
manifest compares are not repeated here (S-7).
"""

from __future__ import annotations

import re

import geopandas as gpd
import numpy as np
import pandas as pd
import pytest
from hypothesis import given, settings
from hypothesis import strategies as st
from shapely.geometry import Point

from hydroloom import is_dendritic, representation
from hydroloom.errors import HydroloomError, RepresentationError, msg

PROPERTY = settings(derandomize=True, max_examples=100, deadline=None)

DIVERGENCE = st.sampled_from([0, 1, 2, np.nan])


@st.composite
def networks(draw):
    """Tables with ``id`` and, by draw, ``toid``, ``divergence``, and ``v``.

    With ``toid``, each flowline lists one or two distinct downstream values,
    so no (``id``, ``toid``) pair repeats (S-6) and ``id`` repeats only at a
    flownetwork divergence (I-6). Without ``toid``, ``id`` may repeat.
    """
    n = draw(st.integers(0, 8))
    labels = draw(
        st.lists(st.integers(1, 40).map(str), min_size=n, max_size=n, unique=True)
    )
    has_toid = draw(st.booleans())
    rows: list[tuple[str, str]] = []
    for label in labels:
        if has_toid:
            k = draw(st.integers(1, 2))
            targets = draw(
                st.lists(
                    st.sampled_from([*labels, "", "x"]),
                    min_size=k,
                    max_size=k,
                    unique=True,
                )
            )
            rows.extend((label, t) for t in targets)
        else:
            rows.extend((label, "") for _ in range(draw(st.integers(1, 2))))

    table = pd.DataFrame({"id": pd.Series([r[0] for r in rows], dtype=object)})
    if has_toid:
        table["toid"] = pd.Series([r[1] for r in rows], dtype=object)
    if draw(st.booleans()):
        values = draw(st.lists(DIVERGENCE, min_size=len(rows), max_size=len(rows)))
        table["divergence"] = pd.Series(values, dtype=float)
    if draw(st.booleans()):
        table["v"] = np.arange(len(rows), dtype=np.int64)

    return table


def _has_minor(table: pd.DataFrame) -> bool:
    return "divergence" in table.columns and bool((table["divergence"] == 2).any())


@PROPERTY
@given(networks(), st.randoms(use_true_random=False))
def test_postconditions(table, rng):
    """is_dendritic.md Postconditions, on one drawn table."""
    before = table.copy()
    strict = is_dendritic(table)
    loose = is_dendritic(table, validate_data=False)

    # A single bool for both values of validate_data.
    assert type(strict) is bool
    assert type(loose) is bool
    # validate_data true implies validate_data false.
    assert loose or not strict
    # A divergence 2 is False either way; without one, the loose test is True.
    if _has_minor(table):
        assert not strict
        assert not loose
    else:
        assert loose
    # The flownetwork pattern with toid is False under validation (A-2).
    if "toid" in table.columns and representation(table) == "hy_flownetwork":
        assert not strict
    # Unchanged by row order (D-7).
    shuffled = table.sample(frac=1, random_state=rng.randint(0, 2**31))
    assert is_dendritic(shuffled) == strict
    assert is_dendritic(shuffled, validate_data=False) == loose
    # The input is not modified (A-8).
    pd.testing.assert_frame_equal(table, before)


@PROPERTY
@given(networks(), st.sampled_from(["v", "name", "levelpath"]))
def test_other_columns_not_read(table, column):
    """Adding, removing, or changing another column leaves the result."""
    strict = is_dendritic(table)
    loose = is_dendritic(table, validate_data=False)
    changed = table.assign(**{column: np.arange(len(table)) * 7})
    removed = table.drop(columns=column, errors="ignore")

    for other in (changed, removed):
        assert is_dendritic(other) == strict
        assert is_dendritic(other, validate_data=False) == loose


@PROPERTY
@given(networks())
def test_source_names_read_after_alignment(table):
    """A column alignment maps to a read name is read under it (D3-197)."""
    source = table.rename(
        columns={"id": "COMID", "toid": "ToCOMID", "divergence": "Divergence"}
    )

    assert is_dendritic(source) == is_dendritic(table)
    assert is_dendritic(source, validate_data=False) == is_dendritic(
        table, validate_data=False
    )


# Interface -----------------------------------------------------------------


def test_geodataframe_answers_as_its_table():
    """Geometry is set aside; the answer is the attribute table's (G-14)."""
    table = pd.DataFrame({"id": [1, 1, 2, 3], "toid": [2, 3, 0, 0]})
    net = gpd.GeoDataFrame(table, geometry=[Point(i, 0) for i in range(4)], crs=5070)

    assert is_dendritic(net) == is_dendritic(table)
    assert is_dendritic(net, validate_data=False) == is_dendritic(
        table, validate_data=False
    )


def test_repeated_pair_raises_as_as_hy():
    """A repeated (id, toid) pair raises at alignment (D3-57, S-6)."""
    table = pd.DataFrame({"id": [1, 1, 2], "toid": [2, 2, 0]})
    text = msg("id_toid_pair_repeated", rows="1")

    for validate in (True, False):
        with pytest.raises(HydroloomError, match=re.escape(text)):
            is_dendritic(table, validate_data=validate)


def test_no_id_raises_as_as_hy():
    """A table with no id raises what as_hy raises (D3-29)."""
    with pytest.raises(RepresentationError, match=re.escape(msg("no_id_column"))):
        is_dendritic(pd.DataFrame({"toid": [1, 2]}))


def test_validate_data_is_keyword_only():
    """`validate_data` is keyword-only, after `network` (A-16)."""
    table = pd.DataFrame({"id": [1, 2], "toid": [2, 0]})

    with pytest.raises(TypeError):
        is_dendritic(table, False)  # type: ignore[misc]
