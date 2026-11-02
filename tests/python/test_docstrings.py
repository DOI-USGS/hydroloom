"""The A-13 docstring composer and its R-phrasing substitution table."""

from __future__ import annotations

import re

import pytest
from numpydoc.docscrape import get_doc_object
from numpydoc.validate import validate

import hydroloom
from hydroloom._docstrings import (
    PER_FUNCTION,
    SUBSTITUTIONS,
    doc,
    per_function,
    r_docs,
    substitute,
)

# Fires of each substitution pair over every field of every alias in
# docstrings.json, keyed by pattern. Regenerating docstrings.json can change
# these; the table and these counts are reviewed together (VERIFICATION 6).
EXPECTED_FIRES = {
    r"data\.frame network compatible with hydroloom_names": 108,
    r"logical if TRUE": 29,
    r"logical if `TRUE`": 5,
    r"\Alogical\. ": 7,
    r"\Alogical ": 26,
    r"\Aboolean ": 23,
    r"\Acharacter or numeric ": 6,
    r"\Acharacter or factor vector, or NULL\. ": 3,
    r"\Acharacter or NULL\. ": 3,
    r"\Acharacter\. ": 1,
    r"\Acharacter ": 41,
    r"\Anumeric\. ": 11,
    r"\Anumeric ": 26,
    r"\Ainteger ": 7,
    r"\Aunits distance ": 3,
    r"\Acrs\. ": 3,
    r"\Adata\.frame\. ": 30,
    r"\Avector of the same length as `nrow\(x\)` containing ": 6,
    r"\Avector ": 35,
    r"sfc_POINT simple feature geometry list of length `nrow\(indexes\)`": 1,
    r"sf sf data\.frame": 1,
    r"sf data\.frame or sfc geometry": 3,
    r"sf data\.frame or sfc": 3,
    r"sf data\.frame": 15,
    r"sf data\.table": 1,
    r"sf or sfc": 3,
    r"an sf point geometry column": 4,
    r"a sf object": 1,
    r"\bsf geometry": 3,
    r"When sfc is provided": 3,
    r"the return value is sfc": 3,
    r"\bsfc geometry\b": 3,
    r"\bsfc\b": 7,
    r"data,frame": 5,
    r"\bdataframe\b": 1,
    r"data\.frame": 125,
    r"or list as returned by make_index_ids": 1,
    r"an `hy` object": 6,
    r"`hy` object": 2,
    r"\bhy object": 15,
    r"an `hy`": 1,
    r"required attributes": 11,
    r"requires attributes": 1,
    r"\(character\)": 1,
    r"\(character:": 1,
    r"\(logical\)": 1,
    r"a logical flag": 5,
    r"a logical value": 1,
    r"logical columns": 1,
    r"`(\w+) = TRUE`": 9,
    r"`(\w+) = FALSE`": 5,
    r"`TRUE`": 9,
    r"`FALSE`": 31,
    r"(?<!= )\bTRUE\b": 36,
    r"(?<!= )\bFALSE\b": 15,
    r"an `NA`": 5,
    r"`NA`": 20,
    r"If missing or NULL": 3,
    r"When non-NULL": 9,
    r"When NULL": 6,
    r"is NULL": 3,
    r"Default NULL": 6,
    r"or NULL": 12,
    r"`Inf`": 6,
    r"`unqiue\(x\$id\)`": 5,
    r"(?<!CRS of )`x`": 98,
    r"c\(UM, DM, UT, DD\) or equivalently c\(upmain, downmain, up, down\)": 6,
    r"`hy\(\)`": 23,
    r"`hy_capabilities\(\)`": 5,
    r"`is\.hy\(\)`": 1,
    r"See hydroloom_names for": 1,
    r"(?i)\b(s)ee details\b(?! for required)": 24,
}


def _fields(entry):
    for key in ("title", "description", "details", "value"):
        if entry.get(key):
            yield entry[key]
    yield from (entry.get("arguments") or {}).values()
    for title, text in (entry.get("sections") or {}).items():
        yield title
        yield text


def test_table_fires_as_counted():
    """Every pair fires, as many times as counted, over the whole JSON."""
    totals = dict.fromkeys((pattern for pattern, _ in SUBSTITUTIONS), 0)
    for entry in r_docs().values():
        for text in _fields(entry):
            counts = substitute(text)[1]
            for pattern, n in zip(totals, counts, strict=True):
                totals[pattern] += n

    assert totals == EXPECTED_FIRES
    assert min(totals.values()) > 0


# Throwaway functions whose signatures follow the Python Signature of each
# description in inst/extdata/conformance/algorithms/. index_points_to_lines
# has no description yet; its signature follows A-9 and ROADMAP 2 ("same").


@doc("make_index_ids")
def make_index_ids(
    network: pd.DataFrame, *, mode: str = "to"
) -> IndexIds | tuple[IndexIds, IndexIds]:
    """Make index ids.

    Examples
    --------
    >>> make_index_ids.__name__
    'make_index_ids'
    """


@doc("navigate_network_dfs")
def navigate_network_dfs(
    network: pd.DataFrame | IndexIds,
    starts: Sequence[str],
    *,
    direction: str = "down",
    reset: bool = False,
) -> list[list[pd.Index]]:
    """Navigate all paths depth first.

    Examples
    --------
    >>> navigate_network_dfs.__name__
    'navigate_network_dfs'
    """


@doc("add_toids")
def add_toids(network: pd.DataFrame, *, return_dendritic: bool = True) -> pd.DataFrame:
    """Add downstream ids.

    Examples
    --------
    >>> add_toids.__name__
    'add_toids'
    """


@doc("index_points_to_lines")
def index_points_to_lines(
    network: gpd.GeoDataFrame,
    points: gpd.GeoDataFrame | gpd.GeoSeries,
    *,
    search_radius: float | None = None,
    precision: float | None = None,
    max_matches: int = 1,
    ids: Sequence[str] | None = None,
    aggregate_ids: Sequence[str] | None = None,
) -> pd.DataFrame:
    """Index points to lines.

    Examples
    --------
    >>> index_points_to_lines.__name__
    'index_points_to_lines'
    """


COMPOSED = [make_index_ids, navigate_network_dfs, add_toids, index_points_to_lines]

# numpydoc codes the composed docstrings may carry. None so far.
ALLOWED_CODES: set[str] = set()


@pytest.mark.parametrize("func", COMPOSED, ids=lambda f: f.__name__)
def test_composed_docstring_validates(func):
    """numpydoc.validate accepts the composed docstring (A-13, VERIFICATION 4)."""
    result = validate(get_doc_object(func))
    codes = {code for code, _ in result["errors"]}

    assert codes <= ALLOWED_CODES, result["errors"]


@pytest.mark.parametrize("func", COMPOSED, ids=lambda f: f.__name__)
def test_composed_sections(func):
    """The R text arrives substituted, with a See Also and the own Examples."""
    text = func.__doc__

    assert f"{func.__name__} : R function `hydroloom::{func.__name__}()`" in text
    assert text.count("Examples\n--------") == 1
    assert f">>> {func.__name__}.__name__" in text
    assert "data.frame" not in text
    assert not re.search(r"\blogical\b", text)
    # `sf` as a class word; the R function `sf::st_segmentize()` in the
    # index_points_to_lines details waits for the per-function pass.
    assert not re.search(r"\bsf\b(?!::)", text)
    # The hand-written summary is replaced by the R title.
    assert text.splitlines()[1] == r_docs()[func.__name__]["title"] + "."


def test_parameters_follow_python_signature():
    """R-only arguments are dropped and the Python order is kept."""
    params = get_doc_object(make_index_ids)["Parameters"]

    assert [p.name for p in params] == ["network", "mode"]
    assert params[0].desc == [
        "A table whose columns use hydroloom names or names registered with "
        "`register_names`."
    ]
    assert params[1].type == "str, default 'to'"


def test_sections_go_to_notes():
    """The transferred Measures section is a Notes paragraph led by its title."""
    notes = "\n".join(get_doc_object(index_points_to_lines)["Notes"])

    assert notes.startswith("Measures:\n\n`id_measure` is an interpolative measure")


def test_python_argument_without_r_text_raises():
    """An argument with no R text cannot ship undocumented."""
    with pytest.raises(KeyError, match=r"make_index_ids\.flavor"):

        @doc("make_index_ids")
        def make_index_ids(network, *, mode="to", flavor=None):
            pass


def test_renamed_argument_maps_to_r_name():
    """A keyword maps a Python argument to the R argument A-9 renames."""

    @doc("make_index_ids", graph_mode="mode")
    def f(network, *, graph_mode="to"):
        pass

    params = get_doc_object(f)["Parameters"]

    assert params[1].name == "graph_mode"
    assert params[1].desc[0].startswith("Indicating the mode of the graph.")


def test_explicit_r_name_wins_over_same_name():
    """A keyword wins over the R argument the Python name also matches (A-9)."""

    @doc("navigate_network_dfs", direction="reset")
    def f(network, *, direction=False):
        pass

    params = get_doc_object(f)["Parameters"]

    assert params[1].name == "direction"
    assert params[1].desc[0].startswith("If `True`, reset graph for each start")


@pytest.mark.parametrize("alias", sorted(PER_FUNCTION))
def test_per_function_pairs_fire_once(alias):
    """Each per-function pair matches its R text once (A-13)."""
    assert per_function(alias, r_docs()[alias])[1] == [
        1 for pairs in PER_FUNCTION[alias].values() for _ in pairs
    ]


PUBLIC = [getattr(hydroloom, name) for name in sorted(hydroloom.__all__)]

# The hy_network_type title is a question; rewording it is roxygen work, the
# maintainer's (A-13), outside this docstring pass.
PUBLIC_ALLOWED = {"representation": {"SS03"}}


@pytest.mark.parametrize("func", PUBLIC, ids=lambda f: f.__name__)
def test_public_docstring_validates(func):
    """numpydoc.validate accepts each public function's docstring (A-13)."""
    result = validate(get_doc_object(func))
    codes = {code for code, _ in result["errors"]}
    allowed = ALLOWED_CODES | PUBLIC_ALLOWED.get(func.__name__, set())

    assert codes <= allowed, result["errors"]


def test_make_index_ids_returns_describe_index_ids():
    """The Returns text is Python's, with the tuple order for mode both."""
    doc_object = get_doc_object(hydroloom.make_index_ids)
    returns = " ".join(" ".join(r.desc) for r in doc_object["Returns"])
    text = hydroloom.make_index_ids.__doc__

    assert "IndexIds" in returns and "`(to, from)`" in returns
    assert "long_form" not in text
    assert "to_list" not in text and "adjacency matrix" not in text
    assert '"to" first and "from" second' in text


def test_python_only_arguments_are_documented():
    """as_hy's id_col and toid_col take their text from PYTHON_ARGUMENTS."""
    params = get_doc_object(hydroloom.as_hy)["Parameters"]

    assert [p.name for p in params] == ["network", "id_col", "toid_col"]
    assert params[1].desc[0].startswith("The input column to take as `id`")
