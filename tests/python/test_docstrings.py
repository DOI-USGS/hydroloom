"""The A-13 docstring composer and its R-phrasing substitution table."""

from __future__ import annotations

import pytest
from numpydoc.docscrape import get_doc_object
from numpydoc.validate import validate

from hydroloom._docstrings import SUBSTITUTIONS, doc, r_docs, substitute

# Fires of each substitution pair over every field of every alias in
# docstrings.json, in table order. Regenerating docstrings.json can change
# these; the table and these counts are reviewed together (VERIFICATION 6).
EXPECTED_FIRES = [
    108, 29, 5, 7, 26, 23, 6, 3, 3, 1, 41, 11, 26, 7, 3, 3, 30, 6, 35, 1,
    1, 3, 3, 15, 1, 3, 4, 1, 3, 3, 3, 3, 7, 5, 1, 125, 1, 1, 1, 1,
    5, 1, 1, 9, 31, 36, 15, 5, 20, 3, 9, 6, 3, 6, 12, 6, 5, 6, 23, 5,
    1, 1,
]  # fmt: skip


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
    totals = [0] * len(SUBSTITUTIONS)
    for entry in r_docs().values():
        for text in _fields(entry):
            counts = substitute(text)[1]
            totals = [a + b for a, b in zip(totals, counts, strict=True)]

    assert totals == EXPECTED_FIRES
    assert min(totals) > 0


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
    assert "logical if TRUE" not in text
    # The hand-written summary is replaced by the R title.
    assert text.splitlines()[1] == r_docs()[func.__name__]["title"] + "."


def test_parameters_follow_python_signature():
    """R-only arguments are dropped and the Python order is kept."""
    params = get_doc_object(make_index_ids)["Parameters"]

    assert [p.name for p in params] == ["network", "mode"]
    assert params[0].desc == ["A table."]
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
