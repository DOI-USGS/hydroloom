"""The A-13 docstring composer and its R-phrasing substitution table."""

from __future__ import annotations

import re

import pytest
from numpydoc.docscrape import get_doc_object
from numpydoc.validate import validate

import hydroloom
from hydroloom._docstrings import (
    SUBSTITUTIONS,
    doc,
    r_docs,
    substitute,
)

# Fires of each substitution pair over every field of every alias in
# docstrings.json, keyed by pattern. Regenerating docstrings.json can change
# these; the table and these counts are reviewed together (VERIFICATION 6).
EXPECTED_FIRES = {
    r"(?s)(?:\A|\n\n)In R,.*?(?=\n\n(?!- |\d+\. )|\Z)": 15,
    r" In R,[^\n]*?[.?!](?=\s|\Z)": 26,
    r"data\.frame network compatible with hydroloom_names": 109,
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
    r"data\.frame": 120,
    r"or list as returned by make_index_ids": 1,
    r"an `hy` object": 6,
    r"`hy` object": 2,
    r"\bhy object": 12,
    r"an `hy`": 1,
    r"required attributes": 11,
    r"requires attributes": 1,
    r"\(character\)": 1,
    r"\(character:": 1,
    r"\(logical\)": 1,
    r"a logical flag": 5,
    r"a logical value": 1,
    r"logical columns": 1,
    r"`(\w+) = TRUE`": 8,
    r"`(\w+) = FALSE`": 5,
    r"`TRUE`": 10,
    r"`FALSE`": 32,
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
    r"(?<!CRS of )`x`": 98,
    r"c\(UM, DM, UT, DD\) or equivalently c\(upmain, downmain, up, down\)": 6,
    r"`hy\(\)`": 27,
    r"`hy_network_type\(\)`": 1,
    r"`hy_capabilities\(\)`": 5,
    r"`is\.hy\(\)`": 1,
    r"\bhydroloom_name_definitions\b": 1,
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
    """The Returns text is the shared text, with the "to" then "from" order."""
    doc_object = get_doc_object(hydroloom.make_index_ids)
    returns = " ".join(" ".join(r.desc) for r in doc_object["Returns"])
    text = hydroloom.make_index_ids.__doc__

    assert "IndexIds" in doc_object["Returns"][0].type
    assert 'For mode "both", the "to" and then the "from" index ids.' in returns
    assert "long_form" not in text
    assert "to_list" not in text and "adjacency matrix" not in text
    assert (
        'If "both", the result holds the "to" and then the "from" version of '
        "the graph." in " ".join(text.split())
    )


def test_python_only_arguments_are_documented():
    """as_hy's id_col and toid_col take their text from PYTHON_ARGUMENTS."""
    params = get_doc_object(hydroloom.as_hy)["Parameters"]

    assert [p.name for p in params] == ["network", "id_col", "toid_col"]
    assert params[1].desc[0].startswith("The input column to take as `id`")


R_ONLY = [
    "S3",
    "subclass",
    "add_topo",
    "not a hydroloom object",
    "row number",
    "In R,",
    "unqiue",
    "froms",
    "long_form",
    "s3 class",
    "hy object",
    "hydroloom class",
    "`x`",
]


@pytest.mark.parametrize("name", ["as_hy", "representation", "make_index_ids"])
def test_public_docstrings_drop_r_classes(name):
    """The "In R," drop pairs remove R class and argument text (A-13, P-4)."""
    text = getattr(hydroloom, name).__doc__

    for phrase in R_ONLY:
        assert phrase not in text


DROP_CASES = {
    "sentence-mid-paragraph": (
        "Shared one. In R, only R. Shared two.",
        "Shared one. Shared two.",
    ),
    "sentence-ends-field": ("Shared one. In R, only R.", "Shared one."),
    "sentence-ends-in-question": (
        "Shared one. In R, is it R? Shared two.",
        "Shared one. Shared two.",
    ),
    "paragraph-at-field-start": ("In R, only R.\n\nShared.", "Shared."),
    "paragraph-then-bullets": (
        "Shared.\n\nIn R, these:\n\n- one\n- two\n\nShared two.",
        "Shared.\n\nShared two.",
    ),
    "paragraph-then-numbered": (
        "Shared.\n\nIn R, these:\n\n1. one\n2. two\n\nShared two.",
        "Shared.\n\nShared two.",
    ),
    "paragraph-then-shared-paragraph": (
        "Shared.\n\nIn R, only R.\n\nShared two.",
        "Shared.\n\nShared two.",
    ),
}


@pytest.mark.parametrize(
    ("text", "expected"), DROP_CASES.values(), ids=DROP_CASES.keys()
)
def test_in_r_drops(text, expected):
    """The two "In R," pairs drop each R-only case the code comment states."""
    assert substitute(text)[0].strip() == expected


def test_no_in_r_left_after_substitution():
    """No field of any alias keeps "In R," once substituted (A-13, P-4)."""
    for alias, entry in r_docs().items():
        for text in _fields(entry):
            assert "In R," not in substitute(text)[0], alias


def test_in_r_sentences_hold_no_abbreviation():
    """An "In R," sentence ends at its first `.`, `?` or `!`, so no "e.g."."""
    for alias, entry in r_docs().items():
        for text in _fields(entry):
            for match in re.finditer(r"In R,[^\n]*?[.?!](?=\s|\Z)", text):
                assert not re.search(r"\b(?:e\.g|i\.e)\.", match.group()), alias


def test_wholly_r_only_fields_add_nothing(monkeypatch):
    """A field that substitutes to nothing adds no part, Returns, or blank run."""
    entry = {
        "title": "Synthetic title",
        "description": "In R, only R.",
        "details": "Shared details.",
        "arguments": {"x": "In R, only R.", "mode": "Shared mode."},
        "value": "In R, a list.",
        "sections": {"Only R": "In R, only R."},
    }
    monkeypatch.setattr("hydroloom._docstrings.r_docs", lambda: {"synthetic": entry})

    @doc("synthetic")
    def f(network, *, mode="to"):
        pass

    text = f.__doc__

    assert "In R," not in text
    assert "\n\n\n" not in text
    assert "Returns" not in text
    assert "Notes" not in text
    assert "Shared details." in text
    assert "network\nmode : default 'to'\n    Shared mode." in text
