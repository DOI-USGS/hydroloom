"""Unit tests of the harness converters and comparison on the stored expectations."""

import functools
import re

import numpy as np
import pandas as pd
import pytest
from hypothesis import given, settings
from hypothesis import strategies as st

from conformance import (
    CONFORMANCE_DIR,
    EXPECTED_DIR,
    PYTHON_NAMES,
    Graph,
    assert_layer_equal,
    assert_raises,
    frame_to_layer,
    graph_to_layer,
    layer_to_frame,
    layer_to_graph,
    layer_to_scalar,
    layer_to_traversal,
    load_manifest,
    load_remedies,
    load_vocab,
    python_functions,
    python_name,
    read_expected,
    read_layer,
    remedy_pattern,
    scalar_to_layer,
    skip_reason,
    traversal_to_layer,
    write_layer,
)

MANIFEST = load_manifest()
FUNCTIONS = MANIFEST["functions"]
LAYER_CASES = [c for c in MANIFEST["cases"] if "raises" not in c]


def _compare_args(case):
    spec = FUNCTIONS[case["function"]]
    return {
        "returns": spec["returns"],
        "key": spec.get("key"),
        "compare": case["compare"],
        "order": case["order"],
    }


def _round_trip(layer, case):
    returns = FUNCTIONS[case["function"]]["returns"]
    if returns == "frame":
        return frame_to_layer(
            layer_to_frame(layer),
            columns=case.get("columns"),
            geometry=case.get("geometry", False),
        )
    if returns == "graph":
        return graph_to_layer(layer_to_graph(layer))
    if returns == "traversal":
        starts, result = layer_to_traversal(layer)
        return traversal_to_layer(result, starts)

    return scalar_to_layer(layer_to_scalar(layer))


@functools.cache
def _case(case_id):
    return next(c for c in MANIFEST["cases"] if c["id"] == case_id)


def _layer_with(function, columns, min_rows=2):
    """Return the first case of a function whose layer has the columns and rows."""
    for case in LAYER_CASES:
        if case["function"] != function:
            continue
        layer = read_expected(case)
        if set(columns) <= set(layer.columns) and len(layer) >= min_rows:
            return case, layer
    raise LookupError(f"no {function} layer with {columns}")


# Manifest and expectation files -------------------------------------------


def test_every_layer_case_has_its_layer():
    """Each case without ``raises`` has a layer in its expectation file (S-7)."""
    import pyogrio

    layers = {}
    for path in EXPECTED_DIR.glob("*.gpkg"):
        layers[path.name] = {name for name, _ in pyogrio.list_layers(path)}
    missing = [
        c["id"]
        for c in LAYER_CASES
        if c["id"] not in layers.get(c.get("expected", f"{c['function']}.gpkg"), ())
    ]

    assert missing == []


def test_case_ids_unique():
    """Case ids are unique, so pytest ids and layer names are (S-4)."""
    ids = [c["id"] for c in MANIFEST["cases"]]

    assert len(ids) == len(set(ids))


def test_vocabulary_reads_from_package_data():
    """The vocabulary comes through ``hydroloom.data`` with a type per column term."""
    vocab = load_vocab()

    assert vocab["id"]["type"] == "id"
    assert all("type" in t for t in vocab.values() if not t.get("concept"))


def test_python_names():
    """Python names map from the R export and default to it."""
    assert python_name("hy") == "as_hy"
    assert python_name("sort_network") == "sort_network"
    assert set(PYTHON_NAMES) <= set(FUNCTIONS)


@pytest.mark.parametrize("function", sorted(FUNCTIONS))
def test_python_name_in_signature(function):
    """The Python name is the one the description's Signature gives (S-4)."""
    text = (CONFORMANCE_DIR / "algorithms" / f"{function}.md").read_text("utf-8")
    signature = text.split("\n## Signature\n", 1)[1].split("\n## ", 1)[0]

    assert re.search(rf"`{re.escape(python_name(function))}[(`]", signature)


def test_skip_reason_names_function():
    """A case skips naming its function, for either reason."""
    case = {"function": "hy", "supported_by": ["r"]}
    assert "hy" in skip_reason(case, [])

    case = {"function": "hy", "supported_by": ["r", "python"]}
    assert "as_hy" in skip_reason(case, [])
    assert skip_reason(case, ["as_hy"]) is None


def test_python_functions_reads_supported_by():
    """The A-18 set holds the Python names of Python-supported cases."""
    manifest = {
        "cases": [
            {"function": "hy", "supported_by": ["r", "python"]},
            {"function": "add_toids", "supported_by": ["r"]},
        ]
    }

    assert python_functions(manifest) == {"as_hy"}


# Round trip and self comparison on every stored layer ----------------------


@pytest.mark.parametrize("case", LAYER_CASES, ids=[c["id"] for c in LAYER_CASES])
def test_layer_round_trip(case):
    """A stored layer converts to its return kind and back, and equals itself."""
    layer = read_expected(case)
    back = _round_trip(layer, case)

    assert_layer_equal(back, read_expected(case), **_compare_args(case))


# Perturbed copies fail, naming the column ---------------------------------


def test_identifier_value_change_fails():
    """A changed identifier fails on its column."""
    case, layer = _layer_with("sort_network", ["id", "toid"])
    bad = layer.copy()
    bad.loc[0, "toid"] = "not-an-id"

    with pytest.raises(AssertionError, match="toid"):
        assert_layer_equal(bad, layer, **_compare_args(case))


def test_identifier_as_integer_fails():
    """An identifier that is not ``str`` fails (I-1)."""
    case, layer = _layer_with("add_topo_sort", ["id"])
    bad = layer.copy()
    bad["id"] = np.arange(len(bad))

    with pytest.raises(AssertionError, match="'id'.*not str"):
        assert_layer_equal(bad, layer, **_compare_args(case))


def test_identifier_string_dtypes_compare_equal():
    """Object and string dtypes of the same identifiers compare equal (I-13)."""
    case, layer = _layer_with("add_topo_sort", ["id", "toid"])
    other = layer.copy()
    other["id"] = other["id"].astype(object)
    other["toid"] = other["toid"].astype(pd.StringDtype())

    assert_layer_equal(other, layer, **_compare_args(case))


def test_count_value_change_fails():
    """A changed count fails on its column."""
    case, layer = _layer_with("add_topo_sort", ["topo_sort"])
    bad = layer.copy()
    bad.loc[0, "topo_sort"] += 1

    with pytest.raises(AssertionError, match="topo_sort"):
        assert_layer_equal(bad, layer, **_compare_args(case))


def test_count_dtype_change_fails():
    """A count returned as int64 fails against the int32 vocabulary dtype (I-9)."""
    case, layer = _layer_with("add_topo_sort", ["topo_sort"])
    bad = layer.copy()
    bad["topo_sort"] = bad["topo_sort"].astype(np.int64)

    with pytest.raises(AssertionError, match="topo_sort"):
        assert_layer_equal(bad, layer, **_compare_args(case))


def _stored_form_layers():
    expected = pd.DataFrame(
        {
            "id": ["1", "2"],
            "toid": ["2", None],
            "nhdplusid": ["5", "6"],
            "topo_sort": np.array([2, 1], dtype=np.int32),
        }
    )
    actual = pd.DataFrame(
        {
            "id": [1, 2],
            "toid": [2.0, np.nan],
            "nhdplusid": [5, 6],
            "topo_sort": [2.0, 1.0],
        }
    )

    return actual, expected


def test_compare_values_brings_actual_to_stored_form():
    """compare: values converts identifiers and text, casts integers (S-4)."""
    actual, expected = _stored_form_layers()

    assert_layer_equal(actual, expected, compare="values")
    with pytest.raises(AssertionError, match="not str"):
        assert_layer_equal(actual, expected, compare="exact")


@pytest.mark.parametrize(
    ("column", "value"),
    [("id", 3), ("nhdplusid", 7), ("topo_sort", 2.5)],
)
def test_compare_values_still_compares_values(column, value):
    """A changed value fails under compare: values; a lossy cast is not made."""
    actual, expected = _stored_form_layers()
    actual[column] = actual[column].astype(object)
    actual.loc[1, column] = value

    with pytest.raises(AssertionError, match=column):
        assert_layer_equal(actual, expected, compare="values")


def test_compare_values_does_not_cast_text_to_integer():
    """Text where the layer stores integers fails under compare: values."""
    actual, expected = _stored_form_layers()
    actual["topo_sort"] = ["2", "1"]

    with pytest.raises(AssertionError, match="topo_sort"):
        assert_layer_equal(actual, expected, compare="values")


def test_count_outside_32_bit_compares_as_double():
    """A count outside the 32-bit range compares as double (I-15)."""
    expected = pd.DataFrame({"topo_sort": [2.0**40, 1.0]})
    actual = pd.DataFrame({"topo_sort": np.array([2**40, 1], dtype=np.int64)})

    assert_layer_equal(actual, expected)
    with pytest.raises(AssertionError, match="topo_sort"):
        assert_layer_equal(actual.assign(topo_sort=[2**40, 2]), expected)


def test_flag_compared_in_stored_type():
    """A flag is not coerced: integer 1/0 fails against stored bool (S-7)."""
    case, layer = _layer_with("hy", ["start_flag"])
    bad = layer.copy()
    bad["start_flag"] = bad["start_flag"].astype(np.int32)

    with pytest.raises(AssertionError, match="start_flag"):
        assert_layer_equal(bad, layer, **_compare_args(case))


def test_length_tolerance():
    """``length`` passes within 1 m under tolerance and fails beyond it (D-13)."""
    case, layer = _layer_with("add_topo_sort", ["length_km"])
    args = _compare_args(case)
    near = layer.copy()
    near["length_km"] += 5e-4
    far = layer.copy()
    far.loc[0, "length_km"] += 2e-3

    assert_layer_equal(near, layer, **{**args, "compare": "tolerance"})
    with pytest.raises(AssertionError, match="length_km"):
        assert_layer_equal(near, layer, **{**args, "compare": "exact"})
    with pytest.raises(AssertionError, match="length_km"):
        assert_layer_equal(far, layer, **{**args, "compare": "tolerance"})


def test_case_atol_overrides_type_atol():
    """A case ``atol`` replaces the D-13 type tolerance (S-4)."""
    case, layer = _layer_with("add_topo_sort", ["length_km"])
    args = {**_compare_args(case), "compare": "tolerance"}
    near = layer.copy()
    near["length_km"] += 5e-4

    with pytest.raises(AssertionError, match="length_km"):
        assert_layer_equal(near, layer, **args, atol=1e-6)


def test_measure_tolerance_per_row():
    """A measure's atol is one meter of the row's flowline, in percent (D-13)."""
    expected = pd.DataFrame({"length_km": [1.0, 0.1], "id_measure": [50.0, 50.0]})
    within = expected.assign(id_measure=[50.09, 50.9])
    beyond = expected.assign(id_measure=[50.2, 50.0])

    assert_layer_equal(within, expected, compare="tolerance")
    with pytest.raises(AssertionError, match="id_measure"):
        assert_layer_equal(beyond, expected, compare="tolerance")


def test_measure_tolerance_needs_length():
    """A measure under tolerance without ``length_km`` fails, naming both (D-13)."""
    expected = pd.DataFrame({"id_measure": [50.0]})

    with pytest.raises(AssertionError, match="'id_measure'.*length_km"):
        assert_layer_equal(expected, expected, compare="tolerance")


def test_order_ignore_sorts_frame_on_id():
    """An ``order: ignore`` frame sorts on ``id``, not on float values (D-7)."""
    expected = pd.DataFrame({"length_km": [1.0, 1.0005], "id": ["a", "b"]})
    actual = pd.DataFrame({"length_km": [1.0003, 1.0004], "id": ["b", "a"]})

    assert_layer_equal(actual, expected, compare="tolerance", order="ignore")


def test_aggregate_measure_rounded_exact():
    """``aggregate_id_measure`` rounded to four decimals is exact (D-13)."""
    expected = pd.DataFrame({"aggregate_id_measure": [12.3456]})

    assert_layer_equal(expected + 1e-6, expected)
    with pytest.raises(AssertionError, match="aggregate_id_measure"):
        assert_layer_equal(expected + 1e-3, expected, compare="tolerance")


def test_row_order():
    """Row order binds under ``order: exact`` and not under ``ignore`` (D-14)."""
    case, layer = _layer_with("add_topo_sort", ["id", "topo_sort"])
    args = _compare_args(case)
    shuffled = layer.iloc[::-1].reset_index(drop=True)

    assert_layer_equal(shuffled, layer, **{**args, "order": "ignore"})
    with pytest.raises(AssertionError):
        assert_layer_equal(shuffled, layer, **{**args, "order": "exact"})


def test_column_order_ignored_and_set_binds():
    """Columns compare by name; a missing or extra column fails, named (D-14)."""
    case, layer = _layer_with("add_topo_sort", ["id", "topo_sort"])
    args = _compare_args(case)

    assert_layer_equal(layer[layer.columns[::-1]], layer, **args)
    with pytest.raises(AssertionError, match="topo_sort"):
        assert_layer_equal(layer.drop(columns="topo_sort"), layer, **args)
    with pytest.raises(AssertionError, match="surplus"):
        assert_layer_equal(layer.assign(surplus=1), layer, **args)


def test_graph_neighbor_change_fails_and_rows_sorted_on_key():
    """A graph layer sorts on ``key``; a changed neighbor fails, named (S-7)."""
    case = _case("make_index_ids_worked_example")
    layer = read_expected(case)
    args = _compare_args(case)
    bad = layer.copy()
    bad.loc[0, "neighbor"] = "not-an-id"

    assert_layer_equal(layer.iloc[::-1], layer, **args)
    with pytest.raises(AssertionError, match="neighbor"):
        assert_layer_equal(bad, layer, **args)


def test_traversal_order_change_fails():
    """A changed visit order in a traversal layer fails, named (S-7, D-6)."""
    case, layer = _layer_with("navigate_network_dfs", ["order"], min_rows=3)
    bad = layer.copy()
    bad.loc[0, "id"], bad.loc[1, "id"] = layer.loc[1, "id"], layer.loc[0, "id"]

    with pytest.raises(AssertionError, match="'id'"):
        assert_layer_equal(bad, layer, **_compare_args(case))


def test_scalar_value_change_fails():
    """A scalar layer with the other value fails on ``value``."""
    case, layer = _layer_with("is_dendritic", ["value"], min_rows=1)
    flipped = scalar_to_layer(not layer_to_scalar(layer))

    with pytest.raises(AssertionError, match="value"):
        assert_layer_equal(flipped, layer, **_compare_args(case))


def test_frame_true_is_scalar_layer():
    """``check_hy_graph`` returning ``True`` is written as the scalar layer (D3-53)."""
    assert frame_to_layer(True).equals(pd.DataFrame({"value": [True]}))


def test_graph_pair_has_mode_column():
    """A mode both pair converts to one layer with ``mode`` (S-7)."""
    to = Graph(np.array([0, 1, 1]), np.array([1]), ["a", "b"])
    frm = Graph(np.array([0, 0, 1]), np.array([0]), ["a", "b"])
    layer = graph_to_layer((to, frm))

    assert list(layer["mode"]) == ["to", "from"]
    assert list(layer["id"]) == ["a", "b"]
    assert list(layer["neighbor"]) == ["b", "a"]


@settings(max_examples=50, derandomize=True)
@given(
    st.integers(0, 8).flatmap(
        lambda n: st.lists(
            st.tuples(st.integers(0, max(n - 1, 0)), st.integers(0, max(n - 1, 0))),
            max_size=3 * n,
        ).map(lambda edges: (n, edges))
    )
)
def test_graph_round_trip_property(graph):
    """Any CSR graph survives graph to layer to graph to layer."""
    n, edges = graph
    edges = sorted(edges) if n else []
    uniques = [f"n{i}" for i in range(n)]
    counts = np.bincount([e[0] for e in edges], minlength=n).astype(np.int64)
    indptr = np.concatenate([[0], np.cumsum(counts)]).astype(np.int64)
    indices = np.array([e[1] for e in edges], dtype=np.int64)
    layer = graph_to_layer(Graph(indptr, indices, uniques))

    back = graph_to_layer(layer_to_graph(layer))

    assert_layer_equal(back, layer, returns="graph", key=["id", "position", "neighbor"])


@settings(max_examples=50, derandomize=True)
@given(
    st.lists(
        st.lists(st.lists(st.sampled_from("abcdef"), min_size=1), min_size=1),
        max_size=4,
    )
)
def test_traversal_round_trip_property(result):
    """Any traversal survives traversal to layer to traversal to layer."""
    starts = [f"s{i}" for i in range(len(result))]
    layer = traversal_to_layer(result, starts)

    back_starts, back_result = layer_to_traversal(layer)

    assert back_starts == starts
    assert back_result == result


# Writing ------------------------------------------------------------------


def test_write_layer_round_trip(tmp_path):
    """A written layer reads back equal, and a second write replaces it."""
    case = _case("make_index_ids_worked_example")
    layer = read_expected(case)
    path = tmp_path / "make_index_ids.gpkg"

    write_layer(layer, path, case["id"])
    write_layer(layer, path, case["id"])

    assert_layer_equal(read_layer(path, case["id"]), layer, **_compare_args(case))


# Conditions ---------------------------------------------------------------


@pytest.mark.parametrize("key", sorted(load_remedies()))
def test_remedy_pattern_matches_formatted_message(key):
    """Each remedies template, formatted, matches its own pattern (A-10)."""
    entry = load_remedies()[key]
    template = entry.get("message_python", entry["message"])
    values = {name: "<value>" for name in entry.get("placeholders", {})}

    assert remedy_pattern(key).fullmatch(template.format(**values))
    assert not remedy_pattern(key).fullmatch("some other message")


def test_assert_raises_checks_key():
    """A case's error must carry its remedies text."""
    key = "needs_topo"
    entry = load_remedies()[key]
    text = entry["message_python"].format(function="f", current="hy")
    case = {"raises": "error", "raises_key": key}

    def good():
        raise ValueError(text)

    def other():
        raise ValueError("unrelated")

    assert_raises(good, case)
    with pytest.raises(AssertionError, match=key):
        assert_raises(other, case)
