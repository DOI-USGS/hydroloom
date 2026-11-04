<!-- Generated from manifest.yaml `supported_by` by
     tests/testthat/helper-conformance-parity.R. Do not edit by hand.
     Regenerate: HYDROLOOM_WRITE_EXPECTED=1, then
     devtools::test(filter = "conformance-parity"). -->

# Parity table

Suite version 0.1.0. A function's `supported_by` lists the languages that support every one of its cases that carries no `r_only`. A case with `r_only` is R-only by design and counts apart, in the `r_only` column (SPEC S-4, RULES P-5); a function whose every case carries it reads `r, python` when its Python name is in `hydroloom.__all__`, and `r` otherwise (SPEC S-4, RULES P-3).

| function | Python | supported_by | cases | r_only |
| --- | --- | --- | --- | --- |
| `hy` | `as_hy` | r, python | 38 | 13 |
| `hy_reverse` | `hy_reverse` | r, python | 11 | 9 |
| `is.hy` | `is_hy` | r, python | 15 | 2 |
| `align_names` | `align_names` | r, python | 26 | 2 |
| `hydroloom_name_definitions` | `name_definitions` | r, python | 1 | 0 |
| `hydroloom_names` | `register_names` | r | 2 | 2 |
| `hy_network_type` | `representation` | r, python | 21 | 7 |
| `hy_capabilities` | `capabilities` | r | 26 | 12 |
| `is_dendritic` | `is_dendritic` | r | 26 | 3 |
| `add_toids` | `add_toids` | r, python | 35 | 19 |
| `make_index_ids` | `make_index_ids` | r | 33 | 3 |
| `check_hy_graph` | `check_hy_graph` | r | 47 | 0 |
| `sort_network` | `sort_network` | r | 69 | 2 |
| `add_topo_sort` | `add_topo_sort` | r | 37 | 1 |
| `navigate_network_dfs` | `navigate_network_dfs` | r | 67 | 1 |
