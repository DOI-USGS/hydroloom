"""Utilities to weave hydrologic fabrics."""

from importlib.metadata import version as _version

from hydroloom._hy import (
    align_names,
    as_hy,
    capabilities,
    hy_reverse,
    is_dendritic,
    is_hy,
    name_definitions,
    register_names,
    representation,
)
from hydroloom.network import (
    add_toids,
    add_topo_sort,
    make_index_ids,
    navigate_network_dfs,
    sort_network,
)

__version__ = _version("hydroloom")

__all__ = [
    "add_toids",
    "add_topo_sort",
    "align_names",
    "as_hy",
    "capabilities",
    "hy_reverse",
    "is_dendritic",
    "is_hy",
    "make_index_ids",
    "name_definitions",
    "navigate_network_dfs",
    "register_names",
    "representation",
    "sort_network",
]
