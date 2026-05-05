# Hydroloom Name Definitions

A names character vector containing definitions of all attributes used
in the hydroloom package.

## Value

named character vector with `hydroloom_names` class to support custom
print method

## Examples

``` r
hydroloom_name_definitions
#> 1 "id": 
#>   shared network identifier for catchment divide and flowpath or flowline
#> 2 "toid": 
#>   indicates to the downstream id. May or may not be dendritic
#> 3 "fromnode": 
#>   indicates the node representing the nexus upstream of a catchment
#> 4 "tonode": 
#>   indicates the node representing the nexus downstream of a catchment
#> 5 "divergence": 
#>   indicates whether a catchment is not downstream of a divergence (0), the primary path downstream of a divergence (1), or a diversion downstream of a divergence (2).
#> 6 "divergence_fraction": 
#>   Indicates the fraction of flow to be apportioned to a given diverted path. Should sum to 1 when considering all diverted flowlines downstream of a diversion.
#> 7 "wbid": 
#>   waterbody id
#> 8 "total_da_sqkm": 
#>   total drainage area at the outlet of a catchment
#> 9 "da_sqkm": 
#>   local drainage area of a catchment
#> 10 "length_km": 
#>   length of a single catchment's flowpath
#> 11 "pathlength_km": 
#>   distance from the outlet of a catchment to the terminal outlet of a network
#> 12 "arbolate_sum": 
#>   total accumulated length of all upstream flowlines
#> 13 "topo_sort": 
#>   Similar to hydrosequence in NHDPlus. Large topo_sort values are upstream of small topo_sort values. Note that there are many valid topological sort orders of a directed graph.
#> 14 "up_topo_sort": 
#>   topo sort value of the upstream mainstem
#> 15 "dn_topo_sort": 
#>   topo sort value of the downstream mainstem
#> 16 "dn_minor_topo_sort": 
#>   topo sort value of the downstream minor network element with the smallest id
#> 17 "terminal_topo_sort": 
#>   topo sort value of the outlet network element
#> 18 "terminal_flag": 
#>   1 for network terminous 0 for within network
#> 19 "terminal_id": 
#>   id of terminal catchment for entire drainage basin
#> 20 "start_flag": 
#>   1 for a headwater, 0 otherwise
#> 21 "levelpath": 
#>   provides an identifier for the collection of flowlines that make up a single mainstem flowpath of a drainage basin
#> 22 "up_levelpath": 
#>   levelpath value of the upstream mainstem
#> 23 "dn_levelpath": 
#>   levelpath value of the downstream mainstem
#> 24 "levelpath_outlet_id": 
#>   id of outlet catchment of a levelpath
#> 25 "stream_level": 
#>   starting at 1 for coastal terminals and 4 for inland terminals increments by 1 for each smaller tributary level
#> 26 "dn_stream_level": 
#>   stream level of downstream mainstem network element
#> 27 "stream_order": 
#>   starting at 1 for headwaters increments by 1 for each larger tributary level, diversions adopt stream order from upstream but returning diverted network does not increment stream order
#> 28 "stream_calculator": 
#>   starting at 1 for headwaters and 0 along diverted paths increments by 1 for each larger tributary level, does no increment along diverted paths. Is equal to stream_order along the dendritic network
#> 29 "feature_type": 
#>   descriptive feature type moniker
#> 30 "feature_type_code": 
#>   compact feature type identifier
#> 31 "vector_proc_unit": 
#>   identifier for processing units based on vector encapsulation
#> 32 "raster_proc_unit": 
#>   identifier for processing units based on raster encapsulation
#> 33 "id_measure": 
#>   interpolative linear reference measure along a single identified feature
#> 34 "aggregate_id": 
#>   aggregate identifier useful for 'reach' or 'flowpath' aggregation of flowlines
#> 35 "aggregate_id_measure": 
#>   interpolative linear reference measure along an aggregate feature
#> 36 "aggregate_id_from_measure": 
#>   interpolative linear reference for downstream end of a single feature that makes up an aggregate feature
#> 37 "aggregate_id_to_measure": 
#>   interpolative linear reference for the upstream end of a single feature that makes up an aggregate feature
#> 38 "point_id": 
#>   identifier of hydrologic location point
#> 39 "offset": 
#>   offset distance from point to line in units of linear reference analysis units
#> 40 "upmain": 
#>   indicates that a given network element is the primary upstream connection at a confluence
#> 41 "downmain": 
#>   indicates that a given network element is the primary downstream connection at a confluence
```
