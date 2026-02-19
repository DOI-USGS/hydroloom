# Get or Set Hydroloom Names

Retrieve hydroloom name mapping from hydroloom environment. Hydroloom
uses a specific set of attribute names within the package and includes
mappings from names used in some data sources. This function will return
those names and can be used to set additional name mappings.

NOTE: these values will reset when R is restarted. Add desired settings
to a project or user .Rprofile to make long term additions.

## Usage

``` r
hydroloom_names(x = NULL, clear = FALSE)
```

## Arguments

- x:

  named character vector of additional names to add to the hydroloom
  environment. If not specified, no names will be added and the current
  value stored in the hydroloom environment will be returned.

- clear:

  logical if TRUE, all names will be removed and replaced with x.

## Value

named character vector containing hydroloom names with registered
attribute name mappings in `names`.

## Examples

``` r
hydroloom_names()
#>                      id3dhp                       comid 
#>                        "id"                        "id" 
#>                   nhdplusid                   featureid 
#>                        "id"                        "id" 
#>        permanent_identifier   from_permanent_identifier 
#>                        "id"                        "id" 
#>               reconciled_id               aggregated_id 
#>                        "id"                        "id" 
#>                        toid                     tocomid 
#>                      "toid"                      "toid" 
#>     to_permanent_identifier             reconciled_toid 
#>                      "toid"                      "toid" 
#>             aggregated_toid                      tonode 
#>                      "toid"                    "tonode" 
#>                    fromnode                  divergence 
#>                  "fromnode"                "divergence" 
#>         divergence_fraction                  wbareacomi 
#>       "divergence_fraction"                      "wbid" 
#>                   totdasqkm                       totda 
#>             "total_da_sqkm"             "total_da_sqkm" 
#>                    areasqkm                    lengthkm 
#>                   "da_sqkm"                 "length_km" 
#>                  pathlength                  arbolatesu 
#>             "pathlength_km"              "arbolate_sum" 
#>                    hydroseq                  uphydroseq 
#>                 "topo_sort"              "up_topo_sort" 
#>                  dnhydroseq                  dnminorhyd 
#>              "dn_topo_sort"        "dn_minor_topo_sort" 
#>                  terminalpa                  terminalfl 
#>        "terminal_topo_sort"             "terminal_flag" 
#>                   startflag                  levelpathi 
#>                "start_flag"                 "levelpath" 
#>                 levelpathid                    outletID 
#>                 "levelpath"       "levelpath_outlet_id" 
#>                  uplevelpat                  dnlevelpat 
#>              "up_levelpath"              "dn_levelpath" 
#>                  streamleve                     dnlevel 
#>              "stream_level"           "dn_stream_level" 
#>                  streamorde                  streamcalc 
#>              "stream_order"         "stream_calculator" 
#>                       ftype                       fcode 
#>              "feature_type"         "feature_type_code" 
#>                       vpuid                       rpuid 
#>          "vector_proc_unit"          "raster_proc_unit" 
#>                   reachcode                  mainstemid 
#>              "aggregate_id"              "aggregate_id" 
#>                  reach_meas           reachcode_measure 
#>      "aggregate_id_measure"      "aggregate_id_measure" 
#>                    frommeas                      tomeas 
#> "aggregate_id_from_measure"   "aggregate_id_to_measure" 
#>     mainstemid_from_measure       mainstemid_to_measure 
#> "aggregate_id_from_measure"   "aggregate_id_to_measure" 
```
