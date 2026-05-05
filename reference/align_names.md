# Align Names to Hydroloom Convention

this function aligns the attribute names in x with those used in
hydroloom. See
[hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md)
for how to add more attribute name mappings if the attributes in your
data are not supported.

See
[hydroloom_name_definitions](https://doi-usgs.github.io/hydroloom/reference/hydroloom_name_definitions.md)
for definitions of the names used in hydroloom.

## Usage

``` r
align_names(x)
```

## Arguments

- x:

  data.frame network compatible with
  [hydroloom_names](https://doi-usgs.github.io/hydroloom/reference/hydroloom_names.md).

## Value

data.frame renamed to match hydroloom as possible.

## Examples

``` r
x <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))

names(x)
#>  [1] "COMID"      "GNIS_ID"    "GNIS_NAME"  "LENGTHKM"   "REACHCODE" 
#>  [6] "WBAREACOMI" "FTYPE"      "FCODE"      "StreamLeve" "StreamOrde"
#> [11] "StreamCalc" "FromNode"   "ToNode"     "Hydroseq"   "LevelPathI"
#> [16] "Pathlength" "TerminalPa" "ArbolateSu" "Divergence" "StartFlag" 
#> [21] "TerminalFl" "DnLevel"    "UpLevelPat" "UpHydroseq" "DnLevelPat"
#> [26] "DnMinorHyd" "DnDrainCou" "DnHydroseq" "FromMeas"   "ToMeas"    
#> [31] "RtnDiv"     "VPUIn"      "VPUOut"     "AreaSqKM"   "TotDASqKM" 
#> [36] "geom"      

x <- align_names(x)

names(x)
#>  [1] "id"                        "GNIS_ID"                  
#>  [3] "GNIS_NAME"                 "length_km"                
#>  [5] "aggregate_id"              "wbid"                     
#>  [7] "feature_type"              "feature_type_code"        
#>  [9] "stream_level"              "stream_order"             
#> [11] "stream_calculator"         "fromnode"                 
#> [13] "tonode"                    "topo_sort"                
#> [15] "levelpath"                 "pathlength_km"            
#> [17] "terminal_topo_sort"        "arbolate_sum"             
#> [19] "divergence"                "start_flag"               
#> [21] "terminal_flag"             "dn_stream_level"          
#> [23] "up_levelpath"              "up_topo_sort"             
#> [25] "dn_levelpath"              "dn_minor_topo_sort"       
#> [27] "DnDrainCou"                "dn_topo_sort"             
#> [29] "aggregate_id_from_measure" "aggregate_id_to_measure"  
#> [31] "RtnDiv"                    "VPUIn"                    
#> [33] "VPUOut"                    "da_sqkm"                  
#> [35] "total_da_sqkm"             "geom"                     
```
