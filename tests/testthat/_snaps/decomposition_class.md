# print.domain_decomposition cheap mode (default) is snapshot-stable

    Code
      print(d)
    Output
      <domain_decomposition: 1 basins, 1 domains, 62 catchments>
        domains:              1
        domain_connectivity:  1 basins
        nexus_registry:       1 nexuses
        overrides:            0 rows
        source_network:       62 catchments
      
      # Use print(x, full = TRUE) for the full tree summary

# print.domain_decomposition full mode is snapshot-stable

    Code
      print(d, full = TRUE)
    Output
      <domain_decomposition: 1 basins, 1 domains, 62 catchments>
      ├─ source_network         <hy_leveled>       62 rows
      ├─ domains                <list>             1 elements
      │  │
      │  └─ <1 domains>
      │        catchments    min     62   median     62   max     62   total 62
      │        stream_order  min      4   median      4   max      4
      │        dendritic     TRUE  (1)
      │
      ├─ domain_connectivity    <list>             1 elements
      │     catchments    min     18   median     18   max     18   total 18
      │
      ├─ nexus_registry         <data.frame>       1 rows
      ├─ catchment_domain_index <named character>  62 entries
      └─ overrides              <NULL>             0 rows   (none)
      
      # Use get_domain(x, "domain_5329303_5329303")

