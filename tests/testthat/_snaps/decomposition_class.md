# print.domain_decomposition cheap mode (default) is snapshot-stable

    Code
      print(d)
    Output
      <domain_decomposition: 1 trunks, 15 compacts, 62 catchments>
        domains:         16  (1 trunks, 15 compacts)
        domain_graph:    15 edges
        nexus_registry:  16 nexuses
        overrides:       0 rows
        source_network:  62 catchments
      
      # Use print(x, full = TRUE) for the full tree summary

# print.domain_decomposition full mode is snapshot-stable

    Code
      print(d, full = TRUE)
    Output
      <domain_decomposition: 1 trunks, 15 compacts, 62 catchments>
      ├─ source_network         <hy_leveled>       62 rows
      ├─ domains                <list>             16 elements
      │  │
      │  ├─ <1 trunk domains>
      │  │     catchments    min     18   median     18   max     18   total 18
      │  │     stream_order  min      4   median      4   max      4
      │  │     dendritic     TRUE  (1)
      │  │
      │  └─ <15 compact domains>
      │        catchments    min      1   median      1   max     16   total 44
      │        stream_order  min      1   median      1   max      3
      │        dendritic     TRUE  (15)
      │
      ├─ domain_graph           <data.frame>       15 rows
      │     relation_type flow (15)
      │     nexus_position (not yet populated)
      │
      ├─ nexus_registry         <data.frame>       16 rows
      ├─ catchment_domain_index <named character>  62 entries
      └─ overrides              <NULL>             0 rows   (none)
      
      # Use get_domain(x, "trunk_5329303_1")

