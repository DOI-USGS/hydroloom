# Hydrofabrics: A Context Pack

## What this covers

This pack summarizes the hydrofabric data theme — what a hydrofabric is, what it is made of, the problems of scale that shape it, and the line of US datasets that have served as hydrofabrics from RF1 through 3DHP. It then places the reference-and-derived distinction and the National Hydrologic Geospatial Fabric (NHGF) that the mainstems service sits inside. Source material is the USGS hydrofabric data theme at water.usgs.gov/themes/hydrofabric and the 3DHP and OGC background it links to. The purpose is to give the proposal its enterprise backdrop: why a stable, resolvable mainstem identifier is infrastructure the commons depends on, not a feature of one project.

## What a hydrofabric is

A hydrofabric is a digital mesh describing how water moves across the landscape — a category of dataset, in the way "house" is a category of building. The term implies a set of characteristics rather than a single specification. The "fabric" metaphor is two things woven together with no gaps: catchments (land areas where water collects) and flowpaths (connected river networks that move water downstream), with hydrologic locations such as gages and dams linked onto that weave. Like fabric woven at different sizes, a hydrofabric can be built at varying resolution, but always respects how water actually moves.

The key to the concept is function. A hydrofabric fulfills at least four functions: it partitions the landscape into hydrologically meaningful units, defines how networks of waterbodies connect and route flow downstream, provides geometric and physical characteristics of hydrologic features, and establishes a common framework for integrating data from different sources. Any given model or analysis draws on a subset of a larger, more complete hydrofabric.

No single hydrofabric serves every purpose at every resolution. A national water-availability assessment needs catchments aggregated to basin size; a local flood model needs drainage areas of a few hectares. This is why deriving fit-for-purpose hydrofabrics from stable reference datasets matters as much as building the reference datasets themselves — the distinction the NHGF is built around.

## Components

A hydrofabric is made of five component types, packaged separately by geometry but related so they can be reassembled.

- **Flowlines** represent flowing water as lines, usually digitized upstream to downstream. Packaged as a table with a unique identifier, a **mainstem identifier**, a name, and optional attributes (length, catchment area, connectivity).
- **Catchments** represent local drainage areas as a continuous polygon coverage. A catchment is the local drainage of exactly one flowline, or an internally drained or coastal area. Most catchments have a flowline and most flowlines have a catchment, but not all.
- **Network** represents how flowing waters connect, packaged as a table of upstream/downstream identifier pairs. It carries branching information — whether a connection is primary or secondary, and the fraction of flow to each branch below a diversion. Useful because many uses need connectivity without flowline geometry, and because it can express real-world connections not drawn as flowlines.
- **Waterbodies** represent areas of constant-enough water extent as non-overlapping polygons, on-network (one or more flowlines pass through) or off-network.
- **Linked hydrologic locations** are monitoring stations, infrastructure, and prediction points attached to the fabric but not inherent to it. They are the bridge between the spatial framework and real-world measurements — a gage linked to a flowline associates its observations with the upstream contributing area.

Beyond the components, nearly every use needs **extended attributes** — slope, roughness, land cover, soils, withdrawals, storage dynamics. These are largely purpose-built and shared for reuse rather than baked into the shared components.

## Scale and the awkward derivatives

In hydrology, scale is the resolution at which a phenomenon is observed — hillslope to reach to basin — and the levels are not independent. Two derived components are hard precisely because they only make sense in relation to the network around them.

**Incremental nested hydrologic units** group catchments into progressively larger drainage areas in a nested hierarchy (the WBD is the US example). Unlike catchments, they are delineated to fall within a target size range rather than following the one-flowline rule, which makes them good indexing units but not a core hydrofabric component. Because they nest, editing one boundary cascades up and down the hierarchy.

**Accumulated attributes** are values summed across everything draining to a point — drainage area is the familiar one, total upstream stream length and upstream land-cover fractions are others. Their values depend entirely on which catchments and connections were used, so two "drainage area" numbers for the same location are only comparable if they came from the same hydrofabric. This is a direct argument for stable reference datasets and for an identifier that ties the same feature across them.

## The dataset lineage: RF1 to 3DHP

Datasets that function as hydrofabrics have evolved over decades, though the term was coined in the 2020s.

- **RF1** (EPA River Reach File 1, 1990s) — the first national digital dataset of this kind, primarily flowlines with network attributes. ERF1 added many estimated attributes on top.
- **NHDPlusV1** (2005) — combined medium-resolution (1:100k) NHD with a DEM to make catchments. Not called a hydrofabric then, but it has all four core components in retrospect; a stepping stone to V2 for most.
- **NHDPlusV2** (2012) — the same pattern refined. Adopted broadly for EPA indexing and summarization and USGS SPARROW and national assessments, and it incorporated WBD boundaries as walls to make catchment and hydrologic-unit boundaries coincide. Its broad reuse is the clearest demonstration of the value of a stable reference hydrofabric.
- **NHDPlusHR** (national snapshot 2022) — V2 methods on high-resolution (1:24k or finer) hydrography and newer elevation. Hard to scale in production and heavy on tools built for V2, so adoption stayed limited.
- **3DHP** (Call to Action 2024) — the end of NHDPlusHR development and a complete redevelopment of US hydrography. 3DHP derives flowlines algorithmically from 3DEP LiDAR (ifsar in Alaska) rather than digitizing from maps, producing denser, terrain-consistent, z-enabled networks. It consolidates NHD, WBD, and NHDPlusHR capabilities into one dataset, is built on HY_Features concepts, and integrates into the Internet of Water through geoconnex.us. At the time of writing (winter 2026) it has incorporated several small regions into the national baseline and will be in production for some time.

## The Watershed Boundary Dataset

The WBD is the US example of incremental nested hydrologic units. The hydrologic unit code system, from Water Supply Paper 2294 in the mid-1980s, establishes a nested identifier system; current delineation rules date to 2009 with editions through 2022. WBD units do not follow the one-inflow/one-outflow catchment rule and have no flowlines, so they are not themselves a hydrofabric component, but each is usually delineated with a single outflow and one downstream unit, so flowline-boundary crossings let units be linked to a hydrofabric for summarization.

Four WBD snapshots are referenced to varying degrees: NHDPlusV2-WBD, WBD-2020, NHDPlusHR-WBD, and WBD-2024. The NHDPlus snapshots are composed of WBD content through time, matching the timeline of regional NHDPlus production; the year snapshots are point-in-time (2020 for the first National Water Availability Assessment, 2024 when WBD edits outside Alaska ended).

## Reference and derived hydrofabrics

Once a dataset is used more than once, later work derives from it, and it becomes a reference to some degree. NHDPlusV2's broad reuse shows the value of establishing stable reference hydrofabrics deliberately rather than by accident.

The theme is careful about two senses of "reference." A **reference framework** is a standard grid for locating things, like latitude and longitude — the fixed basis against which things are positioned. A **reference dataset** is the authoritative, canonical version others compare against. The first describes the nature of a system; the second describes the status of a version. The reference fabric is meant to be both.

The USGS National Hydrologic Model Geospatial Fabric (GF) carries this forward: GFv1 (2014), GFv1.1 (2021, adding transboundary basins; Alaska and Hawaii in 2024), and GFv2 (provisional winter 2026), which adds an explicit separation between reference and derived. For GFv2, pre-processed reference fabric components are saved and distributed so any study or model can derive from the same baseline.

## The National Hydrologic Geospatial Fabric

The NHGF is the USGS effort to steward the data and cyberinfrastructure required to maintain reference hydrofabric data, linked content, and the tools to produce and distribute hydrofabrics. Rather than treating each hydrofabric as an independent project, it maintains stable reference datasets, provides tools to derive application-specific hydrofabrics, ensures compatibility across modeling frameworks, and gives regional and domain users a path to contribute improvements back into the shared system.

This is the home of the mainstems service. The reference fabric is the stable baseline the NHGF stewards; the mainstem identifier is the persistent, scale-independent handle that ties the same river across the datasets in the lineage above; and the service is the lookup utility that makes that identifier resolvable, fast, and dependable at web scale. The accumulated-attribute and reference-dataset arguments are why the identifier has to be stable and why a USGS-stewarded access point for it is infrastructure rather than a convenience.

## Key references

- Hydrofabric data theme: `https://water.usgs.gov/themes/hydrofabric/`
- 3D Hydrography Program: `https://www.usgs.gov/3dhp`
- 3D National Topography Model Call to Action — Part 1 (CIR 1519): `https://doi.org/10.3133/cir1519`
- HY_Features conceptual model (OGC 14-111r6): `http://www.opengis.net/doc/IS/hy-features/1.0`
- WBD delineation rules (TM 11-A3): `https://doi.org/10.3133/tm11A3`
- Water Supply Paper 2294 (hydrologic unit codes): `https://doi.org/10.3133/wsp2294`
- Geospatial Fabric V2 (provisional): `https://www.sciencebase.gov/catalog/item/60be0e53d34e86b93891012b`
- Reference network (code): `https://code.usgs.gov/wma/nhgf/reference-fabric/reference-network`
