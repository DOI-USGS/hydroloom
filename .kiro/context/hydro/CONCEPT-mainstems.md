# Mainstems and Reference Rivers: A Context Pack

## What this covers

This pack summarizes four interlocking resources that together define how persistent mainstem river identifiers are produced, admitted to the community reference registry, and used for cross-dataset integration in US water data infrastructure. The resources are a USGS data release (Mainstems v3), an intermediate USGS data release (3DHP 2024), a GitHub workflow repository (ref_rivers) with its release notes, and an OGC engineering report (25-045) that formalizes the underlying data model. Each resource plays a distinct role in practice, and the boundaries between them matter for reasoning about what's authoritative at each layer.

## The four resources at a glance

**Mainstems v3 data release** — Blodgett, 2026-02-04, DOI [10.5066/P13LNDDQ](https://doi.org/10.5066/P13LNDDQ), ScienceBase item [65cbc0b3d34ef4b119cb37e9](https://www.sciencebase.gov/catalog/item/65cbc0b3d34ef4b119cb37e9). Formal publication of the CONUS mainstem rivers dataset with crosswalks to five hydrographic datasets and four WBD snapshots. Contains ~1.01M mainstems identified by `lp_mainstem_v3` levelpaths.

**3DHP 2024 intermediate release** — Blodgett, Ritmiller, Gold, Hayes, Wieczorek, 2024-09-17, DOI [10.5066/P94H0DAG](https://doi.org/10.5066/P94H0DAG), ScienceBase item [650cba99d34e823a0273a790](https://www.sciencebase.gov/catalog/item/650cba99d34e823a0273a790). Network topology and mainstem identifiers for the final High-Resolution NHD as the basis for the 3D Hydrography Program. Sits methodologically between v2 and v3 of the Mainstems release.

**ref_rivers workflow repository** — [github.com/internetofwater/ref_rivers](https://github.com/internetofwater/ref_rivers). Internet of Water workflow that consumes Mainstems releases and mints/maintains the `https://geoconnex.us/ref/mainstems/{id}` persistent URIs. NEWS.md tracks registry-level changes across four releases (v1, v2, v3, v3.1, v3.2).

**OGC 25-045 engineering report** — Blodgett (ed.), 2026-04-16, [docs.ogc.org/per/25-045.html](https://docs.ogc.org/per/25-045.html). Hydrology DWG engineering report: "Logical data model for hydrographic data based on HY_Features concepts." Specifies the hydrofabric logical data model that the operational pipeline implements.

## The pipeline

The resources form a producer/consumer chain rather than peer sources. Information flows in one direction and the semantics at each stage matter for downstream reasoning.

```
NHDPlusV2 + NHDPlusHR + NHD-3DHP (source hydrography)
      ↓
E2NHD-NWM-Network v3 (updated network attributes, P13IRYTB)
      ↓
Mainstems v3 data release (P13LNDDQ) — ~1.01M lp_mainstem_v3 levelpaths
      ↓
ref_rivers workflow — admission criterion + URI minting
      ↓
geoconnex.us/ref/mainstems registry — ~853k persistent URIs
      ↓
reference.geoconnex.us landing pages + NLDI navigation
```

OGC 25-045 describes the logical model that this pipeline implements, but it isn't itself part of the pipeline — it's the abstract specification against which the implementation is evaluated.

## Mainstems v3 in detail

The v3 data release is the source table. It enumerates every CONUS mainstem levelpath from the August 2024 update of the E2NHD-NWM-Network ([P13IRYTB](https://doi.org/10.5066/P13IRYTB)) and provides crosswalks to five hydrographic networks:

- ERF1 1999 (Enhanced River Reach File 1.2)
- NHDPlusV1 2010
- NHDPlusV2 2015
- NHDPlusHR 2024 (High-Resolution National Release 2)
- NHD-3DHP 2024 (final NHD snapshot used to initialize 3DHP)

And four WBD snapshots:

- NHDPlusV2 snapshot
- October 2020 snapshot
- NHDPlusHR snapshot
- January 2025 (final) snapshot

The primary identifier is `lp_mainstem_v3`. The crosswalk `mainstem_lookup_v2_v3.csv` contains 1,012,973 rows, of which 33,890 carry a `ref_mainstem` URI (carried forward from v2) and the remaining 979,083 are NA. Twelve small v2 mainstems were merged into downstream mainstems during network updates; no reference mainstems were lost. The data release's relationship to v2 is additive — v3 inherits v2's reference URIs, adds the high-resolution crosswalks that were absent in v2, and updates network connectivity where the underlying E2NHD-NWM-Network changed.

The superseding citation on v2's ScienceBase page ([63cb38b2d34e06fef14f40ad](https://www.sciencebase.gov/catalog/item/63cb38b2d34e06fef14f40ad)) points here. The v3 release's `mainstem_schematic.png` and `dataset_lookups.png` attachments are useful visual references for explaining the cross-dataset structure to non-technical audiences.

## 3DHP 2024 as the bridge

The 3DHP 2024 release ([P94H0DAG](https://doi.org/10.5066/P94H0DAG)) is where the mainstem identifier system was extended to high-resolution hydrography for the first time. It doesn't mint new reference URIs — that's ref_rivers' job — but it produces the HR-NHD-to-mainstem crosswalk that the v3 admission criterion depends on.

The release uses a three-tier assignment rule:

1. **~130 rivers crossing four-digit HUC boundaries** — manual headwater selection and dendritic trace validation.
2. **~30,000 reference mainstems from NHDPlusV2** — automated matching to the HR NHD network using the algorithm from Blodgett et al. 2021, with targeted manual validation. This tier preserves the ref_rivers registry's persistence contract: the same URI gets a new, higher-resolution representation.
3. **All other in-network NHD flowlines** — assigned mainstem identifiers based on NHD-derived levelpath in a separate namespace `https://geoconnex.us/usgs/mainstems/`.

The dual-namespace pattern in the 3DHP release (`ref/mainstems` + `usgs/mainstems`) is a validation-status distinction, not a resolution distinction. `ref/` is community-validated and admitted to the reference registry; `usgs/` is USGS-minted and either awaiting validation (for CONUS) or structurally outside the reference registry's scope (OCONUS, where NHDPlusV2 doesn't exist). Most of the ~819k additions that landed in ref_rivers v3 are promotions from `usgs/` to `ref/` after the three-network validation.

One metadata note worth flagging: the 3DHP 2024 XML has four instances of `geoconnes.us` (typo for `geoconnex.us`) in the attribute domain descriptions at lines 503, 897, 911, and 1051. Specifically the rows that reference _both_ namespaces. Rows that reference only `usgs/mainstems` are spelled correctly.

## The ref_rivers workflow and registry

The `ref_rivers` repository consumes Mainstems data releases and produces the persistent `geoconnex.us/ref/mainstems/{id}` URIs served through pygeoapi at `reference.geoconnex.us/collections/mainstems/`. The README establishes three critical facts:

- A mainstem is defined by a triple: a headwater catchment identifier, an outlet catchment identifier, and the dataset namespace those identifiers live in.
- The integer in the URI derives from the source dataset's `levelpathi` — opaque, not semantically meaningful.
- Identifiers are never removed. When source data improves for the same real-world river (same headwater/outlet), the URI persists and the new representation is added alongside the old. When the network path itself changes (different connectivity between endpoints), the URI is marked `superseded: true` with `new_mainstemid` populated pointing to replacements.

NEWS.md documents the registry's evolution:

**v1** — 33,853 URIs minted from Mainstems v1 ([P9BTKP3T](https://doi.org/10.5066/P9BTKP3T)) against NHDPlusV2.1 comids. Known bug affecting ~300 of these noted for future correction.

**v2 (Dec 2022 / 2023)** — Minor update pointing to Mainstems v2 ([P92U7ZUT](https://doi.org/10.5066/P92U7ZUT)) with the updated E2NHD-NWM-Network ([P976XCVT](https://doi.org/10.5066/P976XCVT)) as the connecting network. Not a re-mint: 33,564 URIs updated in place, 161 superseded due to outlet movement, 23 retained but dropped from lookup (empty geometry), 105 new rows added for new outlet locations. ~33.9k URIs active.

**v3** — The major expansion. 819,000 new URIs added from Mainstems v3 ([P13LNDDQ](https://doi.org/10.5066/P13LNDDQ)) under the three-network admission criterion (headwater, outlet, and path must exist in NHDPlusV2, NHDPlusHR, _and_ the 3DHP snapshot). Of 33,900 v2 URIs, 33,745 remain active; 155 (<0.05%) superseded, all in transboundary basins where NHDPlusHR replaces NHDPlusV2 as source geometry. ~28k candidates that met the match criterion were held back because admitting them would leave them isolated in the registry network. Sixteen wholly-Canadian mainstems added for US network completeness. Registry now ~853k.

**v3.1** — Post-process validation release. Adds lookup from mainstem URI to NHDPlusHR ID. 15 late-validation supersessions.

**v3.2** — Bug-fix release. Fixes topo_sort for HR representations, patches missing HR head/outlet entries (#18), fixes major cross-domain river outlet IDs (#22), updates NHDPlusHR outlet IDs (#21), adds validation checks.

The specific HU04s where NHDPlusHR now serves as source geometry for reference rivers are enumerated in NEWS.md — transboundary basins in the Northeast, Great Lakes, North Central, Northwest, South Central, Southwest, Pacific Northwest, and Pacific Southwest. A list of newly superseded mainstems is maintained at `data/review/deprecated_v3.geojson` within the repository, and a list of mainstems with significant headwater/outlet movement is at `data/review/changelog_v3.csv`.

## OGC 25-045 and the abstract model

OGC 25-045 ([docs.ogc.org/per/25-045.html](https://docs.ogc.org/per/25-045.html)) is the engineering report that formalizes the hydrofabric logical data model implemented by everything above. It's based on WaterML2 Part 3 — Surface Hydrology Features (HY_Features, [OGC 14-111r6](http://www.opengis.net/doc/IS/hy-features/1.0)) and documents design rationale, use cases, background context, and outstanding issues. Several sections are directly load-bearing for understanding the operational pipeline.

**§7.9 Reference Flow Network.** Establishes the concept of a most-resolved, most-validated network that all other datasets cross-reference to. The three-network admission criterion used by ref_rivers v3 is the operational version of this: a reference mainstem must agree across NHDPlusV2, NHDPlusHR, and NHD-3DHP before it gets a persistent URI.

**§9.1 Three Classes of HY_Flowpath.** Distinguishes flowline (most resolved, linear feature, may not have a catchment), flowpath (aggregates flowlines between hydrolocations, 1:1 with catchments), and mainstem (least resolved, composite of flowlines from headwater to outlet). Only mainstems carry persistent identifiers. This is why the admission criterion operates at the mainstem level and why registry growth scales with levelpath density, not flowline density.

**§9.2 Role of Mainstem Identifiers.** The economic argument for the three-class split. Persistent identifiers are expensive to maintain — conflation cost scales with identifier count — so the identifier set must be kept as small as is still sufficient to reference every on-network feature. Mainstems are the minimal set. Every "on-network" flowline inherits a mainstem identifier; other flowline-level identifiers are treated as internal to a dataset and don't persist.

**§7.5 Mainstems and Drainage Basins.** Describes the mainstem-as-backbone interpretation formalized in [Blodgett et al. 2021](https://doi.org/10.1016/j.envsoft.2020.104927). A mainstem is the predominant pathway of a drainage basin; nested drainage basins form a tree whose branches are mainstems. Useful background for explaining why the identifier system is cross-scale compatible.

**Annex A.** Summaries of the data models that fed into the hydrofabric logical model: ERF1, WBD, NHD, NHDPlus, Reference and Derived Hydrofabrics ([P9NFPB5S](https://doi.org/10.5066/P9NFPB5S)), NextGen NWM Hydrofabric. This is the single best reference for understanding how each source dataset maps (or doesn't map) into the hydrofabric feature types.

**Outstanding issues (§10.2).** Large waterbodies and the "frontal catchment" problem, non-fluvial landscapes (karst, wetlands, glacial outwash), and complex island-surrounding channel systems remain unresolved in the logical model. Worth knowing when someone proposes an application in these domains.

## Key identifiers and URIs

**Data release DOIs**

- Mainstems v1: [10.5066/P9BTKP3T](https://doi.org/10.5066/P9BTKP3T)
- Mainstems v2: [10.5066/P92U7ZUT](https://doi.org/10.5066/P92U7ZUT)
- Mainstems v3: [10.5066/P13LNDDQ](https://doi.org/10.5066/P13LNDDQ)
- 3DHP 2024: [10.5066/P94H0DAG](https://doi.org/10.5066/P94H0DAG)
- E2NHD-NWM-Network v1: [10.5066/P9W79I7Q](https://doi.org/10.5066/P9W79I7Q)
- E2NHD-NWM-Network v2: [10.5066/P976XCVT](https://doi.org/10.5066/P976XCVT)
- E2NHD-NWM-Network v3: [10.5066/P13IRYTB](https://doi.org/10.5066/P13IRYTB)
- National Hydrologic Geospatial Fabric Reference and Derived Hydrofabrics: [10.5066/P9NFPB5S](https://doi.org/10.5066/P9NFPB5S)

**Namespaces**

- Community reference: `https://geoconnex.us/ref/mainstems/{id}`
- USGS-minted: `https://geoconnex.us/usgs/mainstems/{id}`
- Reference landing pages: `https://reference.geoconnex.us/collections/mainstems/items/{id}`
- NLDI navigation root: `https://labs.waterdata.usgs.gov/api/nldi/linked-data/`

**Publications**

- Mainstems data model: Blodgett et al. 2021, Environmental Modelling & Software 135, [10.1016/j.envsoft.2020.104927](https://doi.org/10.1016/j.envsoft.2020.104927)
- Reference flow network: Blodgett, Johnson, Bock 2023, Environmental Modelling & Software 165, [10.1016/j.envsoft.2023.105726](https://doi.org/10.1016/j.envsoft.2023.105726)
- HY_Features conceptual model: [OGC 14-111r6](http://www.opengis.net/doc/IS/hy-features/1.0)

**Code**

- ref_rivers workflow: [github.com/internetofwater/ref_rivers](https://github.com/internetofwater/ref_rivers)
- reference.geoconnex.us server: [github.com/internetofwater/reference.geoconnex.us](https://github.com/internetofwater/reference.geoconnex.us)
- Mainstems processing pipeline: [code.usgs.gov/wma/nhgf/mainstems](https://code.usgs.gov/wma/nhgf/mainstems)
- nhdplusTools R package: [10.5066/P97AS8JD](https://doi.org/10.5066/P97AS8JD)
- hydroloom R package: [10.5066/P9AQCUY0](https://doi.org/10.5066/P9AQCUY0)

## Accounting summary

The numbers that matter for explaining growth across releases:

|Quantity|Value|Source|
|---|---|---|
|ref_rivers v1 URIs|33,853|NEWS.md|
|ref_rivers v2 URIs (active)|~33.9k|NEWS.md|
|ref_rivers v3 URIs retained from v2|33,745|NEWS.md|
|ref_rivers v3 URIs superseded from v2|155|NEWS.md|
|ref_rivers v3 new URIs admitted|819,000|NEWS.md|
|ref_rivers v3 candidates held back|~28,000|NEWS.md|
|ref_rivers v3 Canadian additions|16|NEWS.md|
|ref_rivers v3 total registry|~853k|derived|
|Mainstems v3 total mainstems|1,012,973|v3 XML eainfo|
|Mainstems v3 rows with ref URI|33,890|v3 XML eainfo|
|Mainstems v3 rows without ref URI|979,083|v3 XML eainfo|

The ~853k ref_rivers registry does not equal Mainstems v3's ~1.01M total. The difference is the ~130k mainstems in the v3 data release that didn't pass the ref_rivers admission criterion, plus the ~28k held back for isolation reasons.

## What each resource is authoritative for

- **Mainstems v3 XML** — enumeration of all CONUS mainstem levelpaths, crosswalks to source networks and WBD snapshots, geometry summaries, process steps for matching across networks.
- **3DHP 2024 XML** — HR-NHD network topology, mainstem assignments for HR-NHD flowlines, the bridge crosswalks that make the v3 admission criterion computable.
- **ref_rivers NEWS.md + README** — registry-level decisions: which URIs exist, which are superseded, the persistence contract, the admission criterion.
- **OGC 25-045** — the logical data model, terminology, design rationale, three-class HY_Flowpath split, discussion of outstanding issues.

For questions about specific mainstems or crosswalks, the XML files are authoritative. For questions about identifier persistence or which URIs are in the registry, NEWS.md is authoritative. For questions about what the data model means or why it's designed this way, 25-045 is authoritative.