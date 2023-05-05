
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hydroloom <img src="man/figures/logo.png" align="right" alt="" width="120" />

## hydroloom:

`hydroloom` is a collection of general hydrologic geospatial fabric
creation tools.

Install: `remotes::install_github("doi-usgs/hydroloom")`

Documentation: <https://doi-usgs.github.io/hydroloom/>

It was largely created from components of
[nhdplusTools](https://doi.org/10.5066/P97AS8JD):

          Blodgett, D., Johnson, J.M., 2022, nhdplusTools: Tools for
          Accessing and Working with the NHDPlus,
          https://doi.org/10.5066/P97AS8JD

`hydroloom` implements algorithms documented in:

NHDPlus Attributes:

McKay, L., Bondelid, T., Dewald, T., Johnston, J., Moore, R., & Rea, A.
(2015). NHD Plus Version 2: User Guide.
<https://www.epa.gov/system/files/documents/2023-04/NHDPlusV2_User_Guide.pdf>

Pfafstetter Attributes:

Verdin, K. L., & Verdin, J. P. (1999). A topological system for
delineation and codification of the Earth’s river basins. Journal of
Hydrology, 218(1–2), 1–12.
<https://doi.org/10.1016/S0022-1694(99)00011-6>

Graph Concepts:

Cormen, T. H., & Leiserson, C. E. (2022). Introduction to Algorithms,
fourth edition. MIT Press.

Key terms:

### Terminology:

The following definitions have been used as much as possible throughout
the package.  
Terms for rivers:  
**Flowline:** A flowline is an linear geometry that represents a segment
of a flowing body of water. Some flowlines have no local drainage area
and are never aggregate features.  
**Flowpath:** A flowpath is a linear geometry that represents the
connection between a catchment’s inlet and its outlet. All flowpaths
have a local drainage area and may be aggregates of flowlines.  
**Catchment:** A physiographic unit with zero or one inlets and one
outlet. A catchment is represented by one or more partial realizations;
flowpath, divide, and networks of flowpaths and divides.

Development of hydroloom is underway and rapid change should be
expected.

# Design Notes:

- Hydroloom uses tibble because dplyr verbs for data.frame was dropping
  the custom hy attributes.
- `hy` class tibble standardizes all attribute names in code.
- graph representation facilitated by `make_index_ids()` and
  `make_fromids()`
- names are plural when referring to identifiers and singular when
  referring to a numerical attribute.

# Disclaimer

This software is preliminary or provisional and is subject to revision.
It is being provided to meet the need for timely best science. The
software has not received final approval by the U.S. Geological Survey
(USGS). No warranty, expressed or implied, is made by the USGS or the
U.S. Government as to the functionality of the software and related
material nor shall the fact of release constitute any such warranty. The
software is provided on the condition that neither the USGS nor the U.S.
Government shall be held liable for any damages resulting from the
authorized or unauthorized use of the software.

[![CC0](https://i.creativecommons.org/p/zero/1.0/88x31.png)](https://creativecommons.org/publicdomain/zero/1.0/)
