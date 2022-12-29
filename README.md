
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hydroloom <img src="man/figures/logo.png" align="right" alt="" width="120" />

## hydroloom:

`hydroloom` is a collection of general hydrologic geospatial fabric
creation tools.

It was created from generic components of
[nhdplusTools](https://doi.org/10.5066/P97AS8JD):

          Blodgett, D., Johnson, J.M., 2022, nhdplusTools: Tools for
          Accessing and Working with the NHDPlus,
          https://doi.org/10.5066/P97AS8JD

Development of hydroloom is underway and rapid change should be
expected.

# Design Notes:

- Using tibble because dplyr verbs for data.frame was dropping the
  custom hy attributes.
- `hy` class tibble standardizes all attribute names in code.
- graph representation facilitated by `make_index_ids()` and
  `make_fromids()`

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
