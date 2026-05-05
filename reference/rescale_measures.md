# Rescale Aggregate id Measure to id Measure

Given an aggregate_id (e.g. reachcode in NHDPlus) measure and the from
and to measure for an id (e.g. COMID flowline in NHDPlus), returns the
measure along the id flowline. This is useful where many flowlines make
up a single aggregate feature. "Measures" are typically referenced to
the aggregate feature. Flowlines have a stated from-measure /
to-measure. In some cases it is useful to rescale the measure such that
it is relative only to the flowline.

from is downstream – 0 is the outlet to is upstream – 100 is the inlet

## Usage

``` r
rescale_measures(measure, from, to)
```

## Arguments

- measure:

  numeric aggregate measure between 0 and 100

- from:

  numeric from-measure relative to the aggregate

- to:

  numeric to-measure relative to the aggregate

## Value

numeric rescaled measure

## Examples

``` r
rescale_measures(40, 0, 50)
#> [1] 80
rescale_measures(60, 50, 100)
#> [1] 20
```
