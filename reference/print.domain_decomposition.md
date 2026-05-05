# Print a domain_decomposition

Two-mode S3 print method for `domain_decomposition`. The default (cheap)
form prints a fixed-size summary of slot counts: basins, domains,
catchments, nexuses, and overrides. Cost is bounded by the number of
domains, so it stays fast on 50,000-catchment decompositions. The full
form prints a hierarchical tree summary with per-block roll-up
statistics and is intended for verifying the shape of a freshly-built
decomposition before running analysis on it.

## Usage

``` r
# S3 method for class 'domain_decomposition'
print(x, full = FALSE, ...)
```

## Arguments

- x:

  object of class `domain_decomposition`.

- full:

  logical. `FALSE` (default) prints the cheap summary; `TRUE` prints the
  full hierarchical tree.

- ...:

  ignored.

## Value

`x`, invisibly.

## Details

The cheap form is the default because at the REPL most users want a
quick "did this come out the way I expected" check, not a full audit.
The footer line of the cheap form advertises how to call the full form.
