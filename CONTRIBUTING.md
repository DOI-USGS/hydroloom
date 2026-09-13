### Contributing:

First, thanks for considering a contribution! I hope to make this
package a community created resource for us all to gain from and won’t
be able to do that without your help!

1)  Contributions should be thoroughly tested with
    [testthat](https://testthat.r-lib.org/).  
2)  Code style should attempt to follow the [tidyverse style
    guide.](https://style.tidyverse.org/)  
3)  Please attempt to describe what you want to do prior to contributing
    by submitting an issue.  
4)  Please follow the typical github [fork - pull-request
    workflow.](https://gist.github.com/Chaser324/ce0505fbed06b947d962)  
5)  Make sure you use roxygen and run Check before contributing. More on
    this front as the package matures.
6)  `hydroloom` uses tidyselection and data masking. Please review the 
    list of `hydroloom` global attribute in `R/00_hydroloom.R` before 
    contributing. They are used as package variables in tidy selection broadly.

### Vignettes and articles:

`vignettes/hydroloom.Rmd` is the only vignette that ships to CRAN. Everything
under `vignettes/articles/` is website-only and is excluded from the source
tarball. A new vignette that needs a web service, a large download, or a heavy
suggested package belongs in `vignettes/articles/`; a self-contained one that
uses only the data in `inst/extdata` can go either place.

Every vignette gates chunk evaluation on `BUILD_VIGNETTES`:

```r
local <- (Sys.getenv("BUILD_VIGNETTES") == "TRUE")
knitr::opts_chunk$set(eval = local)
```

Set `BUILD_VIGNETTES=TRUE` in a local `.Renviron` to render with live code.
New vignettes should follow the same pattern. Because articles are not
installed, do not refer to them with `vignette("name")` — use a link to the
pkgdown page instead.

The build and release process is described in the README under "Build and
release."

Other notes: - consider running `lintr` prior to contributing. -
consider running `goodpractice::gp()` on the package before
contributing. - consider running `devtools::spell_check()` if you wrote
documentation. - this package uses pkgdown. Running
`pkgdown::build_site()` will refresh it.

