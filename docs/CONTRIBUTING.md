# NA

### Contributing:

First, thanks for considering a contribution! I hope to make this
package a community created resource for us all to gain from and won’t
be able to do that without your help!

1.  Contributions should be thoroughly tested with
    [testthat](https://testthat.r-lib.org/).  
2.  Code style should attempt to follow the [tidyverse style
    guide.](https://style.tidyverse.org/)  
3.  Please attempt to describe what you want to do prior to contributing
    by submitting an issue.  
4.  Please follow the typical github [fork - pull-request
    workflow.](https://gist.github.com/Chaser324/ce0505fbed06b947d962)  
5.  Make sure you use roxygen and run Check before contributing. More on
    this front as the package matures.
6.  `hydroloom` uses tidyselection and data masking. Please review the
    list of `hydroloom` global attribute in `R/00_hydroloom.R` before
    contributing. They are used as package variables in tidy selection
    broadly.

Other notes: - consider running `lintr` prior to contributing. -
consider running `goodpractice::gp()` on the package before
contributing. - consider running
[`devtools::spell_check()`](https://devtools.r-lib.org/reference/spell_check.html)
if you wrote documentation. - this package uses pkgdown. Running
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
will refresh it.
