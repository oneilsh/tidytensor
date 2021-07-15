[![DOI](https://zenodo.org/badge/160564655.svg)](https://zenodo.org/badge/latestdoi/160564655)
[![status](https://joss.theoj.org/papers/07ef2e53d083c0eea30c0d08eef0f1cb/status.svg)](https://joss.theoj.org/papers/07ef2e53d083c0eea30c0d08eef0f1cb)
[![R-CMD-check](https://github.com/oneilsh/tidytensor/workflows/R-CMD-check/badge.svg)](https://github.com/oneilsh/tidytensor/actions)
[![codecov](https://codecov.io/gh/oneilsh/tidytensor/branch/master/graph/badge.svg?token=GWMT57CGDK)](https://codecov.io/gh/oneilsh/tidytensor) 


<br />
<img src="man/figures/tidytensor_transparent.png" height=200px/> 

(If you are reading this on GitHub, see the full [documentation](https://oneilsh.github.io/tidytensor/) on GitHub pages.)

TidyTensor is an R package for inspecting and manipulating tensors (multidimensional arrays). It provides an improved `print()` function for summarizing structure, named tensors, conversion to data frames, and high-level manipulation functions. Designed to complement the excellent `keras` package, functionality is layered on top of base R types.

TidyTensor was inspired by a workshop I taught in deep learning with R, and a desire to explain and explore tensors in a more intuitive way.  


<br />
<br />

## Installation and Usage

A simple `devtools::install_github("oneilsh/tidytensor")` will do it. If you don't have `devtools`, first grab it with `install.packages("devtools")`.

See the [Getting Started](https://oneilsh.github.io/tidytensor/articles/tidytensor.html) vignette for a summary of features and usage.

If you use TidyTensor, let us know in an [issue](https://github.com/oneilsh/tidytensor/issues/new)!

<br />
<br />

## Changelog

* v0.9.1: preparation for JOSS submission, many documentation improvements, removing `allow_huge` option from `as.data.frame.tidytensor()`
* v0.9.0: refactor `bottom` paramter of `print()` to `base_rank`
* v0.8.2: minor bugfixes, new combine_ranks() function
* v0.8.1: add [] and [] <- functionality
* v0.8: first version on github





