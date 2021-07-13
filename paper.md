---
title: 'TidyTensor: Utilities for named multidimensional arrays as hierarchical structures'
tags:
  - R
  - deep learning
  - array
  - tensor
  - tidy
authors:
  - name: Shawn T. O'Neil
    orcid: 0000-0001-6220-7080
    affiliation: 1
affiliations:
 - name: Center for Health Artificial Intelligence, University of Colorado Anschutz Medical Campus
   index: 1
date: 13 July 2021
bibliography: paper.bib
---

# Summary

Deep learning applications commonly employ the use of *tensors* (multidimensional arrays) for data storage and manipulation. In addition to being space efficient tensors provide a common framework for [automatic differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation), the engine of deep neural network training. TidyTensor is an R package for inspecting and manipulating tensors. It provides an improved `print()` function for summarizing structure, named tensors, conversion to data frames, and high-level manipulation functions. Designed to complement `keras` package (or other packages that utilize native R arrays), functionality is layered on top of base R types.


# Statement of need

While R supports multidimensional arrays natively, there are important differences between these types and tensors as typically used in deep learning applications. First, R assumes array data are organized in a column-major order as opposed to the row-major order typically used in Python, rendering the default `print()` and other inspection methods far less useful. TidyTensor additionally provides support for "named" tensors, allowing researchers to use semantically-relevant names for working with and manipulating tensors via a familiar tidy-friendly interface. While other packages exist for named arrays in R, TidyTensor leverages a hierarchical interpretation of tensor data that makes it easy to manipulate and investigate tensor data of various kinds. Early versions of TidyTensor supported a 3-week researcher-oriented workshop "Deep Learning for Life Scientists" at Oregon State University's Center for Genome Research and Biocomputing, and these features enabled graduate students and faculty new to the field to apply deep learning techniques to datasets from their own work.


# Related Work

Other packages implement named tensors, though without the "tidy" features of TidyTensor or the simplification of asserting nesting/set-of-sets semantics. In Python, the [xarray](http://xarray.pydata.org/en/stable/) package provides named tensors and a wide variety of manipulation and related features [@hoyer2017xarray], while in R the [garray](https://cran.r-project.org/web/packages/garray/garray.pdf) package provides features similar to TidyTensor [@garray], including implementing names in the same way allowing for easy conversion to and from TidyTensors. The [stars](https://r-spatial.github.io/stars/) R package also provides named arrays with a focus on spatiotemporal data [@stars]. TidyTensor utilizes [abind](https://cran.r-project.org/web/packages/abind/index.html) internally for array manipulation [@abind].

TidyTensor was designed primarily as a companion to the [keras](https://keras.rstudio.com/) and [tensorflow](https://tensorflow.rstudio.com/) ports for R which utilize native R types [@rkeras; @rtensorflow]. By contrast, the recently released [torch](https://torch.mlverse.org/) port for R does not use native R types for tensors [@rtorch]; using TidyTensor with this package would require costly conversion to-and-from Torch-native types. 


# References
