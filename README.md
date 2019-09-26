# TidyTensor - More Fun with Deep Learning

TidyTensor is an R package for inspecting and manipulating tensors (multidimensional arrays). It provides an improved `print()` function for summarizing structure, named tensors, conversion to data frames, and high-level manipulation functions. 

### Background

R natively supports *tensors* as one of vectors (1d arrays, or rank-1 tensors), matrices (2d arrays, or rank-2 tensors), or higher-dimensional arrays. Further, these support `names()`, allowing for indexing of elements by index and/or name. 

```r
> t1 <- c(1.4, 2.5, 0.5, 1.3)
> print(t1[2])
[1] 2.5
> 
> names(t1) <- c("a", "b", "c", "d")
> print(t1["b"])
  b 
2.5 
> 
```
