# TidyTensor - More Fun with Deep Learning

TidyTensor is an R package for inspecting and manipulating tensors (multidimensional arrays). It provides an improved `print()` function for summarizing structure, named tensors, conversion to data frames, and high-level manipulation functions. Designed to be a companion for the `keras` package, functionality is layered on top of base R types.

### Background

R natively supports *tensors* as one of vectors (1d arrays, or rank-1 tensors), matrices (2d arrays, or rank-2 tensors), or higher-dimensional arrays. Further, these support `names()`, allowing for indexing of elements by index and/or name. 

```r
> t1 <- c(1.4, 2.5, 0.5, 1.3)
> t1[3]
[1] 0.5
> 
> names(t1) <- c("a", "b", "c", "d")
> t1["c"]
  c 
0.5 
```

Matrices:

```r
> t2 <- matrix(1:6, nrow = 2, ncol = 3)
> t2
     [,1] [,2] [,3]
[1,]    1    3    5
[2,]    2    4    6
> t2[1, 2]
[1] 3
> 
> dimnames(t2) <- list(c("row1", "row2"), c("col1", "col2", "col3"))
> t2
     col1 col2 col3
row1    1    3    5
row2    2    4    6
> t2["row1", "col2"]
[1] 3
```

Higher-rank arrays can be created with e.g. `array(1:(3*6*6), dim = c(3, 6, 6))` to create a 3x6x6 structure--these also accept optional `dimnames()`. The *shape* of a tensor is the vector returned by `dim()`, here `c(3, 6, 6)`. To stay with convention we use 'rank' of a tensor rather than 'dimension' of a tensor to avoid confusion in situations like 3d-points being stored in a rank-1 tensor of shape (3). 

TidyTensor levereges the fact that tensors usually represent hierarchical "set of" relationships. For example, a 28x28 grayscale image may be encoded as a rank-2, shape (28, 28) tensor; an RBG color image of the same size would be represented with a rank-3 (3, 28, 28) tensor in a "channels first" representation. On the other hand, we might start with a pixel, represented as a rank-1 shape (3) tensor, and a color image as a 28x28 grid of them with shape (28, 28, 3) in a channels-last representation. (In a [row, col] assumption, this makes each *column* a set of pixels, and each *row* a set of columns.) A set of 1800 color images (1 minute video at 30fps) would be a rank-4 tensor with shape (1800, 3, 28, 28); a set of 100 videos would have shape (100, 1800, 3, 28, 28). 

If this were stored in the R array `data`, then `data[20, 57, , , ]` would select the color image from the 57th frame of the 20th video. By default it would return the shape (3, 28, 28) array from that location, dropping the ranks with only one entry. (This doc similarly refers to individual 'ranks' of a tensor, rather than the sometimes used 'axes', mirroring the rank/dimension distinction.) This can be avoided with an additional `drop = F` parameter: `data[20, 57, , , , drop = F]` returns the rank-5 tensor with shape (1, 1, 3, 28, 28).




