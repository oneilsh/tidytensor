# TidyTensor - More Fun with Deep Learning

TidyTensor is an R package for inspecting and manipulating tensors (multidimensional arrays). It provides an improved `print()` function for summarizing structure, named tensors, conversion to data frames, and high-level manipulation functions. Designed to companion the excellent `keras` package, functionality is layered on top of base R types.

TidyTensor was inspired by a workshop I taught in deep learning with R, and a desire to explain and explore tensors in a more intuitive way.  

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

### Printing

The default print function for arrays in R is... not great. Whereas groups are typically organized in tensors "leftward," R breaks them down "rightward" and provides little hierarchical organization. (I think Python's `numpy` arrays are better in this regard, but still leave a lot to be desired.) Consider a simulated dataset of 4 color 10x10 images (channels-first):

```r
> array(rnorm(4*3*10*10), dim = c(4, 3, 10, 10))

, , 1, 1

            [,1]       [,2]      [,3]
[1,]  0.72733056 -0.2473793 0.7246183
[2,] -0.02961571 -0.6277961 1.3537430
[3,]  0.53652082 -0.8531169 1.8976249
[4,]  0.49515707  0.4510950 0.1110309

, , 2, 1

           [,1]        [,2]       [,3]
[1,]  1.8017886 -0.36434137 -0.5022344
[2,]  0.8728468 -0.38282962  0.8931795
[3,] -0.5263026 -0.55057900 -1.1352055
[4,] -0.7984145  0.08755078 -0.5806810

, , 3, 1

            [,1]       [,2]       [,3]
[1,] -0.26588696 -1.2376636 -0.9721222
[2,]  0.04935331 -0.8383761  0.2938657
# ...
```

TidyTensor provides a customized `print()` by allowing R vectors, matrices, and arrays to be converted with `as.tidytensor()`, or
the shorthand `tt()`. 

```r
> library(tidytensor)
> array(rnorm(4*3*10*10), dim = c(4, 3, 10, 10)) %>% 
+   tt()
# Rank 4 tensor, shape: (4, 3, 10, 10)
|  # Rank 3 tensor, shape: (3, 10, 10)
|  |  # Rank 2 tensor, shape: (10, 10)
|  |      -0.839   -1.7  -0.354   -1.03   0.515   0.935  ... 
|  |        1.24   1.24   0.876   -1.09  0.0773    1.84  ... 
|  |       0.213  -1.45   -1.18   -1.14   0.488   -2.13  ... 
|  |      -0.744  0.113   -0.94  0.0892  -0.359    -1.4  ... 
|  |        1.33   1.72   -1.87   -1.56    1.15  -0.279  ... 
|  |       0.845  -1.05  -0.482   -1.36   0.458   0.451  ... 
|  |         ...    ...     ...     ...     ...     ...  ... 
|  |  # ...
|  # ...

```

Here the printout is emphasizing the nested nature of tensors and providing a quick structure summary. 

`as.tidytensor()` simply adds an addtional `tidytensor` class entry, so the array can still be used in all the normal ways. The `print()`
function can be customized to show more or fewers rows and columns in the "bottom" tensors and to show dimension names there (it's on the TODO list to also incorporate dimesion names for higher ranks). 

```r
> images <- array(rnorm(4*3*10*10), dim = c(4, 3, 10, 10))
> dimnames(images)[[3]] <- letters[1:10]
> images %>%
+   tt() %>%
+   print(show_names = T, max_rows = 10, max_cols = 10)
# Rank 4 tensor, shape: (4, 3, 10, 10)
|  # Rank 3 tensor, shape: (3, 10, 10)
|  |  # Rank 2 tensor, shape: (10, 10)
|  |                [,1]    [,2]     [,3]    [,4]    [,5]      [,6]    [,7]    [,8]     [,9]   [,10] 
|  |      ["a",]   0.175  -0.308   -0.445   -2.72  -0.743  -0.00832   -1.44  -0.479    0.656  -0.508 
|  |      ["b",]   0.485  0.0974   -0.147   0.377   0.975      1.93    2.11   0.195    -0.51  -0.869 
|  |      ["c",]   0.517   0.903    0.667   0.023   -1.81     -2.33   -1.03     1.2    0.362   -1.05 
|  |      ["d",]   -0.66   0.707    0.383  -0.166    1.83     0.128   0.878  0.0589    0.261   0.365 
|  |      ["e",]    1.71   -2.25    0.822  -0.919  -0.626     -2.12   0.931   -2.76    0.149   0.338 
|  |      ["f",]    -1.6   0.587   0.0237  -0.539   0.858     0.894   -1.25     1.7  -0.0529   -1.35 
|  |      ["g",]   -1.85   -1.16  -0.0497    1.21    1.69     0.113    2.05  -0.199   -0.865  -0.775 
|  |      ["h",]  -0.374  0.0403    -1.32    1.59   -1.73    -0.249  -0.699   -1.43    0.624   -0.56 
|  |      ["i",]   -0.19   0.959     -0.5    0.23  -0.618     0.978    1.17  -0.563    -1.57    1.34 
|  |      ["j",]   -0.76    1.45    0.685   -1.42  -0.248   -0.0605    2.85  -0.243     0.64   -1.17 
|  |  # ...
|  # ...

```

And we can see more of the structure by increasing the `max_per_level` parameter:

```r
> images %>%
+   tt() %>%
+   print(max_per_level = 2)
# Rank 4 tensor, shape: (4, 3, 10, 10)
|  # Rank 3 tensor, shape: (3, 10, 10)
|  |  # Rank 2 tensor, shape: (10, 10)
|  |      0.175  -0.308  -0.445   -2.72  -0.743  -0.00832  ... 
|  |      0.485  0.0974  -0.147   0.377   0.975      1.93  ... 
|  |      0.517   0.903   0.667   0.023   -1.81     -2.33  ... 
|  |      -0.66   0.707   0.383  -0.166    1.83     0.128  ... 
|  |       1.71   -2.25   0.822  -0.919  -0.626     -2.12  ... 
|  |       -1.6   0.587  0.0237  -0.539   0.858     0.894  ... 
|  |        ...     ...     ...     ...     ...       ...  ... 
|  |  # Rank 2 tensor, shape: (10, 10)
|  |      -0.0687  -0.0817  -0.691  -0.509    0.17    1.91  ... 
|  |         1.07   -0.761  0.0629  0.0457   0.156   0.123  ... 
|  |        -0.58     1.19    0.45    1.53   0.658   0.102  ... 
|  |         1.12    0.572   -1.69   -1.65  -0.994    0.48  ... 
|  |        0.621     2.71    1.87  -0.695    1.23  -0.167  ... 
|  |        0.744    -0.48  -0.936   -1.42  -0.369    1.13  ... 
|  |          ...      ...     ...     ...     ...     ...  ... 
|  |  # ...
|  # Rank 3 tensor, shape: (3, 10, 10)
|  |  # Rank 2 tensor, shape: (10, 10)
|  |         1.05   -0.486  -0.755     1.46   -0.62   0.391  ... 
|  |        0.773   -0.426   0.245   -0.209   0.696   0.367  ... 
|  |        0.248  -0.0409  -0.137       -1  -0.549   0.967  ... 
|  |       -0.295   -0.411   -1.21   -0.194   0.766  -0.419  ... 
|  |      -0.0895    0.445   0.272    -1.24    1.32   0.152  ... 
|  |       -0.527  -0.0465    1.03  -0.0081  -0.955   0.341  ... 
|  |          ...      ...     ...      ...     ...     ...  ... 
|  |  # Rank 2 tensor, shape: (10, 10)
|  |         1.62   0.567  -0.983  -0.566   -2.15   0.385  ... 
|  |       -0.105    1.85  -0.522   -1.34  -0.627    0.85  ... 
|  |       -0.673   -2.27    1.48   -1.66   0.159   -4.23  ... 
|  |        0.279   -1.95   0.829   0.884   0.986  -0.257  ... 
|  |      -0.0818   -1.01  -0.708   -1.48   -1.06   -1.41  ... 
|  |        -2.38  -0.508    1.71   0.667  -0.119    1.38  ... 
|  |          ...     ...     ...     ...     ...     ...  ... 
|  |  # ...
|  # ...
```

What if our data were in channels-last configuration? In cases where we want to visualize the last rank as a vector within a matrix like this
(pixels in an image, or channels in a filter map) we can specify `bottom = "3d"`.

```r
> array(rnorm(4*10*10*3), dim = c(4, 10, 10, 3)) %>%
+   tt() %>%
+   print(bottom = "3d")
# Rank 4 tensor, shape: (4, 10, 10, 3)
|  # Rank 3 tensor, shape: (10, 10, 3)
|          [0.866, 0.183, 1.05]    [0.815, 0.0934, -1.72]  [0.573, 0.425, -1.89]      [-0.356, 0.438, 2.34]    [1.54, -0.159, -1.59]     [0.0329, 1.68, 2.2]  ... 
|         [1.91, -1.59, -0.444]     [0.0151, -1.2, 0.613]   [1.02, 0.882, 0.303]  [-0.764, -0.212, -0.0132]     [0.998, 1.71, 0.358]  [-0.0103, -1.24, -1.6]  ... 
|          [1.15, -1.25, 0.153]   [0.445, -0.185, -0.651]    [1.03, 0.551, 0.07]        [1.7, 0.165, 0.474]  [-0.353, -1.72, -0.193]    [-0.825, 0.29, 1.53]  ... 
|       [-0.566, -0.295, 0.773]     [0.0525, 1.54, 0.262]  [-0.737, -1.53, 1.42]    [0.987, -0.037, -0.589]   [-0.689, 0.95, -0.927]      [2.09, 1.17, 1.39]  ... 
|      [0.453, -0.0282, -0.763]      [-0.785, 0.32, 0.25]  [-0.212, 0.268, 1.28]        [1.15, -1.2, 0.584]   [-0.111, 0.186, 0.969]  [0.656, -0.468, -1.78]  ... 
|          [-1.6, 0.272, 0.455]  [-0.551, -2.03, -0.0733]    [0.601, 1.18, 1.34]     [-0.892, 0.809, 0.107]    [1.32, -0.669, -3.04]   [-0.518, 1.77, 0.748]  ... 
|                           ...                       ...                    ...                        ...                      ...                     ...  ... 
|  # ...
```

