library(tidytensor)
context("testing tt_apply")

test_that("basic tt_apply() works properly", {
  t <- as.tidytensor(array(rnorm(20 * 26 * 26), dim = c(20, 26, 26)))
  ranknames(t) <- c("sample", "row", "col")

  dev_median <- function(t) {
    return(t - median(t))
  }

  t_median <- tt_apply(t, rank = 1, dev_median)
  expect_equal(dim(t_median), c(20, 26, 26)) # the dimensions shouldn't change in this example
  expect_equal(ranknames(t_median), c("sample", "row", "col")) # nor the ranknames

  test2 <- tt_apply(t_median, 1, function(s) {
    return(median(s))
  }, drop_final_1 = FALSE)

  expected_test2 <- as.tidytensor(array(rep(0, 20), dim = c(20, 1)))
  ranknames(expected_test2) <- c("sample", NA)

  expect_equal(expected_test2, test2)

})

test_that("basic tt_apply() works properly, include size-1 ranks", {
  dimslist <- list(c(20, 26, 26),
                   c(1, 26, 26),
                   c(20, 1, 26),
                   c(20, 26, 1),
                   c(1, 1, 26),
                   c(20, 1, 1),
                   c(1, 1, 1))

  for(dims in dimslist) {

    t <- as.tidytensor(array(rnorm(prod(dims)), dim = dims))
    ranknames(t) <- c("sample", "row", "col")

    dev_median <- function(t) {
      #res <- t - median(t)
      # TODO: median() results in non-corformable array error, but mean does not
      # replicate: tt(array(1, dim = c(1,1))) - median(tt(array(1, dim = c(1,1))))
      # I think this is because median attempts to preserve class; from help(median): "This is a generic function for which methods can be written. However, the default method makes use of is.na, sort and mean from package base all of which are generic, and so the default method will work for most classes (e.g., "Date") for which a median is a reasonable concept."
      # the result being that median(t) has class "tidytensor", "array" (which is t's class), and you can't subtract two arrays of different shape
      # I wonder how many other R functions do this; I'd still like to keep rank-1 tensors as arrays for dim() conveniences but
      # user's won't expect something like this to not work, especially when most other basic functions appear to return a basic vector. oh, quantial() has this issue too
      # more weirdly, this only occurs for the edge case of a 1x1 array! anyway switching to mean()
      res <- t - mean(t)
      return(res)
    }

    t_median <- tt_apply(t, rank = 1, dev_median)
    expect_equal(dim(t_median), dims) # the dimensions shouldn't change in this example
    expect_equal(ranknames(t_median), c("sample", "row", "col")) # nor the ranknames


    test2 <- tt_apply(t_median, 1, function(s) {
      return(mean(s))
    }, drop_final_1 = FALSE)

    expected_test2 <- as.tidytensor(array(rep(0, dims[1]), dim = c(dims[1], 1)))
    ranknames(expected_test2) <- c("sample", NA)

    expect_equal(expected_test2, test2)

  }
})


test_that("fancier tt_apply() test", {
  t <- as.tidytensor(array(rnorm(10 * 20 * 26 * 26), dim = c(10, 20, 26, 26)))
  ranknames(t) <- c("batch", "sample", "row", "col")

  reduce_2d <- function(t) {
    subsampled <- as.tidytensor(t[c(T, F), c(T, F)])
    ranknames(subsampled) <- paste0(ranknames(t), "_sub")
    return(subsampled)
  }

  t_reduced <- tt_apply(t, rank = "sample", reduce_2d)
  expect_equal(dim(t_reduced), c(10, 20, 13, 13)) # the dimensions shouldn't change in this example
  expect_equal(ranknames(t_reduced), c("batch", "sample", "row_sub", "col_sub")) # nor the ranknames

  # tidy
  t_reduced <- tt_apply(t, sample, reduce_2d)
  expect_equal(dim(t_reduced), c(10, 20, 13, 13)) # the dimensions shouldn't change in this example
  expect_equal(ranknames(t_reduced), c("batch", "sample", "row_sub", "col_sub")) # nor the ranknames

  # by index
  t_reduced <- tt_apply(t, 2, reduce_2d)
  expect_equal(dim(t_reduced), c(10, 20, 13, 13)) # the dimensions shouldn't change in this example
  expect_equal(ranknames(t_reduced), c("batch", "sample", "row_sub", "col_sub")) # nor the ranknames

})

