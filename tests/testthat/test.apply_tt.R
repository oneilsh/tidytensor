library(tensortree)
context("testing tt_apply")

test_that("basic tt_apply() works properly", {
  t <- as.tensortree(array(rnorm(20 * 26 * 26), dim = c(20, 26, 26)))
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

  expected_test2 <- as.tensortree(array(rep(0, 20), dim = c(20, 1)))
  ranknames(expected_test2) <- c("sample", NA)

  expect_equal(expected_test2, test2)

})

test_that("fancier tt_apply() test", {
  t <- as.tensortree(array(rnorm(10 * 20 * 26 * 26), dim = c(10, 20, 26, 26)))
  ranknames(t) <- c("batch", "sample", "row", "col")

  reduce_2d <- function(t) {
    # TODO: Make a wrapper for [<- that preserves ranknames]
    subsampled <- as.tensortree(t[c(T, F), c(T, F)])
    ranknames(subsampled) <- paste0(ranknames(t), "_sub")
    return(subsampled)
  }

  t_reduced <- tt_apply(t, rank = "sample", reduce_2d)
  expect_equal(dim(t_reduced), c(10, 20, 13, 13)) # the dimensions shouldn't change in this example
  expect_equal(ranknames(t_reduced), c("batch", "sample", "row_sub", "col_sub")) # nor the ranknames
})
