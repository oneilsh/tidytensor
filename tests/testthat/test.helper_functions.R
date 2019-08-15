library(tidytensor)
context("testing rank_to_index()")


test_that("rank_to_index performs ok", {
  t <- as.tidytensor(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  ranknames(t) <- c("sample", "row", "col")

  expect_equal(rank_to_index(t, 2), 2)
  expect_equal(rank_to_index(t, c(3, 2)), c(3, 2))
  expect_equal(rank_to_index(t, "row"), 2)
  expect_equal(rank_to_index(t, "sample"), 1)
  expect_equal(rank_to_index(t, "col"), 3)
  expect_equal(rank_to_index(t, c("col", "sample")), c(3, 1))
  expect_equal(rank_to_index(t, c(TRUE, FALSE, TRUE)), c(1, 3))
})
