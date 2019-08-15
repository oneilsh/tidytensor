context("testing ranknames functions")

test_that("ranknames<- works properly", {
  t <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  ranknames(t) <- c("sample", "row", "col")
  expect_equal(names(dimnames(t)), c("sample", "row", "col"))

  # works like names():
  t <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  ranknames(t) <- c("sample", "row", "col")
  expect_equal(names(dimnames(t)), c("sample", "row", "col"))
})


test_that("ranknames() works properly", {
  t <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  ranknames(t) <- c("sample", "row", "col")
  expect_equal(ranknames(t), c("sample", "row", "col"))
})


test_that("set_ranknames() works properly", {
  t <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t <- set_ranknames(t, .dots = c("sample", "row", "col"))
  expect_equal(names(dimnames(t)), c("sample", "row", "col"))

  t <- set_ranknames(t, rank1name, rank2name, rank3name)
  expect_equal(names(dimnames(t)), c("rank1name", "rank2name", "rank3name"))
})


test_that("dimnames<-() works properly", {
  t <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t <- set_ranknames(t, .dots = c("sample", "row", "col"))

  dimnames(t) <- list(rep("a", 3), rep("b", 4), rep("c", 5))
  expect_equal(ranknames(t), c("sample", "row", "col")) # do we still have the ranknames?
  expect_equal(dimnames(t)[[2]], c("b", "b", "b", "b")) # do we still have the dimnames?
})


test_that("set_dimnames() works properly", {
  t <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t <- set_ranknames(t, .dots = c("sample", "row", "col"))

  t <- set_dimnames(t, list(rep("a", 3), rep("b", 4), rep("c", 5)))
  expect_equal(ranknames(t), c("sample", "row", "col")) # do we still have the ranknames?
  expect_equal(dimnames(t)[[2]], c("b", "b", "b", "b")) # do we still have the dimnames?
})


test_that("set_dimnames_for_rank() works properly", {
  t <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t <- set_ranknames(t, .dots = c("sample", "row", "col"))

  # tidy dimnames, index rank
  t <- set_dimnames_for_rank(t, 2, row1, row2, row3, row4)
  expect_equal(ranknames(t), c("sample", "row", "col")) # do we still have the ranknames?
  expect_equal(dimnames(t)[[2]], c("row1", "row2", "row3", "row4")) # do we still have the dimnames?
  expect_equal(dimnames(t)[[1]], as.character(c(NA, NA, NA))) # others should be NA

  # untidy dimnames, index rank
  t <- set_dimnames_for_rank(t, 2, .dots = c("row1", "row2", "row3", "row4"))
  expect_equal(ranknames(t), c("sample", "row", "col")) # do we still have the ranknames?
  expect_equal(dimnames(t)[[2]], c("row1", "row2", "row3", "row4")) # do we still have the dimnames?
  expect_equal(dimnames(t)[[1]], as.character(c(NA, NA, NA))) # others should be NA

  # tidy dimnames, named rank
  t <- set_dimnames_for_rank(t, "sample", s1, s2, s3)
  expect_equal(dimnames(t)[[1]], c("s1", "s2", "s3")) # others should be NA
  expect_equal(dimnames(t)[[2]], c("row1", "row2", "row3", "row4")) # do we still have the dimnames?

  # untidy dimnames, named rank
  t <- set_dimnames_for_rank(t, "sample", .dots = c("s1", "s2", "s3"))
  expect_equal(dimnames(t)[[1]], c("s1", "s2", "s3")) # others should be NA
  expect_equal(dimnames(t)[[2]], c("row1", "row2", "row3", "row4")) # do we still have the dimnames?

  # tidy dimnames, tidy rank
  t <- set_dimnames_for_rank(t, sample, s1, s2, s3)
  expect_equal(dimnames(t)[[1]], c("s1", "s2", "s3")) # others should be NA
  expect_equal(dimnames(t)[[2]], c("row1", "row2", "row3", "row4")) # do we still have the dimnames?

  # untidytidy dimnames, tidy rank
  t <- set_dimnames_for_rank(t, sample, .dots = c("s1", "s2", "s3"))
  expect_equal(dimnames(t)[[1]], c("s1", "s2", "s3")) # others should be NA
  expect_equal(dimnames(t)[[2]], c("row1", "row2", "row3", "row4")) # do we still have the dimnames?
})

