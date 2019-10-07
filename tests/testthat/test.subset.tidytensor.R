library(tidytensor)
context("testing subset()")

test_that("basic subset() works properly", {
  t <- as.tidytensor(array(rnorm(20 * 26 * 26 * 3), dim = c(20, 26, 26, 3)))
  ranknames(t) <- c("sample", "row", "col", "channel")
  t <- set_dimnames_for_rank(t, channel, R, G, B)

  t1 <- subset(t, sample = 1:10, channel = c("G", "R"), col = 1, drop = FALSE)
  expect_equal(dim(t1), c(10, 26, 1, 2))
  expect_equal(ranknames(t1), c("sample", "row", "col", "channel"))
  expect_equal(dimnames(t1)[[4]], c("G", "R"))

  t2 <- subset(t, sample = 1:10, channel = c("G", "R"), col = 1, drop = TRUE)
  expect_equal(dim(t2), c(10, 26, 2))
  expect_equal(ranknames(t2), c("sample", "row", "channel"))
  expect_equal(dimnames(t2)[[3]], c("G", "R"))
})
