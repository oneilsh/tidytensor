library(tensortree)
context("testing permute()")

test_that("basic permute() works properly", {
  t <- as.tensortree(array(rnorm(20 * 26 * 26), dim = c(20, 26, 26)))
  ranknames(t) <- c("sample", "row", "col")

  t_p1 <- permute(t, c(3, 2, 1))
  t_p2 <- permute(t, c("col", "row", "sample"))

  expect_equal(t[1, 1, 1], t_p1[1, 1, 1])
  expect_equal(t[1, 3, 8], t_p1[8, 3, 1])
  expect_equal(t[2, 6, 1], t_p1[1, 6, 2])
  expect_equal(t[20, 26, 26], t_p1[26, 26, 20])

  expect_equal(t[1, 1, 1], t_p2[1, 1, 1])
  expect_equal(t[1, 3, 8], t_p2[8, 3, 1])
  expect_equal(t[2, 6, 1], t_p2[1, 6, 2])
  expect_equal(t[20, 26, 26], t_p2[26, 26, 20])

  expect_equal(t_p1, t_p2)
})
