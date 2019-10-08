library(tidytensor)
context("testing shuffle()")

test_that("basic shuffle() works properly", {
  t <- as.tidytensor(array(rnorm(20 * 26 * 26 * 3), dim = c(20, 26, 26, 3)))
  t <- set_ranknames(t, one, two, three, four)
  t <- set_dimnames_for_rank(t, one, .dots = letters[1:20])

  t2 <- shuffle(t, seed = 42)

  t <- as.tidytensor(array(rnorm(20 * 4 * 5), dim = c(20, 4, 5)))
  t <- set_ranknames(t, one, two, three)
  t <- set_dimnames_for_rank(t, one, .dots = letters[1:20])

  t3<- shuffle(t, seed = 42)

  expect_equal(dimnames(t2)[[1]], dimnames(t3)[[1]])

})
