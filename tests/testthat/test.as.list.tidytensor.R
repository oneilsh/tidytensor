library(tidytensor)
context("testing as.list()")

test_that("basic as.list() works properly", {
  t <- as.tidytensor(array(rnorm(20 * 26 * 26 * 3), dim = c(20, 26, 26, 3)))
  t <- set_ranknames(t, sample, row, col, channel)
  t <- set_dimnames_for_rank(t, row, .dots = toupper(letters))
  t <- set_dimnames_for_rank(t, col, .dots = tolower(letters))
  t <- set_dimnames_for_rank(t, channel, R, G, B)

  tlist <- as.list(t, rank = sample)
  expect_equal(length(tlist), 20)
  #expect_equal(length(as.list(t, rank = 0)), 1) # attempting to split along rank = 0 just puts the whole thing as the only element in a list
  expect_error(as.list(t, rank = c(1, 2)))

})
