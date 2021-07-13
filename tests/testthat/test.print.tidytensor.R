library(tidytensor)
context("testing print()")

test_that("basic print() works properly", {
  t <- as.tidytensor(array(rnorm(20 * 3 * 26 * 26), dim = c(20, 3, 26, 26)))
  ranknames(t) <- c("sample", "channel", "row", "col")
  t <- set_dimnames_for_rank(t, channel, R, G, B)
  t <- set_dimnames_for_rank(t, row, .dots = toupper(letters))
  t <- set_dimnames_for_rank(t, col, .dots = tolower(letters))

  expect_invisible( print(t) )
  expect_invisible( print(t, show_dimnames = TRUE) )
  expect_invisible( print(t, show_row_names = TRUE) )
  expect_invisible( print(t, show_col_names = TRUE) )
  expect_invisible( print(t, base_rank = 1) )
  expect_invisible( print(t, base_rank = 2) )

  t_channels_last <- permute(t, sample, row, col, channel)
  expect_invisible( print(t_channels_last) )
  expect_invisible( print(t_channels_last, base_rank = 3) )

  expect_invisible( print(t, max_per_level = 2) )
  expect_invisible( print(t, signif_digits = 5) )

  expect_invisible( print( tt(as.numeric(NULL))) )


})
