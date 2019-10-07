library(tidytensor)
library(magrittr)
context("testing partition")

test_that("basic partition works properly", {
  t1 <- as.tidytensor(array(1:(6 * 4 * 4 * 3), dim = c(6, 4, 4, 3)))
  ranknames(t1) <- c("sample", "row", "col", "channel")

  partition_sizes <- partition(t1, sizes = c(0.5, 0.5)) %>%
    lapply(dim) %>%
    lapply(head, n = 1) %>%
    unlist()

  expect_equal(partition_sizes, c(3, 3))

  # equal weights
  partition_sizes <- partition(t1, sizes = c(1, 1, 1)) %>%
    lapply(dim) %>%
    lapply(head, n = 1) %>%
    unlist()

  expect_equal(partition_sizes, c(2, 2, 2))

  parts <- partition(t1, sizes = c(0.5, 0.5))
  expect_equal(ranknames(parts[[1]]), c("sample", "row", "col", "channel"))
})



