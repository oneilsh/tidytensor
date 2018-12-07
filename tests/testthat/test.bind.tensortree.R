context("testing bind")

test_that("basic bind works properly", {
  t1 <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t2 <- as.tensortree(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t3 <- as.tensortree(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  ranknames(t1) <- c("sample", "row", "col")
  ranknames(t2) <- c("sample", "row", "col")
  ranknames(t3) <- c("sample", "row", "col")

  t4 <- bind(t1, t2, t3, new_rank_name = "batch")
  expect_equal(ranknames(t4), c("batch", "sample", "row", "col"))
  expect_equal(dim(t4), c(3, 3, 4, 5))

})

test_that("basic bind works properly with input list", {
  t1 <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t2 <- as.tensortree(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t3 <- as.tensortree(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  ranknames(t1) <- c("sample", "row", "col")
  ranknames(t2) <- c("sample", "row", "col")
  ranknames(t3) <- c("sample", "row", "col")

  t4 <- bind(list(t1, t2, t3), new_rank_name = "batch")
  expect_equal(ranknames(t4), c("batch", "sample", "row", "col"))
  expect_equal(dim(t4), c(3, 3, 4, 5))

})

test_that("warning generated on conflicting names, with first ranknames unset and changed to NAs.", {
  t1 <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t2 <- as.tensortree(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t3 <- as.tensortree(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  ranknames(t1) <- c("sample", "row", "value")
  ranknames(t2) <- c("sample", "row", "col")
  ranknames(t3) <- c("sample", "row", "col")

  expect_warning(t4 <- bind(t1, t2, t3, new_rank_name = "batch"))
  expect_equal(ranknames(t4), c("batch", "sample", "row", "value"))

  t1 <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t2 <- as.tensortree(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t3 <- as.tensortree(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  ranknames(t2) <- c("sample", "row", "col")
  ranknames(t3) <- c("sample", "row", "col")

  expect_warning(t4 <- bind(t1, t2, t3, new_rank_name = "batch"))
  expect_equal(ranknames(t4), c("batch", NA, NA, NA))
})


test_that("error trying to bind with different shapes", {
  t1 <- as.tensortree(array(1:(3 * 4 * 6), dim = c(3, 4, 6)))
  t2 <- as.tensortree(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t3 <- as.tensortree(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))

  expect_error(t4 <- bind(t1, t2, t3))
})
