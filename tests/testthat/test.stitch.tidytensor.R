context("testing stitch()")

test_that("basic stitch works properly", {
  t1 <- as.tidytensor(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t2 <- as.tidytensor(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t3 <- as.tidytensor(array(100 * 1:(3 * 4 * 5), dim = c(10, 4, 5)))
  ranknames(t1) <- c("sample", "row", "col")
  ranknames(t2) <- c("sample", "row", "col")
  ranknames(t3) <- c("sample", "row", "col")

  t4 <- stitch(t1, t2, t3)
  t5 <- stitch(list(t1, t2, t3))
  expect_equal(ranknames(t4), c("sample", "row", "col"))
  expect_equal(dim(t4), c(16, 4, 5))

  expect_equal(ranknames(t5), c("sample", "row", "col"))
  expect_equal(dim(t5), c(16, 4, 5))
})



test_that("warning generated on conflicting names, with first ranknames unset and changed to NAs.", {
  t1 <- as.tidytensor(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t2 <- as.tidytensor(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t3 <- as.tidytensor(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  ranknames(t1) <- c("sample", "row", "value")
  ranknames(t2) <- c("sample", "row", "col")
  ranknames(t3) <- c("sample", "row", "col")

  expect_warning(t4 <- stitch(t1, t2, t3))
  expect_equal(ranknames(t4), c("sample", "row", "value"))

  t1 <- as.tidytensor(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t2 <- as.tidytensor(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t3 <- as.tidytensor(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  ranknames(t2) <- c("sample", "row", "col")
  ranknames(t3) <- c("sample", "row", "col")

  expect_warning(t4 <- stitch(t1, t2, t3))
  expect_equal(is.null(ranknames(t4)), TRUE)
})


test_that("error trying to stitch() with different shapes", {
  t1 <- as.tidytensor(array(1:(3 * 4 * 6), dim = c(3, 4, 6)))
  t2 <- as.tidytensor(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t3 <- as.tidytensor(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))

  expect_error(t4 <- stich(t1, t2, t3))
})



