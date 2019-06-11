context("testing c()")

test_that("basic c works properly", {
  t1 <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t2 <- as.tensortree(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t3 <- as.tensortree(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  ranknames(t1) <- c("sample", "row", "col")
  ranknames(t2) <- c("sample", "row", "col")
  ranknames(t3) <- c("sample", "row", "col")

  t4 <- c(t1, t2, t3)
  expect_equal(ranknames(t4), c("sample", "row", "col"))
  expect_equal(dim(t4), c(9, 4, 5))
  t5 <- c(t1, NULL)
  expect_equal(t1, t5)

})



test_that("warning generated on conflicting names, with first ranknames unset and changed to NAs.", {
  t1 <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t2 <- as.tensortree(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t3 <- as.tensortree(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  ranknames(t1) <- c("sample", "row", "value")
  ranknames(t2) <- c("sample", "row", "col")
  ranknames(t3) <- c("sample", "row", "col")

  expect_warning(t4 <- c(t1, t2, t3))
  expect_equal(ranknames(t4), c("sample", "row", "value"))

  t1 <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t2 <- as.tensortree(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t3 <- as.tensortree(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  ranknames(t2) <- c("sample", "row", "col")
  ranknames(t3) <- c("sample", "row", "col")

  expect_warning(t4 <- c(t1, t2, t3))
  expect_equal(is.null(ranknames(t4)), TRUE)
})


test_that("error trying to c() with different shapes", {
  t1 <- as.tensortree(array(1:(3 * 4 * 6), dim = c(3, 4, 6)))
  t2 <- as.tensortree(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  t3 <- as.tensortree(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))

  expect_error(t4 <- c(t1, t2, t3))
})



