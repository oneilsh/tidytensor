test_that("as.tensortree() works properly", {
  x <- c(1, 2, 3)
  tx <- as.tensortree(x)
  expect_equal(class(tx)[1], "tensortree")
  expect_equal(dim(tx), 3)

  x <- matrix(1:6, nrow = 2, ncol = 3)
  tx <- as.tensortree(x)
  expect_equal(class(tx)[1], "tensortree")
  expect_equal(dim(x), c(2, 3))

  x <- array(1:(3 * 4 * 5), dim = c(3, 4, 5))
  tx <- as.tensortree(x)
  expect_equal(class(tx)[1], "tensortree")
  expect_equal(dim(x), c(3, 4, 5))

  x <- data.frame(col1 = c(1, 2), col2 = c(3, 4))
  expect_error(as.tensortree(x))
})
