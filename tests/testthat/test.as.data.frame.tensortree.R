library(tidytensor)
context("testing as.data.frame()")

# a wild unit-test appears (a generic unit tester even)
test_generic_as.data.frame <- function(tensor) {
  tt_df <- as.data.frame(tensor)
  ok <- TRUE
  for(rownum in 1:nrow(tt_df)) {
    row_indices <- tt_df[rownum, 1:(ncol(tt_df) - 1)]
    row_val <- tt_df$value[rownum]
    args <- c(list(tensor), as.list(row_indices))
    tensor_val <- do.call(`[`, args)
    if(tensor_val != row_val) {
      ok <- FALSE
      row_desc <- paste(tt_df[rownum, ], collapse = ", ")
      warning("test_tt_as.data.frame failed on row number ", rownum, "; tensor value was ", tensor_val, ", computed ", row_val)
    }
  }
  return(ok)
}

test_that("as.data.frame performs ok in various contexts", {
  t <- as.tidytensor(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
  ranknames(t) <- c("sample", "row", "col")

  expect_equal(test_generic_as.data.frame(t), TRUE)

  t2 <- as.tidytensor(array(1:(2*3*2), dim = c(2, 3, 2)))
  # each person has a cat and a dog and a bird, and each animal has a height and weight
  ranknames(t2) <- c("person", "pet", "height_weight")
  dimnames(t2) <- list(c("Joe", "Sally"), c("Cat", "Dog", "Bird"), c("Height", "Weight"))
  # set via array indexing
  t2["Sally", "Bird", "Weight"] <- 30
  t2["Joe", "Dog", "Height"] <- 40
  t2_df <- as.data.frame(t2)

  expect_equal(subset(t2_df, person == "Joe" & pet == "Dog" & height_weight == "Height")$value, 40)
  expect_equal(subset(t2_df, person == "Sally" & pet == "Bird" & height_weight == "Weight")$value, 30)
})
