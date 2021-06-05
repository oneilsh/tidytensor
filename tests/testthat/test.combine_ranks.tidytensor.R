library(tidytensor)
context("testing combine_ranks()")

test_that("basic combine_ranks() works properly", {
  t <- as.tidytensor(array(rnorm(5 * 20 * 26 * 26), dim = c(5, 20, 26, 26)))
  ranknames(t) <- c("batch", "image", "row", "col")

  # given an image tidytensor (26x26), return a set of replicates with noise added
  make_noisy_images <- function(t2) {
    res <- bind(t2,
                t2 + rnorm(length(t2)),
                t2 + rnorm(length(t2)),
                t2 + rnorm(length(t2)), new_rank_name = "replicate")
  }

  # now augment the original data by replacing each image with a set of noisy replicates
  t <- tt_apply(t, image, make_noisy_images)


  # now t is shape (5, 20, 4, 26, 26) with ranknames (batch, image, replicate, row, col)
  expect_equal(dim(t), c(5, 20, 4, 26, 26))
  # lets set some dimnames
  t <- set_dimnames_for_rank(t, image, .dots = 1:20)
  t <- set_dimnames_for_rank(t, replicate, original, rep1, rep2, rep3)

    # to make it compatible with the original shape we combine images and replicates
  t2 <- combine_ranks(t, image, replicate)

  expect_equal(dim(t2), c(5, 80, 26, 26))
  expect_equal(ranknames(t2), c("batch", "image_replicate", "row", "col"))

  # check that things still line up and that the created dimension names are correct
  expect_equal(t[2, "3", "rep2", , ], t2[2, "3_rep2", , ])
})
