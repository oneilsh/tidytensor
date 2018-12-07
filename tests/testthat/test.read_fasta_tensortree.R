context("reading fasta file")

test_that("read_fasta_tensortree() works properly", {
  t <- read_fasta_tensortree(system.file("extdata", "seqs.fasta", package = "tensortree"))
  expect_equal(t[1,1,1:4], c(0, 0, 0, 1)) # first base of first seq is a T
  expect_equal(t[1,4,1:4], c(1, 0, 0, 0)) # second base of first seq is an A

  expect_equal(t[2,1,1:4], c(1, 0, 0, 0)) # first base of first seq is a A
  expect_equal(t[2,4,1:4], c(0, 1, 0, 0)) # second base of first seq is an C

  expect_equal(dim(t), c(97, 140, 4))     # correct shape?


})
