context("reading fasta file, fasta_encode_tensor")

test_that("fasta_encode_tensor() works properly", {
  t <- fasta_encode_tensor(system.file("extdata", "seqs.fasta", package = "tensortree"))
  expect_equal(t[1,1,1:4], tt(c(0, 0, 0, 1))) # first base of first seq is a T
  expect_equal(t[1,4,1:4], tt(c(1, 0, 0, 0))) # fourth base of first seq is an A

  expect_equal(t[2,1,1:4], tt(c(1, 0, 0, 0))) # first base of first seq is a A
  expect_equal(t[2,4,1:4], tt(c(0, 1, 0, 0))) # second base of first seq is an C

  expect_equal(dim(t), c(97, 140, 4))     # correct shape?
  expect_equal(dimnames(t)[[1]][1], "EP535473")
  expect_equal(dimnames(t)[[1]][2], "EP7180000000023_Z")


  t2 <- fasta_encode_tensor(system.file("extdata", "seqs.fasta", package = "tensortree"), start = 4, end = 6)
  expect_equal(t2[1,1,1:4], tt(c(0, 0, 0, 1))) # first base of first seq is a T
  expect_equal(t2[1,4,1:4], tt(c(0, 0, 1, 0))) # fourth base of first seq is a G

  t3 <- fasta_encode_tensor(system.file("extdata", "seqs.fasta", package = "tensortree"), ids = c("EP7180000000023_Z", "EP483640", "EP535473", "EP7180000026554"))
  expect_equal(t3[1,1,1:4], tt(c(1, 0, 0, 0))) # first base of first seq is a A
  expect_equal(t3[1,4,1:4], tt(c(0, 1, 0, 0))) # fourth base of first seq is a C
  expect_equal(t3[2,1,1:4], tt(c(0, 1, 0, 0))) # first base of second seq is a C
  expect_equal(t3[2,4,1:4], tt(c(0, 1, 0, 0))) # fourth base of second seq is a C

  expect_equal(dimnames(t3)[[1]][1], "EP7180000000023_Z")
  expect_equal(dimnames(t3)[[1]][2], "EP483640")

})


test_that("fasta_train_batch() works properly", {
  t <- fasta_train_batch(system.file("extdata", "seqs.fasta", package = "tensortree"),
                           ids = c("EP7180000000023_Z", "EP483640", "EP535473", "EP7180000026554"),
                           targets = c("one", "two", "three", "two"),
                           keras_categorical_targets = TRUE)
  t_tensor <- t[[1]]
  t_target <- t[[2]]
  expect_equal(t_tensor[1,1,1:4], tt(c(1, 0, 0, 0))) # first base of first seq is a A
  expect_equal(t_tensor[1,4,1:4], tt(c(0, 1, 0, 0))) # fourth base of first seq is a C
  expect_equal(t_tensor[2,1,1:4], tt(c(0, 1, 0, 0))) # first base of second seq is a C
  expect_equal(t_tensor[2,4,1:4], tt(c(0, 1, 0, 0))) # fourth base of second seq is a C

  expect_equal(dimnames(t_tensor)[[1]][1], "EP7180000000023_Z")
  expect_equal(dimnames(t_tensor)[[1]][2], "EP483640")

  # ordering of labels is "one", "three", "two" (default factor ordering)
  expect_equal(t_target[1,], tt(c(1, 0, 0))) # first target should be encoded as c(1, 0)
  expect_equal(t_target[2,], tt(c(0, 0, 1))) # second target should be encoded as c(0, 1)

  expect_equal(dimnames(t_target)[[1]][1], "EP7180000000023_Z") # first dim, seq names
  expect_equal(dimnames(t_target)[[2]][1], "one") # second dim, target labels
  expect_equal(dimnames(t_target)[[2]][2], "three") # second dim, target labels
  expect_equal(dimnames(t_target)[[2]][3], "two") # second dim, target labels

  # check factor ordering?
  t <- fasta_train_batch(system.file("extdata", "seqs.fasta", package = "tensortree"),
                         ids = c("EP7180000000023_Z", "EP483640", "EP535473", "EP7180000026554"),
                         targets = factor(c("one", "two", "three", "two"), levels = c("one", "two", "three")),
                         keras_categorical_targets = TRUE)
  t_tensor <- t[[1]]
  t_target <- t[[2]]
  expect_equal(dimnames(t_target)[[2]][1], "one") # second dim, target labels
  expect_equal(dimnames(t_target)[[2]][2], "two") # second dim, target labels
  expect_equal(dimnames(t_target)[[2]][3], "three") # second dim, target labels

})


test_that("flow_sequences_from_fasta() works properly", {
  gen <- flow_sequences_from_fasta(system.file("extdata", "seqs.fasta", package = "tensortree"),
                         ids = c("EP7180000000023_Z", "EP483640", "EP535473", "EP7180000026554"),
                         targets = c("one", "two", "three", "two"),
                         keras_categorical_targets = TRUE,
                         batch_size = 2)
  t1 <- gen()
  t_tensor <- t1[[1]]
  t_target <- t1[[2]]
  expect_equal(t_tensor[1,1,1:4], tt(c(1, 0, 0, 0))) # first base of first seq is a A
  expect_equal(t_tensor[1,4,1:4], tt(c(0, 1, 0, 0))) # fourth base of first seq is a C
  expect_equal(t_tensor[2,1,1:4], tt(c(0, 1, 0, 0))) # first base of second seq is a C
  expect_equal(t_tensor[2,4,1:4], tt(c(0, 1, 0, 0))) # fourth base of second seq is a C

  expect_equal(dimnames(t_tensor)[[1]][1], "EP7180000000023_Z")
  expect_equal(dimnames(t_tensor)[[1]][2], "EP483640")

  # ordering of labels is "one", "three", "two" (default factor ordering)
  expect_equal(t_target[1,], tt(c(1, 0, 0))) # first target should be encoded as c(1, 0)
  expect_equal(t_target[2,], tt(c(0, 0, 1))) # second target should be encoded as c(0, 1)

  expect_equal(dimnames(t_target)[[1]][1], "EP7180000000023_Z") # first dim, seq names
  expect_equal(dimnames(t_target)[[2]][1], "one") # second dim, target labels
  expect_equal(dimnames(t_target)[[2]][2], "three") # second dim, target labels
  expect_equal(dimnames(t_target)[[2]][3], "two") # second dim, target labels


  t2 <- gen()
  t_tensor <- t2[[1]]
  t_target <- t2[[2]]
  print(t_tensor, bottom = "2d", show_names = T)
  expect_equal(t_tensor[1,1,1:4], tt(c(0, 0, 0, 1))) # first base of first seq is a T
  expect_equal(t_tensor[1,4,1:4], tt(c(1, 0, 0, 0))) # fourth base of first seq is a A
  expect_equal(t_tensor[2,1,1:4], tt(c(0, 1, 0, 0))) # first base of second seq is a C
  expect_equal(t_tensor[2,4,1:4], tt(c(0, 0, 0, 1))) # fourth base of second seq is a T

  expect_equal(dimnames(t_tensor)[[1]][1], "EP535473")
  expect_equal(dimnames(t_tensor)[[1]][2], "EP7180000026554")

  # ordering of labels is "one", "three", "two" (default factor ordering)
  expect_equal(t_target[1,], tt(c(0, 1, 0))) # first target should be encoded as "three" (c(0, 1, 0))
  expect_equal(t_target[2,], tt(c(0, 0, 1))) # second target should be encoded as "two" (c(0, 0, 1))

  expect_equal(dimnames(t_target)[[1]][1], "EP535473") # first dim, seq names
  expect_equal(dimnames(t_target)[[2]][1], "one") # second dim, target labels
  expect_equal(dimnames(t_target)[[2]][2], "three") # second dim, target labels
  expect_equal(dimnames(t_target)[[2]][3], "two") # second dim, target labels


  gen <- flow_sequences_from_fasta(system.file("extdata", "seqs.fasta", package = "tensortree"),
                                   ids = c("EP7180000000023_Z", "EP483640", "EP535473", "EP7180000026554"),
                                   targets = c(-1, 2, 3, 0),
                                   keras_categorical_targets = FALSE,
                                   batch_size = 2)
  t3 <- gen()
  t_tensor <- t3[[1]]
  t_target <- t3[[2]]
  expect_equal(t_target[1], tt(-1))
  expect_equal(t_target[2], tt(2))

  t4 <- gen()
  t_tensor <- t4[[1]]
  t_target <- t4[[2]]
  expect_equal(t_target[1], tt(3))
  expect_equal(t_target[2], tt(0))
})



