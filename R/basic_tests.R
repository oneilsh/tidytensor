if(FALSE) {

library(tidytensor)


runif(1:(4*5*1*7*5)) %>%
  array(dim = c(4, 5, 1, 7, 5)) %>%
  tt() %>%
  set_ranknames(one, two, three, four, five) %>%
  set_dimnames_for_rank(one, .dots = letters[1:4]) %>%
  set_dimnames_for_rank(two, .dots = letters[1:5]) %>%
  set_dimnames_for_rank(four, .dots = letters[1:7]) %>%
  set_dimnames_for_rank(five, .dots = rev(letters[1:5])) %>%
  print(show_names = F, bottom = "3d", max_per_level = 2)


t1 <- c(1.4, 2.5, 0.5, 1.3)
t1[3]

names(t1) <- c("a", "b", "c", "d")
t1["c"]


t2 <- matrix(1:6, nrow = 2, ncol = 3)
t2
t2[1, 2]

dimnames(t2) <- list(c("row1", "row2"), c("col1", "col2", "col3"))
t2
t2["row1", "col2"]


library(tidytensor)
array(rnorm(4*3*10*10), dim = c(4, 3, 10, 10)) %>%
  tt()


images <- array(rnorm(4*3*10*10), dim = c(4, 3, 10, 10))
dimnames(images)[[3]] <- letters[1:10]
images %>%
  tt() %>%
  print(show_names = T, max_rows = 10, max_cols = 10)


images %>%
  tt() %>%
  print(max_per_level = 2)


library(keras)
dataset_mnist()$train$x %>%
  tt() %>%
  print(max_rows = 28, max_cols = 28, max_per_level = 2)

array(rnorm(4*10*10*3), dim = c(4, 10, 10, 3)) %>%
  tt() %>%
  print(bottom = "3d")


array(rnorm(4*10*8), dim = c(4, 10, 8)) %>%
  tt() %>%
  print(bottom = "1d", max_per_level = 2)


images <- dataset_cifar10()$train$x

images <- tt(images)
ranknames(images) <- c("image", "row", "col", "channel")
images

# OR
images %>%
  tt() %>%
  set_ranknames(image, row, col, channel)

# OR
images %>%
  tt() %>%
  set_ranknames(.dots = c("image", "row", "col", "channel"))

# OR
images %>%
  tt() %>%
  set_ranknames(image, row, col, channel) %>%
  permute(image, channel, row, col) %>%
  set_dimnames_for_rank(channel, R, G, B) %>%
  print(max_per_level = 2)


} # end if(FALSE)
