if(FALSE) {


  # as.tidytensor.factor(ggplot2::diamonds$cut)
  # testing - probably need to make an as.tidytensor generic that can dispatch
  as.tidytensor.factor <- function(xfac) {
    num_levels <- length(levels(xfac))
    level_names <- levels(xfac)
    ints <- as.numeric(xfac)
    arr <- array(rep(ints, num_levels), dim = c(length(xfac), num_levels))
    arr <- apply(arr, MARGIN = 1, FUN = function(x) {
      keep <- x[1]
      x[T] <- 0
      x[keep] <- 1
      x
    })
    x <- as.tidytensor(arr)
    x <- permute(x, 2, 1)
    ranknames(x) <- c("entry", "label")
    dimnames(x)$label <- level_names
    set_dimnames_for_rank(x, "label", .dots = level_names)
    x
  }


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

images[1:4, , , ] %>%
  tt() %>%
  set_ranknames(image, row, col, channel) %>%
  permute(image, channel, row, col) %>%
  set_dimnames_for_rank(channel, R, G, B) %>%
  as.data.frame() %>%
  head()

library(ggplot2)
library(tidyr)

images <- dataset_cifar10()$train$x %>%
  tt() %>%
  set_ranknames(image, row, col, channel) %>%
  permute(image, channel, row, col) %>%
  set_dimnames_for_rank(channel, R, G, B)


images[1:4, 1, , ] <- images[1:4, 1, , ] * -1

images[1:4, c(2, 3, 1), , ] %>%
  as.data.frame() %>%
  #spread(channel, value) %>%
  ggplot() +
    geom_tile(aes(x = col, y = row, fill = value)) + #fill = rgb(R, G, B, maxColorValue = 255))) +
    facet_grid(channel ~ image) +
    coord_equal() +
    scale_y_reverse() +
    scale_fill_gradient2()

vgg_model <- application_vgg16(include_top = FALSE, input_shape = c(32, 32, 3))

input <- vgg_model$input
output <- get_layer(vgg_model, name = "block1_conv2")$output

compute_featuremaps <- k_function(input, output)

library(dplyr)

compute_featuremaps(images[1:4, , ,]) %>% # produces shape (4, 32, 32, 64) tensor, where last rank are feature maps
  tt() %>%
  set_ranknames(image, row, col, featuremap) %>%
  bind(tt(images[1:4, , ,]), new_rank_name = "type") %>%
  set_dimnames_for_rank(type, featuremap, input) %>%
  as.data.frame(allow_huge = T) %>%
  filter(featuremap <= 6) %>%
  ggplot() +
    geom_tile(aes(x = col, y = row, fill = value)) +
    facet_grid(image ~ featuremap + input) +
    coord_equal() +
    scale_y_reverse()

images <- dataset_cifar10()$train$x
images <- tt(images)

seta <- images[1:4, , ,]
setb <- images[5:7, , ,]
setc <- images[8:16, , ,]



test <- function(...) {
  str(list(...))
}

test(a = 4, b = 5)


} # end if(FALSE)
