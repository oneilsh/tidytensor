# this is all internally used stuff

# e.g. quovars(a, b, "c", d) returns c("a", "b", "c", "d")
# and quovars(.dots = c("a", "b", "c", "d")) returns same
quovars <- function(..., .dots = NULL) {
  # get dots
  if(!is.null(.dots)) {
    return(.dots)
  }
  .dots <- rlang::enexprs(...)
  lang <- unlist(lapply(rlang::enexprs(...), is.language))
  labels <- purrr::map_at(.dots, which(lang), as.character)
  labels <- unlist(labels)
  names(labels) <- NULL
  return(labels)
}


#' @export
rank_to_index <- function(tensor, by = TRUE) {UseMethod("rank_to_index", tensor)}


# used to convert a rank selector (numeric, character (by rankname), or logical)
# to an index vector (numeric)
#' @export
rank_to_index.tidytensor <- function(tensor, by = TRUE) {
  permute_vec <- seq(1:length(dim(tensor))) # no change by default
  if(mode(by) == "logical") {
    logical_full <- rep(by, length.out = length(dim(tensor)))
    permute_vec <- which(logical_full)
  } else if(mode(by) == "numeric") {
    permute_vec <- by

    # there's got to be an easier way to do this case in R; all I want to do is turn
    # c("nameone", "nametwo", "namethree") and c("namethree", "nameone") into c(3, 1)
  } else if(mode(by) == "character") {
    if(is.null(ranknames(tensor))) {
      stop("Cannot select ranks by name for unnamed tensor.")
    }
    selector <- rep(NA, length(dim(tensor)))
    for(i in seq(1, length(by))) {
      entry <- by[i]
      selector[i] <- which(ranknames(tensor) == entry)
    }
    permute_vec <- selector
    permute_vec <- permute_vec[!is.na(permute_vec)]
  }
  return(permute_vec)
}


#' @export
partition <- function(tensor, sizes = c(0.5, 0.5)) {UseMethod("partition", tensor)}


#' @export
#' @title Partition a tidytensor into a list of smaller tidytensors of the same rank
#'
#' @description Partitions a tensor into pieces of sizes relative to \code{sizes}; e.g. a
#' tensor with shape (24, 50, 50, 3) partitioned with \code{partition(sizes = c(0.5, 0.5))}
#' results in a list of two tensors of shape (12, 50, 50, 3).
#'
#' TODO: Ranknames are respected for both inputs and return values.
#'
#' @details Entries in \code{sizes} are treated as relative, so \code{sizes = c(2, 1, 1)}
#' is equivalent to \code{sizes = c(0.5, 0.25, 0.25)}. Non-integer parition boundaries are
#' rounded down, and this may result in entries with shape (0, ...), but only when
#' the size of the first rank is smaller than the number of partitions requested.
#'
#'
#'
#' @param x the tidytensor to apply over.
#' @param sizes relative sizes of partitions
#' @param ... unused
#' @return a list of tidytensors.
#' @seealso \code{\link{index}}, \code{\link{c.tidytensor}}, \code{\link{permute.tidytensor}}
#' @examples
#' # shape [100, 26, 26]
#' t <- as.tidytensor(array(rnorm(100 * 26 * 26), dim = c(100, 26, 26)))
#' ranknames(t) <- c("sample", "row", "col")
#' print(t)
#'
#' partitions <- partition(t, c(0.2, 0.8))
#' print(partitions)
#'
partition.tidytensor <- function(x, sizes = c(0.5, 0.5)) {
  sizes <- sizes / sum(sizes)
  num_entries <- dim(x)[1]

  # https://tinyurl.com/y5qjh9pw
  partition_sizes <- diff(as.integer(cumsum(c(0, sizes)) * num_entries))

  partitions <- as.list(rep(0, length(sizes)))

  sum_accounted <- 0
  i <- 1
  for(partition_size in partitions_sizes) {
    if(partition_size != 0) {
      range <- (sum_accounted + 1):(sum_accounted + partition_size)
      subx <- tt_index(x, indices = range, dimension = 1, drop = FALSE)
    } else {
      subx <- tt_index(x, indices = 0, dimension = 1, drop = FALSE)
    }

    partitions[[i]] <- subx
    sum_accounted <- sum_accounted + partition_size
    i <- i + 1
  }

  return(partitions)
}

#' @export
tt_index <- function(tensor, indices, dimension = 1, drop = TRUE) {UseMethod("tt_index", tensor)}


# just subsets a tensor *without* knowing its rank
# eg suppose we want some_tensor[ , ,1:10 , , ], which is a rank-5 tensor, but we don't know that to begin with,
# we can use tt_index(some_tensor, 1:10, dimension = 3)
#' @export
tt_index.tidytensor <- function(tensor, indices, dimension = 1, drop = TRUE) {
  # here's where it gets tricky: we need to grab the first couple of entries
  # from the first dimenions of the tensor. E.g. if dim(tensor) is c(10, 10, 10, 10), we
  # want tensor[1:n, , , ]. But we can't write it that way because we don't know the rank to hard-code it.
  # The acorn() function (from the abind() package) can help with this, but it also
  # needs to take arguments like acorn(tensor, 2, 10, 10, 10). Fortunately we can use do.call()
  # to call a function with parameters generated from a list.
  args_list <- list();

  # get the dimensions, build the argument list from the tensor and the dimension sizes
  tensor_dims <- dim(tensor)
  args_list[[1]] <- tensor
  for(i in 1:length(tensor_dims)) {
    if(tensor_dims[i] == 0) {
      args_list[[i+1]] <- 0
    } else {
      # yup, this is O(rank^2)
      args_list[[i+1]] <- seq(1,tensor_dims[i])
    }
  }

  # we only want the selected entries in the nth dimenion (first is at index 2 of the param list)
  args_list[[dimension + 1]] <- indices
  args_list[[length(args_list) + 1]] <- drop
  names(args_list)[length(args_list)] <- "drop"
  # make the call
  res <- do.call("[", args_list)
  return(as.tidytensor(res))
}

# these are surprisingly slow, too slow for a loop
# derp, we don't even need them, for some reason

#`[<-.tidytensor` <- function(x, ...) {
#  class(x) <- class(x)[class(x) != "tidytensor"]
#  x <- `[<-`(x, ...)
#  #x <- tt(x)
#  class(x) <- c("tidytensor", class(x))
#  return(x)
#}



# `[.tidytensor` <- function(x, ...) {
#   names <- ranknames(x)
#   class(x) <- class(x)[class(x) != "tidytensor"] # drop the tidytensor class but keep others
#   xnew <- tt(x[...]) #`[`(x, ...)
#
#   #if(!is.null(names) & length(dim(x)) == length(dim(xnew))) {
#   #  ranknames(xnew) <- names
#   #}
#   # alright, so the bummer is that if what is returned is a vector (1d tensor), it drops the names. (YTHO)
#   # I'm not really sure what to do about this...
#   return(xnew)
# }



