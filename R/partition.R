#' @export
#' @title Partition a tidytensor into a list of smaller tidytensors of the same rank
#'
#' @description Partitions a tensor into pieces of sizes relative to \code{sizes}; e.g. a
#' tensor with shape (24, 50, 50, 3) partitioned with \code{partition(sizes = c(0.5, 0.5))}
#' results in a list of two tensors of shape (12, 50, 50, 3).
#'
#' Ranknames are respected for both inputs and return values.
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
#' @seealso \code{\link{c}}, \code{\link{permute}}
#' @examples
#' # shape [100, 26, 26]
#' t <- as.tidytensor(array(rnorm(100 * 26 * 26), dim = c(100, 26, 26)))
#' ranknames(t) <- c("sample", "row", "col")
#' print(t)
#'
#' partitions <- partition(t, c(0.2, 0.8))
#' print(partitions)
partition <- function(x, sizes = c(0.5, 0.5)) {UseMethod("partition", x)}


#' @export
partition.tidytensor <- function(x, sizes = c(0.5, 0.5)) {
  sizes <- sizes / sum(sizes)
  num_entries <- dim(x)[1]

  # https://tinyurl.com/y5qjh9pw
  partition_sizes <- diff(as.integer(cumsum(c(0, sizes) * num_entries)))

  partitions <- as.list(rep(0, length(sizes)))

  sum_accounted <- 0
  i <- 1
  for(partition_size in partition_sizes) {
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
