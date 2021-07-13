#' @export
#' @title Concatenate two or more tidytensors to create a new one with the same rank
#'
#' @description Given multiple tidytensors of the same shape except the first rank, concatenates them together to create a tidytensor
#' of the same shape, but larger in the first. For example, c(x, y, z) where x and have shape [2, 3, 5] and z has shape [10, 3, 5] returns a new tidytensor
#' of shape [14, 3, 5].
#'
#' @details All input tidytensors must have the same shape except for the first rank. If the input ranknames
#' conflict, only those of the first input tidytensor will be used, and a warning will be generated.
#'
#' @param ... a number of tidytensors of the same shape, or a single list of them.
#' @return a new tidytensor.
#' @seealso \code{\link{bind}}
#' @examples
#' # Three tidytensors of the same shape
#' t1 <- as.tidytensor(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' t2 <- as.tidytensor(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' t3 <- as.tidytensor(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' ranknames(t1) <- c("sample", "row", "col")
#' ranknames(t2) <- c("sample", "row", "col")
#' ranknames(t3) <- c("sample", "row", "col")

#' t4 <- stitch(t1, t2, t3)
#' print(t4)
#'
#' list_example <- list(t1, t2, t3)
#' t5 <- stitch(list_example)
#' print(t5)
`stitch` <- function(...) {UseMethod("stitch", list(...)[[1]])}

# I wrote the original to be agnostic over a list of tidytensor or just a bunch of them, so I'll just dispatch list to that
#' @export
`stitch.list` <- function(...) {return(stitch.tidytensor(...))}

# method
#' @export
`stitch.tidytensor` <- function(...) {
  # damnit; I need to get the first element of ..., whether its separate arguments or a single list,
  # so that I can get the ranknames() of that thing.
  input_list <- list(...)
  if(is.list(input_list[[1]])) { # if we accidentally wrapped a list...
    input_list <- input_list[[1]]
  }
  # first we call ranknames on all of them...
  ranknames_list <- lapply(input_list, ranknames)
  if(length(unique(ranknames_list)) > 1) {
    warning("Conflicting ranknames during bind(). Only the first will be used.")
  }

  concatted <- as.tidytensor(abind::abind(..., along = 1))
  example_ranknames <- ranknames_list[[1]]   # if ... is already a list, as.list leaves it so.
  if(!is.null(example_ranknames)) {
    ranknames(concatted) <- example_ranknames
  }
  return(concatted)
}
