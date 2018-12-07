#' @export
#' @title Bind two or more tensortrees to create a new one with a new rank.
#'
#' @description Given multiple tensortrees, or a list of tensortrees, binds them together to create a tensortree
#' of higher rank. For example, bind(x, y, z) where x, y, and z have shape [2, 3, 5] returns a new tensortree
#' of shape [3, 2, 3, 5].
#'
#' @details All input tensortrees must have the same shape. It's also possible to set a new rankname for the
#' newly created dimension; if ranknames were prevously unset lower ranknames are set to NA. If the input ranknames
#' conflict, only those of the first input tensortree will be used, and a warning will be generated.
#'
#' @param x a tensortree of the same shape, or a single list of them.
#' @param ... a number of additional tensortrees to bind with
#' @param new_rank_name a name (length-1 character vector) for the newly created rank.
#' @return a new tensortree.
#' @seealso \code{\link{ranknames}}, \code{\link{c.tensortree}}
#' @examples
#' # Three tensortrees of the same shape
#' t1 <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' t2 <- as.tensortree(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' t3 <- as.tensortree(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' ranknames(t1) <- c("sample", "row", "col")
#' ranknames(t2) <- c("sample", "row", "col")
#' ranknames(t3) <- c("sample", "row", "col")

#' t4 <- bind(t1, t2, t3, new_rank_name = "batch")
#' summary(t4)
`bind` <- function(..., new_rank_name = NULL) {UseMethod("bind", list(...)[[1]])}
setGeneric("bind")

# I wrote the original to be agnostic over a list of tensortree or just a bunch of them, so I'll just dispatch list to that
#' @export
`bind.list` <- function(..., new_rank_name = NULL) {return(bind.tensortree(..., new_rank_name = new_rank_name))}

# method
#' @export
`bind.tensortree` <- function(..., new_rank_name = NULL) {
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

  result <- as.tensortree(abind::abind(input_list, along = 0)) # abind does the actual work!
  base_names <- ranknames_list[[1]]

  if(is.null(new_rank_name)) {
    if(is.null(base_names)) {
      # nothing needed, neither had rank names
    } else {
      # no new name, but the inputs had some
      ranknames(result) <- c(NA, base_names)
    }
  } else {
    if(is.null(base_names)) {
      # new name for the new rank, but nothing there previously
      # need to do new_name, NA, NA, NA ...
      nas <- rep(NA, length(dim(list(...)[[1]])))  # the first of the list of input tensors, get the length of its dimension; repeat NA that many times
      ranknames(result) <- c(new_rank_name, nas)
    } else {
      # names and a new name
      ranknames(result) <- c(new_rank_name, base_names)
    }
  }
  return(result)
}
