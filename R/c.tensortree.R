#' @export
#' @title Concatenate two or more tensortrees to create a new one with the same rank
#'
#' @description Given multiple tensortrees, concatenates them together to create a tensortree
#' of the same rank, but larger in the first dimension. For example, c(x, y, z) where x, y, and z have shape [2, 3, 5] returns a new tensortree
#' of shape [6, 3, 5].
#'
#' @details All input tensortrees must have the same shape. If the input ranknames
#' conflict, only those of the first input tensortree will be used, and a warning will be generated.
#'
#' @param ... a number of tensortrees of the same shape, or a single list of them.
#' @return a new tensortree.
#' @seealso \code{\link{bind}}
#' @examples
#' # Three tensortrees of the same shape
#' t1 <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' t2 <- as.tensortree(array(10 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' t3 <- as.tensortree(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' ranknames(t1) <- c("sample", "row", "col")
#' ranknames(t2) <- c("sample", "row", "col")
#' ranknames(t3) <- c("sample", "row", "col")

#' t4 <- c(t1, t2, t3)
#' summary(t4)
`c.tensortree` <- function(...) {
  ranknames_list <- lapply(list(...), ranknames)
  if(length(unique(ranknames_list)) > 1) {
   warning("Conflicting ranknames during c(). Only the first will be used.")
  }

  concatted <- as.tensortree(abind::abind(..., along = 1))
  example_ranknames <- ranknames_list[[1]]   # if ... is already a list, as.list leaves it so.
  if(!is.null(example_ranknames)) {
    ranknames(concatted) <- example_ranknames
  }
  return(concatted)
}
