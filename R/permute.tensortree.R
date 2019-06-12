#' @export
#' @title Permute the ranks of a tensor
#'
#' @description Permute the ranks of a tensor, for example to convert between "channels first" and "channels last" representations.
#'
#' Ranknames are respected for both inputs and return values.
#'
#' @details The \code{rank} parameter may be an integer numeric vector (for permuting by index), or character vector (for permuting).
#'
#'
#'
#' @param x the tensortree permute.
#' @param rank the permutation vector.
#' @param ... additional arguments passed to FUN.
#' @return a new tensortree.
#' @seealso \code{\link{index}}, \code{\link{c.tensortree}}
#' @examples
#' # shape [20, 26, 26]
#' t <- as.tensortree(array(rnorm(20 * 26 * 26), dim = c(20, 26, 26)))
#' ranknames(t) <- c("sample", "row", "col")
#' print(t)
#'
#' t2 <- permute(t, c(3, 1, 2))
#' t3 <- permute(t, c("col", "sample", "row"))
#'
permute <- function(tensor, rank, ...) {UseMethod("permute", tensor)}

#' @export
permute.tensortree <- function(tensor, rank = TRUE) {
  permute_vec <- rank_to_index(tensor, rank)

  # TODO: create a version tt_index that can subset multiple dimensions, dropping others, so that we can permute AND select
  if(length(permute_vec) != length(dim(tensor))) {
    stop("Error in permute: permutation vector must be same size as rank of tensor.")
  }

  tensor <- tt(aperm(tensor, permute_vec))
  return(tensor)
}
