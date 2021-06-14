#' @export
#' @title Permute the ranks of a tensor
#'
#' @description Permute the ranks of a tensor, for example to convert between "channels first" and "channels last" representations.
#'
#' Ranknames are respected for both inputs and return values.
#'
#' @details The \code{rank} parameter may be an integer numeric vector (for permuting by index), or character vector (for permuting by rankname).
#'
#'
#'
#' @param x the tidytensor permute.
#' @param ... ranknames or integers to permute by (quoted or unquoted).
#' @param .dots character or integer vector to permute by.
#' @return a new tidytensor.
#' @seealso \code{\link{index}}, \code{\link{c.tidytensor}}
#' @examples
#' # shape [20, 26, 26]
#' t <- as.tidytensor(array(rnorm(20 * 26 * 26), dim = c(20, 26, 26)))
#' ranknames(t) <- c("sample", "row", "col")
#' print(t)
#'
#' t2 <- permute(t, col, sample, row)
#' t2 <- permute(t, 3, 1, 2)
#' t2 <- permute(t, .dots = c(3, 1, 2))
#' t2 <- permute(t, .dots = c("col", "sample", "row"))
#'
permute <- function(tensor, ...) {UseMethod("permute", tensor)}

#' @export
permute.tidytensor <- function(tensor, ...) {
  vars <- quovars(...)
  permute_vec <- rank_to_index(tensor, vars)

  # nevermind, this doesn't even make sense, values would have to collapse: create a version tt_index that can subset multiple dimensions, dropping others, so that we can permute AND select
  if(length(permute_vec) != length(dim(tensor))) {
    stop("Error in permute: permutation vector must be same size as rank of tensor.")
  }

  tensor <- tt(aperm(tensor, permute_vec)) # keep.class = T doesn't work here either, hm
  return(tensor)
}
