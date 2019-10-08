#' @export
shuffle <- function(t, seed = NULL) {UseMethod("shuffle", t)}


#' @export
#' @title Shuffle a tidytensor in the first rank.
#'
#' @description Shuffle's the entries in the first rank of a tensor. For example, if
#' \code{x} has shape (3, 5, 5), it may be indexed as \code{x[c(2, 3, 1), , ]}.
#' It's possible to set a custom seed for repeatable shuffling (amongst tensors with
#' the same size in the first rank).
#'
#'
#' @details Since tidytensor consider tensors as representing hierarchical "set of" relationships,
#' shuffling in any rank other than the first would permute lower entities across set boundaries
#' in higher ranks. For example, in a set of color images of shape (500, 28, 28, 3), shuffling the last rank
#' would re-order the channels, but identically for all the images. See \code{\link{tt_apply()}} for applying functions
#' (such as shuffle) over lower ranks of a tensor.
#'
#'
#'
#' @param t the tidytensor to apply over.
#' @param seed random seed to be used for shuffling.
#' @param ... unused
#' @return a tidytensor of the same shape.
#' @seealso \code{\link{tt_apply()}}, \code{\link{c.tidytensor}}, \code{\link{permute.tidytensor}}
#' @examples
#' # shape [100, 26, 26]
#' t <- as.tidytensor(array(rnorm(100 * 26 * 26), dim = c(100, 26, 26)))
#' ranknames(t) <- c("sample", "row", "col")
#' print(t)
#'
#' t <- shuffle(t, seed = 42)
#'
shuffle.tidytensor <- function(t, seed = NULL) {
  if(!is.null(seed)) {
    set.seed(seed)
  }

  sample <- sample(1:dim(t)[1])
  return(subset(t, sample))
}
