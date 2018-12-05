#' @export
#' @title Convert a vector, matrix, or array to a tensortree type.
#'
#' @description Given a vector, matrix, or array, returns a tensortree.
#' If given a vector, converts to a 1-d array supporting dim(), matrices are left as matrices,
#' and in all cases the class 'tensortree' is added.
#'
#' @details Matrices are synonymous with 2-d arrays, so these are left as is. Vectors are converted
#' to 1-d arrays so that they can support dim().
#' @param x input to convert to a tensortree.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a new tensortree.
#' @seealso \code{\link{summary.tensortree}}, \code{\link{ranknames}}.
#' @examples
#' # From an array (representing e.g. 30 26x26 images (30 sets of 26 rows of 26 pixels))
#' a <- array(rnorm(30 * 26 * 26), dim = c(30, 26, 26))
#' t <- as.tensortree(a)
#' ranknames(t) <- c("sample", "row", "pixel")
#' summary(t)
#'
#' # From a matrix (representing e.g. a 26x26 image (26 rows of 26 pixels))
#' m <- matrix(rnorm(26 * 26), nrow = 26, ncol = 26)
#' t <- as.tensortree(m)
#' ranknames(t) <- c("row", "pixel")
#' summary(t)
#'
#' # From a vector (representing e.g. 26 pixel values)
#' v <- rnorm(26)
#' t <- as.tensortree(v)
#' ranknames(t) <- c("pixel")
#' summary(t)
as.tensortree <- function(x, ...) {
  if(!(is.vector(x) | any(c("matrix", "array") %in% class(x)))) {
    stop("Cannot convert to tensortree: input must be vector, matrix, or array.")
  }
  if(is.null(dim(x))) {
    x <- array(x, dim = length(x))
  }
  class(x) <- c("tensortree", class(x))
  return(x)
}




