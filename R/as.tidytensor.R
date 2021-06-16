#' @export
#' @importFrom magrittr %>%
#' @title Convert a vector, matrix, or array to a tidytensor type.
#'
#' @description Given a vector, matrix, or array, returns a tidytensor.
#' If given a vector, converts to a 1-d array supporting dim(), matrices are left as matrices,
#' and in all cases the class 'tidytensor' is added.
#'
#' @details Matrices are synonymous with 2-d arrays, so these are left as is. Vectors are converted
#' to 1-d arrays so that they can support dim().
#' @param x input to convert to a tidytensor.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a new tidytensor.
#' @seealso \code{\link{tt}}, \code{\link{ranknames}}.
#' @examples
#' # From an array (representing e.g. 30 26x26 images (30 sets of 26 rows of 26 pixels))
#' a <- array(rnorm(30 * 26 * 26), dim = c(30, 26, 26))
#' t <- as.tidytensor(a)
#' ranknames(t) <- c("sample", "row", "pixel")
#' print(t)
#'
#' # From a matrix (representing e.g. a 26x26 image (26 rows of 26 pixels))
#' m <- matrix(rnorm(26 * 26), nrow = 26, ncol = 26)
#' t <- as.tidytensor(m)
#' ranknames(t) <- c("row", "pixel")
#' print(t)
#'
#' # From a vector (representing e.g. 26 pixel values)
#' v <- rnorm(26)
#' t <- as.tidytensor(v)
#' ranknames(t) <- c("pixel")
#' print(t)
as.tidytensor <- function(x, ...) {
  if(!(is.vector(x) | any(c("matrix", "array") %in% class(x)))) {
    stop("Cannot convert to tidytensor: input must be vector, matrix, or array.")
  }
  if(is.null(dim(x))) {
    xres <- array(x, dim = length(x))
    if(!is.null(names(x))) {
      dimnames(xres) <- list(names(x))
    }
    x <- xres
  }
  class(x) <- c("tidytensor", class(x))
  return(x)
}


#' @export
#' @title Convert a vector, matrix, or array to a tidytensor type.
#'
#' @description \code{tt()} is a convenience shorthand for \code{as.tidytensor()}. Given a vector, matrix, or array, returns a tidytensor.
#' If given a vector, converts to a 1-d array supporting \code{dim()}, matrices are left as matrices,
#' and in all cases the class 'tidytensor' is added.
#'
#' @details Matrices are synonymous with 2-d arrays, so these are left as is. Vectors are converted
#' to 1-d arrays so that they can support \code{dim()}.
#' @param x input to convert to a tidytensor.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a new tidytensor.
#' @seealso \code{\link{print.tidytensor}}, \code{\link{ranknames}}.
#' @examples
#' # From an array (representing e.g. 30 26x26 images (30 sets of 26 rows of 26 pixels))
#' a <- array(rnorm(30 * 26 * 26), dim = c(30, 26, 26))
#' t <- tt(a)
#' ranknames(t) <- c("sample", "row", "pixel")
#' print(t)
#'
#' # From a matrix (representing e.g. a 26x26 image (26 rows of 26 pixels)) using %>%
#' library(magrittr)
#' t <- matrix(rnorm(26 * 26), nrow = 26, ncol = 26) %>% tt()
#' ranknames(t) <- c("row", "pixel")
#' print(t)
#'
#' # From a vector (representing e.g. 26 pixel values)
#' v <- rnorm(26)
#' t <- tt(rnorm(26))
#' ranknames(t) <- c("pixel")
#' print(t)
tt <- as.tidytensor


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
