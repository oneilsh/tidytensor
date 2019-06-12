#' @export
#' @title Convert a tensortree to a data.frame representation.
#'
#' @description Given a tensortree, returns a data.frame, with each rank of the tensor being represented by a column.
#' Produces an error if the resulting data.frame would have more than 1 million entries; use \code{allow_huge = TRUE} to override.
#'
#' @details Note that this produces a row for each value in the tensor, and a column for each rank; data.frames are a much less
#' efficient representation, but can be useful for e.g. visualization purposes. This method thus produces an error if
#' the resulting data.frame would have more than 1 million entries; but one can set \code{allow_huge = TRUE} to override.
#' If dimnames() are set (naming each dimension), then the columns will be factors, rather than integer indices.
#'
#' If the tensortree ranks are not named, columns will be named \code{index_1}, \code{index_2}, etc., otherwise they will be
#' set to ranknames.
#' Tensor values will be in a column named \code{value}.
#'
#' @param x input to convert to a data.frame
#' @param allow_huge whether to allow the creation of a data.frame with more than 1 million entries
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a data.frame
#' @seealso \code{\link{ranknames}}.
#' @examples
#' # From an array (representing e.g. 30 26x26 images (30 sets of 26 rows of 26 pixels))
#' a <- array(rnorm(30 * 26 * 26), dim = c(30, 26, 26))
#' t <- as.tensortree(a)
#' ranknames(t) <- c("sample", "row", "pixel")
#' df <- as.data.frame(t)
#' print(head(df))
#'
#' # Example with named dimensions:
#' dimnames(t)[[1]] <- paste("sample", 1:30, sep = "_")
#' dimnames(t)[[2]] <- paste("row", 1:26, sep = "_")
#' dimnames(t)[[3]] <- paste("pixel", 1:26, sep = "_")
#' # or with a list:
#' dimnames(t) <- list(paste("sample", 1:30, sep = "_"),
#'                     paste("row", 1:26, sep = "_"),
#'                     paste("pixel", 1:26, sep = "_"))
#'
#' print(head(as.data.frame(t)))
as.data.frame.tensortree <- function(tensor, allow_huge = FALSE) {
  if(length(tensor) * length(dim(tensor)) > 1000000 & !allow_huge) {
    stop("The data frame you are trying to create from a tensor will have more than 1 million entries (in this case ",
         length(tensor),
         " rows and ",
         length(dim(tensor)),
         " columns). To retry and allow the creation of this huge data frame, set allow_huge = TRUE."
    )
  }

  colnames <- ranknames(tensor)
  # if there are no ranknames, the column names will be index_1, index_2, etc.
  if(is.null(colnames)) {
    colnames <- paste("index", seq(1, length(dim(tensor))), sep = "_")
  }

  nrows <- length(tensor)
  dimnames_list <- dimnames(tensor)
  # if there are no dimnames, the "dimnames" will be indices into the ranks
  if(is.null(dimnames_list)) {
    dimsizes <- dim(tensor)
    dimnames_list <- lapply(dimsizes, function(size) {return(seq(1, size))})
  } else { # there are dimnames, but they might be a mess
    for(rank_index in 1:length(dimnames_list)) {
      rank_dimnames <- dimnames_list[[rank_index]]
      if(any(!is.na(rank_dimnames))) { # if there are any already set, we'll assume its a factor/categorical
        rank_dimnames[is.na(rank_dimnames)] <- seq(1, length(rank_dimnames))[is.na(rank_dimnames)]
        dimnames_list[[rank_index]] <- factor(rank_dimnames, levels = unique(rank_dimnames))
      } else { # otherwise it's just indices
        rank_dimnames <- seq(1, length(rank_dimnames))
      }
      dimnames_list[[rank_index]] <- rank_dimnames
    }
  }


  # we're going to pre-generate a dataframe of the right size, containing factors with the right levels.
  # first we use lapply() to generate a list of columns, with the first entry repeated as necessary
  dummy_cols <- lapply(dimnames_list, function(dimnames) {return(rep(dimnames[1], nrows))})

  # build a data frame from that, set the colnames from the ranknames, and add in the values from the tensor itself
  big_df <- data.frame(dummy_cols)
  colnames(big_df) <- colnames
  big_df$value <- array(tensor, dim = length(tensor))

  # for each rank index, fill it according to R's array deconstructing rules
  subfill <- dimnames_list[[1]]
  big_df[, 1] <- rep_len(subfill, nrows)
  if(length(dim(tensor)) > 2) {
    for(i in seq(2,length(dim(tensor)))) {
      dimnames_i <- dimnames_list[[i]]
      last_subfill <- subfill
      subfill <- rep(dimnames_i, each = length(last_subfill))
      big_df[, i] <- rep_len(subfill, nrows)
    }
  }

  return(big_df)
}
