# #' @export
#subset.tidytensor <- function(t, ...) {UseMethod("subset", t)}


#' @export
#' @title Subset dimensions of a tidytensor
#'
#' @description A functional form of e.g. \code{tensor[1:10, 3, ]}, supporting selecting by ranknames, usage with %>%, and
#' indexing when the rank is unknown.
#'
#' @details Subsetting a tidytensor with \code{subset()} as opposed to \code{[]} allows for subsetting even when the number of ranks of the input is unknown; see examples.
#'
#'
#'
#' @param x the tidytensor to apply over.
#' @param drop whether to drop ranks with size 1 (similar to \code{x[..., drop = TRUE]})
#' @param ... named or unnamed parameters specifying subsetting criteria (see examples)
#' @return a tidytensor
#' @seealso \code{\link{shuffle}}, \code{\link{permute}}
#' @examples
#' # shape [100, 26, 26, 3]
#' t <- as.tidytensor(array(rnorm(100 * 26 * 26 * 3), dim = c(100, 26, 26, 3)))
#' ranknames(t) <- c("sample", "row", "col", "channel")
#' t <- set_dimnames_for_rank(t, channel, R, G, B)
#' print(t)
#'
#  # equiv of t2[27, 1:10, , , drop = FALSE]
#' t2 <- subset(t, row = 1:10, sample = 27, drop = FALSE)
#' print(t2)
#'
#' # same thing, but without named ranks (not a good idea to mixes named
#' # subsetting and non-named subsetting)
#' t2 <- subset(t, 27, 1:10, drop = FALSE)
#' print(t2)
#'
#' # equiv of t3[1:20, , , c("G", "R", "B")] # since the last rank has dimnames()
#' # note the re-ordering of channel levels
#' t3 <- subset(t, sample = 1:20, channel = c("G", "R", "B"), drop = FALSE)
#' print(t3)
#'
subset.tidytensor <- function(x, ..., drop = TRUE) {
  select_list <- list(...)
  orig_dimnames <- dimnames(x) # may be NULL
  orig_ranknames <- ranknames(x) # may be NULL
  select_names <- names(select_list) # may be NULL

  if(!is.null(select_names) & is.null(orig_ranknames)) {stop("cannot subset by ranknames on a tensor with no ranknames.")}

  shape <- as.list(dim(x))
  dimselector <- lapply(shape, function(size) {return(1:size)})

  if(!is.null(select_names)) {
    if(any(select_names %in% c("", NA))) {stop("when subsetting a tensor, either all ranks must be selected by name, or none.")}
    for(i in 1:length(select_list)) {
      rankname <- names(select_list)[[i]]
      if(! rankname %in% orig_ranknames) {
         stop(paste0("rankname not found: ", rankname))
      }
      rankname_index <- which(rankname == orig_ranknames)[1]
      dimselector[[rankname_index]] <- select_list[[i]]
    }
  } else {
    for(i in 1:length(select_list)) {
      dimselector[[i]] <- select_list[[i]]
    }
  }

  args_list <- list(1:(length(dimselector) + 2)) # one for the tensor itself, one for the drop param
  args_list[[1]] <- x
  i <- 2
  for(dimselection in dimselector) {
    args_list[[i]] <- dimselection
    i <- i + 1
  }
  args_list[[i]] <- drop
  names(args_list)[i] <- "drop"

  # this is going to strip ranknames, grrr
  result <- do.call("[", args_list)
  result <- tt(result)

  return(result)
}





