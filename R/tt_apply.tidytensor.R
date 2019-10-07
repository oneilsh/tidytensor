#' @export
#' @title Apply a function over lower ranks of a tidytensor
#'
#' @description Applies a function over the lower ranks of a tidytensor, collecting
#' the results into a tidytensor. For example, if \code{FUN} is a function that takes a tidytensor
#' of shape [26, 26] and returns a tidytensor of shape [13, 13], then we could apply \code{FUN}
#' on a tidytensor of shape [3, 100, 26, 26] starting at rank 2 to get back one with shape [3, 100, 13, 13].
#' If \code{flatten = TRUE}, the higher ranks are collapsed to produce shape [300, 26, 26]
#'
#' Ranknames are respected for both inputs and return values.
#'
#' @details The \code{rank} argument should specify a single rank to apply over;
#' if \code{ranknames(t) <- c("sample", "rows", "cols", "channels")} then \code{rank = 2}, \code{rank = "rows"},
#' and \code{rank = c(FALSE, TRUE, FALSE, FALSE)} all indicate that \code{FUN} will be called on tidytensors
#' with ranknames \code{c("rows", "cols", "channels")}.
#'
#'
#'
#' @param x the tidytensor to apply over.
#' @param FUN the function to apply
#' @param rank an indicator of the rank to apply over (see details).
#' @param flatten whether to preserve the higher-rank structure, or collapse into a single rank (see description).
#' @param drop_final_1 If FUN returns a rank-0 tensor (length-1 vector), should it be collapsed? E.g. if final shape is (10, 10, 1), adjusts shape to (10, 10)
#' @param ... additional arguments passed to FUN.
#' @return a new tidytensor.
#' @seealso \code{\link{index}}, \code{\link{c.tidytensor}}, \code{\link{permute.tidytensor}}
#' @examples
#' # shape [20, 26, 26]
#' t <- as.tidytensor(array(rnorm(20 * 26 * 26), dim = c(20, 26, 26)))
#' ranknames(t) <- c("sample", "row", "col")
#' print(t)
#'
#' # compute the deviation from median for each sample
#' dev_median <- function(t) {
#'   return(t - median(t))
#' }
#'
#' median_deviations <- tt_apply(t, dev_median, "sample")
#' print(median_deviation)
#'
tt_apply <- function(x, rank = 1, FUN, flatten = FALSE, drop_final_1 = TRUE, ...) {UseMethod("tt_apply", x)}

# method
#' @export
tt_apply.tidytensor <- function(x, rank = 1, FUN, flatten = FALSE, drop_final_1 = TRUE, ...) {
  if(!is.null(ranknames(x))) {
    rank <- tidyselect::vars_select(ranknames(x), !!rlang::enquo(rank))
  }

  index <- rank_to_index(x, rank)
  ## TODO: it seems like we should be able to apply a function over single values, though I'm not sure why one would want to...
  ## maybe I should dispatch to a separate method for that since it's a special case.
  if(index >= length(dim(x))) {
    stop("Bad rank specified, must be a valid rank index or name, and we cannot apply to each entry in the last rank (individual values).")
  }

  # what we'll apply over
  margin <- seq(1, index)
  dim_margin <- dim(x)[margin]

  # save the non_margin dimensions for later use
  dim_non_margin <- dim(x)[seq(index + 1, length(dim(x)))]

  # oooookay, apply serializes its output, and sticks it on front of the array.
  # so applying like (3, 5, 4, 7) -> (3, 5, 10, 10, 10) (last two dimensions turned into a 10x10x10)
  # actually returns shape (1000, 3, 5); to get it back to the proper shape we need to
  # call the function and capture the shape of it's output for later reshaping.
  function_ret_shape <- NULL
  function_ret_ranknames <- NULL

  # we'll wrap the user's function in one that helps out with ranknames etc.
  wrapper_func <- function(subarray) {
    # omg it even strips the names... we only try to set them if they were before, since it won't let us assign NULL to ranknames()
    subarray <- as.tidytensor(subarray)
    if(!is.null(ranknames(x))) {
      ranknames(subarray) <- ranknames(x)[seq(index + 1, length(dim(x)))]
    }
    func_result <- as.tidytensor(FUN(subarray, ...))
    # if it's a vector, the shape is just the length (user-friendliness)
    if(is.null(dim(func_result))) {
      function_ret_shape <<- length(func_result)
    } else {
      function_ret_shape <<- dim(func_result)
    }
    function_ret_ranknames <<- ranknames(func_result)
    return(func_result)
  }

  result <- apply(x, margin, wrapper_func)


  # if the new shape would be e.g. 10, 10, 1, we drop the last dim
  if((length(function_ret_shape) == 1 & function_ret_shape[1] == 1) & drop_final_1) {
      dim(result) <- dim_margin
  } else {
      # ok, if I strip off 1 and 2, from 1,2,3,4, the function will take 3,4 and return e.g. A,B,C, so
      # I want 1,2,A,B,C as the output, as but apply will return (A*B*C),1,2  [Y THO]
      # first we shape, A,B,C,1,2 (1,2 = dim_margin, A,B,C = function_ret_shape)
      dim(result) <- c(function_ret_shape, dim_margin)
  }
  # then we rotate; do we need to do this differently depending on the case above?
  orig_perm <- 1:length(dim(result))
  tail_perm <- tail(orig_perm, n = length(margin))
  new_perm <- c(tail_perm, orig_perm[!orig_perm %in% tail_perm])
  result <- aperm(result, new_perm)



  # if they want collapsed output, collapse it down - must reverse due to R array reshaping
  if(flatten == TRUE) {
    reversed <- aperm(result, rev(1:length(dim(result))))
    # no sense in setting keep.class = TRUE in the above since we need to run it through array() again
    result <- array(reversed, dim = c(prod(dim_margin), function_ret_shape))
  }

  ##### Handle ranknames section
  ## rules: ranknames should be NULL if there's no data, if any rankname is set, then the others should be NA
  ## further: if the user sets a new_rankname, it should be set (possible upgrading the rankrames from NULL to mostly NAs)
  ## otherwise it should be set to what the rankname was at the applied level in the original data (ie the rankname of the index,
  ## if it is set). flatten=true means we collapse ranks 1 to index into a single rank; if it's not set we inherit those
  ## ranks from the input as well.
  # prime the array with some ranknames so that we can ranknames(result)[1] <- assign (otherwise we'd be assigning to [1] of NULL, which is silly)
  # we're going to have *some* ranknames at least

  result <- as.tidytensor(result)
  if(is.null(ranknames(result))) { ranknames(result) <- rep(NA, length(dim(result)))}

  effective_index <- index
  if(flatten == TRUE) { effective_index <- 1 }

  # set the prelim ranknames
  if(flatten != TRUE & !is.null(ranknames(x))) {
    ranknames(result)[1:index] <- ranknames(x)[1:index]
  }

  # set the index rankname; or update it if it should be updated
  if(!is.null(ranknames(x))){
    ranknames(result)[effective_index] <- ranknames(x)[index]
  }

  # set the remaining ranknames as returned by the function
  if(!is.null(function_ret_ranknames)) {
    ranknames(result)[seq(effective_index + 1, length(dim(result)))] <- function_ret_ranknames
  }

  # finally, if there only NAs left, then we can just strip out the annotation altogether
  if(all(is.na(ranknames(result)))) {
    ranknames(result) <- NULL
  }
  return(result)
}
