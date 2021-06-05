#' @export
#' @title Combine multiple ranks of a tensor into a single rank
#'
#' @description Combine multiple ranks of a tensor into a single rank, for example for use in data augmentation.
#'
#' @details If all ranks being combined have dimension names, the dimension names of the newly produced rank will be combinations of those specified.
#'
#' It is only possible to combine consecutive ranks; use \code{permute()} to first organize ranks.
#'
#' @param x the tidytensor to combine ranks for.
#' @param ... ranknames or integers to combine (quoted or unquoted).
#' @param .dots character or integer vector of ranknames.
#' @return a new tidytensor.
#' @seealso \code{\link{permute}}, \code{\link{bind.tidytensor}}
#' @examples
#' # shape [5, 20, 26, 26] for 5 batches of 20 26x26 "images"
#' t <- as.tidytensor(array(rnorm(5 * 20 * 26 * 26), dim = c(5, 20, 26, 26)))
#' ranknames(t) <- c("batch", "image", "row", "col")
#'
#' # given an image tidytensor (26x26), return a set of replicates with noise added
#' make_noisy_images <- function(t2) {
#'   res <- bind(t2,
#'               t2 + rnorm(length(t2)),
#'               t2 + rnorm(length(t2)),
#'               t2 + rnorm(length(t2)), new_rank_name = "replicate")
#' }
#'
#' #augment the original data by replacing each image with a set of noisy replicates
#' t <- tt_apply(t, image, make_noisy_images)
#' # now t is shape (5, 20, 4, 26, 26) with ranknames (batch, image, replicate, row, col)
#' # let's set some dimension names
#' t <- set_dimnames_for_rank(t, image, .dots = 1:20)  # "1", "2", "3", ...
#' t <- set_dimnames_for_rank(t, replicate, original, rep1, rep2, rep3) # "original", "rep1", "rep2", "rep3"

#'
#' # to make it compatible with the original shape we combine images and replicates
#' t2 <- combine_ranks(t, image, replicate)
#'
#' print(t2)
#'
#' # since the combined ranks both have demension names, the newly created rank does as well and we can verify contents
#' # here we see that the second batch, image 3, replicate 2 is indeed the same
#' print(t[2, "3", "rep2", , ])
#' print(t2[2, "3_rep2", , ])
combine_ranks <- function(tensor, ...) {UseMethod("combine_ranks", tensor)}

#' @export
combine_ranks.tidytensor <- function(x, ..., new_rank_name = NULL) {
  vars <- quovars(...)
  drop_indices <- rank_to_index(x, vars)

  # too-clever way to check if drop_indices contains a set of sequential integers
  dropping_sequential <- all.equal(drop_indices, (min(drop_indices)):(max(drop_indices))) == TRUE
  if(!dropping_sequential) {stop("Can only combine sequential ranks, use permute() to put ranks to combine next to each other if necessary and specify them in their existing order.")}

  all_indices <- 1:length(dim(x))
  pre_indices <- all_indices[all_indices < min(drop_indices)]
  post_indices <- all_indices[all_indices > max(drop_indices)]

  old_dim <- dim(x)
  new_dim <- c(old_dim[pre_indices], prod(old_dim[drop_indices]), old_dim[post_indices])

  old_names <- dimnames(x)
  old_ranknames <- ranknames(x)

  dim(x) <- new_dim

  if(!is.null(old_names)) {
    new_names <- old_names[pre_indices]
    # we'll start by using NA for the dimnames for the new rank
    new_single_rankname <- paste(old_ranknames[drop_indices], collapse = "_")
    new_names[[new_single_rankname]] <- rep(NA, prod(old_dim[drop_indices]))
    # but if all of the old names contain non-NA values... then we'll collect up combinations
    if(all(!is.na(unlist(old_names[drop_indices])))) {
      new_dimnames <- expand.grid(old_names[drop_indices])
      # crazy R hacks, mwhahaha
      undpaste <- function(...) {paste(..., sep = "_")}
      new_dimnames <- do.call("undpaste", new_dimnames)
      new_names[[new_single_rankname]] <- new_dimnames
    }
    new_names <- c(new_names, old_names[post_indices])
    dimnames(x) <- new_names
    ranknames(x) <- names(new_names)
  }

  if(!is.null(new_rank_name)) {
    ranknames(x)[min(drop_indices)] <- new_rank_name
  }

  return(x)
}
