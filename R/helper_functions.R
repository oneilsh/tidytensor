# this is all internally used stuff

# e.g. quovars(a, b, "c", d) returns c("a", "b", "c", "d")
# and quovars(.dots = c("a", "b", "c", "d")) returns same
quovars <- function(..., .dots = NULL) {
  # get dots
  if(!is.null(.dots)) {
    return(.dots)
  }
  .dots <- rlang::enexprs(...)
  lang <- unlist(lapply(rlang::enexprs(...), is.language))
  labels <- purrr::map_at(.dots, which(lang), as.character)
  labels <- unlist(labels)
  names(labels) <- NULL
  return(labels)
}


rank_to_index <- function(tensor, by = TRUE) {UseMethod("rank_to_index", tensor)}


# used to convert a rank selector (numeric, character (by rankname), or logical)
# to an index vector (numeric)
rank_to_index.tidytensor <- function(tensor, by = TRUE) {
  permute_vec <- seq(1:length(dim(tensor))) # no change by default
  if(mode(by) == "logical") {
    logical_full <- rep(by, length.out = length(dim(tensor)))
    permute_vec <- which(logical_full)
  } else if(mode(by) == "numeric") {
    permute_vec <- by

    # there's got to be an easier way to do this case in R; all I want to do is turn
    # c("nameone", "nametwo", "namethree") and c("namethree", "nameone") into c(3, 1)
  } else if(mode(by) == "character") {
    if(is.null(ranknames(tensor))) {
      stop("Cannot select ranks by name for unnamed tensor.")
    }
    selector <- rep(NA, length(dim(tensor)))
    for(i in seq(1, length(by))) {
      entry <- by[i]
      selector[i] <- which(ranknames(tensor) == entry)
    }
    permute_vec <- selector
    permute_vec <- permute_vec[!is.na(permute_vec)]
  }
  return(permute_vec)
}


tt_index <- function(tensor, indices, dimension = 1, drop = TRUE) {UseMethod("tt_index", tensor)}


# just subsets a tensor *without* knowing its rank
# eg suppose we want some_tensor[ , ,1:10 , , ], which is a rank-5 tensor, but we don't know that to begin with,
# we can use tt_index(some_tensor, 1:10, dimension = 3)
tt_index.tidytensor <- function(tensor, indices, dimension = 1, drop = TRUE) {
  # here's where it gets tricky: we need to grab the first couple of entries
  # from the first dimenions of the tensor. E.g. if dim(tensor) is c(10, 10, 10, 10), we
  # want tensor[1:n, , , ]. But we can't write it that way because we don't know the rank to hard-code it.
  # The acorn() function (from the abind() package) can help with this, but it also
  # needs to take arguments like acorn(tensor, 2, 10, 10, 10). Fortunately we can use do.call()
  # to call a function with parameters generated from a list.
  args_list <- list();

  # get the dimensions, build the argument list from the tensor and the dimension sizes
  tensor_dims <- dim(tensor)
  args_list[[1]] <- tensor
  for(i in 1:length(tensor_dims)) {
    if(tensor_dims[i] == 0) {
      args_list[[i+1]] <- 0
    } else {
      # yup, this is O(rank^2)
      args_list[[i+1]] <- seq(1,tensor_dims[i])
    }
  }

  # we only want the selected entries in the nth dimension (first is at index 2 of the param list)
  args_list[[dimension + 1]] <- indices
  args_list[[length(args_list) + 1]] <- drop
  names(args_list)[length(args_list)] <- "drop"
  # make the call
  res <- do.call("[", args_list)
  return(as.tidytensor(res))
}



#' @export
`[.tidytensor` <- function(x, ...) {
  x <- unclass(x)
  result <- x[...]
  return(tt(result))
}

#' @export
`[<-.tidytensor` <- function(x, ..., value) {
  x <- unclass(x)
  x[...] <- value
  return(tt(x))
}



