###################################
##  ranknames(x) <-
###################################

#' @export
#' @title Assign ranknames to a tidytensor.
#'
#' @description A tidytensor t may have ranknames(t); this is a character vector of the same length as dim(t)
#' for future use. Note that ranknames(t) is independent of names(t) or dimnames(t); we are not naming elements,
#' or the dimension names for each rank, but rank names themselves.
#' Like names() and dimnames(), unset ranknames() are NULL.
#'
#' @details Ranknames for a tidytensor t are stored as the names() attribute of dimnames(t). If dimnames(t) happens
#' to be null, before setting ranknames() we create valid dimnames() filled with NA values. The tidytensor
#' package also provides a specialized dimnames() which preserves ranknames when setting dimnames().
#'
#' @param x input tidytensor to set ranknames on.
#' @param value what to store in ranknames(x).
#' @seealso \code{\link{set_ranknames}}, \code{\link{dimnames<-}}
#' @examples
#' t <- as.tidytensor(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' ranknames(t) <- c("sample", "row", "col")
#' print(t)
#'
#' # works like names():
#' t <- as.tidytensor(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' ranknames(t) <- c("sample", "row", "col")
#' print(ranknames(t))
#' ranknames(t)[3] <- "pixel"
#' print(t)
`ranknames<-` <- function(x, value) {UseMethod("ranknames<-", x)}


# method:
#' @export
`ranknames<-.tidytensor` <- function(x, value) {
  # we're going to work with the names() of the dimnames(), so we need to have some dimnames there.
  # this just sets them all to NA if there aren't any already
  if(is.null(attr(x, "dimnames"))) {
    attr(x, "dimnames") <- lapply(dim(x), function(dim) {rep(NA, dim)})
  }

  if(is.null(value)) {
    names(attr(x, "dimnames")) <- NULL
    return(x)
  }


  # otherwise, if the number of names and number of ranks don't match, then no go
  if(any(length(value) != length(dim(x)))) {
    stop(paste0("Mismatched rank. Shape of tensor: ",
                paste0("(", paste0(dim(x), collapse = ", "), ")"),
                "; attempted names: ",
                paste0(value, collapse = ", "),
                "\n\n"))
  }
  # no duplicate ranknames allowed, cmon
  nonas <- value[!is.na(value)]
  if(any(duplicated(nonas))) {
    stop("duplicate ranknames not allowed.")
  }

  names(attr(x, "dimnames")) <- value
  return(x)
}


###################################
##  print(ranknames(x))
###################################

#' @export
#' @title Get ranknames of a tidytensor.
#'
#' @description A tidytensor t may have ranknames(t); this is a character vector of the same length as dim(t)
#' for future use. Note that ranknames(t) is independent of names(t) or dimnames(t); we are not naming elements,
#' or the dimension names for each rank, but rank names themselves.
#' Like names() and dimnames(), unset ranknames() are NULL.
#'
#' @details Ranknames for a tidytensor t are stored as the names() attribute of dimnames(t). If dimnames(t) happens
#' to be null, before setting ranknames() we create valid dimnames() filled with NA values. The tidytensor
#' package also provides a specialized dimnames() which preserves ranknames when setting dimnames().
#'
#' @param x input tidytensor to get ranknames for.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return character vector of the same length as dim(x), or NULL if unset.
#' @seealso \code{\link{set_ranknames}}, \code{\link{ranknames<-}}
#' @examples
#' t <- as.tidytensor(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' ranknames(t) <- c("sample", "row", "col")
#' print(ranknames(t))
`ranknames` <- function(x, ...) {
  UseMethod("ranknames", x)
}
#setGeneric("ranknames")



# method:
#' @export
`ranknames.tidytensor` <- function(x, ...) {
    if(is.null(attr(x, "dimnames"))) {
      return(NULL)
    }
    return(names(attr(x, "dimnames")))
}

###########################################
## set_ranknames(t, c("one", "two", "three"))
###########################################
#' @export
#' @title Assign ranknames to a tidytensor via a standard function call.
#'
#' @description A tidytensor t may have ranknames(t); this is a character vector of the same length as dim(t)
#' for future use. Note that ranknames(t) is independent of names(t) or dimnames(t); we are not naming elements,
#' or the dimension names for each rank, but rank names themselves.
#' Like names() and dimnames(), unset ranknames() are NULL.
#'
#' @details Ranknames for a tidytensor t are stored as the names() attribute of dimnames(t). If dimnames(t) happens
#' to be null, before setting ranknames() we create valid dimnames() filled with NA values. The tidytensor
#' package also provides a specialized dimnames() which preserves ranknames when setting dimnames().
#'
#' @param x input tidytensor to set ranknames on.
#' @param ... new ranknames to assign (quoted or unquoted).
#' @param .dots character vector of new ranknames to assign.
#' @return a tidytensor with ranknames set.
#' @seealso \code{\link{ranknames<-}}
#' @examples
#' t <- as.tidytensor(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' t <- set_ranknames(t, sample, row, col)
#' t <- set_ranknames(t, .dots = c("sample", "row", "col"))
#' print(t)
`set_ranknames` <- function(x, ..., .dots = NULL) {
  UseMethod("set_ranknames", x)
}

# method
#' @export
`set_ranknames.tidytensor` <- function(x, ...) {
  ranknames(x) <- quovars(...)
  return(x)
}


##############################################
### dimnames(t) <-
##############################################
# just a method, the generic already exists in base R
#' @export
`dimnames<-.tidytensor` <- function(x, value) {
  # setting dimnames() directly erases previous ranknames, because those were stored as names(dimnames())
  # save and restore strategy...

  temp_ranknames <- ranknames(x)
  attr(x, "dimnames") <- value
  ranknames(x) <- temp_ranknames
  return(x)
}


###########################################
## set_dimnames(t, list(c("a", "b"), c("l", "m", "n"), c("x", "y"))
###########################################

#' @export
#' @title Set dimnames() via a standard function call.
#'
#' @description Since tidytensors are arrays, they support dimnames(). The usuall syntax dimnames(x) <- works;
#' this function provides a Magritte-compatible regular function, set_dimnames(x, newnames) which returns a new tidytensor.
#'
#' @details Setting dimnames with set_dimnames() preserves any ranknames present.
#'
#' @param x input tidytensor to set dimnames on.
#' @param newnames list of dimnames to assign.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a tidytensor with dimnames set.
#' @seealso \code{\link{ranknames<-}}, \code{\link{dimnames}}
#' @examples
#' t <- as.tidytensor(array(1:(3 * 2), dim = c(3, 2)))
#' t <- set_dimnames(t, list(c("sample1", "sample2", "sample3"), c("valset1", "valset2")))
#' print(t)
#'
#' # We can also assign ranknames:
#' ranknames(t) <- c("sample", "valset")
#' print(t)
#'
`set_dimnames` <- function(x, newnames, ...) {
  UseMethod("set_dimnames", x)
}

# method
#' @export
`set_dimnames.tidytensor` <- function(x, newnames, ...) {
  dimnames(x) <- newnames
  return(x)
}




#' @export
#' @title Set dimnames() via a standard function call, for a particular rank.
#'
#' @description Sets the dimensions names for a particular rank, without requiring dimnames for the other ranks.
#'
#' @details If all dimnames are unset, they will be set to NA for the other ranks, otherwise they will be left alone.
#'
#' @param x input tidytensor to set dimnames on.
#' @param rank rank to set the dimnames on.
#' @param ... dimnames to assign (quoted or unquoted).
#' @param .dots character vector of dimnames to assign (quoted or unquoted).
#' @return a tidytensor with dimnames set.
#' @seealso \code{\link{ranknames<-}}, \code{\link{dimnames}}, \code{\link{set_dimnames}}
#' @examples
#' t <- as.tidytensor(array(1:(3 * 2), dim = c(3, 2)))
#' t <- set_dimnames_for_rank(t, 2, valset1, valset2)
#' t <- set_dimnames_for_rank(t, 2, .dots = c("valset1", "valset2"))
#' print(t)
`set_dimnames_for_rank` <- function(x, rank, ..., .dots = NULL) {
  UseMethod("set_dimnames_for_rank", x)
}

# method
#' @export
`set_dimnames_for_rank.tidytensor` <- function(x, rank, ...) {
  if(!is.null(ranknames(x))) {
    rank <- tidyselect::vars_select(ranknames(x), !!rlang::enquo(rank))
  }
  newnames <- quovars(...)
  rank_index <- rank_to_index(x, rank)
  if(is.null(dimnames(x))) {
    # fill with NAs
    dimnames(x) <- lapply(dim(x), function(count) {return(rep(NA, count))} )
    dimnames(x)[[rank_index]] <- newnames
  } else {
    dimnames(x)[[rank_index]] <- newnames
  }

  return(x)
}




