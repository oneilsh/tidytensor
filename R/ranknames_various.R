###################################
##  ranknames(x) <-
###################################

#' @export
#' @title Assign ranknames to a tensortree.
#'
#' @description A tensor tree t may have ranknames(t); this is a character vector of the same length as dim(t)
#' for future use. Note that ranknames(t) is independent of names(t) or dimnames(t); we are not naming elements,
#' or the dimension names for each rank, but rank names themselves.
#' Like names() and dimnames(), unset ranknames() are NULL.
#'
#' @details Ranknames for a tensortree t are stored as the names() attribute of dimnames(t). If dimnames(t) happens
#' to be null, before setting ranknames() we create valid dimnames() filled with NA values. The tensortree
#' package also provides a specialized dimnames() which preserves ranknames when setting dimnames().
#'
#' @param x input tensortree to set ranknames on.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @param value what to store in ranknames(x).
#' @seealso \code{\link{set_ranknames}}, \code{\link{dimnames<-}}
#' @examples
#' t <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' ranknames(t) <- c("sample", "row", "col")
#' summary(t)
#'
#' # If given a single rankname, will repeat the rankname for all ranks
#' t <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' ranknames(t) <- "generic"
#' summary(t)
#'
#' # works like names():
#' t <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' ranknames(t) <- c("sample", "row", "col")
#' print(ranknames(t))
#' ranknames(t)[3] <- "pixel"
#' summary(t)
`ranknames<-` <- function(x, value, ...) {UseMethod("ranknames<-", x)}


# method:
#' @export
`ranknames<-.tensortree` <- function(x, value, ...) {
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
    stop(paste0("Mismatched rank. Dimensions of tensor: ",
                paste0(dim(x), collapse = ", "),
                "; attempted names: ",
                paste0(value, collapse = ", "),
                "\n\n"
    )
    )
  }

  names(attr(x, "dimnames")) <- value
  return(x)
}


###################################
##  print(ranknames(x))
###################################


#' @export
#' @title Get ranknames of a tensortree.
#'
#' @description A tensor tree t may have ranknames(t); this is a character vector of the same length as dim(t)
#' for future use. Note that ranknames(t) is independent of names(t) or dimnames(t); we are not naming elements,
#' or the dimension names for each rank, but rank names themselves.
#' Like names() and dimnames(), unset ranknames() are NULL.
#'
#' @details Ranknames for a tensortree t are stored as the names() attribute of dimnames(t). If dimnames(t) happens
#' to be null, before setting ranknames() we create valid dimnames() filled with NA values. The tensortree
#' package also provides a specialized dimnames() which preserves ranknames when setting dimnames().
#'
#' @param x input tensortree to get ranknames for.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return character vector of the same length as dim(x), or NULL if unset.
#' @seealso \code{\link{set_ranknames}}, \code{\link{ranknames<-}}
#' @examples
#' t <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' ranknames(t) <- c("sample", "row", "col")
#' print(ranknames(t))
#'
`ranknames` <- function(x, ...) {UseMethod("ranknames", x)}
setGeneric("ranknames")

# method:
#' @export
`ranknames.tensortree` <- function(x, ...) {
    if(is.null(attr(x, "dimnames"))) {
      return(NULL)
    }
    return(names(attr(x, "dimnames")))
}

###########################################
## set_ranknames(t, c("one", "two", "three"))
###########################################
#' @export
#' @title Assign ranknames to a tensortree via a standard function call.
#'
#' @description A tensor tree t may have ranknames(t); this is a character vector of the same length as dim(t)
#' for future use. Note that ranknames(t) is independent of names(t) or dimnames(t); we are not naming elements,
#' or the dimension names for each rank, but rank names themselves.
#' Like names() and dimnames(), unset ranknames() are NULL.
#'
#' @details Ranknames for a tensortree t are stored as the names() attribute of dimnames(t). If dimnames(t) happens
#' to be null, before setting ranknames() we create valid dimnames() filled with NA values. The tensortree
#' package also provides a specialized dimnames() which preserves ranknames when setting dimnames().
#'
#' @param x input tensortree to set ranknames on.
#' @param newnames character vector of new ranknames to assign.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a tensortree with ranknames set.
#' @seealso \code{\link{ranknames<-}}
#' @examples
#' t <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' t <- set_ranknames(t, c("sample", "row", "col"))
#' summary(t)
#'
#' # If given a single rankname, will repeat the rankname for all ranks
#' t <- as.tensortree(array(1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' t <- set_ranknames(t, "generic")
#' summary(t)
#'
`set_ranknames` <- function(x, newnames, ...) {UseMethod("set_ranknames", x)}

# method
#' @export
`set_ranknames.tensortree` <- function(x, newnames, ...) {
  ranknames(x) <- newnames
  return(x)
}


##############################################
### dimnames(t) <-
##############################################
# just a method, the generic already exists in base R
#' @export
`dimnames<-.tensortree` <- function(x, value, ...) {
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
#' @description Since tensortrees are arrays, they support dimnames(). The usuall syntax dimnames(x) <- works;
#' this function provides a Magritte-compatible regular function, set_dimnames(x, newnames) which returns a new tensortree.
#'
#' @details Setting dimnames with set_dimnames() preserves any ranknames present.
#'
#' @param x input tensortree to set ranknames on.
#' @param newnames list of dimnames to assign.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a tensortree with dimnames set.
#' @seealso \code{\link{ranknames<-}}, \code{\link{dimnames}}
#' @examples
#' t <- as.tensortree(array(1:(3 * 2), dim = c(3, 2)))
#' t <- set_dimnames(t, list(c("sample1", "sample2", "sample3"), c("valset1", "valset2"))
#' summary(t)
#'
#' # We can also assign ranknames:
#' ranknames(t) <- c("sample", "valset")
#' summary(t)
#'
`set_dimnames` <- function(x, newnames, ...) {UseMethod("set_dimnames", x)}

# method
#' @export
`set_dimnames.tensortree` <- function(x, newnames, ...) {
  dimnames(x) <- newnames
  return(x)
}
