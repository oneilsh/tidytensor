#' @export
#' @title Convert a tidytensor into a nested list of tensors.
#'
#' @description Convert a tidytensor into a nested list of tensors, nested down to level specified in \code{rank}.
#' If \code{flatten = TRUE}, returns a flattens the structure to a list of tensors (not nested).
#'
#' @details The \code{state} parameter is for internal use, and needn't be set during normal usage.
#'
#' @param x the tidytensor to convert.
#' @param rank an indicator of the rank defining the contained tensors.
#' @param flatten whether to return a nested list (\code{FALSE}) or a flattened list (\code{TRUE}).
#' @param state an internally used parameter for tracking build state-do not set manually.
#' @param ... additional arguments passed to methods (unusued).
#' @return a list.
#' @seealso \code{\link{as.data.frame.tidytensor}}
#' @rdname as.list.tidytensor
#' @examples
#' # Three tidytensors of the same shape
#' t1 <- as.tidytensor(array(100 * 1:(3 * 4 * 5), dim = c(3, 4, 5)))
#' ranknames(t1) <- c("sample", "row", "col")
#' l1 <- as.list(t1)
#' str(l1)
`as.list.tidytensor` <- function(x, rank = 1, flatten = TRUE, state = NULL, ...) {
  if(!is.null(ranknames(x))) {
    rank <- tidyselect::vars_select(ranknames(x), !!rlang::enquo(rank))
  }

  index = rank_to_index(x, rank)
  if(length(index) != 1) {
    stop("Error in as.list.tidytensor: rank = can only specify a single valid rank.")
  }

  # if this is the top level, just set the state to a bunch of empties
  if(rank == 0) {
    return(list(x))
  }
  if(is.null(state)) {
    state <- rep("", length(dim(x)))
  }


  ret_list <- as.list(1:dim(x)[1]) # create a list of the right size

  # note which index in the state we're updating (just for list names)
  state_to_change <- which(state == "")[1]
  # grab each subtensor along the first dimension;
  for(i in 1:dim(x)[1]) {
    state[state_to_change] <- i       # update carried index state (just for list names)

    subtensor <- tt_index(x, i, dimension = 1)
    if(length(dim(subtensor)) == 1) { # tt_index will drop the last rank name if it's only a simple vector (array -> vector dimension reduction); rather than use drop = FALSE (which messes up the recursion), we just check for this case
      ranknames(subtensor) <- ranknames(x)[length(ranknames(x))]
    }

    if(index == 1) {
      ret_list[[i]] <- subtensor #list(subtensor)
    } else {
      sublist <- as.list(subtensor, index - 1, flatten, state)
      ret_list[[i]] <- sublist
    }

    # set names
    index_string <- paste0(c("[", paste(state, collapse = ", "), "]"), collapse = "")
    shape <- rep(" ", length(dim(x)))
    shape_string <- paste0(c("(", paste(dim(subtensor), collapse = ", "), ")"), collapse = "")
    attr(ret_list[[i]], "index_string") <- index_string
    attr(ret_list[[i]], "shape_string") <- shape_string
    #names(ret_list)[i] <- paste0(c("index: ", index_string, " shape: ", shape_string), collapse = "")
    names(ret_list)[i] <- paste0(index_string, ", shape ", shape_string, collapse = "")
    # add rankname, if present
    if(!is.null(ranknames(x))) {
      ranki <- ranknames(x)[1]
      names(ret_list)[i] <- paste0('', ranki, ' ', names(ret_list)[i], collapse = "")
      #names(ret_list)[i] <- ranki
    } else {
      names(ret_list)[i] <- index_string
    }
  }

  # flatten if needed
  # (but, only if this isn't the top level)
  if(flatten & index != 1) {return(purrr::flatten(ret_list))}

  return(ret_list)
}
