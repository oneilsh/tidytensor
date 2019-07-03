#' @export
#' @title Print a tensortree.
#'
#' @description Prints a summary of a tensortree as a nested hierarchy of tensors of lower rank.
#'
#' @details The \code{bottom} argument specifies whether the lowest ranks of the tensor should be shown as 2d matrices, 3d matrices, or 1d arrays; \code{"auto"} will
#' select "3d" if the last rank is of size 3 or 1 (assuming an image and a "channels-last" convention), "2d" if the 3rd-to-last rank is length 3 or 1 (assuming an image and a "channels-first" convention) or if there are only two ranks,
#' and otherwise will default to "1d".
#'
#' @param x a tensortree to summarize.
#' @param max_per_level only show this many sub-tensors per level.
#' @param signif_digits number of significant digits to print for numeric tensors.
#' @param end_n limit the base-level prints to include this many dimensions of each rank.
#' @param show_names show the dimension names, if present, or dimension indices if not in base-level prints.
#' @param bottom either "auto", "1d", "2d", or "3d" - specifies whether the inner-most tensors should be represented as rank 1, 2, or 3.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @seealso \code{\link{print.tensortree}}
#' @examples
#' t <- as.tensortree(array(1:(2 * 3 * 4 * 5), dim = c(2, 3, 4, 5)))
#' ranknames(t) <- c("samples", "batches", rows", "cols")
#' print(t, end = "2d")
#'
#' t <- as.tensortree(array(1:(2 * 3 * 40 * 50 * 3), dim = c(2, 3, 40, 50, 3)))
#' ranknames(t) <- c("sample", "batch", "row", "pixel", "channel")
#' print(t, n = c(6, 6, 3), bottom = "3d")
#'
`print.tensortree` <- function(x, max_per_level = 2, signif_digits = 4, end_n = c(6, 6, 3), show_names = FALSE, bottom = "auto", ...) {
    if(bottom == "auto") {
      bottom = "1d"
      if(length(dim(x)) > 2) { # could be 3d
        if(dim(x)[length(dim(x))] %in% c(3, 1)) {
          bottom = "3d"
        }

      }
      if(length(dim(x)) == 2 | (dim(x)[length(dim(x))] == dim(x)[length(dim(x)) - 1]) | (length(dim(x)) > 2 & dim(x)[length(dim(x))-2] %in% c(3, 1))) { # 2d if there's exactly 2 dims, OR if the last two are equal in size (like a square photo...), OR if the third from last is 3 or 1 (channels-first representation)
        bottom = "2d"
      }
    }


    cat("Rank ", length(dim(x)), " tensor, shape: ", sep = "")
    cat("(", paste0(dim(x), collapse = ", "), ")", sep = "")
    if(!is.null(ranknames(x))) {
      cat(", rank names: ", paste0(ranknames(x), collapse = ", "), sep = "")
    }
    cat("\n")

    orig_dims <- dim(x)
    orig_ranks <- ranknames(x)

    # reduce size of top ranks (above 3d)
    if(length(orig_dims) > 3) {
      for(i in 1:(length(orig_dims) - 3)) {
        x <- tt_index(x, 1:min(max_per_level, orig_dims[i]), i, drop = FALSE)
      }
    }


    # reduce size of last 3 ranks
    last_ranks <- (max(length(orig_dims) - 2, 1)):(length(orig_dims))

    end_n_counter <- 1
    for(i in last_ranks) {
      # the +1 is to include one more than asked for, which will then be truncated with ... to indicate more data available
      x <- tt_index(x, 1:(min(end_n[end_n_counter]+1, orig_dims[i])), i, drop = FALSE)
      end_n_counter <- end_n_counter + 1
    }


    if(bottom == "1d") {
      # don't listify if we don't need to; we'll just hit the base case in print_list immediately
      if(length(dim(x)) == 1) {x_list <- x}
      else {x_list <- as.list(x, rank = max(length(orig_dims) - 1, 0))}
    } else if(bottom == "2d") {
      if(length(orig_dims) < 2) {stop("Cannot print rank 1 tensor with bottom = '2d'.")}
      if(length(dim(x)) == 2) {x_list <- x}
      else {
        x_list <- as.list(x, rank = max(length(orig_dims) - 2, 0))
        }
    } else if(bottom == "3d") {
      if(length(orig_dims) < 3) {stop("Cannot print rank 1 or rank 2 tensor with bottom = '3d'.")}
      if(length(dim(x)) == 3) {x_list <- x}
      else {x_list <- as.list(x, rank = max(length(orig_dims) - 3, 0))}
    }
    print_list(x_list, indent = 2, max_per_level = max_per_level, signif_digits = signif_digits, end_n = end_n, show_names = show_names, orig_dims = orig_dims, orig_ranks = orig_ranks)
  }


# internal recursive function
print_list <- function(xl, indent = 0, max_per_level = 2, signif_digits = 4, end_n = c(6, 6, 3), show_names = FALSE, orig_dims, orig_ranks, state = NULL) {
  # if this is the top level, just set the state to a bunch of empties
  if(is.null(state)) {
    state <- rep("", length(orig_dims))
  }

  ident <- paste(rep(" ", indent), collapse = "")
  if(!is.list(xl)) {  # base case, not a nested list
    # which of these is used will be determined by the input shape of the base tensors
    if(length(dim(xl)) == 1) { tt_print_1d(xl, indent = indent, signif_digits = signif_digits, end_n = end_n, show_names = show_names) }
    else if(length(dim(xl) <= 3)) { tt_print_23d(xl, indent = indent, signif_digits = signif_digits, end_n = end_n, show_names = show_names) }
    else{
      stop("Cannot print tensor of rank > 3. Guru meditation error 1001.")
    }

  } else {  # recursive case
    state_to_change <- which(state == "")[1]
    for(i in 1:min(max_per_level, length(xl))) {
      state[state_to_change] <- i       # update carried index state (just for list names)
      index_string <- paste0(c("[", paste(state, collapse = ", "), "]"), collapse = "")

      cat(ident)
      cat("# ")
      if(!is.null(orig_ranks)) {cat(orig_ranks[1])}
      else {cat("tensor")}
      commas = paste(rep("", length(orig_dims)), collapse = ", ")
      #cat(" [", i, commas,  "]", sep = "")'
      cat(" ", index_string, sep = "")
      cat(" shape: (", paste0(orig_dims[(2):length(orig_dims)], collapse = ", "), ")", sep = "")


      cat("\n", sep = "")
      sublist <- xl[[i]]
      print_list(sublist, indent = indent+2, max_per_level = max_per_level, signif_digits = signif_digits, end_n = end_n, show_names = show_names, orig_dims = orig_dims[-1], orig_ranks = orig_ranks[-1], state = state)
    }

    if(orig_dims[1] > max_per_level) {
      cat(ident, paste("... truncating", orig_dims[1] - max_per_level, orig_ranks[1] ,"entries.", collapse = ""), sep = "")
      cat("\n")
    }

  }
}

# Base functions for printing 1, 2 and 3d tensors, not for external use, big mess of code too...
tt_print_23d <- function (mat, indent = 0, signif_digits = 4, end_n = c(6, 6, 3), show_names = FALSE) {
  if( !length(end_n) %in% c(2, 3) | mode(end_n) != "numeric") {
    stop("n parameter must be a length-2 or 3 numeric vector, representing the max number of rows/cols or max number of rows/columns/cell entries to print, it's length should also match the rank of the tensor. n was: ", end_n)
  }

  saved_dimnames <- dimnames(mat)
  # replace missing dimnames or any NAs with proper dimnames
  # if(is.null(saved_dimnames)) {
  #   saved_dimnames <- lapply(dim(mat), function(d) {return(1:d)})
  # }
  #
  # for(i in 1:length(saved_dimnames)) {
  #   dimnames_i <- saved_dimnames[[i]]
  #   dimnames_i[is.na(dimnames_i)] <- seq(1, length(dimnames_i))[is.na(dimnames_i)]
  #   saved_dimnames[[i]] <- dimnames_i
  # }

  saved_ranknames <- ranknames(mat)
  if(is.numeric(mat)) {
    mat <- signif(mat, signif_digits)
  }

  mat2 <- tt_apply(mat, length(dim(mat)) - 1, as.character, drop_final_1 = FALSE) # make it a character type, but don't deconstruct to a vector
  if(is.character(mat)) { # if the original was a character
    #mat2 <- t(apply(mat, 1, function(vec) { return(paste0("'", vec, "'"))}))
    mat2 <- tt_apply(mat, length(dim(mat)) - 1, function(el) {return(paste0("'", el, "'"))}, drop_final_1 = FALSE)
  }


  is3d <- FALSE
  if(length(dim(mat)) == 3) {
    mat2 <- tt_apply(mat2, 2, function(channels) {
      channels <- as.character(channels)
      if(length(channels) > end_n[length(end_n)]) {
        channels <- c(channels[1:end_n[length(end_n)]], "...") # BEWARE: tt_apply always gets an array (tensortree), even if 1d, so c() here is using c.tensortree, NOT c.character, unless we explictly cast it as a character array. ugh.
      }
      res1 <- paste0("[", paste(channels, collapse = ", "), "]")
      res <- as.tensortree(res1)
      return(res)
    })
    is3d <- TRUE
  }



  #if(is3d) { cat(paste0("Rank 3 tensor (", paste(dim(mat2), collapse = ", "), ")"))}
  #else { cat(paste0("Rank 2 tensor (", paste(dim(mat2), collapse = ", "), ")"))}

  needs_newline = FALSE

  if(!is.null(saved_ranknames)) {
    #res <- paste('"', saved_ranknames, '"', sep = "")
    #cat(paste0("  Rank names: ",
    #           paste(res, collapse = ", ")
    #))
    cat(paste0(rep(" ", indent)), sep = "")
    cat("#", paste(saved_ranknames, collapse = ", "))
    needs_newline = TRUE
  }


  mat <- mat2
  prepend_colnames <- c()
  set_rownames <- FALSE
  set_colnames <- FALSE


  if(show_names == TRUE) {


    if(!is.null(saved_dimnames)) {
      if(length(saved_dimnames) >= 1) {
        if(any(!is.na(saved_dimnames[[1]]))) {
          #saved_dimnames[[1]][is.na(saved_dimnames[[1]])] <- "NA"
          saved_dimnames[[1]][!is.na(saved_dimnames[[1]])] <- paste('"', saved_dimnames[[1]][!is.na(saved_dimnames[[1]])], '"', sep = "")
          saved_dimnames[[1]][is.na(saved_dimnames[[1]])] <- (1:length(saved_dimnames[[1]]))[is.na(saved_dimnames[[1]])]
          if(is3d) {saved_dimnames[[1]] <- paste("[", saved_dimnames[[1]], ",,]", sep = "")}
          else {saved_dimnames[[1]] <- paste("[", saved_dimnames[[1]], ",]", sep = "")}
          mat <- cbind(saved_dimnames[[1]], mat)
          prepend_colnames <- ""
          set_rownames <- TRUE
        }
      }

      if(length(saved_dimnames) >= 2) {
        if(any(!is.na(saved_dimnames[[2]]))) {
          # we need to create an empty corner entry IF we did a cbind above
          #saved_dimnames[[2]][is.na(saved_dimnames[[2]])] <- "NA"
          saved_dimnames[[2]][!is.na(saved_dimnames[[2]])] <- paste('"', saved_dimnames[[2]][!is.na(saved_dimnames[[2]])], '"', sep = "")
          saved_dimnames[[2]][is.na(saved_dimnames[[2]])] <- (1:length(saved_dimnames[[2]]))[is.na(saved_dimnames[[2]])]
          if(is3d) {saved_dimnames[[2]] <- paste("[,", saved_dimnames[[2]], ",]", sep = "")}
          else {saved_dimnames[[2]] <- paste("[,", saved_dimnames[[2]], "]", sep = "")}
          mat <- rbind(c(prepend_colnames, saved_dimnames[[2]]), mat)
          set_colnames <- TRUE
        }
      }
      if(length(saved_dimnames) >= 3) {
        if(any(!is.na(saved_dimnames[[3]]))) {
          res <- paste('"', saved_dimnames[[3]], '"', sep = "")
          res[is.na(saved_dimnames[[3]])] <- NA
          cat(paste0('  ("', saved_ranknames[3] ,'" Dimension names: [',
                     paste(res, collapse = ", "), "])"
          ))
          needs_newline = TRUE
        }
      }
    }


    if(!set_rownames & !set_colnames) {
      row_names <- paste("[", 1:nrow(mat), ",]", sep = "")
      col_names <- paste("[,", 1:(ncol(mat)), "]", sep = "")
      if(is3d) {
        row_names <- paste("[", 1:nrow(mat), ",,]", sep = "")
        col_names <- paste("[,", 1:(ncol(mat)), ",]", sep = "")
      }

      mat <- cbind(row_names, mat)
      mat <- rbind(c("", col_names), mat) # this is

    } else if(!set_rownames) {
      if(is3d) {row_names <- paste("[", 0:nrow(mat), ",,]", sep = "")}
      else {row_names <- paste("[", 0:nrow(mat), ",]", sep = "")}
      mat <- cbind(row_names, mat)
      mat[1, 1] <- ""
    } else if(!set_colnames) {
      if(is3d) {col_names <- paste("[,", 1:ncol(mat)-1, ",]", sep = "")}
      else {col_names <- paste("[,", 1:ncol(mat)-1, "]", sep = "")}
      mat <- rbind(col_names, mat)
      mat[1, 1] <- ""
    }
  }


  if(needs_newline) {cat("\n")}


  mat[is.na(mat)] <- "NA"

  adder <- 0
  if(show_names) {adder <- 1}
  if(nrow(mat) > end_n[2] + adder) {
    mat <- mat[1:(end_n[2] + adder), ,drop = FALSE]
    mat <- rbind(mat, rep("... ", ncol(mat)))
  }


  adder <- 0
  if(show_names) {adder <- 1}
  if(ncol(mat) > end_n[3] + adder) {
    mat <- mat[, 1:(end_n[3] + adder), drop = FALSE]
    mat <- cbind(mat, rep("... ", nrow(mat)))
  }



  wd <- max(nchar(mat)) + 1
  sp <- wd - nchar(mat)
  sp[,1] <- max(nchar(mat[,1])) + 1 - nchar(mat[,1]) # seperate indenting for the first col, we don't need to space according to the rest of the data
  sp[,ncol(mat)] <- max(nchar(mat[,ncol(mat),drop=FALSE])) + 1 - nchar(mat[,ncol(mat),drop=FALSE]) # seperate indenting for the first col, we don't need to space according to the rest of the data
  build <- rstackdeque::rstack()
  for (i in 1:nrow(mat)) {
    build <- rstackdeque::insert_top(build, paste0(rep(" ", indent), collapse = "")) # why even the -2? there's an extra two coming here somehow
    for (j in 1:ncol(mat)) {
      build <- rstackdeque::insert_top(build, paste0(rep(" ", sp[i, j]+1), collapse = ""))
      build <- rstackdeque::insert_top(build, paste0(mat[i, j], collapse = ""))
    }
    build <- rstackdeque::insert_top(build, "\n")
  }

  #cat("\n")
  cat(paste0(rev(unlist(as.list(build))), collapse = ""))
}


tt_print_1d <- function(mat, indent = 0, signif_digits = 4, end_n = 6, show_names = FALSE) {
  saved_ranknames <- ranknames(mat)
  saved_dimnames <- dimnames(mat)

  cat(paste0(rep(" ", indent)), sep = "")
  #cat(paste0("Rank 1 tensor (", length(mat) ,")"))
  if(!is.null(ranknames(mat))) {
    cat("#", ranknames(mat))
    cat("\n")
  }
  #if(!is.null(ranknames(mat))) {
  #  cat(paste0('  Rank name: "', ranknames(mat), '"'))
  #}
  #if(!is.null(dimnames(mat)) & show_names) {
  #  dims <- paste0('"', dimnames(mat)[[1]], '"')
  #  cat(paste0('  (Dimension names: ', paste(dims, collapse = ", "), ')'))
  #}


  if(is.numeric(mat)) {
    mat <- as.tensortree(signif(mat, signif_digits))
  } else if(is.character(mat)) {
    mat <- as.tensortree(paste0('"', mat, '"'))
  }

  ranknames(mat) <- saved_ranknames
  dimnames(mat) <- saved_dimnames

  cat(paste0(rep(" ", indent + 2)), sep = "")
  missing <- 0
  if(length(mat) > end_n[1]) {
    missing <- length(mat) - end_n[1]
    mat <- mat[1:end_n[1]]
  }
  cat(mat)
  if(missing > 0) {
    #cat(paste0(" (+", missing, " more)"))
    cat(" ...")
  }
  cat("\n")
}
