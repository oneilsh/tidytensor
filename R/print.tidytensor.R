#' @export
#' @title Print a tidytensor.
#'
#' @description Prints a summary of a tidytensor as a nested hierarchy of tensors of lower rank.
#'
#' @details The \code{bottom} argument specifies whether the lowest ranks of the tensor should be shown as 2d matrices, 3d matrices, or 1d arrays; \code{"auto"} will
#' select "3d" if the last rank is of size 3 or 1 (assuming an image and a "channels-last" convention), "2d" if the 3rd-to-last rank is length 3 or 1 (assuming an image
#' and a "channels-first" convention) or if there are only two ranks or if the last two ranks are equal, and otherwise will default to "1d".
#'
#' \code{max_per_level} indicates how many replicates
#'
#' @param x a tidytensor to summarize.
#' @param show_names show the dimension names, if present, or dimension indices if not in base-level prints.
#' @param max_per_level only show this many sub-tensors per level.
#' @param bottom either "auto", "1d", "2d", or "3d" - specifies whether the inner-most tensors should be represented as rank 1, 2, or 3.
#' @param max_rows limit the base-level prints to include this many rows (also applies to 1d prints).
#' @param max_cols limit the base-level prints to include this many columns.
#' @param max_depth in 3d representation, limit the base-level prints to include this many entries of the last rank.
#' @param signif_digits number of significant digits to print for numeric tensors.
#' @param indent indent the printout by this much (used internally).
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @seealso \code{\link{print.tidytensor}}
#' @examples
#' t <- as.tidytensor(array(1:(2 * 3 * 4 * 5), dim = c(2, 3, 4, 5)))
#' ranknames(t) <- c("samples", "batches", "rows", "cols")
#' print(t, bottom = "2d")
#'
#' t <- as.tidytensor(array(1:(2 * 3 * 40 * 50 * 3), dim = c(2, 3, 40, 50, 3)))
#' ranknames(t) <- c("sample", "batch", "row", "pixel", "channel")
#' print(t, max_rows = 6, max_cols = 6, max_depth = 3, show_names = TRUE, bottom = "3d")
#'
`print.tidytensor` <- function(x,  show_names = FALSE, max_per_level = 1, bottom = "auto", max_rows = 6, max_cols = 6, max_depth = 3, signif_digits = 3, indent = 0, ...) {
  if(bottom == "auto") {
    bottom = "1d"
    if(length(dim(x)) > 2) { # could be 3d
      if(dim(x)[length(dim(x))] %in% c(3, 1)) {          # looks like channels-first...
        bottom = "3d"
      } else if(dim(x)[length(dim(x))-2] %in% c(3, 1)) { # looks like channels-first... OR the lst two dimensions are exactly equal
        bottom = "2d"
      }
    }
    if(length(dim(x)) >= 2) {
      if((dim(x)[length(dim(x))] == dim(x)[length(dim(x)) - 1]) & dim(x)[length(dim(x))] > 3) { # if the last two are exactly equal, and greader than 3
        bottom = "2d"
      }
    }
  }

  shape <- dim(x)
  if(bottom == "1d" & length(shape) == 1) {
    print_1d_bottom(x, end_n = max_rows, show_names = show_names, indent = indent, signif_digits = signif_digits, ...)
    return(invisible())
  }
  if(bottom == "2d" & length(shape) == 2) {
    print_2d_bottom(x, max_rows = max_rows, max_cols = max_cols, show_names = show_names, indent = indent, signif_digits = signif_digits, ...)
    return(invisible())
  }
  if(bottom == "3d" & length(shape) == 3) {
    print_3d_bottom(x, max_rows = max_rows, max_cols = max_cols, max_depth = max_depth, show_names = show_names, indent = indent, signif_digits = signif_digits, ...)
    return(invisible())
  }

  cat_indent(size = indent, is.tensor = TRUE)
  #ct(rep("| ", indent/2))
  ct("Rank ", length(shape) , " tensor, shape: (", comma(shape), ")")
  if(!is.null(ranknames(x))) {
    ct(", ranknames: ")
    ct(comma(ranknames(x)))
  }
  ct("\n")

  #sub_ts <- tt_index(x, 1:max_per_level, dimension = 1, drop = FALSE)
  #tt_apply(x, rank = 1, function(subt) {
  #  print(subt, show_names = show_names, max_per_level = max_per_level, bottom = bottom, max_rows = max_rows, max_cols = max_cols, max_depth = max_depth, signif_digits = signif_digits, indent = indent + 1, ...)
  #})
  for(i in 1:min(max_per_level, shape[1])) {
    # BUG, TODO: how do I drop the first rank only?
    subt <- tt_index(x, i, dimension = 1, drop = FALSE) # equiv of t[i, , , ] for a rank-4 tensor, but we don't know the rank hence calling tt_index
    # abind::adrop removes class when only one dim left?
    subt <- tt(abind::adrop(subt, drop = 1))
    #str("############")
    #print(i)
    #str(subt)
    print(subt, show_names = show_names, max_per_level = max_per_level, bottom = bottom, max_rows = max_rows, max_cols = max_cols, max_depth = max_depth, signif_digits = signif_digits, indent = indent + 1, ...)
  }
  cat_indent(size = indent+1, is.tensor = FALSE)
  #left <- dim(t)[1] - max_per_level
  #ct("# (and", left, "more Rank", length(dim(t))-1, "tensors ...)", color = crayon::make_style("#BBBBBB"))
  if(shape[1] > max_per_level) {
    ct("# ...")
  }
  ct("\n")
}


ct <- function(...) {
  cat(..., sep = "")
}


comma <-function(...) {
  return(paste0(..., collapse = ", "))
}

cat_indent <- function(size = 1, is.tensor = TRUE) {
  if(!is.tensor) {
    for(i in rep(0, size)) {
      ct("|  ")
    }
  } else {
    for(i in rep(0, max(0, size))) {
      ct("|  ")
    }
    #ct("+--")
    ct("# ")
  }
}

nicecolnames <- function(m, predims = 1, postdims = 0, max_cols = 6) {
  colnames <- as.character(1:ncol(m))
  if(!is.null(colnames(m))) {
    if(!all(is.na(colnames(m)))) {
      colnames <- paste0('"', colnames(m), '"' , sep = "")
    }
  }


  res <- rep("[", length(colnames))
  commas_pre <- paste0(rep(",", predims), collapse = "")
  commas_post <- paste0(rep(",", postdims), collapse = "")
  res <- paste0(res, commas_pre, colnames, commas_post, "]", sep = "")
  if(length(res) > max_cols) {
    res <- res[1:max_cols]
    res <- c(res, "...")
  }
  return(res)
}

nicerownames <- function(m, predims = 0, postdims = 1, max_rows = 6) {
  rownames <- as.character(1:nrow(m))
  if(!is.null(rownames(m))) {
    if(!all(is.na(rownames(m)))) {
      rownames <- paste0('"', rownames(m), '"' , sep = "")
    }
  }

  res <- rep("[", length(rownames))
  commas_pre <- paste0(rep(",", predims), collapse = "")
  commas_post <- paste0(rep(",", postdims), collapse = "")
  res <- paste0(res, commas_pre, rownames, commas_post, "]", sep = "")
  if(length(res) > max_rows) {
    res <- res[1:max_rows]
    res <- c(res, "...")
  }

  return(res)
}

space_fill <- function(charvec) {
  max_chars <- max(nchar(charvec))
  format <- paste0("%", max_chars, "s", collapse = "")
  res <- sprintf(format, charvec)
  return(res)
}

# returns a set of formatted lines (as a character vector)
nicemat <- function(m, show_row_names = TRUE, show_col_names = TRUE, row_predims = 0, row_postdims = 1, col_predims = 1, col_postdims = 0, max_rows = 6, max_cols = 6) {
  # sigh... apply returns a vector in some cases; if it does so this function fixes it back to a matrix
  fix <- function(x) {
    if(!is.matrix(x)) {
      return(t(as.matrix(x)))
    } else {
      return(x)
    }
  }

  m_char <- fix(apply(m, 2, as.character))
  if(nrow(m_char) > max_rows) {
    m_char <- m_char[1:max_rows, , drop = FALSE]
    dots <- rep("...", ncol(m_char))
    m_char <- rbind(m_char, dots, deparse.level = 0)
  }
  if(ncol(m_char) > max_cols) {
    m_char <- m_char[, 1:max_cols, drop = FALSE]
    dots <- rep("...", nrow(m_char))
    m_char <- cbind(m_char, dots)
  }

  if(show_col_names) {
    col_names <- nicecolnames(m, predims = col_predims, postdims = col_postdims, max_cols = max_cols)
    m_char <- rbind(col_names, m_char, deparse.level = 0)
  }
  if(show_row_names) {
    row_names <- nicerownames(m, predims = row_predims, postdims = row_postdims, max_rows = max_rows)
    if(show_col_names) {
      m_char <- cbind(c("", row_names), m_char)
    } else {
      m_char <- cbind(row_names, m_char)
    }
  }
  m_char <- fix(apply(m_char, 2, space_fill))
  collapserow <- function(row) {
    return(paste0(row, collapse = "  "))
  }
  m_char <- fix(apply(m_char, 1, collapserow))
  return(m_char)
}



print_1d_bottom <- function(t, end_n = 6, show_names = TRUE, indent = 0, signif_digits = 3, ...) {
  if(is.null(dim(t))) {stop("print_1d_bottom called on object t without dim(t).")}
  shape <- dim(t)
  t <- signif(t, signif_digits)
  if(length(shape) == 1) {
    #ct(rep(" ", indent))
    cat_indent(size = indent, is.tensor = TRUE)
    ct("Rank 1 tensor, shape: (", shape, ")")
    if(!is.null(ranknames(t))) {
      ct(", ranknames: ")
      ct(comma(ranknames(t)))
    }
    ct("\n")
    lines <- nicemat(t(as.matrix(t)), show_col_names = show_names, show_row_names = FALSE, col_predims = 0, max_cols = end_n)
    for(line in lines) {
      #ct(rep(" ", indent))
      cat_indent(size = indent, is.tensor = FALSE)
      cat("   ", line, "\n")
    }
  } else {
    stop("print_1d_bottom called on object t with length(dim(t)) != 1.")
  }
}

print_2d_bottom <- function(t, max_rows = 6, max_cols = 6, show_names = TRUE, indent = 0, signif_digits = 3, ...) {
  if(is.null(dim(t))) {stop("print_2d_bottom called on object t without dim(t).")}
  shape <- dim(t)
  t <- signif(t, signif_digits)
  if(length(shape) == 2) {
    #ct(rep(" ", indent))
    cat_indent(size = indent, is.tensor = TRUE)
    ct("Rank 2 tensor, shape: (", comma(shape), ")")
    if(!is.null(ranknames(t))) {
      ct(", ranknames: ")
      ct(comma(ranknames(t)))
    }
    ct("\n")
    lines <- nicemat(t, show_col_names = show_names, show_row_names = show_names, max_rows = max_rows, max_cols = max_cols)
    for(line in lines) {
      #ct(rep(" ", indent))
      cat_indent(size = indent, is.tensor = FALSE)
      cat("   ", line, "\n")
    }
  } else {
    stop("print_2d_bottom called on object t with length(dim(t)) != 2.")
  }
}


print_3d_bottom <- function(t, max_rows = 6, max_cols = 6, max_depth = 3, show_names = TRUE, indent = 0, signif_digits = 3, ...) {
  if(is.null(dim(t))) {stop("print_3d_bottom called on object t without dim(t).")}
  shape <- dim(t)
  t <- signif(t, signif_digits)
  if(length(shape) == 3) {
    #ct(rep(" ", indent))
    cat_indent(size = indent, is.tensor = TRUE)

    ct("Rank 3 tensor, shape: (", comma(shape), ")")
    if(!is.null(ranknames(t))) {
      ct(", ranknames: ")
      ct(comma(ranknames(t)))
    }

    recast <- apply(t, c(1,2), comma)
    bracketed <- paste0("[", recast, "]")
    bracketed <- array(bracketed, dim = shape[1:2])
    if(!is.null(dimnames(t))) {
      # arrgh R stop dropping things; this sets dummy dimnames for replacing
      if(is.null(dimnames(bracketed))) {
        dimnames(bracketed) <- lapply(dim(bracketed), function(times) {rep(NA, times)})
      }
      dimnames(bracketed)[[1]] <- dimnames(t)[[1]]
      dimnames(bracketed)[[2]] <- dimnames(t)[[2]]
      if(show_names & any(!is.na(dimnames(t)[[3]]))) {
        ct(" (bottom dimnames: ")
        dn <- paste0('"', dimnames(t)[[3]], '"')
        ct(comma(dn))
        ct(")")
      }
    }
    ct("\n")

    if(shape[3] > max_depth) {
      t <- t[, , 1:(max_depth+1), drop = FALSE]
      t[, , max_depth + 1] <- "..."
    }

    lines <- nicemat(bracketed, show_col_names = show_names, show_row_names = show_names, max_rows = max_rows, max_cols = max_cols, row_postdims = 2, col_postdims = 1)
    for(line in lines) {
      #ct(rep(" ", indent))
      cat_indent(size = indent, is.tensor = FALSE)
      cat("   ", line, "\n")
    }
  } else {
    stop("print_3d_bottom called on object t with length(dim(t)) != 3.")
  }
}




