#' @export
#' @title Parse a FASTA file into a tensortree, using 1-hot encoding for base representation.
#'
#' @description Parses a FASTA-formatted file, returning a 1-hot (or multi-hot if IUPAC codes are used) encoded tensor of rank 3, with ranknames "Sequence", "Base", and "Channel". Allows the user
#' to specify that start and end indices for sequences to grab (e.g. grab the 5th to the 20th sequences), or sequence IDs. The default "nucleotide" encoding
#' accepts lower and upper-case A, C, G, and T and degenerate IUPAC codes. The "protein" alphabet results in length-20 one-hot encoding. Alternatively,
#' one can set alphabet to a named list for endoding; e.g. `alphabet <- list("A" = c(1, 0), "C" = c(0, 1), "G" = c(0, 0), "T" = c(1, 1)`.
#'
#' @details If \code{seqnames} is given, only these are grabbed from the FASTA file for inclusion in the tensor (a warning will be produced
#' if any requested sequence IDs are not present in the file). If \code{seqnames} isn't given, \code{start} and \code{end} are used, defaulting
#' to grabbing all sequences in the file. (Warning: the resulting tensor and processing time may be large.)
#'
#' The \code{alphabet} parameter specifies how each letter in sequences should be encoded, as a named list.
#'
#'
#' @param fasta_file input filename to convert to a tensortree.
#' @param start starting sequence number to grab.
#' @param end ending sequence number to grab (see details).
#' @param ids only grab sequences with these sequence IDs.
#' @param alphabet either "nucleotide", "protein", or a named list that specifies the encoding, per base.
#' @param trim_to integer length to trim all sequences to if they are longer; don't attempt trimming if NULL.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a new tensortree.
#' @seealso \code{\link{ranknames}}.
#' @examples
#' t <- fasta_encode_tensor("seqs.fasta")
#' print(t, bottom = "2d", end_n = c(6, 4))
#'
fasta_encode_tensor <- function(fasta_file, start = 1, end = NULL, ids = NULL, alphabet = "nucleotide", trim_to = NULL, ...  ) {
  # TODO: taking a trim_to parameter for use by other calls, BUT, if trim_to is NULL we should still trim to the shortest in the batch specified so we can create a well-formed tensor
  if(all(alphabet == "nucleotide")) {
    alphabet <- list("A" = c(1, 0, 0, 0),
                     "C" = c(0, 1, 0, 0),
                     "G" = c(0, 0, 1, 0),
                     "T" = c(0, 0, 0, 1),
                     "W" = c(1, 0, 0, 1),
                     "S" = c(0, 1, 1, 0),
                     "M" = c(1, 1, 0, 0),
                     "K" = c(0, 0, 1, 1),
                     "R" = c(1, 0, 1, 0),
                     "Y" = c(0, 1, 0, 1),
                     "B" = c(0, 1, 1, 1),
                     "D" = c(1, 0, 1, 1),
                     "H" = c(1, 1, 0, 1),
                     "V" = c(1, 1, 1, 0),
                     "N" = c(0, 0, 0, 0))
  } else if(all(alphabet == "protein")) {
    letters <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V")
    alphabet <- list()
    for(i in 1:length(letters)) {
      letter <- letters[i]
      alphabet[[letter]] <- rep(0, 20)
      alphabet[[letter]][i] <- 1
    }
  }

  # make sure both upper and lower-case versions are covered
  for(letter in names(alphabet)) {
    alphabet[[tolower(letter)]] <- alphabet[[letter]]
    alphabet[[toupper(letter)]] <- alphabet[[letter]]
  }

  if (!requireNamespace("Rsamtools", quietly = TRUE)) {
    stop(paste0("Package Rsamtools needed for working with FASTA files. Please install it: https://bioconductor.org/packages/release/bioc/html/Rsamtools.html"),
         call. = FALSE)
  }


  if(!file.exists(paste0(fasta_file, "fai", collapse = ""))) {
    Rsamtools::indexFa(fasta_file)
  }

  idx <- Rsamtools::scanFaIndex(fasta_file)
  #names(idx) <- seqnames(idx)
  total_count <- Rsamtools::countFa(fasta_file)
  if(is.null(end)) {
    end <- total_count
  }

  if(start < 1) {start <- 1; warning("start parameter for fasta_encode_tensor less than 1, setting to 1.")}
  if(end > total_count) {end <- total_count; warning("end parameter for fasta_encode_tensor larger than number of sequences, setting to number of sequences.")}

  if(!is.null(ids)) {
    indices <- match(ids, as.character(GenomicRanges::seqnames(idx))) # figure out what indices we want...
    seqs <- Rsamtools::scanFa(fasta_file, idx[indices])
  } else {
    seqs <- Rsamtools::scanFa(fasta_file, idx[start:end])
  }

  seqsch <- as.character(seqs)
  # trim sequences if asked
  if(!is.null(trim_to)) {
    seqsch <- substr(seqsch, 1, trim_to)
  }

  dna_split_list <- stringr::str_split(seqsch, "")
  names(dna_split_list) <- names(seqsch)

  tensor <- array(rep(0, length(dna_split_list) * length(dna_split_list[[1]]) * length(alphabet[[1]])),
                    dim = c(length(dna_split_list),  length(dna_split_list[[1]]),  length(alphabet[[1]])))

  for(seqindex in 1:length(dna_split_list)) {
    for(baseindex in 1:length(dna_split_list[[1]])) {
      base <- dna_split_list[[seqindex]][baseindex]
      tensor[seqindex, baseindex, ] <- alphabet[[base]]
    }
  }

  tensor <- tt(tensor)
  ranknames(tensor) <- c("Sequence", "Base", "Channel")
  dimnames(tensor)[[1]] <- names(dna_split_list)
  return(tensor)
}

# for internal use
get_min_length <- function(fasta_files) {
  answer <- Inf
  unique_lengths_found <- c()
  for(fasta_file in fasta_files) {
    if(!file.exists(paste0(fasta_file, "fai", collapse = ""))) {
      Rsamtools::indexFa(fasta_file)
    }

    idx <- Rsamtools::scanFaIndex(fasta_file)
    seqlens <- GenomicRanges::seqinfo(idx)@seqlengths
    this_answer <- min(seqlens)
    unique_lengths_found <- unique(c(unique_lengths_found, seqlens))
    if(this_answer < answer) {
      answer <- this_answer
    }
  }
  needs_trimming <- FALSE
  if(length(unique_lengths_found) > 1) {
    needs_trimming <- TRUE
  }
  return(list(answer, needs_trimming))
}


#' @export
#' @title Parse entries from a FASTA file into a tensortree with target data to predict.
#'
#' @description Given a FASTA filename, a data.frame of sequence IDs and target values, generates a list with two elements: the first
#' being the tensor encoding the relavent sequences, and the second being a tensor encoding the targets.
#' The \code{class_mode} parameter takes the same values as  \code{\link{keras::flow_images_from_dataframe}} class_mode parameter,
#' with the addition of an "identity" option to support regression (no transformation of targets).
#'
#' @details For options \code{fasta_file}, and \code{alphabet}, see \code{\link{fasta_encode_tensor}}. If any IDs are specified that are not contained in the fasta_file a warning
#' will be produced, and the corresponding targets will be removed from the returned targets tensor.
#'
#' If \code{class_mode = "categorical"}, the targets are first integer-encoded with integers \code{1} through \code{length(unique(targets))} (using \code{as.integer(factor(targets))}, so generally
#' integer targets will be assigned in increasing target order), since \code{keras::to_categorical} can only accept integer labels. \code{dimnames()} of
#' of the target tensor will be set to preserve the original target labels. If \code{targets} is a factor, factor ordering will be used in determining one-hot encoding order.
#'
#'
#' @param ids_targets_df data.frame with IDs and target values of sequences to include in the tensor.
#' @param fasta_file input filename to convert to a tensortree.
#' @param class_mode equivalent to \code{\link{keras::flow_images_from_dataframe}} class_mode parameter. Additionally supports 'identity' for regression.
#' @param alphabet either "nucleotide", "protein", or a named list that specifies the encoding, per base.
#' @param ids_col column name to use for sequence ids.
#' @param targets_col column name to use for target values.
#' @param trim_to integer length to trim all sequences to if they are longer; don't attempt trimming if NULL.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a new tensortree.
#' @seealso \code{\link{fasta_encode_tensor}}.
#' @examples
#' df <- data.frame(seqid = c("EP680779", "EP433799", "EP619387", "EP637315"),
#'                  class = c("A", "B", "C", "B"))
#'
#' t <- fasta_train_batch(df)
#' print(t, bottom = "2d", end_n = c(6, 4))
#'
#' df <- data.frame(seqid = c("EP680779", "EP433799", "EP619387", "EP637315"),
#'                  class = c("yes", "no", "yes", "yes"))
#'
#' t <- fasta_train_batch(df, class_mode = "binary")
#'
#' #' df <- data.frame(seqid = c("EP680779", "EP433799", "EP619387", "EP637315"),
#'                  class = c(-1.2, 2.5, 6, 3.1))
#'
#' t <- fasta_train_batch("seqs.fasta", df, class_mode = "identity")
#' print(t, bottom = "2d", end_n = c(6, 4))
fasta_train_batch <- function(ids_targets_df, fasta_file, class_mode = "categorical", alphabet = "nucleotide", ids_col = "seqid", targets_col = "class", trim_to = NULL, ...  ) {
  # NOTE: putting the dataframe first makes this more pipe-able
  # TODO: implement "binary" and options for specifying column names
  ids <- ids_targets_df[[ids_col]]
  targets <- ids_targets_df[[targets_col]]

  if(all(alphabet == "nucleotide")) {
    alphabet <- list("A" = c(1, 0, 0, 0),
                     "C" = c(0, 1, 0, 0),
                     "G" = c(0, 0, 1, 0),
                     "T" = c(0, 0, 0, 1),
                     "W" = c(1, 0, 0, 1),
                     "S" = c(0, 1, 1, 0),
                     "M" = c(1, 1, 0, 0),
                     "K" = c(0, 0, 1, 1),
                     "R" = c(1, 0, 1, 0),
                     "Y" = c(0, 1, 0, 1),
                     "B" = c(0, 1, 1, 1),
                     "D" = c(1, 0, 1, 1),
                     "H" = c(1, 1, 0, 1),
                     "V" = c(1, 1, 1, 0),
                     "N" = c(0, 0, 0, 0))
  } else if(all(alphabet == "protein")) {
    letters <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V")
    alphabet <- list()
    for(i in 1:length(letters)) {
      letter <- letters[i]
      alphabet[[letter]] <- rep(0, 20)
      alphabet[[letter]][i] <- 1
    }
  }

  # make sure both upper and lower-case versions are covered
  for(letter in names(alphabet)) {
    alphabet[[tolower(letter)]] <- alphabet[[letter]]
    alphabet[[toupper(letter)]] <- alphabet[[letter]]
  }

  # we don't want to re-factor if they are already a factor with defined levels, as this
  # reduces the levels to just the entries present
  # but we do need it to be a factor, so that we can
  if(!is.factor(targets) & class_mode == "categorical") {
    targets <- factor(targets)
  }

  if(length(ids) != length(targets)) {
    stop("Error: number of ids and targets must match in call to fasta_train_batch.")
  }

  tensor <- fasta_encode_tensor(fasta_file, ids = ids, alphabet = alphabet, trim_to = trim_to)
  if(dim(tensor)[1] != length(targets)) {
    warning("Warning: fasta_train_batch called with ids that are not present in the FASTA file, removing corresponding entries from targets.")
    ids <- ids[ids %in% dimnames(tensor)[[1]]]
    targets <- targets[ids %in% dimnames(tensor)[[1]]]
  }

  if(class_mode == "categorical") {
    # why not just dispatch to keras::to_categorical()?
    # - largely due to the fact that we want to allow character-like categorical targets,
    # or factors where the level order determines the dimension ordering, *and* we need
    # to use levels-based encoding because this may be called with a subset of data.
    # (I suppose this is one of those places split-apply-combine doesn't shine)
    # ...but, keras::to_categorical() takes only integers and assumes 0-indexing
    cat_labels <- levels(targets)
    cat_ints <- as.integer(targets) - 1 # keras uses 0 indexing...; note that targets must be a factor at this point
    # this seems to be *breaking* if length(cat_ints) = 1 - is this a bug in keras' to_categorical? Maybe... the shape is different if only 1 target is given
    target_tensor <- as.tensortree(keras::to_categorical(cat_ints, num_classes = length(cat_labels)))
    # fix for above:
    if(length(cat_ints) == 1) {
      target_tensor <- as.tensortree(keras::array_reshape(target_tensor, dim = c(length(cat_ints), length(cat_labels))))
    }
    ranknames(target_tensor) <- c("Sequence", "Target")
    dimnames(target_tensor)[[1]] <- ids
    dimnames(target_tensor)[[2]] <- cat_labels
  } else if(class_mode == "identity"){
    target_tensor <- as.tensortree(targets)
    ranknames(target_tensor) <- "Target"
    dimnames(target_tensor)[[1]] <- ids
    # TODO: add an "input" mode for autoencoders, maybe "other" as well
  } else if(class_mode == "binary") {
    if(length(levels(targets)) != 2) {
      stop("Error: class_mode = 'binary' requires exactly two distinct target values. (If using a factor, be sure length(levels()) is exactly two.)")
    }
    cat_labels <- levels(targets)
    target_tensor <- as.tensortree(as.integer(targets) - 1) # keras uses 0 indexing...; note that targets must be a factor at this point
    ranknames(target_tensor) <- cat_labels[2]
    dimnames(target_tensor)[[1]] <- ids
  } else {
    error("Invalid 'class_mode', use one of: 'categorical', 'binary', or 'identity'.")
  }

  return(list(tensor, target_tensor))
}







# from a fasta, returns a dataframe with a column for seqid (each sequence id in the fasta),
# the filename (repeated), and the class, derived from the filename (repeated)
# meant to be called for different fasta files and combined
# (for internal use)
fasta_to_targets_df <- function(fasta_file) {
  if(!file.exists(paste0(fasta_file, "fai", collapse = ""))) {
    Rsamtools::indexFa(fasta_file)
  }

  idx <- Rsamtools::scanFaIndex(fasta_file)
  ret_df <- data.frame(seqid = as.character(GenomicRanges::seqnames(idx)))
  message(paste0("Found ", nrow(ret_df), " sequences in ", fasta_file, "."))
  ret_df$filename <- fasta_file
  ret_df$class <- basename(tools::file_path_sans_ext(fasta_file))
  #rownames(ret_df) <- paste0(ret_df$filename, ":", ret_df$seqid)
  return(ret_df)
}



#' @export
#' @title Generate a dataframe containing IDs and file-based target labels from a list of fasta-encoded filenames.
#'
#' @description Given a vector of FASTA filenames, returns a dataframe with columns for "filename", "seqid", and "class", where class is derived from the filename.
#' Useful in conjunction with \code{\link{flow_sequences_from_fasta_df}}.
#' #'
#' @param fasta_files vector of fasta file names.
#' @param directory directory to look for fasta files, in; if NULL, use current working directory.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a data.frame.
#' @seealso \code{\link{flow_sequences_from_fasta_df}}.
#' @examples
#' df <- fastas_to_targets_df(c("seqs1.fasta", "seqs2.fasta"))
#'
#' gen <- flow_sequences_from_fasta_df(df)
fastas_to_targets_df <- function(fasta_files, directory = NULL) {
  if(!is.null(directory)) {
    fasta_files <- paste(directory, fasta_files, sep = "/")
  }
  resdfs <- lapply(fasta_files, fasta_to_targets_df)
  return(do.call(rbind, resdfs))
}


#' @export
#' @title A keras-style generator function for training batches of sequence data stored in fasta-formatted files, via data.frame.
#'
#' @description Given data.frame of fasta file paths, sequence IDs and target values, returns a generator function generating
#' a list with two elements: the first being the tensor encoding the relavent sequences, and the second being a tensor encoding the targets.
#' The \code{class_mode} parameter takes the same values as  \code{\link{keras::flow_images_from_dataframe}} class_mode parameter,
#' with the addition of an "identity" option to support regression (no transformation of targets).
#'
#' @details \code{alphabet} may be set to \code{"nucleotide"} (for length-four one-hot encodings (or multi-hot if IUPAC codes))
#' are used)), \code{"protein"} (length-20 one-hot encodings), or a named list specifying per-base encodings; see examples. If any IDs are
#' specified that are not contained in the corresponding fasta_file a warning
#' will be produced, and the corresponding targets will be removed from the returned targets tensor.
#'
#' If \code{class_mode = "categorical"}, the targets are first integer-encoded with integers \code{1} through \code{length(unique(targets))} (using \code{as.integer(factor(targets))}, so generally
#' integer targets will be assigned in increasing target order), since \code{keras::to_categorical} can only accept integer labels. \code{dimnames()} of
#' of the target tensor will be set to preserve the original target labels. If \code{targets} is a factor, factor ordering will be used in determining one-hot encoding order.
#'
#'
#' @param df data.frame to read filenames, sequence IDs, and target values from.
#' @param directory Directory where to find the files; present working directory if NULL.
#' @param filename_col = column name for identifying filenames.
#' @param ids_col column name for identifying sequence IDs.
#' @param targets_col column name for identifying targets.
#' @param class_mode how the encode the target column into a target tensor; one of "categorical", "binary", or "identity".
#' @param batch_size number of sequences to include in each batch.
#' @param shuffle whether to reshuffle sequences between epochs (once all batches have been generated and recycling happens).
#' @param seed random seed to set before each re-shuffle,
#' @param alphabet either "nucleotide", "protein", or a named list that specifies the encoding, per base.
#' @param trim_to integer length to trim all sequences to if they are longer; if NULL, trim to shortest sequence in the files.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a new tensortree.
#' @seealso \code{\link{fasta_encode_tensor}}.
#' @examples
#' df <- data.frame(filename = c("inst/extdata/seqs.fasta", "inst/extdata/seqs.fasta", "inst/extdata/seqs2.fasta", "inst/extdata/seqs2.fasta"),
#'                  seqid = c("EP680779", "EP433799", "EP619387", "EP637315"),
#'                  class = c("yes", "no", "yes", "yes"))
#'
#' gen <- flow_sequences_from_fasta_df(df, class_mode = "binary")
#' batch <- gen()
#'
#'
#' df <- data.frame(filename = c("inst/extdata/seqs.fasta", "inst/extdata/seqs.fasta", "inst/extdata/seqs2.fasta", "inst/extdata/seqs2.fasta"),
#'                     seqid = c("EP680779", "EP433799", "EP619387", "EP637315"),
#'                    class = c(-1.2, 2.5, 6, 3.1))
#'
#' gen <- flow_sequences_from_fasta_df(df, class_mode = "identity")
#' batch <- gen()
#'
#' # specify an alternative target encoding
#' gen <- flow_sequences_from_fasta_df(df, class_mode = "identity", alphabet = list(A = c(1, 0),
#'                                                                               C = c(0, 1),
#'                                                                               G = c(0, -1)
#'                                                                               T = c(-1, 0))
#'
#'
flow_sequences_from_fasta_df <- function(df,
                                         directory = NULL,
                                         filename_col = "filename",
                                         ids_col = "seqid",
                                         targets_col = "class",
                                         class_mode = "categorical",
                                         batch_size = 32,
                                         shuffle = TRUE,
                                         seed = NULL,
                                         alphabet = "nucleotide",
                                         trim_to = NULL, ...) {



  # if it isn't a factor already, make it one - we use the levels information for coding the category labels
  if(!is.factor(df[[targets_col]])) {
    df[[targets_col]] <- factor(df[[targets_col]])
  }

  if(!is.null(directory)) {
    df[[filename_col]] <- paste(directory, filename_col, sep = "/")
  }

  # TODO: it's easy to trim to the shortest amongst all the sequences in the files, but it would be somewhat more
  # flexible to trim to the shortest amongst the sequences in the df
  if(is.null(trim_to)) {
    trim_info <- get_min_length(unique(df[[filename_col]]))
    if(trim_info[[2]] == TRUE) { # needs trimming, as more than one unique length was found
      trim_to <- trim_info[[1]]
      warning("Sequences in files are not all identical length; trimming to shortest (", trim_to, " bp).")
    }
  }

  ##### Shuffle if necessary
  if(shuffle) {
    if(!is.null(seed)) {
      set.seed(seed)
    }
    permutation <- sample(1:nrow(df))
    df <- df[permutation, ]
  }

  index <- 0
  generator <- function() {
    to_grab <- (index:(index + batch_size - 1) %% nrow(df)) + 1

    sub_df <- df[to_grab, ]

    # re-shuffle at the end of each epoch if shuffle=TRUE
    if(shuffle & (index + batch_size) > nrow(df)) {
      if(!is.null(seed)) {
        set.seed(seed)
      }
      permutation <- sample(1:nrow(df))
      df <<- df[permutation, ]
    }

    index <<- ((index + batch_size) %% nrow(df))

    # list of dataframes, one per filename
    df_list <- split(sub_df, sub_df[[filename_col]])
    batch_x_tensors <- as.list(1:length(df_list)) # placeholder for resulting batches
    batch_y_tensors <- as.list(1:length(df_list))
    batch_index <- 1
    for(filename in names(df_list)) {
      file_df <- df_list[[filename]]
      train_batch <- fasta_train_batch(file_df, filename, class_mode, alphabet, ids_col, targets_col, trim_to = trim_to)
      batch_x_tensors[[batch_index]] <- train_batch[[1]]
      batch_y_tensors[[batch_index]] <- train_batch[[2]]

      batch_index <- batch_index + 1
    }

    # batches is now a list of list(x_tensor, y_tensor)
    x_tensor <- do.call(c, batch_x_tensors)
    y_tensor <- do.call(c, batch_y_tensors)

    return(list(x_tensor, y_tensor))
  }

  return(generator)
}

# #' @export
#' @title Categorical keras-style generator function for training batches of sequence data stored in fasta-formatted files.
#'
#' @description Given a list of fasta files, produces a keras-style generator function for generating batches of training data. Each
#' fasta file is treated as identifying a different class and targets are one-hot encoded for classification (class_mode = "binary" may also
#' be specified if there are eactly two classes); see examples.
#'
#' @details \code{alphabet} may be set to \code{"nucleotide"} (for length-four one-hot encodings (or multi-hot if IUPAC codes))
#' are used)), \code{"protein"} (length-20 one-hot encodings), or a named list specifying per-base encodings; see examples. If any IDs are
#' specified that are not contained in the corresponding fasta_file a warning
#' will be produced, and the corresponding targets will be removed from the returned targets tensor.
#'
#'
#' @param fasta_files vector of fasta file names to read.
#' @param directory directory to read fasta_files from, use present working directory if NULL.
#' @param batch_size number of training examples and targets to include in each call of the generator.
#' @param shuffle whether to reshuffle sequences between epochs (once all batches have been generated and recycling happens).
#' @param seed random seed to set before each re-shuffle,
#' @param class_mode how the encode the target column into a target tensor; one of "categorical", "binary", or "identity".
#' @param alphabet either "nucleotide", "protein", or a named list that specifies the encoding, per base.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a new tensortree.
#' @seealso \code{\link{fasta_encode_tensor}}.
#' @examples
#' gen <- flow_sequences_from_fastas(c("inst/extdata/seqs.fasta", "inst/extdata/seqs2.fasta"),
#'                                   class_mode = "binary")
#' batch <- gen()
#'
#' # categorical for more than two classes;
#' # also specify an alternative target encoding
#' gen <- flow_sequences_from_fasta(df,
#'                                  class_mode = "categorical",
#'                                  alphabet = list(A = c(1, 0),
#'                                                  C = c(0, 1),
#'                                                  G = c(0, -1)
#'                                                  T = c(-1, 0))
#'
#' gen()
#'
flow_sequences_from_fastas <- function(fasta_files,
                                       directory = NULL,
                                       batch_size = 32,
                                       shuffle = FALSE,
                                       seed = NULL,
                                       class_mode = "categorical",
                                       alphabet = "nucleotide",
                                       trim_to = NULL, ...) {

  if(is.null(trim_to)) {
    trim_info <- get_min_length(unique(fasta_files))
    if(trim_info[[2]] == TRUE) { # needs trimming, as more than one unique length was found
      trim_to <- trim_info[[1]]
      warning("Sequences in files are not all identical length; trimming to shortest (", trim_to, " bp).")
    }
  }

  ##### Generate list of names present in each file, along with the file they are present in and the target name
  ##### e.g. seqnames[["file1.fasta"]] <- c("id351", "file1.fasta")
  df <- fastas_to_targets_df(fasta_files)
  gen <- flow_sequences_from_fasta_df(df, directory = directory, batch_size = batch_size, shuffle = shuffle, seed = seed, class_mode = class_mode, alphabet = alphabet, trim_to = trim_to)
  return(gen)
}



# TODO: move some of this stuff into unit tests
# TODO: adjust unit tests to use is_keras_available() per the keras rstudio faq
# network <- keras_model_sequential() %>%
#   layer_dense(units = 64, activation = "relu", input_shape = c(140, 4)) %>%
#   layer_flatten() %>%
#   layer_dense(units = 128, activation = "relu") %>%
#   layer_dense(units = 2, activation = "sigmoid") # have to units=2 until I get "binary" implemented
#
# compile(network, optimizer = "rmsprop", loss = "binary_crossentropy")
#
#
#  gen <- flow_sequences_from_fastas(c("inst/extdata/seqs.fasta", "inst/extdata/seqs2.fasta"), batch_size = 100)
#
# fit_generator(network, gen, steps_per_epoch = 10)
#
# #str(predict_generator(network, gen, steps = 4))
