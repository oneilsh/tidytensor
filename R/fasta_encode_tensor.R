#' @export
#' @title Parse a FASTA file into a tensortree, using 1-hot encoding for base representation.
#'
#' @description Parses a FASTA-formatted file, returning a 1-hot encoded tensor of rank 3, with ranknames "Sequence", "Base", and "Channel". Allows the user
#' to specify that start and end indices for sequences to grab (e.g. grab the 5th to the 20th sequences), or sequence IDs. The default one-hot encoding
#' accepts letters "A", "C", "G", "T", and "N" only (with "N" encoded with \code{0, 0, 0, 0}).
#'
#' @details If \code{seqnames} is given, only these are grabbed from the FASTA file for inclusion in the tensor (a warning will be produced
#' if any requested sequence IDs are not present in the file). If \code{seqnames} isn't given, \code{start} and \end{end} are used, defaulting
#' to grabbing all sequences in the file. (Warning: the resulting tensor and processing time may be large.)
#'
#' The \code{alphabet} parameter specifies how each letter in sequences should be encoded, as a named list.
#'
#'
#' @param fasta_file input filename to convert to a tensortree.
#' @param start starting sequence number to grab.
#' @param end ending sequence number to grab (see details).
#' @param ids only grab sequences with these sequence IDs.
#' @param alphabet a list that specifies the one-hot encoding, per base.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a new tensortree.
#' @seealso \code{\link{ranknames}}.
#' @examples
#' t <- fasta_encode_tensor("seqs.fasta")
#' print(t, bottom = "2d", end_n = c(6, 4))
#'
fasta_encode_tensor <- function(fasta_file, start = 1, end = NULL, ids = NULL, keras_format = TRUE, alphabet = list("A" = c(1, 0, 0, 0),
                                                                                                                         "C" = c(0, 1, 0, 0),
                                                                                                                         "G" = c(0, 0, 1, 0),
                                                                                                                         "T" = c(0, 0, 0, 1),
                                                                                                                         "N" = c(0, 0, 0, 0)), ...  ) {
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
  dna_split_list <- stringr::str_split(seqsch, "")
  names(dna_split_list) <- names(seqsch)
  alphabet <- list("A" = c(1, 0, 0, 0), "C" = c(0, 1, 0, 0), "G" = c(0, 0, 1, 0), "T" = c(0, 0, 0, 1), "N" = c(0, 0, 0, 0))

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







#' @export
#' @title Parse entries from a FASTA file into a tensortree with target data to predict.
#'
#' @description Given a FASTA filename, a list of sequence IDs, and a list of target values, generates a list with two elements: the first
#' being the tensor encoding the relavent sequences, and the second being a tensor encoding the targets. If the desired targets are categorical,
#' \code{keras_categorical_targets = TRUE} uses Keras' \code{to_categorical} function to encode them as one-hot vectors as well.
#'
#' @details For options \code{fasta_file}, \code{keras_format}, and \code{alphabet}, see \code{\link{fasta_encode_tensor}}. The \code{ids} and
#' \code{targets} vectors must be identical in length; if any \code{ids} are specified that are not contained in the FASTA file a warning
#' will be produced, and the corresponding targets will be removed from the returned targets tensor.
#'
#' If \code{keras_categorical_targets = TRUE}, the targets are first integer-encoded with integers \code{1} through \code{length(unique(targets))} (using \code{as.integer(factor(targets))}, so generally
#' integer targets will be assigned in increasing target order), since \code{keras::to_categorical} can only accept integer labels. \code{dimnames()} of
#' of the target tensor will be set to preserve the original target labels. If \code{targets} is a factor, factor ordering will be used in target assignment.
#'
#'
#' @param fasta_file input filename to convert to a tensortree.
#' @param ids IDs of sequences to include in the tensor.
#' @param targets training targets for ids.
#' @param keras_categorical_targets convert targets with \code{keras::to_categorical}.
#' @param keras_format format the tensor using Keras/Python-style ordering (row-major), otherwise use default array ordering.
#' @param alphabet a list that specifies the one-hot encoding, per base.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a new tensortree.
#' @seealso \code{\link{fasta_encode_tensor}}.
#' @examples
#' t <- fasta_train_batch("seqs.fasta", c("GL15X3", "AGP4", "CATB"), c(1.2, -4.5, 6.7))
#' print(t, bottom = "2d", end_n = c(6, 4))
#'
#' t <- fasta_train_batch("seqs.fasta", c("GL15X3", "AGP4", "CATB"), c("positive", "negative", "positive"), keras_categorical_targets = TRUE)
#' print(t, bottom = "2d", end_n = c(6, 4))
fasta_train_batch <- function(fasta_file, ids, targets, keras_categorical_targets = FALSE, keras_format = TRUE, alphabet = list("A" = c(1, 0, 0, 0),
                                                                                                                                "C" = c(0, 1, 0, 0),
                                                                                                                                "G" = c(0, 0, 1, 0),
                                                                                                                                "T" = c(0, 0, 0, 1),
                                                                                                                                "N" = c(0, 0, 0, 0)), ...  ) {

  # we don't want to re-factor if they are already a factor with defined levels (as in called from flow_sequences_from_fasta), as this
  # reduces the levels to just the entries present
  if(!is.factor(targets) & keras_categorical_targets) {
    targets <- factor(targets)
  }

  if(length(ids) != length(targets)) {
    stop("Error: number of ids and targets must match in call to fasta_train_batch.")
  }

  tensor <- fasta_encode_tensor(fasta_file, ids = ids, keras_format = keras_format, alphabet = alphabet)
  if(dim(tensor)[1] != length(targets)) {
    warning("Warning: fasta_train_batch called with ids that are not present in the FASTA file, removing corresponding entries from targets.")
    ids <- ids[ids %in% dimnames(tensor)[[1]]]
    targets <- targets[ids %in% dimnames(tensor)[[1]]]
  }

  if(keras_categorical_targets) {
    cat_labels <- levels(targets)
    cat_ints <- as.integer(targets) - 1 # keras uses 0 indexing...
    target_tensor <- as.tensortree(keras::to_categorical(cat_ints, num_classes = length(cat_labels)))
    ranknames(target_tensor) <- c("Sequence", "Target")
    dimnames(target_tensor)[[1]] <- ids
    dimnames(target_tensor)[[2]] <- cat_labels
  } else {
    target_tensor <- as.tensortree(targets)
    ranknames(target_tensor) <- "Target"
    dimnames(target_tensor)[[1]] <- ids
  }

  return(list(tensor, target_tensor))
}






#' @export
#' @title Returns a Keras-style generator for training from sequence data stored in a FASTA file in batches.
#'
#' @description Given a FASTA filename, a list of sequence IDs, and a list of target values, generates a list with two elements: the first
#' being the tensor encoding the relavent sequences, and the second being a tensor encoding the targets. If the desired targets are categorical,
#' \code{keras_categorical_targets = TRUE} uses Keras' \code{to_categorical} function to encode them as one-hot vectors as well.
#'
#' @details For options \code{fasta_file}, \code{keras_format}, and \code{alphabet}, see \code{\link{fasta_encode_tensor}}. The \code{ids} and
#' \code{targets} vectors must be identical in length; if any \code{ids} are specified that are not contained in the FASTA file a warning
#' will be produced, and the corresponding targets will be removed from the returned targets tensor.
#'
#' If \code{keras_categorical_targets = TRUE}, the targets are first integer-encoded with integers \code{1} through \code{length(unique(targets))} (using \code{as.integer(factor(targets))}, so generally
#' integer targets will be assigned in increasing target order), since \code{keras::to_categorical} can only accept integer labels. \code{dimnames()} of
#' of the target tensor will be set to preserve the original target labels.
#'
#'
#' @param fasta_file input filename to generate training batches from.
#' @param ids IDs of sequences to include in the tensor.
#' @param targets training targets for ids.
#' @param keras_categorical_targets convert targets with \code{keras::to_categorical}.
#' @param batch_size returns training batches of this size.
#' @param shuffle should the ids be shuffled first, or batches returned in the order specified?
#' @param seed use this random seed if shuffling.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a generator function, generating training batches with each call.
#' @seealso \code{\link{fasta_encode_tensor}}, \code{\link{fasta_train_batch}}.
#' @examples
#' gen <- flow_sequences_from_fasta("seqs.fasta", c("GL15X3", "AGP4", "CATB"), c(1.2, -4.5, 6.7))
#' batch <- gen()
#' print(batch[[1]])
#' print(batch[[2]])
flow_sequences_from_fasta <- function(fasta_file,
                                      ids,
                                      targets,
                                      keras_categorical_targets = F,
                                      batch_size = 20,
                                      shuffle = FALSE,
                                      seed = NULL, ...) {

  if(!is.factor(targets) & keras_categorical_targets) {
    targets <- factor(targets)
  }

  ##### Figure out which we can use
  if(!file.exists(paste0(fasta_file, "fai", collapse = ""))) {
    Rsamtools::indexFa(fasta_file)
  }

  idx <- Rsamtools::scanFaIndex(fasta_file)
  keep <- which(ids %in% as.character(GenomicRanges::seqnames(idx)))
  if(length(keep) < length(ids)) {
    warning(paste0("Some ids were not found in the fasta file, dropping ids and corresponding targets: ",
                   paste(ids[-keep], collapse = ", ")))
  }

  ids <- ids[keep]
  targets <- targets[keep]

  ##### Shuffle if necessary
  if(!is.null(seed)) {
    set.seed(seed)
  }
  if(shuffle) {
    permutation <- sample(1:length(ids))
    ids <- ids[permutation]
    targets <- targets[permutation]
  }



  index <- 0
  generator <- function() {
    to_grab <- (index:(index + batch_size - 1) %% length(ids)) + 1


    ids_batch <- ids[to_grab]
    targets_batch <- targets[to_grab]

    index <<- ((index + batch_size) %% length(ids))

    return(fasta_train_batch(fasta_file, ids_batch, targets_batch, keras_categorical_targets = keras_categorical_targets))
  }

  return(generator)
}




