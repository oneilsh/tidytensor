#' @export
#' @title Parse a FASTA file into a keras-compatible tensor.
#'
#' @description Given a filename for a FASTA-formatted file, returns a keras-compatible tensor
#' (multidimensional array) that is also a tensortree. The shape is [number of sequences, number of bases per sequence, length],
#' and ranknames ("seq", "base", "channel") are set.
#'
#' @details Because the inputs are parsed into a multidimensional array, each sequence in the
#' file must be the same length. Currently only supports sequences consisting of A, C, T, and G letters.
#'
#' @param fasta_file input filename to convert to a tensortree.
#' @param ... additional arguments to be passed to or from methods (ignored).
#' @return a new tensortree.
#' @seealso \code{\link{ranknames}}.
#' @examples
#' t <- read_fasta_tensortree("seqs.fasta")
#' summary(t)
#'
read_fasta_tensortree <- function(fasta_file, ...) {

  ## TODO: This doesn't seem like the right way to do this...
  for(pkg in c("reticulate", "seqinr", "stringr", "keras")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package ", pkg , " needed for read_fasta_tensortree(). Please install it."),
           call. = FALSE)
    }
  }


  dna_seqs <- seqinr::read.fasta(file = fasta_file, as.string = TRUE, forceDNAtolower = FALSE)
  dna_vec <- unlist(dna_seqs)
  num_seqs <- length(dna_seqs)
  if(length(unique(unlist(lapply(dna_vec, nchar)))) > 1) {
    stop("Sorry, all of the sequences in the fasta file must be the same length to turn them into a tensor.")
  }
  seqs_length <- nchar(dna_seqs[1])
  dna_split_list <- stringr::str_split(dna_vec, "")
  dna_matrix <- do.call(rbind, dna_split_list)
  dna_spread <- keras::array_reshape(dna_matrix, dim = prod(dim(dna_matrix)))
  one_hot <- lapply(dna_spread,
                    function(letter) {
                      if(letter == "A") {
                        return(c(1, 0, 0, 0))
                      } else if(letter == "C") {
                        return(c(0, 1, 0, 0))
                      } else if(letter == "G") {
                        return(c(0, 0, 1, 0))
                      } else {
                        return(c(0, 0, 0, 1))
                      }
                    })
  flattened <- unlist(one_hot)
  dna_tensor <- as.tensortree(keras::array_reshape(flattened, dim = c(num_seqs, seqs_length, 4)))
  #ranknames(dna_tensor) <- c("seq", "base", "channel")
  return(dna_tensor)
}





