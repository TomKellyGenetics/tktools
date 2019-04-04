#' strstrings
#'
#' split multiple strings and extact indicies
#'
#' @param strings vector or list of strings to split
#' @param n indicies of each string to extract (and paste back together)
#' @param split character to split by
#' @keywords plot graphics annotation title legend
#' @importFrom graphics plot
#' @export
#' @examples
#'
#' # example string
#' string <- c("1-4-5", "4-6-3", "1-0-3")
#' splitstrings(string, 2)
#'
#' @export
#' @usage NULL
splitstrings <- function(strings, n = 1, split="-") {
  if(length(n) == 1){
    extract <- function(string) strsplit(string, split = split)[[1]][n]
  } else {
    extract <- function(string) paste(strsplit(string, split = split)[[1]][n], collapse = split)
  }
  sapply(strings, extract)
}

#' stringsplitter
#'
#' split multiple strings and extact indicies
#'
#' @param strings vector or list of strings to split
#' @param n indicies of each string to extract (and paste back together)
#' @param split character to split by
#' @keywords plot graphics annotation title legend
#' @importFrom graphics plot
#' @export
#' @examples
#'
#' # example string
#' string <- c("1-4-5", "4-6-3", "1-0-3")
#' stringsplitter(string, 2)
#'
#' @export
#' @usage NULL
stringsplitter <- splitstrings

#' revcomp
#'
#' compute the reverse-complement of a string of nucleotide bases
#'
#' @return returns a string, vector, character, or list in the same class as the input sequences
#'
#' @param seq string. A sequence of nucleotide bases (can handle ambiguous bases)
#' @param upper.case logical. Whether to force return of upper (TRUE) or lower (FALSE) case. Default to matching input case.
#' @param complement logical. Whether to compute the complementary sequence (defaults to TRUE).
#' @param reverse logical. Whether to reverse the order of the sequence (defaults to TRUE).
#' @keywords sequence bases strings complement molecular-biology
#' @importFrom seqinr comp
#' @export
#' @examples
#'
#' # example plot
#' sequence <- "ATCG"
#' revcomp(sequence)
#' sequence <- "atcg"
#' revcomp(sequence)
#' sequence <- "AtgaNVVMKSvdbTC"
#' revcomp(sequence)
#' sequence <- c("ATACAG", "TATAAT", "TTGACA", "A", "c", "g", "nnn")
#' revcomp(sequence)
#' sequence <- list("ATACAG", "TATAAT", "TTGACA", "A", "c", "g", "nnn")
#' revcomp(sequence)
#' sequence <- factor("ATACAG", "TATAAT", "TTGACA", "A", "c", "g", "nnn")
#' revcomp(sequence)
#' sequence <- "ATCG"
#' revcomp(sequence, reverse = FALSE)
#' revcomp(sequence, complement = FALSE)
#' revcomp(sequence, upper.case = FALSE)
#'
#' @export
#' @usage NULL
revcomp <- function(seq, upper.case = NULL, complement = TRUE, reverse = TRUE) {
  UseMethod("revcomp")
}

#' @rdname revcomp
#' @export
revcomp.factor <-
  function(seq, upper.case = NULL, complement = TRUE, reverse = TRUE) {
    seq <- as.character(seq)
    output <- revcomp(seq, upper.case = NULL, complement = TRUE, reverse = TRUE)
    output <- factor(output, levels = levels(seq))
    return(output)
  }

#' @rdname revcomp
#' @export
revcomp.character <-
  function(seq, upper.case = NULL, complement = TRUE, reverse = TRUE) {
    if(length(seq) > 1){
      output <- sapply(seq, revcomp.default, upper.case = NULL, complement = TRUE, reverse = TRUE)
    } else {
      output <- revcomp.default(seq, upper.case = NULL, complement = TRUE, reverse = TRUE)
    }
    return(output)
  }

#' @rdname revcomp
#' @export
revcomp.list <-
  function(seq, upper.case = NULL, complement = TRUE, reverse = TRUE) {
    output <- lapply(seq, revcomp, upper.case = NULL, complement = TRUE, reverse = TRUE)
    return(output)
  }

#' @rdname revcomp
#' @export
revcomp.default <-
  function(seq, upper.case = NULL, complement = TRUE, reverse = TRUE) {
    input <- unlist(strsplit(seq, split = ""))
    # detect case
    if(seq == toupper(seq)){
      upper.case = TRUE
    } else if(seq == tolower(seq)){
      upper.case = FALSE
    } else {
      upper.case <- unlist(lapply(input, function(base){
        if(base ==  toupper(base)){
          TRUE
        } else if(base ==  tolower(base)){
          FALSE
        } else {
          NA
        }
      }))
    }
    #compute complementary sequence
    if(complement == TRUE){
      seq.comp <- comp(input)
    } else {
      seq.comp <- input
    }
    #compute reverse sequence
    if(reverse == TRUE){
      seq.revcomp <- rev(seq.comp)
      upper.case <- rev(upper.case)
    } else {
      seq.revcomp <- seq.comp
    }
    #output format
    if(upper.case == TRUE && length(upper.case) == 1){
      output <- toupper(seq.revcomp)
    } else if(upper.case == FALSE && length(upper.case) == 1){
      output <- tolower(seq.revcomp)
    } else if(length(upper.case) > 1 && is.na(upper.case) == FALSE){
      output <- ifelse(upper.case, toupper(seq.revcomp), tolower(seq.revcomp))
    }
    output <- paste0(output, collapse = "")
    return(output)
  }




