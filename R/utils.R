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
