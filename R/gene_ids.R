#' find_id
#'
#' find gene ids from annotation data
#'
#' @param symbol a string for a gene symbol or alias to match
#' @param annotation a list of gene symbols named by ids such as those provided by bioconductor AnnotationDbi packages
#' @param n number of ids to list (maximum). Defaults to first. Set to Inf to display all.
#' @keywords bioinformatics annotation bioconductor gene database
#' @seealso find_symbol
#' @export
#' @examples

#'
#' @export
#' @usage NULL
find_id <- function(symbol, annotation, n = 1){
  if(round(n) == n){
    n <- as.integer(n)
  } else {
    stop("n must be an integer")
  }
  ids <- names(annotation)[which(sapply(annotation, function(x) symbol %in% x) == TRUE)]
  return(ids[1:min(n, length(ids))])
}

