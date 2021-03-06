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
#' #load data to identify genes from IDs
#' if(!require("org.Hs.eg.db")){
#'   install.packages("BiocManager")
#'   BiocManager::install("org.Hs.eg.db")
#'   library("org.Hs.eg.db")
#' }
#' annotation <- as.list(org.Hs.egSYMBOL)
#' gene <- "7157"
#' # Entrez ID from human symbol
#' find_symbol(gene, annotation)
#'
#' #Ensembl ID from human symbol
#' annotation <-  as.list(org.Hs.egSYMBOL)
#' names(annotation) <- as.list(org.Hs.egENSEMBL)
#' gene <- "ENSG00000141510"
#' find_symbol(gene, annotation)
#'
#' #Full gene name from Ensembl ID
#' annotation <- as.list(org.Hs.egGENENAME)
#' names(annotation) <- as.list(org.Hs.egENSEMBL)
#' find_symbol("ENSG00000171428", annotation)
#'
#' #load data to identify genes from IDs
#' if(!require("org.Mm.eg.db")){
#'   install.packages("BiocManager")
#'   BiocManager::install("org.Mm.eg.db")
#'   library("org.Mm.eg.db")
#' }
#' annotation <- as.list(org.Mm.egSYMBOL)
#' gene <- "12494"
#' # Entrez ID from mouse symbol
#' find_symbol(gene, annotation)
#'
#' #Ensembl ID from mouse symbol
#' annotation <- as.list(org.Mm.egSYMBOL)
#' names(annotation) <- as.list(org.Mm.egENSEMBL)
#' gene <- "ENSMUSG00000029084"
#' find_symbol(gene, annotation)
#'
#' #load data to identify genes from IDs
#' if(!require("org.Hs.eg.db")){
#'   install.packages("BiocManager")
#'   BiocManager::install("org.Hs.eg.db")
#'   library("org.Hs.eg.db")
#' }
#' annotation <- as.list(org.Hs.egSYMBOL)
#' gene <- "TP53"
#' # Entrez ID from human symbol
#' find_id(gene, annotation)
#'
#' #Ensembl ID from human symbol
#' annotation <- as.list(org.Hs.egSYMBOL)
#' names(annotation) <- as.list(org.Hs.egENSEMBL)
#' gene <- "TP53"
#' find_id(gene, annotation)
#'
#' #load data to identify genes from IDs
#' if(!require("org.Mm.eg.db")){
#'   install.packages("BiocManager")
#'   BiocManager::install("org.Mm.eg.db")
#'   library("org.Mm.eg.db")
#' }
#' annotation <- as.list(org.Mm.egSYMBOL)
#' gene <- "Cd38"
#' # Entrez ID from mouse symbol
#' find_id(gene, annotation)
#'
#' #Ensembl ID from mouse symbol
#' annotation <- as.list(org.Mm.egSYMBOL)
#' names(annotation) <- as.list(org.Mm.egENSEMBL)
#' gene <- "Cd38"
#' find_id(gene, annotation)
#'
#' #load data to identify genes from IDs
#' if(!require("org.At.tair.db")){
#'   install.packages("BiocManager")
#'   BiocManager::install("org.At.tair.db")
#'   library("org.At.tair.db")
#' }
#' annotation <- as.list(org.At.tairSYMBOL)
#' gene <- "SCR"
#' find_id(gene, annotation)
#'
#' genes <- c("WOX5", "WUS")
#' sapply(genes, find_id, annotation)
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

#' find_symbol
#'
#' find gene symbol from annotation data
#'
#' @param id a string for a gene id to match to gene symbol or aliases
#' @param annotation a list of gene symbols named by ids such as those provided by bioconductor AnnotationDbi packages
#' @param n number of symbols to list (maximum). Defaults to first. Set to Inf to display all.
#' @keywords bioinformatics annotation bioconductor gene database
#' @seealso find_symbol
#' @export
#' @examples
#'
#' #load data to identify genes from IDs
#' if(!require("org.At.tair.db")){
#'   install.packages("BiocManager")
#'   BiocManager::install("org.At.tair.db")
#'   library("org.At.tair.db")
#' }
#' annotation <- as.list(org.At.tairSYMBOL)
#' id <- "AT3G11260"
#' find_symbol(id, annotation)
#'
#' #
#' ids <- c("AT3G11260", "AT2G17950")
#' lapply(ids, find_symbol, annotation)
#'
#' # show multiple gene symbols
#' lapply(ids, find_symbol, annotation, n = 2)
#' # show all gene symbols
#' lapply(ids, find_symbol, annotation, n = Inf)
#'
#' @export
#' @usage NULL
find_symbol <- function(id, annotation, n = 1){
  if (is.infinite(n)){
    n <- Inf
  } else if(round(n) == n){
    n <- as.integer(n)
  } else {
    stop("n must be an integer")
  }
  symbols <- annotation[id]
  return(symbols[id][[1]][1:min(n, length(symbols[[1]]))])
}

