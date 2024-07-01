#' @title Enhanced Object Summaries
#' @name super_summary
#' @rdname super_summary
#' @encoding UTF-8
#'
#' @description  super_summary is a generic function used to produce result summaries
#' of the results of various model fitting functions. The function invokes
#' particular methods which depend on the class of the first argument.
#'
#' This version adds addition features for convenient interpretation with
#' all-in-one super_summary statistics.
#'
#' @param object an object for which a super_summary is desired.
#' @param x a result of the default method of super_summary().
#' @param maxsum integer, indicating how many levels should be shown for factors.
#' @param digits integer, used for number formatting with signif()
#' (for super_summary.default) or format() (for super_summary.data.frame).
#' In super_summary.default, if not specified (i.e., missing(.)), signif() will not be
#' called anymore (since R >= 3.4.0, where the default has been changed to only
#' round in the print and format methods).
#' @param quantile.type	integer code used in quantile(*, type=quantile.type)
#' for the default method.
#' @param ... additional arguments affecting the super_summary produced.
#'
#' @details For factors, the frequency of the first maxsum - 1 most frequent
#' levels is shown, and the less frequent levels are summarized in "(Others)"
#' (resulting in at most maxsum frequencies). The functions super_summary.lm and
#'  super_summary.glm are examples of particular methods which summarize the
#'  results produced by lm and glm.
#'
#' @returns The form of the value returned by super_summary depends on the class of
#' its argument. See the documentation of the particular methods for details of
#' what is produced by that method.
#'
#' The default method returns an object of class c("summaryDefault", "table")
#'  which has specialized format and print methods. The factor method returns
#'  an integer vector.
#'
#' The matrix and data frame methods return a matrix of class "table",
#' obtained by applying super_summary to each column and collating the results.
#'
#' @references Chambers, J. M. and Hastie, T. J. (1992)
#' Statistical Models in S. Wadsworth & Brooks/Cole.
#'
#' @seealso [anova()] [summary()] [summary.glm()] [summary.lm()]
#' [sd()]  [summary.glm()]
#'
#' @keywords super_summary statistics
#' @examples
#' super_summary(attenu, digits = 4) #-> super_summary.data.frame(...), default precision
#' super_summary(attenu $ station, maxsum = 20) #-> super_summary.factor(...)
#' lst <- unclass(attenu$station) > 20 # logical with NAs
#' ## super_summary.default() for logicals -- different from *.factor:
#' super_summary(lst)
#' super_summary(as.factor(lst))
#'
#' @export
#' @usage super_summary(object, ...)
#'
#' ## Default S3 method:
#' super_summary(object, ..., digits, quantile.type = 7)
#' ## S3 method for class 'data.frame'
#' super_summary(object, maxsum = 7,
#'         digits = max(3, getOption("digits")-3), ...)
#'
#' ## S3 method for class 'factor'
#' super_summary(object, maxsum = 100, ...)
#'
#' ## S3 method for class 'matrix'
#' super_summary(object, ...)
#'
#' ## S3 method for class 'summaryDefault'
#' format(x, digits = max(3L, getOption("digits") - 3L), ...)
#' ## S3 method for class 'summaryDefault'
#' print(x, digits = max(3L, getOption("digits") - 3L), ...)

super_summary <- function(x, ...) {
  UseMethod("super_summary")
}


super_summary.default <- function(object, digits = max(3, getOption("digits")-3)){
  object <- na.omit(object)
  num_low_outliers <- length(object[object < mean(object, na.rm = TRUE) - 3 * sd(object, na.rm = TRUE)])
  num_high_outliers <- length(object[object > mean(object, na.rm = TRUE) + 3 * sd(object, na.rm = TRUE)])
  mode <- calculate_mode(object)

  mylist <- format(summary(na.omit(as.numeric(object))), digits = digits)
  mylist[[7]] <- as.numeric(mode)
  mylist[[8]] <- num_low_outliers
  mylist[[9]] <- num_high_outliers
  names(mylist)[7:9] <- c("Mode", "Extreme Low", "Extreme High")
  class(mylist) <- c("summaryDefault", "table")
  mylist
}


calculate_mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }

  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
