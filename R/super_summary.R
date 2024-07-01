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
#'
#' super_summary(attenu $ station, maxsum = 20) #-> super_summary.factor(...)
#'
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
#'
#' @export

super_summary <- function(x, ...) {
  UseMethod("super_summary")
}

#' @exportS3method

super_summary.default <- function(object, maxsum = 7, digits = max(3, getOption("digits")-3)){
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

#' @exportS3method

super_summary.factor <- function (object,  maxsum = 100L,
                                  digits = max(3, getOption("digits")-3),
                                  ...){
  nas <- is.na(object)
  ll <- levels(object)
  if (ana <- any(nas))
    maxsum <- maxsum - 1L
  tbl <- table(object)
  tt <- c(tbl)
  names(tt) <- dimnames(tbl)[[1L]]
  if (length(ll) > maxsum) {
    drop <- maxsum:length(ll)
    o <- sort.list(tt, decreasing = TRUE)
    tt <- c(tt[o[-drop]], `(Other)` = sum(tt[o[drop]]))
  }
  if (ana)
    c(tt, `NA's` = sum(nas))
  else tt
}

#' @exportS3method

super_summary.list <- function (object, ...){
  super_summary.data.frame(as.data.frame.matrix(object), ...)
}

#' @exportS3method

super_summary.matrix <- function (object, ...){
  super_summary.data.frame(as.data.frame(object), super_summary, ...)
}

#' @exportS3method

super_summary.data.frame <- function (object, maxsum = 7L,
                                      digits = max(3L, getOption("digits") - 3L)
                                      , ...){
  ncw <- function(x) {
    z <- nchar(x, type = "w", allowNA = TRUE)
    if (any(na <- is.na(z))) {
      z[na] <- nchar(encodeString(z[na]), "b")
    }
    z
  }
  z <- lapply(X = as.list(object), FUN = summary, maxsum = maxsum,
              digits = 12L, ...)
  nv <- length(object)
  nm <- names(object)
  lw <- numeric(nv)
  nr <- if (nv)
    max(vapply(z, function(x) NROW(x) + !is.null(attr(x,
                                                      "NAs")), integer(1)))
  else 0
  for (i in seq_len(nv)) {
    sms <- z[[i]]
    if (is.matrix(sms)) {
      cn <- paste(nm[i], gsub("^ +", "", colnames(sms),
                              useBytes = TRUE), sep = ".")
      tmp <- format(sms)
      if (nrow(sms) < nr)
        tmp <- rbind(tmp, matrix("", nr - nrow(sms),
                                 ncol(sms)))
      sms <- apply(tmp, 1L, function(x) paste(x, collapse = "  "))
      wid <- sapply(tmp[1L, ], nchar, type = "w")
      blanks <- paste(character(max(wid)), collapse = " ")
      wcn <- ncw(cn)
      pad0 <- floor((wid - wcn)/2)
      pad1 <- wid - wcn - pad0
      cn <- paste0(substring(blanks, 1L, pad0), cn, substring(blanks,
                                                              1L, pad1))
      nm[i] <- paste(cn, collapse = "  ")
    }
    else {
      sms <- format(sms, digits = digits)
      lbs <- format(names(sms))
      sms <- paste0(lbs, ":", sms, "  ")
      lw[i] <- ncw(lbs[1L])
      length(sms) <- nr
    }
    z[[i]] <- sms
  }
  if (nv) {
    z <- unlist(z, use.names = TRUE)
    dim(z) <- c(nr, nv)
    if (anyNA(lw))
      warning("probably wrong encoding in names(.) of column ",
              paste(which(is.na(lw)), collapse = ", "))
    blanks <- paste(character(max(lw, na.rm = TRUE) + 2L),
                    collapse = " ")
    pad <- floor(lw - ncw(nm)/2)
    nm <- paste0(substring(blanks, 1, pad), nm)
    dimnames(z) <- list(rep.int("", nr), nm)
  }
  else {
    z <- character()
    dim(z) <- c(nr, nv)
  }
  modes <- sapply(object, calculate_mode)
  modes <- signif(modes, digits = digits)
  modes <- gsub("^", "Mode   : ", modes)
  z <- rbind(z, modes)
  num_low_outliers <- sapply(object, function(element){
    element <- as.numeric(element)
    length(element[element < mean(element, na.rm = TRUE) - 3 * sd(element, na.rm = TRUE)])
  })
  num_low_outliers <- gsub("^", "ExtLow : ",  as.character(num_low_outliers))
  z <- rbind(z, num_low_outliers)
  num_high_outliers <- sapply(object, function(element){
    element <- as.numeric(element)
    length(element[element > mean(element, na.rm = TRUE) + 3 * sd(element, na.rm = TRUE)])
  })
  num_high_outliers <- gsub("^", "ExtHigh: ",  num_high_outliers)
  z <- rbind(z, num_high_outliers)
  rownames(z) <- rep("", nrow(z))

  attr(z, "class") <- c("table")
  z
}


#' @title Calculate Mode
#' @name calculate_mode
#' @rdname calculate_mode
#' @encoding UTF-8
#'
#' @description calculate_mode is a generic function to compute the most
#' abundant element in a list.
#'
#' @param x An R object. Currently there are methods for numeric/logical vectors
#'  and date, date-time and time interval objects. Complex vectors are allowed for trim = 0, only.
#' @param na.rm	a logical evaluating to TRUE or FALSE indicating whether NA
#' values should be stripped before the computation proceeds.
#' @param ...	further arguments passed to or from other methods.
#'
#' @returns the most frequent value.
#'
#' @references https://stackoverflow.com/a/8189441
#'
#' @seealso [mean()] [median()] [sd()] [summary()]
#'
#' @keywords super_summary statistics
#'
#' @usage calculate_mode(x, ...)
#'
#' ## Default S3 method:
#' calculate_mode(x, na.rm = FALSE, ...)

#' @export
calculate_mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }

  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}
