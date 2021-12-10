#' @title plot_wrapper
#' @name plot_wrapper
#' @rdname plot_wrapper
#'
#' @description  plot with legend and title, mtext added
#'
#' @param ... parameters to pass to plot
#' @param legend plot legend
#' @param title plot title
#' @param mtext plot mtext to add
#' @keywords plot graphics annotation title legend
#' @importFrom graphics plot
#' @examples
#' # example plot
#' n <- 5
#' x_mean <- rnorm(n, 1000, 500)
#' y_mean <- rnorm(n, 1000, 500)
#' x_vals <- sapply(x_mean, function(x) rnorm(100, x, 50))
#' y_vals <- sapply(y_mean, function(y) rnorm(100, y, 50))
#' plot(x_vals, y_vals, col = colourscale(n, brewer.pal(11, "Set3"))[unlist(lapply(1:n, function(x) rep(x, 100)))], pch = 20,
#'      legend = legend("topleft", title = "colours", legend = 1:n, col = colourscale(n, brewer.pal(11, "Set3")), pch = 20),
#'      main = "title", xlab = "x-axis", ylab = "y-axis")
#'
#' //@export
#' @usage NULL

plot <- function(..., legend = NULL, title = NULL, mtext = NULL, facet = NULL){
  graphics::plot(...)
  if(!is.null(legend)) legend
  if(!is.null(mtext))  mtext
  # if(!is.null(title)){
  #   title
  #   graphics::plot(..., main = NULL, xlab = " ", ylab = " ")
  # }  else {
  #  graphics::plot(...)
  #}
}
