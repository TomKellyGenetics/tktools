#' @title gg_colour_hue
#' @name gg_colour_hue
#' @rdname gg_colour_hue
#'
#' @description  emulate colour scale for ggplots
#'
#' @param n the number of colors (≥ 1) to be in the palette.
#' @keywords plot graphics annotation title legend
#' @importFrom graphics plot
#' @export
#' @examples
#'
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
#' @usage NULL
#' @export

gg_colour_hue <- function(n){
  if(round(n) == n){
    n <- as.integer(n)
  } else {
    stop("n must be an integer")
  }
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


#' @rdname gg_colour_hue
#' @export
gg_color_hue <- function(n, col = NULL, ...) {
  gg_colour_hue
}



