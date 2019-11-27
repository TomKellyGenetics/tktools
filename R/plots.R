#' colorscale
#'
#' generate a colour scale for plotting scales with a variable number states.
#'
#' @param n number of colours to generate (integer or numeric)
#' @param col,colour,color scale of colours to interpolate
#' @keywords plot graphics colour color
#' @importFrom grDevices colorRampPalette
#' @export
#' @examples
#'
#' # generate colour scale
#' library("RColorBrewer")
#' colours <- brewer.pal(11, "Set3")
#' n <- 3
#' scale <- colourscale(n, colours)
#' # example plot
#' x_mean <- rnorm(n, 1000, 500)
#' y_mean <- rnorm(n, 1000, 500)
#' x_vals <- sapply(x_mean, function(x) rnorm(100, x, 50))
#' y_vals <- sapply(y_mean, function(y) rnorm(100, y, 50))
#' plot(x_vals, y_vals, col = scale[unlist(lapply(1:n, function(x) rep(x, 100)))], pch = 20)
#' legend("topleft", legend = 1:n, col = scale, pch = 20)
#'
#' # generate colour scale
#' library("RColorBrewer")
#' colours <- brewer.pal(11, "Set3")
#' n <- 15
#' scale <- colourscale(n, colours)
#' # example plot
#' x_mean <- rnorm(n, 1000, 500)
#' y_mean <- rnorm(n, 1000, 500)
#' x_vals <- sapply(x_mean, function(x) rnorm(100, x, 50))
#' y_vals <- sapply(y_mean, function(y) rnorm(100, y, 50))
#' plot(x_vals, y_vals, col = scale[unlist(lapply(1:n, function(x) rep(x, 100)))], pch = 20)
#' legend("topleft", legend = 1:n, col = scale, pch = 20)
#'
#' @export
#' @usage NULL
colourscale <- function(n, col = NULL, ...) {
  UseMethod("colourscale")
}

colorscale <- function(n, col = NULL, ...) {
  UseMethod("colourscale")
}

#' @rdname colourscale
#' @export
colourscale.factor <-
  function(factor, col = NULL, colours = NULL, colors = NULL)
  {
    n <- length(levels(factor))
    scale <- colourscale(n, col = col, colours = colours, colors = colors)
    return(scale)
  }

#' @rdname colourscale
#' @export
colourscale.character <-
  function(character, col = NULL, colours = NULL, colors = NULL)
  {
    factor <- as.factor(character)
    scale <- colourscale(factor, col = col, colours = colours, colors = colors)
    return(scale)
  }

#' @rdname colourscale
#' @export
colourscale.numeric <-
  function(vector, col = NULL, colours = NULL, colors = NULL)
  {
    if(is.numeric(vector)){
      if(length(vector) > 1){
        factor <- as.factor(vector)
        scale <- colourscale(factor, col = col, colours = colours, colors = colors)
      } else {
        n <- as.integer(vector)
        scale <- colourscale.default(n, col = col, colours = colours, colors = colors)
      }
    }
    return(scale)
}

#' @rdname colourscale
#' @export
colourscale.default <-
  function(n, col = NULL, colours = NULL, colors = NULL)
  {
    if(sum(c(!is.null(col),!is.null(colours),!is.null(colors))) > 1) warning("specify colour only once")
    if(is.null(col) && is.null(colours) && is.null(colors)) warning("specify colour at least once")
    if(!is.null(colours) && is.null(colors)) colors <- colours
    if(!is.null(colors) && is.null(colours)) colours <- colors
    if(is.null(col)) col <- colors

    if(is.numeric(n)) n <- as.integer(n)
    if(!is.integer(n)) stop("specific a valid number of colours to generate")

    scale <- colorRampPalette(col)(n)
    return(scale)
  }


#' plot
#'
#' plot with legend and title, mtext added
#'
#' @param ... parameters to pass to plot
#' @param legend plot legend
#' @param title plot title
#' @param mtext plot mtext to add
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
#' @export
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

#' gg_color_hue
#'
#' emulate colour scale for ggplots
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
#' @export
#' @usage NULL


gg_color_hue <- function(n){
  if(round(n) == n){
    n <- as.integer(n)
  } else {
    stop("n must be an integer")
  }
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

