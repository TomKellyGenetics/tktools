#' colorscale
#'
#' generate a colour scale for plotting scales with a variable number states.
#'
#' @param n number of colours to generate (integer or numeric)
#' @param col = NULL,colour,color scale of colours to interpolate
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

## plot with legend and title, mtext
