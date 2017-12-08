## Overrides ggplot2 geom_step and adds 
geom_step <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", direction = "hv",
                      na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomStepHalf,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      direction = direction,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
#' @include geom-path.r
GeomStep <- ggproto("GeomStep", GeomPath,
                    draw_panel = function(data, panel_params, coord, direction = "half") {
                      data <- plyr::ddply(data, "group", stairstep, direction = direction)
                      GeomPath$draw_panel(data, panel_params, coord)
                    }
)

# Calculate stairsteps
# Used by [geom_step()]
#
# @keyword internal
stairstep <- function(data, direction="half") {
  direction <- match.arg(direction, c("hv", "vh", "half"))
  data <- as.data.frame(data)[order(data$x), ]
  n <- nrow(data)
  
  if (n <= 1) {
    # Need at least one observation
    return(data[0, , drop = FALSE])
  }
  
  if (direction == "vh") {
    xs <- rep(1:n, each = 2)[-2*n]
    ys <- c(1, rep(2:n, each = 2))
  } else if (direction == "hv") {
    ys <- rep(1:n, each = 2)[-2*n]
    xs <- c(1, rep(2:n, each = 2))
  } else {
    ys <- c(rep(1:n, each = 2)[-2*n],2*n+1)
    xs <- c(1, rep(2:n, each = 2),2*n)
  }
  
  if (direction == "half") {
    gap <- (data$x[2]-data$x[1])/2
    
    dfX <- c(data$x[1],data$x[xs[-1]]-gap)
    dfX[2*n] <- data$x[n]
    
    dfY <- data$y[ys]
    dfY[2*n] <- data$y[n]
    
    data[xs, setdiff(names(data), 
                     c("x", "y"))][2*n,4:8] <- 
      data[xs, setdiff(names(data), c("x", "y"))][1,4:8]
    
    data.frame(
      x = dfX,
      y = dfY,
      data[xs, setdiff(names(data), c("x", "y"))]
    )
  } else {
  
  data.frame(
    x = data$x[xs],
    y = data$y[ys],
    data[xs, setdiff(names(data), c("x", "y"))]
  )
  }
    
}