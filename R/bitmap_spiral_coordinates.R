#' Bitmap spiral coordinates
#'
#' Generate a tibble of spiral cartesian coordinates as either a square spiral or a
#' diamond spiral (a square spiral rotated by 45 degrees) All coordinates returned are
#' integers
#'
#' @param x1 Spiral origin (x coordinate)
#' @param y1 Spiral origin (y coordinate)
#' @param radius Desired radius of bitmap spiral
#' @param res Resolution of spiral (step length)
#' @param type 1 = square, 2 = diamond
#'
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#' # Define radius and resolution
#' rad <- 1000
#' res <- 57
#'
#' # Plot both bitmap spirals
#' bitmap_spiral(0, 0, radius = rad, res = res, type = 1) %>%
#'   mutate(type = "square") %>%
#'   bind_rows(bitmap_spiral(0, 0, radius = rad, res = res, type = 2) %>%
#'               mutate(type = "diamond")) %>%
#'   group_by(type) %>%
#'   mutate(xend = lead(x), yend=lead(y)) %>%
#'   ggplot(aes(x=x, y=y, xend=xend, yend=yend, col=type))+
#'   geom_segment()+
#'   geom_point(aes(x,y))+
#'   coord_equal()+
#'   theme_gray()

bitmap_spiral <- function(x1, y1, radius, res, type=sample(c(1,2), 1)){
  
  if(type == 1){
    # Square
    # Define length parameter for given radius and resolution
    l <- round((radius*4)/res)
    
    r_x <- rep(c(0, -res, 0, res), length.out = l)
    r_y <- rep(c(res, 0, -res, 0), length.out = l)
    ind <- (1:l) - floor((1:l)/2)
    
    x <- cumsum(rep(r_x, ind))
    y <- cumsum(rep(r_y, ind))
    
    return(tibble::tibble(x=c(x1, x1+x), y=c(y1, y1+y)))
    
    }
  
  if(type == 2){
    # Diamond
    # Define length parameter for given radius and resolution
    l <- round((radius*2.5)/res/2.5)
    
    r_x <- rep(c(res, -res), length.out = l)
    x_ind <- seq(1, by = 2, l = l)
    
    r_y <- rep(c(res, -res), length.out = l)
    y_ind <- seq(2, by = 2, l = l)
    
    x <- cumsum(rep(r_x, x_ind))
    y <- cumsum(rep(r_y, y_ind))
    y <- y[1:length(x)]
    
    return(tibble::tibble(x=c(x1, x1+x), y=c(y1, y1+y)))
  }
}

#' Add a spiral to a plot
#'
#' @param spiral as made by bitmap_spiral()
#' @param ... Further arguments passed to graphics::segments()
#'
#' @return
#' @export
#'
#' @examples
plot_spiral <- function(spiral, ...){
  segments(x0=spiral$x, x1 = lead(spiral$x), y0=spiral$y, y1=lead(spiral$y), ...)
}



#' Spiral x-y coordinates
#'
#' Parametereised spiral in cartesian coordinates
#'
#' @param x Centre of whole spiral (x)
#' @param y Centre of whole spiral (y)
#' @param n Number of full rotations of spiral
#' @param res Number of points to generate on spiral
#' @param a0 Radius start of spiral
#' @param a1 Radius end of spiral
#'
#' @return
#' @export
#'
#' @examples
#' sp <- spiral(x=0, y=0, n=5, res=500, a0=0.5, a1=3)
#' plot(sp$x, sp$y, type="l", asp=1)

spiral <- function(x, y, n, res, a0, a1){
  
  b <- (a1 - a0)/(2*pi*n)
  l <- seq(0, 2*n*pi, l=res)
  
  tibble(x = as.integer((a0 + (b*l))*cos(l) + x),
         y = as.integer((a0 + (b*l))*sin(l) + y))
}
