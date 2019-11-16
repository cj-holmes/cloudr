#' Bitmap spiral coordinates
#' 
#' Generate a tibble of spiral cartesian coordinates as either a square spiral or a 
#' diamond spiral (a square spiral rotated by 45 degrees) All coordiabtes returned are
#' integers
#'
#' @param x1 Starting x position 
#' @param y1 Starting y position
#' @param step Step length of spiral (distance before turning a right angle)
#' @param l  Length of spiral (how many straight lines of length step) every 4 l gets a new layer of the spiral
#' @param type 1 = square, 2 = diamond
#'
#' @return
#' @export
#'
#' @examples
#' library(tidyverse)
#' # spiral_coords(0, 0, l=1, step = 1) %>%
#' #     mutate(xend = lead(x),
#' #            yend=lead(y)) %>%
#' #     ggplot(aes(x=x, y=y, xend=xend, yend=yend))+
#' #     geom_segment()+
#' #     geom_point(aes(x=0, y=0), col=2, size=2)+
#' #     coord_equal()
bitmap_spiral <- function(x1, y1, step = 10, l = 100, type=sample(c(1,2), 1)){
  
  if(type == 1){
    r_x <- rep(c(0, -step, 0, step), length.out = l)
    r_y <- rep(c(step, 0, -step, 0), length.out = l)
    ind <- (1:l) - floor((1:l)/2)
    
    x <- cumsum(rep(r_x, ind))
    y <- cumsum(rep(r_y, ind))
    
    return(tibble::tibble(x=c(x1, x1+x), y=c(y1, y1+y)))
  }
  
  if(type == 2){
    r_x <- rep(c(step, -step), length.out = l)
    x_ind <- seq(1, by = 2, l = l)
    
    r_y <- rep(c(step, -step), length.out = l)
    y_ind <- seq(2, by = 2, l = l)
    
    x <- cumsum(rep(r_x, x_ind))
    y <- cumsum(rep(r_y, y_ind))
    y <- y[1:length(x)]
    
    return(tibble::tibble(x=c(x1, x1+x), y=c(y1, y1+y)))
  }
  
}