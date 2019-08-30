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

  tibble(x = (a0 + (b*l))*cos(l) + x,
         y = (a0 + (b*l))*sin(l) + y)
}
