#' Image a matrix
#'
#' A wrapper around image() that plots a given matrix
#' 
#' @param m The matrix to plot
#' @param ... Further arguments passed to image()
#'
#' @return
#' @export
#'
#' @examples
plot_mat <- function(m, ...){
  
  w <- ncol(m)
  h <- nrow(m)
  
  graphics::image(1:w, 1:h, t(m[h:1, ]), 
                  col = grey((45:0) / 50), asp = 1, axes = FALSE, 
                  xlab = "", ylab = "", 
                  ...)
}