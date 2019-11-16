#' Coordinate transform from word to page
#' 
#' Given a word matrix and its starting position on the page, this funtion
#' returns the rows and columns of the page matrix that will be occupied by the word matrix. The
#' page rowas and columns are calculated from placing the centre of the word matrix at the given
#' starting coordinates
#'
#' @param px Page starting x-coordinate of word
#' @param py Page starting y-coordinate of word
#' @param word_m Word matrix
#'
#' @return
#' @export
coord_trans <- function(px, py, word_m){
  
  height <- nrow(word_m)
  width  <- ncol(word_m)
  
  # px and py become bottom left of word
  # rows <- (py - height + 1) : py
  # cols <- px : (px + width - 1)
  
  # px and py become the center of the word
  rows <- floor(py - height/2 + 1) : floor(py + height/2)
  cols <- floor(px - width/2 + 1) : floor(px + width/2)
  
  list(r=rows, c=cols)
}