#' Word matrix
#' 
#' A wrapper around fontr::glyph_bitmap(). After the matrix is extracted, it is 'unclassed' (back to an S3 matrix)
#' and then 0s (the background to the character glyphs) are set as NA
#'
#' @param word Character string
#' @param weight Word size to be passed to pixel_size
#' @param rot Word angle of rotation to be passed to rot
#' @param ... Further parameters to be passed to fontr::glyph_bitmap()
#'
#' @return
#' @importfrom fontr glyph_bitmap
#' @export
word_mat <- function(word, weight, rot, ...){
  
  m <- unclass(fontr::glyph_bitmap(ch=word, pixel_size = weight, rot=rot, ...))
  m[m == 0] <- NA
  m
}