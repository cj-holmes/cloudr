#' Add buffer to word 
#' 
#' Add a simplified buffer to the extremites of a word matrix
#'
#' @param x The word matrix
#' @param buffer The buffer ammount in pixles
#' @param fill The value to fill the buffer with
#'
#' @return A matrix with buffer applied
#' @export
#'
#' @examples
add_buffer <- function(x, buffer=5, fill=0){
  
  # First start by expanding the word matrix on all sides by the buffer ammount
  x <- rbind(matrix(NA, nrow=buffer+1, ncol=ncol(x)),
             x, 
             matrix(NA, nrow=buffer+1, ncol=ncol(x)))
  
  x <- cbind(matrix(NA, nrow=nrow(x), ncol=buffer+1),
             x,
             matrix(NA, nrow=nrow(x), ncol=buffer+1))
  
  mid_point <- floor(ncol(x)/2)
  
  # plot_mat(x)
  # abline(v=mid_point)

  
  
  # Top and bottom buffer on left half of word
  first_left_non_na_row <- which(apply(x[,1:mid_point], 1, function(x) any(!is.na(x))))[1]
  # abline(h=nrow(x) - first_left_non_na_row + 1)
  last_left_non_na_row <- rev(which(apply(x[,1:mid_point], 1, function(x) any(!is.na(x)))))[1]
  # abline(h=nrow(x) - last_left_non_na_row + 1)
  
  # Top and bottom buffer on right half of word
  first_right_non_na_row <- which(apply(x[, (mid_point+1):ncol(x)], 1, function(x) any(!is.na(x))))[1]
  # abline(h=nrow(x) - first_right_non_na_row + 1)
  last_right_non_na_row <- rev(which(apply(x[, (mid_point+1):ncol(x)], 1, function(x) any(!is.na(x)))))[1]
  # abline(h=nrow(x) - last_right_non_na_row + 1)
  
  # Left and right buffer on whole word
  first_non_na_col <- which(apply(x, 2, function(x) any(!is.na(x))))[1]
  # abline(v=first_non_na_col)
  last_non_na_col <- rev(which(apply(x, 2, function(x) any(!is.na(x)))))[1]
  # abline(v=last_non_na_col)
  
  # Add buffer top and bottom (left side)
  first_cols_not_na <- which(!is.na(x[first_left_non_na_row, 1:mid_point]))
  # abline(v=first_cols_not_na, col=4)
  last_cols_not_na <- which(!is.na(x[last_left_non_na_row, 1:mid_point]))
  # abline(v=last_cols_not_na, col=2)
  
  x[(first_left_non_na_row-1-buffer):(first_left_non_na_row-1), first_cols_not_na] <- fill
  x[(last_left_non_na_row+1):(last_left_non_na_row+1+buffer), last_cols_not_na] <- fill
  # plot_mat(x)

  
  # Add buffer top and bottom (right side)
  first_cols_not_na <- mid_point + which(!is.na(x[first_right_non_na_row, (mid_point+1):ncol(x)]))
  # plot_mat(x)
  # abline(v=first_cols_not_na, col=2)
  last_cols_not_na <- mid_point + which(!is.na(x[last_right_non_na_row,(mid_point+1):ncol(x)]))
  # abline(v=last_cols_not_na, col=4)
  
  x[(first_right_non_na_row-1-buffer):(first_right_non_na_row-1), first_cols_not_na] <- fill
  # plot_mat(x)
  x[(last_right_non_na_row+1):(last_right_non_na_row+1+buffer), last_cols_not_na] <- fill
  # plot_mat(x)
  
  # Add buffer left and right
  first_rows_not_na <- which(!is.na(x[, first_non_na_col]))
  last_rows_not_na <- which(!is.na(x[, last_non_na_col]))
  x[first_rows_not_na, 1:(first_non_na_col-1)] <- fill
  x[last_rows_not_na, (last_non_na_col+1):ncol(x)] <- fill
  
  x
}