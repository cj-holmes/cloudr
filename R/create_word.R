#' Create a word from the all_glyphs data
#'
#' @param word word to create
#' @param weight pixel size
#'
#' @return a word matrix
#' @export
#'
#' @examples
#' create_word("howdy", 50) %>% plot_mat()
create_word <- function(word, weight){
  
  spacer <- round(weight/7.5)
  
  d <-
    tibble::tibble(letters = strsplit(word, "") %>% unlist()) %>%
    mutate(glyphs = map2(letters, weight, ~all_glyphs$wm[all_glyphs$word==.x & all_glyphs$weight==.y][[1]]),
           nc = map_int(glyphs, ncol),
           nr = map_int(glyphs, nrow),
           add_rows = max(nr) - nr,
           rb = pmap(list(glyphs, nc, add_rows), ~rbind(matrix(NA, nrow=..3, ncol=..2), ..1)),
           cb = map(rb, ~cbind(.x, matrix(NA, nrow=nrow(.x), ncol=spacer)))
    )
  
  do.call(cbind, d$cb)
  
  # TODO: doesnt work for letters with underhangs at the moment
  # need to think of a way of solving that
}

