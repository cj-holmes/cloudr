library(cloudr)
library(tidyr)
library(purrr)
library(dplyr)
library(tibble)

l <- c(LETTERS, letters, 0:9)

all_glyphs <-
  crossing(word = l, weight = 10:500) %>% 
  mutate(wm = purrr::map2(word, weight, word_mat, rot=0))


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
}

create_word("Howdy", 50) %>% plot_mat()
