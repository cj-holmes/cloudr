library(cloudr)
library(tidyr)
library(purrr)
library(dplyr)

# Only for lowercase letters at the moment
all_glyphs <-
  crossing(word = letters, weight = 50:500) %>%
  mutate(wm = map2(word, weight, word_mat, rot=0))

usethis::use_data(all_glyphs, overwrite = TRUE)