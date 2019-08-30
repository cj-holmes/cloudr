#' Produce wordcloud
#'
#' @param words Character vector of words to use in word cloud
#' @param weights The weigths associated with each word - will be normalised and passed as the text cex
#' @param lower_cex Lower cex for lowest word weight
#' @param upper_cex Upper cex for highest word weight
#' @param buffer Buffer for plot region x dimension (npc units)
#' @param seed Seed for reproducabile plots
#' @param markup Logical. Add word borders, x-y coordinates and page boundaries/buffer zone
#' @param plot_spirals Logical. Choose whether to plott he spirals for each point on the page
#' @param word_buffer A buffer to be added to each word. Default is 0. Negative values will make words plot over eachother
#' @param lo_col Colour for lowest word weight (a continuos colour scale from lo to hi will be constructed)
#' @param hi_col Colour for highest word weight (a continuos colour scale from lo to hi will be constructed)
#' @param spiral_n Number of full rotations in spiral
#' @param spiral_res Resilution of spirals
#' @param spiral_a0 Spiral start relative to x coordinate
#' @param spiral_a1 Spiral end
#' @param vert Logical. Use vertical words in wordcloud
#'
#' @return
#' @export
#' @import dplyr purrr tibble
#' @examples
#' wc(obama$word[1:100],
#'    obama$count[1:100],
#'    lower_cex = 1,
#'    upper_cex = 3.5,
#'    spiral_n = 10,
#'    spiral_res = 500,
#'    lo_col = "grey75",
#'    seed = 1)
wc <- function(words, weights,
               word_buffer = 0,
               lower_cex = 0.5, upper_cex = 2,
               buffer = 0.2,
               lo_col = "black",
               hi_col = "black",
               spiral_n = 20, spiral_res = 400, spiral_a0 = 0, spiral_a1 = 0.4,
               seed=NULL,
               vert=TRUE,
               plot_spirals = FALSE,
               markup = FALSE){

  # Set seed if one is provided
  if(!is.null(seed)) set.seed(seed)

  # Initialise an empty plot and set par()
  par(mai=rep(0.1, 4), xaxs="i", yaxs="i")
  plot(1, xlim=c(0, 1), ylim=c(0,1), type='n', asp=1, ann=FALSE, axes=FALSE)

  # Create dataframe of words, normalised cex values and word widths and heights
  # Generate random x-y coordinates for words and nest spiral coordinates for
  # each x-y coordinate
  # Then arrange by descending normalised weights
  words_df <-
    tibble(word = words, n = weights) %>%
    mutate(nn = (((n-min(n))/max(n-min(n)))*(upper_cex - lower_cex))+lower_cex,
           width = map2_dbl(word, nn, strwidth, units = "user") + word_buffer,
           height = map2_dbl(word, nn, strheight, units = "user") + word_buffer,
           x = runif(n, min = 0+buffer, max = 1-buffer),
           y = runif(n, min = 0.4, max = 0.6),
           spiral = map2(x, y, spiral, n=spiral_n, res=spiral_res, a0=spiral_a0, a1=spiral_a1)) %>%
    arrange(desc(nn)) %>%
    mutate(cols = grDevices::colorRampPalette(c(hi_col, lo_col))(length(words)))

  # plot all spirals
  if(plot_spirals){
    for(i in 1:nrow(words_df)){
      lines(words_df$spiral[[i]]$x, words_df$spiral[[i]]$y, col="grey80")
    }
  }

  # Initialise empty tibble ready to store the coordinates of each word as it placed
  # This is the tibble that will need to be tested against for collisions
  collision_df <- tibble(n=NA, x=NA, y=NA, word=NA, bottom=NA, left=NA, top=NA, right=NA)

  # Loop through each word finding where to print it and printing it
  pb <- txtProgressBar(max=nrow(words_df), style = 3)
  for(i in 1:nrow(words_df)){

    setTxtProgressBar(pb, i)

    # If this is the first word, add it to collision df and print it to device
    if(i == 1){
      collision_df[i, ] <- c(n = i,
                             x = words_df$spiral[[i]]$x[1],
                             y = words_df$spiral[[i]]$y[1],
                             word = words_df$word[i],
                             bottom = words_df$spiral[[i]]$y[1] - words_df$height[i]/2,
                             left = words_df$spiral[[i]]$x[1] - words_df$width[i]/2,
                             top = words_df$spiral[[i]]$y[1] + words_df$height[i]/2,
                             right = words_df$spiral[[i]]$x[1] + words_df$width[i]/2)

      text(words_df$spiral[[i]]$x[1],
           words_df$spiral[[i]]$y[1],
           label = words_df$word[i],
           cex = words_df$nn[i],
           col = words_df$cols[i])

      next
      }

    # Initialise collision variable as TRUE and spiral_index as 0 for each iteration of i
    # collision is used to keep moving the word around its spiral(sp_index) until it
    # finds a place it can fit
    # Initialise another variable that indicates if the current word couldnt be placed
    # (if it gets to the end of it's spiral and there is no place it fits)
    # Initialise a flag to state whether this word needs to be rotated
    collision <- TRUE
    sp_index <- 0
    not_placed <- FALSE
    use_rotated <- FALSE

    # Initialise a vector that is the length of how many words I need to test for a collision against
    # (That is the number of words alreday plotted, nrow(collision_df))
    # Remove NAs here as words that ar eunable to be plotted will be populated with an NA
    sub_collision_df <- collision_df[!is.na(collision_df$n),]
    collision_test_vector <- vector(length = nrow(sub_collision_df))

    ## TEST FOR COLLISIONS #################################################
    # While the word is continuing to collide, keep moving along it's spiral
    while(collision){

      # Increment the spiral index by 1
      sp_index <- sp_index + 1

      # Create a vector of the bounding rectangle for this iteration's word
      # At its current position on its spiral (sp_index)
      rect_coords <- c(bottom = words_df$spiral[[i]]$y[sp_index] - words_df$height[i]/2, # bottom
                       left   = words_df$spiral[[i]]$x[sp_index] - words_df$width[i]/2,  # left
                       top    = words_df$spiral[[i]]$y[sp_index] + words_df$height[i]/2, # top
                       right  = words_df$spiral[[i]]$x[sp_index] + words_df$width[i]/2   # right
                       )
      # And it's rectangle if it were rotated 90 degrees
      if(vert){
      rect_coords_r <- c(bottom = words_df$spiral[[i]]$y[sp_index] - words_df$width[i]/2, # bottom
                         left   = words_df$spiral[[i]]$x[sp_index] - words_df$height[i]/2,  # left
                         top    = words_df$spiral[[i]]$y[sp_index] + words_df$width[i]/2, # top
                         right  = words_df$spiral[[i]]$x[sp_index] + words_df$height[i]/2   # right
                         )
      }

      # Loop through every word in collision_df and test for a collision against the
      # current word trying to be printed at its current position on its spiral
      for(j in seq_along(collision_test_vector)){
        collision_test_vector[j] <-
          sub_collision_df$left[j]    < rect_coords['right'] &&
          sub_collision_df$right[j]   > rect_coords['left'] &&
          sub_collision_df$top[j]     > rect_coords['bottom'] &&
          sub_collision_df$bottom[j]  < rect_coords['top']

        if(collision_test_vector[j] == TRUE){
          # collision_df <-
          #   dplyr::bind_rows(sub_collision_df[j, ],
          #                    collision_df[!collision_df$n == sub_collision_df$n[j], ],
          #                    )
          break
          }
        }

      # If it doesn't collide with any previous word break out of while loop
      # Keeping the default value of use_rotated = FALSE
      if(all(collision_test_vector == FALSE)){break}

      if(vert){
      # Loop through every word in collision_df and test for a collision against the
      # current word trying to be printed at its current position on its spiral
      for(j in seq_along(collision_test_vector)){
        collision_test_vector[j] <-
          sub_collision_df$left[j]    < rect_coords_r['right'] &&
          sub_collision_df$right[j]   > rect_coords_r['left'] &&
          sub_collision_df$top[j]     > rect_coords_r['bottom'] &&
          sub_collision_df$bottom[j]  < rect_coords_r['top']

        if(collision_test_vector[j] == TRUE){
          break
          }
      }

      # If it doesn't collide with any previous word break out of while loop
      # change to use_rotated = TRUE
      if(all(collision_test_vector == FALSE)){
        use_rotated <- TRUE
        break
      }
        }
      # If this is the last value of sp_index (end of spiral) and it is still colliding
      # with another word, set not_placed to TRUE and then collision to FALSE so we end the while loop
      if(sp_index == spiral_res & any(collision_test_vector == TRUE)){
        not_placed <- TRUE
        collision <- FALSE
      }

      }

    # If word could not be placed on spiral skip to next iteration of i with a
    # warning message
    if(not_placed){
      message(paste0("Word ", i, " (", words_df$word[i], ") not placed"))
      next
    }

    # Add the current word to the collision dataframe
    if(use_rotated){
      collision_df[i, ] <- c(n = i,
                             x = words_df$spiral[[i]]$x[sp_index],
                             y = words_df$spiral[[i]]$y[sp_index],
                             word = words_df$word[i],
                             bottom = words_df$spiral[[i]]$y[sp_index] - words_df$width[i]/2, # bottom
                             left   = words_df$spiral[[i]]$x[sp_index] - words_df$height[i]/2,  # left
                             top    = words_df$spiral[[i]]$y[sp_index] + words_df$width[i]/2, # top
                             right  = words_df$spiral[[i]]$x[sp_index] + words_df$height[i]/2)   # right
    } else {
      collision_df[i, ] <- c(n = i,
                           x = words_df$spiral[[i]]$x[sp_index],
                           y = words_df$spiral[[i]]$y[sp_index],
                           word = words_df$word[i],
                           bottom = words_df$spiral[[i]]$y[sp_index] - words_df$height[i]/2,
                           left = words_df$spiral[[i]]$x[sp_index] - words_df$width[i]/2,
                           top = words_df$spiral[[i]]$y[sp_index] + words_df$height[i]/2,
                           right = words_df$spiral[[i]]$x[sp_index] + words_df$width[i]/2)
    }

    # Print the current word
    if(use_rotated){
      text(words_df$spiral[[i]]$x[sp_index],
           words_df$spiral[[i]]$y[sp_index],
           label = words_df$word[i],
           cex = words_df$nn[i],
           col = words_df$cols[i],
           srt=90)
      } else {
        text(words_df$spiral[[i]]$x[sp_index],
             words_df$spiral[[i]]$y[sp_index],
             label = words_df$word[i],
             cex = words_df$nn[i],
             col = words_df$cols[i])
        }


  }

  # Draw plot lines
  if(markup){

    abline(h = c(0,1), v = c(0,1), col = 2)
    abline(h = c(0 + buffer, 1 - buffer),
           v = c(0 + buffer, 1 - buffer),
           col = 2, lty = 2)

    for(i in 1:nrow(collision_df)){
    rect(ybottom = collision_df$bottom[i],
         xleft   = collision_df$left[i],
         ytop    = collision_df$top[i],
         xright  = collision_df$right[i],
         border  = 4)
    }

    for(i in 1:nrow(words_df)){
      points(words_df$x, words_df$y, col=6, pch=4)
    }


  }

  close(pb)

}
