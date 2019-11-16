#' Bitmap wordcloud
#'
#' @param data 
#' @param min_size 
#' @param max_size 
#' @param angle_range 
#' @param buffer 
#' @param show_word_buffer
#' @param view 
#' @param save 
#' @param filename 
#' @param font_face 
#' @param font_family 
#' @param seed 
#' @param spiral_length 
#' @param spiral_step 
#' @param page_res 
#' @param v_buffer 
#' @param h_buffer 
#'
#' @return
#' @export
#' 
#' @import dplyr purrr
#'
#' @examples
bmwc <- function(data,
                 min_size = 10, 
                 max_size = 200,
                 angle_range=c(0,0),
                 buffer = 5,
                 show_word_buffer = FALSE,
                 view = TRUE, 
                 save = FALSE,
                 filename,
                 font_face = "regular",
                 font_family = "sans",
                 seed = NULL, 
                 spiral_length = 50, # spiral will extend to ~length/4 in all directions (radius ~L/4)
                 spiral_step = 50,
                 page_res = 600, 
                 # Vertical and horizontal buffers (%)
                 v_buffer = 40,
                 h_buffer = 30){
  
  # data=cloudr::obama[1:300,]
  # page_res = 300 
  # min_size = 10 
  # max_size = 200
  # # Vertical and horizontal buffers (%)
  # v_buffer = 45
  # h_buffer = 20
  # font_face = "regular"
  # font_family = "sans"
  # spiral_length = 50 # spiral will extend to ~length/4 in all directions (radius ~L/4)
  # spiral_step = 50
  # seed = NULL 
  # view=TRUE 
  # save=TRUE
  # pcnt_vertical = 0.2
  
  # Set seed if given (for reproducability)
  if(!is.null(seed)){set.seed(seed)}
  
  # Set overall page height and width (A4 dimensions in inches * resolution)
  page_w <- floor(8.268 * page_res)
  page_h <- floor(5.825 * page_res)
  
  # Initialise page
  page <- matrix(NA, ncol=page_w, nrow=page_h)
  
  
  d <- 
    data %>% 
    mutate(weight = (((count-min(count))/max(count-min(count)))*(max_size - min_size))+min_size) %>%
    arrange(desc(weight))
  
  # Add word matrices from fontr (some at 90 degrees (vertical))
  # d$word_mat <- pmap(list(d$word, 
  #                         d$weight, 
  #                         sample(c(0, 90), size = nrow(d), 
  #                                prob = c((1-pcnt_vertical), pcnt_vertical), 
  #                                replace = TRUE)),
  #                         word_mat, face=font_face, family=font_family)
  
  d$word_mat <- pmap(list(d$word, 
                          d$weight, 
                          sample(angle_range[1]:angle_range[2], size = nrow(d), replace = TRUE)),
                     word_mat, face=font_face, family=font_family)
  
  
  h_bounds <- floor(c(1+ (page_w/100)*h_buffer, page_w-(page_w/100)*h_buffer))
  v_bounds <- floor(c(1+(page_h/100)*v_buffer, page_h - (page_h/100)*v_buffer))
  
  d <- 
    d %>% 
    mutate(x1 = runif(nrow(d), h_bounds[1], h_bounds[2]),
           y1 = runif(nrow(d), v_bounds[1], v_bounds[2]),
           spiral = map2(x1, y1, bitmap_spiral, l=spiral_length, step = spiral_step))
  
  
  # Add buffer to the words
  print("Adding buffer")
  d <-
    d %>% mutate(word_mat = map(word_mat, add_buffer, buffer=buffer))
  
  # Initialise progress bar
  pb <- txtProgressBar(max=nrow(d), style = 3)
  
  # Initialise vector for words that are not placed
  np <- rep(NA, nrow(d))
  
  for(i in 1:nrow(d)){
    
    # print(paste0("i = ", i))
    if(i == 1){
      ss <- coord_trans(px = d$x1[i], py = d$y1[i], word_m = d$word_mat[[i]])
      page[ss$r, ss$c] <- d$word_mat[[i]]
      next
    }
    
    # Loop through spiral for ith word
    for(j in 1:nrow(d$spiral[[i]])){
      
      # print(paste0("  j = ", j))
      
      # ss (subset) are the elements of the whole page where the new word matrix will sit
      ss <- coord_trans(px = d$spiral[[i]]$x[j], py = d$spiral[[i]]$y[j], word_m = d$word_mat[[i]])
      page_ss <- page[ss$r, ss$c]
      
      # Test if any of the non NA elements intersect
      # The length of the intersect is 0 is there are no overlaps
      l_inter <- length(intersect(which(!is.na(page_ss)), which(!is.na(d$word_mat[[i]]))))
      
      if(l_inter != 0){
        # If they have an overlap, and this is the last coordinate of the spiral, 
        # print message that this word has not been placed on the page and move
        # on to the next word (increment i by 1)
        if(j == nrow(d$spiral[[i]])){
          np[i] <- glue::glue("Word {i} ({d$word[i]}) NOT placed")
          # page[ss$r, ss$c][!is.na(d$word_mat[[i]])] <- d$word_mat[[i]][!is.na(d$word_mat[[i]])]
          break
        }
        # If they have an overlap, but there is more of ths sprial to test, then
        # move on to the next coordinate of the spiral (increment j by 1)
        next
      } else {
        # If there is no collision (length of the intersection == 0) then we can place the word
        # at these coordinates with no overlap of any previously placed word
        page[ss$r, ss$c][!is.na(d$word_mat[[i]])] <- d$word_mat[[i]][!is.na(d$word_mat[[i]])]
        
        # Print message (for debugging)
        # print(glue::glue("word {i} placed"))
        
        
        # Increment progress bar
        setTxtProgressBar(pb, i)
        
        # Break out of this spiral loop and move onto the next word (increment i by 1)
        break
      }
    }
    
  }
  close(pb)
  
  if(sum(!is.na(np)) > 0){print(np[!is.na(np)])}else(message("All words placed"))
  
  col_na <- apply(page, 2, function(x) all(is.na(x)))
  crop_left <- rle(col_na)$lengths[1]
  crop_right <- page_w - rle(rev(col_na))$lengths[1]
  
  row_na <- apply(page, 1, function(x) all(is.na(x)))
  crop_top <- rle(row_na)$lengths[1]
  crop_bottom <- page_h - rle(rev(row_na))$lengths[1]
  
  page_cropped <- page[crop_top:crop_bottom, crop_left:crop_right]
  
  
  # remove the word buffer pixels
  if(!show_word_buffer){page_cropped[page_cropped == 0] <- NA}
  
  # Plot page
  if(view == TRUE){
    par(xaxs="i",  yaxs="i")
    plot_mat(page_cropped)
  }
  
  if(save == TRUE){
    message("Rendering plot and saving to device")
    png(filename, width = page_w, height=page_h, units = "px", res=page_res)
    par(xaxs="i",  yaxs="i")
    plot_mat(page_cropped)
    # axis(1)
    # axis(2)
    invisible(dev.off())
    message("Complete")
  }
}