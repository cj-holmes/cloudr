library(cloudr)

png("obama.png", width=10, height=10, res=300, units = "in")

wc(obama$word[1:100],
   obama$count[1:100],
   lower_cex = 1,
   upper_cex = 3.5,
   spiral_n = 10,
   spiral_res = 500,
   lo_col = "grey75",
   seed = 1)

dev.off()
