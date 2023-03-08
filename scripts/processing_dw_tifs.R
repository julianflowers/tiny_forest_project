## dw tifs

needs(stars, sf, tidyverse, raster, terra, mapview, mapedit, cubeview)

p <- here::here("/Users/julianflowers/Library/CloudStorage/GoogleDrive-julian.flowers12@gmail.com/My Drive/rgee_backup/dw_witney")

tif <- list.files(p, "tif", full.names = TRUE)

pal = c('#419BDF', '#397D49', '#88B053', '#7A87C6','#E49635', '#DFC35A', 
                         '#C4281B', '#A59B8F','#B39FE1')

stack <- rast(tif)
stack <- as.factor(stack)
subset(stack, 1)


plot(stack[[50:60]], col = pal)

stack1 <- stack[[!is.na(stack)]]
