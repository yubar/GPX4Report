save <- function(g){
	png(filename = "Kamcha_tst.png", width=3081, height=4665, units = "px", type="cairo")
	print(g)
	invisible(dev.off())
	
	suppressMessages(library(magick))
	img <- image_read("Kamcha_tst.png")
	i <- image_crop(img, "800x950+0")
	image_write(i, path = "Kamcha_tst.png", format = "png")

}

library(grid)
library(png)

img <- readPNG("Bakening.png")
bg <- rasterGrob(img, interpolate=FALSE)

g <- ggplot() + annotation_custom(bg, xmin=17585000, xmax=17601000, ymin=7180000, ymax=7200000)

g <- g + geom_path(data = tr, aes(x=lon, y=lat), alpha=0.75, size=3, colour="red")