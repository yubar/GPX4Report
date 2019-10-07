'''
$ x: int  961 962 963 964 961 962 963 964 961 962 ...
 $ y: int  327 327 327 327 328 328 328 328 329 329 ...

tilesNeeded <- data.frame(x=c(961,962,963,964,961,962,963,964), y=c(327,327,327,327,328,328,328,328))


listOfTiles <- lapply(
    split(tilesNeeded, 1:nrow(tilesNeeded)),
    function(v) {
      v <- as.numeric(v)
      ggmap:::get_stamenmap_tile("terrain", 10, v[1], v[2], "color", force = F, messaging = "")
    }
  )
  
  
  
'''
library(png)
library(grid)
library(magick)


bbox <- c(left = 157.933309819208, bottom = 53.2396941277069, right = 158.99074390124, top = 54.1873046119818)

img <- image_read("r:\\Kamcha4Fact_overview2.png")

g <- ggplot()
g <- g + geom_point(data=track, aes(x=lon, y=lat), alpha=0.75, size=3, color="cyan")
g <- g +
	scale_y_continuous(limits=c(bbox["bottom"], bbox["top"]), expand = c(0, 0)) +
	scale_x_continuous(limits=c(bbox["left"], bbox["right"]), expand = c(0, 0))
	
g <- g + theme(
			panel.margin = unit(0, "null")
			, plot.margin = unit(c(0, 0, 0, 0), "mm")
			, axis.title.x = element_blank()
			, axis.title.y = element_blank()
			, axis.text.x = element_blank()
			, axis.text.y = element_blank()
			, axis.ticks.length = unit(0, "null")
			, panel.background = element_rect(fill = "transparent")
			, plot.background = element_rect(fill = "transparent", color = NA)
			, panel.grid.major = element_blank()
			, panel.grid.minor = element_blank()
		)

	
	
png(filename = "r:\\Kamcha4Fact_overview_track.png", width=image_info(img)$width, height=image_info(img)$height, units = "px", type="cairo", bg = "transparent")
g
invisible(dev.off())

img2 <- image_read("r:\\Kamcha4Fact_overview_track.png")
out <- image_composite(img, img2)

image_write(out, path = "r:\\Kamcha4Fact_overview_combo.png")