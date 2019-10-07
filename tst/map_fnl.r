'''
library(raster)
library(rasterVis)
library(ggplot2)

map <- raster("r:\\Kamcha4Fact_overview2.tif")

s <- stack("r:\\Kamcha4Fact_overview2.tif")
plotRGB(s)

# Function to get the colourtable with the option of returing it in greyscale
getColTab <- function(rasterfile, greyscale = F){
  colTab <- raster::colortable(rasterfile)
  if(greyscale == T){
    colTab <- col2rgb(colTab)
    colTab <- colTab[1,]*0.2126 + colTab[2,]*0.7152 + colTab[3,]*0.0722
    colTab <- rgb(colTab,colTab,colTab, maxColorValue = 255)
  }
  names(colTab) <- 0:(length(colTab)-1)
  return(colTab)
}

gplot(map, maxpixels = 10e7) +
  geom_tile(aes(fill = factor(value))) +
  scale_fill_manual(values = getColTab(map),guide = "none") +
  coord_equal() 
'''
library(raster)
library(plotKML)
library(ggplot2)

s <- stack("r:\\Kamcha4Fact_overview2.tif")
track <- readGPX("c:\\Git\\scripts\\EleProfile\\Kamcha.gpx")$tracks[[1]][[1]]
tr <- as.data.frame(track)
coordinates(tr) <- ~ lon + lat
proj4string(tr) <- "+init=epsg:4326"
tr <- spTransform(tr, CRS = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
tr <- data.frame(tr)

source("c:\\Git\\scripts\\EleProfile\\ggplotRGB.R")

colour <- "red"
colour2 <- "red3"
arr <- arrow(angle = 20, length = unit(1, "cm"), ends = "last", type = "closed")
arrW <- arrow(angle = 30, length = unit(1.5, "cm"), ends = "last", type = "closed")

#g <- ggplotRGB(s, npix = ncell(s))
g <- ggplot()

g <- g + geom_path(data = tr, aes(x=lon, y=lat), alpha=0.75, size=3, colour=colour)

g <- g +
	scale_y_continuous(breaks=seq(0, max(tr$lat), by = 5000)) +
	scale_x_continuous(breaks=seq(0, max(tr$lon), by = 5000)) +
	theme(axis.text.x = element_text(angle = 90))

#start
g <- g + geom_text(aes(x = tr$lon[1], y = tr$lat[1]), label="Начало маршрута 24.08", colour=colour2, size=10, hjust=0, vjust=0, nudge_x=750)
g <- g + geom_curve(aes(x = tr$lon[1], y = tr$lat[1], xend = tr$lon[10], yend = tr$lat[10]), curvature = 0, arrow = arrW, colour=colour2, size = 2, alpha=0.75, lineend = "butt")
#finish
g <- g + geom_point(aes(x = tr$lon[nrow(tr)], y = tr$lat[nrow(tr)]), colour = "black", shape=21, size=10, stroke=5)
g <- g + geom_point(aes(x = tr$lon[nrow(tr)], y = tr$lat[nrow(tr)]), colour = colour2, shape=21, size=10, stroke=3)
g <- g + geom_text(aes(x = tr$lon[nrow(tr)], y = tr$lat[nrow(tr)]), label="Окончание маршрута 07.09", colour=colour2, size=10, hjust=1, vjust=0.5, nudge_x=-1000)
#arrows
g <- g + geom_curve(aes(x = 17585000, y = 7200000, xend = 17590000, yend = 7190000), curvature = 0.2, arrow = arr, colour=colour2, size = 2, alpha=0.75, lineend = "butt")

g <- g + geom_curve(aes(x = 17604000, y = 7139000, xend = 17609000, yend = 7144000), curvature = 0.2, arrow = arr, colour=colour2, size = 2, alpha=0.75, lineend = "butt")

g <- g + geom_curve(aes(x = 17645000, y = 7135000, xend = 17652500, yend = 7130000), curvature = -0.4, arrow = arr, colour=colour2, size = 2, alpha=0.75, lineend = "butt")
g <- g + geom_curve(aes(x = 17657000, y = 7127000, xend = 17663000, yend = 7123000), curvature = 0.1, arrow = arr, colour=colour2, size = 2, alpha=0.75, lineend = "butt")

g <- g + geom_curve(aes(x = 17685000, y = 7090000, xend = 17692000, yend = 7098000), curvature = 0.2, arrow = arr, colour=colour2, size = 2, alpha=0.75, lineend = "butt")
g <- g + geom_curve(aes(x = 17694500, y = 7095000, xend = 17678000, yend = 7082000), curvature = -0.2, arrow = arr, colour=colour2, size = 2, alpha=0.75, lineend = "butt")

g <- g + geom_curve(aes(x = 17672000, y = 7031000, xend = 17679000, yend = 7031000), curvature = 0.2, arrow = arr, colour=colour2, size = 2, alpha=0.75, lineend = "butt")
g <- g + geom_curve(aes(x = 17681000, y = 7032000, xend = 17674000, yend = 7034000), curvature = 0.3, arrow = arr, colour=colour2, size = 2, alpha=0.75, lineend = "butt")

g


g <- g + theme(
			panel.margin = unit(0, "null")
			, plot.margin = unit(c(0, 0, 0, 0), "mm")
			, axis.title.x = element_blank()
			, axis.title.y = element_blank()
			, axis.text.x = element_blank()
			, axis.text.y = element_blank()
			, axis.ticks.length = unit(0, "null")
			, panel.grid.major = element_blank()
			, panel.grid.minor = element_blank()
		)
#g <- g + annotate("text", x = tr$lon[nrow(tr)], y = tr$lat[nrow(tr)], label="\u25ce", colour=colour2, size=20, hjust=0.5, vjust=0.5)

png(filename = "r:\\Kamcha4Fact_overview2_new.png", width=ncol(s), height=nrow(s), units = "px", type="cairo")
g
invisible(dev.off())


library(magick)
img <- image_read("r:\\Kamcha4Fact_overview2_new.png")
itop <- image_crop(img, "1860x2600+70+30")
ibtm <- image_crop(img, "1860x2600-0-50", "SouthEast")
image_write(itop, path = "r:\\Kamcha4Fact_overview2_new_1.png", format = "png")
image_write(ibtm, path = "r:\\Kamcha4Fact_overview2_new_2.png", format = "png")
