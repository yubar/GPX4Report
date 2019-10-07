#install.packages("dplyr")
#install.packages("plotKML")
#install.packages("geosphere")
#install.packages("ggplot2")
#install.packages("optparse")
#install.packages("yaml")
#install.packages("readr")

parseGPX <- function(filename, timezone, ddx, ddy){

	cat("Reading and processing GPX data... ")
	suppressMessages(library(plotKML))
	suppressMessages(library(dplyr))
	suppressMessages(library(geosphere))

	track <- readGPX(filename)$tracks[[1]][[1]]
	track$ele <- as.numeric(track$ele)
	track$leg <- distGeo(track[1:2])
	track$l <- dplyr::lag(cumsum(track$leg))/1000
	track$l[1] <- 0

	track$dt <- as.POSIXct(track$time, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC")
	attributes(track$dt)$tzone <- timezone

	props<-list()
	props$ymin <- min(track$ele)
	props$ymax <- max(track$ele)
	props$dy <- props$ymax-props$ymin
	props$xmax <- max(track$l)
	props$xlim <- c(0, props$xmax + props$xmax*ddx)
	props$ylim <- c(props$ymin-props$dy*ddy, props$ymax+props$dy*ddy)

	days <- group_modify(group_by(track, as.Date(track$dt, tz=timezone)), ~ head(.x, 1))
	days$n <- seq.int(nrow(days))
	days$avg <- (days$l + dplyr::lead(days$l))/2
	days$avg[nrow(days)] <- (days$l[nrow(days)] + props$xmax)/2
	days$l[1] <- NA
	
	cat("Done\n")
	return(list(
		track=track
		, days=days
		, props=props
	))
}

parsePOI <- function(filename){

	cat("Reading and processing POI data... ")
	suppressMessages(library(readr))	
	
	points <- read_csv(file=filename, trim_ws=FALSE, col_types=list())
	points$sep[is.na(points$sep)] <- ", "
	points$label <- NA
	points$label[!is.na(points$Name)] <- paste(
		gsub("\\\\n", "\\\n", points$Name[!is.na(points$Name)])
		, gsub("\\\\n", "\\\n", points$sep[!is.na(points$Name)])
		, points$Ele[!is.na(points$Name)]
		, sep=""
	)
	points$xnudge <- as.numeric(points$xnudge)
	points$ynudge <- as.numeric(points$ynudge)
	points$xnudge[is.na(points$xnudge)] <- -0.8*(points$hjust[is.na(points$xnudge)]-0.5)*2
	points$ynudge[is.na(points$ynudge)] <- -20*(points$vjust[is.na(points$ynudge)]-0.5)*2
	lines <- points[!is.na(points$length),]
	lines$xmin <- lines$L - lines$length*lines$hjust
	lines$xmax <- lines$L + lines$length*(1-lines$hjust)
	
	cat("Done\n")
	return(list(
		points=points
		,lines=lines
	))
}

plotElevation <- function(gpx, opt, config, usePoints = FALSE, poi = NA) {

	suppressMessages(library(ggplot2))

	ga <- ggplot()
	ga <- ga + theme(
				panel.margin = unit(0, "null")
				, plot.margin = unit(c(0, 0, 0, 0), "mm")
				, axis.title.x = element_text(hjust=1, size=config$Fontsize$Axis_title)
				, axis.title.y = element_text(hjust=0.5,size=config$Fontsize$Axis_title)
				, axis.line.x = element_line(color="black", size = 0.5)
				, axis.line.y = element_line(color="black", size = 0.5)
				, axis.text.x = element_text(size=config$Fontsize$Axis_text)
				, axis.text.y = element_text(size=config$Fontsize$Axis_text)
				, axis.ticks.length=unit(.25, "cm")
			) + 
		scale_y_continuous(limits=gpx$props$ylim, expand = c(0, 0), breaks=seq(0, gpx$props$ymax, by = 100)) +
		scale_x_continuous(limits=gpx$props$xlim, expand = c(0, 0), breaks=seq(0, gpx$props$xmax, by = 10)) +
		labs(x=config$Labels$xAxis, y=config$Labels$yAxis)	

	colVec <- rep(c(config$fill$color1,config$fill$color2),nrow(gpx$days)%/%2+1)
	ga <- ga +
		geom_ribbon(data=gpx$track, aes(x=l, ymin=gpx$props$ylim[1], ymax=ele, fill=factor(as.Date(gpx$track$dt, tz=opt$timezone))), alpha=config$fill$alpha, show.legend = FALSE)+
		scale_fill_manual(values = colVec)

	ga <- ga + 
		geom_linerange(data=gpx$days, aes(x=gpx$days$l, ymin=gpx$props$ylim[1], ymax=gpx$days$ele), color=config$Colors$Days) +
		annotate("text", x = gpx$days$avg, y = gpx$props$ymin-0.85*gpx$props$dy*config$Misc$ddy, label = gpx$days$n, color = config$Colors$Days, size = config$Fontsize$Days, vjust=0) +
		annotate("text", x = 2.5*gpx$props$xmax*config$Misc$ddx, y = gpx$props$ymin, label=config$Labels$Days, color=config$Colors$Days, size=config$Fontsize$Days, fontface="bold", hjust=0, vjust=1)

	ga <- ga + 
		geom_line(data=gpx$track, aes(x=l, y=ele), alpha=0.5, size=1.5, color=config$Colors$Profile) + 
		geom_line(data=gpx$track, aes(x=l, y=ele), alpha=1, size=1, color=config$Colors$Profile)


	if (usePoints) {
		ga <- ga + 
			geom_errorbarh(data=poi$lines, aes(xmin=xmin, xmax=xmax, y=Ele), color=config$Colors$POIs, height=0) +
			geom_point(data=poi$points, aes(x=L, y=Ele), color=config$Colors$POIs, shape=1, size=config$Points$Size, stroke=config$Points$Stroke) +
			geom_text(data=poi$points, aes(x=L, y=Ele, label=label), color = config$Colors$POIs, size = config$Fontsize$POIs, hjust=poi$points$hjust, vjust=poi$points$vjust, nudge_x=poi$points$xnudge, nudge_y=poi$points$ynudge, lineheight=1)
	}

	cat("Saving elevation profile... ")

	png(filename = opt$out, width=config$Dims$Width, height=config$Dims$Height, units = "px", type="cairo")
	print(ga)
	invisible(dev.off())
	
	cat("Completed.\n")
}

plotOverviewMap <- function(gpx, opt, config, usePoints = FALSE, poi = NA){

	suppressMessages(library(raster))
	suppressMessages(library(ggplot2))
	suppressMessages(library(ggspatial))
	suppressMessages(library(shadowtext))
	
	cat("Reading GeoTiff... ")
	s <- stack("Kamcha_overview.tif")
	cat("Done\n")
	
	tr <- as.data.frame(gpx$track)
	coordinates(tr) <- ~ lon + lat
	proj4string(tr) <- "+init=epsg:4326"
	tr <- spTransform(tr, CRS = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
	tr <- data.frame(tr)
	
	

	source("ggplotRGB.R")

	arr <- arrow(angle = 20, length = unit(1, "cm"), ends = "last", type = "closed")
	arrW <- arrow(angle = 30, length = unit(1.5, "cm"), ends = "last", type = "closed")
	
	cat("Making plot... ")
	g <- ggplotRGB(s, npix = ncell(s))
	#g <- ggplot()

	g <- g + geom_path(data = tr, aes(x=lon, y=lat), alpha=0.75, size=3, colour=config$overview$colTrack)

	g <- g + annotation_scale(location = "tl", width_hint = 1/10, height=unit(0.5, "cm"), bar_cols = c("gray30", "white"), text_col="black", text_cex = 2, plot_unit="m", pad_x = unit(20, "cm"), pad_y = unit(2, "cm"))
	g <- g + annotation_scale(location = "br", width_hint = 1/10, height=unit(0.5, "cm"), bar_cols = c("gray30", "white"), text_col="black", text_cex = 2, plot_unit="m", pad_y = unit(4, "cm"))

	# g <- g +
		# scale_y_continuous(breaks=seq(0, max(tr$lat), by = 5000)) +
		# scale_x_continuous(breaks=seq(0, max(tr$lon), by = 5000)) +
		# theme(axis.text.x = element_text(angle = 90))

	#start
	g <- g + geom_text(aes(x = tr$lon[1], y = tr$lat[1]), label=config$Labels$OverviewStart, colour=config$overview$colText, size=10, hjust=0, vjust=1, nudge_x=1000, nudge_y=0)
	g <- g + geom_curve(aes(x = tr$lon[1], y = tr$lat[1], xend = tr$lon[10], yend = tr$lat[10]), curvature = 0, arrow = arrW, colour=config$overview$colText, size = 2, alpha=0.75, lineend = "butt")
	#finish
	g <- g + geom_point(aes(x = tr$lon[nrow(tr)], y = tr$lat[nrow(tr)]), colour = config$overview$colPoints, shape=21, size=15, stroke=3)
	g <- g + geom_text(aes(x = tr$lon[nrow(tr)], y = tr$lat[nrow(tr)]), label=config$Labels$OverviewEnd, colour=config$overview$colText, size=10, hjust=1, vjust=0.5, nudge_x=-1000)
	#arrows
	g <- g + geom_curve(aes(x = 17585000, y = 7200000, xend = 17590000, yend = 7190000), curvature = 0.2, arrow = arr, colour=config$overview$colArrows, size = 2, alpha=0.75, lineend = "butt")

	g <- g + geom_curve(aes(x = 17604000, y = 7139000, xend = 17609000, yend = 7144000), curvature = 0.2, arrow = arr, colour=config$overview$colArrows, size = 2, alpha=0.75, lineend = "butt")

	g <- g + geom_curve(aes(x = 17645000, y = 7135000, xend = 17652500, yend = 7130000), curvature = -0.5, arrow = arr, colour=config$overview$colArrows, size = 2, alpha=0.75, lineend = "butt")
	g <- g + geom_curve(aes(x = 17657000, y = 7127000, xend = 17663000, yend = 7123000), curvature = 0.1, arrow = arr, colour=config$overview$colArrows, size = 2, alpha=0.75, lineend = "butt")

	g <- g + geom_curve(aes(x = 17685000, y = 7090000, xend = 17692000, yend = 7098000), curvature = 0.2, arrow = arr, colour=config$overview$colArrows, size = 2, alpha=0.75, lineend = "butt")
	g <- g + geom_curve(aes(x = 17694500, y = 7095000, xend = 17678000, yend = 7082000), curvature = -0.2, arrow = arr, colour=config$overview$colArrows, size = 2, alpha=0.75, lineend = "butt")

	g <- g + geom_curve(aes(x = 17672000, y = 7031000, xend = 17679000, yend = 7031000), curvature = 0.2, arrow = arr, colour=config$overview$colArrows, size = 2, alpha=0.75, lineend = "butt")
	g <- g + geom_curve(aes(x = 17681000, y = 7032000, xend = 17674000, yend = 7034000), curvature = 0.3, arrow = arr, colour=config$overview$colArrows, size = 2, alpha=0.75, lineend = "butt")
	
	#nights
	
	trd <- as.data.frame(gpx$days)
	coordinates(trd) <- ~ lon + lat
	proj4string(trd) <- "+init=epsg:4326"
	trd <- spTransform(trd, CRS = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
	trd <- data.frame(trd)[2:nrow(trd),]
	trd$label <- format(as.Date(trd$dt, tz = "Asia/Kamchatka")-1, "%d.%m")
	trd$hjust <- 0
	trd$vjust <- 0
	trd$hjust[c(3,4,5,7,9,10)] <- 1
	trd$vjust[c(9,10,14)] <- 1
	trd$xnudge <- (-1)^trd$hjust*550
	trd$ynudge <- (-1)^trd$vjust*250
	trd$xnudge[c(3,4,7,9,10,14)] <- c(-700, -700, -700, -700 , -700, 500)
	trd$ynudge[c(3,4,7,9,10,11,12,14)] <- c(-100, -100, 0, 100, 100, 1000, 100, -1000)
	g <- g + geom_point(aes(x = trd$lon, y = trd$lat), colour = config$overview$colPoints, shape=24, size=9, stroke=2)
	g <- g + geom_shadowtext(aes(x = trd$lon, y = trd$lat, label= trd$label), colour = "red4", bg.colour = "white", bg.r=0.05, size=8, hjust=trd$hjust, vjust=trd$vjust, nudge_x=trd$xnudge, nudge_y=trd$ynudge, alpha=0.5)
	#g
	
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
	cat("Done\n")
	
	cat("Saving output... ")
	png(filename = "Kamcha_overview.png", width=ncol(s), height=nrow(s), units = "px", type="cairo")
	print(g)
	invisible(dev.off())
	cat("Done\n")

	cat("Breaking output... ")
	suppressMessages(library(magick))
	img <- image_read("Kamcha_overview.png")
	itop <- image_crop(img, "1860x2600+70+30")
	ibtm <- image_crop(img, "1860x2600-0-50", "SouthEast")
	image_write(itop, path = "Kamcha_overview_1.png", format = "png")
	image_write(ibtm, path = "Kamcha_overview_2.png", format = "png")
	cat("Done\n")
}

#setwd("d:\\GH\\scripts\\EleProfile")
#setwd("c:\\Git\\scripts\\EleProfile")

suppressMessages(library(optparse))

option_list = list(
	make_option(c("-z", "--timezone"), type="character", default="Asia/Irkutsk", help="Actual time zone"),
	make_option(c("-t", "--track"), type="character", default="track.gpx", help="GPX track file name [default = %default]"),
	make_option(c("-c", "--config"), type="character", default="config.yml", help="YAML configuration file [default = %default]"),
	make_option(c("-p", "--points"), type="character", default="points.csv", help="CSV file with POIs [default = %default]"),
	make_option(c("-o", "--out"), type="character", default="profile.png", help="Output file name [default = %default]")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

if(!file.exists(opt$config)){
	cat(paste("Cannot find config file \"", opt$config, "\".", sep=""))
	q()
}
suppressMessages(library(yaml))
config = yaml.load_file(opt$config)

if(!file.exists(opt$track)){
	cat(paste("Cannot find GPX input file \"", opt$track, "\".", sep=""))
	q()
}
gpx <- parseGPX(opt$track, opt$timezone, config$Misc$ddx, config$Misc$ddy)
#qq <- parseGPX("VAH.gpx", "Asia/Irkutsk", 0.02)
#timezone <- "Asia/Kamchatka"
#gpx <- parseGPX("Kamcha.gpx", "Asia/Kamchatka", 0.0025, 0.03)
#poi <- parsePOI ("Kamcha.csv")
usePoints <- FALSE
if(!file.exists(opt$points)){
	cat(paste("Warning: cannot find csv file with POIs \"", opt$points, "\".\n", sep=""))
} else{
	usePoints <- TRUE
	poi <- parsePOI (opt$points)
}

#plotElevation(gpx, opt, config, usePoints, poi)
plotOverviewMap(gpx, opt, config, usePoints, poi)

cat("\nExecution completed.\n")
