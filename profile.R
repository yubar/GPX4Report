#install.packages("dplyr")
#install.packages("plotKML")
#install.packages("geosphere")
#install.packages("ggplot2")
#install.packages("optparse")
#install.packages("yaml")
#install.packages("readr")
#install.packages("writexl")
#install.packages("pgirmess")

calcProps <- function(track, ddx, ddy){
	props<-list()
	props$ymin <- min(track$ele)
	props$ymax <- max(track$ele)
	props$dy <- props$ymax-props$ymin
	props$xmax <- max(track$l)
	props$xlim <- c(0, props$xmax + props$xmax*ddx)
	props$ylim <- c(props$ymin-props$dy*ddy, props$ymax+props$dy*ddy)
	return(props)
}

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
	
	track$ltm <- c(as.numeric(diff(track$dt)),NA)
	track$spd <- track$leg/track$ltm * 3.6

	props <- calcProps(track, ddx, ddy)

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

secFormat <- function(s, useSec = T) {
	o <- data.frame()
	h <- s %/% 3600
	if(useSec) {
		m <- s %% 3600 %/% 60
		s <- s %% 60
	} else {
		m = round(s %% 3600 / 60)
		h[m==60] <- h[m==60]+1
		m[m==60] <- 0
	}
	out <- sprintf("%02d:%02d", h, m)
	if (useSec) out <- paste0(out, sprintf(":%02d", s))
	
	return(out)
}

trackSummary <- function(x, minspd = 0.5){
	
	suppressMessages(library(zoo))
	
	x <- x[complete.cases(x),]
	
	time_overall <- as.numeric(difftime(x$dt[nrow(x)],x$dt[1], unit="secs"))
	len = sum(x$leg)/1000
	
	x <- x[x$spd >= minspd,]
	
	
	ele <- rollapply(x$ele, width = 5, by = 3, FUN = mean, align = "center")
	
	ele <- c(x$ele[1], ele, x$ele[nrow(x)])
	
	dele <- diff(ele)

	return(data.frame(
		time_overall = time_overall,
		time_moving = sum(x$ltm),
		time_start = min(x$dt),
		time_finish = max(x$dt),
		ele_gain = sum(dele[dele > 0]),
		ele_loss = -sum(dele[dele < 0]),
		ele_start = x$ele[1],
		ele_finish = x$ele[nrow(x)],
		ele_min = min(x$ele),
		ele_max = max(x$ele),
		len = len
	))
}

t<-'
tr <- gpx$track
fileTZ = "Europe/Moscow"
trackTZ = "Asia/Kamchatka"
filename="moving.csv"
'

segmentStats <- function(tr, trackTZ, filename, fileTZ = "Europe/Moscow") {
	suppressMessages(library(readr))

	mov <- read_csv(file=filename, trim_ws=FALSE, col_types=list())

	mov$dt <- as.POSIXct(paste(mov$date, mov$time), format="%d.%m.%Y %H:%M:%OS", tz = fileTZ)
	attributes(mov$dt)$tzone <- trackTZ

	ix <- match(mov$dt,tr$dt)

	ix <- ix[1:length(ix)-1]

	if (any(is.na(ix))) stop("One of moving breaks not found")

	parts <- split(tr, cumsum(1:nrow(tr) %in% (ix + 1)))

	stats <- as.data.frame(
		cbind(mov, do.call(rbind, lapply(parts, trackSummary)))
	)
	
	stats$time_overall_fmt <- secFormat(stats$time_overall)
	stats$time_moving_fmt <- secFormat(stats$time_moving)
	return(stats)
	
}

writeSegmentStats <- function(stats, outfilename = "segmentStats.xlsx") {
	suppressMessages(library(writexl))
	
	out <- data.frame(
		n=stats$n
		, date=stats$date
		, desc=stats$desc
		, len=round(stats$len, 1)
		, gain=paste0("+", round(stats$ele_gain), "\n-", round(stats$ele_loss))
		, movtime=secFormat(stats$time_moving, F)
	)
	summ <- data.frame(
			NA
			, NA
			, "Итого:"
			, round(sum(stats$len), 1)
			, paste0("+", round(sum(stats$ele_gain)), "\n-", round(sum(stats$ele_loss)))
			, secFormat(sum(stats$time_moving), F)
	)
	summ <- setNames(summ, names(out))
	out <- rbind(out, summ)
	write_xlsx(out, outfilename)
}

splitGpxDays <- function(tr){
	suppressMessages(library(pgirmess))
	suppressMessages(library(dplyr))
	
	saveGpx <- function(x){
		df <- data.frame(label="", lon=x$lon, lat=x$lat, ele=x$ele)
		nam <- format(x$dt[1],"%Y%m%d")
		writeGPX(df, nam, type="t")
	}
	
	stays <- data.frame(lon=tr$lon, lat=tr$lat, ele=tr$ele)[2:nrow(tr),]
	stays <- cbind(label=format(tr$dt[1:nrow(tr)-1], "%d.%m.%Y"), stays)
	writeGPX(stays, "stays", type="w")

	group_map(group_by(tr, as.Date(tr$dt, tz=trackTZ)), ~ saveGpx(.x))
}

dayStats <- function(tr){
	stats <- group_modify(group_by(tr, date = as.Date(tr$dt, tz=trackTZ)), ~ trackSummary(.x))
	stats <- cbind(
		data.frame(
			date=format(stats$date, "%d.%m.%Y")
			, len=round(stats$len, 1)
			, ele_gain=round(stats$ele_gain)
			, ele_loss=round(stats$ele_loss)
			, time_start=format(stats$time_start, "%H:%M")
			, time_finish=format(stats$time_finish, "%H:%M")
			, time_overall=secFormat(stats$time_overall, F)
			, time_moving=secFormat(stats$time_moving, F)
		)
		, round(stats[,c(8:11)])
	)
	return(stats)
}

writeDayStats <- function(stats, outfilename = "dayStats.xlsx") {
	suppressMessages(library(writexl))
	write_xlsx(stats, outfilename)
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
	trd$label <- format(as.Date(trd$dt, tz = opt$timezone)-1, "%d.%m")
	trd$hjust <- 0
	trd$vjust <- 0
	trd$hjust[c(3,4,5,7,9,10)] <- 1
	trd$vjust[c(9,10,14)] <- 1
	trd$xnudge <- (-1)^trd$hjust*550
	trd$ynudge <- (-1)^trd$vjust*250
	trd$xnudge[c(3,4,7,9,10,14)] <- c(-700, -700, -700, -700 , -700, 500)
	trd$ynudge[c(3,4,7,9,10,11,12,14)] <- c(-100, -100, 0, 100, 100, 1000, 100, -1000)
	g <- g + geom_point(aes(x = trd$lon, y = trd$lat), colour = config$overview$colShadow, shape=24, size=9, stroke=4, alpha=0.5)
	g <- g + geom_point(aes(x = trd$lon, y = trd$lat), colour = config$overview$colPoints, shape=24, size=9, stroke=2)
	g <- g + geom_shadowtext(aes(x = trd$lon, y = trd$lat, label= trd$label), colour = config$overview$colText, bg.colour = config$overview$colShadow, bg.r=0.3, size=8, hjust=trd$hjust, vjust=trd$vjust, nudge_x=trd$xnudge, nudge_y=trd$ynudge, alpha=0.5)
	g <- g + geom_text(aes(x = trd$lon, y = trd$lat, label= trd$label), colour = config$overview$colText, size=8, hjust=trd$hjust, vjust=trd$vjust, nudge_x=trd$xnudge, nudge_y=trd$ynudge, alpha=0.9)
	
	
	#start
	g <- g + geom_text(aes(x = tr$lon[1], y = tr$lat[1]), label=config$Labels$OverviewStart, colour=config$overview$colText, size=10, hjust=0, vjust=1, nudge_x=1000, nudge_y=0)
	g <- g + geom_curve(aes(x = tr$lon[1], y = tr$lat[1], xend = tr$lon[10], yend = tr$lat[10]), curvature = 0, arrow = arrW, colour=config$overview$colText, size = 2, alpha=0.75, lineend = "butt")
	#finish
	g <- g + geom_point(aes(x = tr$lon[nrow(tr)], y = tr$lat[nrow(tr)]), colour = config$overview$colPoints, shape=21, size=15, stroke=3)
	g <- g + geom_text(aes(x = tr$lon[nrow(tr)], y = tr$lat[nrow(tr)]), label=config$Labels$OverviewEnd, colour=config$overview$colText, size=10, hjust=1, vjust=0.5, nudge_x=-1000)
	
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

prepareDayElevation <- function(tr, config) {
	names(tr)[names(tr)=="l"]<-"lcum"
	tr$l <- dplyr::lag(cumsum(tr$leg))/1000
	tr$l[1] <- 0

	pr <- calcProps(tr, 0, config$Misc$ddy)
	
	return(list(track=tr, props=pr))
}

calcBy <- function(range, count){
	by0 <- round(range/count,2)
	scl <- floor(log(by0,10))
	by1 <- (log(by0/10^scl, 2)) + 1
	if(by1 < 2) by1 <- round(by1) else by1 <- floor(by1);
	if(by1==4) by1 <- 10 else if(by1==3) by1 <- 5
	return(by1*10^scl)
}

plotDayElevation <- function(gpx, config) {

	suppressMessages(library(ggplot2))

	out <- paste0(format(gpx$track$dt[1], "%Y%m%d"), ".png")
	
	yticks <- config$days$height/config$days$tick_px
	xticks <- config$days$width/config$days$tick_px
	
	ybreaks <- seq(0, gpx$props$ymax, by = calcBy(gpx$props$dy, yticks))
	xbreaks <- seq(0, gpx$props$xmax, by = calcBy(gpx$props$xmax, xticks))
	
	ga <- ggplot()
	ga <- ga + theme(
				panel.margin = unit(0, "null")
				, plot.margin = unit(c(0, 0, 0, 0), "mm")
				, axis.title.x = element_text(hjust=1, size=config$days$Axis_title)
				, axis.title.y = element_text(hjust=0.5,size=config$days$Axis_title)
				, axis.line.x = element_line(color="black", size = 0.5)
				, axis.line.y = element_line(color="black", size = 0.5)
				, axis.text.x = element_text(size=config$days$Axis_text)
				, axis.text.x.top = element_text(size=config$days$Axis_text_secondary)
				, axis.text.y = element_text(size=config$days$Axis_text)
				, axis.ticks.length=unit(.25, "cm")
				, axis.ticks.y.right=element_blank()
				, axis.text.y.right=element_blank()
			) + 
		scale_y_continuous(limits=gpx$props$ylim, expand = c(0, 0), breaks=ybreaks
			,sec.axis = sec_axis(~.)) +
		scale_x_continuous(limits=gpx$props$xlim, expand = c(0, 0), breaks=xbreaks
			,sec.axis = sec_axis(~.+gpx$track$lcum[1], breaks=round(xbreaks+gpx$track$lcum[1]))) +
		labs(x=config$Labels$xAxis, y=config$Labels$yAxis)	
	
	ga <- ga +
		geom_ribbon(data=gpx$track, aes(x=l, ymin=gpx$props$ylim[1], ymax=ele), fill=config$fill$color1, alpha=config$fill$alpha, show.legend = FALSE)

	ga <- ga + 
		geom_line(data=gpx$track, aes(x=l, y=ele), alpha=0.5, size=1.5, color=config$Colors$Profile) + 
		geom_line(data=gpx$track, aes(x=l, y=ele), alpha=1, size=1, color=config$Colors$Profile)

	png(filename = out, width=config$days$width, height=config$days$height, units = "px", type="cairo")
	print(ga)
	invisible(dev.off())

}

plotDaysElevations <- function(tr, config, timezone){
	cat("Saving days elevation profiles... ")
	group_map(group_by(tr, date = as.Date(tr$dt, tz=timezone)), ~ plotDayElevation(prepareDayElevation(.x, config), config))
	cat("Completed.\n")
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
plotDaysElevations(gpx$track, config, opt$timezone)
#plotOverviewMap(gpx, opt, config, usePoints, poi)
#stats <- segmentStats(gpx$track, trackTZ="Asia/Kamchatka", filename="moving.csv")
#writeSegmentStats(stats, "SegmentStats.xlsx")

cat("\nExecution completed.\n")
