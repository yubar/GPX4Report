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
	### following is a quick&dirty fix for days without moving
	#days$n2 <- days$n
	#days$n2[10:length(days$n2)] <- days$n2[10:length(days$n2)]+1
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
	
	
	ele <- rollapply(x$ele, width = 5, by = 1, FUN = mean, align = "center")
	
	ele <- c(mean(x$ele[1:3]), mean(x$ele[1:4]), ele, mean(x$ele[(nrow(x)-3):nrow(x)]), mean(x$ele[(nrow(x)-2):nrow(x)]))
	
	dele <- diff(ele)

	x <- x[x$spd >= minspd,]
	
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
		, gain=round(stats$ele_gain)
		, loss=round(stats$ele_loss)
		, gainstr=paste0("+", round(stats$ele_gain), "\n-", round(stats$ele_loss))
		, overalltime=secFormat(stats$time_overall, F)
		, movtime=secFormat(stats$time_moving, F)
	)
	summ <- data.frame(
			NA
			, NA
			, "Итого:"
			, round(sum(stats$len), 1)
			, round(sum(stats$ele_gain))
			, round(sum(stats$ele_loss))
			, paste0("+", round(sum(stats$ele_gain)), "\n-", round(sum(stats$ele_loss)))
			, secFormat(sum(stats$time_overall), F)
			, secFormat(sum(stats$time_moving), F)
	)
	summ <- setNames(summ, names(out))
	out <- rbind(out, summ)
	write_xlsx(out, outfilename)
}

saveDays <- function(gpx){

	suppressMessages(library(rgdal))
	
	labels <- c("Начало маршрута", format(gpx$days$dt[1:(nrow(gpx$days)-1)], "%d.%m.%Y"))
	spdf = SpatialPointsDataFrame(data.frame(x=gpx$days$lon, y=gpx$days$lat), data.frame(name=labels, ele=gpx$days$ele, n=gpx$days$n-1))
	spdf <- rbind(spdf, SpatialPointsDataFrame(data.frame(x=gpx$track$lon[nrow(gpx$track)], y=gpx$track$lat[nrow(gpx$track)]), data.frame(name="Окончание маршрута", ele=gpx$track$ele[nrow(gpx$track)], n=nrow(gpx$days))))
	writeOGR(obj=spdf[1,], dsn="start.gpx", layer="waypoints", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", overwrite_layer=T)
	writeOGR(obj=spdf[nrow(spdf),], dsn="finish.gpx", layer="waypoints", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", overwrite_layer=T)
	writeOGR(obj=spdf[2:(nrow(spdf)-1),], dsn="stays.gpx", layer="waypoints", driver="GPX", dataset_options="GPX_USE_EXTENSIONS=yes", overwrite_layer=T)
}

splitGpxDays <- function(tr, trackTZ){
	
	suppressMessages(library(dplyr))
	suppressMessages(library(rgdal))
	
	saveGpx <- function(x){
		nam <- paste(format(x$dt[1],"%Y%m%d"),".gpx", sep="")
		data <- data.frame(ele=x$ele, time=x$time)
		data[,"track_fid"]<-0
		data[,"track_seg_id"]<-0
		data[,"track_seg_point_id"]<-0:(nrow(data)-1)
		spdf = SpatialPointsDataFrame(data.frame(x=x$lon, y=x$lat), data)
		writeOGR(obj=spdf, dsn=nam, layer="track_points", driver="GPX", dataset_options ="FORCE_GPX_TRACK=true", overwrite_layer=T)
		
	}

	group_map(group_by(tr, as.Date(tr$dt, tz=trackTZ)), ~ saveGpx(.x))
	return(invisible(NULL))
}

dayStats <- function(tr, trackTZ){
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
				, axis.ticks.length=unit(.15, "cm")
				, axis.ticks.length.x.top=unit(.05, "cm")
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
		geom_line(data=gpx$track, aes(x=l, y=ele), alpha=0.5, size=1, color=config$Colors$Profile) + 
		geom_line(data=gpx$track, aes(x=l, y=ele), alpha=1, size=0.5, color=config$Colors$Profile)

	png(filename = out, width=config$days$width, height=config$days$height, units = "px", type="cairo")
	print(ga)
	invisible(dev.off())

}

plotDaysElevations <- function(tr, config, timezone){
	cat("Saving days elevation profiles... ")
	group_map(group_by(tr, date = as.Date(tr$dt, tz=timezone)), ~ plotDayElevation(prepareDayElevation(.x, config), config))
	cat("Completed.\n")
}

extractExtrema <- function(track, minDiff) {
	currentMax <- track$ele[1]
	currentMin <- track$ele[1]
	ixMin <- 1
	ixMax <- 1
	last <- 0
	extrema <- data.frame(i=integer(0), currentEle=numeric(0), typ=character(0), ix=integer(0), ele=numeric(0))
	points <- data.frame(L=numeric(0), Ele=numeric(0), Name=character(0), sep=character(0), hjust=numeric(0), vjust=numeric(0), xnudge=numeric(0), ynudge=numeric(0), length=numeric(0))
	for (i in 2:length(ele)){
		if(track$ele[i] > currentMax) { 
			currentMax = track$ele[i]
			ixMax = i
			if(track$ele[i] >= currentMin + minDiff){
				if (last != -1) {
					#print(c(i,  track$ele[i], "min", ixMin, currentMin))
					#extrema <- rbind(extrema, setNames(as.list(c(i, track$ele[i], "min", ixMin, currentMin)),names(extrema)),stringsAsFactors = FALSE)
					points <- rbind(points, setNames(as.list(c(
						track$l[ixMin], 
						round(currentMin, digits = 0), 
						paste0("min: ", round(track$l[ixMin], digits = 1),"km/",round(currentMin, digits = 1),"m")
						,"\\n"
						,0.5
						,1
						,NA,NA,NA
					))
					,names(points)),stringsAsFactors = FALSE)
					last = -1
				}
				currentMin = track$ele[i]
				ixMin = i
			}
		}
		if(track$ele[i] < currentMin) { 
			currentMin = track$ele[i]
			ixMin = i
			if(track$ele[i] <= currentMax - minDiff){
				if (last != 1){
					#print(c(i,  track$ele[i], "max", ixMax, currentMax))
					#extrema <- rbind(extrema, setNames(as.list(c(i, track$ele[i], "max", ixMax, currentMax)),names(extrema)),stringsAsFactors = FALSE)
					points <- rbind(points, setNames(as.list(c(
						track$l[ixMax], 
						round(currentMax, digits = 0), 
						paste0("max: ", round(track$l[ixMax], digits = 1),"km/",round(currentMax, digits = 1),"m")
						,"\\n"
						,0.5
						,0
						,NA,NA,NA
					))
					,names(points)),stringsAsFactors = FALSE)
					last = 1
				}
				currentMax = track$ele[i]
				ixMax = i
			}
		}
	}
	
	return(points)
}
