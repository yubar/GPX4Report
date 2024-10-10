tr<-gpx$track[as.Date(gpx$track$dt, tz=trackTZ)=="2019-09-07",]
config = yaml.load_file(opt$config)
ggg <- prepareDayElevation(tr, config)
plotDayElevation(ggg, config)
plotDaysElevations(gpx$track) 


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

	cat("Saving elevation profile... ")

	png(filename = out, width=config$days$width, height=config$days$height, units = "px", type="cairo")
	print(ga)
	invisible(dev.off())
	
	cat("Completed.\n")
}

plotDaysElevations <- function(tr){
	group_map(group_by(tr, date = as.Date(tr$dt, tz=trackTZ)), ~ plotDayElevation(prepareDayElevation(.x, config), config))
}


group_modify(group_by(tr, date = as.Date(tr$dt, tz=trackTZ)), ~ .x[1,])

saveStays <- function(tr, trackTZ){
	
	suppressMessages(library(dplyr))
	suppressMessages(library(rgdal))
	
	stays <- SpatialPointsDataFrame(data.frame(x=tr$lon[1], y=tr$lat[1]), data.frame(name="Start"))
	
	addStay <- function(x){
		nam <- format(x$dt[1],"%d.%m")
		stays <<- rbind(stays, SpatialPointsDataFrame(data.frame(x=x$lon[nrow(x)], y=x$lat[nrow(x)]), data.frame(name=nam)))
		return(invisible(NULL))
	}

	group_map(group_by(tr, as.Date(tr$dt, tz=trackTZ)), ~ addStay(.x))
	
	writeOGR(obj=stays, dsn="stays.gpx", layer="waypoints", driver="GPX", overwrite_layer=T)
	return(invisible(NULL))
}