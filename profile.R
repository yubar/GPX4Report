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

library(ggplot2)

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

cat("Saving output... ")

png(filename = opt$out, width=config$Dims$Width, height=config$Dims$Height, units = "px", type="cairo")
ga
invisible(dev.off())

cat("Completed.\n")