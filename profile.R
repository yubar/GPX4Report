#install.packages("dplyr")
#install.packages("plotKML")
#install.packages("geosphere")
#install.packages("ggplot2")
#install.packages("optparse")
#install.packages("yaml")
#install.packages("readr")

#setwd("d:\\GH\\scripts\\EleProfile")
#setwd("c:\\Git\\scripts\\EleProfile")

cat("Loading packages... ")

suppressMessages(library(plotKML))
suppressMessages(library(dplyr))
suppressMessages(library(geosphere))
suppressMessages(library(optparse))
suppressMessages(library(yaml))
suppressMessages(library(readr))

cat("Done\n")

option_list = list(
	make_option(c("-z", "--timezone"), type="character", default="Asia/Irkutsk", help="Actual time zone"),
	make_option(c("-t", "--track"), type="character", default="track.gpx", help="GPX track file name [default = %default]"),
	make_option(c("-c", "--config"), type="character", default="config.yml", help="YAML configuration file [default = %default]"),
	make_option(c("-p", "--points"), type="character", default="points.csv", help="CSV file with POIs [default = %default]"),
	make_option(c("-o", "--out"), type="character", default="profile.png", help="Output file name [default = %default]")
)

opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

config = yaml.load_file("config.yml")

cat("Reading and processing GPX data... ")

track <- readGPX(opt$track)$tracks[[1]][[1]]
track$ele <- as.numeric(track$ele)
track$leg <- distGeo(track[1:2])
track$l <- dplyr::lag(cumsum(track$leg))/1000
track$l[1] <- 0

track$dt <- as.POSIXct(track$time, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC")
attributes(track$dt)$tzone <- opt$timezone

ymin = min(track$ele)
ymax = max(track$ele)
dy = ymax-ymin
xmax = max(track$l)
xlim = c(0, xmax)
ylim = c(ymin-dy*config$Misc$ddy, ymax+dy*config$Misc$ddy)

days <- group_modify(group_by(track, as.Date(track$dt, tz=opt$timezone)), ~ head(.x, 1))
days$N <- seq.int(nrow(days))
days$avg <- (days$l + dplyr::lead(days$l))/2
days$avg[nrow(days)] =  (days$l[nrow(days)] + xmax)/2
days$l[1] = NA

points <- read_csv(file=opt$points)
points$sep[is.na(points$sep)] <- ", "
points$label <- paste(
	gsub("\\\\n", "\\\n", points$Name)
	, gsub("\\\\n", "\\\n", points$sep)
	, points$Ele
	, sep=""
)
points$xnudge <- as.numeric(points$xnudge)
points$ynudge <- as.numeric(points$ynudge)
points$xnudge[is.na(points$xnudge)] <- -0.8*(points$hjust[is.na(points$xnudge)]-0.5)*2
points$ynudge[is.na(points$ynudge)] <- -20*(points$vjust[is.na(points$ynudge)]-0.5)*2
lines <- points[!is.na(points$length),]
lines$xmin <- lines$L + lines$length*lines$hjust
lines$xmax <- lines$L + lines$length*(1-lines$hjust)


library(ggplot2)

ga <- ggplot()
ga <- ga + geom_linerange(data=days, aes(x=days$l, ymin=ylim[1], ymax=days$ele), color=config$Colors$Days)
ga <- ga + geom_line(data=track, aes(x=l, y=ele), alpha=0.5, size=1.5, color=config$Colors$Profile) + geom_line(data=track, aes(x=l, y=ele), alpha=1, size=1, color=config$Colors$Profile)
ga <- ga + geom_errorbarh(data=lines, aes(xmin=lines$xmin, xmax=lines$xmax, y=Ele), color=config$Colors$POIs, height=0)
ga <- ga + geom_point(data=points, aes(x=L, y=Ele), color=config$Colors$POIs, shape=1, size=config$Points$Size, stroke=config$Points$Stroke)
ga <- ga + geom_text(data=points, aes(x=L, y=Ele, label=label), color = config$Colors$POIs, size = config$Fontsize$POIs, hjust=points$hjust, vjust=points$vjust, nudge_x=points$xnudge, nudge_y=points$ynudge, lineheight=1)
ga <- ga + annotate("text", x = days$avg, y = ymin-dy*config$Misc$ddy/2, label = days$N, color = config$Colors$Days, size = config$Fontsize$Days)
ga <- ga + annotate("text", x = days$l[2]/4, y = ymin+dy*config$Misc$ddy/3, label=config$Labels$Days, color=config$Colors$Days, size=config$Fontsize$Days, fontface="bold")
ga <- ga + scale_x_continuous(limits=xlim, expand = c(0, 0), breaks=seq(0, xmax, by = 10))
ga <- ga + scale_y_continuous(limits=ylim, expand = c(0, 0), breaks=seq(0, ymax, by = 100))
ga <- ga + theme(
	panel.margin = unit(0, "null")
	, plot.margin = unit(c(0, 2, 0, 0), "mm")
	, axis.title.x = element_text(hjust=1, size=config$Fontsize$Axis_title)
	, axis.title.y = element_text(hjust=0.5,size=config$Fontsize$Axis_title)
	, axis.line.x = element_line(color="black", size = 0.5)
	, axis.line.y = element_line(color="black", size = 0.5)
	, axis.text.x = element_text(size=config$Fontsize$Axis_text)
	, axis.text.y = element_text(size=config$Fontsize$Axis_text)
	, axis.ticks.length=unit(.25, "cm")
)
ga <- ga + labs(x = config$Labels$xAxis, y = config$Labels$yAxis)

cat("Done\n")
cat("Saving output... ")

png(filename = opt$out, width=config$Dims$Width, height=config$Dims$Height, units = "px", type="cairo")
ga
invisible(dev.off())

cat("Completed.\n")