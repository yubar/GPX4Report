#install.packages("dplyr")
#install.packages("plotKML")
#install.packages("geosphere")
#install.packages("ggplot2")

#setwd("d:\\GH\\scripts\\EleProfile")

##consts
#ddy <- 0.02
#colorDays <- "red3"
#colorProfile <- "steelblue4"

#Sys.setlocale("LC_ALL","Russian.UTF8")

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

opt$timezone

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

library(ggplot2)

calcPosition <- function(L, length, xpos) {
	#xmax/config$Dims$Width
	return(list(xmin=L + length*(xpos-1), xmax=L + length*xpos))
}

ga <- ggplot()
ga <- ga + geom_linerange(data=days, aes(x=days$l, ymin=ylim[1], ymax=days$ele), color=config$Colors$Days)
ga <- ga + geom_line(data=track, aes(x=l, y=ele), alpha=0.5, size=1, color=config$Colors$Profile) + geom_line(data=track, aes(x=l, y=ele), alpha=1, size=0.5, color=config$Colors$Profile)
ga <- ga + geom_errorbarh(data=points[points$xpos!=0.5,], aes(xmin=calcPosition(L, length, xpos)$xmin, xmax=calcPosition(L, length, xpos)$xmax, y=Ele), color=config$Colors$Days, height=0)
ga <- ga + geom_point(data=points, aes(x=L, y=Ele), color=config$Colors$Days, shape=1)
ga <- ga + annotate("text", x = points$L, y = points$Ele, label = points$Name, color = config$Colors$Days, size = 3, hjust=points$xpos, vjust=points$ypos)
ga <- ga + annotate("text", x = days$avg, y = ymin-dy*config$Misc$ddy/2, label = days$N, color = config$Colors$Days, size = 4)
ga <- ga + scale_x_continuous(limits=xlim, expand = c(0, 0), breaks=seq(0, xmax, by = 10))
ga <- ga + scale_y_continuous(limits=ylim, expand = c(0, 0), breaks=seq(0, ymax, by = 100))
ga <- ga + theme(
	panel.margin = unit(0, "null")
	, plot.margin = unit(c(1, 1, 0, 0), "mm")
	, axis.title.x = element_text(hjust=1)
	, axis.title.y = element_text(hjust=0.5)
)
ga <- ga + labs(x = config$Labels$xAxis, y = config$Labels$yAxis)



cat("Done\n")
cat("Saving output... ")

png(filename = opt$out, width=config$Dims$Width, height=config$Dims$Height, units = "px", type="cairo") #, bg = "transparent"
ga
invisible(dev.off())

cat("Completed.\n")


#######
#g + geom_text_repel(data=track, aes(x=l, y=ele, label=labels, color=config$Colors$Days, size=(if(labels=="") 0.125 else 3)), show.legend=FALSE, box.padding=0.125)
# ga <- ga + theme(
	# panel.grid.major = element_blank()
	# , panel.grid.minor = element_blank()
	# , panel.background = element_blank()
	# , panel.margin = unit(0,"null")
    # , plot.margin = rep(unit(0,"null"),4)
	# , rect = element_rect(fill = "transparent")
	# , axis.title.x = element_text(hjust=1)
	# , axis.title.y = element_text(hjust=0.5)
# )

# summarise(group_map(group_by(track, as.Date(track$dt)), head(.x, 1)), x=l,ymin=500,ymax=ele)

# geom_linerange(aes(x=10000,ymin=500, ymax=1700))
# g + geom_linerange(aes(x=10000,ymin=500, ymax=1700))
# for(d in days) {g <- g + geom_linerange(aes(x=d$l,ymin=500, ymax=d$ele))}

# days_ <- data.frame(x=c(10000,50000),ymin=500, ymax=c(1500, 1700))
# for(d in days_) {d$x}


# for(i in c(1,2)) {g <- g + geom_linerange(aes(x=days_$x[i],ymin=days_$ymin[i], ymax=days_$ymax[i]))}


# for(i in c(1,3)) {g2 <- g2 + geom_linerange(aes(x=20000*i*i,ymin=500, ymax=1000*i))}

# g + geom_linerange(data = days_, aes(x=days_$x,ymin=days_$ymin, ymax=days_$ymin))

# gs <- ggplot(days_)
# gs + geom_linerange(aes(x=days_$x,ymin=days_$ymin, ymax=days_$ymax))


# g + geom_linerange(data = days, aes(x=days$l,ymin=500, ymax=days$ele))