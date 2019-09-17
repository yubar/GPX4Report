#install.packages("dplyr")
#install.packages("plotKML")
#install.packages("geosphere")
#install.packages("ggplot2")

library(plotKML)
library(dplyr)
library(geosphere)


#setwd("d:\\GH\\Tools\\Profile")
setwd("c:\\Git\\scripts\\EleProfile")

ddy <- 0.02
colorDays <- "red3"
colorProfile <- "steelblue4"

track <- readGPX("track.gpx")$tracks[[1]][[1]]
track$ele <- as.numeric(track$ele)
track$leg <- distGeo(track[1:2])
track$l <- dplyr::lag(cumsum(track$leg))/1000
track$l[1] <- 0

track$dt <- as.POSIXct(track$time, format="%Y-%m-%dT%H:%M:%OS", tz = "UTC")
attributes(track$dt)$tzone <- "Asia/Irkutsk"

ymin = min(track$ele)
ymax = max(track$ele)
dy = ymax-ymin
xmax = max(track$l)
xlim = c(0, xmax)
ylim = c(ymin-dy*ddy, ymax+dy*ddy)

days <- group_modify(group_by(track, as.Date(track$dt)), ~ head(.x, 1))
days$N <- seq.int(nrow(days))
days$avg <- (days$l + dplyr::lead(days$l))/2
days$avg[nrow(days)] =  (days$l[nrow(days)] + xmax)/2
days$l[1] = NA
#plot(track$l, track$ele)
#distm(c(track$lon[1], track$lat[1]), c(track$lon[2], track$lat[2]), fun = distGeo)

library(ggplot2)
#g <- ggplot(track, aes(x=l, y=ele))
#g + geom_line(alpha=0.7, size=1)
#g <- g + geom_line(alpha=0.75, size=1)

#g + geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE)

#sp <- as.data.frame(spline(track$l, track$ele))
#g + geom_line(data = sp, aes(x = x, y = y))


ga <- ggplot()
ga <- ga + geom_linerange(data=days, aes(x=days$l, ymin=ylim[1], ymax=days$ele), color=colorDays)
ga <- ga + geom_line(data=track, aes(x=l, y=ele), alpha=0.5, size=1, color=colorProfile) + geom_line(data=track, aes(x=l, y=ele), alpha=1, size=0.5, color=colorProfile)
ga <- ga + annotate("text", x = days$avg, y = ymin-dy*ddy/2, label = days$N, color = colorDays, size = 4)
ga <- ga + scale_x_continuous(limits=xlim, expand = c(0, 0), breaks=seq(0, xmax, by = 10))
ga <- ga + scale_y_continuous(limits=ylim, expand = c(0, 0), breaks=seq(0, ymax, by = 100))
ga <- ga + theme(
	panel.margin = unit(0,"null")
    , plot.margin = rep(unit(0,"null"),4)
	, axis.title.x = element_text(hjust=1)
	, axis.title.y = element_text(hjust=0.5)
)
ga <- ga + labs(x = "Расстояние, км.", y = "Высота, м.")



png(filename = "ele_alias.png",width = 1280, height = 768, units = "px", type="cairo", bg = "transparent")
ga
dev.off()



#######

ga <- ga + theme(
	panel.grid.major = element_blank()
	, panel.grid.minor = element_blank()
	, panel.background = element_blank()
	, panel.margin = unit(0,"null")
    , plot.margin = rep(unit(0,"null"),4)
	, rect = element_rect(fill = "transparent")
	, axis.title.x = element_text(hjust=1)
	, axis.title.y = element_text(hjust=0.5)
)

summarise(group_map(group_by(track, as.Date(track$dt)), head(.x, 1)), x=l,ymin=500,ymax=ele)

geom_linerange(aes(x=10000,ymin=500, ymax=1700))
g + geom_linerange(aes(x=10000,ymin=500, ymax=1700))
for(d in days) {g <- g + geom_linerange(aes(x=d$l,ymin=500, ymax=d$ele))}

days_ <- data.frame(x=c(10000,50000),ymin=500, ymax=c(1500, 1700))
for(d in days_) {d$x}


for(i in c(1,2)) {g <- g + geom_linerange(aes(x=days_$x[i],ymin=days_$ymin[i], ymax=days_$ymax[i]))}


for(i in c(1,3)) {g2 <- g2 + geom_linerange(aes(x=20000*i*i,ymin=500, ymax=1000*i))}

g + geom_linerange(data = days_, aes(x=days_$x,ymin=days_$ymin, ymax=days_$ymin))

gs <- ggplot(days_)
gs + geom_linerange(aes(x=days_$x,ymin=days_$ymin, ymax=days_$ymax))


g + geom_linerange(data = days, aes(x=days$l,ymin=500, ymax=days$ele))