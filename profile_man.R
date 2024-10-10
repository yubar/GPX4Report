#install.packages("dplyr")
#install.packages("plotKML")
#install.packages("geosphere")
#install.packages("ggplot2")
#install.packages("optparse")
#install.packages("yaml")
#install.packages("readr")
#install.packages("writexl")
#install.packages("pgirmess")

setwd("c:\\Git\\GPX4Report")

source("profilelib.R")

opt<-list()
opt$track <- "Fanns.gpx"
opt$config <- "config.yml"
opt$timezone <- "Asia/Dushanbe"
opt$out <- "profile.png"
opt$points <- "points.csv"
opt$moving <- "moving.csv"

suppressMessages(library(yaml))
config = yaml.load_file("config.yml")

gpx <- parseGPX(opt$track, opt$timezone, 0.0025, 0.03)
#poi <- parsePOI (opt$points)

# ddd <-extractExtrema (gpx$track,100)
# write.csv(ddd,"qqq.csv", row.names = FALSE, quote=FALSE, na = "")

#plotElevation(gpx, opt, config, usePoints, poi)
#plotDaysElevations(gpx$track, config, opt$timezone)

#stats <- dayStats(gpx$track, trackTZ=opt$timezone)
#writeDayStats(stats)
#saveDays(gpx)

#if(file.exists(opt$moving)){
#	stats <- segmentStats(gpx$track, trackTZ=opt$timezone, filename=opt$moving)
#	writeSegmentStats(stats, "SegmentStats.xlsx")
#}

#splitGpxDays(gpx$track, opt$timezone)
