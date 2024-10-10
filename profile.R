source("profilelib.R")

suppressMessages(library(optparse))

option_list = list(
	make_option(c("-z", "--timezone"), type="character", default="Asia/Irkutsk", help="Actual time zone"),
	make_option(c("-t", "--track"), type="character", default="track.gpx", help="GPX track file name [default = %default]"),
	make_option(c("-c", "--config"), type="character", default="config.yml", help="YAML configuration file [default = %default]"),
	make_option(c("-p", "--points"), type="character", default="points.csv", help="CSV file with POIs [default = %default]"),
	make_option(c("-o", "--out"), type="character", default="profile.png", help="Output file name [default = %default]"),
	make_option(c("-m", "--moving"), type="character", default="moving.csv", help="CSV file with breaks for moving stats [default = %default]")
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

usePoints <- FALSE
if(!file.exists(opt$points)){
	cat(paste("Warning: cannot find csv file with POIs \"", opt$points, "\".\n", sep=""))
} else{
	usePoints <- TRUE
	poi <- parsePOI (opt$points)
}

#plotElevation(gpx, opt, config, usePoints, poi)
#plotDaysElevations(gpx$track, config, opt$timezone)

#stats <- dayStats(gpx$track, trackTZ=opt$timezone)
#writeDayStats(stats)
saveDays(gpx)

#if(file.exists(opt$moving)){
#	stats <- segmentStats(gpx$track, trackTZ=opt$timezone, filename=opt$moving)
#	writeSegmentStats(stats, "SegmentStats.xlsx")
#}

#splitGpxDays(gpx$track, opt$timezone)
#saveStays(gpx$track, opt$timezone)

cat("\nExecution completed.\n")
