library(plotKML)
library(ggmap)


filename = "c:\\Git\\scripts\\EleProfile\\Kamcha.gpx"
track <- readGPX(filename)$tracks[[1]][[1]]

#http://91.237.82.95:8088/pub/ggc/2km.png/z13/3/x3854/1/y1326.png
setwd("c:\\Git\\scripts\\EleProfile")
source("getmap.R")
map <- getmap(bbox = c(left = 157.933309819208, bottom = 53.2396941277069, right = 158.99074390124, top = 54.1873046119818))
ggmap(map)

'''
g <- ggmap(map)
g <- g + geom_point(data=track, aes(x=lon, y=lat), alpha=0.7, size=0.5, color="white")
g <- g + geom_point(data=track, aes(x=lon, y=lat), alpha=0.5, size=0.255, color="red3")
'''


"r:\\Kamcha4Fact_overview2.jpg"
#k <- raster("r:\\Kamcha4Fact_overview2.tif", xmn=157.933309819208, xmx=158.99074390124, ymn=53.2396941277069, ymx=54.1873046119818)
k <- raster("r:\\Kamcha4Fact_overview2.tif")

C:\ProgramData\SAS.Planet\cache\ggc2km\z13\3\x3854\1\y1324.png