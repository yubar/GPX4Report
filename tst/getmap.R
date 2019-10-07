getmap <- function(
  bbox = c(left = -95.80204, bottom = 29.38048, right = -94.92313, top = 30.14344),
  zoom = 10, maptype = c("terrain","terrain-background","terrain-labels",
    "terrain-lines", "toner", "toner-2010", "toner-2011", "toner-background",
    "toner-hybrid", "toner-labels", "toner-lines", "toner-lite", "watercolor"),
  crop = TRUE, messaging = FALSE, urlonly = FALSE, color = c("color","bw"), force = FALSE,
  where = tempdir(), https = FALSE, ...
){
  
  #library(ggmap)
  
  # enumerate argument checking (added in lieu of checkargs function)
  args <- as.list(match.call(expand.dots = TRUE)[-1])
  argsgiven <- names(args)

  if ("location" %in% argsgiven) {
    warning("location is not a valid argument to get_stamenmap(); it is ignored.")
  }

  if("bbox" %in% argsgiven){
    if(!(is.numeric(bbox) && length(bbox) == 4)){
      stop("bounding box improperly specified.  see ?get_openstreetmap", call. = F)
    }
  }

  if("zoom" %in% argsgiven){
    if(!(is.numeric(zoom) && length(zoom) == 1 &&
    zoom == round(zoom) && zoom >= 0 && zoom <= 18)){
      stop("scale must be a positive integer 0-18, see ?get_stamenmap.", call. = F)
    }
  }

  if("messaging" %in% argsgiven) stopifnot(is.logical(messaging))

  if("urlonly" %in% argsgiven) stopifnot(is.logical(urlonly))


  # color arg checked by match.arg


  # argument checking (no checks for language, region, markers, path, visible, style)
  #args <- as.list(match.call(expand.dots = TRUE)[-1])
  #if(checkargs) get_stamenmap_checkargs(args)
  maptype <- match.arg(maptype)
  color <- match.arg(color)
  if(is.null(names(bbox))) names(bbox) <- c("left","bottom","right","top")

  # set image type (stamen only)
  if(maptype %in% c("watercolor")){
    filetype <- "jpg"
    message("Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.")
  } else {
    filetype <- "png"
    message("Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.")
  }

  # determine tiles to get
  fourCorners <- expand.grid(
    lon = c(bbox["left"], bbox["right"]),
    lat = c(bbox["bottom"], bbox["top"])
  )
  fourCorners$zoom <- zoom
  row.names(fourCorners) <- c("lowerleft","lowerright","upperleft","upperright")
  fourCornersTiles <- apply(fourCorners, 1, function(v) LonLat2XY(v[1],v[2],v[3]))

  xsNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, function(df) df$X)))))
  ysNeeded <- Reduce(":", sort(unique(as.numeric(sapply(fourCornersTiles, function(df) df$Y)))))
  tilesNeeded <- expand.grid(x = xsNeeded, y = ysNeeded)
  if(nrow(tilesNeeded) > 40){
    message(nrow(tilesNeeded), " tiles needed, this may take a while ",
      "(try a smaller zoom).")
  }


  # make urls - e.g. http://tile.stamen.com/[maptype]/[zoom]/[x]/[y].jpg
  base_url <- if (https) "https://stamen-tiles.a.ssl.fastly.net/" else "http://tile.stamen.com/"
  # base_url <- "http://b.b.tile.stamen.com/"
  base_url <- paste(base_url, maptype, "/", zoom, sep = "")
  urls <- paste(base_url, apply(tilesNeeded, 1, paste, collapse = "/"), sep = "/")
  urls <- paste(urls, filetype, sep = ".")
  if(messaging) message(length(urls), " tiles required.")
  if(urlonly) return(urls)

	str(maptype)
	str(zoom)
	str(color)
	str(force)

  # make list of tiles
  listOfTiles <- lapply(
    split(tilesNeeded, 1:nrow(tilesNeeded)),
    function(v) {
      v <- as.numeric(v)
      ggmap:::get_stamenmap_tile(maptype, zoom, v[1], v[2], color, force = force, messaging = messaging)
    }
  )


  # stitch tiles together
  str(listOfTiles)
  print('---------------')
  typeof(listOfTiles[[1]])
  str(listOfTiles[[1]])
  map <- ggmap:::stitch(listOfTiles)


  # format map and return if not cropping
  if(!crop) {
    # additional map meta-data
    attr(map, "source")  <- "stamen"
    attr(map, "maptype") <- maptype
    attr(map, "zoom")    <- zoom

    # return
    return(map)
  }


  # crop map
  if(crop){
  	mbbox <- attr(map, "bb")

  	size <- 256L * c(length(xsNeeded), length(ysNeeded))

    # slon is the sequence of lons corresponding to the pixels left to right
  	slon <- seq(mbbox$ll.lon, mbbox$ur.lon, length.out = size[1])

    # slat is the sequence of lats corresponding to the pixels bottom to top
    # slat is more complicated due to the mercator projection
    slat <- vector("double", length = 256L*length(ysNeeded))
    for(k in seq_along(ysNeeded)){
      slat[(k-1)*256 + 1:256] <-
        sapply(as.list(0:255), function(y){
          XY2LonLat(X = xsNeeded[1], Y = ysNeeded[k], zoom, x = 0, y = y)$lat
        })
    }
    slat <- rev(slat)
    ##slat <- seq(mbbox$ll.lat, mbbox$ur.lat, length.out = size[2])

    keep_x_ndcs <- which(bbox["left"] <= slon & slon <= bbox["right"])
    keep_y_ndcs <- sort( size[2] - which(bbox["bottom"] <= slat & slat <= bbox["top"]) )

    croppedmap <- map[keep_y_ndcs, keep_x_ndcs]
  }


  # format map
  croppedmap <- as.raster(croppedmap)
  class(croppedmap) <- c("ggmap","raster")
  attr(croppedmap, "bb") <- data.frame(
    ll.lat = bbox["bottom"], ll.lon = bbox["left"],
    ur.lat = bbox["top"], ur.lon = bbox["right"]
  )

  # additional map meta-data
  attr(croppedmap, "source")  <- "stamen"
  attr(croppedmap, "maptype") <- maptype
  attr(croppedmap, "zoom")    <- zoom


  # return
  croppedmap
}