ggplotRGB <- function(inRGBRst,npix=NA,scale = 'lin'){
  
  rgblinstretch <- function(rgbDf){
    maxList <- apply(rgbDf,2,max)
    minList <- apply(rgbDf,2,min)
    temp<-rgbDf
    for(i in c(1:3)){
      temp[,i] <- (temp[,i]-minList[i])/(maxList[i]-minList[i])
    }
    return(temp)
  }
  
  rgbeqstretch<-function(rgbDf){
    
    temp<-rgbDf
    for(i in c(1:3)){
      unique <- na.omit(temp[,i])
      if (length(unique>0)){
        ecdf<-ecdf(unique)
        temp[,i] <- apply(temp[,i,drop=FALSE],2,FUN=function(x) ecdf(x))
      }
    }
    return(temp)
  }
  
  if(is.na(npix)){
    if(ncell(inRGBRst)>5000){
      npix <- 5000
    }
    else{
      npix <- ncell(inRGBRst)
    }
  }
  x <- sampleRegular(inRGBRst, size=npix, asRaster = TRUE)
  dat <- as.data.frame(x, xy=TRUE)
  colnames(dat)[3:5]<-c('r','g','b')
  
  if(scale=='lin'){
    dat[,3:5]<- rgblinstretch(dat[,3:5])
  } else if(scale=='stretch'){
    dat[,3:5]<- rgbeqstretch(dat[,3:5])
  }
  
  ggplot()+ geom_tile(data=dat, aes(x=x, y=y, fill=rgb(r,g,b))) + scale_fill_identity() + 
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    labs(x = "x", y = "y") + 
    theme_bw()
}