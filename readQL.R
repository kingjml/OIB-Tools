
##### Read OIB Quicklook Data, return projected points
# filename: absolute filepath to SBET data, string
# samples: max number of samples to return, currently randomly sampled from dataset
# coords: extent coordinates array, in meters, use c(xmin, ymin, xmax, xmin), currently UTM coords only!
# offset: max distance beyond cords to include, in meters, must provide coords if used
# GCS (Geographic coordinate system):RGDAL CRS string, defaults to WGS84
# PCS (Projected coordinate system):RGDAL CRS string, defaults to NULL (ie. unprojected)
readQL <- function(filename, coords = NULL, boxOffset=0, GCS="+proj=longlat +ellps=WGS84", PCS=NULL){
 cat("Reading QL data from file\n")
 qlRaw <-read.csv(filename,head=TRUE,sep=",")
 qlRaw$lon = qlRaw$lon - 360 #convert to common dec deg system

 coordinates(qlRaw) <- ~ lon+lat
 proj4string(qlRaw) <- CRS(GCS)
 if(!is.null(PCS)){
 	    qlPro <- spTransform(qlRaw, CRS(PCS))
 } else {
      qlPro = qlRaw[,]
 }

 #create bounding polygon and clip flightlines within it
 if(!is.null(coords)){
    mpBbox<-extentPoly(coords, boxOffset, GCS, PCS)
    keep <- which(!is.na(over(qlPro,mpBbox)))
    qlPro=qlPro[keep,]
    #keep <- gContains( mpBbox, qlPro ,byid=TRUE ) #| gOverlaps( mpBbox, qlPro ,byid=TRUE )
    #qlPro=qlPro[drop(keep),]
 }
 
 qlPro$x<-coordinates(qlPro)[,1]
 qlPro$y<-coordinates(qlPro)[,2]

 qlPro$tDiff = c(0,diff(qlPro$elapsed))
 qlPro$line = 1

 #Split into flight lines (This is not efficient, need to remove loop)
 lineSteps = c(1,which(qlPro$tDiff>100),length(qlPro$tDiff))
 for(i in seq(length(lineSteps)-1)){
  qlPro$line[lineSteps[i]:lineSteps[i+1]] = i
 }

 qlSuccessful = length(qlPro[which(qlPro$snow_depth>0),])

 cat("Imported QL points: ", length(qlPro), "\n")
 cat("Imported QL lines: ", max(qlPro$line), "\n")
 cat("Successful retrievals: ",qlSuccessful, "\n")
 cat("Failed retirievals: ", length(qlPro)-qlSuccessful, "\n")

 return(qlPro)
}