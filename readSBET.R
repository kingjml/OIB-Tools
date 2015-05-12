###### Read SBET flight data, return projected points
# filename: absolute filepath to SBET data, string
# samples: max number of samples to return, currently randomly sampled from dataset
# coords: extent coordinates array, in meters, use c(xmin, ymin, xmax, xmin), currently UTM coords only!
# offset: max distance beyond cords to include, in meters, must provide coords if used
# GCS (Geographic coordinate system):RGDAL CRS string, defaults to WGS84
# PCS (Projected coordinate system):RGDAL CRS string, defaults to NULL (ie. unprojected)
readSBET <- function(filename, coords = NULL,  boxOffset=0, GCS="+proj=longlat +ellps=WGS84", PCS=NULL){
 
 fdOpen = file(filename, "rb")
 recordSize = 136 #in bytes
 recordNum = 17 #number of values per record (time, lat, lon, ... etc) 
 recordEst = (file.info(filename)$size)/recordSize #estimate number of records based on file size in bytes
 cat("Reading in", recordEst, " flight data records\n")
 #Dim 0 padded arrays to speed up read from binary file
 time <- array(0,recordEst)
 lat <- array(0,recordEst)
 lon <- array(0,recordEst)
 elv <- array(0,recordEst)
 
 pb <- txtProgressBar(min = 1, max = recordEst, style = 3)
 i = 1
  while (length(a <- readBin(fdOpen, double(), n=recordNum)) > 0) { 
    time[i] = a[1]  	   #GPS time
    lat[i] = a[2]		   #In rad
    lon[i] = a[3]		   #In rad
    elv[i] = a[4]	         #In meters
    #Additional params available, see SBET format pdf, I don't read them here to cut down on memory
    i= i+1
    setTxtProgressBar(pb, i)
  }
 close(fdOpen)
 cat("\nFlight data read\n")
 #TODO find a better method (or one that works?) to convert GPS epoc time to UTC.
 #time[i] = strftime(as.POSIXct(a[1], origin="1970-01-01", tz = "UTC"), format="%H%M%S")
 lat = lat*(180/pi) #Convert to deg from rad
 lon = lon*(180/pi) #Convert to deg from rad
 cat("Projecting flight data\n")
 fdRaw = data.frame(time, lat, lon, elv)
 coordinates(fdRaw) <- ~ lon+lat
 proj4string(fdRaw) <- CRS(GCS)

 if(!is.null(PCS)){
   fdPro<- spTransform(fdRaw, CRS(PCS))
 }
 
 cat("Extracting relevant flight data\n")
 #create bounding polygon and clip flight lines
 
 if(!is.null(coords)){
    mpBbox<-extentPoly(coords, boxOffset, GCS, PCS) 
    keep <- which(!is.na(over(fdPro,mpBbox)))
    fdPro=fdPro[keep,]
}

 fdPro$x<-coordinates(fdPro)[,1]
 fdPro$y<-coordinates(fdPro)[,2]

 fdPro$tDiff = c(0,diff(fdPro$time))
 fdPro$line = 1

 cat("Splitting flight data into flight lines\n")
 #Split into flight lines based on time difference
#TODO Linearize
 lineSteps = c(1,which(fdPro$tDiff>100),length(fdPro$tDiff))
 for(i in seq(length(lineSteps)-1)){
	fdPro$line[lineSteps[i]:lineSteps[i+1]] = i
	#fdPro$line[lineSteps[i]:lineSteps[i+1]-1] = i
 }

 cat("Imported FP points: ", length(fdPro), "\n")
 cat("Mean elv above geoid (m):", round(mean(fdPro$elv)), "\n")

 return(fdPro)
}
