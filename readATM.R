##### Read ATM (Airborne Topographic Mapper) HDF file and return projcted points
#Requires rhdf5, install with source("http://bioconductor.org/biocLite.R")
#filename: string to the directory path with the atm files
#coords: bounding box coordinates, provide as c(xmin,ymin,xmax,ymax)
#offset: increase bbox extent if needed, in m
#GCS (Geographic coordinate system):RGDAL CRS string, defaults to WGS84
#PCS (Projected coordinate system):RGDAL CRS string, defaults to NULL (ie. unprojected)
readATM<-function(filepath,coords=NULL, offset=0,GCS="+proj=longlat +ellps=WGS84", PCS=NULL){
 filePaths = list.files(filepath, pattern="*.h5$", full.names=TRUE)
 fileNames = sub(".h5", "", filePaths )
 fileNames = sub(paste0(filepath,"/"), "", fileNames )
 fileNum = length(fileNames)
 cat("ATM files to be processed:", as.character(fileNum),"\n")
 mpBbox<-extentPoly(coords, offset, GCS, PCS) 
 atmFull = NULL

 pb <- txtProgressBar(min = 1, max = fileNum, style = 3)
	for (i in 1:fileNum){
		lon = 0 
		lat = 0
		elv = 0
		setTxtProgressBar(pb, i)
		curFile = filePaths[i]
		#print(h5ls(curFile))
		lon <- h5read(curFile , "longitude")
		lat <- h5read(curFile , "latitude")
		elv <- h5read(curFile , "elevation")
    #atmFile <- array(fileNames[i],length(elv))
		#atmLine <- data.frame(atmFile,lat, lon, elv) 
		atmLine <- data.frame(lat, lon, elv) 
		coordinates(atmLine) <- ~ lon+lat
		proj4string(atmLine) <- CRS(GCS)
		atmLine<- spTransform(atmLine, CRS(PCS))
	 #atmLine.df <- SpatialPointsDataFrame(atmLine, data.frame(id=1:length(atmLine), filename=atmFile, elv=as.numeric(elv)))
		atmLine.df <- SpatialPointsDataFrame(atmLine, data.frame(id=1:length(atmLine), elv=as.numeric(elv)))
		
    if(!is.null(coords)){
      #Using over is WAYYYYY faster
		  keep <- which(!is.na(over(atmLine.df,mpBbox)))
    } else{
      keep <- seq(1:nrow(atmLine.df))
    }
    
		numPoints = length(keep) 
    
		if(numPoints>0){
			atmLine.df=atmLine.df[keep,]
			if (is.null(atmFull)&length(atmLine.df)>0){
				atmFull=atmLine.df
			} else if(length(atmLine.df)>0) {
				atmFull=rbind(atmFull, atmLine.df)
			}
		}

	#cat("\nProcessed file", i, " and added", numPoints, " points")


   }
 
 #Append coordinates to dataframe and return
 atmFull$x<-coordinates(atmFull)[,1]
 atmFull$y<-coordinates(atmFull)[,2]
 cat("\nTotal lidar points:", length(atmFull),"\n")
 return(atmFull)
}

