#NASA GranualMetaData XML to ESRI polygon
#Input: NASA GMD XML
#	xmlPath: XML data folder path, string (e.g. "D:/OIBData/DMS")
#	outPath: Shapefile output folder, string
#	GCS: (Geographic coordinate system):RGDAL CRS string, defaults to WGS84, 
#		string, optional (e.g "+proj=longlat +ellps=WGS84")
#		I am reasonably certaint that all GMDXML files are in WGS84
#	PCS: (Projected coordinate system):RGDAL CRS string, defaults to NULL (ie. unprojected)
#		string, optional (e.g. ""+proj=utm +zone=16 +north +units=m +ellps=WGS84")
#Output: ESRI Shapefile to outPath
#Usage example: readGMDXML("D:/OIBData/DMS","D:/OIBData/Shapefile")
#J King 06-03-2015

readGMDXML<-function(xmlPath,outPath, GCS="+proj=longlat +ellps=WGS84", PCS=NULL){
	require(XML)
	require(sp)
	require(rgdal)
	cat("GMDXML to ESRI Shapefile\n")
	filePaths = list.files(xmlPath, pattern="*.xml", full.names=TRUE)
	fileNames =list.files(xmlPath, pattern="*.xml", full.names=FALSE)
	fileNames = sub(".tif.xml", "", fileNames )
	fileNum = length(fileNames)
	cat(fileNum, "files to process\n")
	
	coords = matrix(0, fileNum ,8)
	cat("Reading XML data\n")
	pb <- txtProgressBar(min = 0, max = fileNum, style = 3)
	for (f  in 1:length(filePaths )){
		setTxtProgressBar(pb, f)
		curFile = filePaths[f]
		xml_data <- xmlParse(curFile)
		xml_top = xmlRoot(xml_data)
		pos = 1
			for (i in 1:4){
				coords[f,pos] = as.numeric(xmlToList(xml_top[[3]][[9]][[1]][[1]][[1]][[i]][[1]]))
				coords[f,pos+1] = as.numeric(xmlToList(xml_top[[3]][[9]][[1]][[1]][[1]][[i]][[2]]))
				pos = pos+2
			}
	}
	cat("\nCreating Polygons...")
	polys <- SpatialPolygons(mapply(function(poly, id) {
  			xy <- matrix(poly, ncol=2, byrow=TRUE)
  			Polygons(list(Polygon(xy)), ID=id)}, 
			split(coords, row(coords)), ID),
			proj4string=CRS(GCS))
      cat("Done\n")
	if(!is.null(PCS)){
		cat("Projecting Polygons...")
		polys <- spTransform(polys, CRS(PCS))
	 	cat("Done\n")
	}
	polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))
	cat("Exporting Polygons...")
	strptime(Sys.time(), "%m/%d/%y %H:%M:%S")

	writeOGR(polys.df, outPath, layer=paste0("DMS_Polygon_",format(Sys.time(), "%Y%m%d%H%M%S")), driver="ESRI Shapefile")
	cat("Done\nFinished\n")
}
