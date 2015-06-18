#NASA GranualMetaData XML to ESRI polygon
#JKing 06-03-2015

library(XML)
library("sp")
library("rgdal")
xmlDataPath = "D:/OIBData/DMS" #change this to your relivant path
shapefilePath = paste(xmlDataPath, "/Shapefiles", sep ="")
filePaths = list.files(xmlDataPath, pattern="*.xml", full.names=TRUE)
fileNames = sub(".*DMS/", "", filenames )
fileNames = sub(".tif.xml", "", filenames )
fileNum = length(fileNames)

coords = matrix(0, fileNum ,8)
pb <- txtProgressBar(min = 0, max = fileNum, style = 3)

print("Reading XML data")
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

polys <- SpatialPolygons(mapply(function(poly, id) {
  xy <- matrix(poly, ncol=2, byrow=TRUE)
  Polygons(list(Polygon(xy)), ID=id)
  }, split(coords, row(coords)), ID),proj4string=CRS("+proj=longlat +ellps=WGS84"))
polys <- spTransform(polys, CRS("+proj=utm +zone=16 +north +units=m +ellps=WGS84"))

polys.df <- SpatialPolygonsDataFrame(polys, data.frame(id=ID, row.names=ID))

writeOGR(polys.df, shapefilePath , layer="DMS_Polygons", driver="ESRI Shapefile")


