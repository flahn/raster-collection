library(raster.collection)
library(raster)
library(rgdal)


polygons = readOGR("C:/code/raster-collection/example",layer="polygons")
path = "C:/code/openeo-r-workspace/data/landsat7"
files = list.files(path,pattern ="*.tif$",full.names = TRUE)

rasters = lapply(files,function(x){raster(x=x)})

dates = as.POSIXct(strptime(substr(list.files(path,pattern ="*.tif$"),10,16),"%Y%j"))

coll = RasterCollection$new(dates = dates,raster = rasters)
coll$print()
coll$data

start = Sys.time()
vals = extract(stack(rasters),polygons,name <- function(variables) {
  return(mean(variables))
})
end = Sys.time() - start
end
#168 images 4 polygons 32,1 sec


start = Sys.time()
vals = extract(stack(rasters),polygons[1,],name <- function(variables) {
  return(mean(variables))
})
end = Sys.time() - start
#168 images 1 polygon 28,9 sec


start = Sys.time()
f = function() {

  stack = stack(rasters)
  # stack = stack(lapply(rasters,function(raster,...){
  #   args = list(...)
  #   return(crop(raster, extent(args$polygon)))
  # },polygon=polygons[1,]))
  clip1 = crop(stack,extent(polygons[1,]))
  clip2 = mask(clip1,polygons[1,])
  vals = extract(clip2,polygons[1,],fun=mean)

  # vals = extract(x=clip2,y=polygons[1,],fun = function(variables,...) {
  #   return(mean(variables,...))
  # })
  # vals = getValues(clip2)

  return(vals)
}
vals = f()
end = Sys.time() - start
end
#168 images 1 polygon 36,2 sec

img.coord = function(raster, polygon) {
  #0,0 top left
  #xmin,1/res(x),0
  #ymax, 1, -1/res(y)
  xmin = xmin(extent(raster))
  ymax = ymax(extent(raster))

  e = rbind(c(1,1),as.matrix(extent(polygon))-matrix(c(xmin,ymax,xmin,ymax),ncol=2,nrow=2))

  m = matrix(c(0,0,1/xres(raster),0,0,-1/yres(raster)),nrow=2,ncol=3)

  result=m %*% e
  rownames(result) <- c("x","y")

  tmp = result["y","min"]
  result["y","min"] = result["y","max"]
  result["y","max"] = tmp
  result[,"min"] = floor(result[,"min"])
  result[,"max"] = ceiling(result[,"max"])

  return(result)
}

img.coord(l,polygons[1,])

# test rasterize ----
start = Sys.time()
readGDAL.subset.extract = function() {

  for (index in 1:length(polygons)) {
    polygon = polygons[index]

    # stack = stack(rasters)
    # polygon = polygons[1,]

    sink("nul")
    vals = sapply(rasters, function(r) {
      image.coords = img.coord(r,polygon)
      raster.subset = raster(readGDAL(filename(r),
                               offset=c(image.coords["y","min"],image.coords["x","min"]),
                               region.dim=c(image.coords["y","max"]-image.coords["y","min"],
                                            image.coords["x","max"]-image.coords["x","min"]),
                               output.dim = c(image.coords["y","max"]-image.coords["y","min"],
                                              image.coords["x","max"]-image.coords["x","min"])))

      vals = extract(raster.subset,polygon,fun=mean)

      return(vals)
    })
    sink()

    return(vals)
  }
}
vals = readGDAL.subset.extract()
end = Sys.time() - start
end
#168 images 1 polygon 36,2 sec

# prepare ndvi sentinels
sentinel.2a.path = "C:/code/openeo-r-workspace/data/sentinel2_2017"
folders = list.files(sentinel.2a.path)
dates = as.POSIXct(strptime(substr(folders,12,26),format="%Y%m%dT%H%M%S"))
granule.path=paste(sentinel.2a.path,folders,"GRANULE",sep="/")
img.data = paste(granule.path,list.files(granule.path),"IMG_DATA","R10m",sep="/")
list.files(img.data)

files = list.files(sentinel.2a.path,full.names=TRUE,recursive = TRUE,pattern="B0[4|8]_10m\\.tif$")

rs = list()
for (i in seq(1,length(files),2)) {
  red = raster(files[i])
  nir = raster(files[i+1])
  rs = append(rs,stack(list(red,nir)))
}

rs1 = rs[[1]][[1]]
ndvi1=calc(rs[[1]],function(x) {(x[[2]]-x[[1]])/(x[[2]]+x[[1]])},filename="ndvi1.tif")

for (index in 2:length(rs))  {
  calc(rs[[index]],function(x) {(x[[2]]-x[[1]])/(x[[2]]+x[[1]])},filename=paste("ndvi",index,".tif",sep =""))
}

#change to where the calculated ndvis are
library(rgdal)
library(raster)
library(rgeos)
library(raster.collection)
setwd("D:/rastertest")

filenames = list("S2A_MSIL1C_20170430T103021_N0205_R108_T32UNE_20170430T103024",
                 "S2A_MSIL1C_20170602T104021_N0205_R008_T32UNE_20170602T104212",
                 "S2A_MSIL1C_20170709T103021_N0205_R108_T32UNE_20170709T103159",
                 "S2A_MSIL1C_20170917T103021_N0205_R108_T32UNE_20170917T103018",
                 "S2B_MSIL1C_20170823T103019_N0205_R108_T32UNE_20170823T103018")
dates = as.POSIXct(strptime(substr(filenames,12,26),format="%Y%m%dT%H%M%S"))

ndvis = lapply(list.files(pattern="ndvi"),raster)
polygons.invekos = readOGR(dsn=".",layer="invekos")

setwd("E:/Projekte/raster-collection")

coll2 = RasterCollection$new(dates=dates,raster=ndvis)


coll2$subset.time(from=strptime("2017-06-01", format="%Y-%m-%d"))
coll2$getData()

vals = coll2$extract(geoms = polygons.invekos,fun = function(x,na.rm){ return(mean(x,na.rm = na.rm))})

for (index in 1:length(vals)) {
  values = vals[[index]]
  plot(values~coll2$getData()$time,ylab="NDVI",xlab="Date",type="l",ylim=c(0,1))
}

f1 = polygons.invekos[1,]
as(extent(r1),"SpatialPolygons")
bbox = as(extent(r1),"SpatialPolygons")
crs(bbox) <- crs(ndvis[[1]])
gCoveredBy(f1, bbox)


extent2polygon = function(extent,crs) {
  polygon = as(extent,"SpatialPolygons")
  crs(polygon) <- crs
  return(polygon)
}

coll2$getData()$space[[1]]
plot(coll2$getData()$space[[1]])

spplot(crop(coll2$getData()[[1,"image"]],y = f1),at=seq(-1,1,by=2/25),col.regions=colorRampPalette(c("black","lightgreen"))(26))

coll2$select.space(extent(f1),crs(f1))

# extract naming tests
coll2 = RasterCollection$new(dates=dates,raster=ndvis)
vals2 = coll2$extract(geoms = polygons.invekos,fun = function(x,na.rm){
  return(list(mean=mean(x,na.rm = na.rm),sd=sd(x,na.rm=na.rm)))
})
