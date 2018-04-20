#' @importFrom R6 R6Class
#' @export
RasterCollection <- R6Class(
  "RasterCollection",
  # public ----
  public = list(
    # attributes ====
    images = NULL,
    data = NULL,
    view = NULL,

    # functions ====
    initialize = function(dates=NULL,raster=list()) {
      self$images = raster
      space.column = lapply(raster,function(r) {
        extent2polygon(extent(r),crs(r))
      })
      self$data = tibble(time=dates,space=space.column,image=raster)
      self$view = CollectionView$new()
    },

    addGranule = function(date, raster) {
      if (is.null(self$data)) {
        self$data = tibble(time=.POSIXct(integer(0)),space=list(),image=list())
      }
      # create a raster object reference first
      self$images = append(self$images,raster)
      add_row(self$data, date=date, space=list(extent(raster)),image=list(raster))
    },

    getData = function() {
      return(self$data[match(sort(self$data$time),self$data$time),])
    },

    extent = function() {
      xmin = min(sapply(self$data$space, xmin))
      xmax = max(sapply(self$data$space, xmax))
      ymin = min(sapply(self$data$space, ymin))
      ymax = max(sapply(self$data$space, ymax))

      return(extent(xmin,xmax,ymin,ymax))
    },

    extract = function(geoms, fun,raster.fun=raster,col.id="id",attribute.names,row.handler=NULL,cpuLoad=0.7) {
      totalCores = detectCores()
      coresToUse = floor(totalCores * cpuLoad)
      if (coresToUse < 1) coresToUse = 1

      if (!is.null(row.handler)) {
        tryCatch({

          cluster = makeCluster(coresToUse)

          registerDoParallel(cluster)

          foreach(jobid= 1:coresToUse,
                  # self=self,
                  # geoms = geoms,
                  # raster.fun=raster.fun,
                  # fun=fun,
                  # col.id = col.id,
                  # attribute.names = attribute.names,
                  # row.handler = row.handler,
                  .export = c("self",
                              "geoms",
                              "raster.fun",
                              "fun",
                              "col.id",
                              "attribute.names",
                              "row.handler"),
                  .packages = c("raster.collection",
                                "raster",
                                "rgdal",
                                "tibble",
                                "rpostgis")) %dopar% {

            nr.geoms = ceiling(length(geoms)/5)
            start = ((jobid-1) * nr.geoms)+1

            end = jobid * nr.geoms
            if (end > length(geoms)) {
              end = length(geoms)
            }

            selector = seq(start,end,1)

            raster.collection:::extractSingle(collection=self,
                                  geoms = geoms[selector,],
                                  fun = fun,
                                  raster.fun = raster.fun,
                                  col.id = col.id,
                                  attribute.names = attribute.names,
                                  row.handler = row.handler)
          }


        }, finally=stopCluster(cluster))
      } else {
        return(raster.collection:::extractSingle(collection=self,
                                                 fun= fun,
                                                 geoms = geoms,
                                                 raster.fun = raster.fun,
                                                 col.id = col.id,
                                                 attribute.names = attribute.names,
                                                 row.handler = row.handler))
      }




    },
    select.space = function(extent,crs) {
      if (class(extent) != "Extent") {
        stop("Extent is no raster Extent object")
      }
      bbox = as(extent,"SpatialPolygons")
      crs(bbox) <- crs

      l = apply(self$getData(), 1, function(row) {
        return(gIntersects(bbox,row$space))
      })

      return(self$getData()[l,])
    },

    subset.time= function(from=NULL,to=NULL) {
      if (is.null(from) && !is.null(to)) {
        self$data=self$data[self$data$time <= to,]
      } else if (!is.null(from) &&is.null(to)) {
        self$data=self$data[self$data$time >= from,]
      } else if (!is.null(from) && !is.null(to)) {
        self$data=self$data[self$data$time >= from & self$data$time <= to,]
      }

      return(self$data)
    },

    print = function() {
      print(self$data)
    }

  ),
  # private ----
  private = list(

    # functions ====

  )
)

# statics ----
extent2polygon = function(extent,crs) {
  polygon = as(extent,"SpatialPolygons")
  crs(polygon) <- crs
  return(polygon)
}

img.coord = function(raster, polygon) {
  raster.polygon = raster.collection:::extent2polygon(extent(raster),crs(raster))
  polygon.bbox = raster.collection:::extent2polygon(extent(polygon),crs(polygon))

  if (!gIntersects(raster.polygon,polygon.bbox)) {
    stop("Polygon and image do not intersect at all")
  }
  intersection = gIntersection(raster.polygon,polygon.bbox)

  #0,0 top left
  #xmin,1/res(x),0
  #ymax, 1, -1/res(y)
  xmin = xmin(raster)
  ymax = ymax(raster)

  e = rbind(c(1,1),rbind(c(xmin(intersection),xmax(intersection)),c(ymin(intersection),ymax(intersection)))-matrix(c(xmin,ymax,xmin,ymax),ncol=2,nrow=2))

  m = matrix(c(0,0,1/xres(raster),0,0,-1/yres(raster)),nrow=2,ncol=3)

  result=m %*% e
  rownames(result) <- c("x","y")
  colnames(result) <- c("min","max")

  tmp = result["y","min"]
  result["y","min"] = result["y","max"]
  result["y","max"] = tmp
  result[,"min"] = floor(result[,"min"])
  result[,"max"] = ceiling(result[,"max"])

  return(result)
}

extractSingle = function(collection, geoms, fun,raster.fun=raster,col.id="id",attribute.names,row.handler=NULL) {
  tryCatch({
    result = NULL
    sink("nul")
    for (index in 1:length(geoms)) {
      geom = geoms[index,]

      rasters = raster.collection:::.select.space(collection,extent(geom),crs(geom))

      for(rindex in 1:nrow(rasters)){
        row = rasters[rindex,]
        r = row$image[[1]]
        image.coords = raster.collection:::img.coord(r,geom)
        raster.subset = raster.fun(readGDAL(filename(r),
                                            offset=c(image.coords["y","min"],image.coords["x","min"]),
                                            region.dim=c(image.coords["y","max"]-image.coords["y","min"],
                                                         image.coords["x","max"]-image.coords["x","min"]),
                                            output.dim = c(image.coords["y","max"]-image.coords["y","min"],
                                                           image.coords["x","max"]-image.coords["x","min"])))

        e = extract(raster.subset,geom,fun=fun,df=TRUE)

        if (nrow(e) != length(attribute.names)) {
          stop("Extract functions output values don't match the given attribute names.")
        }

        e[,"ID"] <- NULL
        e = as.data.frame(t(e),stringsAsFactors=FALSE)
        #TODO consider multiple bands here, now we only have one row called band1

        colnames(e) = attribute.names
        e = cbind(time=as.character(row$time),id=as.numeric(geom@data[,col.id]),e,stringsAsFactors=FALSE)


        if (!is.null(row.handler)) {
          row.handler(e)
        } else {
          if (is.null(result)) {
            result = as_tibble(e)
          } else {
            result = do.call("add_row", append(list(.data=result),as.list(e)))
          }
        }
      }
    }
    return(result)
  },finally={
    sink()
  })
}

.select.space = function(collection,extent,crs) {
  if (class(extent) != "Extent") {
    stop("Extent is no raster Extent object")
  }
  bbox = as(extent,"SpatialPolygons")
  crs(bbox) <- crs

  l = apply(collection$getData(), 1, function(row) {
    return(gIntersects(bbox,row$space))
  })

  return(collection$getData()[l,])
}

#' @title Extract
#'
#' @description The extract function wraps the similar called function of the raster package, with the twist that
#' it is not loading that much data into memory. We will use a polygons bounding box and extract those
#' relatively small subsets directly from multiple files using GDAL (rgdal).
#' We have also the possibility to state a custom row handling which might be used to inject extracted values
#' directly into a database or write it to a file. Otherwise it will create a large tibble table that resides
#' in memory.
#'
#' @name RasterCollection.extract
#' @param geoms SpatialPolygon or SpatialPolygonsDataFrame that shall be used for extraction
#' @param fun the function that will be passed to raster::extract function
#' @param raster.fun function of the raster package that is applied when building an image format in R, when loading the small bounding boxes for polygons (raster as default)
#' @param col.id the name of the polygon id column when creating the row data.frame ("id" as default)
#' @param attribute.names character vector that labels the columns for calculated attributes for the row data.frame
#' @param row.handler a function that defines custom behavior how to deal with the row (extracted values) if omitted or NULL a tibble will be created
#' @param cpuLoad if a row.handler is specified this parameter is used to calculate the amount of cores used to reach the targeted cpu load (values between 0 and 1)
#'
#' @return a tibble if row.handler is omitted otherwise it is void
NULL
#geoms, fun,raster.fun=raster,col.id="id",attribute.names,row.handler=NULL,cpuLoad=0.7
