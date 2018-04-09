#' @importFrom R6 R6Class
#' @export
RasterCollection <- R6Class(
  "RasterCollection",
  public = list(
    images = NULL,
    data = NULL,

    initialize = function(dates=NULL,raster=list()) {
      self$images = raster
      self$data = tibble(time=dates,space=lapply(raster,extent),image=raster)
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

    extract = function(geoms, fun) {
      tryCatch({
        result = list()
        sink("nul")
        for (index in 1:length(geoms)) {
          geom = geoms[index,]

          rasters = self$getData()$image


          vals = sapply(rasters, function(r) {

            image.coords = private$img.coord(r,geom)
            raster.subset = raster(readGDAL(filename(r),
                                            offset=c(image.coords["y","min"],image.coords["x","min"]),
                                            region.dim=c(image.coords["y","max"]-image.coords["y","min"],
                                                         image.coords["x","max"]-image.coords["x","min"]),
                                            output.dim = c(image.coords["y","max"]-image.coords["y","min"],
                                                           image.coords["x","max"]-image.coords["x","min"])))

            vals = extract(raster.subset,geom,fun=fun)

            return(vals)
          })
          result = append(result,list(vals))
        }
        return(result)
      },finally={
        sink()
      })

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
  private = list(
    img.coord = function(raster, polygon) {
      #0,0 top left
      #xmin,1/res(x),0
      #ymax, 1, -1/res(y)
      xmin = xmin(extent(raster))
      ymax = ymax(extent(raster))

      e = rbind(c(1,1),rbind(c(xmin(polygon),xmax(polygon)),c(ymin(polygon),ymax(polygon)))-matrix(c(xmin,ymax,xmin,ymax),ncol=2,nrow=2))

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
  )
)
