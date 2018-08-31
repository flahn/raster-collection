CollectionView <- R6Class(
  "CollectionView",
  # public ----
  public = list(
    initialize = function() {

    },
    getTimeInterval = function() {
      timeInterval = list(private$t.min,private$t.max)
      names(timeInterval) = c("tmin","tmax")
      return(unlist(timeInterval))
    },
    getExtent = function() {
      return(extent(private$x.min, private$x.max, private$y.min, private$y.max))
    },
    setExtent = function(e) {
      if (class(e) != "Extent") {
        stop("Extent is no raster Extent class")
      }
      private$x.min = xmin(e)
      private$x.max = xmax(e)
      private$y.min = ymin(e)
      private$y.max = ymax(e)

    },
    setCRS = function(crs) {
      private$crs = crs
    },
    getCRS = function() {
      return(private$crs)
    },
    setTMin=function(x) {
      private$t.min = x
    },
    setTMax=function(x) {
      private$t.max = x
    }
  ),
  #private ----
  private = list(
    #attributes ====
    x.min = NULL,
    x.max = NULL,
    y.min = NULL,
    y.max = NULL,
    t.min = NULL,
    t.max = NULL,
    crs = NULL
  )
)
