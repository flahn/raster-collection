setwd("C:/Users/Florian Lahn/Documents/sentinel2 data")
library(rgdal)
library(raster.collection)

dates2 = as.POSIXct(c("2017-04-30 10:30:21 CEST",
                      "2017-06-02 10:40:21 CEST",
                      "2017-07-09 10:30:21 CEST",
                      "2017-09-17 10:30:21 CEST",
                      "2017-08-23 10:30:19 CEST"))

ndvis = lapply(list.files("./",pattern="*.tif$"),raster)

polygons = readOGR(dsn=".",layer="invekos")

coll = RasterCollection$new(dates=dates2,raster = ndvis)

extract.fun = function(val, na.rm) {
  res = list(
    mean = mean(val,na.rm=na.rm),
    sd = sd(val,na.rm=na.rm),
    total = length(val),
    na = sum(is.na(val))
  )
}

values = coll$extract(geoms = polygons, fun=extract.fun,attribute.names = c("mean","sd","total","na"))

###
#
# integrated pgsql injection
#
###
library(rpostgis)

coll = RasterCollection$new(dates=dates2,raster = ndvis)

connection = function() {
  dbConnect("PostgreSQL", dbname="invekos", host="localhost", user="postgres", password="postgres")
}

# same as for connection
disconnect = function(con) {
  dbDisconnect(con)
}

# using sql to insert row wise, make sure that the data we type cast and use nullif to filter out NA (which we will set for
# all the NULL, NA, Inf, NaN stuff)
insertRow = function(row) {
  insertStatement = "insert into zone2 (\"time\",id,mean,sd,\"all\",na) values
  (nullif($1,'NA')::timestamp,
  nullif($2,'NA'),
  nullif($3,'NA')::numeric,
  nullif($4,'NA')::numeric,
  nullif($5,'NA')::int,
  nullif($6,'NA')::int);"

  #remove NA NAN Inf -Inf and set them NA (NULL would erase the entry)
  for (index in names(row)) {
    val = row[[index]]
    if (val %in% c(NA,NaN,NULL,Inf,-Inf)) {
      row[[index]] <- NA
    }
  }

  con = dbConnect("PostgreSQL", dbname="invekos", host="localhost", user="postgres", password="postgres")
  tryCatch(
    dbSendQuery(con,insertStatement, row),
    finally = dbDisconnect(con)
  )
}

coll = RasterCollection$new(dates=dates2,raster = ndvis)
values = coll$extract(geoms = polygons, fun=extract.fun,attribute.names = c("mean","sd","total","na"),row.handler = insertRow)

values = coll$extract(geoms = polygons, fun=extract.fun,attribute.names = c("mean","sd","total","na"))

