############# Percent watershed burned in Greenwood Fire area ##################################
# Date: 9-20-21
# updated: 9-21-21
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#setwd("C:/Users/FWL/Documents/ClearLakeCA")
setwd("C:/Users/immcc/Documents/SplashNBurn")

#### R libraries ####
library(raster)
library(rgeos)
library(rgdal)

#### Input data ####
lakes_1ha <- shapefile("C:/Ian_GIS/LAGOS_US_GIS/LAGOS_US_All_lakes_1ha_v0.5.shp")

# UPDATE- hand-digitized fire polygon from 9-13-21; fire size hasn't changed since then; expect to redo this when fire is totally out
burn_polygon <- shapefile("Data/GreenwoodFirePolygon/GreenwoodFirePolygon.shp")

# select by location in ArcGIS based on above burn polygon
burned_watersheds_shp <- shapefile("Data/BurnedWatersheds/BurnedWatersheds_9.13_intersect.shp")
burned_watersheds <- burned_watersheds_shp@data[,c(2:5)]
burned_watersheds$Shape_Area <- burned_watersheds$Shape_Area/10000 #covert sq m to ha
names(burned_watersheds) <- c('lagoslakeid','ws_zoneid','ws_perim_m','ws_area_ha')


######## Main program #########
burned_lagoslakeids <- burned_watersheds$lagoslakeid
length(unique(burned_lagoslakeids))

burned_lakes <- subset(lakes_1ha, lagoslakei %in% burned_lagoslakeids)

#dsnname <- "Data/BurnedLakes"
#layername <- "BurnedLakes"
#writeOGR(burned_lakes, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)

lagoslakeid <- burned_lagoslakeids[1]
watersheds <- burned_watersheds_shp
lakes_polygon <- burned_lakes

greenwood_fire_function <- function(lagoslakeid, burn_polygon, watersheds, lakes_polygon) {
  #lagoslakeid: lake ID of interest
  #burn_polygon: fire perimeter polygon
  #watersheds: watershed polygons
  #lakes_polygon: lakes polygon
  ######assumes all GIS files in same coordinate system ####
  lake_polygon <- subset(burned_lakes, lagoslakei==lagoslakeid)
  lake_polygon <- gBuffer(lake_polygon, width=0, byid=T)
  ws_polygon <- subset(watersheds, lagoslakei==lagoslakeid)
  ws_polygon <- gBuffer(ws_polygon, width=0, byid=T)
  burn_polygon <- gBuffer(burn_polygon, width=0, byid=T)
  ws_erased <- erase(ws_polygon, lake_polygon)
  ws_area_ha <- gArea(ws_erased)/10000
  ws_burn <- suppressWarnings(raster::intersect(ws_erased, burn_polygon))
  ws_burn_ha <- gArea(ws_burn)/10000
  ws_burn_pct <- ws_burn_ha/ws_area_ha
  
  # create output 
  output <- data.frame(lagoslakeid=lagoslakeid, ws_burn_ha=ws_burn_ha, ws_burn_pct=ws_burn_pct)
  return(output)
  lake_polygon=NULL
  ws_polygon=NULL
  ws_erased=NULL
  ws_area_ha=NULL
  ws_burn=NULL
  ws_burn_ha=NULL
  ws_burn_pct=NULL
  burn_polygon=NULL
}

#test <- greenwood_fire_function(lagoslakeid=lagoslakeid, burn_polygon=burn_polygon, watersheds=watersheds, lakes_polygon=burned_lakes)


output_vec <- list()

for (i in 1:length(burned_lagoslakeids)) {
  xx <- greenwood_fire_function(lagoslakeid=burned_lagoslakeids[i], burn_polygon=burn_polygon, watersheds=watersheds, lakes_polygon=burned_lakes)
  output_vec[[i]] <- xx
  xx <- NULL
}

greenwood_fire_history <- do.call(rbind.data.frame, output_vec)

hist(greenwood_fire_history$ws_burn_pct, breaks=seq(0,1,0.1))
#write.csv(greenwood_fire_history, file='Data/greenwood_fire_pct_watershed_burned.csv', row.names=F)

burned_lakes_pctwsburned <- merge(burned_lakes, greenwood_fire_history, by.x='lagoslakei', by.y='lagoslakeid')

#dsnname <- "Data/BurnedLakes"
#layername <- "BurnedLakes"
#writeOGR(burned_lakes_pctwsburned, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)
