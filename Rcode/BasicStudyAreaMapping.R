##################### Basic study area mapping ##############################
# Date created: 4-28-22
# updated:
# author: Ian McCullough (immccull@gmail.com)
#############################################################################

#### R libraries ####
library(dplyr)
library(raster)
library(rgdal)
setwd("C:/Users/immcc/Documents/SplashNBurn")

#### Input data ####
burned_lakes <- read.csv("Data/BurnedLakes/burned_lake_accessibility-12-3-21 CTF2-8-22-1500.csv")
control <- read.csv("Data/ControlLakes/control_candidates_2.8.22_attributes.csv")

depths <- read.csv("Data/lake_depth.csv")
depths <- depths[,c('lagoslakeid','lake_maxdepth_m','lake_meandepth_m')]

control <- merge(control, depths, by='lagoslakeid', all.x=T)
#write.csv(control, "C:/Users/immcc/Documents/SplashNBurn/Data/ControlLakes/Lakes10kmBuff_wDepth.csv", row.names=F)

# LAGOS LOCUS v1.0
#lake_char <- read.csv("C:/Users/immcc/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LOCUS/LOCUS_v1.0/lake_characteristics.csv")
lake_info <- read.csv("C:/Users/immcc/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LOCUS/LOCUS_v1.0/lake_information.csv")

minnesota_lakes <- shapefile("Data/Minnesota_lakes_1ha/Minnesota_lakes_1ha.shp")
minnesota <- shapefile("Data/Minnesota_outline/Minnesota.shp")

burn_perimeter <- shapefile("Data/GreenwoodFirePolygon/GreenwoodFirePolygon.shp")

#### Main program ####
#plot(minnesota)
#plot(burn_perimeter)

hist(control$Hectares, breaks=seq(0,80,10))
summary(control$Hectares)
hist(burned_lakes$lake_waterarea_ha)
summary(burned_lakes$lake_waterarea_ha)

hist(burned_lakes$ws_burn_pct, main='Proportion watershed burned',
     xlab='Proportion', breaks=seq(0,1,0.1))

# get burned and control lake polygons
burned_lakes_shp <- subset(minnesota_lakes, lagoslakei %in% burned_lakes$lagoslakeid)
plot(burned_lakes_shp, add=T, col='red')

control_lakes_shp <- subset(minnesota_lakes, lagoslakei %in% control$lagoslakeid)
plot(control_lakes_shp, add=T, col='dodgerblue')


plot(control_lakes_shp, col='dodgerblue')
plot(burn_perimeter, add=T)
plot(burned_lakes_shp, add=T, col='red')
#legend('topright', legend=c('Burned','Control'), col=c('red','dodgerblue'), pch=c(16,16))

# save burned and control lake shapefiles:
dsnname <- "Data/ControlLakes"
layername <- 'control_lakes'
#writeOGR(control_lakes_shp, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)

dsnname <- "Data/BurnedLakes"
layername <- 'burned_lakes'
#writeOGR(burned_lakes_shp, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)

