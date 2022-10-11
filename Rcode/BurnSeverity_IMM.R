############### Watershed/buffer burn severity calculations ###################
# Date: 10-11-22
# updated: 
# Author: Ian McCullough, immccull@gmail.com
###############################################################################


#### R libraries ####

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")

# LAGOS lake attributes 
LOCUS <- read.csv("Data/LAGOS/LAGOS_LOCUS_Table.csv")
LOCUS$Type <- ifelse(LOCUS$Type=='sample','burned', LOCUS$Type)

# burn severity calculations from ArcGIS Tabulate area
ws_sbs <- read.csv("Data/BurnSeverity/Ian_calculations/burned_ws_sbs_tabarea.csv")[,c(2:8)]

#### Main program ####
## Soil burn severity, watershed
ws_area <- LOCUS[,c('lagoslakeid','Site','Type','ws_area_ha','Source')]

# wrangle table and convert areas to hectares
names(ws_sbs) <- c('lagoslakeid','unburned1','unburned_low','low','moderate','high','unburned2')
ws_sbs$unburned <- ws_sbs$unburned1 + ws_sbs$unburned2
ws_sbs <- ws_sbs[,c(1,3,4,5,6,8)]
ws_sbs$unburned_ha <- ws_sbs$unburned/10000
ws_sbs$unburned_low_ha <- ws_sbs$unburned_low/10000
ws_sbs$low_ha <- ws_sbs$low/10000
ws_sbs$moderate_ha <- ws_sbs$moderate/10000
ws_sbs$high_ha <- ws_sbs$high/10000
ws_sbs <- ws_sbs[,c(1,7:11)]

ws_sbs <- merge(ws_sbs, ws_area, by='lagoslakeid',all=F)

ws_sbs$unburned_pct <- (ws_sbs$unburned_ha/ws_sbs$ws_area_ha)*100
ws_sbs$unburned_low_pct <- (ws_sbs$unburned_low_ha/ws_sbs$ws_area_ha)*100
ws_sbs$low_pct <- (ws_sbs$low_ha/ws_sbs$ws_area_ha)*100
ws_sbs$moderate_pct <- (ws_sbs$moderate_ha/ws_sbs$ws_area_ha)*100
ws_sbs$high_pct <- (ws_sbs$high_ha/ws_sbs$ws_area_ha)*100

# check to see if rows sum to 100%; if they don't, assuming other calculations are correct, 
# this is likely because some portions of watersheds lie outside burn severity raster; can assume these extra areas are unburned 
ws_sbs$totalcheck <- rowSums(ws_sbs[,c(11:15)])
ws_sbs$extra_pct <- 100-ws_sbs$totalcheck
ws_sbs$unburned_pct <- ws_sbs$unburned_pct + ws_sbs$extra_pct
ws_sbs <- ws_sbs[,c(1:15)]
#write.csv(ws_sbs, "Data/BurnSeverity/Ian_calculations/burned_ws_sbs_pct.csv", row.names=F)
