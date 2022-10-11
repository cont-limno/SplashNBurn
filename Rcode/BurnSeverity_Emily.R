
## set working directory ##
setwd("c:/Users/FWL/Desktop/Emily/R/BURN")


## R libraries ##
library(dplyr)
library(readr)
library(ggplot2)


## Data Sets ##
soil <- read.csv('c:/Users/FWL/Desktop/Emily/GIS/Burn_Severity/MostRecentUSE/soilWS/soil_burn15ws.csv')
soil_buffer <- read.csv('c:/Users/FWL/Desktop/Emily/GIS/Burn_Severity/MostRecentUSE/soilBUFF/soil_burn15buff.csv')
vegetation <- read.csv('c:/Users/FWL/Desktop/Emily/GIS/Burn_Severity/MostRecentUSE/vegWS/veg_burn15ws.csv')
vegetation_buffer <- read.csv('c:/Users/FWL/Desktop/Emily/GIS/Burn_Severity/MostRecentUSE/vegBUFF/veg_burn15buff.csv')

burn_attribute <- read.csv('c:/Users/FWL/Desktop/Emily/GIS/Burn_Severity/MostRecentUSE/attributes/burn15.csv')
control_buffer_attribute <- read.csv('c:/Users/FWL/Desktop/Emily/GIS/Burn_Severity/VegBufferBurnSeverity/COntrolBufferAttribute.csv')
burn_ws <- read.csv(('c:/Users/FWL/Desktop/Emily/GIS/Burn_Severity/MostRecentUSE/attributes/burn15ws.csv'))

Master_Table <- read.csv('c:/Users/FWL/Desktop/Emily/LAGOS_GEO/MasterDataTable.csv')

burn_drainage_lagoslakeid <- c(14101, 223, 113774, 27717, 38199, 414, 430, 66295, 50749, 15139, 133329)
burn_isolated_lagoslakeid <- c(51824, 66287, 29814, 73430)
burn_lagoslakeid <- c(14101, 223, 113774, 27717, 38199, 414, 430, 66295, 50749, 15139, 133329, 51824, 66287, 29814, 73430)


## Data Selections ##
control_buffer_attribute_select <- select(control_buffer_attribute, lagoslakei, GNIS_Name, Connectivi, AreaSqKm, Shape_Area, Area_With_, Hectares, Shape_Leng, Perimeter_, BUFF_DIST)
names(control_buffer_attribute_select)[names(control_buffer_attribute_select) == "lagoslakei"] <- "lagoslakeid"

burn_area <- select(burn_attribute, lagoslakeid, AreaSqKm, Hectares)
ws_area <- select(burn_ws, lagoslakeid, Shape_Area)

## watershed unit conversions ##
ws_area$ws_area_ha <- ws_area$Shape_Area/10000



## Merge/Join Tables ##
vegetation_buffer_area <- merge(vegetation_buffer, burn_area, by = 'lagoslakeid')
soil_buffer_area <- merge(soil_buffer, burn_area, by = 'lagoslakeid')
vegetation_ws_area <- merge(vegetation, ws_area, by = 'lagoslakeid')
soil_ws_area <- merge(soil, ws_area, by = 'lagoslakeid')



## unit conversions ##
soil_ws_area$low_severity_ha <- soil_ws_area$low_severity_m2/10000
soil_ws_area$moderate_severity_ha <- soil_ws_area$moderate_severity_m2/10000
soil_ws_area$high_severity_ha <- soil_ws_area$high_severity_m2/10000
soil_ws_area_select <- select(soil_ws_area, lagoslakeid, low_severity_ha, moderate_severity_ha, high_severity_ha, ws_area_ha)


soil_buffer_area$low_severity_ha <- soil_buffer_area$low_severity_m2/10000
soil_buffer_area$moderate_severity_ha <- soil_buffer_area$moderate_severity__m2/10000
soil_buffer_area$high_severity_ha <- soil_buffer_area$high_severity_m2/10000
soil_buffer_area$area_m2 <- soil_buffer_area$AreaSqKm*1000000
soil_buffer_area$area_ha <- soil_buffer_area$area_m2/10000
soil_buffer_area_select <- select(soil_buffer_area, lagoslakeid, low_severity_ha, moderate_severity_ha, high_severity_ha, area_ha)


vegetation_ws_area$low_severity_ha <- vegetation_ws_area$low_severity_m2/10000
vegetation_ws_area$moderate_low_severity_ha <- vegetation_ws_area$moderate_low_severity_m2/10000
vegetation_ws_area$moderate_high_severity_ha <- vegetation_ws_area$moderate_high_severity_m2/10000
vegetation_ws_area$high_severity_ha <- vegetation_ws_area$high_severity_m2/10000
vegetation_ws_area_select <- select(vegetation_ws_area, lagoslakeid, low_severity_ha, moderate_low_severity_ha, moderate_high_severity_ha, high_severity_ha, ws_area_ha)


vegetation_buffer_area$low_severity_ha <- vegetation_buffer_area$low_severity_m2/10000
vegetation_buffer_area$moderate_low_severity_ha <- vegetation_buffer_area$moderate_low_severity_m2/10000
vegetation_buffer_area$moderate_high_severity_ha <- vegetation_buffer_area$moderate_high_severity_m2/10000
vegetation_buffer_area$high_severity_ha <- vegetation_buffer_area$high_severity_m2/10000
vegetation_buffer_area$area_m2 <- vegetation_buffer_area$AreaSqKm*1000000
vegetation_buffer_area$area_ha <- vegetation_buffer_area$area_m2/10000
vegetation_buffer_area_select <- select(vegetation_buffer_area, lagoslakeid, low_severity_ha, moderate_low_severity_ha, moderate_high_severity_ha, high_severity_ha, area_ha)


## percentages ##
soil_ws_area_select$low_severity_pct <- soil_ws_area_select$low_severity_ha / soil_ws_area_select$ws_area_ha *100
soil_ws_area_select$moderate_severity_pct <- soil_ws_area_select$moderate_severity_ha / soil_ws_area_select$ws_area_ha *100
soil_ws_area_select$high_severity_pct <- soil_ws_area_select$high_severity_ha / soil_ws_area_select$ws_area_ha *100


soil_buffer_area_select$low_severity_pct <- soil_buffer_area_select$low_severity_ha / soil_buffer_area_select$area_ha *100
soil_buffer_area_select$moderate_severity_pct <- soil_buffer_area_select$moderate_severity_ha / soil_buffer_area_select$area_ha *100
soil_buffer_area_select$high_severity_pct <- soil_buffer_area_select$high_severity_ha / soil_buffer_area_select$area_ha *100


vegetation_ws_area_select$low_severity_pct <- vegetation_ws_area_select$low_severity_ha / vegetation_ws_area_select$ws_area_ha *100
vegetation_ws_area_select$moderate_low_severity_pct <- vegetation_ws_area_select$moderate_low_severity_ha / vegetation_ws_area_select$ws_area_ha *100
vegetation_ws_area_select$moderate_high_severity_pct <- vegetation_ws_area_select$moderate_high_severity_ha / vegetation_ws_area_select$ws_area_ha *100
vegetation_ws_area_select$high_severity_pct <- vegetation_ws_area_select$high_severity_ha / vegetation_ws_area_select$ws_area_ha *100


vegetation_buffer_area_select$low_severity_pct <- vegetation_buffer_area_select$low_severity_ha / vegetation_buffer_area_select$area_ha *100
vegetation_buffer_area_select$moderate_low_severity_pct <- vegetation_buffer_area_select$moderate_low_severity_ha / vegetation_buffer_area_select$area_ha *100
vegetation_buffer_area_select$moderate_high_severity_pct <- vegetation_buffer_area_select$moderate_high_severity_ha / vegetation_buffer_area_select$area_ha *100
vegetation_buffer_area_select$high_severity_pct <- vegetation_buffer_area_select$high_severity_ha / vegetation_buffer_area_select$area_ha *100



## exporting tables ##
write.csv(soil_ws_area_select,  file = 'c:/Users/FWL/Desktop/Emily/GIS/Burn_Severity/CSV/soil_ws_area.csv', row.names = FALSE)
write.csv(soil_buffer_area_select,  file = 'c:/Users/FWL/Desktop/Emily/GIS/Burn_Severity/CSV/soil_buffer_area.csv', row.names = FALSE)
write.csv(vegetation_ws_area_select,  file = 'c:/Users/FWL/Desktop/Emily/GIS/Burn_Severity/CSV/vegetation_ws_area.csv', row.names = FALSE)
write.csv(vegetation_buffer_area_select,  file = 'c:/Users/FWL/Desktop/Emily/GIS/Burn_Severity/CSV/vegetation_buffer_area.csv', row.names = FALSE)

##for only calculating burn severity stop here##




?cor.test

## drainage/isolated subsets ##
soil_ws_area_dr <- subset(soil_ws_area_select, lagoslakeid %in% burn_drainage_lagoslakeid)
soil_ws_area_iso <- subset(soil_ws_area_select, lagoslakeid %in% burn_isolated_lagoslakeid)

soil_buff_area_dr <- subset(soil_buffer_area_select, lagoslakeid %in% burn_drainage_lagoslakeid)
soil_buff_area_iso <- subset(soil_buffer_area_select, lagoslakeid %in% burn_isolated_lagoslakeid)

vegetation_ws_area_dr <- subset(vegetation_ws_area_select, lagoslakeid %in% burn_drainage_lagoslakeid)
vegetation_ws_area_iso <- subset(vegetation_ws_area_select, lagoslakeid %in% burn_isolated_lagoslakeid)

vegetation_buff_area_dr <- subset(vegetation_buffer_area_select, lagoslakeid %in% burn_drainage_lagoslakeid)
vegetation_buff_area_iso <- subset(vegetation_buffer_area_select, lagoslakeid %in% burn_isolated_lagoslakeid)


## drainage/isolated filters ##
soil_ws_low_dr <- soil_ws_area_dr$low_severity_pct
soil_ws_low_iso <- soil_ws_area_iso$low_severity_pct
soil_ws_mod_dr <- soil_ws_area_dr$moderate_severity_pct
soil_ws_mod_iso <- soil_ws_area_iso$moderate_severity_pct
soil_ws_high_dr <- soil_ws_area_dr$high_severity_pct
soil_ws_high_iso <- soil_ws_area_iso$high_severity_pct

soil_buff_low_dr <- soil_buff_area_dr$low_severity_pct
soil_buff_low_iso <- soil_buff_area_iso$low_severity_pct
soil_buff_mod_dr <- soil_buff_area_dr$moderate_severity_pct
soil_buff_mod_iso <- soil_buff_area_dr$moderate_severity_pct
soil_buff_high_dr <- soil_buff_area_dr$high_severity_pct
soil_buff_high_iso <- soil_buff_area_iso$high_severity_pct

vegetation_ws_low_dr <- vegetation_ws_area_dr$low_severity_pct
vegetation_ws_low_iso <- vegetation_ws_area_iso$low_severity_pct
vegetation_ws_mod_low_dr <- vegetation_ws_area_dr$moderate_low_severity_pct
vegetation_ws_mod_low_iso <- vegetation_ws_area_iso$moderate_low_severity_pct
vegetation_ws_mod_high_dr <- vegetation_ws_area_dr$moderate_high_severity_pct
vegetation_ws_mod_high_iso <- vegetation_ws_area_iso$moderate_high_severity_pct
vegetation_ws_high_dr <- vegetation_ws_area_dr$high_severity_pct
vegetation_ws_high_iso <- vegetation_ws_area_iso$high_severity_pct

vegetation_buff_low_dr <- vegetation_buff_area_dr$low_severity_pct
vegetation_buff_low_iso <- vegetation_buff_area_iso$low_severity_pct
vegetation_buff_mod_low_dr <- vegetation_buff_area_dr$moderate_low_severity_pct
vegetation_buff_mod_low_iso <- vegetation_buff_area_dr$moderate_low_severity_pct
vegetation_ws_mod_high_dr <- vegetation_ws_area_dr$moderate_high_severity_pct
vegetation_ws_mod_high_iso <- vegetation_ws_area_iso$moderate_high_severity_pct
vegetation_buff_high_dr <- vegetation_buff_area_dr$high_severity_pct
vegetation_buff_high_iso <- vegetation_buff_area_iso$high_severity_pct


## histograms ##
hist(soil_ws_low_dr, col = "red", xlim = c(0, 100), ylim = c(0, 8), breaks = seq(0, 100, 5), main = "Soil Burn Severity % of watershed", xlab = "% watershed low severity")
hist(soil_ws_low_iso, col = "coral", breaks = seq(0, 100, 5), add = TRUE)
legend('topright', legend = c('burned drainage lakes', 'burned isolated lakes'), pch = c(15, 15), col = c('red', 'coral'))

hist(soil_ws_mod_dr, col = "red", xlim = c(0, 100), ylim = c(0, 8), breaks = seq(0, 100, 5), main = "Soil Burn Severity % of watershed", xlab = "% watershed moderate severity")
hist(soil_ws_mod_iso, col = "coral", breaks = seq(0, 100, 5), add = TRUE)
legend('topright', legend = c('burned drainage lakes', 'burned isolated lakes'), pch = c(15, 15), col = c('red', 'coral'))

hist(soil_ws_high_dr, col = "red", xlim = c(0, 100), ylim = c(0, 8), breaks = seq(0, 100, 5), main = "Soil Burn Severity % of watershed", xlab = "% watershed high severity")
hist(soil_ws_high_iso, col = "coral", breaks = seq(0, 100, 5), add = TRUE)
legend('topright', legend = c('burned drainage lakes', 'burned isolated lakes'), pch = c(15, 15), col = c('red', 'coral'))



hist(soil_buff_low_dr, col = "red", xlim = c(0, 100), ylim = c(0, 8), breaks = seq(0, 150, 5), main = "Soil Burn Severity % within 100m lakeshore buffer", xlab = "% low severity")
hist(soil_buff_low_iso, col = "coral", breaks = seq(0, 100, 5), add = TRUE)
legend('topright', legend = c('burned drainage lakes', 'burned isolated lakes'), pch = c(15, 15), col = c('red', 'coral'))

hist(soil_buff_mod_dr, col = "red", xlim = c(0, 100), ylim = c(0, 8), breaks = seq(0, 100, 5), main = "Soil Burn Severity % within 100m lakeshore buffer", xlab = "% moderate severity")
hist(soil_buff_mod_iso, col = "coral", breaks = seq(0, 100, 5), add = TRUE)
legend('topright', legend = c('burned drainage lakes', 'burned isolated lakes'), pch = c(15, 15), col = c('red', 'coral'))

hist(soil_buff_high_dr, col = "red", xlim = c(0, 100), ylim = c(0, 8), breaks = seq(0, 100, 5), main = "Soil Burn Severity % within 100m lakeshore buffer", xlab = "% high severity")
hist(soil_buff_high_iso, col = "coral", breaks = seq(0, 100, 5), add = TRUE)
legend('topright', legend = c('burned drainage lakes', 'burned isolated lakes'), pch = c(15, 15), col = c('red', 'coral'))



hist(vegetation_ws_low_dr, col = "red", xlim = c(0, 100), ylim = c(0, 8), breaks = seq(0, 100, 5), main = "Vegetation Burn Severity % of watershed", xlab = "% watershed low severity")
hist(vegetation_ws_low_iso, col = "coral", breaks = seq(0, 100, 5), add = TRUE)
legend('topright', legend = c('burned drainage lakes', 'burned isolated lakes'), pch = c(15, 15), col = c('red', 'coral'))

hist(vegetation_ws_mod_low_dr, col = "red", xlim = c(0, 100), ylim = c(0, 8), breaks = seq(0, 100, 5), main = "Vegetation Burn Severity % of watershed", xlab = "% watershed moderate low severity")
hist(vegetation_ws_mod_low_iso, col = "coral", breaks = seq(0, 100, 5), add = TRUE)
legend('topright', legend = c('burned drainage lakes', 'burned isolated lakes'), pch = c(15, 15), col = c('red', 'coral'))

hist(vegetation_ws_mod_high_dr, col = "red", xlim = c(0, 100), ylim = c(0, 8), breaks = seq(0, 100, 5), main = "Vegetation Burn Severity % of watershed", xlab = "% watershed moderate high severity")
hist(vegetation_ws_mod_high_iso, col = "coral", breaks = seq(0, 100, 5), add = TRUE)
legend('topright', legend = c('burned drainage lakes', 'burned isolated lakes'), pch = c(15, 15), col = c('red', 'coral'))

hist(vegetation_ws_high_dr, col = "red", xlim = c(0, 100), ylim = c(0, 8), breaks = seq(0, 100, 5), main = "Vegetation Burn Severity % of watershed", xlab = "% watershed high severity")
hist(vegetation_ws_high_iso, col = "coral", breaks = seq(0, 100, 5), add = TRUE)
legend('topright', legend = c('burned drainage lakes', 'burned isolated lakes'), pch = c(15, 15), col = c('red', 'coral'))



hist(vegetation_buff_low_dr, col = "red", xlim = c(0, 100), ylim = c(0, 8), breaks = seq(0, 100, 5), main = "Vegetation Burn Severity % within 100m lakeshore buffer", xlab = "% low severity")
hist(vegetation_buff_low_iso, col = "coral", breaks = seq(0, 100, 5), add = TRUE)
legend('topright', legend = c('burned drainage lakes', 'burned isolated lakes'), pch = c(15, 15), col = c('red', 'coral'))

hist(vegetation_buff_mod_low_dr, col = "red", xlim = c(0, 100), ylim = c(0, 8), breaks = seq(0, 100, 5), main = "Vegetation Burn Severity % within 100m lakeshore buffer", xlab = "% moderate low severity")
hist(vegetation_buff_mod_low_iso, col = "coral", breaks = seq(0, 100, 5), add = TRUE)
legend('topright', legend = c('burned drainage lakes', 'burned isolated lakes'), pch = c(15, 15), col = c('red', 'coral'))

hist(vegetation_ws_mod_high_dr, col = "red", xlim = c(0, 100), ylim = c(0, 8), breaks = seq(0, 100, 5), main = "Vegetation Burn Severity % within 100m lakeshore buffer", xlab = "% moderate high severity")
hist(vegetation_ws_mod_high_iso, col = "coral", breaks = seq(0, 100, 5), add = TRUE)
legend('topright', legend = c('burned drainage lakes', 'burned isolated lakes'), pch = c(15, 15), col = c('red', 'coral'))

hist(vegetation_buff_high_dr, col = "red", xlim = c(0, 100), ylim = c(0, 8), breaks = seq(0, 200, 5), main = "Vegetation Burn Severity % within 100m lakeshore buffer", xlab = "% high severity")
hist(vegetation_buff_high_iso, col = "coral", breaks = seq(0, 200, 5), add = TRUE)
legend('topright', legend = c('burned drainage lakes', 'burned isolated lakes'), pch = c(15, 15), col = c('red', 'coral'))

