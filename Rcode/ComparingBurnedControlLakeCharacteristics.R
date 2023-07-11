########## Comparing characteristics of control/burned lakes ##################
# Date: 6-28-23
# updated: 7-10-23
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(dplyr)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")
LAGOStable <- read.csv("Data/LAGOS/LAGOS_LOCUS_GEO_DEPTH_combined.csv")
waterquality <- read.csv("Data/WaterQuality/combined_lab_field_may_sep.csv")

#### Main program ####
LAGOS_burned <- subset(LAGOStable, type=='burned')
LAGOS_control <- subset(LAGOStable, type=='control')

## Lake water area
boxplot(LAGOStable$lake_waterarea_ha ~ LAGOStable$type, main='Lake area',
        xlab='Type', ylab='Lake water area (ha)', las=1)

LAGOStable %>%
  dplyr::group_by(type) %>%
  dplyr::summarize(min=min(lake_waterarea_ha), median=median(lake_waterarea_ha),
                   mean=mean(lake_waterarea_ha), max=max(lake_waterarea_ha))

ks.test(LAGOS_burned$lake_waterarea_ha, LAGOS_control$lake_waterarea_ha)

# if take out Greenwood (very large burned lake)
LAGOStable_noGreen <- subset(LAGOStable, !(Site=='Greenwood Lake'))

LAGOStable_noGreen %>%
  dplyr::group_by(type) %>%
  dplyr::summarize(min=min(lake_waterarea_ha), median=median(lake_waterarea_ha),
                   mean=mean(lake_waterarea_ha), max=max(lake_waterarea_ha))

## Lake watershed area
boxplot(LAGOStable$ws_area_ha ~ LAGOStable$type, main='Lake watershed area',
        xlab='Type', ylab='Lake watershed area (ha)', las=1)

LAGOStable %>%
  dplyr::group_by(type) %>%
  dplyr::summarize(min=min(ws_area_ha), median=median(ws_area_ha),
                   mean=mean(ws_area_ha), max=max(ws_area_ha))

ks.test(LAGOS_burned$ws_area_ha, LAGOS_control$ws_area_ha)

## Drainage ratio
boxplot(LAGOStable$ws_lake_arearatio ~ LAGOStable$type, main='Drainage ratio',
        xlab='Type', ylab='Watershed/lake area ratio', las=1)

LAGOStable %>%
  dplyr::group_by(type) %>%
  dplyr::summarize(min=min(ws_lake_arearatio), median=median(ws_lake_arearatio),
                   mean=mean(ws_lake_arearatio), max=max(ws_lake_arearatio))

ks.test(LAGOS_burned$ws_lake_arearatio, LAGOS_control$ws_lake_arearatio)

## Max depth
WQ_sub <- waterquality[,c('Site','zMax_m','Type')]

# get mean of max depths for each lake
meanmax <- WQ_sub %>%
  dplyr::group_by(Site) %>%
  dplyr::summarize(meanmax=mean(zMax_m, na.rm=T))
meanmax <- merge(meanmax, LAGOStable[,c('Site','type')])

boxplot(meanmax$meanmax ~ meanmax$type, main='Max depth',
        xlab='Type', ylab='Max depth (m)', las=1)

meanmax %>%
  dplyr::group_by(type) %>%
  dplyr::summarize(min=min(meanmax), median=median(meanmax),
                   mean=mean(meanmax), max=max(meanmax))

meanmax_burned <- subset(meanmax, type=='burned')
meanmax_control <- subset(meanmax, type=='control')

ks.test(meanmax_burned$meanmax, meanmax_control$meanmax)

## Soil properties
# organic C content; no big difference
boxplot(LAGOStable$soil_orgcarbon_gperkg ~ LAGOStable$type, main='soil_orgcarbon_gperkg',
        xlab='Type', ylab='soil_orgcarbon_gperkg', las=1)

LAGOStable %>%
  dplyr::group_by(type) %>%
  dplyr::summarize(min=min(soil_orgcarbon_gperkg), median=median(soil_orgcarbon_gperkg),
                   mean=mean(soil_orgcarbon_gperkg), max=max(soil_orgcarbon_gperkg))

ks.test(LAGOS_burned$soil_orgcarbon_gperkg, LAGOS_control$soil_orgcarbon_gperkg)

# K factor; signif difference, but medians are very similar and there is not much
# variability in control lakes besides 3 outliers, which are likely behind the difference
# wouldn't read into this too much
boxplot(LAGOStable$soil_kffact ~ LAGOStable$type, main='soil_kffact',
        xlab='Type', ylab='soil_kffact', las=1)

LAGOStable %>%
  dplyr::group_by(type) %>%
  dplyr::summarize(min=min(soil_kffact), median=median(soil_kffact),
                   mean=mean(soil_kffact), max=max(soil_kffact))

ks.test(LAGOS_burned$soil_kffact, LAGOS_control$soil_kffact)

## Difference in differences (DID) estimator
# the interaction coefficent between treatment and time would represent
# the DID estimator; however, we can't implement this without both pre- and
# post-fire data
# tutorial provided by reviewer: https://rpubs.com/phle/r_tutorial_difference_in_differences
# found another one by Oscar Torres-Reyna that basically had the same info in shorter form: http://www.princeton.edu/~otorres/
#LAGOStable$treated <- ifelse(LAGOStable$type=='burned', 1, 0)
