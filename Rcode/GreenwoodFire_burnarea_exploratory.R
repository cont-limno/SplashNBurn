############# Exploratory analysis of lakes/watersheds in Greenwood Fire burn area #############
# Date: 9-14-21
# updated: 10-2-21
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#setwd("C:/Users/FWL/Documents/ClearLakeCA")
setwd("C:/Users/immcc/Documents/SplashNBurn")

#### R libraries ####
library(LAGOSNE)
library(dplyr)
library(ggplot2)
library(raster)

#### Input data ####
# From manually georeferenced 9.13.21 fire polygon: https://inciweb.nwcg.gov/incident/map/7805/0/125373
# watersheds from LAGOS-US-LOCUS v1.0:  https://doi.org/10.1002/lol2.10203; https://doi.org/10.6073/pasta/e5c2fb8d77467d3f03de4667ac2173ca 
burned_watersheds <- read.csv("Data/BurnedWatersheds_9.13d.csv")
burned_watersheds$Shape_Area <- burned_watersheds$Shape_Area/10000 #covert sq m to ha
names(burned_watersheds) <- c('lagoslakeid','ws_zoneid','ws_perim_m','ws_area_ha','est_burned_pct')

lakeinfo <- read.csv("C:/Users/immcc/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LOCUS/LOCUS_v1.0/lake_information.csv")
lakechar <- read.csv("C:/Users/immcc/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LOCUS/LOCUS_v1.0/lake_characteristics.csv")

# preliminary lake depth data
depths <- read.csv("C:/Users/immcc/Dropbox/CL_LAGOSUS_exports/LAGOSUS_DEPTH/DEPTH_v0.1/lake_depth.csv")

# UPDATE- hand-digitized fire polygon from 9-13-21; fire size hasn't changed since then; expect to redo this when fire is totally out
burn_polygon <- shapefile("Data/GreenwoodFirePolygon/GreenwoodFirePolygon.shp")

# select by location in ArcGIS based on above burn polygon
burned_watersheds_shp <- shapefile("Data/BurnedWatersheds/BurnedWatersheds_9.13_intersect.shp")
burned_watersheds <- burned_watersheds_shp@data[,c(2:5)]
burned_watersheds$Shape_Area <- burned_watersheds$Shape_Area/10000 #covert sq m to ha
names(burned_watersheds) <- c('lagoslakeid','ws_zoneid','ws_perim_m','ws_area_ha')

# greenwood fire percent watershed burned (from other script)
greenwood_pct_burned <- read.csv("Data/greenwood_fire_pct_watershed_burned.csv")

# Landscape/lake attributes from LAGOSNE
dt <- lagosne_load(version = '1.087.3')
lagosne_lulc <- dt$iws.lulc 
lagosne_limno <- dt$epi_nutr

# example smoke product
smoke090521 <- shapefile("Data/Smoke/hms_smoke20210905/hms_smoke20210905.shp")
smoke090521 <- spTransform(smoke090521, crs(burn_polygon))

######## Main program #########
burned_lagoslakeids <- burned_watersheds$lagoslakeid
length(unique(burned_lagoslakeids))

# only keep attributes for burned watersheds
lakeinfo_burned <- subset(lakeinfo, lagoslakeid %in% burned_lagoslakeids)
lakechar_burned <- subset(lakechar, lagoslakeid %in% burned_lagoslakeids)

# only keep variables of interest
lakeinfo_vars <- c('lagoslakeid','lake_namegnis','lake_namelagos','lake_elevation_m',
                   'lake_lat_decdeg','lake_lon_decdeg','lake_centroidstate')
lakechar_vars <- c('lagoslakeid','lake_waterarea_ha','lake_totalarea_ha','lake_perimeter_m',
                   'lake_shorelinedevfactor','lake_connectivity_class','lake_connectivity_fluctuates',
                   'lake_connectivity_permanent')

lakeinfo_burned <- lakeinfo_burned[,lakeinfo_vars]
lakechar_burned <- lakechar_burned[,lakechar_vars]

burned_watersheds <- merge(burned_watersheds, greenwood_pct_burned, by='lagoslakeid')

locus_attributes <- merge(lakeinfo_burned, lakechar_burned, by='lagoslakeid')
locus_attributes <- merge(locus_attributes, burned_watersheds, by='lagoslakeid')

# exploratory analysis
locus_attributes$drainage_ratio <- locus_attributes$lake_waterarea_ha/locus_attributes$ws_area_ha
summary(locus_attributes)
cor(locus_attributes$lake_totalarea_ha, locus_attributes$lake_waterarea_ha)

connclass_summary <- locus_attributes %>%
  group_by(lake_connectivity_class) %>%
  summarize(nType=n())

par(mfrow=c(2,3))
hist(locus_attributes$lake_totalarea_ha, main='Total lake area (ha)', xlim=c(0,600), breaks=seq(0,3140,10), xlab='')
hist(locus_attributes$lake_elevation_m, main='Lake elevation (m)', xlab='')
hist(locus_attributes$lake_shorelinedevfactor, main='Shoreline development factor', xlab='')
hist(locus_attributes$ws_area_ha, main='Watershed area (ha)', xlab='', xlim=c(0,12200), breaks=seq(0,44600,100))
hist(locus_attributes$drainage_ratio, main='Drainage ratio', xlab='')
barplot(connclass_summary$nType, names.arg=connclass_summary$lake_connectivity_class, main='Lake connectivity class')
mtext(side=3, 'dont worry KSC, I wouldnt forget this!', cex=0.6)

## LULC analysis (only for lakes >= 4ha)
lagosne_lulc$iws_totalforest2011_pct <- lagosne_lulc$iws_nlcd2011_pct_41 + lagosne_lulc$iws_nlcd2011_pct_42 + lagosne_lulc$iws_nlcd2011_pct_43
lagosne_lulc$iws_totalwetland2011_pct <- lagosne_lulc$iws_nlcd2011_pct_90 + lagosne_lulc$iws_nlcd2011_pct_95
lagosne_lulc$iws_totalag2011_pct <- lagosne_lulc$iws_nlcd2011_pct_81 + lagosne_lulc$iws_nlcd2011_pct_82

lulc_burned <- subset(lagosne_lulc, lagoslakeid %in% burned_lagoslakeids)
par(mfrow=c(2,3))
hist(lulc_burned$iws_totalag2011_pct, main='Total ag pct 2011', xlim=c(0,100), breaks=seq(0,100,5), xlab='')
hist(lulc_burned$iws_totalwetland2011_pct, main='Total wetland pct 2011', xlim=c(0,100), breaks=seq(0,100,5), xlab='')
hist(lulc_burned$iws_totalforest2011_pct, main='Total forest pct 2011', xlim=c(0,100), breaks=seq(0,100,5), xlab='')
hist(lulc_burned$iws_nlcd2011_pct_41, main='Deciduous forest pct 2011', xlim=c(0,100), breaks=seq(0,100,5), xlab='')
hist(lulc_burned$iws_nlcd2011_pct_42, main='Coniferous forest pct 2011', xlim=c(0,100), breaks=seq(0,100,5), xlab='')
hist(lulc_burned$iws_nlcd2011_pct_43, main='Mixed forest pct 2011', xlim=c(0,100), breaks=seq(0,100,5), xlab='')

# limno data available?
# first must convert dates, keep only data between 6-15 and 9-15
limno_burned <- subset(lagosne_limno, lagoslakeid %in% burned_lagoslakeids)
limno_burned$Month <- as.numeric(format(limno_burned$sampledate, "%m"))
limno_burned$Day <- as.numeric(format(limno_burned$sampledate, "%d"))
limno_burned <- subset(limno_burned, Month %in% c(6,7,8,9))
limno_burned$KeepSeason <- ifelse(limno_burned$Month %in% c(6) & limno_burned$Day < 15, 'Toss', 'Keep')
limno_burned$KeepSeason <- ifelse(limno_burned$Month %in% c(9) & limno_burned$Day > 15, 'Toss', limno_burned$KeepSeason)
limno_burned <- subset(limno_burned, KeepSeason=='Keep')

length(unique(limno_burned$lagoslakeid))

limno_burned_summary <- limno_burned %>%
  group_by(lagoslakeid) %>%
  summarize(min_chla=min(chla, na.rm=T),
            mean_chla=mean(chla, na.rm=T),
            median_chla=median(chla, na.rm=T),
            max_chla=max(chla, na.rm=T),
            n_chla=sum(!is.na(chla)),
            min_doc=min(doc, na.rm=T),
            mean_doc=mean(doc, na.rm=T),
            median_doc=median(doc, na.rm=T),
            max_doc=max(doc, na.rm=T),
            n_doc=sum(!is.na(doc)),
            min_secchi=min(secchi, na.rm=T),
            mean_secchi=mean(secchi, na.rm=T),
            median_secchi=median(secchi, na.rm=T),
            max_secchi=max(secchi, na.rm=T),
            n_secchi=sum(!is.na(secchi)),
            min_tp=min(tp, na.rm=T),
            mean_tp=mean(tp, na.rm=T),
            median_tp=median(tp, na.rm=T),
            max_tp=max(tp, na.rm=T),
            n_tp=sum(!is.na(tp)))

# just plotting secchi...other variables are only from one lake
par(mfrow=c(1,1))
hist(limno_burned_summary$mean_secchi, xlab='', main='Mean Secchi (m)')

# proportion watershed burned (VISUAL ESTIMATE ONLY for illustrative purposes)
png("Figures/ws_prop_burned.png",width = 6,height = 4,units = 'in',res=300)
  hist(locus_attributes$ws_burn_pct, xlim=c(0,1), breaks=seq(0,1,0.1), col='gray',xlab='',main='Proportion watershed burned')
  #mtext(side=3, 'not eyeballed!')
  mtext(side=3, 'n = 28 lakes')
dev.off()

hist(locus_attributes$lake_waterarea_ha, xlim=c(0,1000), breaks=seq(0,3100,50), main='Lakes with burned watersheds',
     xlab='Area (ha)', col='dodgerblue')

png("Figures/connclass_burned.png",width = 3.5,height = 3.5,units = 'in',res=300)
  barplot(connclass_summary$nType, las=1,names.arg=c('DRS','DRLS','HW','ISO'), xlab='Lake connectivity class')
dev.off()

plot(locus_attributes$lake_totalarea_ha ~ locus_attributes$ws_burn_pct,
     ylim=c(0,1000), pch=20, xlab='Watershed prop burned', ylab='Total lake area (ha)')

lakearea_propburned <-ggplot(locus_attributes, aes(x=ws_burn_pct,y=lake_totalarea_ha))+
  geom_point(aes(colour=lake_connectivity_class), size=3) +
  ggtitle('Lake area vs. proportion watershed burned')+
  #scale_color_manual(values=c("dodgerblue"), labels=c("Stable"), name='')+
  theme_bw() 
lakearea_propburned


# what lakes have depth data?
depths_burned <- subset(depths, lagoslakeid %in% burned_lagoslakeids)
depths_burned <- depths_burned[,c(1,7,8)]
hist(depths_burned$lake_maxdepth_m, main='Max depth (m)', breaks=seq(1,8,1), xlab='')

#write.csv(locus_attributes, file='Data/attribute_data.csv', row.names=F)
