############# Exploratory analysis Minnesota fire history records  ######################
# Date: 11-12-21
# updated:
# Author: Ian McCullough, immccull@gmail.com
################################################################################################

#setwd("C:/Users/FWL/Documents/ClearLakeCA")
setwd("C:/Users/immcc/Documents/SplashNBurn")

#### R libraries ####
library(raster)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

#### Input data ####
## Minnesota wildfire history from MN DNR ##
# but these are only fires for which MN DNR was "primary responding agency"- so not all fires
# downloaded 11-12-21
# https://gisdata.mn.gov/dataset/env-wildfires-tracked-by-mndnr
MN_fire_shp <- shapefile("Data/shp_env_wildfires_tracked_by_mndnr/wildfires_tracked_by_mndnr.shp")

# Monitoring Trends in Burn Severity
# downloaded 11-12-21; 9-27-21 data release (goes through 2019)
MTBS <- shapefile("Data/mtbs_perimeter_data/mtbs_perims_DD.shp")

##### Main program #####
# MN DNR data
MN_fire_data <- MN_fire_shp@data

MN_fire_data$start_date <- as.Date(MN_fire_data$start_date)
MN_fire_data$key_year <- ifelse(MN_fire_data$key_year==0, '2021', MN_fire_data$key_year)# key_year has 0 for active fires for some reeason; correct to 2021

MN_fire_annual_summary <- MN_fire_data %>%
  group_by(key_year) %>%
  summarize(total_area=sum(acres_tot))

MN_fire_annual_summary$area_ha <- MN_fire_annual_summary$total_area*0.404686

barplot(MN_fire_annual_summary$area_ha, names.arg=MN_fire_annual_summary$key_year, las=3,
        main='Minnesota wildfire history', ylab='Hectares', cex.names=0.75)
mtext(side=3, 'Source: Minnesota DNR')

summary(MN_fire_annual_summary$area_ha)

# Where does the Greenwood Fire rank according to fire size?
summary(MN_fire_data$acres_tot)
hist(MN_fire_data$acres_tot)

### MTBS data ###
# MTBS counts as "eastern US" by MTBS standards, so have fires > 202 ha
MTBS_data <- MTBS@data
MTBS_data_MN <- MTBS_data[str_detect(MTBS_data$Event_ID, "MN"), ]

MTBS_data_MN$Ig_Date <- as.Date(MTBS_data_MN$Ig_Date)
MTBS_data_MN$year <- year(MTBS_data_MN$Ig_Date)
MTBS_data_MN$burned_ha <- as.numeric(MTBS_data_MN$BurnBndAc)*0.404686
MTBS_data_MN$rank <- rank(-MTBS_data_MN$burned_ha) #greenwood=10844.361ha

MTBS_data_MN_annual_total <- MTBS_data_MN %>%
  group_by(year) %>%
  summarize(burned_ha=sum(burned_ha))

MTBS_data_MN_annual <- MTBS_data_MN %>%
  group_by(year, Incid_Type) %>%
  summarize(burned_ha=sum(burned_ha), .groups='keep')

ggplot(MTBS_data_MN_annual, aes(fill=Incid_Type, y=burned_ha, x=year)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  ggtitle('MTBS Minnesota fire history')+
  scale_fill_manual(values = c("dodgerblue", "gray50", "firebrick","gold"))+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))
  