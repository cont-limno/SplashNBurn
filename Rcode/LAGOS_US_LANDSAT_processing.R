################# LAGOS-US-LANDSAT v1.0 chlorophyll-a processing ##############
# Date: 11-1-22
# updated: 
# Author: Ian McCullough, immccull@gmail.com
###############################################################################
## LAGOS-US-LANDSAT v1.0 chlorophyll-a estimates
# with help from: https://dept.stat.lsa.umich.edu/~jerrick/courses/stat701/notes/sql.html
# warning: R can't read file this large
#landsat <- read.csv("C:/Users/immcc/Documents/SplashNBurn/Data/LAGOS/LAGOS_US_chlorophyll_predictions_v1.csv")

#### R libraries ####
library(sqldf)
library(lubridate)
library(dplyr)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")
waterquality <- read.csv("Data/WaterQuality/combined_lab_field_may_sep.csv")
burned_lakes <- subset(waterquality, Type=='Burned')
control_lakes <- subset(waterquality, Type=='Control')
lakenames <- waterquality[,c("lagoslakeid","Site")]

#### Define constants ####
min_pixels <- 1
data_months <- c(5,6,7,8,9)

#### Main program ####
burned_lakes_IDs <- unique(burned_lakes$lagoslakeid)
control_lakes_IDs <- unique(control_lakes$lagoslakeid)

# testing
# burned <- read.csv.sql(file="C:/Users/immcc/Documents/SplashNBurn/Data/LAGOS/LAGOS_US_chlorophyll_predictions_v1.csv",
#                        sql= "select * from file where lagoslakeid ='223' OR lagoslakeid = '27717'", header=T, sep=",")

# extract only rows from huge LANDSAT data frame that match burned or control lakes
burnedall <- read.csv.sql(file="C:/Users/immcc/Documents/SplashNBurn/Data/LAGOS/LAGOS_US_chlorophyll_predictions_v1.csv",
                       sql= "select * from file WHERE lagoslakeid IN (51824,14101,223,113774,66287,27717,
                       38199,414,430,66295,50749,29814,15139,73430,133329)", 
                       header=T, sep=",")

controlall <- read.csv.sql(file="C:/Users/immcc/Documents/SplashNBurn/Data/LAGOS/LAGOS_US_chlorophyll_predictions_v1.csv",
                          sql= "select * from file WHERE lagoslakeid IN (63,139473,57275,2867,37142,121215,
                       20349,3839,59050,3550,58010,21441,127093,30871,102493)", 
                          header=T, sep=",")
## Burned lakes wrangling
# reformat table
burnedall_df <- burnedall[,c(2:7)]
burnedall_df$FullDate <- lubridate::parse_date_time(burnedall_df$SENSING_TIME, orders='ymdHMS')
burnedall_df$Date <- round_date(burnedall_df$FullDate, unit='day')
burnedall_df$Month <- month(burnedall_df$Date)
burnedall_df$Year <- year(burnedall_df$Date)

# keep only desired data
burnedall_df <- subset(burnedall_df, Pixelcount >= min_pixels)
burnedall_df <- subset(burnedall_df, Month %in% data_months)
burnedall_df <- subset(burnedall_df, neg_reflectance_flag=='FALSE')
burnedall_df <- merge(burnedall_df, lakenames, by='lagoslakeid')

# create individual lake summaries
# burned_summary <- burnedall_df %>%
#   group_by(lagoslakeid, Year) %>%
#   summarize(min=min(chl_log10_pred, na.rm=T), median=median(chl_log10_pred, na.rm=T),
#             mean=mean(chl_log10_pred, na.rm=T), max=max(chl_log10_pred), nSamples=n(), .groups='keep')

burned_lakes_IDs_left <- unique(burnedall_df$lagoslakeid)
burned_lakes_list <- list()

for (i in 1:length(burned_lakes_IDs_left)){
  xx <- subset(burnedall_df, lagoslakeid==burned_lakes_IDs_left[[i]])
  xxSummary <- xx %>%
    group_by(Year) %>%
    summarize(min_logChl=min(chl_log10_pred, na.rm=T), median_logChl=median(chl_log10_pred, na.rm=T),
              mean_logChl=mean(chl_log10_pred, na.rm=T), max_logChl=max(chl_log10_pred, na.rm=T),
              nSamples=n())
  xxSummary$lagoslakeid <- burned_lakes_IDs_left[[i]]
  xxSummary$Type <- 'Burned'
  xxSummary$Site <- xx$Site[1]
  burned_lakes_list[[i]] <- xxSummary
  xxdf <- as.data.frame(burned_lakes_list[[i]])
  filename <- paste0('Data/LAGOS/LANDSAT_timeseries/LANDSAT_logchl_timeseries_MaySep_',burned_lakes_IDs_left[[i]],".csv")
  write.csv(xxdf, filename, row.names=F)
  xx=NULL
  xxSummary=NULL
  xxdf=NULL
  filename=NULL
}

## Control lakes wrangling
# reformat table
controlall_df <- controlall[,c(2:7)]
controlall_df$FullDate <- lubridate::parse_date_time(controlall_df$SENSING_TIME, orders='ymdHMS')
controlall_df$Date <- round_date(controlall_df$FullDate, unit='day')
controlall_df$Month <- month(controlall_df$Date)
controlall_df$Year <- year(controlall_df$Date)

# keep only desired data
controlall_df <- subset(controlall_df, Pixelcount >= min_pixels)
controlall_df <- subset(controlall_df, Month %in% data_months)
controlall_df <- subset(controlall_df, neg_reflectance_flag=='FALSE')
controlall_df <- merge(controlall_df, lakenames, by='lagoslakeid')

# create individual lake summaries
control_lakes_IDs_left <- unique(controlall_df$lagoslakeid)
control_lakes_list <- list()

for (i in 1:length(control_lakes_IDs_left)){
  xx <- subset(controlall_df, lagoslakeid==control_lakes_IDs_left[[i]])
  xxSummary <- xx %>%
    group_by(Year) %>%
    summarize(min_logChl=min(chl_log10_pred, na.rm=T), median_logChl=median(chl_log10_pred, na.rm=T),
              mean_logChl=mean(chl_log10_pred, na.rm=T), max_logChl=max(chl_log10_pred, na.rm=T),
              nSamples=n())
  xxSummary$lagoslakeid <- control_lakes_IDs_left[[i]]
  xxSummary$Type <- 'Control'
  xxSummary$Site <- xx$Site[1]
  control_lakes_list[[i]] <- xxSummary
  xxdf <- as.data.frame(control_lakes_list[[i]])
  filename <- paste0('Data/LAGOS/LANDSAT_timeseries/LANDSAT_logchl_timeseries_MaySep_',control_lakes_IDs_left[[i]],".csv")
  write.csv(xxdf, filename, row.names=F)
  xx=NULL
  xxSummary=NULL
  xxdf=NULL
  filename=NULL
}
