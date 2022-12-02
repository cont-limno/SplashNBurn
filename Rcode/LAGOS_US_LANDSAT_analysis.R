################# LAGOS-US-LANDSAT v1.0 chlorophyll-a analysis ##############
# Date: 11-1-22
# updated: 12-2-22; added 2022 sample data
# Author: Ian McCullough, immccull@gmail.com
###############################################################################
## LAGOS-US-LANDSAT v1.0 chlorophyll-a estimates
# individual lake time series processed in LAGOS_US_LANDSAT_processing.R

#### R libraries ####
library(dplyr)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")
LOCUS <- read.csv("Data/LAGOS/LAGOS_LOCUS_Table.csv")
waterquality <- read.csv("Data/WaterQuality/combined_lab_field_may_sep.csv")

#### Main program ####
# basic table wrangling
lake_table <- LOCUS[,c('lagoslakeid','Type','Site')]
lake_table$Type <- ifelse(lake_table$Type=='sample','burned', lake_table$Type)
burned_lakes <- subset(lake_table, Type=='burned')
control_lakes <- subset(lake_table, Type=='control')
burned_lakes_IDs <- burned_lakes$lagoslakeid
control_lakes_IDs <- control_lakes$lagoslakeid

# chlorophyll samples
chla_samples <- waterquality[,c('Site','Month_factor','Chloro_ppb','logChloro','lagoslakeid')]
chla_2022 <- chla_samples %>%
  group_by(lagoslakeid) %>%
  summarize(min_logChl=min(logChloro, na.rm=T),
    median_logChl=median(logChloro, na.rm=T),
    mean_logChl=mean(logChloro, na.rm=T),
    max_logChl=max(logChloro, na.rm=T), nSamples=n())

chla_2022$Year <- 2022
chla_2022$Type <- ifelse(chla_2022$lagoslakeid %in% burned_lakes_IDs, 'Burned','Control')
chla_2022 <- merge(chla_2022, lake_table[,c(1,3)], by='lagoslakeid')
chla_2022 <- chla_2022[,c(7,2,3,4,5,6,1,8,9)] 

# read in LANDSAT time series
csv_list <- list.files("Data/LAGOS/LANDSAT_timeseries", pattern='.csv')
landsat_timeseries <- list()
for (i in 1:length(csv_list)){
  xx <- read.csv(paste0("Data/LAGOS/LANDSAT_timeseries/",csv_list[[i]]))
  #xx <- merge(xx, lake_table[,c(1,3)], by='lagoslakeid')
  landsat_timeseries[[i]] <- xx
  xx=NULL
}

# loop through lakes, plotting annual chla
test <- landsat_timeseries[[4]]
testlakeid <- test$lagoslakeid[1]
sample22 <- subset(chla_2022, lagoslakeid==testlakeid)
fulldata <- rbind.data.frame(test, sample22)
plot(median_logChl ~ Year, data=fulldata, type='b', pch=20,
     xlab='Year', ylab='log Chla', col='black', ylim=c(0,3))
lines(min_logChl ~ Year, data=fulldata, type='b', pch=20, col='blue')
lines(max_logChl ~ Year, data=fulldata, type='b', pch=20, col='red')
abline(v=2021, lty=2)
legend('topleft', legend=c('Min','Median','Max'), pch=c(16,16,16), 
       col=c('blue','black','red'), bty='n', horiz=T)
mtext(side=3, test$Type[1])
title(test$Site[1])

for (i in 1:length(landsat_timeseries)){
  df <- landsat_timeseries[[i]]
  lakeid <- df$lagoslakeid[1]
  sample22 <- subset(chla_2022, lagoslakeid==lakeid)
  df <- rbind.data.frame(df, sample22)
  plot(median_logChl ~ Year, data=df, type='b', pch=20, lwd=2, las=1,
       xlab='Year', ylab='log Chla', col='black', ylim=c(0,3))
  lines(min_logChl ~ Year, data=df, type='b', pch=20, col='blue', lwd=2)
  lines(max_logChl ~ Year, data=df, type='b', pch=20, col='red', lwd=2)
  abline(v=2021, lty=2)
  legend('topleft', legend=c('Min','Median','Max'), pch=c(16,16,16), 
         col=c('blue','black','red'), bty='n', horiz=T)
  mtext(side=3, df$Type[1])
  title(df$Site[1])
  df=NULL
  lakeid=NULL
  sample22=NULL
}
