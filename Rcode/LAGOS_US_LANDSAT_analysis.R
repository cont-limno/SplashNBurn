################# LAGOS-US-LANDSAT v1.0 chlorophyll-a analysis ##############
# Date: 11-1-22
# updated:
# Author: Ian McCullough, immccull@gmail.com
###############################################################################
## LAGOS-US-LANDSAT v1.0 chlorophyll-a estimates
# individual lake time series processed in LAGOS_US_LANDSAT_processing.R

#### R libraries ####

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")
LOCUS <- read.csv("Data/LAGOS/LAGOS_LOCUS_Table.csv")

#### Main program ####
# basic table wrangling
lake_table <- LOCUS[,c('lagoslakeid','Type','Site')]
lake_table$Type <- ifelse(lake_table$Type=='sample','burned', lake_table$Type)
burned_lakes <- subset(lake_table, Type=='burned')
control_lakes <- subset(lake_table, Type=='control')
burned_lakes_IDs <- burned_lakes$lagoslakeid
control_lakes_IDs <- control_lakes$lagoslakeid

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
plot(median_logChl ~ Year, data=test, type='b', pch=20,
     xlab='Year', ylab='log Chla', col='black', ylim=c(0,3))
lines(min_logChl ~ Year, data=test, type='b', pch=20, col='blue')
lines(max_logChl ~ Year, data=test, type='b', pch=20, col='red')
legend('topright', legend=c('Min','Median','Max'), pch=c(16,16,16), 
       col=c('blue','black','red'), bty='n', horiz=T)
mtext(side=3, test$Type[1])
title(test$Site[1])

for (i in 1:length(landsat_timeseries)){
  df <- landsat_timeseries[[i]]
  plot(median_logChl ~ Year, data=df, type='l', pch=20, lwd=2, las=1,
       xlab='Year', ylab='log Chla', col='black', ylim=c(0,2))
  lines(min_logChl ~ Year, data=df, type='l', pch=20, col='blue', lwd=2)
  lines(max_logChl ~ Year, data=df, type='l', pch=20, col='red', lwd=2)
  legend('topright', legend=c('Min','Median','Max'), pch=c(16,16,16), 
         col=c('blue','black','red'), bty='n', horiz=T)
  mtext(side=3, df$Type[1])
  title(df$Site[1])
  df=NULL
}
