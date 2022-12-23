##################### Exploring NOAA climate data #############################
# Date: 8-25-22
# updated: 12-23-22; add dplyr::
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(dplyr)
library(lubridate)
library(weathermetrics)
library(zyp)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")
precip <- read.csv("Data/NOAA_climate/MN-075-pcp-all-9-1895-2022.csv")[c(5:1535),]
tmax <- read.csv("Data/NOAA_climate/MN-075-tmax-all-9-1895-2022.csv")[c(5:1535),]
tmin <- read.csv("Data/NOAA_climate/MN-075-tmin-all-9-1895-2022.csv")[c(5:1535),]
tmean <- read.csv("Data/NOAA_climate/MN-075-tavg-all-9-1895-2022.csv")[c(5:1535),]

#### Main program ####
## precip
colnames(precip) <- c('Date','Precip_in','Precip_anomaly_in')
precip$Date <- ym(precip$Date)
precip$Year <- year(precip$Date)
precip$Month <- month(precip$Date, label=T)
precip$WaterYear <- ifelse(precip$Month %in% c(10,11,12), precip$Year+1, precip$Year)

precip$Precip_mm <- inches_to_metric(as.numeric(precip$Precip_in), unit='mm')
precip$Precip_anomaly_mm <- as.numeric(precip$Precip_anomaly_in) * 25.4 #can't use inches_to_metric...messes up with negative values

precip <- precip[,c(1,5,4,6,7,8)]

precip_wy <- precip %>%
  dplyr::group_by(WaterYear) %>%
  dplyr::summarize(WY_total=sum(Precip_mm), nMonths=n())
precip_wy <- subset(precip_wy, WaterYear < 2022) #2022 is incomplete year
precip_base <- subset(precip_wy, WaterYear %in% seq(1991,2020,1)) 
precip_wymean <- mean(precip_base$WY_total, na.rm=T)

jpeg('Figures/NOAA_climate/wyprecip_18952021.jpeg',width = 7,height = 5,units = 'in',res=600)
plot(WY_total ~ WaterYear, data=precip_wy, type='b', col='navy', pch=20, xlab='Water year',
     ylab='Precipitation (mm)', las=2, xaxt='n', ylim=c(400,1100), main='Total precipitation')
mtext('Lake County, MN', side=3)
axis(1, at=seq(1895,2025,5), labels=seq(1895,2025,5), las=2)
abline(h=precip_wymean, lwd=2, col='black', lty=2)
abline(v=2021, lwd=2, col='firebrick', lty=1)
legend('topleft', legend=c('1991-2020 avg','Greenwood fire'), col=c('black','firebrick'), lwd=c(2,2), lty=c(2,1), 
       cex=0.75, horiz=T, bty='n')
dev.off()

precip_anomaly_wy <- precip %>%
  dplyr::group_by(WaterYear) %>%
  dplyr::summarize(WY_anomaly_total=sum(Precip_anomaly_mm), nMonths=n())
precip_anomaly_wy <- subset(precip_anomaly_wy, WaterYear < 2022)

plot(WY_anomaly_total ~ WaterYear, data=precip_anomaly_wy, type='b', col='navy', pch=20, xlab='Water year',
     ylab='Precipitation anomaly (mm)', las=2, xaxt='n', ylim=c(), main='Precipitation anomaly')
mtext('Lake County, MN', side=3)
axis(1, at=seq(1895,2025,5), labels=seq(1895,2025,5), las=2)
abline(v=2021, lwd=2, col='firebrick', lty=1)
abline(h=0, lwd=2, col='black', lty=2)

## air temp
# tmax
colnames(tmax) <- c('Date','Tmax_F','Tmax_anomaly_F')
tmax$Date <- ym(tmax$Date)
tmax$Year <- year(tmax$Date)
tmax$Month <- month(tmax$Date, label=T)
tmax$WaterYear <- ifelse(tmax$Month %in% c(10,11,12), tmax$Year+1, tmax$Year)

tmax$tmax_C <- fahrenheit.to.celsius(as.numeric(tmax$Tmax_F))

tmax$tmaxavgF <- as.numeric(tmax$Tmax_F)- as.numeric(tmax$Tmax_anomaly_F)
tmax$tmaxavgC <- fahrenheit.to.celsius(tmax$tmaxavgF)
tmax$tmax_anomaly_C <- tmax$tmaxavgC - tmax$tmax_C

tmax <- tmax[,c(1,5,4,6,7,10)]

# tmean
colnames(tmean) <- c('Date','Tmean_F','Tmean_anomaly_F')
tmean$Date <- ym(tmean$Date)
tmean$Year <- year(tmean$Date)
tmean$Month <- month(tmean$Date, label=T)
tmean$WaterYear <- ifelse(tmean$Month %in% c(10,11,12), tmean$Year+1, tmean$Year)

tmean$tmean_C <- fahrenheit.to.celsius(as.numeric(tmean$Tmean_F))

tmean$tmeanavgF <- as.numeric(tmean$Tmean_F)- as.numeric(tmean$Tmean_anomaly_F)
tmean$tmeanavgC <- fahrenheit.to.celsius(tmean$tmeanavgF)
tmean$tmean_anomaly_C <- tmean$tmeanavgC - tmean$tmean_C

tmean <- tmean[,c(1,5,4,6,7,10)]

# tmin
colnames(tmin) <- c('Date','Tmin_F','Tmin_anomaly_F')
tmin$Date <- ym(tmin$Date)
tmin$Year <- year(tmin$Date)
tmin$Month <- month(tmin$Date, label=T)
tmin$WaterYear <- ifelse(tmin$Month %in% c(10,11,12), tmin$Year+1, tmin$Year)

tmin$tmin_C <- fahrenheit.to.celsius(as.numeric(tmin$Tmin_F))

tmin$tminavgF <- as.numeric(tmin$Tmin_F)- as.numeric(tmin$Tmin_anomaly_F)
tmin$tminavgC <- fahrenheit.to.celsius(tmin$tminavgF)
tmin$tmin_anomaly_C <- abs(tmin$tminavgC) - abs(tmin$tmin_C)

tmin <- tmin[,c(1,5,4,6,7,10)]

## all air temp
airtemp <- merge(tmax, tmin, by='Date')
airtemp <- merge(airtemp, tmean, by='Date')
airtemp <- airtemp[,c(1:4,5,6,10,11,15,16)]
colnames(airtemp)[c(2:4)] <- c('Month','Year','WaterYear')

airtemp_wy <- airtemp %>%
  dplyr::group_by(WaterYear) %>%
  dplyr::summarize(WY_tmax=mean(tmax_C, na.rm=T),
            WY_tmin=mean(tmin_C, na.rm=T),
            WY_tmean=mean(tmean_C, na.rm=T), nMonths=n())
airtemp_wy <- subset(airtemp_wy, WaterYear < 2022)

airtemp_base <- subset(airtemp_wy, WaterYear %in% seq(1991,2020,1)) 
airtemp_wymean <- mean(airtemp_base$WY_tmean, na.rm=T)
airtemp_wymax <- mean(airtemp_base$WY_tmax, na.rm=T)
airtemp_wymin <- mean(airtemp_base$WY_tmin, na.rm=T)

airtemp_anomaly_wy <- airtemp %>%
  dplyr::group_by(WaterYear) %>%
  dplyr::summarize(WY_tmax_anomaly=mean(tmax_anomaly_C, na.rm=T),
            WY_tmin_anomaly=mean(tmin_anomaly_C, na.rm=T),
            WY_tmean_anomaly=mean(tmean_anomaly_C, na.rm=T), nMonths=n())
airtemp_anomaly_wy <- subset(airtemp_anomaly_wy, WaterYear < 2022)

plot(WY_tmean ~ WaterYear, data=airtemp_wy, type='b', col='black', pch=20, xlab='Water year',
     ylab='Temperature (C)', las=2, xaxt='n', ylim=c(), main='Mean air temperature')
mtext('Lake County, MN', side=3)
axis(1, at=seq(1895,2025,5), labels=seq(1895,2025,5), las=2)
abline(h=airtemp_wymean, lwd=2, col='black', lty=2)
abline(v=2021, lwd=2, col='firebrick', lty=2)
#legend('bottomright', legend='1991-2020 normal', col='black', lwd=2, lty=2, bty='n')

plot(WY_tmax ~ WaterYear, data=airtemp_wy, type='b', col='black', pch=20, xlab='Water year',
     ylab='Temperature (C)', las=2, xaxt='n', ylim=c(), main='Max air temperature')
mtext('Lake County, MN', side=3)
axis(1, at=seq(1895,2025,5), labels=seq(1895,2025,5), las=2)
abline(h=airtemp_wymax, lwd=2, col='black', lty=2)
abline(v=2021, lwd=2, col='firebrick', lty=2)
#legend('bottomright', legend='1991-2020 normal', col='black', lwd=2, lty=2, bty='n')

plot(WY_tmin ~ WaterYear, data=airtemp_wy, type='b', col='black', pch=20, xlab='Water year',
     ylab='Temperature (C)', las=2, xaxt='n', ylim=c(), main='Min air temperature')
mtext('Lake County, MN', side=3)
axis(1, at=seq(1895,2025,5), labels=seq(1895,2025,5), las=2)
abline(h=airtemp_wymin, lwd=2, col='black', lty=2)
abline(v=2021, lwd=2, col='firebrick', lty=2)
#legend('bottomright', legend='1991-2020 normal', col='black', lwd=2, lty=2, bty='n')

# all max, mean and min at once
plot(WY_tmean ~ WaterYear, data=airtemp_wy, type='b', col='black', pch=20, xlab='Water year',
     ylab='Temperature (C)', las=2, xaxt='n', ylim=c(-10,15), main='Air temperature')
mtext('Lake County, MN', side=3)
axis(1, at=seq(1895,2025,5), labels=seq(1895,2025,5), las=2)
abline(h=airtemp_wymean, lwd=2, col='black', lty=2)
abline(v=2021, lwd=2, col='firebrick', lty=1)

lines(WY_tmin ~ WaterYear, data=airtemp_wy, type='b', col='dodgerblue', pch=20)
lines(WY_tmax ~ WaterYear, data=airtemp_wy, type='b', col='gold', pch=20)
abline(h=airtemp_wymax, lwd=2, col='gold', lty=2)
abline(h=airtemp_wymin, lwd=2, col='dodgerblue', lty=2)
legend('topleft', legend=c('1991-2020 avg', 'Greenwood fire'), col=c('black','firebrick'), lwd=c(2,2), lty=c(2,1), bty='n', 
       cex=0.75, horiz=T)

## plot airtemp anomalies
plot(WY_tmean_anomaly ~ WaterYear, data=airtemp_anomaly_wy, type='b', col='black', pch=20, xlab='Water year',
     ylab='Temperature anomaly (C)', las=2, xaxt='n', ylim=c(), main='Mean air temperature anomaly')
mtext('Lake County, MN', side=3)
axis(1, at=seq(1895,2025,5), labels=seq(1895,2025,5), las=2)
abline(h=0, lwd=2, col='black', lty=2)
abline(v=2021, lwd=2, col='firebrick', lty=1)

plot(WY_tmax_anomaly ~ WaterYear, data=airtemp_anomaly_wy, type='b', col='gold', pch=20, xlab='Water year',
     ylab='Temperature anomaly (C)', las=2, xaxt='n', ylim=c(), main='Max air temperature anomaly')
mtext('Lake County, MN', side=3)
axis(1, at=seq(1895,2025,5), labels=seq(1895,2025,5), las=2)
abline(h=0, lwd=2, col='black', lty=2)
abline(v=2021, lwd=2, col='firebrick', lty=1)

plot(WY_tmin_anomaly ~ WaterYear, data=airtemp_anomaly_wy, type='b', col='dodgerblue', pch=20, xlab='Water year',
     ylab='Temperature anomaly (C)', las=2, xaxt='n', ylim=c(), main='Min air temperature anomaly')
mtext('Lake County, MN', side=3)
axis(1, at=seq(1895,2025,5), labels=seq(1895,2025,5), las=2)
abline(h=0, lwd=2, col='black', lty=2)
abline(v=2021, lwd=2, col='firebrick', lty=1)

## How about summer only temperatures?
airtemp$Season <- ifelse(airtemp$Month %in% c('Sep','Oct','Nov'), 'Fall', NA)
airtemp$Season <- ifelse(airtemp$Month %in% c('Dec','Jan','Feb'), 'Winter', airtemp$Season)
airtemp$Season <- ifelse(airtemp$Month %in% c('Mar','Apr','May'), 'Spring', airtemp$Season)
airtemp$Season <- ifelse(airtemp$Month %in% c('Jun','Jul','Aug'), 'Summer', airtemp$Season)

airtemp_summer <- subset(airtemp, Season=='Summer')
airtemp_summery <- airtemp_summer %>%
  dplyr::group_by(WaterYear) %>%
  dplyr::summarize(summer_tmean=mean(tmean_C),
            summer_tmax=mean(tmax_C),
            summer_tmin=mean(tmin_C),
            nMonths=n())
airtemp_summery <- subset(airtemp_summery, WaterYear < 2022)

summer_base <- subset(airtemp_summery, WaterYear %in% seq(1991,2020,1))
summer_tmeanbase <- mean(summer_base$summer_tmean) 
summer_tmaxbase <- mean(summer_base$summer_tmax)
summer_tminbase <- mean(summer_base$summer_tmin)

jpeg('Figures/NOAA_climate/summertmean_18952021.jpeg',width = 7,height = 5,units = 'in',res=600)
plot(summer_tmean ~ WaterYear, data=airtemp_summery, type='b', col='black', pch=20, xlab='Water year',
     ylab='Temperature (?C)', las=2, xaxt='n', ylim=c(14,19), main='Mean summer air temperature')
mtext('Lake County, MN', side=3)
axis(1, at=seq(1895,2025,5), labels=seq(1895,2025,5), las=2)
abline(h=summer_tmeanbase, lwd=2, col='black', lty=2)
abline(v=2021, lwd=2, col='firebrick', lty=1)
legend('topleft', legend=c('1991-2020 avg', 'Greenwood fire'), col=c('black','firebrick'), lwd=c(2,2),
       lty=c(2,1), horiz=T, cex=0.75, bty='n')
dev.off()

plot(summer_tmax ~ WaterYear, data=airtemp_summery, type='b', col='gold', pch=20, xlab='Water year',
     ylab='Temperature (C)', las=2, xaxt='n', ylim=c(), main='Max summer air temperature')
mtext('Lake County, MN', side=3)
axis(1, at=seq(1895,2025,5), labels=seq(1895,2025,5), las=2)
abline(h=summer_tmaxbase, lwd=2, col='gold', lty=2)
abline(v=2021, lwd=2, col='firebrick', lty=1)

plot(summer_tmin ~ WaterYear, data=airtemp_summery, type='b', col='dodgerblue', pch=20, xlab='Water year',
     ylab='Temperature (C)', las=2, xaxt='n', ylim=c(), main='Min summer air temperature')
mtext('Lake County, MN', side=3)
axis(1, at=seq(1895,2025,5), labels=seq(1895,2025,5), las=2)
abline(h=summer_tminbase, lwd=2, col='black', lty=2)
abline(v=2021, lwd=2, col='firebrick', lty=1)

#### Are we picking up a climate change signal? ##
## precip #actually seems to be getting slightly wetter
zyp.sen(WY_total ~ WaterYear, precip_wy)
attach(precip_wy)
zyp.trend.vector(WY_total, method='yuepilon')
detach(precip_wy)

# recent past 30 years; not picking up signal for precip
precip_wy_19912020 <- subset(precip_wy, WaterYear %in% seq(1991,2020,1))
zyp.sen(WY_total ~ WaterYear, precip_wy_19912020)
attach(precip_wy_19912020)
zyp.trend.vector(WY_total, method='yuepilon')
detach(precip_wy_19912020)

## airtemp; small but significant warming trend
zyp.sen(WY_tmean ~ WaterYear, airtemp_wy)
attach(airtemp_wy)
zyp.trend.vector(WY_tmean, method='yuepilon')
detach(airtemp_wy)

zyp.sen(WY_tmin ~ WaterYear, airtemp_wy)
attach(airtemp_wy)
zyp.trend.vector(WY_tmin, method='yuepilon')
detach(airtemp_wy)

zyp.sen(WY_tmax ~ WaterYear, airtemp_wy)
attach(airtemp_wy)
zyp.trend.vector(WY_tmax, method='yuepilon')
detach(airtemp_wy)

# recent past 30 years; indicating no significant signal for this period
airtemp_wy_19912020 <- subset(airtemp_wy, WaterYear %in% seq(1991,2020,1))

zyp.sen(WY_tmean ~ WaterYear, airtemp_wy_19912020)
attach(airtemp_wy_19912020)
zyp.trend.vector(WY_tmean, method='yuepilon')
detach(airtemp_wy_19912020)

zyp.sen(WY_tmax ~ WaterYear, airtemp_wy_19912020)
attach(airtemp_wy_19912020)
zyp.trend.vector(WY_tmax, method='yuepilon')
detach(airtemp_wy_19912020)

zyp.sen(WY_tmin ~ WaterYear, airtemp_wy_19912020)
attach(airtemp_wy_19912020)
zyp.trend.vector(WY_tmin, method='yuepilon')
detach(airtemp_wy_19912020)

# summer only
airtemp_summery_19912020 <- subset(airtemp_summery, WaterYear %in% seq(1991,2020,1))
zyp.sen(summer_tmean ~ WaterYear, airtemp_summery_19912020)
attach(airtemp_summery_19912020)
zyp.trend.vector(summer_tmean, method='yuepilon')
detach(airtemp_summery_19912020)