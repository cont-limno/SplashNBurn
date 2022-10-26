##################### Exploring weather station data ##########################
# Date: 8-24-22
# updated: 10-26-22; add sample dates to plots
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(dplyr)
library(weathermetrics)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")
#station <- read.csv("Data/weatherstation/kestrelmet_download_all_8.24.22.csv")
station <- read.csv("Data/weatherstation/ambient-weather-20220524-20220926.csv")

#### Main program ####
# get rid of unwanted variables
station <- station[,c(2,3,6,7,8,9,10,11,13)]
colnames(station) <- c('DateTime','AirTemp_F','WindSpeed_mph','WindGust_mph','WindDirection',
                       'HourlyRain_inhr','DailyRain_in','Rain24hr_in','OutdoorHumidity_pct')

# deal with date column
station$Date <- as.Date(station$DateTime) #can leave DateTime as character for now; don't really need it

# convert to metric
station$AirTemp_C <- fahrenheit.to.celsius(station$AirTemp_F)

station$WindSpeed_kmph <- convert_wind_speed(station$WindSpeed_mph, old_metric='mph', new_metric='kmph')
station$WindGust_kmph <- convert_wind_speed(station$WindGust_mph, old_metric='mph', new_metric='kmph')

station$HourlyRain_mmhr <- inches_to_metric(station$HourlyRain_inhr, unit='mm')
station$DailyRain_mm <- inches_to_metric(station$DailyRain_in, unit='mm')# this is the column we want; appears to be cumulative total of rain for a given day; 24 hour column is the last 24 hours, so not a calendar day
station$Rain24hr_mm <- inches_to_metric(station$Rain24hr_in, unit='mm')

## calculate daily summaries
weather_summary <- station %>%
  group_by(Date) %>%
  summarize(AirTempC_dailymax=max(AirTemp_C, na.rm=T),
            AirTempC_dailymin=min(AirTemp_C, na.rm=T),
            AirTempC_dailymean=mean(AirTemp_C, na.rm=T),
            Precipmm_dailytotal=max(DailyRain_mm, na.rm=T),
            WindSpeed_kmph_dailymax=max(WindSpeed_kmph, na.rm=T),
            WindSpeed_kmph_dailymin=min(WindSpeed_kmph, na.rm=T),
            WindSpeed_kmph_dailymean=mean(WindSpeed_kmph, na.rm=T),
            WindGust_kmph_dailymax=max(WindGust_kmph, na.rm=T),
            WindGust_kmph_dailymin=min(WindGust_kmph, na.rm=T),
            WindGust_kmph_dailymean=mean(WindGust_kmph, na.rm=T),
            nReadings=n()) #possibly helpful if need to eliminate partial days

## plotting
# air temp
jpeg('Figures/airtemp_plot.jpeg',width = 7,height = 5,units = 'in',res=600)
  plot(AirTempC_dailymax ~ Date, data=weather_summary, pch=20, col='firebrick', 
     ylim=c(0,40), ylab='Air temperature (°C)', xaxt='n', xlim=c(as.Date("2022/05/17"), as.Date("2022/09/27")), 
     xlab='', main='Daily Air Temperature')
  axis.Date(1,Day,at=seq(as.Date("2022/05/17"), as.Date("2022/09/27"),by="weeks"),
                         labels=seq(as.Date("2022/05/17"), as.Date("2022/09/27"),by="weeks"), cex.axis=0.8, las=2)
  lines(AirTempC_dailymax ~ Date, data=weather_summary, col='firebrick')
  points(AirTempC_dailymean ~ Date, data=weather_summary, pch=20, col='black')
  lines(AirTempC_dailymean ~ Date, data=weather_summary, pch=20, col='black')
  points(AirTempC_dailymin ~ Date, data=weather_summary, pch=20, col='dodgerblue')
  lines(AirTempC_dailymin ~ Date, data=weather_summary, pch=20, col='dodgerblue')
  legend('topright', legend=c('Max','Mean','Min'), col=c('firebrick','black','dodgerblue'),
       pch=c(16,16,16), horiz=T, bty='n')
  abline(v=as.Date("2022/05/19"), lty=2) #median day of sample trips
  abline(v=as.Date("2022/06/09"), lty=2) 
  abline(v=as.Date("2022/07/15"), lty=2)
  abline(v=as.Date("2022/08/23"), lty=2)
  abline(v=as.Date("2022/09/23"), lty=2)
dev.off()

# precip
jpeg('Figures/precip_plot.jpeg',width = 7,height = 5,units = 'in',res=600)
  plot(Precipmm_dailytotal ~ Date, data=weather_summary, col='navy', type='l', lwd=1.5,
     ylim=c(0,30), ylab='Precipitation (mm)', xaxt='n', xlim=c(as.Date("2022/05/17"), as.Date("2022/09/27")),
     xlab='', main='Daily Precipitation')
  axis.Date(1,Day,at=seq(as.Date("2022/05/17"), as.Date("2022/09/27"),by="weeks"), 
            labels=seq(as.Date("2022/05/17"), as.Date("2022/09/27"),by="weeks"), cex.axis=0.8, las=2)
  abline(v=as.Date("2022/05/19"), lty=2) #median day of sample trips
  abline(v=as.Date("2022/06/09"), lty=2) 
  abline(v=as.Date("2022/07/15"), lty=2)
  abline(v=as.Date("2022/08/23"), lty=2)
  abline(v=as.Date("2022/09/23"), lty=2)
dev.off()