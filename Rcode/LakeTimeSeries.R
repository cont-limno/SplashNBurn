################### Exploring lake times series data ##########################
# Date: 8-25-22
# updated:
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")

may_june_july <- read.csv("Data/WaterQuality/may_june_july.csv")

#### Main program ####
all_lakes <- unique(may_june_july$Site) # string of all lake names
control_lakes <- subset(may_june_july, Type=='control') # subset of just control lakes
control_lakes <- unique(control_lakes$Site) # string of control lake names
burned_lakes <- subset(may_june_july, Type=='sample') # subset of just burned lakes
burned_lakes <- unique(burned_lakes$Site) # string of burned lake names

# create an empty plot; will fill in lines/points with for loop
plot(TP ~ Month_Order, data=may_june_july, pch=20, xaxt='n', ylab='Total phosphorus (ppb)', 
     xlab='Month', ylim=c(0,100), main='Total phosphorus time series by lake', type='n')
axis(1, at=seq(5,7,1), labels=c('May','June','July'))

# iteratively add line for each lake by looping through burned_lakes string
for (i in 1:length(burned_lakes)) {
  single_lake <- subset(may_june_july, Site==burned_lakes[i])
  lines(TP ~ Month_Order, data=single_lake, lwd=1.5, col='firebrick', type='b')
  single_lake <- NULL #remove intermediate output so can overwrite with next lake
}

# same for control
for (i in 1:length(control_lakes)) {
  single_lake <- subset(may_june_july, Site==control_lakes[i])
  lines(TP ~ Month_Order, data=single_lake, lwd=1.5, col='dodgerblue', type='b')
  single_lake <- NULL
}
