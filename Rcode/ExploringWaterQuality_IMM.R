####################### Exploring water quality data ##########################
# Date: 8-19-22
# updated:
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(ggplot2)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")
may_june_july <- read.csv("Data/WaterQuality/may_june_july.csv")

#### Main program ####
may_june_july$Month_factor <- factor(as.character(may_june_july$Month), levels = c('may', 'june', 'july'))

month_colors <- c('dodgerblue','tan','gold')

ggplot(may_june_july, aes(x = Lake, y = TP, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Total phosphorus (ppb)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_june_july, aes(x = Lake, y = TN, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Total nitrogen (ppb)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_june_july, aes(x = Lake, y = NO2NO3N, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "NO2/NO3-N (ppb)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_june_july, aes(x = Lake, y = NH4N, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "NH4-N (ppb)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_june_july, aes(x = Lake, y = DOC, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Dissolved organic carbon (ppm)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_june_july, aes(x = Lake, y = TSS, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Total suspended solids (mg/L)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_june_july, aes(x = Lake, y = ANCmg, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Acid neutralizing capacity (mg CaCO3/L)")+
  scale_fill_manual("Month", values=month_colors)
