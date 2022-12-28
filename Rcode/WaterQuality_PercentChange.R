################### Control vs. burned percent change #########################
# Date: 11-17-22
# updated: 12-27-22; recalculate with drainage vs isolated
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(dplyr)
library(tidyverse)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")

# water quality
waterquality <- read.csv("Data/WaterQuality/combined_lab_field_may_sep.csv")

#### Main program ####
month_order <- c('May','Jun','Jul','Aug','Sep')
common_y_limits <- c(-100,250)
common_y_breaks <- seq(-100,250,25)

## TP
TP <- waterquality[,c('TP_ppb','Month_factor','Type','ConnClass')]
TP_burned <- subset(TP, Type=='Burned')
TP_burned_drainage <- subset(TP_burned, ConnClass=='Drainage')
TP_burned_isolated <- subset(TP_burned, ConnClass=='Isolated')
TP_control <- subset(TP, Type=='Control')
TP_control_drainage <- subset(TP_control, ConnClass=='Drainage')
TP_control_isolated <- subset(TP_control, ConnClass=='Isolated')
TP_burned_median <- median(TP_burned$TP_ppb, na.rm=T)
TP_burned_drainage_median <- median(TP_burned_drainage$TP_ppb, na.rm=T)
TP_burned_isolated_median <- median(TP_burned_isolated$TP_ppb, na.rm=T)
TP_control_median <- median(TP_control$TP_ppb, na.rm=T)
TP_control_drainage_median <- median(TP_control_drainage$TP_ppb, na.rm=T)
TP_control_isolated_median <- median(TP_control_isolated$TP_ppb, na.rm=T)

TP_byMonth_burned <- TP_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TP_MedianBurned_all=median(TP_ppb, na.rm=T),
                   nTP_burned_all=sum(!is.na(TP_ppb)))

TP_byMonth_burned_drainage <- TP_burned_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TP_MedianBurned_drainage=median(TP_ppb, na.rm=T),
                   nTP_burned_drainage=sum(!is.na(TP_ppb)))

TP_byMonth_burned_isolated <- TP_burned_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TP_MedianBurned_isolated=median(TP_ppb, na.rm=T),
                   nTP_burned_isolated=sum(!is.na(TP_ppb)))

TP_byMonth_control <- TP_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TP_MedianControl_all=median(TP_ppb, na.rm=T),
                   nTP_control_all=sum(!is.na(TP_ppb)))

TP_byMonth_control_drainage <- TP_control_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TP_MedianControl_drainage=median(TP_ppb, na.rm=T),
                   nTP_control_drainage=sum(!is.na(TP_ppb)))

TP_byMonth_control_isolated <- TP_control_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TP_MedianControl_isolated=median(TP_ppb, na.rm=T),
                   nTP_control_isolated=sum(!is.na(TP_ppb)))

TP_merge_list <- list(TP_byMonth_burned, TP_byMonth_control, 
                      TP_byMonth_burned_drainage, TP_byMonth_control_drainage,
                      TP_byMonth_burned_isolated, TP_byMonth_control_isolated)
#TP_byMonth <- merge(TP_byMonth_burned, TP_byMonth_control, by='Month_factor', all=T)
TP_byMonth <- TP_merge_list %>% reduce(full_join, by='Month_factor')

TP_byMonth <- TP_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
TP_byMonth <- as.data.frame(TP_byMonth)
TP_byMonth[6,] <- c('All months', TP_burned_median, sum(TP_byMonth$nTP_burned_all), 
                    TP_control_median, sum(TP_byMonth$nTP_control_all),
                    TP_burned_drainage_median, sum(TP_byMonth$nTP_burned_drainage),
                    TP_control_drainage_median, sum(TP_byMonth$nTP_control_drainage),
                    TP_burned_isolated_median, sum(TP_byMonth$nTP_burned_isolated),
                    TP_control_isolated_median, sum(TP_byMonth$nTP_control_isolated))
TP_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
TP_byMonth[,c(2:13)] <- TP_byMonth[,c(2:13)] %>% mutate_if(is.character, as.numeric)
TP_byMonth$TP_MedianDiffPct <- ((TP_byMonth$TP_MedianBurned_all - TP_byMonth$TP_MedianControl_all)/TP_byMonth$TP_MedianControl_all)*100
TP_byMonth$TP_MedianDiffAbs <-  TP_byMonth$TP_MedianBurned_all - TP_byMonth$TP_MedianControl_all
TP_byMonth$TP_MedianDrainageDiffPct <- ((TP_byMonth$TP_MedianBurned_drainage - TP_byMonth$TP_MedianControl_drainage)/TP_byMonth$TP_MedianControl_drainage)*100
TP_byMonth$TP_MedianDrainageDiffAbs <-  TP_byMonth$TP_MedianBurned_drainage - TP_byMonth$TP_MedianControl_drainage
TP_byMonth$TP_MedianIsolatedDiffPct <- ((TP_byMonth$TP_MedianBurned_isolated - TP_byMonth$TP_MedianControl_isolated)/TP_byMonth$TP_MedianControl_isolated)*100
TP_byMonth$TP_MedianIsolatedDiffAbs <-  TP_byMonth$TP_MedianBurned_isolated - TP_byMonth$TP_MedianControl_isolated

#TP_byMonth[6,3] <- sum(TP_byMonth$nTP_burned, na.rm=T)
#TP_byMonth[6,5] <- sum(TP_byMonth$nTP_control, na.rm=T)
TP_formatted <- data.frame(Month=c(month_order, 'All months'), 
                           TP_burned_drainage=paste0(TP_byMonth$TP_MedianBurned_drainage, ', ', TP_byMonth$nTP_burned_drainage),
                           TP_control_drainage=paste0(TP_byMonth$TP_MedianControl_drainage, ', ', TP_byMonth$nTP_control_drainage),
                           TP_diff_drainage=paste0(TP_byMonth$TP_MedianDrainageDiffAbs, ', ', round(TP_byMonth$TP_MedianDrainageDiffPct,2), '%'),
                           TP_burned_isolated=paste0(TP_byMonth$TP_MedianBurned_isolated, ', ', TP_byMonth$nTP_burned_isolated),
                           TP_control_isolated=paste0(TP_byMonth$TP_MedianControl_isolated, ', ', TP_byMonth$nTP_control_isolated),
                           TP_diff_isolated=paste0(TP_byMonth$TP_MedianIsolatedDiffAbs, ', ', round(TP_byMonth$TP_MedianIsolatedDiffPct,2), '%'),
                           TP_burned_all=paste0(TP_byMonth$TP_MedianBurned_all, ', ', TP_byMonth$nTP_burned_all),
                           TP_control_all=paste0(TP_byMonth$TP_MedianControl_all, ', ', TP_byMonth$nTP_control_all),
                           TP_diff_all=paste0(TP_byMonth$TP_MedianDiffAbs, ', ', round(TP_byMonth$TP_MedianDiffPct,2), '%'))

TP_byMonth$Month_factor <- factor(TP_byMonth$Month_factor ,                                    # Change ordering manually
                  levels = c("May", "Jun", "Jul", "Aug", "Sep","All months"))

# basic plot
ggplot(TP_byMonth, aes(x=Month_factor, y=TP_MedianDiffPct))+
  geom_bar(stat='identity', fill='firebrick')+
  theme_classic()+
  scale_y_continuous('Percent difference',limits=common_y_limits, breaks=common_y_breaks)+
  scale_x_discrete('Month')+
  theme(axis.title.y=element_text(color='black'),
        axis.title.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))+
  ggtitle('Total phosphorus')+
  geom_hline(yintercept = 0, linetype='dashed')


## TN
TN <- waterquality[,c('TN_ppb','Month_factor','Type','ConnClass')]
TN_burned <- subset(TN, Type=='Burned')
TN_burned_drainage <- subset(TN_burned, ConnClass=='Drainage')
TN_burned_isolated <- subset(TN_burned, ConnClass=='Isolated')
TN_control <- subset(TN, Type=='Control')
TN_control_drainage <- subset(TN_control, ConnClass=='Drainage')
TN_control_isolated <- subset(TN_control, ConnClass=='Isolated')
TN_burned_median <- median(TN_burned$TN_ppb, na.rm=T)
TN_burned_drainage_median <- median(TN_burned_drainage$TN_ppb, na.rm=T)
TN_burned_isolated_median <- median(TN_burned_isolated$TN_ppb, na.rm=T)
TN_control_median <- median(TN_control$TN_ppb, na.rm=T)
TN_control_drainage_median <- median(TN_control_drainage$TN_ppb, na.rm=T)
TN_control_isolated_median <- median(TN_control_isolated$TN_ppb, na.rm=T)

TN_byMonth_burned <- TN_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TN_MedianBurned_all=median(TN_ppb, na.rm=T),
                   nTN_burned_all=sum(!is.na(TN_ppb)))

TN_byMonth_burned_drainage <- TN_burned_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TN_MedianBurned_drainage=median(TN_ppb, na.rm=T),
                   nTN_burned_drainage=sum(!is.na(TN_ppb)))

TN_byMonth_burned_isolated <- TN_burned_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TN_MedianBurned_isolated=median(TN_ppb, na.rm=T),
                   nTN_burned_isolated=sum(!is.na(TN_ppb)))

TN_byMonth_control <- TN_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TN_MedianControl_all=median(TN_ppb, na.rm=T),
                   nTN_control_all=sum(!is.na(TN_ppb)))

TN_byMonth_control_drainage <- TN_control_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TN_MedianControl_drainage=median(TN_ppb, na.rm=T),
                   nTN_control_drainage=sum(!is.na(TN_ppb)))

TN_byMonth_control_isolated <- TN_control_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TN_MedianControl_isolated=median(TN_ppb, na.rm=T),
                   nTN_control_isolated=sum(!is.na(TN_ppb)))

TN_merge_list <- list(TN_byMonth_burned, TN_byMonth_control, 
                      TN_byMonth_burned_drainage, TN_byMonth_control_drainage,
                      TN_byMonth_burned_isolated, TN_byMonth_control_isolated)
#TN_byMonth <- merge(TN_byMonth_burned, TN_byMonth_control, by='Month_factor', all=T)
TN_byMonth <- TN_merge_list %>% reduce(full_join, by='Month_factor')

TN_byMonth <- TN_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
TN_byMonth <- as.data.frame(TN_byMonth)
TN_byMonth[6,] <- c('All months', TN_burned_median, sum(TN_byMonth$nTN_burned_all), 
                    TN_control_median, sum(TN_byMonth$nTN_control_all),
                    TN_burned_drainage_median, sum(TN_byMonth$nTN_burned_drainage),
                    TN_control_drainage_median, sum(TN_byMonth$nTN_control_drainage),
                    TN_burned_isolated_median, sum(TN_byMonth$nTN_burned_isolated),
                    TN_control_isolated_median, sum(TN_byMonth$nTN_control_isolated))
TN_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
TN_byMonth[,c(2:13)] <- TN_byMonth[,c(2:13)] %>% mutate_if(is.character, as.numeric)
TN_byMonth$TN_MedianDiffPct <- ((TN_byMonth$TN_MedianBurned_all - TN_byMonth$TN_MedianControl_all)/TN_byMonth$TN_MedianControl_all)*100
TN_byMonth$TN_MedianDiffAbs <-  TN_byMonth$TN_MedianBurned_all - TN_byMonth$TN_MedianControl_all
TN_byMonth$TN_MedianDrainageDiffPct <- ((TN_byMonth$TN_MedianBurned_drainage - TN_byMonth$TN_MedianControl_drainage)/TN_byMonth$TN_MedianControl_drainage)*100
TN_byMonth$TN_MedianDrainageDiffAbs <-  TN_byMonth$TN_MedianBurned_drainage - TN_byMonth$TN_MedianControl_drainage
TN_byMonth$TN_MedianIsolatedDiffPct <- ((TN_byMonth$TN_MedianBurned_isolated - TN_byMonth$TN_MedianControl_isolated)/TN_byMonth$TN_MedianControl_isolated)*100
TN_byMonth$TN_MedianIsolatedDiffAbs <-  TN_byMonth$TN_MedianBurned_isolated - TN_byMonth$TN_MedianControl_isolated

#TN_byMonth[6,3] <- sum(TN_byMonth$nTN_burned, na.rm=T)
#TN_byMonth[6,5] <- sum(TN_byMonth$nTN_control, na.rm=T)
TN_formatted <- data.frame(Month=c(month_order, 'All months'), 
                           TN_burned_drainage=paste0(TN_byMonth$TN_MedianBurned_drainage, ', ', TN_byMonth$nTN_burned_drainage),
                           TN_control_drainage=paste0(TN_byMonth$TN_MedianControl_drainage, ', ', TN_byMonth$nTN_control_drainage),
                           TN_diff_drainage=paste0(TN_byMonth$TN_MedianDrainageDiffAbs, ', ', round(TN_byMonth$TN_MedianDrainageDiffPct,2), '%'),
                           TN_burned_isolated=paste0(TN_byMonth$TN_MedianBurned_isolated, ', ', TN_byMonth$nTN_burned_isolated),
                           TN_control_isolated=paste0(TN_byMonth$TN_MedianControl_isolated, ', ', TN_byMonth$nTN_control_isolated),
                           TN_diff_isolated=paste0(TN_byMonth$TN_MedianIsolatedDiffAbs, ', ', round(TN_byMonth$TN_MedianIsolatedDiffPct,2), '%'),
                           TN_burned_all=paste0(TN_byMonth$TN_MedianBurned_all, ', ', TN_byMonth$nTN_burned_all),
                           TN_control_all=paste0(TN_byMonth$TN_MedianControl_all, ', ', TN_byMonth$nTN_control_all),
                           TN_diff_all=paste0(TN_byMonth$TN_MedianDiffAbs, ', ', round(TN_byMonth$TN_MedianDiffPct,2), '%'))

TN_byMonth$Month_factor <- factor(TN_byMonth$Month_factor ,                                    # Change ordering manually
                                  levels = c("May", "Jun", "Jul", "Aug", "Sep","All months"))

# basic plot
ggplot(TN_byMonth, aes(x=Month_factor, y=TN_MedianDiffPct))+
  geom_bar(stat='identity', fill='firebrick')+
  theme_classic()+
  scale_y_continuous('Percent difference',limits=common_y_limits, breaks=common_y_breaks)+
  scale_x_discrete('Month')+
  theme(axis.title.y=element_text(color='black'),
        axis.title.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))+
  ggtitle('Total nitrogen')+
  geom_hline(yintercept = 0, linetype='dashed')


# ## NO2NO3 # why so many 0s?
# NO2NO3 <- waterquality[,c('NO2NO3_ppb','Month_factor','Type')]
# NO2NO3_burned <- subset(NO2NO3, Type=='Burned')
# NO2NO3_control <- subset(NO2NO3, Type=='Control')
# 
# NO2NO3_byMonth_burned <- NO2NO3_burned %>%
#   group_by(Month_factor) %>%
#   summarize(NO2NO3_MedianBurned=median(NO2NO3_ppb, na.rm=T))
# 
# NO2NO3_byMonth_control <- NO2NO3_control %>%
#   group_by(Month_factor) %>%
#   summarize(NO2NO3_MedianControl=median(NO2NO3_ppb, na.rm=T))
# 
# NO2NO3_byMonth <- merge(NO2NO3_byMonth_burned, NO2NO3_byMonth_control, by='Month_factor', all=T)
# NO2NO3_byMonth <- NO2NO3_byMonth %>%
#   mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
#   arrange(Month_factor)
# NO2NO3_byMonth$NO2NO3_MedianDiff <- ((NO2NO3_byMonth$NO2NO3_MedianBurned - NO2NO3_byMonth$NO2NO3_MedianControl)/NO2NO3_byMonth$NO2NO3_MedianControl)*100
# 
# # basic plot
# ggplot(NO2NO3_byMonth, aes(x=Month_factor, y=NO2NO3_MedianDiff))+
#   geom_bar(stat='identity', fill='firebrick')+
#   theme_classic()+
#   scale_y_continuous('Percent difference',limits=common_y_limits, breaks=common_y_breaks)+
#   scale_x_discrete('Month')+
#   theme(axis.title.y=element_text(color='black'),
#         axis.title.x=element_text(color='black'),
#         axis.text.y=element_text(color='black'),
#         axis.text.x=element_text(color='black'))+
#   ggtitle('NO2NO3')+
#   geom_hline(yintercept = 0, linetype='dashed')


## NH4N
# NH4N <- waterquality[,c('NH4N_ppb','Month_factor','Type')]
# NH4N_burned <- subset(NH4N, Type=='Burned')
# NH4N_control <- subset(NH4N, Type=='Control')
# NH4N_burned_median <- median(NH4N_burned$NH4N_ppb, na.rm=T)
# NH4N_control_median <- median(NH4N_control$NH4N_ppb, na.rm=T)
# 
# NH4N_byMonth_burned <- NH4N_burned %>%
#   dplyr::group_by(Month_factor) %>%
#   dplyr::summarize(NH4N_MedianBurned=median(NH4N_ppb, na.rm=T),
#                    nNH4N_burned=sum(!is.na(NH4N_ppb)))
# 
# NH4N_byMonth_control <- NH4N_control %>%
#   dplyr::group_by(Month_factor) %>%
#   dplyr::summarize(NH4N_MedianControl=median(NH4N_ppb, na.rm=T),
#                    nNH4N_control=sum(!is.na(NH4N_ppb)))
# 
# NH4N_byMonth <- merge(NH4N_byMonth_burned, NH4N_byMonth_control, by='Month_factor', all=T)
# NH4N_byMonth <- NH4N_byMonth %>%
#   mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
#   arrange(Month_factor)
# NH4N_byMonth[6,] <- c('All months', NH4N_burned_median, NA, NH4N_control_median,NA)
# NH4N_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
# NH4N_byMonth[,c(2:5)] <- NH4N_byMonth[,c(2:5)] %>% mutate_if(is.character, as.numeric)
# NH4N_byMonth$NH4N_MedianDiffPct <- ((NH4N_byMonth$NH4N_MedianBurned - NH4N_byMonth$NH4N_MedianControl)/NH4N_byMonth$NH4N_MedianControl)*100
# NH4N_byMonth$NH4N_MedianDiffAbs <-  NH4N_byMonth$NH4N_MedianBurned - NH4N_byMonth$NH4N_MedianControl
# NH4N_byMonth[6,3] <- sum(NH4N_byMonth$nNH4N_burned, na.rm=T)
# NH4N_byMonth[6,5] <- sum(NH4N_byMonth$nNH4N_control, na.rm=T)
# NH4N_formatted <- data.frame(Month=c(month_order, 'All months'), 
#                              NH4N_burned=paste0(NH4N_byMonth$NH4N_MedianBurned, ', ', NH4N_byMonth$nNH4N_burned),
#                              NH4N_control=paste0(NH4N_byMonth$NH4N_MedianControl, ', ', NH4N_byMonth$nNH4N_control),
#                              NH4N_diff=paste0(NH4N_byMonth$NH4N_MedianDiffAbs, ', ', round(NH4N_byMonth$NH4N_MedianDiffPct,2), '%'))
# 
# NH4N_byMonth$Month_factor <- factor(NH4N_byMonth$Month_factor ,                                    # Change ordering manually
#                                     levels = c("May", "Jun", "Jul", "Aug", "Sep","All months"))
# 
# # basic plot
# ggplot(NH4N_byMonth, aes(x=Month_factor, y=NH4N_MedianDiffPct))+
#   geom_bar(stat='identity', fill='firebrick')+
#   theme_classic()+
#   scale_y_continuous('Percent difference',limits=common_y_limits, breaks=common_y_breaks)+
#   scale_x_discrete('Month')+
#   theme(axis.title.y=element_text(color='black'),
#         axis.title.x=element_text(color='black'),
#         axis.text.y=element_text(color='black'),
#         axis.text.x=element_text(color='black'))+
#   ggtitle('NH4N')+
#   geom_hline(yintercept = 0, linetype='dashed')

## DOC
DOC <- waterquality[,c('DOC_ppm','Month_factor','Type','ConnClass')]
DOC_burned <- subset(DOC, Type=='Burned')
DOC_burned_drainage <- subset(DOC_burned, ConnClass=='Drainage')
DOC_burned_isolated <- subset(DOC_burned, ConnClass=='Isolated')
DOC_control <- subset(DOC, Type=='Control')
DOC_control_drainage <- subset(DOC_control, ConnClass=='Drainage')
DOC_control_isolated <- subset(DOC_control, ConnClass=='Isolated')
DOC_burned_median <- median(DOC_burned$DOC_ppm, na.rm=T)
DOC_burned_drainage_median <- median(DOC_burned_drainage$DOC_ppm, na.rm=T)
DOC_burned_isolated_median <- median(DOC_burned_isolated$DOC_ppm, na.rm=T)
DOC_control_median <- median(DOC_control$DOC_ppm, na.rm=T)
DOC_control_drainage_median <- median(DOC_control_drainage$DOC_ppm, na.rm=T)
DOC_control_isolated_median <- median(DOC_control_isolated$DOC_ppm, na.rm=T)

DOC_byMonth_burned <- DOC_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(DOC_MedianBurned_all=median(DOC_ppm, na.rm=T),
                   nDOC_burned_all=sum(!is.na(DOC_ppm)))

DOC_byMonth_burned_drainage <- DOC_burned_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(DOC_MedianBurned_drainage=median(DOC_ppm, na.rm=T),
                   nDOC_burned_drainage=sum(!is.na(DOC_ppm)))

DOC_byMonth_burned_isolated <- DOC_burned_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(DOC_MedianBurned_isolated=median(DOC_ppm, na.rm=T),
                   nDOC_burned_isolated=sum(!is.na(DOC_ppm)))

DOC_byMonth_control <- DOC_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(DOC_MedianControl_all=median(DOC_ppm, na.rm=T),
                   nDOC_control_all=sum(!is.na(DOC_ppm)))

DOC_byMonth_control_drainage <- DOC_control_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(DOC_MedianControl_drainage=median(DOC_ppm, na.rm=T),
                   nDOC_control_drainage=sum(!is.na(DOC_ppm)))

DOC_byMonth_control_isolated <- DOC_control_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(DOC_MedianControl_isolated=median(DOC_ppm, na.rm=T),
                   nDOC_control_isolated=sum(!is.na(DOC_ppm)))

DOC_merge_list <- list(DOC_byMonth_burned, DOC_byMonth_control, 
                       DOC_byMonth_burned_drainage, DOC_byMonth_control_drainage,
                       DOC_byMonth_burned_isolated, DOC_byMonth_control_isolated)
#DOC_byMonth <- merge(DOC_byMonth_burned, DOC_byMonth_control, by='Month_factor', all=T)
DOC_byMonth <- DOC_merge_list %>% reduce(full_join, by='Month_factor')

DOC_byMonth <- DOC_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
DOC_byMonth <- as.data.frame(DOC_byMonth)
DOC_byMonth[6,] <- c('All months', DOC_burned_median, sum(DOC_byMonth$nDOC_burned_all), 
                     DOC_control_median, sum(DOC_byMonth$nDOC_control_all),
                     DOC_burned_drainage_median, sum(DOC_byMonth$nDOC_burned_drainage),
                     DOC_control_drainage_median, sum(DOC_byMonth$nDOC_control_drainage),
                     DOC_burned_isolated_median, sum(DOC_byMonth$nDOC_burned_isolated),
                     DOC_control_isolated_median, sum(DOC_byMonth$nDOC_control_isolated))
DOC_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
DOC_byMonth[,c(2:13)] <- DOC_byMonth[,c(2:13)] %>% mutate_if(is.character, as.numeric)
DOC_byMonth$DOC_MedianDiffPct <- ((DOC_byMonth$DOC_MedianBurned_all - DOC_byMonth$DOC_MedianControl_all)/DOC_byMonth$DOC_MedianControl_all)*100
DOC_byMonth$DOC_MedianDiffAbs <-  DOC_byMonth$DOC_MedianBurned_all - DOC_byMonth$DOC_MedianControl_all
DOC_byMonth$DOC_MedianDrainageDiffPct <- ((DOC_byMonth$DOC_MedianBurned_drainage - DOC_byMonth$DOC_MedianControl_drainage)/DOC_byMonth$DOC_MedianControl_drainage)*100
DOC_byMonth$DOC_MedianDrainageDiffAbs <-  DOC_byMonth$DOC_MedianBurned_drainage - DOC_byMonth$DOC_MedianControl_drainage
DOC_byMonth$DOC_MedianIsolatedDiffPct <- ((DOC_byMonth$DOC_MedianBurned_isolated - DOC_byMonth$DOC_MedianControl_isolated)/DOC_byMonth$DOC_MedianControl_isolated)*100
DOC_byMonth$DOC_MedianIsolatedDiffAbs <-  DOC_byMonth$DOC_MedianBurned_isolated - DOC_byMonth$DOC_MedianControl_isolated

#DOC_byMonth[6,3] <- sum(DOC_byMonth$nDOC_burned, na.rm=T)
#DOC_byMonth[6,5] <- sum(DOC_byMonth$nDOC_control, na.rm=T)
DOC_formatted <- data.frame(Month=c(month_order, 'All months'), 
                            DOC_burned_drainage=paste0(DOC_byMonth$DOC_MedianBurned_drainage, ', ', DOC_byMonth$nDOC_burned_drainage),
                            DOC_control_drainage=paste0(DOC_byMonth$DOC_MedianControl_drainage, ', ', DOC_byMonth$nDOC_control_drainage),
                            DOC_diff_drainage=paste0(DOC_byMonth$DOC_MedianDrainageDiffAbs, ', ', round(DOC_byMonth$DOC_MedianDrainageDiffPct,2), '%'),
                            DOC_burned_isolated=paste0(DOC_byMonth$DOC_MedianBurned_isolated, ', ', DOC_byMonth$nDOC_burned_isolated),
                            DOC_control_isolated=paste0(DOC_byMonth$DOC_MedianControl_isolated, ', ', DOC_byMonth$nDOC_control_isolated),
                            DOC_diff_isolated=paste0(DOC_byMonth$DOC_MedianIsolatedDiffAbs, ', ', round(DOC_byMonth$DOC_MedianIsolatedDiffPct,2), '%'),
                            DOC_burned_all=paste0(DOC_byMonth$DOC_MedianBurned_all, ', ', DOC_byMonth$nDOC_burned_all),
                            DOC_control_all=paste0(DOC_byMonth$DOC_MedianControl_all, ', ', DOC_byMonth$nDOC_control_all),
                            DOC_diff_all=paste0(DOC_byMonth$DOC_MedianDiffAbs, ', ', round(DOC_byMonth$DOC_MedianDiffPct,2), '%'))

DOC_byMonth$Month_factor <- factor(DOC_byMonth$Month_factor ,                                    # Change ordering manually
                                   levels = c("May", "Jun", "Jul", "Aug", "Sep","All months"))

# basic plot
ggplot(DOC_byMonth, aes(x=Month_factor, y=DOC_MedianDiffPct))+
  geom_bar(stat='identity', fill='firebrick')+
  theme_classic()+
  scale_y_continuous('Percent difference',limits=common_y_limits, breaks=common_y_breaks)+
  scale_x_discrete('Month')+
  theme(axis.title.y=element_text(color='black'),
        axis.title.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))+
  ggtitle('DOC')+
  geom_hline(yintercept = 0, linetype='dashed')

## TSS
TSS <- waterquality[,c('TSS_mgL','Month_factor','Type','ConnClass')]
TSS_burned <- subset(TSS, Type=='Burned')
TSS_burned_drainage <- subset(TSS_burned, ConnClass=='Drainage')
TSS_burned_isolated <- subset(TSS_burned, ConnClass=='Isolated')
TSS_control <- subset(TSS, Type=='Control')
TSS_control_drainage <- subset(TSS_control, ConnClass=='Drainage')
TSS_control_isolated <- subset(TSS_control, ConnClass=='Isolated')
TSS_burned_median <- median(TSS_burned$TSS_mgL, na.rm=T)
TSS_burned_drainage_median <- median(TSS_burned_drainage$TSS_mgL, na.rm=T)
TSS_burned_isolated_median <- median(TSS_burned_isolated$TSS_mgL, na.rm=T)
TSS_control_median <- median(TSS_control$TSS_mgL, na.rm=T)
TSS_control_drainage_median <- median(TSS_control_drainage$TSS_mgL, na.rm=T)
TSS_control_isolated_median <- median(TSS_control_isolated$TSS_mgL, na.rm=T)

TSS_byMonth_burned <- TSS_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TSS_MedianBurned_all=median(TSS_mgL, na.rm=T),
                   nTSS_burned_all=sum(!is.na(TSS_mgL)))

TSS_byMonth_burned_drainage <- TSS_burned_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TSS_MedianBurned_drainage=median(TSS_mgL, na.rm=T),
                   nTSS_burned_drainage=sum(!is.na(TSS_mgL)))

TSS_byMonth_burned_isolated <- TSS_burned_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TSS_MedianBurned_isolated=median(TSS_mgL, na.rm=T),
                   nTSS_burned_isolated=sum(!is.na(TSS_mgL)))

TSS_byMonth_control <- TSS_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TSS_MedianControl_all=median(TSS_mgL, na.rm=T),
                   nTSS_control_all=sum(!is.na(TSS_mgL)))

TSS_byMonth_control_drainage <- TSS_control_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TSS_MedianControl_drainage=median(TSS_mgL, na.rm=T),
                   nTSS_control_drainage=sum(!is.na(TSS_mgL)))

TSS_byMonth_control_isolated <- TSS_control_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TSS_MedianControl_isolated=median(TSS_mgL, na.rm=T),
                   nTSS_control_isolated=sum(!is.na(TSS_mgL)))

TSS_merge_list <- list(TSS_byMonth_burned, TSS_byMonth_control, 
                       TSS_byMonth_burned_drainage, TSS_byMonth_control_drainage,
                       TSS_byMonth_burned_isolated, TSS_byMonth_control_isolated)
#TSS_byMonth <- merge(TSS_byMonth_burned, TSS_byMonth_control, by='Month_factor', all=T)
TSS_byMonth <- TSS_merge_list %>% reduce(full_join, by='Month_factor')

TSS_byMonth <- TSS_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
TSS_byMonth <- as.data.frame(TSS_byMonth)
TSS_byMonth[6,] <- c('All months', TSS_burned_median, sum(TSS_byMonth$nTSS_burned_all), 
                     TSS_control_median, sum(TSS_byMonth$nTSS_control_all),
                     TSS_burned_drainage_median, sum(TSS_byMonth$nTSS_burned_drainage),
                     TSS_control_drainage_median, sum(TSS_byMonth$nTSS_control_drainage),
                     TSS_burned_isolated_median, sum(TSS_byMonth$nTSS_burned_isolated),
                     TSS_control_isolated_median, sum(TSS_byMonth$nTSS_control_isolated))
TSS_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
TSS_byMonth[,c(2:13)] <- TSS_byMonth[,c(2:13)] %>% mutate_if(is.character, as.numeric)
TSS_byMonth$TSS_MedianDiffPct <- ((TSS_byMonth$TSS_MedianBurned_all - TSS_byMonth$TSS_MedianControl_all)/TSS_byMonth$TSS_MedianControl_all)*100
TSS_byMonth$TSS_MedianDiffAbs <-  TSS_byMonth$TSS_MedianBurned_all - TSS_byMonth$TSS_MedianControl_all
TSS_byMonth$TSS_MedianDrainageDiffPct <- ((TSS_byMonth$TSS_MedianBurned_drainage - TSS_byMonth$TSS_MedianControl_drainage)/TSS_byMonth$TSS_MedianControl_drainage)*100
TSS_byMonth$TSS_MedianDrainageDiffAbs <-  TSS_byMonth$TSS_MedianBurned_drainage - TSS_byMonth$TSS_MedianControl_drainage
TSS_byMonth$TSS_MedianIsolatedDiffPct <- ((TSS_byMonth$TSS_MedianBurned_isolated - TSS_byMonth$TSS_MedianControl_isolated)/TSS_byMonth$TSS_MedianControl_isolated)*100
TSS_byMonth$TSS_MedianIsolatedDiffAbs <-  TSS_byMonth$TSS_MedianBurned_isolated - TSS_byMonth$TSS_MedianControl_isolated

#TSS_byMonth[6,3] <- sum(TSS_byMonth$nTSS_burned, na.rm=T)
#TSS_byMonth[6,5] <- sum(TSS_byMonth$nTSS_control, na.rm=T)
TSS_formatted <- data.frame(Month=c(month_order, 'All months'), 
                            TSS_burned_drainage=paste0(TSS_byMonth$TSS_MedianBurned_drainage, ', ', TSS_byMonth$nTSS_burned_drainage),
                            TSS_control_drainage=paste0(TSS_byMonth$TSS_MedianControl_drainage, ', ', TSS_byMonth$nTSS_control_drainage),
                            TSS_diff_drainage=paste0(TSS_byMonth$TSS_MedianDrainageDiffAbs, ', ', round(TSS_byMonth$TSS_MedianDrainageDiffPct,2), '%'),
                            TSS_burned_isolated=paste0(TSS_byMonth$TSS_MedianBurned_isolated, ', ', TSS_byMonth$nTSS_burned_isolated),
                            TSS_control_isolated=paste0(TSS_byMonth$TSS_MedianControl_isolated, ', ', TSS_byMonth$nTSS_control_isolated),
                            TSS_diff_isolated=paste0(TSS_byMonth$TSS_MedianIsolatedDiffAbs, ', ', round(TSS_byMonth$TSS_MedianIsolatedDiffPct,2), '%'),
                            TSS_burned_all=paste0(TSS_byMonth$TSS_MedianBurned_all, ', ', TSS_byMonth$nTSS_burned_all),
                            TSS_control_all=paste0(TSS_byMonth$TSS_MedianControl_all, ', ', TSS_byMonth$nTSS_control_all),
                            TSS_diff_all=paste0(TSS_byMonth$TSS_MedianDiffAbs, ', ', round(TSS_byMonth$TSS_MedianDiffPct,2), '%'))

TSS_byMonth$Month_factor <- factor(TSS_byMonth$Month_factor ,                                    # Change ordering manually
                                   levels = c("May", "Jun", "Jul", "Aug", "Sep","All months"))

# basic plot
ggplot(TSS_byMonth, aes(x=Month_factor, y=TSS_MedianDiffPct))+
  geom_bar(stat='identity', fill='firebrick')+
  theme_classic()+
  scale_y_continuous('Percent difference',limits=common_y_limits, breaks=common_y_breaks)+
  scale_x_discrete('Month')+
  theme(axis.title.y=element_text(color='black'),
        axis.title.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))+
  ggtitle('TSS')+
  geom_hline(yintercept = 0, linetype='dashed')


## Chloro
Chloro <- waterquality[,c('Chloro_ppb','Month_factor','Type','ConnClass')]
Chloro_burned <- subset(Chloro, Type=='Burned')
Chloro_burned_drainage <- subset(Chloro_burned, ConnClass=='Drainage')
Chloro_burned_isolated <- subset(Chloro_burned, ConnClass=='Isolated')
Chloro_control <- subset(Chloro, Type=='Control')
Chloro_control_drainage <- subset(Chloro_control, ConnClass=='Drainage')
Chloro_control_isolated <- subset(Chloro_control, ConnClass=='Isolated')
Chloro_burned_median <- median(Chloro_burned$Chloro_ppb, na.rm=T)
Chloro_burned_drainage_median <- median(Chloro_burned_drainage$Chloro_ppb, na.rm=T)
Chloro_burned_isolated_median <- median(Chloro_burned_isolated$Chloro_ppb, na.rm=T)
Chloro_control_median <- median(Chloro_control$Chloro_ppb, na.rm=T)
Chloro_control_drainage_median <- median(Chloro_control_drainage$Chloro_ppb, na.rm=T)
Chloro_control_isolated_median <- median(Chloro_control_isolated$Chloro_ppb, na.rm=T)

Chloro_byMonth_burned <- Chloro_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Chloro_MedianBurned_all=median(Chloro_ppb, na.rm=T),
                   nChloro_burned_all=sum(!is.na(Chloro_ppb)))

Chloro_byMonth_burned_drainage <- Chloro_burned_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Chloro_MedianBurned_drainage=median(Chloro_ppb, na.rm=T),
                   nChloro_burned_drainage=sum(!is.na(Chloro_ppb)))

Chloro_byMonth_burned_isolated <- Chloro_burned_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Chloro_MedianBurned_isolated=median(Chloro_ppb, na.rm=T),
                   nChloro_burned_isolated=sum(!is.na(Chloro_ppb)))

Chloro_byMonth_control <- Chloro_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Chloro_MedianControl_all=median(Chloro_ppb, na.rm=T),
                   nChloro_control_all=sum(!is.na(Chloro_ppb)))

Chloro_byMonth_control_drainage <- Chloro_control_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Chloro_MedianControl_drainage=median(Chloro_ppb, na.rm=T),
                   nChloro_control_drainage=sum(!is.na(Chloro_ppb)))

Chloro_byMonth_control_isolated <- Chloro_control_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Chloro_MedianControl_isolated=median(Chloro_ppb, na.rm=T),
                   nChloro_control_isolated=sum(!is.na(Chloro_ppb)))

Chloro_merge_list <- list(Chloro_byMonth_burned, Chloro_byMonth_control, 
                          Chloro_byMonth_burned_drainage, Chloro_byMonth_control_drainage,
                          Chloro_byMonth_burned_isolated, Chloro_byMonth_control_isolated)
#Chloro_byMonth <- merge(Chloro_byMonth_burned, Chloro_byMonth_control, by='Month_factor', all=T)
Chloro_byMonth <- Chloro_merge_list %>% reduce(full_join, by='Month_factor')

Chloro_byMonth <- Chloro_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
Chloro_byMonth <- as.data.frame(Chloro_byMonth)
Chloro_byMonth[6,] <- c('All months', Chloro_burned_median, sum(Chloro_byMonth$nChloro_burned_all), 
                        Chloro_control_median, sum(Chloro_byMonth$nChloro_control_all),
                        Chloro_burned_drainage_median, sum(Chloro_byMonth$nChloro_burned_drainage),
                        Chloro_control_drainage_median, sum(Chloro_byMonth$nChloro_control_drainage),
                        Chloro_burned_isolated_median, sum(Chloro_byMonth$nChloro_burned_isolated),
                        Chloro_control_isolated_median, sum(Chloro_byMonth$nChloro_control_isolated))
Chloro_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
Chloro_byMonth[,c(2:13)] <- Chloro_byMonth[,c(2:13)] %>% mutate_if(is.character, as.numeric)
Chloro_byMonth$Chloro_MedianDiffPct <- ((Chloro_byMonth$Chloro_MedianBurned_all - Chloro_byMonth$Chloro_MedianControl_all)/Chloro_byMonth$Chloro_MedianControl_all)*100
Chloro_byMonth$Chloro_MedianDiffAbs <-  Chloro_byMonth$Chloro_MedianBurned_all - Chloro_byMonth$Chloro_MedianControl_all
Chloro_byMonth$Chloro_MedianDrainageDiffPct <- ((Chloro_byMonth$Chloro_MedianBurned_drainage - Chloro_byMonth$Chloro_MedianControl_drainage)/Chloro_byMonth$Chloro_MedianControl_drainage)*100
Chloro_byMonth$Chloro_MedianDrainageDiffAbs <-  Chloro_byMonth$Chloro_MedianBurned_drainage - Chloro_byMonth$Chloro_MedianControl_drainage
Chloro_byMonth$Chloro_MedianIsolatedDiffPct <- ((Chloro_byMonth$Chloro_MedianBurned_isolated - Chloro_byMonth$Chloro_MedianControl_isolated)/Chloro_byMonth$Chloro_MedianControl_isolated)*100
Chloro_byMonth$Chloro_MedianIsolatedDiffAbs <-  Chloro_byMonth$Chloro_MedianBurned_isolated - Chloro_byMonth$Chloro_MedianControl_isolated

#Chloro_byMonth[6,3] <- sum(Chloro_byMonth$nChloro_burned, na.rm=T)
#Chloro_byMonth[6,5] <- sum(Chloro_byMonth$nChloro_control, na.rm=T)
Chloro_formatted <- data.frame(Month=c(month_order, 'All months'), 
                               Chloro_burned_drainage=paste0(Chloro_byMonth$Chloro_MedianBurned_drainage, ', ', Chloro_byMonth$nChloro_burned_drainage),
                               Chloro_control_drainage=paste0(Chloro_byMonth$Chloro_MedianControl_drainage, ', ', Chloro_byMonth$nChloro_control_drainage),
                               Chloro_diff_drainage=paste0(Chloro_byMonth$Chloro_MedianDrainageDiffAbs, ', ', round(Chloro_byMonth$Chloro_MedianDrainageDiffPct,2), '%'),
                               Chloro_burned_isolated=paste0(Chloro_byMonth$Chloro_MedianBurned_isolated, ', ', Chloro_byMonth$nChloro_burned_isolated),
                               Chloro_control_isolated=paste0(Chloro_byMonth$Chloro_MedianControl_isolated, ', ', Chloro_byMonth$nChloro_control_isolated),
                               Chloro_diff_isolated=paste0(Chloro_byMonth$Chloro_MedianIsolatedDiffAbs, ', ', round(Chloro_byMonth$Chloro_MedianIsolatedDiffPct,2), '%'),
                               Chloro_burned_all=paste0(Chloro_byMonth$Chloro_MedianBurned_all, ', ', Chloro_byMonth$nChloro_burned_all),
                               Chloro_control_all=paste0(Chloro_byMonth$Chloro_MedianControl_all, ', ', Chloro_byMonth$nChloro_control_all),
                               Chloro_diff_all=paste0(Chloro_byMonth$Chloro_MedianDiffAbs, ', ', round(Chloro_byMonth$Chloro_MedianDiffPct,2), '%'))

Chloro_byMonth$Month_factor <- factor(Chloro_byMonth$Month_factor ,                                    # Change ordering manually
                                      levels = c("May", "Jun", "Jul", "Aug", "Sep","All months"))

# basic plot
ggplot(Chloro_byMonth, aes(x=Month_factor, y=Chloro_MedianDiffPct))+
  geom_bar(stat='identity', fill='firebrick')+
  theme_classic()+
  scale_y_continuous('Percent difference',limits=common_y_limits, breaks=common_y_breaks)+
  scale_x_discrete('Month')+
  theme(axis.title.y=element_text(color='black'),
        axis.title.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))+
  ggtitle('Chloro')+
  geom_hline(yintercept = 0, linetype='dashed')


## Secchi
Secchi <- waterquality[,c('SecchiDepth_m','Month_factor','Type','ConnClass')]
Secchi_burned <- subset(Secchi, Type=='Burned')
Secchi_burned_drainage <- subset(Secchi_burned, ConnClass=='Drainage')
Secchi_burned_isolated <- subset(Secchi_burned, ConnClass=='Isolated')
Secchi_control <- subset(Secchi, Type=='Control')
Secchi_control_drainage <- subset(Secchi_control, ConnClass=='Drainage')
Secchi_control_isolated <- subset(Secchi_control, ConnClass=='Isolated')
Secchi_burned_median <- median(Secchi_burned$SecchiDepth_m, na.rm=T)
Secchi_burned_drainage_median <- median(Secchi_burned_drainage$SecchiDepth_m, na.rm=T)
Secchi_burned_isolated_median <- median(Secchi_burned_isolated$SecchiDepth_m, na.rm=T)
Secchi_control_median <- median(Secchi_control$SecchiDepth_m, na.rm=T)
Secchi_control_drainage_median <- median(Secchi_control_drainage$SecchiDepth_m, na.rm=T)
Secchi_control_isolated_median <- median(Secchi_control_isolated$SecchiDepth_m, na.rm=T)

Secchi_byMonth_burned <- Secchi_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Secchi_MedianBurned_all=median(SecchiDepth_m, na.rm=T),
                   nSecchi_burned_all=sum(!is.na(SecchiDepth_m)))

Secchi_byMonth_burned_drainage <- Secchi_burned_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Secchi_MedianBurned_drainage=median(SecchiDepth_m, na.rm=T),
                   nSecchi_burned_drainage=sum(!is.na(SecchiDepth_m)))

Secchi_byMonth_burned_isolated <- Secchi_burned_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Secchi_MedianBurned_isolated=median(SecchiDepth_m, na.rm=T),
                   nSecchi_burned_isolated=sum(!is.na(SecchiDepth_m)))

Secchi_byMonth_control <- Secchi_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Secchi_MedianControl_all=median(SecchiDepth_m, na.rm=T),
                   nSecchi_control_all=sum(!is.na(SecchiDepth_m)))

Secchi_byMonth_control_drainage <- Secchi_control_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Secchi_MedianControl_drainage=median(SecchiDepth_m, na.rm=T),
                   nSecchi_control_drainage=sum(!is.na(SecchiDepth_m)))

Secchi_byMonth_control_isolated <- Secchi_control_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Secchi_MedianControl_isolated=median(SecchiDepth_m, na.rm=T),
                   nSecchi_control_isolated=sum(!is.na(SecchiDepth_m)))

Secchi_merge_list <- list(Secchi_byMonth_burned, Secchi_byMonth_control, 
                          Secchi_byMonth_burned_drainage, Secchi_byMonth_control_drainage,
                          Secchi_byMonth_burned_isolated, Secchi_byMonth_control_isolated)
#Secchi_byMonth <- merge(Secchi_byMonth_burned, Secchi_byMonth_control, by='Month_factor', all=T)
Secchi_byMonth <- Secchi_merge_list %>% reduce(full_join, by='Month_factor')

Secchi_byMonth <- Secchi_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
Secchi_byMonth <- as.data.frame(Secchi_byMonth)
Secchi_byMonth[6,] <- c('All months', Secchi_burned_median, sum(Secchi_byMonth$nSecchi_burned_all), 
                        Secchi_control_median, sum(Secchi_byMonth$nSecchi_control_all),
                        Secchi_burned_drainage_median, sum(Secchi_byMonth$nSecchi_burned_drainage),
                        Secchi_control_drainage_median, sum(Secchi_byMonth$nSecchi_control_drainage),
                        Secchi_burned_isolated_median, sum(Secchi_byMonth$nSecchi_burned_isolated),
                        Secchi_control_isolated_median, sum(Secchi_byMonth$nSecchi_control_isolated))
Secchi_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
Secchi_byMonth[,c(2:13)] <- Secchi_byMonth[,c(2:13)] %>% mutate_if(is.character, as.numeric)
Secchi_byMonth$Secchi_MedianDiffPct <- ((Secchi_byMonth$Secchi_MedianBurned_all - Secchi_byMonth$Secchi_MedianControl_all)/Secchi_byMonth$Secchi_MedianControl_all)*100
Secchi_byMonth$Secchi_MedianDiffAbs <-  Secchi_byMonth$Secchi_MedianBurned_all - Secchi_byMonth$Secchi_MedianControl_all
Secchi_byMonth$Secchi_MedianDrainageDiffPct <- ((Secchi_byMonth$Secchi_MedianBurned_drainage - Secchi_byMonth$Secchi_MedianControl_drainage)/Secchi_byMonth$Secchi_MedianControl_drainage)*100
Secchi_byMonth$Secchi_MedianDrainageDiffAbs <-  Secchi_byMonth$Secchi_MedianBurned_drainage - Secchi_byMonth$Secchi_MedianControl_drainage
Secchi_byMonth$Secchi_MedianIsolatedDiffPct <- ((Secchi_byMonth$Secchi_MedianBurned_isolated - Secchi_byMonth$Secchi_MedianControl_isolated)/Secchi_byMonth$Secchi_MedianControl_isolated)*100
Secchi_byMonth$Secchi_MedianIsolatedDiffAbs <-  Secchi_byMonth$Secchi_MedianBurned_isolated - Secchi_byMonth$Secchi_MedianControl_isolated

#Secchi_byMonth[6,3] <- sum(Secchi_byMonth$nSecchi_burned, na.rm=T)
#Secchi_byMonth[6,5] <- sum(Secchi_byMonth$nSecchi_control, na.rm=T)
Secchi_formatted <- data.frame(Month=c(month_order, 'All months'), 
                               Secchi_burned_drainage=paste0(Secchi_byMonth$Secchi_MedianBurned_drainage, ', ', Secchi_byMonth$nSecchi_burned_drainage),
                               Secchi_control_drainage=paste0(Secchi_byMonth$Secchi_MedianControl_drainage, ', ', Secchi_byMonth$nSecchi_control_drainage),
                               Secchi_diff_drainage=paste0(Secchi_byMonth$Secchi_MedianDrainageDiffAbs, ', ', round(Secchi_byMonth$Secchi_MedianDrainageDiffPct,2), '%'),
                               Secchi_burned_isolated=paste0(Secchi_byMonth$Secchi_MedianBurned_isolated, ', ', Secchi_byMonth$nSecchi_burned_isolated),
                               Secchi_control_isolated=paste0(Secchi_byMonth$Secchi_MedianControl_isolated, ', ', Secchi_byMonth$nSecchi_control_isolated),
                               Secchi_diff_isolated=paste0(Secchi_byMonth$Secchi_MedianIsolatedDiffAbs, ', ', round(Secchi_byMonth$Secchi_MedianIsolatedDiffPct,2), '%'),
                               Secchi_burned_all=paste0(Secchi_byMonth$Secchi_MedianBurned_all, ', ', Secchi_byMonth$nSecchi_burned_all),
                               Secchi_control_all=paste0(Secchi_byMonth$Secchi_MedianControl_all, ', ', Secchi_byMonth$nSecchi_control_all),
                               Secchi_diff_all=paste0(Secchi_byMonth$Secchi_MedianDiffAbs, ', ', round(Secchi_byMonth$Secchi_MedianDiffPct,2), '%'))

Secchi_byMonth$Month_factor <- factor(Secchi_byMonth$Month_factor ,                                    # Change ordering manually
                                      levels = c("May", "Jun", "Jul", "Aug", "Sep","All months"))

# basic plot
ggplot(Secchi_byMonth, aes(x=Month_factor, y=Secchi_MedianDiffPct))+
  geom_bar(stat='identity', fill='firebrick')+
  theme_classic()+
  scale_y_continuous('Percent difference',limits=common_y_limits, breaks=common_y_breaks)+
  scale_x_discrete('Month')+
  theme(axis.title.y=element_text(color='black'),
        axis.title.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))+
  ggtitle('Secchi')+
  geom_hline(yintercept = 0, linetype='dashed')


## pH
pH <- waterquality[,c('pH','Month_factor','Type','ConnClass')]
pH_burned <- subset(pH, Type=='Burned')
pH_burned_drainage <- subset(pH_burned, ConnClass=='Drainage')
pH_burned_isolated <- subset(pH_burned, ConnClass=='Isolated')
pH_control <- subset(pH, Type=='Control')
pH_control_drainage <- subset(pH_control, ConnClass=='Drainage')
pH_control_isolated <- subset(pH_control, ConnClass=='Isolated')
pH_burned_median <- median(pH_burned$pH, na.rm=T)
pH_burned_drainage_median <- median(pH_burned_drainage$pH, na.rm=T)
pH_burned_isolated_median <- median(pH_burned_isolated$pH, na.rm=T)
pH_control_median <- median(pH_control$pH, na.rm=T)
pH_control_drainage_median <- median(pH_control_drainage$pH, na.rm=T)
pH_control_isolated_median <- median(pH_control_isolated$pH, na.rm=T)

pH_byMonth_burned <- pH_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(pH_MedianBurned_all=median(pH, na.rm=T),
                   npH_burned_all=sum(!is.na(pH)))

pH_byMonth_burned_drainage <- pH_burned_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(pH_MedianBurned_drainage=median(pH, na.rm=T),
                   npH_burned_drainage=sum(!is.na(pH)))

pH_byMonth_burned_isolated <- pH_burned_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(pH_MedianBurned_isolated=median(pH, na.rm=T),
                   npH_burned_isolated=sum(!is.na(pH)))

pH_byMonth_control <- pH_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(pH_MedianControl_all=median(pH, na.rm=T),
                   npH_control_all=sum(!is.na(pH)))

pH_byMonth_control_drainage <- pH_control_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(pH_MedianControl_drainage=median(pH, na.rm=T),
                   npH_control_drainage=sum(!is.na(pH)))

pH_byMonth_control_isolated <- pH_control_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(pH_MedianControl_isolated=median(pH, na.rm=T),
                   npH_control_isolated=sum(!is.na(pH)))

pH_merge_list <- list(pH_byMonth_burned, pH_byMonth_control, 
                      pH_byMonth_burned_drainage, pH_byMonth_control_drainage,
                      pH_byMonth_burned_isolated, pH_byMonth_control_isolated)
#pH_byMonth <- merge(pH_byMonth_burned, pH_byMonth_control, by='Month_factor', all=T)
pH_byMonth <- pH_merge_list %>% reduce(full_join, by='Month_factor')

pH_byMonth <- pH_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
pH_byMonth <- as.data.frame(pH_byMonth)
pH_byMonth[6,] <- c('All months', pH_burned_median, sum(pH_byMonth$npH_burned_all), 
                    pH_control_median, sum(pH_byMonth$npH_control_all),
                    pH_burned_drainage_median, sum(pH_byMonth$npH_burned_drainage),
                    pH_control_drainage_median, sum(pH_byMonth$npH_control_drainage),
                    pH_burned_isolated_median, sum(pH_byMonth$npH_burned_isolated),
                    pH_control_isolated_median, sum(pH_byMonth$npH_control_isolated))
pH_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
pH_byMonth[,c(2:13)] <- pH_byMonth[,c(2:13)] %>% mutate_if(is.character, as.numeric)
pH_byMonth$pH_MedianDiffPct <- ((pH_byMonth$pH_MedianBurned_all - pH_byMonth$pH_MedianControl_all)/pH_byMonth$pH_MedianControl_all)*100
pH_byMonth$pH_MedianDiffAbs <-  pH_byMonth$pH_MedianBurned_all - pH_byMonth$pH_MedianControl_all
pH_byMonth$pH_MedianDrainageDiffPct <- ((pH_byMonth$pH_MedianBurned_drainage - pH_byMonth$pH_MedianControl_drainage)/pH_byMonth$pH_MedianControl_drainage)*100
pH_byMonth$pH_MedianDrainageDiffAbs <-  pH_byMonth$pH_MedianBurned_drainage - pH_byMonth$pH_MedianControl_drainage
pH_byMonth$pH_MedianIsolatedDiffPct <- ((pH_byMonth$pH_MedianBurned_isolated - pH_byMonth$pH_MedianControl_isolated)/pH_byMonth$pH_MedianControl_isolated)*100
pH_byMonth$pH_MedianIsolatedDiffAbs <-  pH_byMonth$pH_MedianBurned_isolated - pH_byMonth$pH_MedianControl_isolated

#pH_byMonth[6,3] <- sum(pH_byMonth$npH_burned, na.rm=T)
#pH_byMonth[6,5] <- sum(pH_byMonth$npH_control, na.rm=T)
pH_formatted <- data.frame(Month=c(month_order, 'All months'), 
                           pH_burned_drainage=paste0(pH_byMonth$pH_MedianBurned_drainage, ', ', pH_byMonth$npH_burned_drainage),
                           pH_control_drainage=paste0(pH_byMonth$pH_MedianControl_drainage, ', ', pH_byMonth$npH_control_drainage),
                           pH_diff_drainage=paste0(pH_byMonth$pH_MedianDrainageDiffAbs, ', ', round(pH_byMonth$pH_MedianDrainageDiffPct,2), '%'),
                           pH_burned_isolated=paste0(pH_byMonth$pH_MedianBurned_isolated, ', ', pH_byMonth$npH_burned_isolated),
                           pH_control_isolated=paste0(pH_byMonth$pH_MedianControl_isolated, ', ', pH_byMonth$npH_control_isolated),
                           pH_diff_isolated=paste0(pH_byMonth$pH_MedianIsolatedDiffAbs, ', ', round(pH_byMonth$pH_MedianIsolatedDiffPct,2), '%'),
                           pH_burned_all=paste0(pH_byMonth$pH_MedianBurned_all, ', ', pH_byMonth$npH_burned_all),
                           pH_control_all=paste0(pH_byMonth$pH_MedianControl_all, ', ', pH_byMonth$npH_control_all),
                           pH_diff_all=paste0(pH_byMonth$pH_MedianDiffAbs, ', ', round(pH_byMonth$pH_MedianDiffPct,2), '%'))

pH_byMonth$Month_factor <- factor(pH_byMonth$Month_factor ,                                    # Change ordering manually
                                  levels = c("May", "Jun", "Jul", "Aug", "Sep","All months"))

# basic plot
ggplot(pH_byMonth, aes(x=Month_factor, y=pH_MedianDiffPct))+
  geom_bar(stat='identity', fill='firebrick')+
  theme_classic()+
  scale_y_continuous('Percent difference',limits=common_y_limits, breaks=common_y_breaks)+
  scale_x_discrete('Month')+
  theme(axis.title.y=element_text(color='black'),
        axis.title.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))+
  ggtitle('pH')+
  geom_hline(yintercept = 0, linetype='dashed')


## WaterTemp_C
WaterTemp_C <- waterquality[,c('WaterTemp_C','Month_factor','Type','ConnClass')]
WaterTemp_C_burned <- subset(WaterTemp_C, Type=='Burned')
WaterTemp_C_burned_drainage <- subset(WaterTemp_C_burned, ConnClass=='Drainage')
WaterTemp_C_burned_isolated <- subset(WaterTemp_C_burned, ConnClass=='Isolated')
WaterTemp_C_control <- subset(WaterTemp_C, Type=='Control')
WaterTemp_C_control_drainage <- subset(WaterTemp_C_control, ConnClass=='Drainage')
WaterTemp_C_control_isolated <- subset(WaterTemp_C_control, ConnClass=='Isolated')
WaterTemp_C_burned_median <- median(WaterTemp_C_burned$WaterTemp_C, na.rm=T)
WaterTemp_C_burned_drainage_median <- median(WaterTemp_C_burned_drainage$WaterTemp_C, na.rm=T)
WaterTemp_C_burned_isolated_median <- median(WaterTemp_C_burned_isolated$WaterTemp_C, na.rm=T)
WaterTemp_C_control_median <- median(WaterTemp_C_control$WaterTemp_C, na.rm=T)
WaterTemp_C_control_drainage_median <- median(WaterTemp_C_control_drainage$WaterTemp_C, na.rm=T)
WaterTemp_C_control_isolated_median <- median(WaterTemp_C_control_isolated$WaterTemp_C, na.rm=T)

WaterTemp_C_byMonth_burned <- WaterTemp_C_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(WaterTemp_C_MedianBurned_all=median(WaterTemp_C, na.rm=T),
                   nWaterTemp_C_burned_all=sum(!is.na(WaterTemp_C)))

WaterTemp_C_byMonth_burned_drainage <- WaterTemp_C_burned_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(WaterTemp_C_MedianBurned_drainage=median(WaterTemp_C, na.rm=T),
                   nWaterTemp_C_burned_drainage=sum(!is.na(WaterTemp_C)))

WaterTemp_C_byMonth_burned_isolated <- WaterTemp_C_burned_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(WaterTemp_C_MedianBurned_isolated=median(WaterTemp_C, na.rm=T),
                   nWaterTemp_C_burned_isolated=sum(!is.na(WaterTemp_C)))

WaterTemp_C_byMonth_control <- WaterTemp_C_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(WaterTemp_C_MedianControl_all=median(WaterTemp_C, na.rm=T),
                   nWaterTemp_C_control_all=sum(!is.na(WaterTemp_C)))

WaterTemp_C_byMonth_control_drainage <- WaterTemp_C_control_drainage %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(WaterTemp_C_MedianControl_drainage=median(WaterTemp_C, na.rm=T),
                   nWaterTemp_C_control_drainage=sum(!is.na(WaterTemp_C)))

WaterTemp_C_byMonth_control_isolated <- WaterTemp_C_control_isolated %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(WaterTemp_C_MedianControl_isolated=median(WaterTemp_C, na.rm=T),
                   nWaterTemp_C_control_isolated=sum(!is.na(WaterTemp_C)))

WaterTemp_C_merge_list <- list(WaterTemp_C_byMonth_burned, WaterTemp_C_byMonth_control, 
                               WaterTemp_C_byMonth_burned_drainage, WaterTemp_C_byMonth_control_drainage,
                               WaterTemp_C_byMonth_burned_isolated, WaterTemp_C_byMonth_control_isolated)
#WaterTemp_C_byMonth <- merge(WaterTemp_C_byMonth_burned, WaterTemp_C_byMonth_control, by='Month_factor', all=T)
WaterTemp_C_byMonth <- WaterTemp_C_merge_list %>% reduce(full_join, by='Month_factor')

WaterTemp_C_byMonth <- WaterTemp_C_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
WaterTemp_C_byMonth <- as.data.frame(WaterTemp_C_byMonth)
WaterTemp_C_byMonth[6,] <- c('All months', WaterTemp_C_burned_median, sum(WaterTemp_C_byMonth$nWaterTemp_C_burned_all), 
                             WaterTemp_C_control_median, sum(WaterTemp_C_byMonth$nWaterTemp_C_control_all),
                             WaterTemp_C_burned_drainage_median, sum(WaterTemp_C_byMonth$nWaterTemp_C_burned_drainage),
                             WaterTemp_C_control_drainage_median, sum(WaterTemp_C_byMonth$nWaterTemp_C_control_drainage),
                             WaterTemp_C_burned_isolated_median, sum(WaterTemp_C_byMonth$nWaterTemp_C_burned_isolated),
                             WaterTemp_C_control_isolated_median, sum(WaterTemp_C_byMonth$nWaterTemp_C_control_isolated))
WaterTemp_C_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
WaterTemp_C_byMonth[,c(2:13)] <- WaterTemp_C_byMonth[,c(2:13)] %>% mutate_if(is.character, as.numeric)
WaterTemp_C_byMonth$WaterTemp_C_MedianDiffPct <- ((WaterTemp_C_byMonth$WaterTemp_C_MedianBurned_all - WaterTemp_C_byMonth$WaterTemp_C_MedianControl_all)/WaterTemp_C_byMonth$WaterTemp_C_MedianControl_all)*100
WaterTemp_C_byMonth$WaterTemp_C_MedianDiffAbs <-  WaterTemp_C_byMonth$WaterTemp_C_MedianBurned_all - WaterTemp_C_byMonth$WaterTemp_C_MedianControl_all
WaterTemp_C_byMonth$WaterTemp_C_MedianDrainageDiffPct <- ((WaterTemp_C_byMonth$WaterTemp_C_MedianBurned_drainage - WaterTemp_C_byMonth$WaterTemp_C_MedianControl_drainage)/WaterTemp_C_byMonth$WaterTemp_C_MedianControl_drainage)*100
WaterTemp_C_byMonth$WaterTemp_C_MedianDrainageDiffAbs <-  WaterTemp_C_byMonth$WaterTemp_C_MedianBurned_drainage - WaterTemp_C_byMonth$WaterTemp_C_MedianControl_drainage
WaterTemp_C_byMonth$WaterTemp_C_MedianIsolatedDiffPct <- ((WaterTemp_C_byMonth$WaterTemp_C_MedianBurned_isolated - WaterTemp_C_byMonth$WaterTemp_C_MedianControl_isolated)/WaterTemp_C_byMonth$WaterTemp_C_MedianControl_isolated)*100
WaterTemp_C_byMonth$WaterTemp_C_MedianIsolatedDiffAbs <-  WaterTemp_C_byMonth$WaterTemp_C_MedianBurned_isolated - WaterTemp_C_byMonth$WaterTemp_C_MedianControl_isolated

#WaterTemp_C_byMonth[6,3] <- sum(WaterTemp_C_byMonth$nWaterTemp_C_burned, na.rm=T)
#WaterTemp_C_byMonth[6,5] <- sum(WaterTemp_C_byMonth$nWaterTemp_C_control, na.rm=T)
WaterTemp_C_formatted <- data.frame(Month=c(month_order, 'All months'), 
                                    WaterTemp_C_burned_drainage=paste0(WaterTemp_C_byMonth$WaterTemp_C_MedianBurned_drainage, ', ', WaterTemp_C_byMonth$nWaterTemp_C_burned_drainage),
                                    WaterTemp_C_control_drainage=paste0(WaterTemp_C_byMonth$WaterTemp_C_MedianControl_drainage, ', ', WaterTemp_C_byMonth$nWaterTemp_C_control_drainage),
                                    WaterTemp_C_diff_drainage=paste0(WaterTemp_C_byMonth$WaterTemp_C_MedianDrainageDiffAbs, ', ', round(WaterTemp_C_byMonth$WaterTemp_C_MedianDrainageDiffPct,2), '%'),
                                    WaterTemp_C_burned_isolated=paste0(WaterTemp_C_byMonth$WaterTemp_C_MedianBurned_isolated, ', ', WaterTemp_C_byMonth$nWaterTemp_C_burned_isolated),
                                    WaterTemp_C_control_isolated=paste0(WaterTemp_C_byMonth$WaterTemp_C_MedianControl_isolated, ', ', WaterTemp_C_byMonth$nWaterTemp_C_control_isolated),
                                    WaterTemp_C_diff_isolated=paste0(WaterTemp_C_byMonth$WaterTemp_C_MedianIsolatedDiffAbs, ', ', round(WaterTemp_C_byMonth$WaterTemp_C_MedianIsolatedDiffPct,2), '%'),
                                    WaterTemp_C_burned_all=paste0(WaterTemp_C_byMonth$WaterTemp_C_MedianBurned_all, ', ', WaterTemp_C_byMonth$nWaterTemp_C_burned_all),
                                    WaterTemp_C_control_all=paste0(WaterTemp_C_byMonth$WaterTemp_C_MedianControl_all, ', ', WaterTemp_C_byMonth$nWaterTemp_C_control_all),
                                    WaterTemp_C_diff_all=paste0(WaterTemp_C_byMonth$WaterTemp_C_MedianDiffAbs, ', ', round(WaterTemp_C_byMonth$WaterTemp_C_MedianDiffPct,2), '%'))

WaterTemp_C_byMonth$Month_factor <- factor(WaterTemp_C_byMonth$Month_factor ,                                    # Change ordering manually
                                           levels = c("May", "Jun", "Jul", "Aug", "Sep","All months"))

# basic plot
ggplot(WaterTemp_C_byMonth, aes(x=Month_factor, y=WaterTemp_C_MedianDiffPct))+
  geom_bar(stat='identity', fill='firebrick')+
  theme_classic()+
  scale_y_continuous('Percent difference',limits=common_y_limits, breaks=common_y_breaks)+
  scale_x_discrete('Month')+
  theme(axis.title.y=element_text(color='black'),
        axis.title.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))+
  ggtitle('WaterTemp_C')+
  geom_hline(yintercept = 0, linetype='dashed')


## SpecCond_uScm
# SpecCond_uScm <- waterquality[,c('SpecCond_uScm','Month_factor','Type')]
# SpecCond_uScm_burned <- subset(SpecCond_uScm, Type=='Burned')
# SpecCond_uScm_control <- subset(SpecCond_uScm, Type=='Control')
# SpecCond_uScm_burned_median <- median(SpecCond_uScm_burned$SpecCond_uScm, na.rm=T)
# SpecCond_uScm_control_median <- median(SpecCond_uScm_control$SpecCond_uScm, na.rm=T)
# 
# SpecCond_uScm_byMonth_burned <- SpecCond_uScm_burned %>%
#   dplyr::group_by(Month_factor) %>%
#   dplyr::summarize(SpecCond_uScm_MedianBurned=median(SpecCond_uScm, na.rm=T),
#                    nSpecCond_uScm_burned=sum(!is.na(SpecCond_uScm)))
# 
# SpecCond_uScm_byMonth_control <- SpecCond_uScm_control %>%
#   dplyr::group_by(Month_factor) %>%
#   dplyr::summarize(SpecCond_uScm_MedianControl=median(SpecCond_uScm, na.rm=T),
#                    nSpecCond_uScm_control=sum(!is.na(SpecCond_uScm)))
# 
# SpecCond_uScm_byMonth <- merge(SpecCond_uScm_byMonth_burned, SpecCond_uScm_byMonth_control, by='Month_factor', all=T)
# SpecCond_uScm_byMonth <- SpecCond_uScm_byMonth %>%
#   mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
#   arrange(Month_factor)
# SpecCond_uScm_byMonth[6,] <- c('All months', SpecCond_uScm_burned_median, NA, SpecCond_uScm_control_median,NA)
# SpecCond_uScm_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
# SpecCond_uScm_byMonth[,c(2:5)] <- SpecCond_uScm_byMonth[,c(2:5)] %>% mutate_if(is.character, as.numeric)
# SpecCond_uScm_byMonth$SpecCond_uScm_MedianDiffPct <- ((SpecCond_uScm_byMonth$SpecCond_uScm_MedianBurned - SpecCond_uScm_byMonth$SpecCond_uScm_MedianControl)/SpecCond_uScm_byMonth$SpecCond_uScm_MedianControl)*100
# SpecCond_uScm_byMonth$SpecCond_uScm_MedianDiffAbs <-  SpecCond_uScm_byMonth$SpecCond_uScm_MedianBurned - SpecCond_uScm_byMonth$SpecCond_uScm_MedianControl
# SpecCond_uScm_byMonth[6,3] <- sum(SpecCond_uScm_byMonth$nSpecCond_uScm_burned, na.rm=T)
# SpecCond_uScm_byMonth[6,5] <- sum(SpecCond_uScm_byMonth$nSpecCond_uScm_control, na.rm=T)
# SpecCond_uScm_formatted <- data.frame(Month=c(month_order, 'All months'), 
#                                       SpecCond_uScm_burned=paste0(SpecCond_uScm_byMonth$SpecCond_uScm_MedianBurned, ', ', SpecCond_uScm_byMonth$nSpecCond_uScm_burned),
#                                       SpecCond_uScm_control=paste0(SpecCond_uScm_byMonth$SpecCond_uScm_MedianControl, ', ', SpecCond_uScm_byMonth$nSpecCond_uScm_control),
#                                       SpecCond_uScm_diff=paste0(SpecCond_uScm_byMonth$SpecCond_uScm_MedianDiffAbs, ', ', round(SpecCond_uScm_byMonth$SpecCond_uScm_MedianDiffPct,2), '%'))
# 
# SpecCond_uScm_byMonth$Month_factor <- factor(SpecCond_uScm_byMonth$Month_factor ,                                    # Change ordering manually
#                                              levels = c("May", "Jun", "Jul", "Aug", "Sep","All months"))
# 
# # basic plot
# ggplot(SpecCond_uScm_byMonth, aes(x=Month_factor, y=SpecCond_uScm_MedianDiffPct))+
#   geom_bar(stat='identity', fill='firebrick')+
#   theme_classic()+
#   scale_y_continuous('Percent difference',limits=common_y_limits, breaks=common_y_breaks)+
#   scale_x_discrete('Month')+
#   theme(axis.title.y=element_text(color='black'),
#         axis.title.x=element_text(color='black'),
#         axis.text.y=element_text(color='black'),
#         axis.text.x=element_text(color='black'))+
#   ggtitle('SpecCond_uScm')+
#   geom_hline(yintercept = 0, linetype='dashed')
# 
# ## ANC
# ANC_mgCaCO3L <- waterquality[,c('ANC_mgCaCO3L','Month_factor','Type')]
# ANC_mgCaCO3L_burned <- subset(ANC_mgCaCO3L, Type=='Burned')
# ANC_mgCaCO3L_control <- subset(ANC_mgCaCO3L, Type=='Control')
# ANC_mgCaCO3L_burned_median <- median(ANC_mgCaCO3L_burned$ANC_mgCaCO3L, na.rm=T)
# ANC_mgCaCO3L_control_median <- median(ANC_mgCaCO3L_control$ANC_mgCaCO3L, na.rm=T)
# 
# ANC_mgCaCO3L_byMonth_burned <- ANC_mgCaCO3L_burned %>%
#   dplyr::group_by(Month_factor) %>%
#   dplyr::summarize(ANC_mgCaCO3L_MedianBurned=median(ANC_mgCaCO3L, na.rm=T),
#                    nANC_mgCaCO3L_burned=sum(!is.na(ANC_mgCaCO3L)))
# 
# ANC_mgCaCO3L_byMonth_control <- ANC_mgCaCO3L_control %>%
#   dplyr::group_by(Month_factor) %>%
#   dplyr::summarize(ANC_mgCaCO3L_MedianControl=median(ANC_mgCaCO3L, na.rm=T),
#                    nANC_mgCaCO3L_control=sum(!is.na(ANC_mgCaCO3L)))
# 
# ANC_mgCaCO3L_byMonth <- merge(ANC_mgCaCO3L_byMonth_burned, ANC_mgCaCO3L_byMonth_control, by='Month_factor', all=T)
# ANC_mgCaCO3L_byMonth <- ANC_mgCaCO3L_byMonth %>%
#   mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
#   arrange(Month_factor)
# ANC_mgCaCO3L_byMonth[6,] <- c('All months', ANC_mgCaCO3L_burned_median, NA, ANC_mgCaCO3L_control_median,NA)
# ANC_mgCaCO3L_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
# ANC_mgCaCO3L_byMonth[,c(2:5)] <- ANC_mgCaCO3L_byMonth[,c(2:5)] %>% mutate_if(is.character, as.numeric)
# ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianDiffPct <- ((ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianBurned - ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianControl)/ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianControl)*100
# ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianDiffAbs <-  ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianBurned - ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianControl
# ANC_mgCaCO3L_byMonth[6,3] <- sum(ANC_mgCaCO3L_byMonth$nANC_mgCaCO3L_burned, na.rm=T)
# ANC_mgCaCO3L_byMonth[6,5] <- sum(ANC_mgCaCO3L_byMonth$nANC_mgCaCO3L_control, na.rm=T)
# ANC_mgCaCO3L_formatted <- data.frame(Month=c(month_order, 'All months'), 
#                                      ANC_mgCaCO3L_burned=paste0(ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianBurned, ', ', ANC_mgCaCO3L_byMonth$nANC_mgCaCO3L_burned),
#                                      ANC_mgCaCO3L_control=paste0(ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianControl, ', ', ANC_mgCaCO3L_byMonth$nANC_mgCaCO3L_control),
#                                      ANC_mgCaCO3L_diff=paste0(ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianDiffAbs, ', ', round(ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianDiffPct,2), '%'))
# 
# ANC_mgCaCO3L_byMonth$Month_factor <- factor(ANC_mgCaCO3L_byMonth$Month_factor ,                                    # Change ordering manually
#                                             levels = c("May", "Jun", "Jul", "Aug", "Sep","All months"))
# 
# # basic plot
# ggplot(ANC_mgCaCO3L_byMonth, aes(x=Month_factor, y=ANC_mgCaCO3L_MedianDiffPct))+
#   geom_bar(stat='identity', fill='firebrick')+
#   theme_classic()+
#   scale_y_continuous('Percent difference',limits=common_y_limits, breaks=common_y_breaks)+
#   scale_x_discrete('Month')+
#   theme(axis.title.y=element_text(color='black'),
#         axis.title.x=element_text(color='black'),
#         axis.text.y=element_text(color='black'),
#         axis.text.x=element_text(color='black'))+
#   ggtitle('ANC_mgCaCO3L')+
#   geom_hline(yintercept = 0, linetype='dashed')

## combine for export

#put all data frames into list
#df_list <- list(TP_formatted, TN_formatted, DOC_formatted, TSS_formatted,
#                Chloro_formatted, Secchi_formatted, pH_formatted, ANC_mgCaCO3L_formatted,
#                SpecCond_uScm_formatted, NH4N_formatted, WaterTemp_C_formatted)

df_list <- list(TP_formatted, TN_formatted, DOC_formatted, TSS_formatted,
                Chloro_formatted, Secchi_formatted, pH_formatted, WaterTemp_C_formatted)

#merge all data frames in list
bigmamatable <- df_list %>% reduce(full_join, by='Month')
#write.csv(bigmamatable, file='Data/WaterQuality/burned_control_change_summary.csv', row.names=F)
#write.csv(bigmamatable, file='Data/WaterQuality/burned_control_change_summary_conn.csv', row.names=F)
