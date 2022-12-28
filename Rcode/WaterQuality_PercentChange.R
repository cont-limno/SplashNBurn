################### Control vs. burned percent change #########################
# Date: 11-17-22
# updated: 12-12-22; rerun with corrected July Teamster TP data
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
TP_byMonth$TP_MedianIsolatedDiffPct <- ((TP_byMonth$TP_MedianBurned_drainage - TP_byMonth$TP_MedianControl_isolated)/TP_byMonth$TP_MedianControl_isolated)*100
TP_byMonth$TP_MedianIsolatedDiffAbs <-  TP_byMonth$TP_MedianBurned_drainage - TP_byMonth$TP_MedianControl_isolated

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
TN <- waterquality[,c('TN_ppb','Month_factor','Type')]
TN_burned <- subset(TN, Type=='Burned')
TN_control <- subset(TN, Type=='Control')
TN_burned_median <- median(TN_burned$TN_ppb, na.rm=T)
TN_control_median <- median(TN_control$TN_ppb, na.rm=T)

TN_byMonth_burned <- TN_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TN_MedianBurned=median(TN_ppb, na.rm=T),
                   nTN_burned=sum(!is.na(TN_ppb)))

TN_byMonth_control <- TN_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TN_MedianControl=median(TN_ppb, na.rm=T),
                   nTN_control=sum(!is.na(TN_ppb)))

TN_byMonth <- merge(TN_byMonth_burned, TN_byMonth_control, by='Month_factor', all=T)
TN_byMonth <- TN_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
TN_byMonth[6,] <- c('All months', TN_burned_median, NA, TN_control_median,NA)
TN_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
TN_byMonth[,c(2:5)] <- TN_byMonth[,c(2:5)] %>% mutate_if(is.character, as.numeric)
TN_byMonth$TN_MedianDiffPct <- ((TN_byMonth$TN_MedianBurned - TN_byMonth$TN_MedianControl)/TN_byMonth$TN_MedianControl)*100
TN_byMonth$TN_MedianDiffAbs <-  TN_byMonth$TN_MedianBurned - TN_byMonth$TN_MedianControl
TN_byMonth[6,3] <- sum(TN_byMonth$nTN_burned, na.rm=T)
TN_byMonth[6,5] <- sum(TN_byMonth$nTN_control, na.rm=T)
TN_formatted <- data.frame(Month=c(month_order, 'All months'), 
                           TN_burned=paste0(TN_byMonth$TN_MedianBurned, ', ', TN_byMonth$nTN_burned),
                           TN_control=paste0(TN_byMonth$TN_MedianControl, ', ', TN_byMonth$nTN_control),
                           TN_diff=paste0(TN_byMonth$TN_MedianDiffAbs, ', ', round(TN_byMonth$TN_MedianDiffPct,2), '%'))

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
NH4N <- waterquality[,c('NH4N_ppb','Month_factor','Type')]
NH4N_burned <- subset(NH4N, Type=='Burned')
NH4N_control <- subset(NH4N, Type=='Control')
NH4N_burned_median <- median(NH4N_burned$NH4N_ppb, na.rm=T)
NH4N_control_median <- median(NH4N_control$NH4N_ppb, na.rm=T)

NH4N_byMonth_burned <- NH4N_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(NH4N_MedianBurned=median(NH4N_ppb, na.rm=T),
                   nNH4N_burned=sum(!is.na(NH4N_ppb)))

NH4N_byMonth_control <- NH4N_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(NH4N_MedianControl=median(NH4N_ppb, na.rm=T),
                   nNH4N_control=sum(!is.na(NH4N_ppb)))

NH4N_byMonth <- merge(NH4N_byMonth_burned, NH4N_byMonth_control, by='Month_factor', all=T)
NH4N_byMonth <- NH4N_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
NH4N_byMonth[6,] <- c('All months', NH4N_burned_median, NA, NH4N_control_median,NA)
NH4N_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
NH4N_byMonth[,c(2:5)] <- NH4N_byMonth[,c(2:5)] %>% mutate_if(is.character, as.numeric)
NH4N_byMonth$NH4N_MedianDiffPct <- ((NH4N_byMonth$NH4N_MedianBurned - NH4N_byMonth$NH4N_MedianControl)/NH4N_byMonth$NH4N_MedianControl)*100
NH4N_byMonth$NH4N_MedianDiffAbs <-  NH4N_byMonth$NH4N_MedianBurned - NH4N_byMonth$NH4N_MedianControl
NH4N_byMonth[6,3] <- sum(NH4N_byMonth$nNH4N_burned, na.rm=T)
NH4N_byMonth[6,5] <- sum(NH4N_byMonth$nNH4N_control, na.rm=T)
NH4N_formatted <- data.frame(Month=c(month_order, 'All months'), 
                             NH4N_burned=paste0(NH4N_byMonth$NH4N_MedianBurned, ', ', NH4N_byMonth$nNH4N_burned),
                             NH4N_control=paste0(NH4N_byMonth$NH4N_MedianControl, ', ', NH4N_byMonth$nNH4N_control),
                             NH4N_diff=paste0(NH4N_byMonth$NH4N_MedianDiffAbs, ', ', round(NH4N_byMonth$NH4N_MedianDiffPct,2), '%'))

NH4N_byMonth$Month_factor <- factor(NH4N_byMonth$Month_factor ,                                    # Change ordering manually
                                    levels = c("May", "Jun", "Jul", "Aug", "Sep","All months"))

# basic plot
ggplot(NH4N_byMonth, aes(x=Month_factor, y=NH4N_MedianDiffPct))+
  geom_bar(stat='identity', fill='firebrick')+
  theme_classic()+
  scale_y_continuous('Percent difference',limits=common_y_limits, breaks=common_y_breaks)+
  scale_x_discrete('Month')+
  theme(axis.title.y=element_text(color='black'),
        axis.title.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))+
  ggtitle('NH4N')+
  geom_hline(yintercept = 0, linetype='dashed')

## DOC
DOC <- waterquality[,c('DOC_ppm','Month_factor','Type')]
DOC_burned <- subset(DOC, Type=='Burned')
DOC_control <- subset(DOC, Type=='Control')
DOC_burned_median <- median(DOC_burned$DOC_ppm, na.rm=T)
DOC_control_median <- median(DOC_control$DOC_ppm, na.rm=T)

DOC_byMonth_burned <- DOC_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(DOC_MedianBurned=median(DOC_ppm, na.rm=T),
                   nDOC_burned=sum(!is.na(DOC_ppm)))

DOC_byMonth_control <- DOC_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(DOC_MedianControl=median(DOC_ppm, na.rm=T),
                   nDOC_control=sum(!is.na(DOC_ppm)))

DOC_byMonth <- merge(DOC_byMonth_burned, DOC_byMonth_control, by='Month_factor', all=T)
DOC_byMonth <- DOC_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
DOC_byMonth[6,] <- c('All months', DOC_burned_median, NA, DOC_control_median,NA)
DOC_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
DOC_byMonth[,c(2:5)] <- DOC_byMonth[,c(2:5)] %>% mutate_if(is.character, as.numeric)
DOC_byMonth$DOC_MedianDiffPct <- ((DOC_byMonth$DOC_MedianBurned - DOC_byMonth$DOC_MedianControl)/DOC_byMonth$DOC_MedianControl)*100
DOC_byMonth$DOC_MedianDiffAbs <-  DOC_byMonth$DOC_MedianBurned - DOC_byMonth$DOC_MedianControl
DOC_byMonth[6,3] <- sum(DOC_byMonth$nDOC_burned, na.rm=T)
DOC_byMonth[6,5] <- sum(DOC_byMonth$nDOC_control, na.rm=T)
DOC_formatted <- data.frame(Month=c(month_order, 'All months'), 
                            DOC_burned=paste0(DOC_byMonth$DOC_MedianBurned, ', ', DOC_byMonth$nDOC_burned),
                            DOC_control=paste0(DOC_byMonth$DOC_MedianControl, ', ', DOC_byMonth$nDOC_control),
                            DOC_diff=paste0(DOC_byMonth$DOC_MedianDiffAbs, ', ', round(DOC_byMonth$DOC_MedianDiffPct,2), '%'))

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
TSS <- waterquality[,c('TSS_mgL','Month_factor','Type')]
TSS_burned <- subset(TSS, Type=='Burned')
TSS_control <- subset(TSS, Type=='Control')
TSS_burned_median <- median(TSS_burned$TSS_mgL, na.rm=T)
TSS_control_median <- median(TSS_control$TSS_mgL, na.rm=T)

TSS_byMonth_burned <- TSS_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TSS_MedianBurned=median(TSS_mgL, na.rm=T),
                   nTSS_burned=sum(!is.na(TSS_mgL)))

TSS_byMonth_control <- TSS_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(TSS_MedianControl=median(TSS_mgL, na.rm=T),
                   nTSS_control=sum(!is.na(TSS_mgL)))

TSS_byMonth <- merge(TSS_byMonth_burned, TSS_byMonth_control, by='Month_factor', all=T)
TSS_byMonth <- TSS_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
TSS_byMonth[6,] <- c('All months', TSS_burned_median, NA, TSS_control_median,NA)
TSS_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
TSS_byMonth[,c(2:5)] <- TSS_byMonth[,c(2:5)] %>% mutate_if(is.character, as.numeric)
TSS_byMonth$TSS_MedianDiffPct <- ((TSS_byMonth$TSS_MedianBurned - TSS_byMonth$TSS_MedianControl)/TSS_byMonth$TSS_MedianControl)*100
TSS_byMonth$TSS_MedianDiffAbs <-  TSS_byMonth$TSS_MedianBurned - TSS_byMonth$TSS_MedianControl
TSS_byMonth[6,3] <- sum(TSS_byMonth$nTSS_burned, na.rm=T)
TSS_byMonth[6,5] <- sum(TSS_byMonth$nTSS_control, na.rm=T)
TSS_formatted <- data.frame(Month=c(month_order, 'All months'), 
                            TSS_burned=paste0(TSS_byMonth$TSS_MedianBurned, ', ', TSS_byMonth$nTSS_burned),
                            TSS_control=paste0(TSS_byMonth$TSS_MedianControl, ', ', TSS_byMonth$nTSS_control),
                            TSS_diff=paste0(TSS_byMonth$TSS_MedianDiffAbs, ', ', round(TSS_byMonth$TSS_MedianDiffPct,2), '%'))

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
Chloro <- waterquality[,c('Chloro_ppb','Month_factor','Type')]
Chloro_burned <- subset(Chloro, Type=='Burned')
Chloro_control <- subset(Chloro, Type=='Control')
Chloro_burned_median <- median(Chloro_burned$Chloro_ppb, na.rm=T)
Chloro_control_median <- median(Chloro_control$Chloro_ppb, na.rm=T)

Chloro_byMonth_burned <- Chloro_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Chloro_MedianBurned=median(Chloro_ppb, na.rm=T),
                   nChloro_burned=sum(!is.na(Chloro_ppb)))

Chloro_byMonth_control <- Chloro_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Chloro_MedianControl=median(Chloro_ppb, na.rm=T),
                   nChloro_control=sum(!is.na(Chloro_ppb)))

Chloro_byMonth <- merge(Chloro_byMonth_burned, Chloro_byMonth_control, by='Month_factor', all=T)
Chloro_byMonth <- Chloro_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
Chloro_byMonth[6,] <- c('All months', Chloro_burned_median, NA, Chloro_control_median,NA)
Chloro_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
Chloro_byMonth[,c(2:5)] <- Chloro_byMonth[,c(2:5)] %>% mutate_if(is.character, as.numeric)
Chloro_byMonth$Chloro_MedianDiffPct <- ((Chloro_byMonth$Chloro_MedianBurned - Chloro_byMonth$Chloro_MedianControl)/Chloro_byMonth$Chloro_MedianControl)*100
Chloro_byMonth$Chloro_MedianDiffAbs <-  Chloro_byMonth$Chloro_MedianBurned - Chloro_byMonth$Chloro_MedianControl
Chloro_byMonth[6,3] <- sum(Chloro_byMonth$nChloro_burned, na.rm=T)
Chloro_byMonth[6,5] <- sum(Chloro_byMonth$nChloro_control, na.rm=T)
Chloro_formatted <- data.frame(Month=c(month_order, 'All months'), 
                               Chloro_burned=paste0(Chloro_byMonth$Chloro_MedianBurned, ', ', Chloro_byMonth$nChloro_burned),
                               Chloro_control=paste0(Chloro_byMonth$Chloro_MedianControl, ', ', Chloro_byMonth$nChloro_control),
                               Chloro_diff=paste0(Chloro_byMonth$Chloro_MedianDiffAbs, ', ', round(Chloro_byMonth$Chloro_MedianDiffPct,2), '%'))

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
Secchi <- waterquality[,c('SecchiDepth_m','Month_factor','Type')]
Secchi_burned <- subset(Secchi, Type=='Burned')
Secchi_control <- subset(Secchi, Type=='Control')
Secchi_burned_median <- median(Secchi_burned$SecchiDepth_m, na.rm=T)
Secchi_control_median <- median(Secchi_control$SecchiDepth_m, na.rm=T)

Secchi_byMonth_burned <- Secchi_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Secchi_MedianBurned=median(SecchiDepth_m, na.rm=T),
                   nSecchi_burned=sum(!is.na(SecchiDepth_m)))

Secchi_byMonth_control <- Secchi_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(Secchi_MedianControl=median(SecchiDepth_m, na.rm=T),
                   nSecchi_control=sum(!is.na(SecchiDepth_m)))

Secchi_byMonth <- merge(Secchi_byMonth_burned, Secchi_byMonth_control, by='Month_factor', all=T)
Secchi_byMonth <- Secchi_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
Secchi_byMonth[6,] <- c('All months', Secchi_burned_median, NA, Secchi_control_median,NA)
Secchi_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
Secchi_byMonth[,c(2:5)] <- Secchi_byMonth[,c(2:5)] %>% mutate_if(is.character, as.numeric)
Secchi_byMonth$Secchi_MedianDiffPct <- ((Secchi_byMonth$Secchi_MedianBurned - Secchi_byMonth$Secchi_MedianControl)/Secchi_byMonth$Secchi_MedianControl)*100
Secchi_byMonth$Secchi_MedianDiffAbs <-  Secchi_byMonth$Secchi_MedianBurned - Secchi_byMonth$Secchi_MedianControl
Secchi_byMonth[6,3] <- sum(Secchi_byMonth$nSecchi_burned, na.rm=T)
Secchi_byMonth[6,5] <- sum(Secchi_byMonth$nSecchi_control, na.rm=T)
Secchi_formatted <- data.frame(Month=c(month_order, 'All months'), 
                               Secchi_burned=paste0(Secchi_byMonth$Secchi_MedianBurned, ', ', Secchi_byMonth$nSecchi_burned),
                               Secchi_control=paste0(Secchi_byMonth$Secchi_MedianControl, ', ', Secchi_byMonth$nSecchi_control),
                               Secchi_diff=paste0(Secchi_byMonth$Secchi_MedianDiffAbs, ', ', round(Secchi_byMonth$Secchi_MedianDiffPct,2), '%'))

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
pH <- waterquality[,c('pH','Month_factor','Type')]
pH_burned <- subset(pH, Type=='Burned')
pH_control <- subset(pH, Type=='Control')
pH_burned_median <- median(pH_burned$pH, na.rm=T)
pH_control_median <- median(pH_control$pH, na.rm=T)

pH_byMonth_burned <- pH_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(pH_MedianBurned=median(pH, na.rm=T),
                   npH_burned=sum(!is.na(pH)))

pH_byMonth_control <- pH_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(pH_MedianControl=median(pH, na.rm=T),
                   npH_control=sum(!is.na(pH)))

pH_byMonth <- merge(pH_byMonth_burned, pH_byMonth_control, by='Month_factor', all=T)
pH_byMonth <- pH_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
pH_byMonth[6,] <- c('All months', pH_burned_median, NA, pH_control_median,NA)
pH_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
pH_byMonth[,c(2:5)] <- pH_byMonth[,c(2:5)] %>% mutate_if(is.character, as.numeric)
pH_byMonth$pH_MedianDiffPct <- ((pH_byMonth$pH_MedianBurned - pH_byMonth$pH_MedianControl)/pH_byMonth$pH_MedianControl)*100
pH_byMonth$pH_MedianDiffAbs <-  pH_byMonth$pH_MedianBurned - pH_byMonth$pH_MedianControl
pH_byMonth[6,3] <- sum(pH_byMonth$npH_burned, na.rm=T)
pH_byMonth[6,5] <- sum(pH_byMonth$npH_control, na.rm=T)
pH_formatted <- data.frame(Month=c(month_order, 'All months'), 
                           pH_burned=paste0(pH_byMonth$pH_MedianBurned, ', ', pH_byMonth$npH_burned),
                           pH_control=paste0(pH_byMonth$pH_MedianControl, ', ', pH_byMonth$npH_control),
                           pH_diff=paste0(pH_byMonth$pH_MedianDiffAbs, ', ', round(pH_byMonth$pH_MedianDiffPct,2), '%'))

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
WaterTemp_C <- waterquality[,c('WaterTemp_C','Month_factor','Type')]
WaterTemp_C_burned <- subset(WaterTemp_C, Type=='Burned')
WaterTemp_C_control <- subset(WaterTemp_C, Type=='Control')
WaterTemp_C_burned_median <- median(WaterTemp_C_burned$WaterTemp_C, na.rm=T)
WaterTemp_C_control_median <- median(WaterTemp_C_control$WaterTemp_C, na.rm=T)

WaterTemp_C_byMonth_burned <- WaterTemp_C_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(WaterTemp_C_MedianBurned=median(WaterTemp_C, na.rm=T),
                   nWaterTemp_C_burned=sum(!is.na(WaterTemp_C)))

WaterTemp_C_byMonth_control <- WaterTemp_C_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(WaterTemp_C_MedianControl=median(WaterTemp_C, na.rm=T),
                   nWaterTemp_C_control=sum(!is.na(WaterTemp_C)))

WaterTemp_C_byMonth <- merge(WaterTemp_C_byMonth_burned, WaterTemp_C_byMonth_control, by='Month_factor', all=T)
WaterTemp_C_byMonth <- WaterTemp_C_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
WaterTemp_C_byMonth[6,] <- c('All months', WaterTemp_C_burned_median, NA, WaterTemp_C_control_median,NA)
WaterTemp_C_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
WaterTemp_C_byMonth[,c(2:5)] <- WaterTemp_C_byMonth[,c(2:5)] %>% mutate_if(is.character, as.numeric)
WaterTemp_C_byMonth$WaterTemp_C_MedianDiffPct <- ((WaterTemp_C_byMonth$WaterTemp_C_MedianBurned - WaterTemp_C_byMonth$WaterTemp_C_MedianControl)/WaterTemp_C_byMonth$WaterTemp_C_MedianControl)*100
WaterTemp_C_byMonth$WaterTemp_C_MedianDiffAbs <-  WaterTemp_C_byMonth$WaterTemp_C_MedianBurned - WaterTemp_C_byMonth$WaterTemp_C_MedianControl
WaterTemp_C_byMonth[6,3] <- sum(WaterTemp_C_byMonth$nWaterTemp_C_burned, na.rm=T)
WaterTemp_C_byMonth[6,5] <- sum(WaterTemp_C_byMonth$nWaterTemp_C_control, na.rm=T)
WaterTemp_C_formatted <- data.frame(Month=c(month_order, 'All months'), 
                                    WaterTemp_C_burned=paste0(WaterTemp_C_byMonth$WaterTemp_C_MedianBurned, ', ', WaterTemp_C_byMonth$nWaterTemp_C_burned),
                                    WaterTemp_C_control=paste0(WaterTemp_C_byMonth$WaterTemp_C_MedianControl, ', ', WaterTemp_C_byMonth$nWaterTemp_C_control),
                                    WaterTemp_C_diff=paste0(WaterTemp_C_byMonth$WaterTemp_C_MedianDiffAbs, ', ', round(WaterTemp_C_byMonth$WaterTemp_C_MedianDiffPct,2), '%'))

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
SpecCond_uScm <- waterquality[,c('SpecCond_uScm','Month_factor','Type')]
SpecCond_uScm_burned <- subset(SpecCond_uScm, Type=='Burned')
SpecCond_uScm_control <- subset(SpecCond_uScm, Type=='Control')
SpecCond_uScm_burned_median <- median(SpecCond_uScm_burned$SpecCond_uScm, na.rm=T)
SpecCond_uScm_control_median <- median(SpecCond_uScm_control$SpecCond_uScm, na.rm=T)

SpecCond_uScm_byMonth_burned <- SpecCond_uScm_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(SpecCond_uScm_MedianBurned=median(SpecCond_uScm, na.rm=T),
                   nSpecCond_uScm_burned=sum(!is.na(SpecCond_uScm)))

SpecCond_uScm_byMonth_control <- SpecCond_uScm_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(SpecCond_uScm_MedianControl=median(SpecCond_uScm, na.rm=T),
                   nSpecCond_uScm_control=sum(!is.na(SpecCond_uScm)))

SpecCond_uScm_byMonth <- merge(SpecCond_uScm_byMonth_burned, SpecCond_uScm_byMonth_control, by='Month_factor', all=T)
SpecCond_uScm_byMonth <- SpecCond_uScm_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
SpecCond_uScm_byMonth[6,] <- c('All months', SpecCond_uScm_burned_median, NA, SpecCond_uScm_control_median,NA)
SpecCond_uScm_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
SpecCond_uScm_byMonth[,c(2:5)] <- SpecCond_uScm_byMonth[,c(2:5)] %>% mutate_if(is.character, as.numeric)
SpecCond_uScm_byMonth$SpecCond_uScm_MedianDiffPct <- ((SpecCond_uScm_byMonth$SpecCond_uScm_MedianBurned - SpecCond_uScm_byMonth$SpecCond_uScm_MedianControl)/SpecCond_uScm_byMonth$SpecCond_uScm_MedianControl)*100
SpecCond_uScm_byMonth$SpecCond_uScm_MedianDiffAbs <-  SpecCond_uScm_byMonth$SpecCond_uScm_MedianBurned - SpecCond_uScm_byMonth$SpecCond_uScm_MedianControl
SpecCond_uScm_byMonth[6,3] <- sum(SpecCond_uScm_byMonth$nSpecCond_uScm_burned, na.rm=T)
SpecCond_uScm_byMonth[6,5] <- sum(SpecCond_uScm_byMonth$nSpecCond_uScm_control, na.rm=T)
SpecCond_uScm_formatted <- data.frame(Month=c(month_order, 'All months'), 
                                      SpecCond_uScm_burned=paste0(SpecCond_uScm_byMonth$SpecCond_uScm_MedianBurned, ', ', SpecCond_uScm_byMonth$nSpecCond_uScm_burned),
                                      SpecCond_uScm_control=paste0(SpecCond_uScm_byMonth$SpecCond_uScm_MedianControl, ', ', SpecCond_uScm_byMonth$nSpecCond_uScm_control),
                                      SpecCond_uScm_diff=paste0(SpecCond_uScm_byMonth$SpecCond_uScm_MedianDiffAbs, ', ', round(SpecCond_uScm_byMonth$SpecCond_uScm_MedianDiffPct,2), '%'))

SpecCond_uScm_byMonth$Month_factor <- factor(SpecCond_uScm_byMonth$Month_factor ,                                    # Change ordering manually
                                             levels = c("May", "Jun", "Jul", "Aug", "Sep","All months"))

# basic plot
ggplot(SpecCond_uScm_byMonth, aes(x=Month_factor, y=SpecCond_uScm_MedianDiffPct))+
  geom_bar(stat='identity', fill='firebrick')+
  theme_classic()+
  scale_y_continuous('Percent difference',limits=common_y_limits, breaks=common_y_breaks)+
  scale_x_discrete('Month')+
  theme(axis.title.y=element_text(color='black'),
        axis.title.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))+
  ggtitle('SpecCond_uScm')+
  geom_hline(yintercept = 0, linetype='dashed')

## ANC
ANC_mgCaCO3L <- waterquality[,c('ANC_mgCaCO3L','Month_factor','Type')]
ANC_mgCaCO3L_burned <- subset(ANC_mgCaCO3L, Type=='Burned')
ANC_mgCaCO3L_control <- subset(ANC_mgCaCO3L, Type=='Control')
ANC_mgCaCO3L_burned_median <- median(ANC_mgCaCO3L_burned$ANC_mgCaCO3L, na.rm=T)
ANC_mgCaCO3L_control_median <- median(ANC_mgCaCO3L_control$ANC_mgCaCO3L, na.rm=T)

ANC_mgCaCO3L_byMonth_burned <- ANC_mgCaCO3L_burned %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(ANC_mgCaCO3L_MedianBurned=median(ANC_mgCaCO3L, na.rm=T),
                   nANC_mgCaCO3L_burned=sum(!is.na(ANC_mgCaCO3L)))

ANC_mgCaCO3L_byMonth_control <- ANC_mgCaCO3L_control %>%
  dplyr::group_by(Month_factor) %>%
  dplyr::summarize(ANC_mgCaCO3L_MedianControl=median(ANC_mgCaCO3L, na.rm=T),
                   nANC_mgCaCO3L_control=sum(!is.na(ANC_mgCaCO3L)))

ANC_mgCaCO3L_byMonth <- merge(ANC_mgCaCO3L_byMonth_burned, ANC_mgCaCO3L_byMonth_control, by='Month_factor', all=T)
ANC_mgCaCO3L_byMonth <- ANC_mgCaCO3L_byMonth %>%
  mutate(Month_factor =  factor(Month_factor, levels = month_order)) %>%
  arrange(Month_factor)
ANC_mgCaCO3L_byMonth[6,] <- c('All months', ANC_mgCaCO3L_burned_median, NA, ANC_mgCaCO3L_control_median,NA)
ANC_mgCaCO3L_byMonth$Month_factor <- c('May','Jun','Jul','Aug','Sep','All months')
ANC_mgCaCO3L_byMonth[,c(2:5)] <- ANC_mgCaCO3L_byMonth[,c(2:5)] %>% mutate_if(is.character, as.numeric)
ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianDiffPct <- ((ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianBurned - ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianControl)/ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianControl)*100
ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianDiffAbs <-  ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianBurned - ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianControl
ANC_mgCaCO3L_byMonth[6,3] <- sum(ANC_mgCaCO3L_byMonth$nANC_mgCaCO3L_burned, na.rm=T)
ANC_mgCaCO3L_byMonth[6,5] <- sum(ANC_mgCaCO3L_byMonth$nANC_mgCaCO3L_control, na.rm=T)
ANC_mgCaCO3L_formatted <- data.frame(Month=c(month_order, 'All months'), 
                                     ANC_mgCaCO3L_burned=paste0(ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianBurned, ', ', ANC_mgCaCO3L_byMonth$nANC_mgCaCO3L_burned),
                                     ANC_mgCaCO3L_control=paste0(ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianControl, ', ', ANC_mgCaCO3L_byMonth$nANC_mgCaCO3L_control),
                                     ANC_mgCaCO3L_diff=paste0(ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianDiffAbs, ', ', round(ANC_mgCaCO3L_byMonth$ANC_mgCaCO3L_MedianDiffPct,2), '%'))

ANC_mgCaCO3L_byMonth$Month_factor <- factor(ANC_mgCaCO3L_byMonth$Month_factor ,                                    # Change ordering manually
                                            levels = c("May", "Jun", "Jul", "Aug", "Sep","All months"))

# basic plot
ggplot(ANC_mgCaCO3L_byMonth, aes(x=Month_factor, y=ANC_mgCaCO3L_MedianDiffPct))+
  geom_bar(stat='identity', fill='firebrick')+
  theme_classic()+
  scale_y_continuous('Percent difference',limits=common_y_limits, breaks=common_y_breaks)+
  scale_x_discrete('Month')+
  theme(axis.title.y=element_text(color='black'),
        axis.title.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.text.x=element_text(color='black'))+
  ggtitle('ANC_mgCaCO3L')+
  geom_hline(yintercept = 0, linetype='dashed')

## combine for export

#put all data frames into list
df_list <- list(TP_formatted, TN_formatted, DOC_formatted, TSS_formatted,
                Chloro_formatted, Secchi_formatted, pH_formatted, ANC_mgCaCO3L_formatted,
                SpecCond_uScm_formatted, NH4N_formatted, WaterTemp_C_formatted)

#merge all data frames in list
bigmamatable <- df_list %>% reduce(full_join, by='Month')
#write.csv(bigmamatable, file='Data/WaterQuality/burned_control_change_summary.csv', row.names=F)
