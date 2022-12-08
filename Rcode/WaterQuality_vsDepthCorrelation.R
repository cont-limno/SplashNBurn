############### How does water quality vary with depth? #######################
# Date: 12-8-22
# updated: #there is some similar analysis in ExploringWaterQuality script
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(Hmisc)
library(dplyr)
library(tidyverse)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")

# water quality
waterquality <- read.csv("Data/WaterQuality/combined_lab_field_may_sep.csv")

#### Main program ####
waterquality$logSecchi <- log(waterquality$SecchiDepth_m)
waterquality_ext <- waterquality[,c('logTP','logTN','logDOC','logSecchi',
                                    'logTSS','logChloro','zMax_m',
                                    'lagoslakeid','Site','Month','Type')] 

mayWQ <- subset(waterquality_ext, Month==5)
junWQ <- subset(waterquality_ext, Month==6)
julWQ <- subset(waterquality_ext, Month==7)
augWQ <- subset(waterquality_ext, Month==8)
sepWQ <- subset(waterquality_ext, Month==9)

## all months
allmonths_means <- waterquality_ext %>%
  dplyr::group_by(lagoslakeid) %>%
  dplyr::summarize(meanlogTP=mean(logTP, na.rm=T),
            meanlogTN=mean(logTN, na.rm=T),
            meanlogDOC=mean(logDOC, na.rm=T),
            meanlogChloro=mean(logChloro, na.rm=T),
            meanlogTSS=mean(logTSS, na.rm=T),
            meanlogSecchi=mean(logSecchi, na.rm=T))
allmonths_means <- merge(allmonths_means, junWQ[,c('lagoslakeid','zMax_m','Type')], by='lagoslakeid')
allmonths_means_burned <- subset(allmonths_means, Type=='Burned')
allmonths_means_control <- subset(allmonths_means, Type=='Control')

cormat_all <- rcorr(as.matrix(allmonths_means[,c(1:8)]), type='pearson')
cormat_all_r <- as.data.frame(cormat_all$r)
cormat_all_p <- as.data.frame(cormat_all$P)
cormat_all <- data.frame(var=rownames(cormat_all_r), zmax_r=cormat_all_r$zMax_m, zmax_p=cormat_all_p$zMax_m)
cormat_all <- cormat_all[2:7,]

cormat_all_burned <- rcorr(as.matrix(allmonths_means_burned[,c(1:8)]), type='pearson')
cormat_all_burned_r <- as.data.frame(cormat_all_burned$r)
cormat_all_burned_p <- as.data.frame(cormat_all_burned$P)
cormat_all_burned <- data.frame(var=rownames(cormat_all_burned_r), zmax_r_burned=cormat_all_burned_r$zMax_m, zmax_p_burned=cormat_all_burned_p$zMax_m)
cormat_all_burned <- cormat_all_burned[2:7,]

cormat_all_control <- rcorr(as.matrix(allmonths_means_control[,c(1:8)]), type='pearson')
cormat_all_control_r <- as.data.frame(cormat_all_control$r)
cormat_all_control_p <- as.data.frame(cormat_all_control$P)
cormat_all_control <- data.frame(var=rownames(cormat_all_control_r), zmax_r_control=cormat_all_control_r$zMax_m, zmax_p_control=cormat_all_control_p$zMax_m)
cormat_all_control <- cormat_all_control[2:7,]

temp_list <- list(cormat_all, cormat_all_burned, cormat_all_control)
cormat_all_summary <- temp_list %>% reduce(full_join, by='var')

# May
mayWQ_burned <- subset(mayWQ, Type=='Burned')
mayWQ_control <- subset(mayWQ, Type=='Control')
cormat_may <- rcorr(as.matrix(mayWQ[,c(1:7)]), type='pearson')
cormat_may_r <- as.data.frame(cormat_may$r)
cormat_may_p <- as.data.frame(cormat_may$P)
cormat_may <- data.frame(var=rownames(cormat_may_r), zmax_r=cormat_may_r$zMax_m, zmax_p=cormat_may_p$zMax_m)
cormat_may <- cormat_may[1:6,]

cormat_may_burned <- rcorr(as.matrix(mayWQ_burned[,c(1:8)]), type='pearson')
cormat_may_burned_r <- as.data.frame(cormat_may_burned$r)
cormat_may_burned_p <- as.data.frame(cormat_may_burned$P)
cormat_may_burned <- data.frame(var=rownames(cormat_may_burned_r), zmax_r_burned=cormat_may_burned_r$zMax_m, zmax_p_burned=cormat_may_burned_p$zMax_m)
cormat_may_burned <- cormat_may_burned[1:6,]

cormat_may_control <- rcorr(as.matrix(mayWQ_control[,c(1:8)]), type='pearson')
cormat_may_control_r <- as.data.frame(cormat_may_control$r)
cormat_may_control_p <- as.data.frame(cormat_may_control$P)
cormat_may_control <- data.frame(var=rownames(cormat_may_control_r), zmax_r_control=cormat_may_control_r$zMax_m, zmax_p_control=cormat_may_control_p$zMax_m)
cormat_may_control <- cormat_may_control[1:6,]

temp_list <- list(cormat_may, cormat_may_burned, cormat_may_control)
cormat_may_summary <- temp_list %>% reduce(full_join, by='var')

# June
junWQ_burned <- subset(junWQ, Type=='Burned')
junWQ_control <- subset(junWQ, Type=='Control')
cormat_jun <- rcorr(as.matrix(junWQ[,c(1:7)]), type='pearson')
cormat_jun_r <- as.data.frame(cormat_jun$r)
cormat_jun_p <- as.data.frame(cormat_jun$P)
cormat_jun <- data.frame(var=rownames(cormat_jun_r), zmax_r=cormat_jun_r$zMax_m, zmax_p=cormat_jun_p$zMax_m)
cormat_jun <- cormat_jun[1:6,]

cormat_jun_burned <- rcorr(as.matrix(junWQ_burned[,c(1:8)]), type='pearson')
cormat_jun_burned_r <- as.data.frame(cormat_jun_burned$r)
cormat_jun_burned_p <- as.data.frame(cormat_jun_burned$P)
cormat_jun_burned <- data.frame(var=rownames(cormat_jun_burned_r), zmax_r_burned=cormat_jun_burned_r$zMax_m, zmax_p_burned=cormat_jun_burned_p$zMax_m)
cormat_jun_burned <- cormat_jun_burned[1:6,]

cormat_jun_control <- rcorr(as.matrix(junWQ_control[,c(1:8)]), type='pearson')
cormat_jun_control_r <- as.data.frame(cormat_jun_control$r)
cormat_jun_control_p <- as.data.frame(cormat_jun_control$P)
cormat_jun_control <- data.frame(var=rownames(cormat_jun_control_r), zmax_r_control=cormat_jun_control_r$zMax_m, zmax_p_control=cormat_jun_control_p$zMax_m)
cormat_jun_control <- cormat_jun_control[1:6,]

temp_list <- list(cormat_jun, cormat_jun_burned, cormat_jun_control)
cormat_jun_summary <- temp_list %>% reduce(full_join, by='var')

# July
julWQ_burned <- subset(julWQ, Type=='Burned')
julWQ_control <- subset(julWQ, Type=='Control')
cormat_jul <- rcorr(as.matrix(julWQ[,c(1:7)]), type='pearson')
cormat_jul_r <- as.data.frame(cormat_jul$r)
cormat_jul_p <- as.data.frame(cormat_jul$P)
cormat_jul <- data.frame(var=rownames(cormat_jul_r), zmax_r=cormat_jul_r$zMax_m, zmax_p=cormat_jul_p$zMax_m)
cormat_jul <- cormat_jul[1:6,]

cormat_jul_burned <- rcorr(as.matrix(julWQ_burned[,c(1:8)]), type='pearson')
cormat_jul_burned_r <- as.data.frame(cormat_jul_burned$r)
cormat_jul_burned_p <- as.data.frame(cormat_jul_burned$P)
cormat_jul_burned <- data.frame(var=rownames(cormat_jul_burned_r), zmax_r_burned=cormat_jul_burned_r$zMax_m, zmax_p_burned=cormat_jul_burned_p$zMax_m)
cormat_jul_burned <- cormat_jul_burned[1:6,]

cormat_jul_control <- rcorr(as.matrix(julWQ_control[,c(1:8)]), type='pearson')
cormat_jul_control_r <- as.data.frame(cormat_jul_control$r)
cormat_jul_control_p <- as.data.frame(cormat_jul_control$P)
cormat_jul_control <- data.frame(var=rownames(cormat_jul_control_r), zmax_r_control=cormat_jul_control_r$zMax_m, zmax_p_control=cormat_jul_control_p$zMax_m)
cormat_jul_control <- cormat_jul_control[1:6,]

temp_list <- list(cormat_jul, cormat_jul_burned, cormat_jul_control)
cormat_jul_summary <- temp_list %>% reduce(full_join, by='var')

# Aug
augWQ_burned <- subset(augWQ, Type=='Burned')
augWQ_control <- subset(augWQ, Type=='Control')
cormat_aug <- rcorr(as.matrix(augWQ[,c(1:7)]), type='pearson')
cormat_aug_r <- as.data.frame(cormat_aug$r)
cormat_aug_p <- as.data.frame(cormat_aug$P)
cormat_aug <- data.frame(var=rownames(cormat_aug_r), zmax_r=cormat_aug_r$zMax_m, zmax_p=cormat_aug_p$zMax_m)
cormat_aug <- cormat_aug[1:6,]

cormat_aug_burned <- rcorr(as.matrix(augWQ_burned[,c(1:8)]), type='pearson')
cormat_aug_burned_r <- as.data.frame(cormat_aug_burned$r)
cormat_aug_burned_p <- as.data.frame(cormat_aug_burned$P)
cormat_aug_burned <- data.frame(var=rownames(cormat_aug_burned_r), zmax_r_burned=cormat_aug_burned_r$zMax_m, zmax_p_burned=cormat_aug_burned_p$zMax_m)
cormat_aug_burned <- cormat_aug_burned[1:6,]

cormat_aug_control <- rcorr(as.matrix(augWQ_control[,c(1:8)]), type='pearson')
cormat_aug_control_r <- as.data.frame(cormat_aug_control$r)
cormat_aug_control_p <- as.data.frame(cormat_aug_control$P)
cormat_aug_control <- data.frame(var=rownames(cormat_aug_control_r), zmax_r_control=cormat_aug_control_r$zMax_m, zmax_p_control=cormat_aug_control_p$zMax_m)
cormat_aug_control <- cormat_aug_control[1:6,]

temp_list <- list(cormat_aug, cormat_aug_burned, cormat_aug_control)
cormat_aug_summary <- temp_list %>% reduce(full_join, by='var')

# Sep
sepWQ_burned <- subset(sepWQ, Type=='Burned')
sepWQ_control <- subset(sepWQ, Type=='Control')
cormat_sep <- rcorr(as.matrix(sepWQ[,c(1:7)]), type='pearson')
cormat_sep_r <- as.data.frame(cormat_sep$r)
cormat_sep_p <- as.data.frame(cormat_sep$P)
cormat_sep <- data.frame(var=rownames(cormat_sep_r), zmax_r=cormat_sep_r$zMax_m, zmax_p=cormat_sep_p$zMax_m)
cormat_sep <- cormat_sep[1:6,]

cormat_sep_burned <- rcorr(as.matrix(sepWQ_burned[,c(1:8)]), type='pearson')
cormat_sep_burned_r <- as.data.frame(cormat_sep_burned$r)
cormat_sep_burned_p <- as.data.frame(cormat_sep_burned$P)
cormat_sep_burned <- data.frame(var=rownames(cormat_sep_burned_r), zmax_r_burned=cormat_sep_burned_r$zMax_m, zmax_p_burned=cormat_sep_burned_p$zMax_m)
cormat_sep_burned <- cormat_sep_burned[1:6,]

cormat_sep_control <- rcorr(as.matrix(sepWQ_control[,c(1:8)]), type='pearson')
cormat_sep_control_r <- as.data.frame(cormat_sep_control$r)
cormat_sep_control_p <- as.data.frame(cormat_sep_control$P)
cormat_sep_control <- data.frame(var=rownames(cormat_sep_control_r), zmax_r_control=cormat_sep_control_r$zMax_m, zmax_p_control=cormat_sep_control_p$zMax_m)
cormat_sep_control <- cormat_sep_control[1:6,]

temp_list <- list(cormat_sep, cormat_sep_burned, cormat_sep_control)
cormat_sep_summary <- temp_list %>% reduce(full_join, by='var')

## calculate average zmax for each lake
zmax_mean <- waterquality_ext %>%
  dplyr::group_by(Site) %>%
  dplyr::summarize(zmax_mean=mean(zMax_m, na.rm=T))
zmax_mean <- merge(zmax_mean, junWQ[,c('Type','lagoslakeid','Site')], by='Site')

boxplot(zmax_mean ~ Type, data=zmax_mean)

# burned vs control
zmax_mean_stats <- waterquality_ext %>%
  dplyr::group_by(Type) %>%
  dplyr::summarize(median=median(zMax_m, na.rm=T),
                   mean=mean(zMax_m, na.rm=T),
                   min=min(zMax_m, na.rm=T),
                   max=max(zMax_m, na.rm=T))
