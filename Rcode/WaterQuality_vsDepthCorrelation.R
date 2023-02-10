############### How does water quality vary with depth? #######################
# Date: 12-8-22
# updated: 12-29-22; analyze max depths by lake groups
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
waterquality$logWaterTemp <- log(waterquality$WaterTemp_C)
waterquality_ext <- waterquality[,c('logTP','logTN','logDOC','logSecchi',
                                    'logTSS','logChloro','pH', 'logWaterTemp',
                                    'zMax_m',
                                    'lagoslakeid','Site','Month','Type','Group')] 

mayWQ <- subset(waterquality_ext, Month==5)
junWQ <- subset(waterquality_ext, Month==6)
julWQ <- subset(waterquality_ext, Month==7)
augWQ <- subset(waterquality_ext, Month==8)
sepWQ <- subset(waterquality_ext, Month==9)

## data pooled across all months, but not averaged across for each lake
# i.e., to account for fact that max depth varies in a given lake seasonally
burned_drainage <- subset(waterquality_ext, Group=='Burned_Drainage')
burned_isolated <- subset(waterquality_ext, Group=='Burned_Isolated')
control_drainage <- subset(waterquality_ext, Group=='Control_Drainage')
control_isolated <- subset(waterquality_ext, Group=='Control_Isolated')
all_burned <- subset(waterquality_ext, Type=='Burned')
all_control <- subset(waterquality_ext, Type=='Control')

options(scipen=999)

# all lakes pooled (probably not very useful)
cormat_pooled <- rcorr(as.matrix(waterquality_ext[,c(1:10)]), type='pearson')
cormat_pooled_r <- as.data.frame(cormat_pooled$r)
cormat_pooled_p <- as.data.frame(cormat_pooled$P)
cormat_pooled <- data.frame(var=rownames(cormat_pooled_r), zmax_r=cormat_pooled_r$zMax_m, zmax_p=cormat_pooled_p$zMax_m)
cormat_pooled <- cormat_pooled[1:9,]

# burned only
cormat_allburned <- rcorr(as.matrix(all_burned[,c(1:10)]), type='pearson')
cormat_allburned_r <- as.data.frame(cormat_allburned$r)
cormat_allburned_p <- as.data.frame(cormat_allburned$P)
cormat_allburned <- data.frame(var=rownames(cormat_allburned_r), zmax_r_allburned=cormat_allburned_r$zMax_m, zmax_p_allburned=cormat_allburned_p$zMax_m)
cormat_allburned <- cormat_allburned[1:9,]

# control only
cormat_allcontrol <- rcorr(as.matrix(all_control[,c(1:10)]), type='pearson')
cormat_allcontrol_r <- as.data.frame(cormat_allcontrol$r)
cormat_allcontrol_p <- as.data.frame(cormat_allcontrol$P)
cormat_allcontrol <- data.frame(var=rownames(cormat_allcontrol_r), zmax_r_allcontrol=cormat_allcontrol_r$zMax_m, zmax_p_allcontrol=cormat_allcontrol_p$zMax_m)
cormat_allcontrol <- cormat_allcontrol[1:9,]

# individual lake groups
cormat_burned_drainage <- rcorr(as.matrix(burned_drainage[,c(1:10)]), type='pearson')
cormat_burned_drainage_r <- as.data.frame(cormat_burned_drainage$r)
cormat_burned_drainage_p <- as.data.frame(cormat_burned_drainage$P)
cormat_burned_drainage <- data.frame(var=rownames(cormat_burned_drainage_r), zmax_r_BD=cormat_burned_drainage_r$zMax_m, zmax_p_BD=cormat_burned_drainage_p$zMax_m)
cormat_burned_drainage <- cormat_burned_drainage[1:9,]

cormat_burned_isolated <- rcorr(as.matrix(burned_isolated[,c(1:10)]), type='pearson')
cormat_burned_isolated_r <- as.data.frame(cormat_burned_isolated$r)
cormat_burned_isolated_p <- as.data.frame(cormat_burned_isolated$P)
cormat_burned_isolated <- data.frame(var=rownames(cormat_burned_isolated_r), zmax_r_BI=cormat_burned_isolated_r$zMax_m, zmax_p_BI=cormat_burned_isolated_p$zMax_m)
cormat_burned_isolated <- cormat_burned_isolated[1:9,]

cormat_control_drainage <- rcorr(as.matrix(control_drainage[,c(1:10)]), type='pearson')
cormat_control_drainage_r <- as.data.frame(cormat_control_drainage$r)
cormat_control_drainage_p <- as.data.frame(cormat_control_drainage$P)
cormat_control_drainage <- data.frame(var=rownames(cormat_control_drainage_r), zmax_r_CD=cormat_control_drainage_r$zMax_m, zmax_p_CD=cormat_control_drainage_p$zMax_m)
cormat_control_drainage <- cormat_control_drainage[1:9,]

cormat_control_isolated <- rcorr(as.matrix(control_isolated[,c(1:10)]), type='pearson')
cormat_control_isolated_r <- as.data.frame(cormat_control_isolated$r)
cormat_control_isolated_p <- as.data.frame(cormat_control_isolated$P)
cormat_control_isolated <- data.frame(var=rownames(cormat_control_isolated_r), zmax_r_CI=cormat_control_isolated_r$zMax_m, zmax_p_CI=cormat_control_isolated_p$zMax_m)
cormat_control_isolated <- cormat_control_isolated[1:9,]

temp_list <- list(cormat_burned_drainage, cormat_control_drainage, 
                  cormat_burned_isolated, cormat_control_isolated,
                  cormat_allburned, cormat_allcontrol)
cormat_pooled_summary <- temp_list %>% reduce(full_join, by='var')
#write.csv(cormat_pooled_summary[c(1:8),], file='Data/Correlation_matrices/waterquality_vs_depth_summary.csv', row.names=F)

# ## means across all months
# allmonths_means <- waterquality_ext %>%
#   dplyr::group_by(lagoslakeid) %>%
#   dplyr::summarize(meanlogTP=mean(logTP, na.rm=T),
#             meanlogTN=mean(logTN, na.rm=T),
#             meanlogDOC=mean(logDOC, na.rm=T),
#             meanlogChloro=mean(logChloro, na.rm=T),
#             meanlogTSS=mean(logTSS, na.rm=T),
#             meanlogSecchi=mean(logSecchi, na.rm=T),
#             meanpH=mean(pH, na.rm=T),
#             meanlogWaterTemp=mean(logWaterTemp, na.rm=T))
# allmonths_means <- merge(allmonths_means, junWQ[,c('lagoslakeid','zMax_m','Type','Group')], by='lagoslakeid')
# allmonths_means_burned <- subset(allmonths_means, Type=='Burned')
# allmonths_means_control <- subset(allmonths_means, Type=='Control')
# 
# cormat_all <- rcorr(as.matrix(allmonths_means[,c(1:10)]), type='pearson')
# cormat_all_r <- as.data.frame(cormat_all$r)
# cormat_all_p <- as.data.frame(cormat_all$P)
# cormat_all <- data.frame(var=rownames(cormat_all_r), zmax_r=cormat_all_r$zMax_m, zmax_p=cormat_all_p$zMax_m)
# cormat_all <- cormat_all[2:9,]
# 
# cormat_all_burned <- rcorr(as.matrix(allmonths_means_burned[,c(1:10)]), type='pearson')
# cormat_all_burned_r <- as.data.frame(cormat_all_burned$r)
# cormat_all_burned_p <- as.data.frame(cormat_all_burned$P)
# cormat_all_burned <- data.frame(var=rownames(cormat_all_burned_r), zmax_r_burned=cormat_all_burned_r$zMax_m, zmax_p_burned=cormat_all_burned_p$zMax_m)
# cormat_all_burned <- cormat_all_burned[2:9,]
# 
# cormat_all_control <- rcorr(as.matrix(allmonths_means_control[,c(1:10)]), type='pearson')
# cormat_all_control_r <- as.data.frame(cormat_all_control$r)
# cormat_all_control_p <- as.data.frame(cormat_all_control$P)
# cormat_all_control <- data.frame(var=rownames(cormat_all_control_r), zmax_r_control=cormat_all_control_r$zMax_m, zmax_p_control=cormat_all_control_p$zMax_m)
# cormat_all_control <- cormat_all_control[2:9,]
# 
# temp_list <- list(cormat_all, cormat_all_burned, cormat_all_control)
# cormat_all_summary <- temp_list %>% reduce(full_join, by='var')
# 
# # May
# mayWQ_burned <- subset(mayWQ, Type=='Burned')
# mayWQ_control <- subset(mayWQ, Type=='Control')
# cormat_may <- rcorr(as.matrix(mayWQ[,c(1:9)]), type='pearson')
# cormat_may_r <- as.data.frame(cormat_may$r)
# cormat_may_p <- as.data.frame(cormat_may$P)
# cormat_may <- data.frame(var=rownames(cormat_may_r), zmax_r=cormat_may_r$zMax_m, zmax_p=cormat_may_p$zMax_m)
# cormat_may <- cormat_may[1:8,]
# 
# cormat_may_burned <- rcorr(as.matrix(mayWQ_burned[,c(1:9)]), type='pearson')
# cormat_may_burned_r <- as.data.frame(cormat_may_burned$r)
# cormat_may_burned_p <- as.data.frame(cormat_may_burned$P)
# cormat_may_burned <- data.frame(var=rownames(cormat_may_burned_r), zmax_r_burned=cormat_may_burned_r$zMax_m, zmax_p_burned=cormat_may_burned_p$zMax_m)
# cormat_may_burned <- cormat_may_burned[1:8,]
# 
# cormat_may_control <- rcorr(as.matrix(mayWQ_control[,c(1:9)]), type='pearson')
# cormat_may_control_r <- as.data.frame(cormat_may_control$r)
# cormat_may_control_p <- as.data.frame(cormat_may_control$P)
# cormat_may_control <- data.frame(var=rownames(cormat_may_control_r), zmax_r_control=cormat_may_control_r$zMax_m, zmax_p_control=cormat_may_control_p$zMax_m)
# cormat_may_control <- cormat_may_control[1:8,]
# 
# temp_list <- list(cormat_may, cormat_may_burned, cormat_may_control)
# cormat_may_summary <- temp_list %>% reduce(full_join, by='var')
# 
# # June
# junWQ_burned <- subset(junWQ, Type=='Burned')
# junWQ_control <- subset(junWQ, Type=='Control')
# cormat_jun <- rcorr(as.matrix(junWQ[,c(1:9)]), type='pearson')
# cormat_jun_r <- as.data.frame(cormat_jun$r)
# cormat_jun_p <- as.data.frame(cormat_jun$P)
# cormat_jun <- data.frame(var=rownames(cormat_jun_r), zmax_r=cormat_jun_r$zMax_m, zmax_p=cormat_jun_p$zMax_m)
# cormat_jun <- cormat_jun[1:8,]
# 
# cormat_jun_burned <- rcorr(as.matrix(junWQ_burned[,c(1:9)]), type='pearson')
# cormat_jun_burned_r <- as.data.frame(cormat_jun_burned$r)
# cormat_jun_burned_p <- as.data.frame(cormat_jun_burned$P)
# cormat_jun_burned <- data.frame(var=rownames(cormat_jun_burned_r), zmax_r_burned=cormat_jun_burned_r$zMax_m, zmax_p_burned=cormat_jun_burned_p$zMax_m)
# cormat_jun_burned <- cormat_jun_burned[1:8,]
# 
# cormat_jun_control <- rcorr(as.matrix(junWQ_control[,c(1:9)]), type='pearson')
# cormat_jun_control_r <- as.data.frame(cormat_jun_control$r)
# cormat_jun_control_p <- as.data.frame(cormat_jun_control$P)
# cormat_jun_control <- data.frame(var=rownames(cormat_jun_control_r), zmax_r_control=cormat_jun_control_r$zMax_m, zmax_p_control=cormat_jun_control_p$zMax_m)
# cormat_jun_control <- cormat_jun_control[1:8,]
# 
# temp_list <- list(cormat_jun, cormat_jun_burned, cormat_jun_control)
# cormat_jun_summary <- temp_list %>% reduce(full_join, by='var')
# 
# # July
# julWQ_burned <- subset(julWQ, Type=='Burned')
# julWQ_control <- subset(julWQ, Type=='Control')
# cormat_jul <- rcorr(as.matrix(julWQ[,c(1:9)]), type='pearson')
# cormat_jul_r <- as.data.frame(cormat_jul$r)
# cormat_jul_p <- as.data.frame(cormat_jul$P)
# cormat_jul <- data.frame(var=rownames(cormat_jul_r), zmax_r=cormat_jul_r$zMax_m, zmax_p=cormat_jul_p$zMax_m)
# cormat_jul <- cormat_jul[1:8,]
# 
# cormat_jul_burned <- rcorr(as.matrix(julWQ_burned[,c(1:9)]), type='pearson')
# cormat_jul_burned_r <- as.data.frame(cormat_jul_burned$r)
# cormat_jul_burned_p <- as.data.frame(cormat_jul_burned$P)
# cormat_jul_burned <- data.frame(var=rownames(cormat_jul_burned_r), zmax_r_burned=cormat_jul_burned_r$zMax_m, zmax_p_burned=cormat_jul_burned_p$zMax_m)
# cormat_jul_burned <- cormat_jul_burned[1:8,]
# 
# cormat_jul_control <- rcorr(as.matrix(julWQ_control[,c(1:9)]), type='pearson')
# cormat_jul_control_r <- as.data.frame(cormat_jul_control$r)
# cormat_jul_control_p <- as.data.frame(cormat_jul_control$P)
# cormat_jul_control <- data.frame(var=rownames(cormat_jul_control_r), zmax_r_control=cormat_jul_control_r$zMax_m, zmax_p_control=cormat_jul_control_p$zMax_m)
# cormat_jul_control <- cormat_jul_control[1:8,]
# 
# temp_list <- list(cormat_jul, cormat_jul_burned, cormat_jul_control)
# cormat_jul_summary <- temp_list %>% reduce(full_join, by='var')
# 
# # Aug
# augWQ_burned <- subset(augWQ, Type=='Burned')
# augWQ_control <- subset(augWQ, Type=='Control')
# cormat_aug <- rcorr(as.matrix(augWQ[,c(1:9)]), type='pearson')
# cormat_aug_r <- as.data.frame(cormat_aug$r)
# cormat_aug_p <- as.data.frame(cormat_aug$P)
# cormat_aug <- data.frame(var=rownames(cormat_aug_r), zmax_r=cormat_aug_r$zMax_m, zmax_p=cormat_aug_p$zMax_m)
# cormat_aug <- cormat_aug[1:8,]
# 
# cormat_aug_burned <- rcorr(as.matrix(augWQ_burned[,c(1:9)]), type='pearson')
# cormat_aug_burned_r <- as.data.frame(cormat_aug_burned$r)
# cormat_aug_burned_p <- as.data.frame(cormat_aug_burned$P)
# cormat_aug_burned <- data.frame(var=rownames(cormat_aug_burned_r), zmax_r_burned=cormat_aug_burned_r$zMax_m, zmax_p_burned=cormat_aug_burned_p$zMax_m)
# cormat_aug_burned <- cormat_aug_burned[1:8,]
# 
# cormat_aug_control <- rcorr(as.matrix(augWQ_control[,c(1:9)]), type='pearson')
# cormat_aug_control_r <- as.data.frame(cormat_aug_control$r)
# cormat_aug_control_p <- as.data.frame(cormat_aug_control$P)
# cormat_aug_control <- data.frame(var=rownames(cormat_aug_control_r), zmax_r_control=cormat_aug_control_r$zMax_m, zmax_p_control=cormat_aug_control_p$zMax_m)
# cormat_aug_control <- cormat_aug_control[1:8,]
# 
# temp_list <- list(cormat_aug, cormat_aug_burned, cormat_aug_control)
# cormat_aug_summary <- temp_list %>% reduce(full_join, by='var')
# 
# # Sep
# sepWQ_burned <- subset(sepWQ, Type=='Burned')
# sepWQ_control <- subset(sepWQ, Type=='Control')
# cormat_sep <- rcorr(as.matrix(sepWQ[,c(1:9)]), type='pearson')
# cormat_sep_r <- as.data.frame(cormat_sep$r)
# cormat_sep_p <- as.data.frame(cormat_sep$P)
# cormat_sep <- data.frame(var=rownames(cormat_sep_r), zmax_r=cormat_sep_r$zMax_m, zmax_p=cormat_sep_p$zMax_m)
# cormat_sep <- cormat_sep[1:8,]
# 
# cormat_sep_burned <- rcorr(as.matrix(sepWQ_burned[,c(1:9)]), type='pearson')
# cormat_sep_burned_r <- as.data.frame(cormat_sep_burned$r)
# cormat_sep_burned_p <- as.data.frame(cormat_sep_burned$P)
# cormat_sep_burned <- data.frame(var=rownames(cormat_sep_burned_r), zmax_r_burned=cormat_sep_burned_r$zMax_m, zmax_p_burned=cormat_sep_burned_p$zMax_m)
# cormat_sep_burned <- cormat_sep_burned[1:8,]
# 
# cormat_sep_control <- rcorr(as.matrix(sepWQ_control[,c(1:9)]), type='pearson')
# cormat_sep_control_r <- as.data.frame(cormat_sep_control$r)
# cormat_sep_control_p <- as.data.frame(cormat_sep_control$P)
# cormat_sep_control <- data.frame(var=rownames(cormat_sep_control_r), zmax_r_control=cormat_sep_control_r$zMax_m, zmax_p_control=cormat_sep_control_p$zMax_m)
# cormat_sep_control <- cormat_sep_control[1:8,]
# 
# temp_list <- list(cormat_sep, cormat_sep_burned, cormat_sep_control)
# cormat_sep_summary <- temp_list %>% reduce(full_join, by='var')

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

#### Drainage vs isolated depth differences ####
# first analyze with all individual max depth measurements
extrafun <- waterquality[,c('lagoslakeid','Site','Type','ConnClass','Group','zMax_m')]
extrafun$Group <- factor(extrafun$Group, levels=c('Burned_Drainage','Control_Drainage','Burned_Isolated','Control_Isolated'))

boxplot(extrafun$zMax_m ~ extrafun$Group, names=c('BD','CD','BI','CI'),
        xlab='', ylab='Maximum depth (m)')

allmax_aov <- aov(zMax_m ~ Group, data=extrafun)
summary(allmax_aov)
TukeyHSD(allmax_aov)
plot(TukeyHSD(allmax_aov))

# number of depths by group
nDepth <- extrafun %>%
  dplyr::group_by(Group) %>%
  dplyr::summarize(nDepth=n())

allmax_stats <- extrafun %>%
  dplyr::group_by(Group) %>%
  dplyr::summarize(mean=mean(zMax_m, na.rm=T),
                   median=median(zMax_m, na.rm=T),
                   min=min(zMax_m, na.rm=T),
                   max=max(zMax_m, na.rm=T))

# reanalyze with each lake's max depth as mean of max depths for that lake
extrafun_mean <- extrafun %>%
  dplyr::group_by(Site) %>%
  dplyr::summarize(meanmax_m =mean(zMax_m, na.rm=T))

extrafun_mean <- merge(extrafun_mean, waterquality[,c('Site','Group')], by='Site', all=F)
extrafun_mean <- extrafun_mean %>% distinct()

extrafun_mean$Group <- factor(extrafun_mean$Group, levels=c('Burned_Drainage','Control_Drainage','Burned_Isolated','Control_Isolated'))
boxplot(extrafun_mean$meanmax_m ~ extrafun_mean$Group, 
        xlab='', ylab='Mean maximum depth (m)', names=c('BD','CD','BI','CI'))

meanmax_aov <- aov(meanmax_m ~ Group, data=extrafun_mean)
summary(meanmax_aov)

TukeyHSD(meanmax_aov)
