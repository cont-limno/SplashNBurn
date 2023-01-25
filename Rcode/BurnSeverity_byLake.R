################### Exploring lake times series data ##########################
# Date: 8-26-22
# updated: 1-24-23; change plot title
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")

may_june_july <- read.csv("Data/WaterQuality/may_june_july.csv")
ws_burn_severity <- read.csv("Data/BurnSeverity/Ian_calculations/burned_ws_vbs_pct.csv")
ws_soilburn_severity <- read.csv("Data/BurnSeverity/Ian_calculations/burned_ws_sbs_pct.csv")
buffer_burn_severity <- read.csv("Data/BurnSeverity/Ian_calculations/burned_buff100m_vbs_pct.csv")
buffer_soilburn_severity <- read.csv("Data/BurnSeverity/Ian_calculations/burned_buff100m_sbs_pct.csv")

#### Main program ####
# how correlated are % ws burn variables? Seem to be highly so
ws_burn_severity_pct <- ws_burn_severity[,c(1, 12:17)]
ws_burn_severity_pct$total_ws_burn_pct <- rowSums(ws_burn_severity_pct[,c(2:7)])
cor(ws_burn_severity_pct, method='pearson', use='pairwise.complete.obs')
cor(ws_burn_severity_pct, method='spearman', use='pairwise.complete.obs')

# calculate unburned pct
#ws_burn_severity_pct$unburned_pct <- 100-ws_burn_severity_pct$total_ws_burn_pct
# if any slight rounding errors result in negative burned area
#ws_burn_severity_pct$unburned_pct <- ifelse(ws_burn_severity_pct$unburned_pct < 0, 0, ws_burn_severity_pct$unburned_pct)

## prepare for plot
# get names + lagoslakeids only
lakenamesids <- may_june_july[,c('Lagoslakeid','Site','Type')]
lakenamesids <- subset(lakenamesids, Type=='sample')
lakenamesids <- lakenamesids[!duplicated(lakenamesids), ]
lakenamesids <- lakenamesids[,c(1:2)]

ws_burn_severity_pct <- merge(ws_burn_severity_pct, lakenamesids, by.x='lagoslakeid', by.y='Lagoslakeid')
# ERL category is so small...just lump in with unburned
ws_burn_severity_pct$Unburned_pct <- ws_burn_severity_pct$Unburned_pct + ws_burn_severity_pct$ERL_pct
ws_burn_severity_pct_melted <- reshape2::melt(ws_burn_severity_pct[,c(3:7,9)], 
      id.var='Site', variable.name='Severity', value.name='Percent')

ws_burn_severity_pct_melted$Severity <- factor(ws_burn_severity_pct_melted$Severity, levels=c('Unburned_pct','Low_pct',
                                                          'ModerateLow_pct','ModerateHigh_pct','High_pct'))

# get rid of word 'Lake' to make labels smaller
ws_burn_severity_pct_melted$Lake <- gsub(paste('Lake',collapse='|'),"",ws_burn_severity_pct_melted$Site)
ws_burn_severity_pct_melted$LakeFac <- as.factor(ws_burn_severity_pct_melted$Lake)
ws_burn_severity_pct_melted$LakeFac <- factor(ws_burn_severity_pct_melted$Lake, levels=c("Fourth McDougal","Middle McDougal","Wampus ","North McDougal",
                                                             "Stony ","Fishtrap ","South McDougal","Sand ","Slate "," Gegoka",
                                                             "Greenwood ","West Chub ","Teamster ","Lil Chub ","Unnamed "))

jpeg('Figures/BurnSeverity_byLake.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(ws_burn_severity_pct_melted, aes(fill=Severity, y=Percent, x=LakeFac)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=70, vjust=1, hjust=1, color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x.bottom=element_blank())+
  scale_fill_manual(values=c('gray90','gold','orange','firebrick','black'), 
                    labels=c('Unburned','Low','Low-Moderate','Moderate-High','High'),
                    'Severity (%)')+
  ggtitle('B) Watershed vegetation burn severity')
dev.off()

### watershed soil burn severity ###
# how correlated are % ws soilburn variables? Seem to be highly so
ws_soilburn_severity_pct <- ws_soilburn_severity[,c(1,11:15)]
ws_soilburn_severity_pct$total_ws_soilburn_pct <- rowSums(ws_soilburn_severity_pct[,c(2:6)])
cor(ws_soilburn_severity_pct, method='pearson', use='pairwise.complete.obs')
cor(ws_soilburn_severity_pct, method='spearman', use='pairwise.complete.obs')

# calculate unburned pct; no longer needed
#ws_soilburn_severity_pct$unburned_pct <- 100-ws_soilburn_severity_pct$total_ws_soilburn_pct
# if any slight rounding errors result in negative soilburned area
#ws_soilburn_severity_pct$unburned_pct <- ifelse(ws_soilburn_severity_pct$unburned_pct < 0, 0, ws_soilburn_severity_pct$unburned_pct)

## prepare for plot
ws_soilburn_severity_pct <- merge(ws_soilburn_severity_pct, lakenamesids, by.x='lagoslakeid', by.y='Lagoslakeid')
ws_soilburn_severity_pct_melted <- reshape2::melt(ws_soilburn_severity_pct[,c(2:6,8)], 
                                                  id.var='Site', variable.name='Severity', value.name='Percent')

ws_soilburn_severity_pct_melted$Severity <- factor(ws_soilburn_severity_pct_melted$Severity, levels=c('unburned_pct','unburned_low_pct','low_pct',
                                                                                                      'moderate_pct','high_pct'))

# get rid of word 'Lake' to make labels smaller
ws_soilburn_severity_pct_melted$Lake <- gsub(paste('Lake',collapse='|'),"",ws_soilburn_severity_pct_melted$Site)
ws_soilburn_severity_pct_melted$LakeFac <- as.factor(ws_soilburn_severity_pct_melted$Lake)
ws_soilburn_severity_pct_melted$LakeFac <- factor(ws_soilburn_severity_pct_melted$Lake, levels=c("Middle McDougal","Wampus ","Stony ", "Fourth McDougal", "North McDougal",
                                                                                                 "South McDougal","Fishtrap ", "Sand ","Lil Chub ", "Slate ",
                                                                                                 " Gegoka","Greenwood ","West Chub ", "Teamster ","Unnamed "))
jpeg('Figures/soilburnSeverity_byLake.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(ws_soilburn_severity_pct_melted, aes(fill=Severity, y=Percent, x=LakeFac)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=70, vjust=1, hjust=1, color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x.bottom=element_blank())+
  scale_fill_manual(values=c('gray90','gold','orange','firebrick','black'), 
                    labels=c('Unburned','Unburned-Low','Low','Moderate','High'),
                    'Severity (%)')+
  ggtitle('Watershed soil burn severity')
dev.off()

### shoreline buffers ###
## vegetation burn severity
# how correlated are % buffer burn variables? Seem to be highly so
buffer_burn_severity_pct <- buffer_burn_severity[,c(1,8:12)]
buffer_burn_severity_pct$total_buffer_burn_pct <- rowSums(buffer_burn_severity_pct[,c(2:6)], na.rm=T)
cor(buffer_burn_severity_pct, method='pearson', use='pairwise.complete.obs')
cor(buffer_burn_severity_pct, method='spearman', use='pairwise.complete.obs')

# calculate unburned pct
#buffer_burn_severity_pct$unburned_pct <- 100-buffer_burn_severity_pct$total_buffer_burn_pct
# if any slight rounding errors result in negative burned area
#buffer_burn_severity_pct$unburned_pct <- ifelse(buffer_burn_severity_pct$unburned_pct < 0, 0, buffer_burn_severity_pct$unburned_pct)

## prepare for plot
buffer_burn_severity_pct <- merge(buffer_burn_severity_pct, lakenamesids, by.x='lagoslakeid', by.y='Lagoslakeid')
buffer_burn_severity_pct <- tidyr::replace_na(buffer_burn_severity_pct, list(Unburned_pct=100)) #replace Unburned NAs as 100 (i.e., everything not captured assumed to be unburned)
buffer_burn_severity_pct_melted <- reshape2::melt(buffer_burn_severity_pct[,c(2:6,8)], 
                                                  id.var='Site', variable.name='Severity', value.name='Percent')

buffer_burn_severity_pct_melted$Severity <- factor(buffer_burn_severity_pct_melted$Severity, levels=c('Unburned_pct','Low_pct',
                                                                                                      'ModerateLow_pct','ModerateHigh_pct','High_pct'))

# get rid of word 'Lake' to make labels smaller
buffer_burn_severity_pct_melted$Lake <- gsub(paste('Lake',collapse='|'),"",buffer_burn_severity_pct_melted$Site)
buffer_burn_severity_pct_melted$LakeFac <- as.factor(buffer_burn_severity_pct_melted$Lake)
buffer_burn_severity_pct_melted$LakeFac <- factor(buffer_burn_severity_pct_melted$Lake, levels=c("Fourth McDougal","Stony ", "South McDougal", "Middle McDougal", "Fishtrap ",
                                                                                                 "Wampus ", "North McDougal", "Unnamed ","Greenwood ", " Gegoka",
                                                                                                 "Lil Chub ","Sand ","Slate ", "Teamster ", "West Chub "))

jpeg('Figures/BurnSeverity_buffer_byLake.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(buffer_burn_severity_pct_melted, aes(fill=Severity, y=Percent, x=LakeFac)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=70, vjust=1, hjust=1, color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x.bottom = element_blank())+
  scale_fill_manual(values=c('gray90','gold','orange','firebrick','black'), 
                    labels=c('Unburned','Low','Low-Moderate','Moderate-High','High'),
                    'Severity (%)')+
  ggtitle('100m lake buffer vegetation burn severity')
dev.off()

## soil burn severity 100m buffer
# how correlated are % buffer soilburn variables? Seem to be highly so
buffer_soilburn_severity_pct <- buffer_soilburn_severity[,c(1,8:12)]
buffer_soilburn_severity_pct$total_buffer_soilburn_pct <- rowSums(buffer_soilburn_severity_pct[,c(2:6)])
cor(buffer_soilburn_severity_pct, method='pearson', use='pairwise.complete.obs')
cor(buffer_soilburn_severity_pct, method='spearman', use='pairwise.complete.obs')

# calculate unburned pct; no longer needed
#buffer_soilburn_severity_pct$unburned_pct <- 100-buffer_soilburn_severity_pct$total_buffer_soilburn_pct
# if any slight rounding errors result in negative soilburned area
#buffer_soilburn_severity_pct$unburned_pct <- ifelse(buffer_soilburn_severity_pct$unburned_pct < 0, 0, buffer_soilburn_severity_pct$unburned_pct)

## prepare for plot
buffer_soilburn_severity_pct <- merge(buffer_soilburn_severity_pct, lakenamesids, by.x='lagoslakeid', by.y='Lagoslakeid')
buffer_soilburn_severity_pct_melted <- reshape2::melt(buffer_soilburn_severity_pct[,c(2:6,8)], 
                                                      id.var='Site', variable.name='Severity', value.name='Percent')

buffer_soilburn_severity_pct_melted$Severity <- factor(buffer_soilburn_severity_pct_melted$Severity, levels=c('unburned_pct','unburned_low_pct','low_pct',
                                                                                                              'moderate_pct','high_pct'))

# get rid of word 'Lake' to make labels smaller
buffer_soilburn_severity_pct_melted$Lake <- gsub(paste('Lake',collapse='|'),"",buffer_soilburn_severity_pct_melted$Site)
buffer_soilburn_severity_pct_melted$LakeFac <- as.factor(buffer_soilburn_severity_pct_melted$Lake)
buffer_soilburn_severity_pct_melted$LakeFac <- factor(buffer_soilburn_severity_pct_melted$Lake, levels=c("Fishtrap ","South McDougal", "Stony ", "Fourth McDougal", "Wampus ",
                                                                                                         "Middle McDougal","North McDougal", "Unnamed ","Greenwood ", " Gegoka",
                                                                                                         "Lil Chub ","Sand ","Slate ", "Teamster ", "West Chub "))

jpeg('Figures/soilburnSeverity_buffer_byLake.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(buffer_soilburn_severity_pct_melted, aes(fill=Severity, y=Percent, x=LakeFac)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=70, vjust=1, hjust=1, color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x.bottom = element_blank())+
  scale_fill_manual(values=c('gray90','gold','orange','firebrick','black'), 
                    labels=c('Unburned','Unburned-Low','Low','Moderate','High'),
                    'Severity (%)')+
  ggtitle('100m lake buffer soil burn severity')
dev.off()


### how correlated are vegetation and soil burn severity? ###
# for now, analyzing watershed only (more data points)
# veg_soil_severity <- merge(ws_burn_severity_pct, ws_soilburn_severity_pct, by='lagoslakeid')[,c(1:13)]
# names(veg_soil_severity) <- c('lagoslakeid','veg_low_severity_pct','veg_moderate_low_severity_pct','veg_moderate_high_severity_pct',
#                              'veg_high_severity_pct','veg_total_ws_burn_pct','veg_unburned_pct','Site',
#                              'soil_low_severity_pct','soil_moderate_severity_pct','soil_high_severity_pct','soil_total_ws_burn_pct','soil_unburned_pct')
# cor(veg_soil_severity[,c(2:7,9:13)], method='pearson', use='pairwise.complete.obs')
