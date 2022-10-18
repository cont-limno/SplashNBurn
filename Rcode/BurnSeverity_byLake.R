################### Exploring lake times series data ##########################
# Date: 8-26-22
# updated: 10-18-22, now with soil burn severity data corrected
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(ggplot2)
library(reshape2)
library(dplyr)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")

may_june_july <- read.csv("Data/WaterQuality/may_june_july.csv")
ws_burn_severity <- read.csv("Data/BurnSeverity/vegetation_ws_area.csv")
#ws_soilburn_severity <- read.csv("Data/BurnSeverity/soil_ws_area.csv")
ws_soilburn_severity <- read.csv("Data/BurnSeverity/Ian_calculations/burned_ws_sbs_pct.csv")
buffer_burn_severity <- read.csv("Data/BurnSeverity/vegetation_buffer_area.csv")
#buffer_soilburn_severity <- read.csv("Data/BurnSeverity/soil_buffer_area.csv")
buffer_soilburn_severity <- read.csv("Data/BurnSeverity/Ian_calculations/burned_buff100m_sbs_pct.csv")

#### Main program ####
# how correlated are % ws burn variables? Seem to be highly so
ws_burn_severity_pct <- ws_burn_severity[,c(1,3,5,7,9)]
ws_burn_severity_pct$total_ws_burn_pct <- rowSums(ws_burn_severity_pct[,c(2:5)])
cor(ws_burn_severity_pct, method='pearson', use='pairwise.complete.obs')
cor(ws_burn_severity_pct, method='spearman', use='pairwise.complete.obs')

# calculate unburned pct
ws_burn_severity_pct$unburned_pct <- 100-ws_burn_severity_pct$total_ws_burn_pct
# if any slight rounding errors result in negative burned area
ws_burn_severity_pct$unburned_pct <- ifelse(ws_burn_severity_pct$unburned_pct < 0, 0, ws_burn_severity_pct$unburned_pct)

## prepare for plot
# get names + lagoslakeids only
lakenamesids <- may_june_july[,c('Lagoslakeid','Site','Type')]
lakenamesids <- subset(lakenamesids, Type=='sample')
lakenamesids <- lakenamesids[!duplicated(lakenamesids), ]
lakenamesids <- lakenamesids[,c(1:2)]

ws_burn_severity_pct <- merge(ws_burn_severity_pct, lakenamesids, by.x='lagoslakeid', by.y='Lagoslakeid')
ws_burn_severity_pct_melted <- reshape2::melt(ws_burn_severity_pct[,c(2:5,7,8)], 
      id.var='Site', variable.name='Severity', value.name='Percent')

ws_burn_severity_pct_melted$Severity <- factor(ws_burn_severity_pct_melted$Severity, levels=c('unburned_pct','low_severity_pct',
                                                          'moderate_low_severity_pct','moderate_high_severity_pct','high_severity_pct'))

# get rid of word 'Lake' to make labels smaller
ws_burn_severity_pct_melted$Lake <- gsub(paste('Lake',collapse='|'),"",ws_burn_severity_pct_melted$Site)

jpeg('Figures/BurnSeverity_byLake.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(ws_burn_severity_pct_melted, aes(fill=Severity, y=Percent, x=Lake)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, color='black'),
        axis.text.y=element_text(color='black'))+
  scale_fill_manual(values=c('gray90','gold','orange','firebrick','black'), 
                    labels=c('Unburned','Low','Low-Moderate','Moderate-High','High'),
                    'Severity (%)')+
  ggtitle('Watershed vegetation burn severity')
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

jpeg('Figures/soilburnSeverity_byLake.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(ws_soilburn_severity_pct_melted, aes(fill=Severity, y=Percent, x=Lake)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, color='black'),
        axis.text.y=element_text(color='black'))+
  scale_fill_manual(values=c('gray90','gold','orange','firebrick','black'), 
                    labels=c('Unburned','Unburned-Low','Low','Moderate','High'),
                    'Severity (%)')+
  ggtitle('Watershed soil burn severity')
dev.off()

### shoreline buffers ###
## vegetation burn severity
# how correlated are % buffer burn variables? Seem to be highly so
buffer_burn_severity_pct <- buffer_burn_severity[,c(1,3,5,7,9)]
buffer_burn_severity_pct$total_buffer_burn_pct <- rowSums(buffer_burn_severity_pct[,c(2:5)])
cor(buffer_burn_severity_pct, method='pearson', use='pairwise.complete.obs')
cor(buffer_burn_severity_pct, method='spearman', use='pairwise.complete.obs')

# calculate unburned pct
buffer_burn_severity_pct$unburned_pct <- 100-buffer_burn_severity_pct$total_buffer_burn_pct
# if any slight rounding errors result in negative burned area
buffer_burn_severity_pct$unburned_pct <- ifelse(buffer_burn_severity_pct$unburned_pct < 0, 0, buffer_burn_severity_pct$unburned_pct)

## prepare for plot
buffer_burn_severity_pct <- merge(buffer_burn_severity_pct, lakenamesids, by.x='lagoslakeid', by.y='Lagoslakeid')
buffer_burn_severity_pct_melted <- reshape2::melt(buffer_burn_severity_pct[,c(2:5,7,8)], 
                                                  id.var='Site', variable.name='Severity', value.name='Percent')

buffer_burn_severity_pct_melted$Severity <- factor(buffer_burn_severity_pct_melted$Severity, levels=c('unburned_pct','low_severity_pct',
                                                                                                      'moderate_low_severity_pct','moderate_high_severity_pct','high_severity_pct'))

# get rid of word 'Lake' to make labels smaller
buffer_burn_severity_pct_melted$Lake <- gsub(paste('Lake',collapse='|'),"",buffer_burn_severity_pct_melted$Site)

jpeg('Figures/BurnSeverity_buffer_byLake.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(buffer_burn_severity_pct_melted, aes(fill=Severity, y=Percent, x=Lake)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, color='black'),
        axis.text.y=element_text(color='black'))+
  scale_fill_manual(values=c('gray90','gold','orange','firebrick','black'), 
                    labels=c('Unburned','Low','Low-Moderate','Moderate-High','High'),
                    'Severity (%)')+
  ggtitle('Vegetation burn severity (100m shoreline buffer)')
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

jpeg('Figures/soilburnSeverity_buffer_byLake.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(buffer_soilburn_severity_pct_melted, aes(fill=Severity, y=Percent, x=Lake)) + 
  geom_bar(position="stack", stat="identity")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1, color='black'),
        axis.text.y=element_text(color='black'))+
  scale_fill_manual(values=c('gray90','gold','orange','firebrick','black'), 
                    labels=c('Unburned','Unburned-Low','Low','Moderate','High'),
                    'Severity (%)')+
  ggtitle('100m lake buffer soil burn severity')
dev.off()


### how correlated are vegetation and soil burn severity? ###
# for now, analyzing watershed only (more data points)
veg_soil_severity <- merge(ws_burn_severity_pct, ws_soilburn_severity_pct, by='lagoslakeid')[,c(1:13)]
names(veg_soil_severity) <- c('lagoslakeid','veg_low_severity_pct','veg_moderate_low_severity_pct','veg_moderate_high_severity_pct',
                             'veg_high_severity_pct','veg_total_ws_burn_pct','veg_unburned_pct','Site',
                             'soil_low_severity_pct','soil_moderate_severity_pct','soil_high_severity_pct','soil_total_ws_burn_pct','soil_unburned_pct')
cor(veg_soil_severity[,c(2:7,9:13)], method='pearson', use='pairwise.complete.obs')
