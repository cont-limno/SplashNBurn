################### Exploring lake times series data ##########################
# Date: 8-26-22
# updated:
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(ggplot2)
library(reshape2)
library(dplyr)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")

may_june_july <- read.csv("Data/WaterQuality/may_june_july.csv")
ws_burn_severity <- read.csv("Data/BurnSeverity/vegetation_ws_area_new.csv")# still missing Fourth McDougal

#### Main program ####
# how correlated are % ws burn variables? Seem to be highly so
ws_burn_severity_pct <- ws_burn_severity[,c(1,3,5,7,9,11)]
cor(ws_burn_severity_pct, method='pearson', use='pairwise.complete.obs')
cor(ws_burn_severity_pct, method='spearman', use='pairwise.complete.obs')

# calculate unburned pct
ws_burn_severity_pct$unburned_pct <- 100-ws_burn_severity_pct$total_ws_burn_pct

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
                    'Severity (%)')
dev.off()




