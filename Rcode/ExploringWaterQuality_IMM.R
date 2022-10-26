####################### Exploring water quality data ##########################
# Date: 8-19-22
# updated: 10-25-22# move burn severity gradient analysis to other script
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(ggplot2)
library(plotly)
library(gridExtra)
library(lubridate)
library(tidyr)
library(tidyverse)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")

# water quality
may_june_july <- read.csv("Data/WaterQuality/may_june_july.csv")
may_thru_aug <- read.csv("Data/WaterQuality/LabAnalyses_Lakes9-30-22.csv")
may_thru_sep <- read.csv("Data/WaterQuality/LabAnalyses_Lakes10-20-22.csv")
field_obs <- read.csv("Data/WaterQuality/FieldSampling_Lakes-secchi_field_sheet10-19-22complete.csv")

# LAGOS ancillary data
LAGOStable <- read.csv("Data/LAGOS/LAGOS_LOCUS_Table.csv")

# burn severity variables
ws_vbs <- read.csv("Data/BurnSeverity/Ian_calculations/burned_ws_vbs_pct.csv")
ws_sbs <- read.csv("Data/BurnSeverity/Ian_calculations/burned_ws_sbs_pct.csv")
buff100m_vbs <- read.csv("Data/BurnSeverity/Ian_calculations/burned_buff100m_vbs_pct.csv")
buff100m_sbs <- read.csv("Data/BurnSeverity/Ian_calculations/burned_buff100m_sbs_pct.csv")

#### D-fine functions ####
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 

#### Main program ####
# get basic LAGOS data in may_thru_sep table
LAGOS_layover <- LAGOStable[,c('lagoslakeid','Site','Type','lake_connectivity_class')]
LAGOS_layover$Type <- ifelse(LAGOS_layover$Type=='sample', 'Burned', 'Control')
LAGOS_layover$ConnClass <- ifelse(LAGOS_layover$lake_connectivity_class %in% c('DrainageLk','Drainage','Headwater'), 'Drainage', LAGOS_layover$lake_connectivity_class)
LAGOS_layover$Group <- paste(LAGOS_layover$Type, LAGOS_layover$ConnClass, sep='_')
  
may_thru_sep <- merge(may_thru_sep, LAGOS_layover, by='Site', all=T)
may_thru_sep$Date <- lubridate::dmy(may_thru_sep$Date)
may_thru_sep$Month <- lubridate::month(may_thru_sep$Date)
may_thru_sep$Month_factor <- lubridate::month(may_thru_sep$Date, label=T, abbr=T)
may_thru_sep <- may_thru_sep %>% drop_na(Date) #row with no data for some reason; remove
may_thru_sep <- droplevels(may_thru_sep)

# combine burn severity datasets
# for total burn columns, considering low to high as burned
ws_vbs$ws_vbs_total_burn_pct <- rowSums(ws_vbs[,c(14:17)], na.rm=T)
ws_vbs <- ws_vbs[,c(1,13:18)]
names(ws_vbs) <- c('lagoslakeid','ws_vbs_Unburned_pct',
                   'ws_vbs_Low_pct','ws_vbs_ModerateLow_pct','ws_vbs_ModerateHigh_pct',
                   'ws_vbs_High_pct','ws_vbs_total_burn_pct')

ws_sbs$ws_sbs_total_burn_pct <- rowSums(ws_sbs[,c(13:15)], na.rm=T)
ws_sbs <- ws_sbs[,c(1,11:16)]
names(ws_sbs) <- c('lagoslakeid','ws_sbs_Unburned_pct',
                   'ws_sbs_Unburned_Low_pct','ws_sbs_Low_pct','ws_sbs_Moderate_pct',
                   'ws_sbs_High_pct','ws_sbs_total_burn_pct')

buff100m_vbs$buff100m_vbs_total_burn_pct <- rowSums(buff100m_vbs[,c(9:12)], na.rm=T)
buff100m_vbs <- buff100m_vbs[,c(1,8:13)]
names(buff100m_vbs) <- c('lagoslakeid','buff100m_vbs_Unburned_pct',
                         'buff100m_vbs_Low_pct','buff100m_vbs_ModerateLow_pct','buff100m_vbs_ModerateHigh_pct',
                         'buff100m_vbs_High_pct','buff100m_vbs_total_burn_pct')

buff100m_sbs$buff100m_sbs_total_burn_pct <- rowSums(buff100m_sbs[,c(10:12)], na.rm=T)
buff100m_sbs <- buff100m_sbs[,c(1,8:13)]
names(buff100m_sbs) <- c('lagoslakeid','buff100m_sbs_Unburned_pct',
                         'buff100m_sbs_UnburnedLow_pct','buff100m_sbs_Low_pct','buff100m_sbs_Moderate_pct',
                         'buff100m_sbs_High_pct','buff100m_sbs_total_burn_pct')

df_list <- list(ws_vbs, ws_sbs, buff100m_vbs, buff100m_sbs)
burn_severity <- df_list %>% reduce(full_join, by='lagoslakeid')
burn_severity[is.na(burn_severity)] <- 0 #replace NAs with 0 (represent true 0s)

#write.csv(burn_severity, file='Data/BurnSeverity/Ian_calculations/all_burn_severity_variables.csv', row.names=F)

### grouped boxplots
may_june_july$Month_factor <- factor(as.character(may_june_july$Month), levels = c('may', 'june', 'july'))

month_colors <- c('dodgerblue','tan','gold','orange','darkgreen')

## TP
ggplot(may_thru_sep, aes(x = Group, y = TP_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Total phosphorus (ppb)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_thru_sep, aes(x = Group, y = TP_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c(0,100))+
  labs(x = "Lake Type", y = "Total phosphorus (ppb)")+
  scale_fill_manual("Month", values=month_colors)

## different version; candidate for MW CASC proposal
ggplot(may_thru_sep, aes(x = Group, y = TP_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Total phosphorus (ppb)")+
  scale_y_continuous(limits=c(0,50))+
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c(0.8,0.8),
        axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))+
  scale_x_discrete(labels=c('Burned drainage','Burned isolated',
                            'Control drainage','Control isolated'))
## TN
ggplot(may_thru_sep, aes(x = Group, y = TN_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Total nitrogen (ppb)")+
  scale_fill_manual("Month", values=month_colors)

## NO2NO3
ggplot(may_thru_sep, aes(x = Group, y = NO2NO3_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "NO2/NO3-N (ppb)")+
  scale_fill_manual("Month", values=month_colors)

## NH4
ggplot(may_thru_sep, aes(x = Group, y = NH4N_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "NH4-N (ppb)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_thru_sep, aes(x = Group, y = NH4N_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c(0,50))+
  labs(x = "Lake Type", y = "NH4-N (ppb)")+
  scale_fill_manual("Month", values=month_colors)

## DOC
ggplot(may_thru_sep, aes(x = Group, y = DOC_ppm, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c(0,40))+
  labs(x = "Lake Type", y = "DOC (ppm)")+
  scale_fill_manual("Month", values=month_colors)+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  #ggtitle("B) Hydrologic connectivity")+
  scale_x_discrete(labels=c('Burned drainage','Burned isolated',
                            'Control drainage','Control isolated'))

## TSS
ggplot(may_thru_sep, aes(x = Group, y = TSS_mgL, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c())+
  labs(x = "Lake Type", y = "Total suspended solids (mg/L)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_thru_sep, aes(x = Group, y = TSS_mgL, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c(0,10))+
  labs(x = "Lake Type", y = "Total suspended solids (mg/L)")+
  scale_fill_manual("Month", values=month_colors)

## ANC
ggplot(may_thru_sep, aes(x = Group, y = ANC_mgCaCO3L, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c())+
  labs(x = "Lake Type", y = "Acid neutralizing capacity (mg CaCO3/L)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_thru_sep, aes(x = Group, y = ANC_ueqL, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c())+
  labs(x = "Lake Type", y = "Acid neutralizing capacity (ueq/L)")+
  scale_fill_manual("Month", values=month_colors)

## chloro
ggplot(may_thru_sep, aes(x = Group, y = Chloro_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c())+
  labs(x = "Lake Type", y = "Chlorophyll-a (ppb)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_thru_sep, aes(x = Group, y = Phaeo_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c())+
  labs(x = "Lake Type", y = "Phaeo_ppb")+
  scale_fill_manual("Month", values=month_colors)


## fire predictors of water quality (recall burn severity predictors are highly correlated with each other)
# first log transform WQ variables
# may_june_july$logChloro <- log(may_june_july$Chloro)
# may_june_july$logTP <- log(may_june_july$TP)
# may_june_july$logOP <- log(may_june_july$OP)
# may_june_july$logTN <- log(may_june_july$TN)
# may_june_july$logNH4N <- log(may_june_july$NH4N)
# may_june_july$logNO2NO3 <- log(may_june_july$NO2NO3)
# may_june_july$logANCmg <- log(may_june_july$ANCmg)
# may_june_july$logANCaueq <- log(may_june_july$ANCaueq)
# may_june_july$logTSS <- log(may_june_july$TSS)
# may_june_july$logTSVS <- log(may_june_july$TSVS)
# may_june_july$logDOC <- log(may_june_july$DOC)

may_thru_sep$logChloro <- log(may_thru_sep$Chloro_ppb)
may_thru_sep$logTP <- log(may_thru_sep$TP_ppb)
#may_thru_sep$logOP <- log(may_thru_sep$OP)
may_thru_sep$logTN <- log(may_thru_sep$TN_ppb)
may_thru_sep$logNH4N <- log(may_thru_sep$NH4N_ppb)
may_thru_sep$logNO2NO3 <- log(may_thru_sep$NO2NO3_ppb)
may_thru_sep$logANCmg <- log(may_thru_sep$ANC_mgCaCO3L)
may_thru_sep$logANCaueq <- log(may_thru_sep$ANC_ueqL)
may_thru_sep$logTSS <- log(may_thru_sep$TSS_mgL)
may_thru_sep$logTSVS <- log(may_thru_sep$TSVS_mgL)
may_thru_sep$logDOC <- log(may_thru_sep$DOC_ppm)

# May
mayWQ <- subset(may_thru_sep, Month_factor=='May')
mayWQ_fire <- merge(mayWQ[,c(1,14, 21:30)], burn_severity, by='lagoslakeid')

cormayWQ_fire <- as.data.frame(t(cor(mayWQ_fire[, unlist(lapply(mayWQ_fire, is.numeric))], use='pairwise.complete.obs')))[,c(1:11)] 

# June
juneWQ <- subset(may_thru_sep, Month_factor=='Jun')
juneWQ_fire <- merge(juneWQ[,c(1,14, 21:30)], burn_severity, by='lagoslakeid')

corjuneWQ_fire <- as.data.frame(cor(juneWQ_fire[, unlist(lapply(juneWQ_fire, is.numeric))], use='pairwise.complete.obs'))[,c(1:11)] 

# July
julyWQ <- subset(may_thru_sep, Month_factor=='Jul')
julyWQ_fire <- merge(julyWQ[,c(1,14, 21:30)], burn_severity, by='lagoslakeid')

corjulyWQ_fire <- as.data.frame(cor(julyWQ_fire[, unlist(lapply(julyWQ_fire, is.numeric))], use='pairwise.complete.obs'))[,c(1:11)] 

# Aug
augWQ <- subset(may_thru_sep, Month_factor=='Aug')
augWQ_fire <- merge(augWQ[,c(1,14, 21:30)], burn_severity, by='lagoslakeid')

coraugWQ_fire <- as.data.frame(cor(augWQ_fire[, unlist(lapply(augWQ_fire, is.numeric))], use='pairwise.complete.obs'))[,c(1:11)] 

# Sep
sepWQ <- subset(may_thru_sep, Month_factor=='Sep')
sepWQ_fire <- merge(sepWQ[,c(1,14, 21:30)], burn_severity, by='lagoslakeid')

corsepWQ_fire <- as.data.frame(cor(sepWQ_fire[, unlist(lapply(sepWQ_fire, is.numeric))], use='pairwise.complete.obs'))[,c(1:11)] 


## "gradient" plots
# may need to play around with plot aesthetics for individual plots
#mayWQ_fire <- merge(mayWQ, ws_burn_severity[,c(1,3,5,7,9,11)], by.x='Lagoslakeid', by.y='lagoslakeid')
mayWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",mayWQ_fire$Site)
juneWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",juneWQ_fire$Site)
julyWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",julyWQ_fire$Site)
augWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",augWQ_fire$Site)
sepWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",sepWQ_fire$Site)

mayWQ_fire <- merge(mayWQ_fire, LAGOS_layover, by='lagoslakeid', all=F)
juneWQ_fire <- merge(juneWQ_fire, LAGOS_layover, by='lagoslakeid', all=F)
julyWQ_fire <- merge(julyWQ_fire, LAGOS_layover, by='lagoslakeid', all=F)
augWQ_fire <- merge(augWQ_fire, LAGOS_layover, by='lagoslakeid', all=F)
sepWQ_fire <- merge(sepWQ_fire, LAGOS_layover, by='lagoslakeid', all=F)

## May: example plot for CASC proposal
# plot A
wqvar <- 'logTP'
firevar <- 'ws_vbs_High_pct'
plotcor <- cor.test(mayWQ_fire[,wqvar], mayWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(mayWQ_fire[,wqvar] ~ mayWQ_fire[,firevar])

plotA <- ggplot(data=mayWQ_fire, aes(x=ws_vbs_High_pct, y=logTP, color=ConnClass, label=LakeName))+
  #ggtitle('May total phosphorus')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  #geom_abline(slope = coef(plotlm)[2], 
  #            intercept = coef(plotlm)[["(Intercept)"]])+
  geom_vline(xintercept=25, linetype='dashed')+
  #annotate(geom="text", x=5, y=4, label=paste0("r=",rval),
  #         color="red")+
  #annotate(geom="text", x=5, y=3.8, label=paste0("p=",pval),
  #         color="red")+
  theme_classic()+
  scale_x_continuous(limits=c(0,100), name='Watershed % high severity burn')+
  scale_y_continuous(limits=c(), name='log(Total phosphorus) (ppb)')+
  #xlab('Watershed high severity burn (%)')+
  #ylab('Log total phosphorus (ppb)')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.9,0.2))+
  ggtitle('A) Burn severity gradient')+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotA

###########################################################################

## can we explain residuals from gradient plots?
maytplm <- lm(logTP ~ ws_vbs_High_pct, data=mayWQ_fire)
mayalldata <- merge(mayWQ_fire, LAGOStable, by='lagoslakeid')
mayalldata$element <- rownames(mayalldata)
resids <- as.data.frame(residuals(maytplm))
colnames(resids) <- 'residuals'
resids$element <- rownames(resids)
mayalldata <- merge(mayalldata, resids, by='element', all=T)

# data frame that shows correlation between residuals and given variable
residcor <- as.data.frame(cor(mayalldata[, unlist(lapply(mayalldata, is.numeric))], use='pairwise.complete.obs'))  
residcor <- data.frame(variable=rownames(residcor), residuals=residcor$residuals)

## what about the "nutrient space"?
ggplot(data=may_thru_sep, aes(x=TN_ppb, y=TP_ppb, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_color_manual('Month', values=month_colors)
  
ggplot(data=may_thru_sep, aes(x=DOC_ppm, y=TP_ppb, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_color_manual('Month', values=month_colors)

ggplot(data=may_thru_sep, aes(x=TSS_mgL, y=TP_ppb, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_color_manual('Month', values=month_colors)


## interactive plot
interplot <- may_thru_sep %>%
  ggplot(aes(x=TN_ppb, y=TP_ppb, color=Month_factor, label=Site, shape=Type)) +
  geom_point(size=2) +
  theme_classic()+
  #theme(legend.title=element_text('dd'))+
  scale_color_manual('Month', values=month_colors)

ggplotly(interplot, tooltip=c('x','y','label','shape'))

#### Secchi and depth data ####
field_obs <- merge(field_obs, LAGOS_layover, by='Site', all=T)
field_obs$Date <- lubridate::mdy(field_obs$Date)
field_obs$Month <- lubridate::month(field_obs$Date)
field_obs$Month_factor <- lubridate::month(field_obs$Date, label=T, abbr=T)
field_obs <- field_obs %>% drop_na(Date) #row with no data for some reason; remove
field_obs <- droplevels(field_obs)

ggplot(field_obs, aes(x = Group, y = SecchiDepth_m, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Secchi (m)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(field_obs, aes(x = Group, y = zMax_m, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Max depth (m)")+
  scale_fill_manual("Month", values=month_colors)

## get table of just Secchi and few other vars to merge to other water quality
secchi_depth <- field_obs[,c('SecchiDepth_m','zMax_m','Site','Month')]
allWQ_data <- merge(may_thru_sep, secchi_depth, by=c("Site","Month"), all=T)
#write.csv(allWQ_data, "Data/WaterQuality/combined_lab_field_may_sep.csv", row.names=F)

ggplot(data=allWQ_data, aes(x=TP_ppb, y=SecchiDepth_m, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_color_manual('Month', values=month_colors)

ggplot(data=allWQ_data, aes(x=DOC_ppm, y=SecchiDepth_m, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(limits=c(0,40))+
  scale_color_manual('Month', values=month_colors)

ggplot(data=allWQ_data, aes(x=TSS_mgL, y=SecchiDepth_m, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(limits=c())+
  scale_color_manual('Month', values=month_colors)

ggplot(data=allWQ_data, aes(x=TN_ppb, y=SecchiDepth_m, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(limits=c())+
  scale_color_manual('Month', values=month_colors)

ggplot(data=allWQ_data, aes(x=Chloro_ppb, y=SecchiDepth_m, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(limits=c())+
  scale_color_manual('Month', values=month_colors)

ggplot(data=allWQ_data, aes(x=Chloro_ppb, y=TP_ppb, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(limits=c())+
  scale_color_manual('Month', values=month_colors)

ggplot(data=allWQ_data, aes(x=Chloro_ppb, y=TN_ppb, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(limits=c())+
  scale_color_manual('Month', values=month_colors)

## separate the months
allWQ_data_may <- subset(allWQ_data, Month_factor=='May')
allWQ_data_jun <- subset(allWQ_data, Month_factor=='Jun')
allWQ_data_jul <- subset(allWQ_data, Month_factor=='Jul')
allWQ_data_aug <- subset(allWQ_data, Month_factor=='Aug')
allWQ_data_sep <- subset(allWQ_data, Month==9)

burn_colors <- c('firebrick','dodgerblue')

## DOC vs Secchi
a <- ggplot(data=allWQ_data_may, aes(x=DOC_ppm, y=SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=c(0,35))+
  scale_y_continuous(name='Secchi (m)', limits=c(0,4))+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Connectivity', color='Group')

b <- ggplot(data=allWQ_data_jun, aes(x=DOC_ppm, y=SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=c(0,35))+
  scale_y_continuous(name='Secchi (m)', limits=c(0,4))+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

c <- ggplot(data=allWQ_data_jul, aes(x=DOC_ppm, y=SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=c(0,35))+
  scale_y_continuous(name='Secchi (m)', limits=c(0,4))+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

d <- ggplot(data=allWQ_data_aug, aes(x=DOC_ppm, y=SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=c(0,35))+
  scale_y_continuous(name='Secchi (m)', limits=c(0,4))+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

e <- ggplot(data=allWQ_data_sep, aes(x=DOC_ppm, y=SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=c(0,35))+
  scale_y_continuous(name='Secchi (m)', limits=c(0,4))+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

legendplot <- ggplot(data=allWQ_data_may, aes(x=DOC_ppm, y=SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=c(0,35))+
  scale_y_continuous(name='Secchi (m)', limits=c(0,4))+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.2,0.6))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Connectivity', color='Group')

legend <- g_legend(legendplot) 
grid.arrange(a,b,c,d,e,legend)

jpeg('Figures/DOC_vs_Secchi.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d,e,legend)
dev.off()

# TP vs Secchi
xlimz <- c(0,50)
ylimz <- c(0,4)
a <- ggplot(data=allWQ_data_may, aes(x=TP_ppb, y=SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Connectivity', color='Group')

b <- ggplot(data=allWQ_data_jun, aes(x=TP_ppb, y=SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

c <- ggplot(data=allWQ_data_jul, aes(x=TP_ppb, y=SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

d <- ggplot(data=allWQ_data_aug, aes(x=TP_ppb, y=SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

e <- ggplot(data=allWQ_data_sep, aes(x=TP_ppb, y=SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

grid.arrange(a,b,c,d,e,legend)

jpeg('Figures/TP_vs_Secchi.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d,e,legend)
dev.off()

# DOC vs TP
xlimz <- c(0,50)
ylimz <- c(0,35)
a <- ggplot(data=allWQ_data_may, aes(x=DOC_ppm, y=TP_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Connectivity', color='Group')

b <- ggplot(data=allWQ_data_jun, aes(x=DOC_ppm, y=TP_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

c <- ggplot(data=allWQ_data_jul, aes(x=DOC_ppm, y=TP_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

d <- ggplot(data=allWQ_data_aug, aes(x=DOC_ppm, y=TP_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

e <- ggplot(data=allWQ_data_sep, aes(x=DOC_ppm, y=TP_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

grid.arrange(a,b,c,d,e,legend)

jpeg('Figures/DOC_vs_TP.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d,e,legend)
dev.off()

# TN vs TP
xlimz <- c(0,1500)
ylimz <- c(0,50)
a <- ggplot(data=allWQ_data_may, aes(x=TN_ppb, y=TP_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  geom_abline(intercept=0, slope=16)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')

b <- ggplot(data=allWQ_data_jun, aes(x=TN_ppb, y=TP_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  geom_abline(intercept=0, slope=16)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(color='Type')
#b + guides(shape='none')

c <- ggplot(data=allWQ_data_jul, aes(x=TN_ppb, y=TP_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  geom_abline(intercept=0, slope=16)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

d <- ggplot(data=allWQ_data_aug, aes(x=TN_ppb, y=TP_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  geom_abline(intercept=0, slope=16)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

e <- ggplot(data=allWQ_data_sep, aes(x=TN_ppb, y=TP_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  geom_abline(intercept=0, slope=16)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

grid.arrange(a,b,c,d,e,legend)

jpeg('Figures/TN_vs_TP.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d,e,legend)
dev.off()

# TP vs TSS
xlimz <- c(0,50)
ylimz <- c()
a <- ggplot(data=allWQ_data_may, aes(x=TP_ppb, y=TSS_mgL, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='TSS (mg/L)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')

b <- ggplot(data=allWQ_data_jun, aes(x=TP_ppb, y=TSS_mgL, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='TSS (mg/L)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

c <- ggplot(data=allWQ_data_jul, aes(x=TP_ppb, y=TSS_mgL, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='TSS (mg/L)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

d <- ggplot(data=allWQ_data_aug, aes(x=TP_ppb, y=TSS_mgL, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='TSS (mg/L)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

e <- ggplot(data=allWQ_data_sep, aes(x=TP_ppb, y=TSS_mgL, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='TSS (mg/L)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

grid.arrange(a,b,c,d,e,legend)

jpeg('Figures/TP_vs_TSS.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d,e,legend)
dev.off()

# TP vs chloro
xlimz <- c(0,50)
ylimz <- c(0,10)
a <- ggplot(data=allWQ_data_may, aes(x=TP_ppb, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')

b <- ggplot(data=allWQ_data_jun, aes(x=TP_ppb, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

c <- ggplot(data=allWQ_data_jul, aes(x=TP_ppb, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

d <- ggplot(data=allWQ_data_aug, aes(x=TP_ppb, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

e <- ggplot(data=allWQ_data_sep, aes(x=TP_ppb, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

grid.arrange(a,b,c,d,e,legend)

jpeg('Figures/TP_vs_chloro.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d,e,legend)
dev.off()

# TN vs chloro
xlimz <- c(0,1500)
ylimz <- c(0,25)
a <- ggplot(data=allWQ_data_may, aes(x=TN_ppb, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')

b <- ggplot(data=allWQ_data_jun, aes(x=TN_ppb, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

c <- ggplot(data=allWQ_data_jul, aes(x=TN_ppb, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

d <- ggplot(data=allWQ_data_aug, aes(x=TN_ppb, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

e <- ggplot(data=allWQ_data_sep, aes(x=TN_ppb, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

grid.arrange(a,b,c,d,e,legend)

jpeg('Figures/TN_vs_chloro.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d,e,legend)
dev.off()

# TSS vs chloro
xlimz <- c(0,10)
ylimz <- c(0,25)
a <- ggplot(data=allWQ_data_may, aes(x=TSS_mgL, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TSS (mg/L)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')

b <- ggplot(data=allWQ_data_jun, aes(x=TSS_mgL, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TSS (mg/L)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

c <- ggplot(data=allWQ_data_jul, aes(x=TSS_mgL, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TSS (mg/L)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

d <- ggplot(data=allWQ_data_aug, aes(x=TSS_mgL, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TSS (mg/L)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

e <- ggplot(data=allWQ_data_sep, aes(x=TSS_mgL, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TSS (mg/L)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

grid.arrange(a,b,c,d,e,legend)

jpeg('Figures/TSS_vs_chloro.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d,e,legend)
dev.off()

# DOC vs chloro
xlimz <- c(0,45)
ylimz <- c(0,25)
a <- ggplot(data=allWQ_data_may, aes(x=DOC_ppm, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')

b <- ggplot(data=allWQ_data_jun, aes(x=DOC_ppm, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

c <- ggplot(data=allWQ_data_jul, aes(x=DOC_ppm, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

d <- ggplot(data=allWQ_data_aug, aes(x=DOC_ppm, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

e <- ggplot(data=allWQ_data_sep, aes(x=DOC_ppm, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

grid.arrange(a,b,c,d,e,legend)

jpeg('Figures/DOC_vs_chloro.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d,e,legend)
dev.off()

# Secchi vs chloro
xlimz <- c(0,5)
ylimz <- c(0,25)
a <- ggplot(data=allWQ_data_may, aes(x=SecchiDepth_m, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Secchi (m)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')

b <- ggplot(data=allWQ_data_jun, aes(x=SecchiDepth_m, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Secchi (m)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

c <- ggplot(data=allWQ_data_jul, aes(x=SecchiDepth_m, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Secchi (m)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

d <- ggplot(data=allWQ_data_aug, aes(x=SecchiDepth_m, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Secchi (m)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

e <- ggplot(data=allWQ_data_sep, aes(x=SecchiDepth_m, y=Chloro_ppb, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Secchi (m)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

grid.arrange(a,b,c,d,e,legend)

jpeg('Figures/Secchi_vs_chloro.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d,e,legend)
dev.off()



# TP vs zMax
xlimz <- c(0,50)
ylimz <- c(0,10)
a <- ggplot(data=allWQ_data_may, aes(x=TP_ppb, y=zMax_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Max depth (m)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')

b <- ggplot(data=allWQ_data_jun, aes(x=TP_ppb, y=zMax_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Max depth (m)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Connectivity', color='Type')


c <- ggplot(data=allWQ_data_jul, aes(x=TP_ppb, y=zMax_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Max depth (m)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

d <- ggplot(data=allWQ_data_aug, aes(x=TP_ppb, y=zMax_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Max depth (m)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

e <- ggplot(data=allWQ_data_sep, aes(x=TP_ppb, y=zMax_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Max depth (m)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

grid.arrange(a,b,c,d,e,legend)

jpeg('Figures/TP_vs_zMax.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d,e,legend)
dev.off()


# Secchi vs zMax
xlimz <- c(0,10)
ylimz <- c(0,4)
a <- ggplot(data=allWQ_data_may, aes(x=zMax_m, SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Max depth (m)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(c('none')))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')

b <- ggplot(data=allWQ_data_jun, aes(x=zMax_m, SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Max depth (m)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(color='Type')


c <- ggplot(data=allWQ_data_jul, aes(x=zMax_m, SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Max depth (m)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

d <- ggplot(data=allWQ_data_aug, aes(x=zMax_m, SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Max depth (m)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

e <- ggplot(data=allWQ_data_sep, aes(x=zMax_m, SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Max depth (m)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

grid.arrange(a,b,c,d,e,legend)

jpeg('Figures/Secchi_vs_zMax.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d,e,legend)
dev.off()

## How much does max depth vary for a given lake across sample months? ## 
allWQ_data$Lake <- gsub(paste('Lake',collapse='|'),"",allWQ_data$Site)

jpeg('Figures/MaxDepth_variation.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(allWQ_data, aes(x = Lake, y = zMax_m, fill = Type)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "", y = "Max depth (m)")+
  theme(axis.text.x=element_text(color='black', angle=90, size=8, hjust=1, vjust=0.5))+
  scale_fill_manual("Type", values=burn_colors)
dev.off()

#### Similar plots, but with regression lines/confidence intervals
# Note that for now, lumped isolated and drainage so would only be 
# regression lines for control vs burned

# first make new legend with only control vs burned
legendplot <- ggplot(data=allWQ_data_may, aes(x=DOC_ppm, y=SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=c(0,35))+
  scale_y_continuous(name='Secchi (m)', limits=c(0,4))+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.2,0.6))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Connectivity', color='Group')

legend2 <- g_legend(legendplot)

# TP vs chloro
xlimz <- c(0,50)
ylimz <- c(0,10)
a <- ggplot(data=allWQ_data_may, aes(x=TP_ppb, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.1,0.7))+
  scale_color_manual(values=burn_colors, guide='none')
a + geom_smooth(method='lm')

b <- ggplot(data=allWQ_data_jun, aes(x=TP_ppb, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
b + geom_smooth(method='lm')

c <- ggplot(data=allWQ_data_jul, aes(x=TP_ppb, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
c + geom_smooth(method='lm')

d <- ggplot(data=allWQ_data_aug, aes(x=TP_ppb, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
d + geom_smooth(method='lm')

e <- ggplot(data=allWQ_data_sep, aes(x=TP_ppb, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
e + geom_smooth(method='lm')

grid.arrange(a+geom_smooth(method='lm'),b+geom_smooth(method='lm'),
             c+geom_smooth(method='lm'),d+geom_smooth(method='lm'),
             e+geom_smooth(method='lm'),legend2)

## DOC vs Secchi
a <- ggplot(data=allWQ_data_may, aes(x=DOC_ppm, y=SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=c(0,35))+
  scale_y_continuous(name='Secchi (m)', limits=c(0,4))+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Connectivity', color='Group')
a + geom_smooth(method='lm')

b <- ggplot(data=allWQ_data_jun, aes(x=DOC_ppm, y=SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=c(0,35))+
  scale_y_continuous(name='Secchi (m)', limits=c(0,4))+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
b + geom_smooth(method='lm')

c <- ggplot(data=allWQ_data_jul, aes(x=DOC_ppm, y=SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=c(0,35))+
  scale_y_continuous(name='Secchi (m)', limits=c(0,4))+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
c + geom_smooth(method='lm')

d <- ggplot(data=allWQ_data_aug, aes(x=DOC_ppm, y=SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=c(0,35))+
  scale_y_continuous(name='Secchi (m)', limits=c(0,4))+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
d + geom_smooth(method='lm')

e <- ggplot(data=allWQ_data_sep, aes(x=DOC_ppm, y=SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=c(0,35))+
  scale_y_continuous(name='Secchi (m)', limits=c(0,4))+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
e + geom_smooth(method='lm')

grid.arrange(a+geom_smooth(method='lm'),b+geom_smooth(method='lm'),
             c+geom_smooth(method='lm'),d+geom_smooth(method='lm'),
             e+geom_smooth(method='lm'),legend2)

# TP vs Secchi
xlimz <- c(0,50)
ylimz <- c(0,4)
a <- ggplot(data=allWQ_data_may, aes(x=TP_ppb, y=SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Connectivity', color='Group')
a+geom_smooth(method='lm')

b <- ggplot(data=allWQ_data_jun, aes(x=TP_ppb, y=SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
b+geom_smooth(method='lm')

c <- ggplot(data=allWQ_data_jul, aes(x=TP_ppb, y=SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
c+geom_smooth(method='lm')

d <- ggplot(data=allWQ_data_aug, aes(x=TP_ppb, y=SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
d+geom_smooth(method='lm')

e <- ggplot(data=allWQ_data_sep, aes(x=TP_ppb, y=SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
e+geom_smooth(method='lm')

grid.arrange(a+geom_smooth(method='lm'),b+geom_smooth(method='lm'),
             c+geom_smooth(method='lm'),d+geom_smooth(method='lm'),
             e+geom_smooth(method='lm'),legend2)

# DOC vs TP
xlimz <- c(0,50)
ylimz <- c(0,35)
a <- ggplot(data=allWQ_data_may, aes(x=DOC_ppm, y=TP_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Connectivity', color='Group')
a+geom_smooth(method='lm')

b <- ggplot(data=allWQ_data_jun, aes(x=DOC_ppm, y=TP_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
b+geom_smooth(method='lm')

c <- ggplot(data=allWQ_data_jul, aes(x=DOC_ppm, y=TP_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
c+geom_smooth(method='lm')

d <- ggplot(data=allWQ_data_aug, aes(x=DOC_ppm, y=TP_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
d+geom_smooth(method='lm')

e <- ggplot(data=allWQ_data_sep, aes(x=DOC_ppm, y=TP_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
e+geom_smooth(method='lm')

grid.arrange(a+geom_smooth(method='lm'),b+geom_smooth(method='lm'),
             c+geom_smooth(method='lm'),d+geom_smooth(method='lm'),
             e+geom_smooth(method='lm'),legend2)

# TN vs TP
xlimz <- c(0,1500)
ylimz <- c(0,50)
a <- ggplot(data=allWQ_data_may, aes(x=TN_ppb, y=TP_ppb, color=Type))+
  geom_point(size=2)+
  geom_abline(intercept=0, slope=16)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')
a +geom_smooth(method='lm')

b <- ggplot(data=allWQ_data_jun, aes(x=TN_ppb, y=TP_ppb, color=Type))+
  geom_point(size=2)+
  geom_abline(intercept=0, slope=16)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(color='Type')
b + geom_smooth(method='lm')

c <- ggplot(data=allWQ_data_jul, aes(x=TN_ppb, y=TP_ppb, color=Type))+
  geom_point(size=2)+
  geom_abline(intercept=0, slope=16)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
c + geom_smooth(method='lm')

d <- ggplot(data=allWQ_data_aug, aes(x=TN_ppb, y=TP_ppb, color=Type))+
  geom_point(size=2)+
  geom_abline(intercept=0, slope=16)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
d + geom_smooth(method='lm')

e <- ggplot(data=allWQ_data_sep, aes(x=TN_ppb, y=TP_ppb, color=Type))+
  geom_point(size=2)+
  geom_abline(intercept=0, slope=16)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
e + geom_smooth(method='lm')

grid.arrange(a+geom_smooth(method='lm'),b+geom_smooth(method='lm'),
             c+geom_smooth(method='lm'),d+geom_smooth(method='lm'),
             e+geom_smooth(method='lm'),legend2)

# TP vs TSS
xlimz <- c(0,50)
ylimz <- c()
a <- ggplot(data=allWQ_data_may, aes(x=TP_ppb, y=TSS_mgL, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='TSS (mg/L)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')
a + geom_smooth(method='lm')

b <- ggplot(data=allWQ_data_jun, aes(x=TP_ppb, y=TSS_mgL, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='TSS (mg/L)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
b + geom_smooth(method='lm')

c <- ggplot(data=allWQ_data_jul, aes(x=TP_ppb, y=TSS_mgL, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='TSS (mg/L)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
c + geom_smooth(method='lm')

d <- ggplot(data=allWQ_data_aug, aes(x=TP_ppb, y=TSS_mgL, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='TSS (mg/L)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
d + geom_smooth(method='lm')

e <- ggplot(data=allWQ_data_sep, aes(x=TP_ppb, y=TSS_mgL, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='TSS (mg/L)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
e + geom_smooth(method='lm')

grid.arrange(a+geom_smooth(method='lm'),b+geom_smooth(method='lm'),
             c+geom_smooth(method='lm'),d+geom_smooth(method='lm'),
             e+geom_smooth(method='lm'),legend2)


# TN vs chloro
xlimz <- c(0,1500)
ylimz <- c(0,25)
a <- ggplot(data=allWQ_data_may, aes(x=TN_ppb, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')
a + geom_smooth(method='lm')

b <- ggplot(data=allWQ_data_jun, aes(x=TN_ppb, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
b + geom_smooth(method='lm')

c <- ggplot(data=allWQ_data_jul, aes(x=TN_ppb, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
c + geom_smooth(method='lm')

d <- ggplot(data=allWQ_data_aug, aes(x=TN_ppb, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
d + geom_smooth(method='lm')

e <- ggplot(data=allWQ_data_sep, aes(x=TN_ppb, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
e + geom_smooth(method='lm')

grid.arrange(a+geom_smooth(method='lm'),b+geom_smooth(method='lm'),
             c+geom_smooth(method='lm'),d+geom_smooth(method='lm'),
             e+geom_smooth(method='lm'),legend2)

# TSS vs chloro
xlimz <- c(0,10)
ylimz <- c(0,25)
a <- ggplot(data=allWQ_data_may, aes(x=TSS_mgL, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TSS (mg/L)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')
a + geom_smooth(method='lm')

b <- ggplot(data=allWQ_data_jun, aes(x=TSS_mgL, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TSS (mg/L)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
b + geom_smooth(method='lm')

c <- ggplot(data=allWQ_data_jul, aes(x=TSS_mgL, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TSS (mg/L)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
c + geom_smooth(method='lm')

d <- ggplot(data=allWQ_data_aug, aes(x=TSS_mgL, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TSS (mg/L)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
d + geom_smooth(method='lm')

e <- ggplot(data=allWQ_data_sep, aes(x=TSS_mgL, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TSS (mg/L)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
e + geom_smooth(method='lm')

grid.arrange(a+geom_smooth(method='lm'),b+geom_smooth(method='lm'),
             c+geom_smooth(method='lm'),d+geom_smooth(method='lm'),
             e+geom_smooth(method='lm'),legend2)

# DOC vs chloro
xlimz <- c(0,45)
ylimz <- c(0,25)
a <- ggplot(data=allWQ_data_may, aes(x=DOC_ppm, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')
a + geom_smooth(method='lm')

b <- ggplot(data=allWQ_data_jun, aes(x=DOC_ppm, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
b + geom_smooth(method='lm')

c <- ggplot(data=allWQ_data_jul, aes(x=DOC_ppm, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
c + geom_smooth(method='lm')

d <- ggplot(data=allWQ_data_aug, aes(x=DOC_ppm, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
d + geom_smooth(method='lm')

e <- ggplot(data=allWQ_data_sep, aes(x=DOC_ppm, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
e + geom_smooth(method='lm')

grid.arrange(a+geom_smooth(method='lm'),b+geom_smooth(method='lm'),
             c+geom_smooth(method='lm'),d+geom_smooth(method='lm'),
             e+geom_smooth(method='lm'),legend2)

# Secchi vs chloro
xlimz <- c(0,5)
ylimz <- c(0,25)
a <- ggplot(data=allWQ_data_may, aes(x=SecchiDepth_m, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Secchi (m)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')
a + geom_smooth(method='lm')

b <- ggplot(data=allWQ_data_jun, aes(x=SecchiDepth_m, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Secchi (m)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
b + geom_smooth(method='lm')

c <- ggplot(data=allWQ_data_jul, aes(x=SecchiDepth_m, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Secchi (m)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
c + geom_smooth(method='lm')

d <- ggplot(data=allWQ_data_aug, aes(x=SecchiDepth_m, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Secchi (m)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
d + geom_smooth(method='lm')

e <- ggplot(data=allWQ_data_sep, aes(x=SecchiDepth_m, y=Chloro_ppb, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Secchi (m)',limits=xlimz)+
  scale_y_continuous(name='Chlorophyll-a (ppb)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
e +geom_smooth(method='lm')

grid.arrange(a+geom_smooth(method='lm'),b+geom_smooth(method='lm'),
             c+geom_smooth(method='lm'),d+geom_smooth(method='lm'),
             e+geom_smooth(method='lm'),legend2)

# TP vs zMax
xlimz <- c(0,50)
ylimz <- c(0,10)
a <- ggplot(data=allWQ_data_may, aes(x=TP_ppb, y=zMax_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Max depth (m)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')
a + geom_smooth(method='lm')

b <- ggplot(data=allWQ_data_jun, aes(x=TP_ppb, y=zMax_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Max depth (m)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Connectivity', color='Type')
b + geom_smooth(method='lm')

c <- ggplot(data=allWQ_data_jul, aes(x=TP_ppb, y=zMax_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Max depth (m)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
c + geom_smooth(method='lm')

d <- ggplot(data=allWQ_data_aug, aes(x=TP_ppb, y=zMax_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Max depth (m)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
d + geom_smooth(method='lm')

e <- ggplot(data=allWQ_data_sep, aes(x=TP_ppb, y=zMax_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Max depth (m)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
e + geom_smooth(method='lm')

grid.arrange(a+geom_smooth(method='lm'),b+geom_smooth(method='lm'),
             c+geom_smooth(method='lm'),d+geom_smooth(method='lm'),
             e+geom_smooth(method='lm'),legend2)


# Secchi vs zMax
xlimz <- c(0,10)
ylimz <- c(0,4)
a <- ggplot(data=allWQ_data_may, aes(x=zMax_m, SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Max depth (m)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(c('none')))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')
a + geom_smooth(method='lm')

b <- ggplot(data=allWQ_data_jun, aes(x=zMax_m, SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Max depth (m)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(color='Type')
b + geom_smooth(method='lm')

c <- ggplot(data=allWQ_data_jul, aes(x=zMax_m, SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Max depth (m)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
c + geom_smooth(method='lm')

d <- ggplot(data=allWQ_data_aug, aes(x=zMax_m, SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Max depth (m)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
d + geom_smooth(method='lm')

e <- ggplot(data=allWQ_data_sep, aes(x=zMax_m, SecchiDepth_m, color=Type))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='Max depth (m)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('September 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
e + geom_smooth(method='lm')

grid.arrange(a+geom_smooth(method='lm'),b+geom_smooth(method='lm'),
             c+geom_smooth(method='lm'),d+geom_smooth(method='lm'),
             e+geom_smooth(method='lm'),legend2)

#####
## Can we cram multiple grouped boxed plots into multi-panel plot?
TP_box <- ggplot(allWQ_data, aes(x = Group, y = TP_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "", y = "TP (ppb)")+
  scale_y_continuous(limits=c(0,50))+ #0,50
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c('none'),
        axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.background=element_rect(color = 'black', fill = 'white', linetype='solid'))+
  scale_x_discrete(labels=c('BD','BI','CD','CI'))

TN_box <- ggplot(allWQ_data, aes(x = Group, y = TN_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "", y = "TN (ppb)")+
  scale_y_continuous(limits=c())+
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c('none'),
        axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))+
  scale_x_discrete(labels=c('BD','BI','CD','CI'))

DOC_box <- ggplot(allWQ_data, aes(x = Group, y = DOC_ppm, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "", y = "DOC (ppm)")+ #0,40
  scale_y_continuous(limits=c())+
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c(0.85,0.75), #0.85,0.7
        axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.title=element_blank(),
        legend.key.size=unit(0.5, "cm"))+
        #legend.background=element_rect(color = 'black', fill = 'white', linetype='solid'))+
  scale_x_discrete(labels=c('BD','BI','CD','CI'))

TSS_box <- ggplot(allWQ_data, aes(x = Group, y = TSS_mgL, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c(0,10))+
  labs(x = "", y = "TSS (mg/L)")+
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c('none'),
        axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))+
  scale_x_discrete(labels=c('BD','BI','CD','CI'))

chla_box <- ggplot(allWQ_data, aes(x = Group, y = Chloro_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c())+
  labs(x = "", y = "Chlorophyll-a (ppb)")+
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c('none'),
        axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))+
  scale_x_discrete(labels=c('BD','BI','CD','CI'))

secchi_box <- ggplot(allWQ_data, aes(x = Group, y = SecchiDepth_m, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c())+
  labs(x = "", y = "Secchi (m)")+
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c('none'),
        axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))+
  scale_x_discrete(labels=c('BD','BI','CD','CI'))

legendbox <- ggplot(may_thru_sep, aes(x = Group, y = Chloro_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c())+
  labs(x = "", y = "Chlorophyll-a (ppb)")+
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c(0.5,0.5),
        axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'))+
  scale_x_discrete(labels=c('BD','BI','CD','CI'))
legendbox <- g_legend(legendbox)

grid.arrange(TP_box, TN_box, DOC_box, TSS_box, chla_box, secchi_box, nrow=2)

jpeg('Figures/multipanel_month_boxplots.jpeg',width = 7,height = 5,units = 'in',res=600)
  grid.arrange(TP_box, TN_box, DOC_box, TSS_box, chla_box, secchi_box, nrow=2)
dev.off()
