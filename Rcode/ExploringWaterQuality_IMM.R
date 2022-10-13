####################### Exploring water quality data ##########################
# Date: 8-19-22
# updated: 10-13-22# now with all lakes
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(ggplot2)
library(plotly)
library(gridExtra)
library(lubridate)
library(tidyr)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")
may_june_july <- read.csv("Data/WaterQuality/may_june_july.csv")
may_thru_aug <- read.csv("Data/WaterQuality/LabAnalyses_Lakes9-30-22.csv")
ws_burn_severity <- read.csv("Data/BurnSeverity/vegetation_ws_area.csv")
LAGOStable <- read.csv("Data/LAGOS/LAGOS_LOCUS_Table.csv")
field_obs <- read.csv("Data/WaterQuality/FieldSampling_ Lakes-secchi10-5-22.csv")

#### Main program ####
# get basic LAGOS data in may_thru_aug table
LAGOS_layover <- LAGOStable[,c('lagoslakeid','Site','Type','lake_connectivity_class')]
LAGOS_layover$Type <- ifelse(LAGOS_layover$Type=='sample', 'Burned', 'Control')
LAGOS_layover$ConnClass <- ifelse(LAGOS_layover$lake_connectivity_class %in% c('DrainageLk','Drainage','Headwater'), 'Drainage', LAGOS_layover$lake_connectivity_class)
LAGOS_layover$Group <- paste(LAGOS_layover$Type, LAGOS_layover$ConnClass, sep='_')
  
may_thru_aug <- merge(may_thru_aug, LAGOS_layover, by='Site', all=T)
may_thru_aug$Date <- lubridate::dmy(may_thru_aug$Date)
may_thru_aug$Month <- lubridate::month(may_thru_aug$Date)
may_thru_aug$Month_factor <- lubridate::month(may_thru_aug$Date, label=T, abbr=T)
may_thru_aug <- may_thru_aug %>% drop_na(Date) #row with no data for some reason; remove
may_thru_aug <- droplevels(may_thru_aug)

### grouped boxplots
may_june_july$Month_factor <- factor(as.character(may_june_july$Month), levels = c('may', 'june', 'july'))

month_colors <- c('dodgerblue','tan','gold','orange')

## TP
ggplot(may_thru_aug, aes(x = Group, y = TP, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Total phosphorus (ppb)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_thru_aug, aes(x = Group, y = TP, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c(0,100))+
  labs(x = "Lake Type", y = "Total phosphorus (ppb)")+
  scale_fill_manual("Month", values=month_colors)

## different version; candidate for MW CASC proposal
ggplot(may_june_july, aes(x = Lake, y = TP, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Total phosphorus (ppb)")+
  scale_y_continuous(limits=c(0,50))+
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c(0.8,0.8),
        axis.text.x=element_text(color='black'))
## TN
ggplot(may_june_july, aes(x = Lake, y = TN, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Total nitrogen (ppb)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_thru_aug, aes(x = Group, y = TN, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Total nitrogen (ppb)")+
  scale_fill_manual("Month", values=month_colors)

## NO2NO3
ggplot(may_june_july, aes(x = Lake, y = NO2NO3N, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "NO2/NO3-N (ppb)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_thru_aug, aes(x = Group, y = NO2NO3N, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "NO2/NO3-N (ppb)")+
  scale_fill_manual("Month", values=month_colors)

## NH4
ggplot(may_june_july, aes(x = Lake, y = NH4N, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "NH4-N (ppb)")+
  scale_fill_manual("Month", values=month_colors)

## NH4
ggplot(may_thru_aug, aes(x = Group, y = NH4N, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "NH4-N (ppb)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_thru_aug, aes(x = Group, y = NH4N, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c(0,50))+
  labs(x = "Lake Type", y = "NH4-N (ppb)")+
  scale_fill_manual("Month", values=month_colors)

## DOC
ggplot(may_june_july, aes(x = Lake, y = DOC, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Dissolved organic carbon (ppm)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_thru_aug, aes(x = Group, y = DOC, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c(0,40))+
  labs(x = "Lake Type", y = "DOC (ppm)")+
  scale_fill_manual("Month", values=month_colors)

## TSS
ggplot(may_june_july, aes(x = Lake, y = TSS, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Total suspended solids (mg/L)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_thru_aug, aes(x = Group, y = TSS, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c())+
  labs(x = "Lake Type", y = "Total suspended solids (mg/L)")+
  scale_fill_manual("Month", values=month_colors)

## ANC
ggplot(may_june_july, aes(x = Lake, y = ANCmg, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "Lake Type", y = "Acid neutralizing capacity (mg CaCO3/L)")+
  scale_fill_manual("Month", values=month_colors)

ggplot(may_thru_aug, aes(x = Group, y = ANCmg, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c())+
  labs(x = "Lake Type", y = "Acid neutralizing capacity (mg CaCO3/L)")+
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

may_thru_aug$logChloro <- log(may_thru_aug$Chloro)
may_thru_aug$logTP <- log(may_thru_aug$TP)
#may_thru_aug$logOP <- log(may_thru_aug$OP)
may_thru_aug$logTN <- log(may_thru_aug$TN)
may_thru_aug$logNH4N <- log(may_thru_aug$NH4N)
may_thru_aug$logNO2NO3 <- log(may_thru_aug$NO2NO3)
may_thru_aug$logANCmg <- log(may_thru_aug$ANCmg)
may_thru_aug$logANCaueq <- log(may_thru_aug$ANCaueq)
may_thru_aug$logTSS <- log(may_thru_aug$TSS)
may_thru_aug$logTSVS <- log(may_thru_aug$TSVS)
may_thru_aug$logDOC <- log(may_thru_aug$DOC)

# correlation matrices
ws_burn_severity$total_burn_pct <- rowSums(ws_burn_severity[,c(3,5,7,9)])

# May
mayWQ <- subset(may_thru_aug, Month_factor=='May')
mayWQ_fire <- merge(mayWQ[,c(1,14, 21:30)], ws_burn_severity[,c(1,3,5,7,9,11)], by='lagoslakeid')

cormayWQ_fire <- as.data.frame(t(cor(mayWQ_fire[, unlist(lapply(mayWQ_fire, is.numeric))], use='pairwise.complete.obs')))[,c(1:12)] 
cormayWQ_fire <-cormayWQ_fire[c('low_severity_pct','moderate_low_severity_pct','moderate_high_severity_pct','high_severity_pct','total_burn_pct'),]

# June
juneWQ <- subset(may_thru_aug, Month_factor=='Jun')
juneWQ_fire <- merge(juneWQ[,c(1,14, 21:30)], ws_burn_severity[,c(1,3,5,7,9,11)], by='lagoslakeid')

corjuneWQ_fire <- as.data.frame(cor(juneWQ_fire[, unlist(lapply(juneWQ_fire, is.numeric))], use='pairwise.complete.obs'))[,c(1:12)] 
corjuneWQ_fire <-corjuneWQ_fire[c('low_severity_pct','moderate_low_severity_pct','moderate_high_severity_pct','high_severity_pct','total_burn_pct'),]

# July
julyWQ <- subset(may_thru_aug, Month_factor=='Jul')
julyWQ_fire <- merge(julyWQ[,c(1,14, 21:30)], ws_burn_severity[,c(1,3,5,7,9,11)], by='lagoslakeid')

corjulyWQ_fire <- as.data.frame(cor(julyWQ_fire[, unlist(lapply(julyWQ_fire, is.numeric))], use='pairwise.complete.obs'))[,c(1:12)] 
corjulyWQ_fire <-corjulyWQ_fire[c('low_severity_pct','moderate_low_severity_pct','moderate_high_severity_pct','high_severity_pct','total_burn_pct'),]

# Aug
augWQ <- subset(may_thru_aug, Month_factor=='Aug')
augWQ_fire <- merge(augWQ[,c(1,14, 21:30)], ws_burn_severity[,c(1,3,5,7,9,11)], by='lagoslakeid')

coraugWQ_fire <- as.data.frame(cor(augWQ_fire[, unlist(lapply(augWQ_fire, is.numeric))], use='pairwise.complete.obs'))[,c(1:12)] 
coraugWQ_fire <-coraugWQ_fire[c('low_severity_pct','moderate_low_severity_pct','moderate_high_severity_pct','high_severity_pct','total_burn_pct'),]

# Sep

## "gradient" plots
# may need to play around with plot aesthetics for individual plots
#mayWQ_fire <- merge(mayWQ, ws_burn_severity[,c(1,3,5,7,9,11)], by.x='Lagoslakeid', by.y='lagoslakeid')
mayWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",mayWQ_fire$Site)
juneWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",juneWQ_fire$Site)
julyWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",julyWQ_fire$Site)
augWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",augWQ_fire$Site)

## May
# plot A
wqvar <- 'logTP'
firevar <- 'high_severity_pct'
plotcor <- cor.test(mayWQ_fire[,wqvar], mayWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(mayWQ_fire[,wqvar] ~ mayWQ_fire[,firevar])

plotA <- ggplot(data=mayWQ_fire, aes(x=high_severity_pct, y=logTP, color=Source, label=LakeName))+
  #ggtitle('May total phosphorus')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  annotate(geom="text", x=5, y=4, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=5, y=3.8, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=c(0,100), name='Watershed % high severity')+
  scale_y_continuous(limits=c(), name='log(Total phosphorus (ppb)')+
  #xlab('Watershed high severity burn (%)')+
  #ylab('Log total phosphorus (ppb)')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.9,0.2))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotA

# plot B
wqvar <- 'logTN'
firevar <- 'high_severity_pct'
plotcor <- cor.test(mayWQ_fire[,wqvar], mayWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(mayWQ_fire[,wqvar] ~ mayWQ_fire[,firevar])

plotB <- ggplot(data=mayWQ_fire, aes(x=high_severity_pct, y=logTN, color=Source, label=LakeName))+
  ggtitle('May total nitrogen')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  annotate(geom="text", x=5, y=6.0, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=5, y=5.9, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=c(0,100))+
  #xlab('Watershed high severity burn (%)')+
  #ylab('Log total phosphorus (ppb)')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.9,0.2))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotB

# plot C
wqvar <- 'logDOC'
firevar <- 'moderate_low_severity_pct'
plotcor <- cor.test(mayWQ_fire[,wqvar], mayWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(mayWQ_fire[,wqvar] ~ mayWQ_fire[,firevar])

plotC <- ggplot(data=mayWQ_fire, aes(x=moderate_low_severity_pct, y=logDOC, color=Source, label=LakeName))+
  ggtitle('May dissolved organic carbon')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  annotate(geom="text", x=5, y=4, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=5, y=3.8, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=c(0,40))+
  #xlab('Watershed high severity burn (%)')+
  #ylab('Log total phosphorus (ppb)')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.9,0.9))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotC

# plot D
wqvar <- 'logTSS'
firevar <- 'high_severity_pct'
plotcor <- cor.test(mayWQ_fire[,wqvar], mayWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(mayWQ_fire[,wqvar] ~ mayWQ_fire[,firevar])

plotD <- ggplot(data=mayWQ_fire, aes(x=high_severity_pct, y=logTSS, color=Source, label=LakeName))+
  ggtitle('May total suspended solids')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  annotate(geom="text", x=5, y=-0.5, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=5, y=-0.7, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=c(0,100))+
  scale_y_continuous(limits=c(-1,2))+
  #xlab('Watershed high severity burn (%)')+
  #ylab('Log total phosphorus (ppb)')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.9,0.2))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotD

# jpeg('Figures/multipanel_gradient_may.jpeg',width = 6,height = 6,units = 'in',res=600)
#   grid.arrange(plotA, plotB, plotC, plotD, nrow=2)
# dev.off()

## June
# plot A
wqvar <- 'logTP'
firevar <- 'high_severity_pct'
plotcor <- cor.test(juneWQ_fire[,wqvar], juneWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(juneWQ_fire[,wqvar] ~ juneWQ_fire[,firevar])

plotA <- ggplot(data=juneWQ_fire, aes(x=high_severity_pct, y=logTP, color=Source, label=LakeName))+
  ggtitle('June total phosphorus')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  annotate(geom="text", x=5, y=4, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=5, y=3.8, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=c(0,100))+
  #xlab('Watershed high severity burn (%)')+
  #ylab('Log total phosphorus (ppb)')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.9,0.2))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotA

# plot B
wqvar <- 'logTN'
firevar <- 'high_severity_pct'
plotcor <- cor.test(juneWQ_fire[,wqvar], juneWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(juneWQ_fire[,wqvar] ~ juneWQ_fire[,firevar])

plotB <- ggplot(data=juneWQ_fire, aes(x=high_severity_pct, y=logTN, color=Source, label=LakeName))+
  ggtitle('June total nitrogen')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  annotate(geom="text", x=5, y=6.0, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=5, y=5.9, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=c(0,100))+
  #xlab('Watershed high severity burn (%)')+
  #ylab('Log total phosphorus (ppb)')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.9,0.2))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotB

# plot C
wqvar <- 'logDOC'
firevar <- 'moderate_low_severity_pct'
plotcor <- cor.test(juneWQ_fire[,wqvar], juneWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(juneWQ_fire[,wqvar] ~ juneWQ_fire[,firevar])

plotC <- ggplot(data=juneWQ_fire, aes(x=moderate_low_severity_pct, y=logDOC, color=Source, label=LakeName))+
  ggtitle('June dissolved organic carbon')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  annotate(geom="text", x=5, y=4, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=5, y=3.8, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=c(0,40))+
  #xlab('Watershed high severity burn (%)')+
  #ylab('Log total phosphorus (ppb)')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.9,0.9))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotC

# plot D
wqvar <- 'logTSS'
firevar <- 'high_severity_pct'
plotcor <- cor.test(juneWQ_fire[,wqvar], juneWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(juneWQ_fire[,wqvar] ~ juneWQ_fire[,firevar])

plotD <- ggplot(data=juneWQ_fire, aes(x=high_severity_pct, y=logTSS, color=Source, label=LakeName))+
  ggtitle('June total suspended solids')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  annotate(geom="text", x=5, y=-0.5, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=5, y=-0.7, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=c(0,100))+
  scale_y_continuous(limits=c(-1,2))+
  #xlab('Watershed high severity burn (%)')+
  #ylab('Log total phosphorus (ppb)')+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.9,0.2))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotD

## can we explain residuals from gradient plots?
maytplm <- lm(logTP ~ high_severity_pct, data=mayWQ_fire)
mayalldata <- merge(mayWQ_fire, LAGOStable, by.x='Lagoslakeid', by.y='lagoslakeid')
mayalldata$element <- rownames(mayalldata)
resids <- as.data.frame(residuals(maytplm))
colnames(resids) <- 'residuals'
resids$element <- rownames(resids)
mayalldata <- merge(mayalldata, resids, by='element', all=T)

# data frame that shows correlation between residuals and given variable
residcor <- as.data.frame(cor(mayalldata[, unlist(lapply(mayalldata, is.numeric))], use='pairwise.complete.obs'))  
residcor <- data.frame(variable=rownames(residcor), residuals=residcor$residuals)

## what about the "nutrient space"?
ggplot(data=may_thru_aug, aes(x=TN, y=TP, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_color_manual('Month', values=month_colors)
  
ggplot(data=may_thru_aug, aes(x=DOC, y=TP, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_color_manual('Month', values=month_colors)

ggplot(data=may_thru_aug, aes(x=TSS, y=TP, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_color_manual('Month', values=month_colors)


## interactive plot
interplot <- may_thru_aug %>%
  ggplot(aes(x=TN, y=TP, color=Month_factor, label=Site, shape=Type)) +
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
allWQ_data <- merge(may_thru_aug, secchi_depth, by=c("Site","Month"), all=T)
#write.csv(allWQ_data, "Data/WaterQuality/combined_lab_field_may_aug.csv", row.names=F)

ggplot(data=allWQ_data, aes(x=TP, y=SecchiDepth_m, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_color_manual('Month', values=month_colors)

ggplot(data=allWQ_data, aes(x=DOC, y=SecchiDepth_m, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(limits=c(0,40))+
  scale_color_manual('Month', values=month_colors)

ggplot(data=allWQ_data, aes(x=TSS, y=SecchiDepth_m, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(limits=c())+
  scale_color_manual('Month', values=month_colors)

ggplot(data=allWQ_data, aes(x=TN, y=SecchiDepth_m, color=Month_factor, shape=Group))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(limits=c())+
  scale_color_manual('Month', values=month_colors)

## separate the months
allWQ_data_may <- subset(allWQ_data, Month_factor=='May')
allWQ_data_jun <- subset(allWQ_data, Month_factor=='Jun')
allWQ_data_jul <- subset(allWQ_data, Month_factor=='Jul')
allWQ_data_aug <- subset(allWQ_data, Month_factor=='Aug')

burn_colors <- c('firebrick','dodgerblue')

## DOC vs Secchi
a <- ggplot(data=allWQ_data_may, aes(x=DOC, y=SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=c(0,35))+
  scale_y_continuous(name='Secchi (m)', limits=c(0,4))+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.7))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Connectivity', color='Group')

b <- ggplot(data=allWQ_data_jun, aes(x=DOC, y=SecchiDepth_m, color=Type, shape=ConnClass))+
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

c <- ggplot(data=allWQ_data_jul, aes(x=DOC, y=SecchiDepth_m, color=Type, shape=ConnClass))+
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

d <- ggplot(data=allWQ_data_aug, aes(x=DOC, y=SecchiDepth_m, color=Type, shape=ConnClass))+
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

grid.arrange(a,b,c,d)

jpeg('Figures/DOC_vs_Secchi.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d)
dev.off()

# TP vs Secchi
xlimz <- c(0,50)
ylimz <- c(0,4)
a <- ggplot(data=allWQ_data_may, aes(x=TP, y=SecchiDepth_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Secchi (m)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.7))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Connectivity', color='Group')

b <- ggplot(data=allWQ_data_jun, aes(x=TP, y=SecchiDepth_m, color=Type, shape=ConnClass))+
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

c <- ggplot(data=allWQ_data_jul, aes(x=TP, y=SecchiDepth_m, color=Type, shape=ConnClass))+
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

d <- ggplot(data=allWQ_data_aug, aes(x=TP, y=SecchiDepth_m, color=Type, shape=ConnClass))+
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

grid.arrange(a,b,c,d)

jpeg('Figures/TP_vs_Secchi.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d)
dev.off()

# DOC vs TP
xlimz <- c(0,50)
ylimz <- c(0,35)
a <- ggplot(data=allWQ_data_may, aes(x=DOC, y=TP, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='DOC (ppm)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.7))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Connectivity', color='Group')

b <- ggplot(data=allWQ_data_jun, aes(x=DOC, y=TP, color=Type, shape=ConnClass))+
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

c <- ggplot(data=allWQ_data_jul, aes(x=DOC, y=TP, color=Type, shape=ConnClass))+
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

d <- ggplot(data=allWQ_data_aug, aes(x=DOC, y=TP, color=Type, shape=ConnClass))+
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

grid.arrange(a,b,c,d)

jpeg('Figures/DOC_vs_TP.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d)
dev.off()


# TN vs TP
xlimz <- c(0,1500)
ylimz <- c(0,50)
a <- ggplot(data=allWQ_data_may, aes(x=TN, y=TP, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.2,0.7))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')

b <- ggplot(data=allWQ_data_jun, aes(x=TN, y=TP, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.2,0.7))+
  scale_color_manual(values=burn_colors)+
  labs(color='Type')
b + guides(shape='none')

c <- ggplot(data=allWQ_data_jul, aes(x=TN, y=TP, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('July 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

d <- ggplot(data=allWQ_data_aug, aes(x=TN, y=TP, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TN (ppb)',limits=xlimz)+
  scale_y_continuous(name='TP (ppb)', limits=ylimz)+
  ggtitle('August 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')

grid.arrange(a,b + guides(shape='none'),c,d)

jpeg('Figures/TN_vs_TP.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b + guides(shape='none'),c,d)
dev.off()

# TP vs TSS
xlimz <- c(0,50)
ylimz <- c()
a <- ggplot(data=allWQ_data_may, aes(x=TP, y=TSS, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='TSS (mg/L)', limits=ylimz)+
  ggtitle('May 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.2,0.7))+
  scale_color_manual(values=burn_colors, guide='none')+
  labs(shape='Connectivity')

b <- ggplot(data=allWQ_data_jun, aes(x=TP, y=TSS, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='TSS (mg/L)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.2,0.7))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Class', color='Type')
b + guides(shape='none')

c <- ggplot(data=allWQ_data_jul, aes(x=TP, y=TSS, color=Type, shape=ConnClass))+
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

d <- ggplot(data=allWQ_data_aug, aes(x=TP, y=TSS, color=Type, shape=ConnClass))+
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

grid.arrange(a,b + guides(shape='none'),c,d)

jpeg('Figures/TP_vs_TSS.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b + guides(shape='none'),c,d)
dev.off()


# TP vs zMax
xlimz <- c(0,50)
ylimz <- c(0,10)
a <- ggplot(data=allWQ_data_may, aes(x=TP, y=zMax_m, color=Type, shape=ConnClass))+
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

b <- ggplot(data=allWQ_data_jun, aes(x=TP, y=zMax_m, color=Type, shape=ConnClass))+
  geom_point(size=2)+
  theme_classic()+
  scale_x_continuous(name='TP (ppb)',limits=xlimz)+
  scale_y_continuous(name='Max depth (m)', limits=ylimz)+
  ggtitle('June 2022')+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  scale_color_manual(values=burn_colors)+
  labs(shape='Connectivity', color='Type')


c <- ggplot(data=allWQ_data_jul, aes(x=TP, y=zMax_m, color=Type, shape=ConnClass))+
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

d <- ggplot(data=allWQ_data_aug, aes(x=TP, y=zMax_m, color=Type, shape=ConnClass))+
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

grid.arrange(a,b,c,d)

jpeg('Figures/TP_vs_zMax.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d)
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

grid.arrange(a,b,c,d)

jpeg('Figures/Secchi_vs_zMax.jpeg',width = 7,height = 7,units = 'in',res=600)
  grid.arrange(a,b,c,d)
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
