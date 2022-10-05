####################### Exploring water quality data ##########################
# Date: 8-19-22
# updated: 10-4-22# now with all lakes
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(ggplot2)
library(plotly)
library(gridExtra)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")
may_june_july <- read.csv("Data/WaterQuality/may_june_july.csv")
ws_burn_severity <- read.csv("Data/BurnSeverity/vegetation_ws_area.csv")
LAGOStable <- read.csv("Data/LAGOS/LAGOS_GEO_LOCUS_Combined.csv")

#### Main program ####
## grouped boxplots
may_june_july$Month_factor <- factor(as.character(may_june_july$Month), levels = c('may', 'june', 'july'))

month_colors <- c('dodgerblue','tan','gold')

ggplot(may_june_july, aes(x = Lake, y = TP, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
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

## fire predictors of water quality (recall burn severity predictors are highly correlated with each other)
# first log transform WQ variables
may_june_july$logChloro <- log(may_june_july$Chloro)
may_june_july$logTP <- log(may_june_july$TP)
may_june_july$logOP <- log(may_june_july$OP)
may_june_july$logTN <- log(may_june_july$TN)
may_june_july$logNH4N <- log(may_june_july$NH4N)
may_june_july$logNO2NO3 <- log(may_june_july$NO2NO3)
may_june_july$logANCmg <- log(may_june_july$ANCmg)
may_june_july$logANCaueq <- log(may_june_july$ANCaueq)
may_june_july$logTSS <- log(may_june_july$TSS)
may_june_july$logTSVS <- log(may_june_july$TSVS)
may_june_july$logDOC <- log(may_june_july$DOC)

# correlation matrices
ws_burn_severity$total_burn_pct <- rowSums(ws_burn_severity[,c(3,5,7,9)])
mayWQ <- subset(may_june_july, Month=='may')
mayWQ_fire <- merge(mayWQ[,c(3,5,6,24:34)], ws_burn_severity[,c(1,3,5,7,9,11)], by.x='Lagoslakeid', by.y='lagoslakeid')

cormayWQ_fire <- as.data.frame(t(cor(mayWQ_fire[, unlist(lapply(mayWQ_fire, is.numeric))], use='pairwise.complete.obs')))[,c(1:12)] 
cormayWQ_fire <-cormayWQ_fire[c('low_severity_pct','moderate_low_severity_pct','moderate_high_severity_pct','high_severity_pct','total_burn_pct'),]

juneWQ <- subset(may_june_july, Month=='june')
juneWQ_fire <- merge(juneWQ[,c(3,5,6,24:34)], ws_burn_severity[,c(1,3,5,7,9,11)], by.x='Lagoslakeid', by.y='lagoslakeid')

corjuneWQ_fire <- as.data.frame(cor(juneWQ_fire[, unlist(lapply(juneWQ_fire, is.numeric))], use='pairwise.complete.obs'))[,c(1:12)] 
corjuneWQ_fire <-corjuneWQ_fire[c('low_severity_pct','moderate_low_severity_pct','moderate_high_severity_pct','high_severity_pct','total_burn_pct'),]

## "gradient" plots
# may need to play around with plot aesthetics for individual plots
#mayWQ_fire <- merge(mayWQ, ws_burn_severity[,c(1,3,5,7,9,11)], by.x='Lagoslakeid', by.y='lagoslakeid')
mayWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",mayWQ_fire$Site)
juneWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",juneWQ_fire$Site)

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
ggplot(data=may_june_july, aes(x=TN, y=TP, color=Month_factor, shape=Lake))+
  geom_point(size=2)+
  theme_classic()+
  scale_color_manual('Month', values=month_colors)
  
ggplot(data=may_june_july, aes(x=DOC, y=TP, color=Month_factor, shape=Lake))+
  geom_point(size=2)+
  theme_classic()+
  scale_color_manual('Month', values=month_colors)

ggplot(data=may_june_july, aes(x=TSS, y=TP, color=Month_factor, shape=Lake))+
  geom_point(size=2)+
  theme_classic()+
  scale_color_manual('Month', values=month_colors)


## interactive plot
interplot <- may_june_july %>%
  ggplot(aes(x=TN, y=TP, color=Month_factor, label=Site, shape=Type)) +
  geom_point(size=2) +
  theme_classic()+
  #theme(legend.title=element_text('dd'))+
  scale_color_manual('Month', values=month_colors)

ggplotly(interplot, tooltip=c('x','y','label','shape'))
