####################### Water quality/fire gradient analysis ##################
# Date: 10-25-22
# updated: 11-29-22; added breakpoint analysis at end
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(ggplot2)
library(gridExtra)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")

# water quality
waterquality <- read.csv("Data/WaterQuality/combined_lab_field_may_sep.csv")

# burn severity variables
burn_severity <- read.csv("Data/BurnSeverity/Ian_calculations/all_burn_severity_variables.csv")

#### D-fine functions ####
g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 

##### Main program #####
# how correlated are burn severity variables?
# seems could use total watershed % burned if you had to pick one variable
cor(burn_severity, method='pearson', use='pairwise.complete.obs')
cor(burn_severity, method='spearman', use='pairwise.complete.obs')
corPearson_df <- as.data.frame(cor(burn_severity, method='pearson', use='pairwise.complete.obs'))
corPearson_df_total <- as.data.frame(corPearson_df$ws_vbs_total_burn_pct)
colnames(corPearson_df_total) <- 'ws_vbs_total_burn_pct'
rownames(corPearson_df_total) <- rownames(corPearson_df)

## prepare individual months
waterquality$logSecchi <- log(waterquality$SecchiDepth_m)
waterquality$logTNTP <- log(waterquality$TNTP)

# May
mayWQ <- subset(waterquality, Month_factor=='May')
mayWQ_fire <- merge(mayWQ, burn_severity, by='lagoslakeid')

cormayWQ_fire <- as.data.frame(t(cor(mayWQ_fire[, unlist(lapply(mayWQ_fire, is.numeric))], use='pairwise.complete.obs')))[,c(1:11)] 

# June
juneWQ <- subset(waterquality, Month_factor=='Jun')
juneWQ_fire <- merge(juneWQ, burn_severity, by='lagoslakeid')

corjuneWQ_fire <- as.data.frame(cor(juneWQ_fire[, unlist(lapply(juneWQ_fire, is.numeric))], use='pairwise.complete.obs'))[,c(1:11)] 

# July
julyWQ <- subset(waterquality, Month_factor=='Jul')
julyWQ_fire <- merge(julyWQ, burn_severity, by='lagoslakeid')

corjulyWQ_fire <- as.data.frame(cor(julyWQ_fire[, unlist(lapply(julyWQ_fire, is.numeric))], use='pairwise.complete.obs'))[,c(1:11)] 

# Aug
augWQ <- subset(waterquality, Month_factor=='Aug')
augWQ_fire <- merge(augWQ, burn_severity, by='lagoslakeid')

coraugWQ_fire <- as.data.frame(cor(augWQ_fire[, unlist(lapply(augWQ_fire, is.numeric))], use='pairwise.complete.obs'))[,c(1:11)] 

# Sep
sepWQ <- subset(waterquality, Month_factor=='Sep')
sepWQ_fire <- merge(sepWQ, burn_severity, by='lagoslakeid')

corsepWQ_fire <- as.data.frame(cor(sepWQ_fire[, unlist(lapply(sepWQ_fire, is.numeric))], use='pairwise.complete.obs'))[,c(1:11)] 

## prepare lake names without word lake and bring in LAGOS attribute data
mayWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",mayWQ_fire$Site)
juneWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",juneWQ_fire$Site)
julyWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",julyWQ_fire$Site)
augWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",augWQ_fire$Site)
sepWQ_fire$LakeName <- gsub(paste('Lake',collapse='|'),"",sepWQ_fire$Site)

#### Gradient plots ####

## Chlorophyll-a
# set variables and plotting parameters for all plots
wqvar <- 'logChloro'
firevar <- 'ws_vbs_High_pct'
#firevar <- 'ws_vbs_total_burn_pct'
colorvar <- 'ConnClass'
labelvar <- 'LakeName'
xlimz <- c(0,100)
ylimz <- c(-1,3)
xlabb <- 'Watershed % burned high severity'
#xlabb <- 'Watershed % burned'
ylabb <- 'log(Chla) (ppb)'
rvalx <- 95
rvaly <- -0.4
pvalx <- 95
pvaly <- -0.7

# May
plotcor <- cor.test(mayWQ_fire[,wqvar], mayWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(mayWQ_fire[,wqvar] ~ mayWQ_fire[,firevar])
plotMay <- ggplot(data=mayWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('A) May')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+ #0.5,0.2
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotMay

# June
plotcor <- cor.test(juneWQ_fire[,wqvar], juneWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(juneWQ_fire[,wqvar] ~ juneWQ_fire[,firevar])
plotJun <- ggplot(data=juneWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('B) June')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJun

# July
plotcor <- cor.test(julyWQ_fire[,wqvar], julyWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(julyWQ_fire[,wqvar] ~ julyWQ_fire[,firevar])
plotJul <- ggplot(data=julyWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('C) July')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJul

# August
plotcor <- cor.test(augWQ_fire[,wqvar], augWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(augWQ_fire[,wqvar] ~ augWQ_fire[,firevar])
plotAug <- ggplot(data=augWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('D) August')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotAug

# September
plotcor <- cor.test(sepWQ_fire[,wqvar], sepWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(sepWQ_fire[,wqvar] ~ sepWQ_fire[,firevar])
plotSep <- ggplot(data=sepWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('E) September')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotSep

grid.arrange(plotMay, plotJun, plotJul, plotAug, plotSep, nrow=2)

### TP ###
# set variables and plotting parameters for all plots
wqvar <- 'logTP'
firevar <- 'ws_vbs_High_pct'
#firevar <- 'ws_vbs_total_burn_pct'
colorvar <- 'ConnClass'
labelvar <- 'LakeName'
xlimz <- c(0,100)
ylimz <- c(2,4)
xlabb <- 'Watershed % burned high severity'
#xlabb <- 'Watershed % burned'
ylabb <- 'log(Total phosphorus) (ppb)'
rvalx <- 5
rvaly <- 4
pvalx <- 5
pvaly <- 3.8

# May
plotcor <- cor.test(mayWQ_fire[,wqvar], mayWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(mayWQ_fire[,wqvar] ~ mayWQ_fire[,firevar])
plotMay <- ggplot(data=mayWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('A) May')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.5,0.2))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotMay

# June
plotcor <- cor.test(juneWQ_fire[,wqvar], juneWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(juneWQ_fire[,wqvar] ~ juneWQ_fire[,firevar])
plotJun <- ggplot(data=juneWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('B) June')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJun

# July
plotcor <- cor.test(julyWQ_fire[,wqvar], julyWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(julyWQ_fire[,wqvar] ~ julyWQ_fire[,firevar])
plotJul <- ggplot(data=julyWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('C) July')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJul

# August
plotcor <- cor.test(augWQ_fire[,wqvar], augWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(augWQ_fire[,wqvar] ~ augWQ_fire[,firevar])
plotAug <- ggplot(data=augWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('D) August')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotAug

# September
plotcor <- cor.test(sepWQ_fire[,wqvar], sepWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(sepWQ_fire[,wqvar] ~ sepWQ_fire[,firevar])
plotSep <- ggplot(data=sepWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('E) September')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotSep

grid.arrange(plotMay, plotJun, plotJul, plotAug, plotSep, nrow=2)

# jpeg('Figures/multipanel_gradient_may.jpeg',width = 6,height = 6,units = 'in',res=600)
#   grid.arrange(plotA, plotB, plotC, plotD, nrow=2)
# dev.off()

### TN ###
# set variables and plotting parameters for all plots
wqvar <- 'logTN'
#firevar <- 'ws_vbs_High_pct'
firevar <- 'ws_vbs_total_burn_pct'
colorvar <- 'ConnClass'
labelvar <- 'LakeName'
xlimz <- c(0,100)
ylimz <- c(6,7)
#xlabb <- 'Watershed % burned high severity'
xlabb <- 'Watershed % burned'
ylabb <- 'log(Total nitrogen) (ppb)'
rvalx <- 95
rvaly <- 6.3
pvalx <- 95
pvaly <- 6.1

# May
plotcor <- cor.test(mayWQ_fire[,wqvar], mayWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(mayWQ_fire[,wqvar] ~ mayWQ_fire[,firevar])
plotMay <- ggplot(data=mayWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('A) May')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.5,0.2))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotMay

# June
plotcor <- cor.test(juneWQ_fire[,wqvar], juneWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(juneWQ_fire[,wqvar] ~ juneWQ_fire[,firevar])
plotJun <- ggplot(data=juneWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('B) June')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJun

# July
plotcor <- cor.test(julyWQ_fire[,wqvar], julyWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(julyWQ_fire[,wqvar] ~ julyWQ_fire[,firevar])
plotJul <- ggplot(data=julyWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('C) July')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJul

# August
plotcor <- cor.test(augWQ_fire[,wqvar], augWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(augWQ_fire[,wqvar] ~ augWQ_fire[,firevar])
plotAug <- ggplot(data=augWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('D) August')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotAug

# September
plotcor <- cor.test(sepWQ_fire[,wqvar], sepWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(sepWQ_fire[,wqvar] ~ sepWQ_fire[,firevar])
plotSep <- ggplot(data=sepWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('E) September')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotSep

grid.arrange(plotMay, plotJun, plotJul, plotAug, plotSep, nrow=2)

### TNTP ###
# set variables and plotting parameters for all plots
wqvar <- 'logTNTP'
firevar <- 'ws_vbs_High_pct'
#firevar <- 'ws_vbs_total_burn_pct'
colorvar <- 'ConnClass'
labelvar <- 'LakeName'
xlimz <- c(0,100)
ylimz <- c(3,5)
xlabb <- 'Watershed % burned high severity'
#xlabb <- 'Watershed % burned'
ylabb <- 'log(TN/TP)'
rvalx <- 95
rvaly <- 4.8
pvalx <- 95
pvaly <- 4.6

# May
plotcor <- cor.test(mayWQ_fire[,wqvar], mayWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(mayWQ_fire[,wqvar] ~ mayWQ_fire[,firevar])
plotMay <- ggplot(data=mayWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('A) May')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.5,0.8))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotMay

# June
plotcor <- cor.test(juneWQ_fire[,wqvar], juneWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(juneWQ_fire[,wqvar] ~ juneWQ_fire[,firevar])
plotJun <- ggplot(data=juneWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('B) June')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJun

# July
plotcor <- cor.test(julyWQ_fire[,wqvar], julyWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(julyWQ_fire[,wqvar] ~ julyWQ_fire[,firevar])
plotJul <- ggplot(data=julyWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('C) July')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJul

# August
plotcor <- cor.test(augWQ_fire[,wqvar], augWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(augWQ_fire[,wqvar] ~ augWQ_fire[,firevar])
plotAug <- ggplot(data=augWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('D) August')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotAug

# September
plotcor <- cor.test(sepWQ_fire[,wqvar], sepWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(sepWQ_fire[,wqvar] ~ sepWQ_fire[,firevar])
plotSep <- ggplot(data=sepWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('E) September')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotSep

grid.arrange(plotMay, plotJun, plotJul, plotAug, plotSep, nrow=2)


### DOC ###
# set variables and plotting parameters for all plots
wqvar <- 'logDOC'
firevar <- 'ws_vbs_High_pct'
#firevar <- 'ws_vbs_total_burn_pct'
colorvar <- 'ConnClass'
labelvar <- 'LakeName'
xlimz <- c(0,100)
ylimz <- c(1,4)
xlabb <- 'Watershed % burned high severity'
#xlabb <- 'Watershed % burned'
ylabb <- 'log(DOC) (ppm)'
rvalx <- 95
rvaly <- 1.3
pvalx <- 95
pvaly <- 1.1

# May
plotcor <- cor.test(mayWQ_fire[,wqvar], mayWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(mayWQ_fire[,wqvar] ~ mayWQ_fire[,firevar])
plotMay <- ggplot(data=mayWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('A) May')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.5,0.2))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotMay

# June
plotcor <- cor.test(juneWQ_fire[,wqvar], juneWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(juneWQ_fire[,wqvar] ~ juneWQ_fire[,firevar])
plotJun <- ggplot(data=juneWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('B) June')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJun

# July
plotcor <- cor.test(julyWQ_fire[,wqvar], julyWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(julyWQ_fire[,wqvar] ~ julyWQ_fire[,firevar])
plotJul <- ggplot(data=julyWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('C) July')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJul

# August
plotcor <- cor.test(augWQ_fire[,wqvar], augWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(augWQ_fire[,wqvar] ~ augWQ_fire[,firevar])
plotAug <- ggplot(data=augWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('D) August')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotAug

# September
plotcor <- cor.test(sepWQ_fire[,wqvar], sepWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(sepWQ_fire[,wqvar] ~ sepWQ_fire[,firevar])
plotSep <- ggplot(data=sepWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('E) September')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotSep

grid.arrange(plotMay, plotJun, plotJul, plotAug, plotSep, nrow=2)

### TSS ###
# set variables and plotting parameters for all plots
wqvar <- 'logTSS'
#firevar <- 'ws_vbs_High_pct'
firevar <- 'ws_vbs_total_burn_pct'
colorvar <- 'ConnClass'
labelvar <- 'LakeName'
xlimz <- c(0,100)
ylimz <- c(-1,2)
#xlabb <- 'Watershed % burned high severity'
xlabb <- 'Watershed % burned'
ylabb <- 'log(TSS) (mgL)'
rvalx <- 95
rvaly <- -0.7
pvalx <- 95
pvaly <- -0.9

# May
plotcor <- cor.test(mayWQ_fire[,wqvar], mayWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(mayWQ_fire[,wqvar] ~ mayWQ_fire[,firevar])
plotMay <- ggplot(data=mayWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('A) May')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.5,0.2))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotMay

# June
plotcor <- cor.test(juneWQ_fire[,wqvar], juneWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(juneWQ_fire[,wqvar] ~ juneWQ_fire[,firevar])
plotJun <- ggplot(data=juneWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('B) June')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJun

# July
plotcor <- cor.test(julyWQ_fire[,wqvar], julyWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(julyWQ_fire[,wqvar] ~ julyWQ_fire[,firevar])
plotJul <- ggplot(data=julyWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('C) July')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJul

# August
plotcor <- cor.test(augWQ_fire[,wqvar], augWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(augWQ_fire[,wqvar] ~ augWQ_fire[,firevar])
plotAug <- ggplot(data=augWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('D) August')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotAug

# September
plotcor <- cor.test(sepWQ_fire[,wqvar], sepWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(sepWQ_fire[,wqvar] ~ sepWQ_fire[,firevar])
plotSep <- ggplot(data=sepWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('E) September')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotSep

grid.arrange(plotMay, plotJun, plotJul, plotAug, plotSep, nrow=2)

### Secchi ###
# set variables and plotting parameters for all plots
wqvar <- 'logSecchi'
firevar <- 'ws_vbs_High_pct'
#firevar <- 'ws_vbs_total_burn_pct'
colorvar <- 'ConnClass'
labelvar <- 'LakeName'
xlimz <- c(0,100)
ylimz <- c(-1,2)
xlabb <- 'Watershed % burned high severity'
#xlabb <- 'Watershed % burned'
ylabb <- 'log(Secchi) (m)'
rvalx <- 95
rvaly <- -0.7
pvalx <- 95
pvaly <- -0.9

# May
plotcor <- cor.test(mayWQ_fire[,wqvar], mayWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(mayWQ_fire[,wqvar] ~ mayWQ_fire[,firevar])
plotMay <- ggplot(data=mayWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('A) May')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.2,0.8))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotMay

# June
plotcor <- cor.test(juneWQ_fire[,wqvar], juneWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(juneWQ_fire[,wqvar] ~ juneWQ_fire[,firevar])
plotJun <- ggplot(data=juneWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('B) June')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJun

# July
plotcor <- cor.test(julyWQ_fire[,wqvar], julyWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(julyWQ_fire[,wqvar] ~ julyWQ_fire[,firevar])
plotJul <- ggplot(data=julyWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('C) July')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJul

# August
plotcor <- cor.test(augWQ_fire[,wqvar], augWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(augWQ_fire[,wqvar] ~ augWQ_fire[,firevar])
plotAug <- ggplot(data=augWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('D) August')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotAug

# September
plotcor <- cor.test(sepWQ_fire[,wqvar], sepWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(sepWQ_fire[,wqvar] ~ sepWQ_fire[,firevar])
plotSep <- ggplot(data=sepWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('E) September')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotSep

grid.arrange(plotMay, plotJun, plotJul, plotAug, plotSep, nrow=2)

## pH
# set variables and plotting parameters for all plots
wqvar <- 'pH'
#firevar <- 'ws_vbs_High_pct'
firevar <- 'ws_vbs_total_burn_pct'
colorvar <- 'ConnClass'
labelvar <- 'LakeName'
xlimz <- c(0,100)
ylimz <- c(6,10)
#xlabb <- 'Watershed % burned high severity'
xlabb <- 'Watershed % burned'
ylabb <- 'pH'
rvalx <- 95
rvaly <- 9.5
pvalx <- 95
pvaly <- 9

# May
plotcor <- cor.test(mayWQ_fire[,wqvar], mayWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(mayWQ_fire[,wqvar] ~ mayWQ_fire[,firevar])
plotMay <- ggplot(data=mayWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('A) May')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+ #0.5,0.2
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotMay

# June
plotcor <- cor.test(juneWQ_fire[,wqvar], juneWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(juneWQ_fire[,wqvar] ~ juneWQ_fire[,firevar])
plotJun <- ggplot(data=juneWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('B) June')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJun

# July
plotcor <- cor.test(julyWQ_fire[,wqvar], julyWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(julyWQ_fire[,wqvar] ~ julyWQ_fire[,firevar])
plotJul <- ggplot(data=julyWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('C) July')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotJul

# August
plotcor <- cor.test(augWQ_fire[,wqvar], augWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(augWQ_fire[,wqvar] ~ augWQ_fire[,firevar])
plotAug <- ggplot(data=augWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('D) August')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotAug

# September
plotcor <- cor.test(sepWQ_fire[,wqvar], sepWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(sepWQ_fire[,wqvar] ~ sepWQ_fire[,firevar])
plotSep <- ggplot(data=sepWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('E) September')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=3, nudge_x=0.5)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red")+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red")+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotSep

grid.arrange(plotMay, plotJun, plotJul, plotAug, plotSep, nrow=2)


##### Segmented regression #####
library(segmented)
# good help here: https://rpubs.com/MarkusLoew/12164

# burn table to help with interpretation of plots
burntable <- juneWQ_fire[,c('Site','Type','ws_vbs_High_pct','ws_vbs_total_burn_pct')]
par(mfrow=c(1,1))
hist(burntable$ws_vbs_total_burn_pct, main='Watershed % burned', xlab='',
     breaks=seq(0,100,10))
hist(burntable$ws_vbs_High_pct, main='Watershed % burned high severity', 
     xlab='', breaks=seq(0,100,10))

## May TP
plot(logTP ~ ws_vbs_High_pct, data=mayWQ_fire, pch=16, main='May TP',
     xlab='Watershed % high severity burn', ylab='logTP')
tp_lm <- lm(logTP ~ ws_vbs_High_pct, data=mayWQ_fire)
summary(tp_lm)

test_seg <- segmented(tp_lm, 
                    seg.Z = ~ ws_vbs_High_pct, 
                    psi = list(ws_vbs_High_pct = c(10, 30)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)


## Jun TP (warning: no breakpoint estimated)
plot(logTP ~ ws_vbs_High_pct, data=juneWQ_fire, pch=16, main='June TP',
     xlab='Watershed % high severity burn', ylab='logTP')
tp_lm <- lm(logTP ~ ws_vbs_High_pct, data=juneWQ_fire)
summary(tp_lm)

test_seg <- segmented(tp_lm, 
                      seg.Z = ~ ws_vbs_High_pct, 
                      psi = list(ws_vbs_High_pct = c(10, 30)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Jul TP: no point, basically flat line
plot(logTP ~ ws_vbs_High_pct, data=julyWQ_fire, pch=16, main='July TP',
     xlab='Watershed % high severity burn', ylab='logTP')
tp_lm <- lm(logTP ~ ws_vbs_High_pct, data=julyWQ_fire)
summary(tp_lm)

test_seg <- segmented(tp_lm, 
                      seg.Z = ~ ws_vbs_High_pct, 
                      psi = list(ws_vbs_High_pct = c(10, 30)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Aug TP: warning, no breakpoint estimated
plot(logTP ~ ws_vbs_High_pct, data=augWQ_fire, pch=16, main='August TP',
     xlab='Watershed % high severity burn', ylab='logTP')
tp_lm <- lm(logTP ~ ws_vbs_High_pct, data=augWQ_fire)
summary(tp_lm)

test_seg <- segmented(tp_lm, 
                      seg.Z = ~ ws_vbs_High_pct, 
                      psi = list(ws_vbs_High_pct = c(10, 30))) #tried 20, 25 as endpoints also
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)


## Sep TP
plot(logTP ~ ws_vbs_High_pct, data=sepWQ_fire, pch=16, main='September TP',
     xlab='Watershed % high severity burn', ylab='logTP')
tp_lm <- lm(logTP ~ ws_vbs_High_pct, data=sepWQ_fire)
summary(tp_lm)

test_seg <- segmented(tp_lm, 
                      seg.Z = ~ ws_vbs_High_pct, 
                      psi = list(ws_vbs_High_pct = c(10, 20))) #tried 20, 25 as endpoints also
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

### same analysis for TN
## May TN
plot(logTN ~ ws_vbs_High_pct, data=mayWQ_fire, pch=16, main='May TN',
     xlab='Watershed % high severity burn', ylab='logTN')
tp_lm <- lm(logTN ~ ws_vbs_High_pct, data=mayWQ_fire)
summary(tp_lm)

test_seg <- segmented(tp_lm, 
                      seg.Z = ~ ws_vbs_High_pct, 
                      psi = list(ws_vbs_High_pct = c(10, 30)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)


## Jun TN
plot(logTN ~ ws_vbs_High_pct, data=juneWQ_fire, pch=16, main='June TN',
     xlab='Watershed % high severity burn', ylab='logTN')
tp_lm <- lm(logTN ~ ws_vbs_High_pct, data=juneWQ_fire)
summary(tp_lm)

test_seg <- segmented(tp_lm, 
                      seg.Z = ~ ws_vbs_High_pct, 
                      psi = list(ws_vbs_High_pct = c(10, 30)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Jul TN
plot(logTN ~ ws_vbs_High_pct, data=julyWQ_fire, pch=16, main='July TN',
     xlab='Watershed % high severity burn', ylab='logTN')
tp_lm <- lm(logTN ~ ws_vbs_High_pct, data=julyWQ_fire)
summary(tp_lm)

test_seg <- segmented(tp_lm, 
                      seg.Z = ~ ws_vbs_High_pct, 
                      psi = list(ws_vbs_High_pct = c(10, 20)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Aug TN: warning, no breakpoint estimated
plot(logTN ~ ws_vbs_High_pct, data=augWQ_fire, pch=16, main='August TN',
     xlab='Watershed % high severity burn', ylab='logTN')
tp_lm <- lm(logTN ~ ws_vbs_High_pct, data=augWQ_fire)
summary(tp_lm)

test_seg <- segmented(tp_lm, 
                      seg.Z = ~ ws_vbs_High_pct, 
                      psi = list(ws_vbs_High_pct = c(10, 20))) #tried 20, 25 as endpoints also
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)


## Sep TN
plot(logTN ~ ws_vbs_High_pct, data=sepWQ_fire, pch=16, main='September TN',
     xlab='Watershed % high severity burn', ylab='logTN')
tp_lm <- lm(logTN ~ ws_vbs_High_pct, data=sepWQ_fire)
summary(tp_lm)

test_seg <- segmented(tp_lm, 
                      seg.Z = ~ ws_vbs_High_pct, 
                      psi = list(ws_vbs_High_pct = c(5, 20))) #tried 20, 25 as endpoints also
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

### limitation of high severity gradient is loss of Fourth McDougal after June
# due to accessibility, so perhaps total % watershed burned is better for this
## May TP
plot(logTP ~ ws_vbs_total_burn_pct, data=mayWQ_fire, pch=16, main='May TP',
     xlab='Watershed % total burn', ylab='logTP')
tp_lm <- lm(logTP ~ ws_vbs_total_burn_pct, data=mayWQ_fire)
summary(tp_lm)

test_seg <- segmented(tp_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)


## Jun TP
plot(logTP ~ ws_vbs_total_burn_pct, data=juneWQ_fire, pch=16, main='June TP',
     xlab='Watershed % total burn', ylab='logTP')
tp_lm <- lm(logTP ~ ws_vbs_total_burn_pct, data=juneWQ_fire)
summary(tp_lm)

test_seg <- segmented(tp_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Jul TP (Warning: no breakpoint estimated)
plot(logTP ~ ws_vbs_total_burn_pct, data=julyWQ_fire, pch=16, main='July TP',
     xlab='Watershed % total burn', ylab='logTP')
tp_lm <- lm(logTP ~ ws_vbs_total_burn_pct, data=julyWQ_fire)
summary(tp_lm)

test_seg <- segmented(tp_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Aug TP: warning, no breakpoint estimated
plot(logTP ~ ws_vbs_total_burn_pct, data=augWQ_fire, pch=16, main='August TP',
     xlab='Watershed % total burn', ylab='logTP')
tp_lm <- lm(logTP ~ ws_vbs_total_burn_pct, data=augWQ_fire)
summary(tp_lm)

test_seg <- segmented(tp_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60))) 
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Sep TP
plot(logTP ~ ws_vbs_total_burn_pct, data=sepWQ_fire, pch=16, main='September TP',
     xlab='Watershed % total burn', ylab='logTP')
tp_lm <- lm(logTP ~ ws_vbs_total_burn_pct, data=sepWQ_fire)
summary(tp_lm)

test_seg <- segmented(tp_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60))) #tried 20, 25 as endpoints also
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)


## May TN
plot(logTN ~ ws_vbs_total_burn_pct, data=mayWQ_fire, pch=16, main='May TN',
     xlab='Watershed % total burn', ylab='logTN')
TN_lm <- lm(logTN ~ ws_vbs_total_burn_pct, data=mayWQ_fire)
summary(TN_lm)

test_seg <- segmented(TN_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Jun TN
plot(logTN ~ ws_vbs_total_burn_pct, data=juneWQ_fire, pch=16, main='June TN',
     xlab='Watershed % total burn', ylab='logTN')
TN_lm <- lm(logTN ~ ws_vbs_total_burn_pct, data=juneWQ_fire)
summary(TN_lm)

test_seg <- segmented(TN_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Jul TN
plot(logTN ~ ws_vbs_total_burn_pct, data=julyWQ_fire, pch=16, main='July TN',
     xlab='Watershed % total burn', ylab='logTN')
TN_lm <- lm(logTN ~ ws_vbs_total_burn_pct, data=julyWQ_fire)
summary(TN_lm)

test_seg <- segmented(TN_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Aug TN: warning, no breakpoint estimated
plot(logTN ~ ws_vbs_total_burn_pct, data=augWQ_fire, pch=16, main='August TN',
     xlab='Watershed % total burn', ylab='logTN')
TN_lm <- lm(logTN ~ ws_vbs_total_burn_pct, data=augWQ_fire)
summary(TN_lm)

test_seg <- segmented(TN_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60))) 
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Sep TN
plot(logTN ~ ws_vbs_total_burn_pct, data=sepWQ_fire, pch=16, main='September TN',
     xlab='Watershed % total burn', ylab='logTN')
TN_lm <- lm(logTN ~ ws_vbs_total_burn_pct, data=sepWQ_fire)
summary(TN_lm)

test_seg <- segmented(TN_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60))) #tried 20, 25 as endpoints also
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

#### chloro: no breakpoints detected outside September for total % ws burned
## May Chloro
plot(logChloro ~ ws_vbs_total_burn_pct, data=mayWQ_fire, pch=16, main='May Chloro',
     xlab='Watershed % total burn', ylab='logChloro')
Chloro_lm <- lm(logChloro ~ ws_vbs_total_burn_pct, data=mayWQ_fire)
summary(Chloro_lm)

test_seg <- segmented(Chloro_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Jun Chloro
plot(logChloro ~ ws_vbs_total_burn_pct, data=juneWQ_fire, pch=16, main='June Chloro',
     xlab='Watershed % total burn', ylab='logChloro')
Chloro_lm <- lm(logChloro ~ ws_vbs_total_burn_pct, data=juneWQ_fire)
summary(Chloro_lm)

test_seg <- segmented(Chloro_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Jul Chloro
plot(logChloro ~ ws_vbs_total_burn_pct, data=julyWQ_fire, pch=16, main='July Chloro',
     xlab='Watershed % total burn', ylab='logChloro')
Chloro_lm <- lm(logChloro ~ ws_vbs_total_burn_pct, data=julyWQ_fire)
summary(Chloro_lm)

test_seg <- segmented(Chloro_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Aug Chloro: warning, no breakpoint estimated
plot(logChloro ~ ws_vbs_total_burn_pct, data=augWQ_fire, pch=16, main='August Chloro',
     xlab='Watershed % total burn', ylab='logChloro')
Chloro_lm <- lm(logChloro ~ ws_vbs_total_burn_pct, data=augWQ_fire)
summary(Chloro_lm)

test_seg <- segmented(Chloro_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60))) 
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Sep Chloro
plot(logChloro ~ ws_vbs_total_burn_pct, data=sepWQ_fire, pch=16, main='September Chloro',
     xlab='Watershed % total burn', ylab='logChloro')
Chloro_lm <- lm(logChloro ~ ws_vbs_total_burn_pct, data=sepWQ_fire)
summary(Chloro_lm)

test_seg <- segmented(Chloro_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60))) #tried 20, 25 as endpoints also
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

### trying with DOC
## May DOC
plot(logDOC ~ ws_vbs_total_burn_pct, data=mayWQ_fire, pch=16, main='May DOC',
     xlab='Watershed % total burn', ylab='logDOC')
DOC_lm <- lm(logDOC ~ ws_vbs_total_burn_pct, data=mayWQ_fire)
summary(DOC_lm)

test_seg <- segmented(DOC_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Jun DOC
plot(logDOC ~ ws_vbs_total_burn_pct, data=juneWQ_fire, pch=16, main='June DOC',
     xlab='Watershed % total burn', ylab='logDOC')
DOC_lm <- lm(logDOC ~ ws_vbs_total_burn_pct, data=juneWQ_fire)
summary(DOC_lm)

test_seg <- segmented(DOC_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Jul DOC
plot(logDOC ~ ws_vbs_total_burn_pct, data=julyWQ_fire, pch=16, main='July DOC',
     xlab='Watershed % total burn', ylab='logDOC')
DOC_lm <- lm(logDOC ~ ws_vbs_total_burn_pct, data=julyWQ_fire)
summary(DOC_lm)

test_seg <- segmented(DOC_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60)))
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Aug DOC: warning, no breakpoint estimated
plot(logDOC ~ ws_vbs_total_burn_pct, data=augWQ_fire, pch=16, main='August DOC',
     xlab='Watershed % total burn', ylab='logDOC')
DOC_lm <- lm(logDOC ~ ws_vbs_total_burn_pct, data=augWQ_fire)
summary(DOC_lm)

test_seg <- segmented(DOC_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60))) 
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)

## Sep DOC
plot(logDOC ~ ws_vbs_total_burn_pct, data=sepWQ_fire, pch=16, main='September DOC',
     xlab='Watershed % total burn', ylab='logDOC')
DOC_lm <- lm(logDOC ~ ws_vbs_total_burn_pct, data=sepWQ_fire)
summary(DOC_lm)

test_seg <- segmented(DOC_lm, 
                      seg.Z = ~ ws_vbs_total_burn_pct, 
                      psi = list(ws_vbs_total_burn_pct = c(5, 60))) #tried 20, 25 as endpoints also
summary(test_seg)
test_seg$psi

plot.segmented(test_seg, add=T)
points.segmented(test_seg, col='firebrick', pch=16)
seg_lines <- test_seg$psi[,2]
abline(v=seg_lines[1], lty=2, xpd=F)
abline(v=seg_lines[2], lty=2, xpd=F)


#### trying generalized additive models ####
library(mgcv)
# bring in LAGOS variables
LAGOS <- read.csv("Data/LAGOS/LAGOS_LOCUS_GEO_DEPTH_combined.csv")

mayWQ_fire <- merge(mayWQ_fire, LAGOS, by='lagoslakeid')
juneWQ_fire <- merge(juneWQ_fire, LAGOS, by='lagoslakeid')
julyWQ_fire <- merge(julyWQ_fire, LAGOS, by='lagoslakeid')
augWQ_fire <- merge(augWQ_fire, LAGOS, by='lagoslakeid')
sepWQ_fire <- merge(sepWQ_fire, LAGOS, by='lagoslakeid')

plot(logTP ~ ws_vbs_High_pct, data=mayWQ_fire, pch=16)
tp_gam <- gam(logTP ~ ws_vbs_High_pct, data=mayWQ_fire,
              main='May TP')# this is still the basic linear model
summary(tp_gam)

tp_gam <- gam(logTP ~ s(ws_vbs_High_pct, bs='cr'), data=mayWQ_fire)
summary(tp_gam)
plot(tp_gam, main='May TP')

tp_gam2 <- gam(logTP ~ s(ws_vbs_High_pct) + s(ws_lake_arearatio), data=mayWQ_fire)
summary(tp_gam2)
plot(tp_gam2, main='May TP')



