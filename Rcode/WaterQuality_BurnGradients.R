####################### Water quality/fire gradient analysis ##################
# Date: 10-25-22
# updated: 
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(ggplot2)

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