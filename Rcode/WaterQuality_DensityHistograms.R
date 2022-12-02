################## Water quality density plots/histograms #####################
# Date: 10-26-22
# updated: 12-2-22; supplemental plots across variables 
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(dplyr)
library(gridExtra)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")

# water quality
waterquality <- read.csv("Data/WaterQuality/combined_lab_field_may_sep.csv")

#### Main program ####
# with help from: http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization
# Density plots seem to be a clean way to convey burned vs. control differences
# Can add histograms to density plots, but seems to make plot busier without changing the underlying message

# Start with all months lumped together

## TP
ggplot(waterquality, aes(x=TP_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Total phosphorus (ppb)')+
  ylab('Density')+
  scale_x_continuous(limits=c())+
  scale_fill_manual(values=c('firebrick','dodgerblue'))

ggplot(waterquality, aes(x=TP_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Total phosphorus (ppb)')+
  ylab('Density')+
  scale_x_continuous(limits=c(0,50))+
  scale_fill_manual(values=c('firebrick','dodgerblue'))

# pretty messy when you add all 4 groups 
ggplot(waterquality, aes(x=TP_ppb, fill=Group)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Total phosphorus (ppb)')+
  ylab('Density')+
  scale_x_continuous(limits=c(0,50))+
  scale_fill_manual(values=c('firebrick','gray10','white','orange'))

## TN
ggplot(waterquality, aes(x=TN_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Total nitrogen (ppb)')+
  ylab('Density')+
  scale_x_continuous(limits=c())+
  scale_fill_manual(values=c('firebrick','dodgerblue'))

# NO2NO3
ggplot(waterquality, aes(x=NO2NO3_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('NO2NO3_ppb (ppb)')+
  ylab('Density')+
  scale_x_continuous(limits=c())+
  scale_fill_manual(values=c('firebrick','dodgerblue'))

#NH4
ggplot(waterquality, aes(x=NH4N_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('NH4 (ppb)')+
  ylab('Density')+
  scale_x_continuous(limits=c(0,100))+ #maybe some outliers?
  scale_fill_manual(values=c('firebrick','dodgerblue'))

## DOC
ggplot(waterquality, aes(x=DOC_ppm, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Dissolved organic carbon (ppm)')+
  ylab('Density')+
  scale_x_continuous(limits=c())+
  scale_fill_manual(values=c('firebrick','dodgerblue'))

## TSS
ggplot(waterquality, aes(x=TSS_mgL, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Total suspended solids (mgL)')+
  ylab('Density')+
  scale_x_continuous(limits=c())+
  scale_fill_manual(values=c('firebrick','dodgerblue'))

## Secchi
ggplot(waterquality, aes(x=SecchiDepth_m, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Secchi (m)')+
  ylab('Density')+
  scale_x_continuous(limits=c())+
  scale_fill_manual(values=c('firebrick','dodgerblue'))

## Chlorophyll-a
ggplot(waterquality, aes(x=Chloro_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Chlorophyll-a (ppb)')+
  ylab('Density')+
  scale_x_continuous(limits=c())+
  scale_fill_manual(values=c('firebrick','dodgerblue'))

## pH
ggplot(waterquality, aes(x=pH, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('pH')+
  ylab('Density')+
  scale_x_continuous(limits=c())+
  scale_fill_manual(values=c('firebrick','dodgerblue'))

## ANC
ggplot(waterquality, aes(x=ANC_mgCaCO3L, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('ANC (mgCaCO3/L)')+
  ylab('Density')+
  scale_x_continuous(limits=c())+
  scale_fill_manual(values=c('firebrick','dodgerblue'))

## Surface temp
ggplot(waterquality, aes(x=WaterTemp_C, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Surface temperature (C)')+
  ylab('Density')+
  scale_x_continuous(limits=c())+
  scale_fill_manual(values=c('firebrick','dodgerblue'))

## DO
ggplot(waterquality, aes(x=LDO_pct, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Surface dissolved oxygen (%)')+
  ylab('Density')+
  scale_x_continuous(limits=c())+
  scale_fill_manual(values=c('firebrick','dodgerblue'))

ggplot(waterquality, aes(x=LDO_mgL, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Surface dissolved oxygen (mg/L)')+
  ylab('Density')+
  scale_x_continuous(limits=c(0,15))+
  scale_fill_manual(values=c('firebrick','dodgerblue'))

###### treat months separately #####
mayWQ <- subset(waterquality, Month_factor=='May')
junWQ <- subset(waterquality, Month_factor=='Jun')
julWQ <- subset(waterquality, Month_factor=='Jul')
augWQ <- subset(waterquality, Month_factor=='Aug')
sepWQ <- subset(waterquality, Month_factor=='Sep')

## TP
mayTP_plot <- ggplot(mayWQ, aes(x=TP_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Total phosphorus (ppb)')+
  ylab('Density')+
  ggtitle('May 2022')+
  scale_x_continuous(limits=c(0,60))+
  scale_y_continuous(limits=c(0,0.08))+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
mayTP_plot

junTP_plot <- ggplot(junWQ, aes(x=TP_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Total phosphorus (ppb)')+
  ylab('Density')+
  ggtitle('Jun 2022')+
  scale_x_continuous(limits=c(0,60))+
  scale_y_continuous(limits=c(0,0.08))+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
junTP_plot

julTP_plot <- ggplot(julWQ, aes(x=TP_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Total phosphorus (ppb)')+
  ylab('Density')+
  ggtitle('Jul 2022')+
  scale_x_continuous(limits=c(0,60))+
  scale_y_continuous(limits=c(0,0.08))+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
julTP_plot

augTP_plot <- ggplot(augWQ, aes(x=TP_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Total phosphorus (ppb)')+
  ylab('Density')+
  ggtitle('Aug 2022')+
  scale_x_continuous(limits=c(0,60))+
  scale_y_continuous(limits=c(0,0.08))+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
augTP_plot

sepTP_plot <- ggplot(sepWQ, aes(x=TP_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Total phosphorus (ppb)')+
  ylab('Density')+
  ggtitle('Sep 2022')+
  scale_x_continuous(limits=c(0,60))+
  scale_y_continuous(limits=c(0,0.08))+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
sepTP_plot

allmonthsTP_plot  <- ggplot(waterquality, aes(x=TP_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Total phosphorus (ppb)')+
  ylab('Density')+
  ggtitle('All months 2022')+
  scale_x_continuous(limits=c(0,60))+
  scale_y_continuous(limits=c(0,0.08))+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsTP_plot

grid.arrange(mayTP_plot, junTP_plot, julTP_plot,
             augTP_plot, sepTP_plot, allmonthsTP_plot, nrow=2)

# jpeg('Figures/supplemental_density_plots/TP_density_plot.jpeg',width = 8,height = 6,units = 'in',res=600)
#     grid.arrange(mayTP_plot, junTP_plot, julTP_plot,
#                  augTP_plot, sepTP_plot, allmonthsTP_plot, nrow=2)
# dev.off()

## TN
xlimitz <- c(0,2000)
ylimitz <- c(0,0.003)
mayTN_plot <- ggplot(mayWQ, aes(x=TN_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Total nitrogen (ppb)')+
  ylab('Density')+
  ggtitle('May 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
mayTN_plot

junTN_plot <- ggplot(junWQ, aes(x=TN_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Total nitrogen (ppb)')+
  ylab('Density')+
  ggtitle('Jun 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
junTN_plot

julTN_plot <- ggplot(julWQ, aes(x=TN_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Total nitrogen (ppb)')+
  ylab('Density')+
  ggtitle('Jul 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
julTN_plot

augTN_plot <- ggplot(augWQ, aes(x=TN_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Total nitrogen (ppb)')+
  ylab('Density')+
  ggtitle('Aug 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
augTN_plot

sepTN_plot <- ggplot(sepWQ, aes(x=TN_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Total nitrogen (ppb)')+
  ylab('Density')+
  ggtitle('Sep 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
sepTN_plot

allmonthsTN_plot  <- ggplot(waterquality, aes(x=TN_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Total nitrogen (ppb)')+
  ylab('Density')+
  ggtitle('All months 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsTN_plot

grid.arrange(mayTN_plot, junTN_plot, julTN_plot,
             augTN_plot, sepTN_plot, allmonthsTN_plot, nrow=2)

# jpeg('Figures/supplemental_density_plots/TN_density_plot.jpeg',width = 8,height = 6,units = 'in',res=600)
#      grid.arrange(mayTN_plot, junTN_plot, julTN_plot,
#                   augTN_plot, sepTN_plot, allmonthsTN_plot, nrow=2)
# dev.off()

## TN/TP
xlimitz <- c(0,125)
ylimitz <- c(0,0.06)
mayTNTP_plot <- ggplot(mayWQ, aes(x=TNTP, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('TN/TP')+
  ylab('Density')+
  ggtitle('May 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
mayTNTP_plot

junTNTP_plot <- ggplot(junWQ, aes(x=TNTP, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('TN/TP')+
  ylab('Density')+
  ggtitle('Jun 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
junTNTP_plot

julTNTP_plot <- ggplot(julWQ, aes(x=TNTP, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('TN/TP')+
  ylab('Density')+
  ggtitle('Jul 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
julTNTP_plot

augTNTP_plot <- ggplot(augWQ, aes(x=TNTP, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('TN/TP')+
  ylab('Density')+
  ggtitle('Aug 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
augTNTP_plot

sepTNTP_plot <- ggplot(sepWQ, aes(x=TNTP, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('TN/TP')+
  ylab('Density')+
  ggtitle('Sep 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
sepTNTP_plot

allmonthsTNTP_plot  <- ggplot(waterquality, aes(x=TNTP, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('TN/TP')+
  ylab('Density')+
  ggtitle('All months 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsTNTP_plot

grid.arrange(mayTNTP_plot, junTNTP_plot, julTNTP_plot,
             augTNTP_plot, sepTNTP_plot, allmonthsTNTP_plot, nrow=2)

## DOC
xlimitz <- c(0,50)
ylimitz <- c(0,0.1)
mayDOC_plot <- ggplot(mayWQ, aes(x=DOC_ppm, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Dissolved organic carbon (ppm)')+
  ylab('Density')+
  ggtitle('May 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
mayDOC_plot

junDOC_plot <- ggplot(junWQ, aes(x=DOC_ppm, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Dissolved organic carbon (ppm)')+
  ylab('Density')+
  ggtitle('Jun 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
junDOC_plot

julDOC_plot <- ggplot(julWQ, aes(x=DOC_ppm, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Dissolved organic carbon (ppm)')+
  ylab('Density')+
  ggtitle('Jul 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
julDOC_plot

augDOC_plot <- ggplot(augWQ, aes(x=DOC_ppm, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Dissolved organic carbon (ppm)')+
  ylab('Density')+
  ggtitle('Aug 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
augDOC_plot

sepDOC_plot <- ggplot(sepWQ, aes(x=DOC_ppm, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Dissolved organic carbon (ppm)')+
  ylab('Density')+
  ggtitle('Sep 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
sepDOC_plot

allmonthsDOC_plot  <- ggplot(waterquality, aes(x=DOC_ppm, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Dissolved organic carbon (ppm)')+
  ylab('Density')+
  ggtitle('All months 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsDOC_plot

grid.arrange(mayDOC_plot, junDOC_plot, julDOC_plot,
             augDOC_plot, sepDOC_plot, allmonthsDOC_plot, nrow=2)

jpeg('Figures/supplemental_density_plots/DOC_density_plot.jpeg',width = 8,height = 6,units = 'in',res=600)
       grid.arrange(mayDOC_plot, junDOC_plot, julDOC_plot,
                    augDOC_plot, sepDOC_plot, allmonthsDOC_plot, nrow=2)
dev.off()

## Chlorophyll-a
xlimitz <- c(0,25)
ylimitz <- c(0,0.3)
mayChla_plot <- ggplot(mayWQ, aes(x=Chloro_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Chlorophyll-a (ppb)')+
  ylab('Density')+
  ggtitle('May 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
mayChla_plot

junChla_plot <- ggplot(junWQ, aes(x=Chloro_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Chlorophyll-a (ppb)')+
  ylab('Density')+
  ggtitle('Jun 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
junChla_plot

julChla_plot <- ggplot(julWQ, aes(x=Chloro_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Chlorophyll-a (ppb)')+
  ylab('Density')+
  ggtitle('Jul 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
julChla_plot

augChla_plot <- ggplot(augWQ, aes(x=Chloro_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Chlorophyll-a (ppb)')+
  ylab('Density')+
  ggtitle('Aug 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
augChla_plot

sepChla_plot <- ggplot(sepWQ, aes(x=Chloro_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Chlorophyll-a (ppb)')+
  ylab('Density')+
  ggtitle('Sep 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
sepChla_plot

allmonthsChla_plot  <- ggplot(waterquality, aes(x=Chloro_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Chlorophyll-a (ppb)')+
  ylab('Density')+
  ggtitle('All months 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsChla_plot

grid.arrange(mayChla_plot, junChla_plot, julChla_plot,
             augChla_plot, sepChla_plot, allmonthsChla_plot, nrow=2)

# jpeg('Figures/supplemental_density_plots/Chla_density_plot.jpeg',width = 8,height = 6,units = 'in',res=600)
#        grid.arrange(mayChla_plot, junChla_plot, julChla_plot,
#                     augChla_plot, sepChla_plot, allmonthsChla_plot, nrow=2)
# dev.off()

## Secchi
xlimitz <- c(0,5)
ylimitz <- c(0,2.2)
maySecchi_plot <- ggplot(mayWQ, aes(x=SecchiDepth_m, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Secchi (m)')+
  ylab('Density')+
  ggtitle('May 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
maySecchi_plot

junSecchi_plot <- ggplot(junWQ, aes(x=SecchiDepth_m, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Secchi (m)')+
  ylab('Density')+
  ggtitle('Jun 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
junSecchi_plot

julSecchi_plot <- ggplot(julWQ, aes(x=SecchiDepth_m, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Secchi (m)')+
  ylab('Density')+
  ggtitle('Jul 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
julSecchi_plot

augSecchi_plot <- ggplot(augWQ, aes(x=SecchiDepth_m, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Secchi (m)')+
  ylab('Density')+
  ggtitle('Aug 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
augSecchi_plot

sepSecchi_plot <- ggplot(sepWQ, aes(x=SecchiDepth_m, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Secchi (m)')+
  ylab('Density')+
  ggtitle('Sep 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
sepSecchi_plot

allmonthsSecchi_plot  <- ggplot(waterquality, aes(x=SecchiDepth_m, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Secchi (m)')+
  ylab('Density')+
  ggtitle('All months 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsSecchi_plot

grid.arrange(maySecchi_plot, junSecchi_plot, julSecchi_plot,
             augSecchi_plot, sepSecchi_plot, allmonthsSecchi_plot, nrow=2)

# jpeg('Figures/supplemental_density_plots/Secchi_density_plot.jpeg',width = 8,height = 6,units = 'in',res=600)
#    grid.arrange(maySecchi_plot, junSecchi_plot, julSecchi_plot,
#               augSecchi_plot, sepSecchi_plot, allmonthsSecchi_plot, nrow=2)
# dev.off()

## TSS
xlimitz <- c(0,18)
ylimitz <- c(0,0.6)
mayTSS_plot <- ggplot(mayWQ, aes(x=TSS_mgL, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Total suspended solids (mg/L)')+
  ylab('Density')+
  ggtitle('May 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
mayTSS_plot

junTSS_plot <- ggplot(junWQ, aes(x=TSS_mgL, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Total suspended solids (mg/L)')+
  ylab('Density')+
  ggtitle('Jun 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
junTSS_plot

julTSS_plot <- ggplot(julWQ, aes(x=TSS_mgL, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Total suspended solids (mg/L)')+
  ylab('Density')+
  ggtitle('Jul 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
julTSS_plot

augTSS_plot <- ggplot(augWQ, aes(x=TSS_mgL, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Total suspended solids (mg/L)')+
  ylab('Density')+
  ggtitle('Aug 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
augTSS_plot

sepTSS_plot <- ggplot(sepWQ, aes(x=TSS_mgL, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Total suspended solids (mg/L)')+
  ylab('Density')+
  ggtitle('Sep 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
sepTSS_plot

allmonthsTSS_plot  <- ggplot(waterquality, aes(x=TSS_mgL, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Total suspended solids (mg/L)')+
  ylab('Density')+
  ggtitle('All months 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsTSS_plot

grid.arrange(mayTSS_plot, junTSS_plot, julTSS_plot,
             augTSS_plot, sepTSS_plot, allmonthsTSS_plot, nrow=2)

# jpeg('Figures/supplemental_density_plots/TSS_density_plot.jpeg',width = 8,height = 6,units = 'in',res=600)
#   grid.arrange(mayTSS_plot, junTSS_plot, julTSS_plot,
#              augTSS_plot, sepTSS_plot, allmonthsTSS_plot, nrow=2)
# dev.off()

## pH
xlimitz <- c(4,10)
ylimitz <- c(0,1.2)
maypH_plot <- ggplot(mayWQ, aes(x=pH, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('pH')+
  ylab('Density')+
  ggtitle('May 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
maypH_plot

junpH_plot <- ggplot(junWQ, aes(x=pH, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('pH')+
  ylab('Density')+
  ggtitle('Jun 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
junpH_plot

julpH_plot <- ggplot(julWQ, aes(x=pH, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('pH')+
  ylab('Density')+
  ggtitle('Jul 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
julpH_plot

augpH_plot <- ggplot(augWQ, aes(x=pH, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('pH')+
  ylab('Density')+
  ggtitle('Aug 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
augpH_plot

seppH_plot <- ggplot(sepWQ, aes(x=pH, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('pH')+
  ylab('Density')+
  ggtitle('Sep 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
seppH_plot

allmonthspH_plot  <- ggplot(waterquality, aes(x=pH, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('pH')+
  ylab('Density')+
  ggtitle('All months 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthspH_plot

grid.arrange(maypH_plot, junpH_plot, julpH_plot,
             augpH_plot, seppH_plot, allmonthspH_plot, nrow=2)

# jpeg('Figures/supplemental_density_plots/pH_density_plot.jpeg',width = 8,height = 6,units = 'in',res=600)
#    grid.arrange(maypH_plot, junpH_plot, julpH_plot,
#                 augpH_plot, seppH_plot, allmonthspH_plot, nrow=2)
# dev.off()


## Surface temp
xlimitz <- c(5,32)
ylimitz <- c(0,0.5)
mayTemp_plot <- ggplot(mayWQ, aes(x=WaterTemp_C, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Surface temperature (°C)')+
  ylab('Density')+
  ggtitle('May 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
mayTemp_plot

junTemp_plot <- ggplot(junWQ, aes(x=WaterTemp_C, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Surface temperature (°C)')+
  ylab('Density')+
  ggtitle('Jun 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
junTemp_plot

julTemp_plot <- ggplot(julWQ, aes(x=WaterTemp_C, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Surface temperature (°C)')+
  ylab('Density')+
  ggtitle('Jul 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
julTemp_plot

augTemp_plot <- ggplot(augWQ, aes(x=WaterTemp_C, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Surface temperature (°C)')+
  ylab('Density')+
  ggtitle('Aug 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
augTemp_plot

sepTemp_plot <- ggplot(sepWQ, aes(x=WaterTemp_C, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Surface temperature (°C)')+
  ylab('Density')+
  ggtitle('Sep 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
sepTemp_plot

allmonthsTemp_plot  <- ggplot(waterquality, aes(x=WaterTemp_C, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Surface temperature (°C)')+
  ylab('Density')+
  ggtitle('All months 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsTemp_plot

grid.arrange(mayTemp_plot, junTemp_plot, julTemp_plot,
             augTemp_plot, sepTemp_plot, allmonthsTemp_plot, nrow=2)

# jpeg('Figures/supplemental_density_plots/SurfTemp_density_plot.jpeg',width = 8,height = 6,units = 'in',res=600)
#     grid.arrange(mayTemp_plot, junTemp_plot, julTemp_plot,
#                  augTemp_plot, sepTemp_plot, allmonthsTemp_plot, nrow=2)
# dev.off()

## NO2NO3
xlimitz <- c(0,150)
ylimitz <- c() #really hard to find common scale
mayNO2NO3_plot <- ggplot(mayWQ, aes(x=NO2NO3_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('NO2NO3 (ppb)')+
  ylab('Density')+
  ggtitle('May 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
mayNO2NO3_plot

junNO2NO3_plot <- ggplot(junWQ, aes(x=NO2NO3_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('NO2NO3 (ppb)')+
  ylab('Density')+
  ggtitle('Jun 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
junNO2NO3_plot

julNO2NO3_plot <- ggplot(julWQ, aes(x=NO2NO3_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('NO2NO3 (ppb)')+
  ylab('Density')+
  ggtitle('Jul 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
julNO2NO3_plot

augNO2NO3_plot <- ggplot(augWQ, aes(x=NO2NO3_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('NO2NO3 (ppb)')+
  ylab('Density')+
  ggtitle('Aug 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
augNO2NO3_plot

sepNO2NO3_plot <- ggplot(sepWQ, aes(x=NO2NO3_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('NO2NO3 (ppb)')+
  ylab('Density')+
  ggtitle('Sep 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
sepNO2NO3_plot

allmonthsNO2NO3_plot  <- ggplot(waterquality, aes(x=NO2NO3_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('NO2NO3 (ppb)')+
  ylab('Density')+
  ggtitle('All months 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsNO2NO3_plot

grid.arrange(mayNO2NO3_plot, junNO2NO3_plot, julNO2NO3_plot,
             augNO2NO3_plot, sepNO2NO3_plot, allmonthsNO2NO3_plot, nrow=2)

# jpeg('Figures/supplemental_density_plots/NO2NO3_density_plot.jpeg',width = 8,height = 6,units = 'in',res=600)
#   grid.arrange(mayNO2NO3_plot, junNO2NO3_plot, julNO2NO3_plot,
#              augNO2NO3_plot, sepNO2NO3_plot, allmonthsNO2NO3_plot, nrow=2)
# dev.off()

## NH4N
xlimitz <- c(0,100)
ylimitz <- c(0,0.15) 
mayNH4N_plot <- ggplot(mayWQ, aes(x=NH4N_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('NH4N (ppb)')+
  ylab('Density')+
  ggtitle('May 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
mayNH4N_plot

junNH4N_plot <- ggplot(junWQ, aes(x=NH4N_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('NH4N (ppb)')+
  ylab('Density')+
  ggtitle('Jun 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
junNH4N_plot

julNH4N_plot <- ggplot(julWQ, aes(x=NH4N_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('NH4N (ppb)')+
  ylab('Density')+
  ggtitle('Jul 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
julNH4N_plot

augNH4N_plot <- ggplot(augWQ, aes(x=NH4N_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('NH4N (ppb)')+
  ylab('Density')+
  ggtitle('Aug 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
augNH4N_plot

sepNH4N_plot <- ggplot(sepWQ, aes(x=NH4N_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('NH4N (ppb)')+
  ylab('Density')+
  ggtitle('Sep 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
sepNH4N_plot

allmonthsNH4N_plot  <- ggplot(waterquality, aes(x=NH4N_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('NH4N (ppb)')+
  ylab('Density')+
  ggtitle('All months 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsNH4N_plot

grid.arrange(mayNH4N_plot, junNH4N_plot, julNH4N_plot,
             augNH4N_plot, sepNH4N_plot, allmonthsNH4N_plot, nrow=2)

# jpeg('Figures/supplemental_density_plots/NH4_density_plot.jpeg',width = 8,height = 6,units = 'in',res=600)
#   grid.arrange(mayNH4N_plot, junNH4N_plot, julNH4N_plot,
#                augNH4N_plot, sepNH4N_plot, allmonthsNH4N_plot, nrow=2)
# dev.off()

## ANC_mgCaCO3L
xlimitz <- c(0,100)
ylimitz <- c(0,0.08) 
mayANC_mgCaCO3L_plot <- ggplot(mayWQ, aes(x=ANC_mgCaCO3L, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('ANC (mgCaCO3/L)')+
  ylab('Density')+
  ggtitle('May 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
mayANC_mgCaCO3L_plot

junANC_mgCaCO3L_plot <- ggplot(junWQ, aes(x=ANC_mgCaCO3L, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('ANC (mgCaCO3/L)')+
  ylab('Density')+
  ggtitle('Jun 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
junANC_mgCaCO3L_plot

julANC_mgCaCO3L_plot <- ggplot(julWQ, aes(x=ANC_mgCaCO3L, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('ANC (mgCaCO3/L)')+
  ylab('Density')+
  ggtitle('Jul 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
julANC_mgCaCO3L_plot

augANC_mgCaCO3L_plot <- ggplot(augWQ, aes(x=ANC_mgCaCO3L, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('ANC (mgCaCO3/L)')+
  ylab('Density')+
  ggtitle('Aug 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
augANC_mgCaCO3L_plot

sepANC_mgCaCO3L_plot <- ggplot(sepWQ, aes(x=ANC_mgCaCO3L, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('ANC (mgCaCO3/L)')+
  ylab('Density')+
  ggtitle('Sep 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
sepANC_mgCaCO3L_plot

allmonthsANC_mgCaCO3L_plot  <- ggplot(waterquality, aes(x=ANC_mgCaCO3L, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('ANC (mgCaCO3/L)')+
  ylab('Density')+
  ggtitle('All months 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsANC_mgCaCO3L_plot

grid.arrange(mayANC_mgCaCO3L_plot, junANC_mgCaCO3L_plot, julANC_mgCaCO3L_plot,
             augANC_mgCaCO3L_plot, sepANC_mgCaCO3L_plot, allmonthsANC_mgCaCO3L_plot, nrow=2)

# jpeg('Figures/supplemental_density_plots/ANC_density_plot.jpeg',width = 8,height = 6,units = 'in',res=600)
#   grid.arrange(mayANC_mgCaCO3L_plot, junANC_mgCaCO3L_plot, julANC_mgCaCO3L_plot,
#              augANC_mgCaCO3L_plot, sepANC_mgCaCO3L_plot, allmonthsANC_mgCaCO3L_plot, nrow=2)
# dev.off()

## DO
xlimitz <- c(0,15)
ylimitz <- c(0,1) 
mayDO_plot <- ggplot(mayWQ, aes(x=LDO_mgL, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.25,0.8))+
  xlab('Dissolved oxygen (mg/L)')+
  ylab('Density')+
  ggtitle('May 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
mayDO_plot

junDO_plot <- ggplot(junWQ, aes(x=LDO_mgL, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Dissolved oxygen (mg/L)')+
  ylab('Density')+
  ggtitle('Jun 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
junDO_plot

julDO_plot <- ggplot(julWQ, aes(x=LDO_mgL, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Dissolved oxygen (mg/L)')+
  ylab('Density')+
  ggtitle('Jul 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
julDO_plot

augDO_plot <- ggplot(augWQ, aes(x=LDO_mgL, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Dissolved oxygen (mg/L)')+
  ylab('Density')+
  ggtitle('Aug 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
augDO_plot

sepDO_plot <- ggplot(sepWQ, aes(x=LDO_mgL, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Dissolved oxygen (mg/L)')+
  ylab('Density')+
  ggtitle('Sep 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
sepDO_plot

allmonthsDO_plot  <- ggplot(waterquality, aes(x=LDO_mgL, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Dissolved oxygen (mg/L)')+
  ylab('Density')+
  ggtitle('All months 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsDO_plot

grid.arrange(mayDO_plot, junDO_plot, julDO_plot,
             augDO_plot, sepDO_plot, allmonthsDO_plot, nrow=2)

#jpeg('Figures/supplemental_density_plots/DO_density_plot.jpeg',width = 8,height = 6,units = 'in',res=600)
#  grid.arrange(mayDO_plot, junDO_plot, julDO_plot,
#             augDO_plot, sepDO_plot, allmonthsDO_plot, nrow=2)
#dev.off()

## Spec conductivity
xlimitz <- c(0,250)
ylimitz <- c(0,0.03) 
maySpecCond_plot <- ggplot(mayWQ, aes(x=SpecCond_uScm, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8))+
  xlab('Specific conductivity (μS/cm)')+
  ylab('Density')+
  ggtitle('May 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
maySpecCond_plot

junSpecCond_plot <- ggplot(junWQ, aes(x=SpecCond_uScm, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Specific conductivity (μS/cm)')+
  ylab('Density')+
  ggtitle('Jun 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
junSpecCond_plot

julSpecCond_plot <- ggplot(julWQ, aes(x=SpecCond_uScm, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Specific conductivity (μS/cm)')+
  ylab('Density')+
  ggtitle('Jul 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
julSpecCond_plot

augSpecCond_plot <- ggplot(augWQ, aes(x=SpecCond_uScm, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Specific conductivity (μS/cm)')+
  ylab('Density')+
  ggtitle('Aug 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
augSpecCond_plot

sepSpecCond_plot <- ggplot(sepWQ, aes(x=SpecCond_uScm, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Specific conductivity (μS/cm)')+
  ylab('Density')+
  ggtitle('Sep 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
sepSpecCond_plot

allmonthsSpecCond_plot  <- ggplot(waterquality, aes(x=SpecCond_uScm, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'))+
  xlab('Specific conductivity (μS/cm)')+
  ylab('Density')+
  ggtitle('All months 2022')+
  scale_x_continuous(limits=xlimitz)+
  scale_y_continuous(limits=ylimitz)+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsSpecCond_plot

grid.arrange(maySpecCond_plot, junSpecCond_plot, julSpecCond_plot,
             augSpecCond_plot, sepSpecCond_plot, allmonthsSpecCond_plot, nrow=2)

# jpeg('Figures/supplemental_density_plots/SpecCond_density_plot.jpeg',width = 8,height = 6,units = 'in',res=600)
#   grid.arrange(maySpecCond_plot, junSpecCond_plot, julSpecCond_plot,
#              augSpecCond_plot, sepSpecCond_plot, allmonthsSpecCond_plot, nrow=2)
# dev.off()

##### manuscript-worthy integrative figure (Candidate, at least) ########
allmonthsTP_plot  <- ggplot(waterquality, aes(x=TP_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c(0.8,0.8),
        plot.title=element_text(size=11))+
  xlab('Total phosphorus (ppb)')+
  ylab('Density')+
  ggtitle('A) Total phosphorus')+
  scale_x_continuous(limits=c(0,60))+
  scale_y_continuous(limits=c(0,0.08))+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsTP_plot

allmonthsTN_plot  <- ggplot(waterquality, aes(x=TN_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'),
        plot.title=element_text(size=11))+
  xlab('Total nitrogen (ppb)')+
  ylab('Density')+
  ggtitle('B) Total nitrogen')+
  scale_x_continuous(limits=c(0,2000))+
  scale_y_continuous(limits=c(0,0.003))+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsTN_plot

allmonthsDOC_plot  <- ggplot(waterquality, aes(x=DOC_ppm, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'),
        plot.title=element_text(size=11))+
  xlab('Dissolved organic carbon (ppm)')+
  ylab('Density')+
  ggtitle('C) Dissolved organic carbon')+
  scale_x_continuous(limits=c(0,50))+
  scale_y_continuous(limits=c(0,0.08))+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsDOC_plot

allmonthsTSS_plot  <- ggplot(waterquality, aes(x=TSS_mgL, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'),
        plot.title=element_text(size=11))+
  xlab('Total suspended solids (mg/L)')+
  ylab('Density')+
  ggtitle('D) Total suspended solids')+
  scale_x_continuous(limits=c(0,15))+
  scale_y_continuous(limits=c(0,0.5))+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsTSS_plot

allmonthsChla_plot  <- ggplot(waterquality, aes(x=Chloro_ppb, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'),
        plot.title=element_text(size=11))+
  xlab('Chlorophyll-a (ppb)')+
  ylab('Density')+
  ggtitle('E) Chlorophyll-a')+
  scale_x_continuous(limits=c(0,25))+
  scale_y_continuous(limits=c(0,0.25))+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsChla_plot

allmonthsSecchi_plot  <- ggplot(waterquality, aes(x=SecchiDepth_m, fill=Type)) + 
  geom_density(alpha=0.8, lwd=0.8)+
  theme_classic()+
  theme(axis.text.x=element_text(color='black'),
        axis.text.y=element_text(color='black'),
        legend.position=c('none'),
        plot.title=element_text(size=11))+
  xlab('Secchi (m)')+
  ylab('Density')+
  ggtitle('F) Secchi')+
  scale_x_continuous(limits=c(0,5))+
  scale_y_continuous(limits=c(0,1))+
  scale_fill_manual(values=c('firebrick','dodgerblue'))
allmonthsSecchi_plot

grid.arrange(allmonthsTP_plot, allmonthsTN_plot, allmonthsDOC_plot,
             allmonthsTSS_plot, allmonthsChla_plot, allmonthsSecchi_plot, nrow=2)

jpeg('Figures/multipanel_densityplots_allmonths_6var.jpeg',width = 8,height = 6,units = 'in',res=600)
  grid.arrange(allmonthsTP_plot, allmonthsTN_plot, allmonthsDOC_plot,
             allmonthsTSS_plot, allmonthsChla_plot, allmonthsSecchi_plot, nrow=2)
dev.off()
