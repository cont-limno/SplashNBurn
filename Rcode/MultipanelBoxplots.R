############ Boxplots to show seasonal dynamics in fire response ##############
# Date: 2-8-23
# updated: 
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(ggplot2)
library(gridExtra)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")

# water quality
waterquality <- read.csv("Data/WaterQuality/combined_lab_field_may_sep.csv") #compiled table

#### Main program ####
## Can we cram multiple grouped boxed plots into multi-panel plot?
waterquality$Month_factor <- as.factor(waterquality$Month_factor)
waterquality$Month_factor <- factor(waterquality$Month_factor,
                                    levels=c("May","Jun","Jul","Aug","Sep"))

waterquality$GroupFac <- as.factor(waterquality$Group)
waterquality$GroupFac <- factor(waterquality$GroupFac,
                                levels=c("Burned_Drainage","Control_Drainage",
                                         "Burned_Isolated","Control_Isolated"),
                                labels=c("Burned_Drainage","Control_Drainage",
                                "Burned_Isolated","Control_Isolated"))

title_font <- 10
month_colors <- c('dodgerblue','tan','gold','orange','darkgreen')

TP_box <- ggplot(waterquality, aes(x = GroupFac, y = TP_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "", y = "Total phosphorus (ppb)")+
  scale_y_continuous(limits=c(0,50))+ #0,50
  scale_fill_manual("Month", values=month_colors)+
  ggtitle("A) TP")+
  theme(legend.position=c('none'),
        plot.title=element_text(size=title_font),
        axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black', size=8),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),
        legend.background=element_rect(color = 'black', fill = 'white', linetype='solid'))+
  scale_x_discrete(labels=c('BD','CD','BI','CI'))

TN_box <- ggplot(waterquality, aes(x = GroupFac, y = TN_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "", y = "Total nitrogen (ppb)")+
  scale_y_continuous(limits=c())+
  ggtitle("B) TN")+
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c('none'),
        plot.title=element_text(size=title_font),
        axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black', size=8),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  scale_x_discrete(labels=c('BD','CD','BI','CI'))

DOC_box <- ggplot(waterquality, aes(x = GroupFac, y = DOC_ppm, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  labs(x = "", y = "Dissolved organic carbon (ppm)")+ #0,40
  scale_y_continuous(limits=c())+
  ggtitle("C) DOC")+
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c(0.85,0.8), #0.85,0.7
        plot.title=element_text(size=title_font),
        axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black', size=8),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),
        legend.title=element_blank(),
        legend.key.size=unit(0.4, "cm"),
        legend.margin=margin(c(0.25,0.25,0.25,0.25)))+
        #legend.background=element_rect(color = 'black', fill = 'white', linetype='solid'))+
  scale_x_discrete(labels=c('BD','CD','BI','CI'))

TSS_box <- ggplot(waterquality, aes(x = GroupFac, y = TSS_mgL, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c(0,10))+
  ggtitle("E) TSS")+
  labs(x = "", y = "Total suspended solids (mg/L)")+
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c('none'),
        plot.title=element_text(size=title_font),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),
        axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black', size=8))+
  scale_x_discrete(labels=c('BD','CD','BI','CI'))

chla_box <- ggplot(waterquality, aes(x = GroupFac, y = Chloro_ppb, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c())+
  ggtitle("F) Chlorophyll-a")+
  labs(x = "", y = "Chlorophyll-a (ppb)")+
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c('none'),
        plot.title=element_text(size=title_font),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),
        axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black', size=8))+
  scale_x_discrete(labels=c('BD','CD','BI','CI'))

secchi_box <- ggplot(waterquality, aes(x = GroupFac, y = SecchiDepth_m, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c())+
  ggtitle("G) Secchi")+
  labs(x = "", y = "Secchi (m)")+
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c('none'),
        plot.title=element_text(size=title_font),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),
        axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black', size=8))+
  scale_x_discrete(labels=c('BD','CD','BI','CI'))

pH_box <- ggplot(waterquality, aes(x = GroupFac, y = pH, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c())+
  ggtitle("D) pH")+
  labs(x = "", y = "pH")+
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c('none'),
        plot.title=element_text(size=title_font),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),
        axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black', size=8))+
  scale_x_discrete(labels=c('BD','CD','BI','CI'))

WaterTemp_box <- ggplot(waterquality, aes(x = GroupFac, y = WaterTemp_C, fill = Month_factor)) +
  geom_boxplot() + 
  theme_classic() +
  scale_y_continuous(limits=c())+
  ggtitle("H) Temperature")+
  labs(x = "", y = "Temperature (°C)")+
  scale_fill_manual("Month", values=month_colors)+
  theme(legend.position=c('none'),
        plot.title=element_text(size=title_font),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8),
        axis.text.x=element_text(color='black', size=8),
        axis.text.y=element_text(color='black', size=8))+
  scale_x_discrete(labels=c('BD','CD','BI','CI'))

grid.arrange(TP_box, TN_box, DOC_box, pH_box,
             TSS_box, chla_box, secchi_box, WaterTemp_box, nrow=2)

jpeg('Figures/multipanel_month_boxplots.jpeg',width = 8,height = 6,units = 'in',res=600)
  grid.arrange(TP_box, TN_box, DOC_box, pH_box,
               TSS_box, chla_box, secchi_box, WaterTemp_box, nrow=2)
dev.off()
