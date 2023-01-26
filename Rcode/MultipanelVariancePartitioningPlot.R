############## Multipanel variance partitioning results plot ##################
# Date: 1-26-23
# updated: 
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(ggplot2)
library(gridExtra)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")
pct_ws_burn <- read.csv("C:/Users/immcc/Documents/SplashNBurn/Data/VariancePartitioning/pct_ws_burned.csv")
pct_buff_burn <- read.csv("C:/Users/immcc/Documents/SplashNBurn/Data/VariancePartitioning/pct_buffer_burned.csv")
pct_ws_burn_HSveg <- read.csv("C:/Users/immcc/Documents/SplashNBurn/Data/VariancePartitioning/pct_ws_burned_HSveg.csv")
pct_ws_burn_HSsoil <- read.csv("C:/Users/immcc/Documents/SplashNBurn/Data/VariancePartitioning/pct_ws_burned_HSsoil.csv")
pct_buff_burn_HSveg <- read.csv("C:/Users/immcc/Documents/SplashNBurn/Data/VariancePartitioning/pct_buffer_burned_HSveg.csv")
pct_buff_burn_HSsoil <- read.csv("C:/Users/immcc/Documents/SplashNBurn/Data/VariancePartitioning/pct_buffer_burned_HSsoil.csv")

# unfortunately when data were exported, factors were lost
pct_ws_burn <- as.data.frame(unclass(pct_ws_burn),stringsAsFactors = T)
pct_ws_burn$predictor <- factor(pct_ws_burn$predictor, levels=c('ws_vbs_total_burn_pct','ConnClass','ws_lake_arearatio','lake_totalarea_ha','mean_depth','Month','Random: lake','ws_area_ha'))
pct_ws_burn$variable <- factor(pct_ws_burn$variable, levels=c('logChloro','logDOC','logTN','logTP','logTSS','pH','SecchiDepth_m','WaterTemp_C'))

pct_buff_burn <- as.data.frame(unclass(pct_buff_burn),stringsAsFactors = T)
pct_buff_burn$predictor <- factor(pct_buff_burn$predictor, levels=c('buff100m_vbs_total_burn_pct','ConnClass','ws_lake_arearatio','lake_totalarea_ha','mean_depth','Month','Random: lake','ws_area_ha'))
pct_buff_burn$variable <- factor(pct_buff_burn$variable, levels=c('logChloro','logDOC','logTN','logTP','logTSS','pH','SecchiDepth_m','WaterTemp_C'))

pct_ws_burn_HSveg <- as.data.frame(unclass(pct_ws_burn_HSveg),stringsAsFactors = T)
pct_ws_burn_HSveg$predictor <- factor(pct_ws_burn_HSveg$predictor, levels=c('ws_vbs_High_pct','ConnClass','ws_lake_arearatio','lake_totalarea_ha','mean_depth','Month','Random: lake','ws_area_ha'))
pct_ws_burn_HSveg$variable <- factor(pct_ws_burn_HSveg$variable, levels=c('logChloro','logDOC','logTN','logTP','logTSS','pH','SecchiDepth_m','WaterTemp_C'))

pct_buff_burn_HSveg <- as.data.frame(unclass(pct_buff_burn_HSveg),stringsAsFactors = T)
pct_buff_burn_HSveg$predictor <- factor(pct_buff_burn_HSveg$predictor, levels=c('buff100m_vbs_High_pct','ConnClass','ws_lake_arearatio','lake_totalarea_ha','mean_depth','Month','Random: lake','ws_area_ha'))
pct_buff_burn_HSveg$variable <- factor(pct_buff_burn_HSveg$variable, levels=c('logChloro','logDOC','logTN','logTP','logTSS','pH','SecchiDepth_m','WaterTemp_C'))

pct_ws_burn_HSsoil <- as.data.frame(unclass(pct_ws_burn_HSsoil),stringsAsFactors = T)
pct_ws_burn_HSsoil$predictor <- factor(pct_ws_burn_HSsoil$predictor, levels=c('ws_sbs_High_pct','ConnClass','ws_lake_arearatio','lake_totalarea_ha','mean_depth','Month','Random: lake','ws_area_ha'))
pct_ws_burn_HSsoil$variable <- factor(pct_ws_burn_HSsoil$variable, levels=c('logChloro','logDOC','logTN','logTP','logTSS','pH','SecchiDepth_m','WaterTemp_C'))

pct_buff_burn_HSsoil <- as.data.frame(unclass(pct_buff_burn_HSsoil),stringsAsFactors = T)
pct_buff_burn_HSsoil$predictor <- factor(pct_buff_burn_HSsoil$predictor, levels=c('buff100m_sbs_High_pct','ConnClass','ws_lake_arearatio','lake_totalarea_ha','mean_depth','Month','Random: lake','ws_area_ha'))
pct_buff_burn_HSsoil$variable <- factor(pct_buff_burn_HSsoil$variable, levels=c('logChloro','logDOC','logTN','logTP','logTSS','pH','SecchiDepth_m','WaterTemp_C'))


#### Main program ####
# set up plots individually
pct_ws_burn_plot <- ggplot(pct_ws_burn, aes(fill=predictor, y=variance, x=variable)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("% Watershed burned") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.65, hjust=0.6, color='black'),
        axis.text.y = element_text(size = 10, color='black'),
        axis.title = element_text(size = 10),
        axis.title.x=element_blank()) +
  ylab("Variance explained") +
  #xlab("Lake response variable")+
  scale_x_discrete(breaks=c("logTP","pH","logTN","logChloro","logTSS","logDOC","SecchiDepth_m","WaterTemp_C"),
                   labels=c("TP","pH","TN","Chlorophyll","TSS","DOC","Secchi","Temp"))+
  scale_fill_manual("Predictor", values=c('darkred','khaki','salmon','blue','navy','navajowhite4','gray70','aquamarine'),
                    labels=c('% WS Burn','Connectivity','Drainage ratio','Lake area','Max depth','Month','Random:lake','Watershed area'))
pct_ws_burn_plot

pct_ws_HSburnveg_plot <- ggplot(pct_ws_burn_HSveg, aes(fill=predictor, y=variance, x=variable)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("% Watershed burned HS (veg)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.65, hjust=0.6, color='black'),
        axis.text.y = element_text(size = 10, color='black'),
        axis.title = element_text(size = 10),
        axis.title.x=element_blank()) +
  ylab("Variance explained") +
  #xlab("Lake response variable")+
  scale_x_discrete(breaks=c("logTP","logTSS","logTN","logChloro","pH","logDOC","SecchiDepth_m","WaterTemp_C"),
                   labels=c("TP","TSS","TN","Chlorophyll","pH","DOC","Secchi","Temp"))+
  scale_fill_manual("Predictor", values=c('darkred','khaki','salmon','blue','navy','navajowhite4','gray70','aquamarine'),
                    labels=c('% WS HS veg','Connectivity','Drainage ratio','Lake area','Max depth','Month','Random:lake','Watershed area'))
pct_ws_HSburnveg_plot

pct_buff_burn_plot <- ggplot(pct_buff_burn, aes(fill=predictor, y=variance, x=variable)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("% Shoreline burned") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.65, hjust=0.6, color='black'),
        axis.text.y = element_text(size = 10, color='black'),
        axis.title = element_text(size = 10),
        axis.title.x=element_blank()) +
  ylab("Variance explained") +
  #xlab("Lake response variable")+
  scale_x_discrete(breaks=c("logTP","logTN","logTSS","pH","logChloro","SecchiDepth_m","logDOC","WaterTemp_C"),
                   labels=c("TP","TN","TSS","pH","Chlorophyll","Secchi","DOC","Temp"))+
  scale_fill_manual("Predictor", values=c('darkred','khaki','salmon','blue','navy','navajowhite4','gray70','aquamarine'),
                    labels=c('% Buff Burn','Connectivity','Drainage ratio','Lake area','Max depth','Month','Random:lake','Watershed area'))
pct_buff_burn_plot

pct_buff_HSburnveg_plot <- ggplot(pct_buff_burn_HSveg, aes(fill=predictor, y=variance, x=variable)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("% Shoreline burned HS (veg)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.65, hjust=0.6, color='black'),
        axis.text.y = element_text(size = 10, color='black'),
        axis.title = element_text(size = 10),
        axis.title.x=element_blank()) +
  ylab("Variance explained") +
  #xlab("Lake response variable")+
  scale_x_discrete(breaks=c("logTP","logTSS","logTN","logChloro","pH","logDOC","SecchiDepth_m","WaterTemp_C"),
                   labels=c("TP","TSS","TN","Chlorophyll","pH","DOC","Secchi","Temp"))+
  scale_fill_manual("Predictor", values=c('darkred','khaki','salmon','blue','navy','navajowhite4','gray70','aquamarine'),
                    labels=c('% Buff HS veg','Connectivity','Drainage ratio','Lake area','Max depth','Month','Random:lake','Watershed area'))
pct_buff_HSburnveg_plot


pct_buff_HSburnsoil_plot <- ggplot(pct_buff_burn_HSsoil, aes(fill=predictor, y=variance, x=variable)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("% Shoreline burned HS (soil)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.65, hjust=0.6, color='black'),
        axis.text.y = element_text(size = 10, color='black'),
        axis.title = element_text(size = 10),
        axis.title.x=element_blank()) +
  ylab("Variance explained") +
  #xlab("Lake response variable")+
  scale_x_discrete(breaks=c("logTP","logTSS","logDOC","pH","logChloro", "logTN", "SecchiDepth_m","WaterTemp_C"),
                   labels=c("TP","TSS","DOC","pH","Chlorophyll","TN", "Secchi","Temp"))+
  scale_fill_manual("Predictor", values=c('darkred','khaki','salmon','blue','navy','navajowhite4','gray70','aquamarine'),
                    labels=c('% Buff HS soil','Connectivity','Drainage ratio','Lake area','Max depth','Month','Random:lake','Watershed area'))
pct_buff_HSburnsoil_plot


pct_ws_HSburnsoil_plot <- ggplot(pct_ws_burn_HSsoil, aes(fill=predictor, y=variance, x=variable)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("% Watershed burned HS (soil)") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 0.65, hjust=0.6, color='black'),
        axis.text.y = element_text(size = 10, color='black'),
        axis.title = element_text(size = 10),
        axis.title.x=element_blank()) +
  ylab("Variance explained") +
  #xlab("Lake response variable")+
  scale_x_discrete(breaks=c("logTP","logTSS","logDOC","pH","logTN", "logChloro", "SecchiDepth_m","WaterTemp_C"),
                   labels=c("TP","TSS","DOC","pH","TN","Chlorophyll", "Secchi","Temp"))+
  scale_fill_manual("Predictor", values=c('darkred','khaki','salmon','blue','navy','navajowhite4','gray70','aquamarine'),
                    labels=c('% WS HS soil','Connectivity','Drainage ratio','Lake area','Max depth','Month','Random:lake','Watershed area'))
pct_ws_HSburnsoil_plot

### multipanel plot
# very basic for now; need to adjust margins, fonts, legends, etc.
grid.arrange(pct_ws_burn_plot, pct_buff_burn_plot,
             pct_ws_HSburnveg_plot, pct_buff_HSburnveg_plot,
             pct_ws_HSburnsoil_plot, pct_buff_HSburnsoil_plot,
             nrow=3)
