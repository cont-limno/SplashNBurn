####################### Water quality/fire gradient analysis ##################
# Date: 10-25-22
# updated: 12-14-22; updated corr matrices for pH and water temp
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(ggplot2)
library(gridExtra)
library(rstatix)
library(Hmisc)
library(dplyr)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")

# water quality
waterquality <- read.csv("Data/WaterQuality/combined_lab_field_may_sep.csv")

# burn severity variables
burn_severity <- read.csv("Data/BurnSeverity/Ian_calculations/all_burn_severity_variables.csv")

## LAGOS data
LAGOStable <- read.csv("Data/LAGOS/LAGOS_LOCUS_GEO_DEPTH_combined.csv")

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
waterquality$logWaterTemp_C <- log(waterquality$WaterTemp_C)

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

allmonths <- rbind.data.frame(mayWQ_fire, juneWQ_fire, julyWQ_fire, augWQ_fire, sepWQ_fire)
#allmonths_list <- list(mayWQ_fire, juneWQ_fire, julyWQ_fire, augWQ_fire, sepWQ_fire)
#allmonths <- Reduce(function(x, y) merge(x, y, all=T), allmonths_list)


## calculate 5-month averages for each water quality variable, then merge to burn_severity
# to see what fire variables are most correlated with water quality
TP_all <- allmonths %>%
  dplyr::group_by(lagoslakeid) %>%
  dplyr::summarize(meanTP = mean(TP_ppb, na.rm=T),
            meanlogTP = mean(logTP, na.rm=T))
TP_all_list <- list(mayWQ_fire[,c('lagoslakeid','TP_ppb','logTP')],
                    juneWQ_fire[,c('lagoslakeid','TP_ppb','logTP')],
                    julyWQ_fire[,c('lagoslakeid','TP_ppb','logTP')],
                    augWQ_fire[,c('lagoslakeid','TP_ppb','logTP')],
                    sepWQ_fire[,c('lagoslakeid','TP_ppb','logTP')])
TP_all_list <- Reduce(function(x, y) merge(x, y, all=T, by='lagoslakeid'), TP_all_list)
colnames(TP_all_list) <- c('lagoslakeid','mayTP','maylogTP', 'junTP','junlogTP',
                           'julTP','jullogTP','augTP','auglogTP','sepTP','seplogTP')

TP_all <- merge(TP_all, TP_all_list, by='lagoslakeid',all=T) 
TP_all <- merge(TP_all, burn_severity, by='lagoslakeid', all=T)
TP_cor <- as.data.frame(cor(TP_all, method='pearson', use='pairwise.complete.obs'))#[,c(2:13)]

# extract select variables from correlation matrix
TP_cor_ext <- TP_cor[,c('ws_vbs_total_burn_pct','ws_vbs_High_pct',
                        'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                        'buff100m_vbs_High_pct','buff100m_sbs_High_pct')] #extract desired columns
TP_cor_ext <- TP_cor_ext[c('maylogTP','junlogTP','jullogTP','auglogTP',
                           'seplogTP','meanlogTP'),] #extract desired rows
TP_cor_ext <- round(TP_cor_ext, 2)
#write.csv(TP_cor_ext, file='Data/Correlation_matrices/TP_correlation_matrix.csv', row.names=T)

# get p values
psdf <- TP_all[,c('maylogTP','junlogTP','jullogTP','auglogTP',
                  'seplogTP','meanlogTP','ws_vbs_total_burn_pct','ws_vbs_High_pct',
                  'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                  'buff100m_vbs_High_pct','buff100m_sbs_High_pct')]

TP_P <- rcorr(as.matrix(psdf), type='pearson')
TP_P <- as.data.frame(TP_P$P)[c(1:6),c(7:12)]

chloro_all <- allmonths %>%
  dplyr::group_by(lagoslakeid) %>%
  dplyr::summarize(meanChloro = mean(Chloro_ppb, na.rm=T),
            meanlogChloro = mean(logChloro, na.rm=T))
chloro_all_list <- list(mayWQ_fire[,c('lagoslakeid','Chloro_ppb','logChloro')],
                        juneWQ_fire[,c('lagoslakeid','Chloro_ppb','logChloro')],
                        julyWQ_fire[,c('lagoslakeid','Chloro_ppb','logChloro')],
                        augWQ_fire[,c('lagoslakeid','Chloro_ppb','logChloro')],
                        sepWQ_fire[,c('lagoslakeid','Chloro_ppb','logChloro')])
chloro_all_list <- Reduce(function(x, y) merge(x, y, all=T, by='lagoslakeid'), chloro_all_list)
colnames(chloro_all_list) <- c('lagoslakeid','maychloro','maylogChloro', 'junchloro','junlogChloro',
                               'julchloro','jullogChloro','augchloro','auglogChloro','sepchloro','seplogChloro')

chloro_all <- merge(chloro_all, chloro_all_list, by='lagoslakeid',all=T) 
chloro_all <- merge(chloro_all, burn_severity, by='lagoslakeid', all=T)
chloro_cor <- as.data.frame(cor(chloro_all, method='pearson', use='pairwise.complete.obs'))#[,c(2:13)]

# extract select variables from correlation matrix
chloro_cor_ext <- chloro_cor[,c('ws_vbs_total_burn_pct','ws_vbs_High_pct',
                        'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                        'buff100m_vbs_High_pct','buff100m_sbs_High_pct')] #extract desired columns
chloro_cor_ext <- chloro_cor_ext[c('maylogChloro','junlogChloro','jullogChloro','auglogChloro',
                           'seplogChloro','meanlogChloro'),] #extract desired rows
chloro_cor_ext <- round(chloro_cor_ext, 2)
#write.csv(chloro_cor_ext, file='Data/Correlation_matrices/chloro_correlation_matrix.csv', row.names=T)

# get p values
psdf <- chloro_all[,c('maylogChloro','junlogChloro','jullogChloro','auglogChloro',
                      'seplogChloro','meanlogChloro','ws_vbs_total_burn_pct','ws_vbs_High_pct',
                      'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                      'buff100m_vbs_High_pct','buff100m_sbs_High_pct')]

chloro_P <- rcorr(as.matrix(psdf), type='pearson')
chloro_P <- as.data.frame(chloro_P$P)[c(1:6),c(7:12)]


TN_all <- allmonths %>%
  dplyr::group_by(lagoslakeid) %>%
  dplyr::summarize(meanTN = mean(TN_ppb, na.rm=T),
            meanlogTN = mean(logTN, na.rm=T))
TN_all_list <- list(mayWQ_fire[,c('lagoslakeid','TN_ppb','logTN')],
                    juneWQ_fire[,c('lagoslakeid','TN_ppb','logTN')],
                    julyWQ_fire[,c('lagoslakeid','TN_ppb','logTN')],
                    augWQ_fire[,c('lagoslakeid','TN_ppb','logTN')],
                    sepWQ_fire[,c('lagoslakeid','TN_ppb','logTN')])
TN_all_list <- Reduce(function(x, y) merge(x, y, all=T, by='lagoslakeid'), TN_all_list)
colnames(TN_all_list) <- c('lagoslakeid','mayTN','maylogTN', 'junTN','junlogTN',
                           'julTN','jullogTN','augTN','auglogTN','sepTN','seplogTN')

TN_all <- merge(TN_all, TN_all_list, by='lagoslakeid',all=T) 
TN_all <- merge(TN_all, burn_severity, by='lagoslakeid', all=T)
TN_cor <- as.data.frame(cor(TN_all, method='pearson', use='pairwise.complete.obs'))#[,c(2:13)]

# extract select variables from correlation matrix
TN_cor_ext <- TN_cor[,c('ws_vbs_total_burn_pct','ws_vbs_High_pct',
                        'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                        'buff100m_vbs_High_pct','buff100m_sbs_High_pct')] #extract desired columns
TN_cor_ext <- TN_cor_ext[c('maylogTN','junlogTN','jullogTN','auglogTN',
                           'seplogTN','meanlogTN'),] #extract desired rows
TN_cor_ext <- round(TN_cor_ext, 2)
#write.csv(TN_cor_ext, file='Data/Correlation_matrices/TN_correlation_matrix.csv', row.names=T)

# get p values
psdf <- TN_all[,c('maylogTN','junlogTN','jullogTN','auglogTN',
                  'seplogTN','meanlogTN','ws_vbs_total_burn_pct','ws_vbs_High_pct',
                  'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                  'buff100m_vbs_High_pct','buff100m_sbs_High_pct')]

TN_P <- rcorr(as.matrix(psdf), type='pearson')
TN_P <- as.data.frame(TN_P$P)[c(1:6),c(7:12)]


DOC_all <- allmonths %>%
  dplyr::group_by(lagoslakeid) %>%
  dplyr::summarize(meanDOC = mean(DOC_ppm, na.rm=T),
            meanlogDOC = mean(logDOC, na.rm=T))
DOC_all_list <- list(mayWQ_fire[,c('lagoslakeid','DOC_ppm','logDOC')],
                     juneWQ_fire[,c('lagoslakeid','DOC_ppm','logDOC')],
                     julyWQ_fire[,c('lagoslakeid','DOC_ppm','logDOC')],
                     augWQ_fire[,c('lagoslakeid','DOC_ppm','logDOC')],
                     sepWQ_fire[,c('lagoslakeid','DOC_ppm','logDOC')])
DOC_all_list <- Reduce(function(x, y) merge(x, y, all=T, by='lagoslakeid'), DOC_all_list)
colnames(DOC_all_list) <- c('lagoslakeid','mayDOC','maylogDOC', 'junDOC','junlogDOC',
                            'julDOC','jullogDOC','augDOC','auglogDOC','sepDOC','seplogDOC')

DOC_all <- merge(DOC_all, DOC_all_list, by='lagoslakeid',all=T) 
DOC_all <- merge(DOC_all, burn_severity, by='lagoslakeid', all=T)
DOC_cor <- as.data.frame(cor(DOC_all, method='pearson', use='pairwise.complete.obs'))#[,c(2:13)]

# extract select variables from correlation matrix
DOC_cor_ext <- DOC_cor[,c('ws_vbs_total_burn_pct','ws_vbs_High_pct',
                          'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                          'buff100m_vbs_High_pct','buff100m_sbs_High_pct')] #extract desired columns
DOC_cor_ext <- DOC_cor_ext[c('maylogDOC','junlogDOC','jullogDOC','auglogDOC',
                             'seplogDOC','meanlogDOC'),] #extract desired rows
DOC_cor_ext <- round(DOC_cor_ext, 2)
#write.csv(DOC_cor_ext, file='Data/Correlation_matrices/DOC_correlation_matrix.csv', row.names=T)

# get p values
psdf <- DOC_all[,c('maylogDOC','junlogDOC','jullogDOC','auglogDOC',
                   'seplogDOC','meanlogDOC','ws_vbs_total_burn_pct','ws_vbs_High_pct',
                   'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                   'buff100m_vbs_High_pct','buff100m_sbs_High_pct')]

DOC_P <- rcorr(as.matrix(psdf), type='pearson')
DOC_P <- as.data.frame(DOC_P$P)[c(1:6),c(7:12)]


TSS_all <- allmonths %>%
  dplyr::group_by(lagoslakeid) %>%
  dplyr::summarize(meanTSS = mean(TSS_mgL, na.rm=T),
            meanlogTSS = mean(logTSS, na.rm=T))
TSS_all_list <- list(mayWQ_fire[,c('lagoslakeid','TSS_mgL','logTSS')],
                     juneWQ_fire[,c('lagoslakeid','TSS_mgL','logTSS')],
                     julyWQ_fire[,c('lagoslakeid','TSS_mgL','logTSS')],
                     augWQ_fire[,c('lagoslakeid','TSS_mgL','logTSS')],
                     sepWQ_fire[,c('lagoslakeid','TSS_mgL','logTSS')])
TSS_all_list <- Reduce(function(x, y) merge(x, y, all=T, by='lagoslakeid'), TSS_all_list)
colnames(TSS_all_list) <- c('lagoslakeid','mayTSS','maylogTSS', 'junTSS','junlogTSS',
                            'julTSS','jullogTSS','augTSS','auglogTSS','sepTSS','seplogTSS')

TSS_all <- merge(TSS_all, TSS_all_list, by='lagoslakeid',all=T) 
TSS_all <- merge(TSS_all, burn_severity, by='lagoslakeid', all=T)
TSS_cor <- as.data.frame(cor(TSS_all, method='pearson', use='pairwise.complete.obs'))#[,c(2:13)]

# extract select variables from correlation matrix
TSS_cor_ext <- TSS_cor[,c('ws_vbs_total_burn_pct','ws_vbs_High_pct',
                          'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                          'buff100m_vbs_High_pct','buff100m_sbs_High_pct')] #extract desired columns
TSS_cor_ext <- TSS_cor_ext[c('maylogTSS','junlogTSS','jullogTSS','auglogTSS',
                             'seplogTSS','meanlogTSS'),] #extract desired rows
TSS_cor_ext <- round(TSS_cor_ext, 2)
#write.csv(TSS_cor_ext, file='Data/Correlation_matrices/TSS_correlation_matrix.csv', row.names=T)

# get p values
psdf <- TSS_all[,c('maylogTSS','junlogTSS','jullogTSS','auglogTSS',
                   'seplogTSS','meanlogTSS','ws_vbs_total_burn_pct','ws_vbs_High_pct',
                   'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                   'buff100m_vbs_High_pct','buff100m_sbs_High_pct')]

TSS_P <- rcorr(as.matrix(psdf), type='pearson')
TSS_P <- as.data.frame(TSS_P$P)[c(1:6),c(7:12)]


Secchi_all <- allmonths %>%
  dplyr::group_by(lagoslakeid) %>%
  dplyr::summarize(meanSecchi = mean(SecchiDepth_m, na.rm=T),
            meanlogSecchi = mean(logSecchi, na.rm=T))
Secchi_all_list <- list(mayWQ_fire[,c('lagoslakeid','SecchiDepth_m','logSecchi')],
                        juneWQ_fire[,c('lagoslakeid','SecchiDepth_m','logSecchi')],
                        julyWQ_fire[,c('lagoslakeid','SecchiDepth_m','logSecchi')],
                        augWQ_fire[,c('lagoslakeid','SecchiDepth_m','logSecchi')],
                        sepWQ_fire[,c('lagoslakeid','SecchiDepth_m','logSecchi')])
Secchi_all_list <- Reduce(function(x, y) merge(x, y, all=T, by='lagoslakeid'), Secchi_all_list)
colnames(Secchi_all_list) <- c('lagoslakeid','maySecchi','maylogSecchi', 'junSecchi','junlogSecchi',
                               'julSecchi','jullogSecchi','augSecchi','auglogSecchi','sepSecchi','seplogSecchi')

Secchi_all <- merge(Secchi_all, Secchi_all_list, by='lagoslakeid',all=T) 
Secchi_all <- merge(Secchi_all, burn_severity, by='lagoslakeid', all=T)
Secchi_cor <- as.data.frame(cor(Secchi_all, method='pearson', use='pairwise.complete.obs'))#[,c(2:13)]

# extract select variables from correlation matrix
Secchi_cor_ext <- Secchi_cor[,c('ws_vbs_total_burn_pct','ws_vbs_High_pct',
                                'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                                'buff100m_vbs_High_pct','buff100m_sbs_High_pct')] #extract desired columns
Secchi_cor_ext <- Secchi_cor_ext[c('maylogSecchi','junlogSecchi','jullogSecchi','auglogSecchi',
                                   'seplogSecchi','meanlogSecchi'),] #extract desired rows
Secchi_cor_ext <- round(Secchi_cor_ext, 2)
#write.csv(Secchi_cor_ext, file='Data/Correlation_matrices/Secchi_correlation_matrix.csv', row.names=T)

# get p values
psdf <- Secchi_all[,c('maylogSecchi','junlogSecchi','jullogSecchi','auglogSecchi',
                      'seplogSecchi','meanlogSecchi','ws_vbs_total_burn_pct','ws_vbs_High_pct',
                      'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                      'buff100m_vbs_High_pct','buff100m_sbs_High_pct')]

Secchi_P <- rcorr(as.matrix(psdf), type='pearson')
Secchi_P <- as.data.frame(Secchi_P$P)[c(1:6),c(7:12)]


pH_all <- allmonths %>%
  dplyr::group_by(lagoslakeid) %>%
  dplyr::summarize(meanpH = mean(pH, na.rm=T)) #seems approx normally distributed
pH_all_list <- list(mayWQ_fire[,c('lagoslakeid','pH')],
                    juneWQ_fire[,c('lagoslakeid','pH')],
                    julyWQ_fire[,c('lagoslakeid','pH')],
                    augWQ_fire[,c('lagoslakeid','pH')],
                    sepWQ_fire[,c('lagoslakeid','pH')])
pH_all_list <- Reduce(function(x, y) merge(x, y, all=T, by='lagoslakeid'), pH_all_list)
colnames(pH_all_list) <- c('lagoslakeid','maypH','junpH',
                           'julpH','augpH','seppH')

pH_all <- merge(pH_all, pH_all_list, by='lagoslakeid',all=T) 
pH_all <- merge(pH_all, burn_severity, by='lagoslakeid', all=T)
pH_cor <- as.data.frame(cor(pH_all, method='pearson', use='pairwise.complete.obs'))#[,c(2:13)]

# extract select variables from correlation matrix
pH_cor_ext <- pH_cor[,c('ws_vbs_total_burn_pct','ws_vbs_High_pct',
                        'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                        'buff100m_vbs_High_pct','buff100m_sbs_High_pct')] #extract desired columns
pH_cor_ext <- pH_cor_ext[c('maypH','junpH','julpH','augpH',
                           'seppH','meanpH'),] #extract desired rows
pH_cor_ext <- round(pH_cor_ext, 2)
#write.csv(pH_cor_ext, file='Data/Correlation_matrices/pH_correlation_matrix.csv', row.names=T)

# get p values
psdf <- pH_all[,c('maypH','junpH','julpH','augpH',
                  'seppH','meanpH','ws_vbs_total_burn_pct','ws_vbs_High_pct',
                  'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                  'buff100m_vbs_High_pct','buff100m_sbs_High_pct')]

pH_P <- rcorr(as.matrix(psdf), type='pearson')
pH_P <- as.data.frame(pH_P$P)[c(1:6),c(7:12)]


WaterTemp_C_all <- allmonths %>%
  dplyr::group_by(lagoslakeid) %>%
  dplyr::summarize(meanWaterTemp_C = mean(WaterTemp_C, na.rm=T),
                   meanlogWaterTemp_C = mean(logWaterTemp_C, na.rm=T))
WaterTemp_C_all_list <- list(mayWQ_fire[,c('lagoslakeid','WaterTemp_C','logWaterTemp_C')],
                             juneWQ_fire[,c('lagoslakeid','WaterTemp_C','logWaterTemp_C')],
                             julyWQ_fire[,c('lagoslakeid','WaterTemp_C','logWaterTemp_C')],
                             augWQ_fire[,c('lagoslakeid','WaterTemp_C','logWaterTemp_C')],
                             sepWQ_fire[,c('lagoslakeid','WaterTemp_C','logWaterTemp_C')])
WaterTemp_C_all_list <- Reduce(function(x, y) merge(x, y, all=T, by='lagoslakeid'), WaterTemp_C_all_list)
colnames(WaterTemp_C_all_list) <- c('lagoslakeid','mayWaterTemp_C','maylogWaterTemp_C', 'junWaterTemp_C','junlogWaterTemp_C',
                                    'julWaterTemp_C','jullogWaterTemp_C','augWaterTemp_C','auglogWaterTemp_C','sepWaterTemp_C','seplogWaterTemp_C')

WaterTemp_C_all <- merge(WaterTemp_C_all, WaterTemp_C_all_list, by='lagoslakeid',all=T) 
WaterTemp_C_all <- merge(WaterTemp_C_all, burn_severity, by='lagoslakeid', all=T)
WaterTemp_C_cor <- as.data.frame(cor(WaterTemp_C_all, method='pearson', use='pairwise.complete.obs'))#[,c(2:13)]

# extract select variables from correlation matrix
WaterTemp_C_cor_ext <- WaterTemp_C_cor[,c('ws_vbs_total_burn_pct','ws_vbs_High_pct',
                                          'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                                          'buff100m_vbs_High_pct','buff100m_sbs_High_pct')] #extract desired columns
WaterTemp_C_cor_ext <- WaterTemp_C_cor_ext[c('maylogWaterTemp_C','junlogWaterTemp_C','jullogWaterTemp_C','auglogWaterTemp_C',
                                             'seplogWaterTemp_C','meanlogWaterTemp_C'),] #extract desired rows
WaterTemp_C_cor_ext <- round(WaterTemp_C_cor_ext, 2)
#write.csv(WaterTemp_C_cor_ext, file='Data/Correlation_matrices/WaterTemp_C_correlation_matrix.csv', row.names=T)

# get p values
psdf <- WaterTemp_C_all[,c('maylogWaterTemp_C','junlogWaterTemp_C','jullogWaterTemp_C','auglogWaterTemp_C',
                           'seplogWaterTemp_C','meanlogWaterTemp_C','ws_vbs_total_burn_pct','ws_vbs_High_pct',
                           'ws_sbs_High_pct','buff100m_vbs_total_burn_pct',
                           'buff100m_vbs_High_pct','buff100m_sbs_High_pct')]

WaterTemp_C_P <- rcorr(as.matrix(psdf), type='pearson')
WaterTemp_C_P <- as.data.frame(WaterTemp_C_P$P)[c(1:6),c(7:12)]

NO2NO3_all <- allmonths %>%
  dplyr::group_by(lagoslakeid) %>%
  dplyr::summarize(meanNO2NO3 = mean(NO2NO3_ppb, na.rm=T),
            meanlogNO2NO3 = mean(logNO2NO3, na.rm=T))

NH4N_all <- allmonths %>%
  dplyr::group_by(lagoslakeid) %>%
  dplyr::summarize(meanNH4N = mean(NH4N_ppb, na.rm=T),
            meanlogNH4N = mean(logNH4N, na.rm=T))

ANC_all <- allmonths %>%
  dplyr::group_by(lagoslakeid) %>%
  dplyr::summarize(meanANCmg = mean(ANC_mgCaCO3L, na.rm=T),
            meanlogANCmg = mean(logANCmg, na.rm=T))

DO_all <- allmonths %>%
  dplyr::group_by(lagoslakeid) %>%
  dplyr::summarize(meanDOpct = mean(LDO_pct, na.rm=T),
            meanDOmgL = mean(LDO_mgL, na.rm=T)) #seems approx normally distributed

TempC_all <- allmonths %>%
  dplyr::group_by(lagoslakeid) %>%
  dplyr::summarize(meanTempC = mean(WaterTemp_C, na.rm=T)) #seems approx normally distributed

# waterquality_all_list <- list(TP_all, TN_all, DOC_all, TSS_all, chloro_all, Secchi_all,
#                               pH_all, ANC_all, NO2NO3_all, NH4N_all, DO_all, TempC_all)
# 
# waterquality_monthly_means <- Reduce(function(x, y) merge(x, y, all=T), waterquality_all_list)
# waterquality_monthly_means_fire <- merge(waterquality_monthly_means, burn_severity, by='lagoslakeid', all=T)


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
xlabb <- 'Watershed % burned high severity (veg)'
#xlabb <- 'Watershed % burned'
ylabb <- 'log(Chlorophyll-a) (ppb)'
rvalx <- 95
rvaly <- -0.4
pvalx <- 95
pvaly <- -0.7
labelnudge <- 0.5

# May
plotcor <- cor.test(mayWQ_fire[,wqvar], mayWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(mayWQ_fire[,wqvar] ~ mayWQ_fire[,firevar])
plotMay <- ggplot(data=mayWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('A) May')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red", size=3)+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red", size=3)+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
        legend.position=c('none'))+ #0.55,0.2; use this for high severity
  scale_color_manual("", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotMay

# June
plotcor <- cor.test(juneWQ_fire[,wqvar], juneWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(juneWQ_fire[,wqvar] ~ juneWQ_fire[,firevar])
plotJun <- ggplot(data=juneWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('B) June')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red", size=3)+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red", size=3)+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
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
  geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red", size=3)+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red", size=3)+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
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
  geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red", size=3)+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red", size=3)+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
        legend.position=c(0.55,0.2))+ #use this for total % burn
  scale_color_manual("", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotAug

# September
plotcor <- cor.test(sepWQ_fire[,wqvar], sepWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(sepWQ_fire[,wqvar] ~ sepWQ_fire[,firevar])
plotSep <- ggplot(data=sepWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('E) September')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red", size=3)+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red", size=3)+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotSep

# all months
wqvarall <- 'meanlogChloro'
chloro_plot_data <- merge(chloro_all, juneWQ_fire[,c('lagoslakeid','LakeName','ConnClass')], by='lagoslakeid',all=T)
plotcor <- cor.test(chloro_all$meanlogChloro, chloro_all[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(chloro_all$meanlogChloro ~ chloro_all[,firevar])
plotAll <- ggplot(data=chloro_plot_data, aes_string(x=firevar, y=wqvarall, color=colorvar, label=labelvar))+
  ggtitle('F) All months')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red", size=3)+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red", size=3)+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotAll

grid.arrange(plotMay, plotJun, plotJul, plotAug, plotSep, plotAll, nrow=2)

# jpeg('Figures/multipanel_HSburn_gradient_chla.jpeg',width = 8,height = 6,units = 'in',res=600)
#    grid.arrange(plotMay, plotJun, plotJul, plotAug, plotSep, plotAll, nrow=2)
# dev.off()

# jpeg('Figures/multipanel_totalburn_gradient_chla.jpeg',width = 8,height = 6,units = 'in',res=600)
#   grid.arrange(plotMay, plotJun, plotJul, plotAug, plotSep, plotAll, nrow=2)
# dev.off()

## can we explain residuals with other variables?
test_lm <- lm(logChloro ~ ws_vbs_High_pct, data=mayWQ_fire)
summary(test_lm)
resids <- as.data.frame(residuals(test_lm)) #note this is screwed up when there's a missing value, so have to merge instead of cbind
names(resids) <- 'residuals'
sites <- as.data.frame(mayWQ_fire$Site)
names(sites) <- 'Site'
sites$row <- rownames(sites)
resids$row <- rownames(resids)
resids <- merge(resids, sites, by='row', all=T)
alldata <- merge(mayWQ_fire, LAGOStable, by='lagoslakeid', all=F)
alldata <- merge(alldata, resids, by.x='Site.x', by.y='Site', all=T)

residcor <- as.data.frame(cor(alldata[, unlist(lapply(alldata, is.numeric))], use='pairwise.complete.obs'))  
residcor <- data.frame(variable=rownames(residcor), residuals=residcor$residuals)

# try with subset of select variables
resid_predictors <- alldata[,c('zMax_m','logChloro','logTP','logTN',
                               'logNO2NO3','logNH4N','logTSS','logDOC','logANCmg',
                               'logSecchi','logTNTP','streams_all_mperha',
                               'lake_waterarea_ha','lake_elevation_m','ws_area_ha',
                               'ws_lake_arearatio','residuals')]
#getOption("na.action")
#xx <- cor.test(resid_predictors, method='pearson')

options(scipen = 999)
residcor <- resid_predictors %>% cor_test(
  vars = "residuals", use='pairwise.complete.obs')

# try with all months pooled
test_alllm <- lm(logChloro ~ ws_vbs_High_pct, data=allmonths)
summary(test_alllm)
resids <- as.data.frame(residuals(test_alllm)) #note this is screwed up when there's a missing value, so have to merge instead of cbind
names(resids) <- 'residuals'
sites <- as.data.frame(allmonths$Site)
names(sites) <- 'Site'
sites$row <- rownames(sites)
resids$row <- rownames(resids)
resids <- merge(resids, sites, by='row', all=T)
alldata <- merge(allmonths, LAGOStable, by='lagoslakeid', all=F)
alldata <- merge(alldata, resids, by.x='Site.x', by.y='Site', all=T)

residcor_allmonths <- as.data.frame(cor(alldata[, unlist(lapply(alldata, is.numeric))], use='pairwise.complete.obs'))  
residcor_allmonths <- data.frame(variable=rownames(residcor_allmonths), residuals=residcor_allmonths$residuals)

options(scipen = 999)
residcor_allmonths <- resid_predictors %>% cor_test(
  vars = "residuals", use='pairwise.complete.obs')

### TP ###
# set variables and plotting parameters for all plots
wqvar <- 'logTP'
firevar <- 'ws_sbs_High_pct'
#firevar <- 'ws_vbs_total_burn_pct'
colorvar <- 'ConnClass'
labelvar <- 'LakeName'
xlimz <- c(0,3) #0-100 for vbs, 0-3 for sbs
ylimz <- c(2,4)
xlabb <- 'Watershed % burned high severity (soil)'
#xlabb <- 'Watershed % burned'
ylabb <- 'log(Total phosphorus) (ppb)'
# rvalx <- 5
# rvaly <- 4
# pvalx <- 5
# pvaly <- 3.8
rvalx <- 2.5 #from chla plots #2.5 for sbs, 95 for vbs
rvaly <- 2.3
pvalx <- 2.5
pvaly <- 2.1
labelnudge <- 0.1 #had been 0.5 for vbs, 0.1 for sbs

# May
plotcor <- cor.test(mayWQ_fire[,wqvar], mayWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(mayWQ_fire[,wqvar] ~ mayWQ_fire[,firevar])
plotMay <- ggplot(data=mayWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('A) May')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red", size=3)+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red", size=3)+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
        legend.position=c(0.25,0.2))+ # or keep at (0.25, 0.2) for veg
  scale_color_manual("", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotMay

# June
plotcor <- cor.test(juneWQ_fire[,wqvar], juneWQ_fire[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(juneWQ_fire[,wqvar] ~ juneWQ_fire[,firevar])
plotJun <- ggplot(data=juneWQ_fire, aes_string(x=firevar, y=wqvar, color=colorvar, label=labelvar))+
  ggtitle('B) June')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red", size=3)+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red", size=3)+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
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
  geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red", size=3)+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red", size=3)+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
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
  geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red", size=3)+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red", size=3)+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
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
  geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red", size=3)+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red", size=3)+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotSep

# all months
wqvarall <- 'meanlogTP'
TP_plot_data <- merge(TP_all, juneWQ_fire[,c('lagoslakeid','LakeName','ConnClass')], by='lagoslakeid',all=T)
plotcor <- cor.test(TP_all$meanlogTP, TP_all[,firevar], method='pearson')
rval <- round(plotcor$estimate, 2)
pval <- round(plotcor$p.value, 2)
plotlm <- lm(TP_all$meanlogTP ~ TP_all[,firevar])
plotAll <- ggplot(data=TP_plot_data, aes_string(x=firevar, y=wqvarall, color=colorvar, label=labelvar))+
  ggtitle('F) All months')+
  geom_point(size=2)+ 
  geom_text(hjust=0, vjust=0, size=2, nudge_x=labelnudge)+
  #geom_smooth(method='lm')+ separate lines for isolated and drainage
  geom_abline(slope = coef(plotlm)[2], 
              intercept = coef(plotlm)[["(Intercept)"]])+
  #geom_vline(xintercept=25, linetype='dashed')+
  annotate(geom="text", x=rvalx, y=rvaly, label=paste0("r=",rval),
           color="red", size=3)+
  annotate(geom="text", x=pvalx, y=pvaly, label=paste0("p=",pval),
           color="red", size=3)+
  theme_classic()+
  scale_x_continuous(limits=xlimz, name=xlabb)+
  scale_y_continuous(limits=ylimz, name=ylabb)+
  theme(axis.text.x = element_text(color='black'),
        axis.text.y=element_text(color='black'),
        axis.title.x=element_text(size=9),
        axis.title.y=element_text(size=9),
        legend.position=c('none'))+
  scale_color_manual("Class", values=c('black','firebrick'), labels=c('Drainage','Isolated'))
plotAll

grid.arrange(plotMay, plotJun, plotJul, plotAug, plotSep, plotAll, nrow=2)

# jpeg('Figures/multipanel_HSburn_gradient_TP.jpeg',width = 8,height = 6,units = 'in',res=600)
#     grid.arrange(plotMay, plotJun, plotJul, plotAug, plotSep, plotAll, nrow=2)
# dev.off()

jpeg('Figures/multipanel_HSburnsoil_gradient_TP.jpeg',width = 8,height = 6,units = 'in',res=600)
   grid.arrange(plotMay, plotJun, plotJul, plotAug, plotSep, plotAll, nrow=2)
dev.off()

# jpeg('Figures/multipanel_totalburn_gradient_TP.jpeg',width = 8,height = 6,units = 'in',res=600)
#    grid.arrange(plotMay, plotJun, plotJul, plotAug, plotSep, plotAll, nrow=2)
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

tp_gam <- gam(logTP ~ s(ws_vbs_High_pct, bs='tp'), data=mayWQ_fire)
summary(tp_gam)
plot(tp_gam, main='May TP')

tp_gam2 <- gam(logTP ~ s(ws_vbs_High_pct) + s(ws_lake_arearatio), data=mayWQ_fire)
summary(tp_gam2)
plot(tp_gam2, main='May TP')

#### What are the best fire predictors of a given water quality variable? ####
# first make sure all profile depths are 0
mayWQ_fire$ProfileDepth_m <- 0
juneWQ_fire$ProfileDepth_m <- 0
julyWQ_fire$ProfileDepth_m <- 0
augWQ_fire$ProfileDepth_m <- 0
sepWQ_fire$ProfileDepth_m <- 0
allmonths$ProfileDepth_m <- 0

maycor <- cor(mayWQ_fire[, unlist(lapply(mayWQ_fire, is.numeric))], method='pearson', use='pairwise.complete.obs')
maycor <- as.data.frame(maycor)[14:58]

df_list <- list(mayWQ_fire, juneWQ_fire, julyWQ_fire, augWQ_fire, sepWQ_fire)

