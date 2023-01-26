# rm(list=ls())

library(tidyverse)
# library(rstan)
library(fields)
library(Matrix)
library(lubridate)
library(stringr)
library(car)  # logit function
library(corrr)
library(Hmsc)
library(corrplot)
library(ggpubr)
library(circlize)

setwd("C:/Users/immcc/Documents/SplashNBurn")

# load data ---------------------------------------------------------------
#chem_dat = read_csv("../data/combined_lab_field_may_sep_NEWJan13.csv")
chem_dat <- read.csv("Data/WaterQuality/combined_lab_field_may_sep.csv")
str(chem_dat)
dim(chem_dat)

#burn_dat = read_csv("../data/all_burn_severity_variables.csv")
burn_dat <- read.csv("Data/BurnSeverity/Ian_calculations/all_burn_severity_variables.csv")
str(burn_dat)
dim(burn_dat)


#locus_dat = read_csv("../data/LAGOS_LOCUS_GEO_DEPTH_combined.csv")
locus_dat <- read.csv("Data/LAGOS/LAGOS_LOCUS_GEO_DEPTH_combined.csv")
str(locus_dat)
dim(locus_dat)



# Merge chem and burn data
dat <- chem_dat %>% 
  left_join(locus_dat, by="lagoslakeid")
dim(dat)

dat <- dat %>% 
  mutate(Burned = factor(Type),
         lake_connectivity_class = factor(lake_connectivity_class.x),
         ConnClass = factor(ConnClass),
         Group = factor(Group),
         Month = factor(Month_factor),
         Site = factor(Site.x)) 
str(dat)

# Get mean depth of max depths for lakes
depths <- dat %>% 
  group_by(lagoslakeid) %>% 
  summarise(mean_depth = mean(zMax_m))
# Join in mean depths
dat <- dat %>% 
  left_join(depths, by="lagoslakeid")
# Join in burn data (but if you do this, you can only run analysis on burned lakes)
dat <- dat %>%
  left_join(burn_dat, by='lagoslakeid')

# Remove NAs
dat <- dat %>% 
  filter(!is.na(mean_depth)) %>% 
  filter(!is.na(SecchiDepth_m)) %>% 
  filter(!is.na(logTN)) %>% 
  filter(!is.na(logChloro)) %>% 
  filter(!is.na(pH))

# cor1 <- dat %>%
#   dplyr::select(zMax_m, ws_area_ha, ws_lake_arearatio, lake_totalarea_ha) %>%
#   correlate()
# cor1



# response matrix
ydat <- dat %>% 
  dplyr::select(logTP, logTN, logDOC, pH,
                logTSS, logChloro, SecchiDepth_m, WaterTemp_C,
         ) 
summary(ydat)

# Predictor matrix
xdat <- dat %>% 
  dplyr::select(ws_area_ha, ws_lake_arearatio, lake_totalarea_ha, Burned,
         Month, ConnClass, lagoslakeid, mean_depth)
summary(xdat)

##############################
### HMSC
##############################
# setup study design and random effect for estimating residual associations
studyDesign = data.frame(lake = as.factor(dat$lagoslakeid) )

rL1 = HmscRandomLevel(units = levels(studyDesign$lake))
m <- Hmsc(Y=ydat, XData=xdat, XFormula = ~ ws_area_ha + ws_lake_arearatio +
            lake_totalarea_ha + Burned + Month + ConnClass + mean_depth , 
           distr="normal",
          studyDesign = studyDesign, ranLevels=list(
            "lake"=rL1)
) 


thin <- 5
samples <- 1000
transient <- 500 * thin
nChains <- 2
verbose <- 500 * thin
m <- sampleMcmc(m, thin = thin, samples = samples, transient = transient, 
                nChains = nChains, verbose = verbose)

mpost <- convertToCodaObject(m)

#saveRDS(mpost, file="burn_coda.out.rds")
#saveRDS(m, file="burn_model.out.rds")

#mpost <- readRDS(file="burn_coda.out.rds")
#m <- readRDS(file="burn_model.out.rds")

# Diagnostics
effectiveSize(mpost$Beta)
gelman.diag(mpost$Beta, multivariate=FALSE)$psrf

# To assess the model’s explanatory power, we apply the evaluateModelFit function to the posterior predictive
# distribution simulated by the function computePredictedValues
preds = computePredictedValues(m)
pred_stats <- evaluateModelFit(hM = m, predY = preds)

R2s <- data.frame(colnames(ydat), round(pred_stats$R2,3)) 
colnames(R2s) <- c('Variable','R2')
head(R2s)
# write.csv(R2s, 'model_overall_R2.csv', row.names = F)

# Look at estimates of predictors
beta.estimates <- round(summary(mpost$Beta)$quantiles, 4)
# write.csv(beta.estimates, 'model_beta_estimates.csv')

############## Variance partitoning
VP = computeVariancePartitioning(m)


# Percentage variation explained by fixed and random effects
VP$vals
write.csv(VP$vals, 'variance_partitioning.csv')
vps <- VP$vals

plot.vc <- data.frame(VP$vals)
plot.vc$predictor <- row.names(plot.vc)

plot.long <- gather(plot.vc, variable, variance, logTP:WaterTemp_C,factor_key=TRUE)
plot.long$predictor <- as.factor(plot.long$predictor)
str(plot.long)

t1 <- plot.long %>% 
  filter(predictor=='Burned') %>% 
  arrange(-(variance))
t1

# Sort predictors as a function of % variance explained by Burned
plot.long <- plot.long %>%
  mutate(variable = fct_relevel(variable, 
                                 "SecchiDepth_m", "logDOC","logTP",
                                 "logTN", "logTSS", "logChloro", "pH", "WaterTemp_C"))

plot.long <- plot.long %>%
  mutate(predictor=fct_relevel(predictor,
                              'Burned','ConnClass','ws_lake_arearatio','lake_totalarea_ha',
                              'mean_depth','Month','Random: lake','ws_area_ha'))

# rename predictors for plotting
#plot.long$plot_predictor <- ifelse(plot.long$predictor=='ws_area_ha', 'Watershed area (ha)', plot.long$predictor)
#plot.long$plot_predictor <- ifelse(plot.long$predictor=='mean_depth', 'Max depth (m)', plot.long$predictor)

#jpeg('Figures/VariancePartitioning.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(plot.long, aes(fill=predictor, y=variance, x=variable)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Variance partitioning") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.65, hjust=0.6, color='black'),
        axis.text.y = element_text(size = 12, color='black'),
        axis.title = element_text(size = 12),
        axis.title.x=element_blank()) +
  ylab("Variance explained") +
  #xlab("Lake response variable")+
  scale_x_discrete(breaks=c("SecchiDepth_m","logDOC","logTN","logTP","logTSS","logChloro","pH","WaterTemp_C"),
                   labels=c("Secchi","DOC","TN","TP","TSS","Chlorophyll","pH","Temp"))+
  scale_fill_manual("Predictor", values=c('darkred','khaki','salmon','blue','navy','navajowhite4','gray70','aquamarine'),
                    labels=c('Burned','Connectivity','Drainage ratio','Lake area','Max depth','Month','Random:lake','Watershed area'))
#ggsave("burn_lakes.pdf", height=8, width=8, units="in")
#dev.off()
#write.csv(plot.long, file="Data/VariancePartitioning/burned_v_control.csv", row.names=F)

####################
# Re-order factor levels for plotting
# dat <- dat %>% 
#   mutate(Month_factor = Month_factor %>% 
#            fct_relevel("May", "Jun", "Jul", "Aug", "Sep") )

# 
# dat %>% 
# ggplot() +
#   geom_point(aes(ws_vbs_total_burn_pct,TN_ppb)) +
#   facet_wrap(~Month_factor,nrow=2,scales="free_y") +
#   labs(x="% Burn",y="log(TN)") +
#   scale_x_continuous(breaks=seq(0,100,by=10))+
#   theme(legend.position = "none") + 
#   theme_bw()
# ggsave("figures/burn_lakes.pdf", height=8, width=8, units="in")

### trying again with only burned lakes and different fire variables ###
# (can't use control lakes when using % burned vars, unless set all pcts to 0)
dat_burned <- subset(dat, Burned=='Burned')
# response matrix
ydat_burned <- dat_burned %>% 
  dplyr::select(logTP, logTN, logDOC, pH,
                logTSS, logChloro, SecchiDepth_m, WaterTemp_C,
  ) 
summary(ydat_burned)

# Predictor matrix
xdat_burned <- dat_burned %>% 
  dplyr::select(ws_area_ha, ws_lake_arearatio, lake_totalarea_ha, 
                Month, ConnClass, lagoslakeid, mean_depth, ws_vbs_total_burn_pct,
                ws_vbs_High_pct, ws_sbs_High_pct, buff100m_vbs_total_burn_pct,
                buff100m_vbs_High_pct, buff100m_sbs_High_pct)
summary(xdat_burned)

studyDesign2 = data.frame(lake = as.factor(dat_burned$lagoslakeid) )

rL2 = HmscRandomLevel(units = levels(studyDesign2$lake))

m2 <- Hmsc(Y=ydat_burned, XData=xdat_burned, XFormula = ~ ws_area_ha + ws_lake_arearatio +
             lake_totalarea_ha + ws_vbs_total_burn_pct + Month + ConnClass + mean_depth , 
           distr="normal",
           studyDesign = studyDesign2, ranLevels=list(
             "lake"=rL2)
) 


thin <- 5
samples <- 1000
transient <- 500 * thin
nChains <- 2
verbose <- 500 * thin
m2 <- sampleMcmc(m2, thin = thin, samples = samples, transient = transient, 
                 nChains = nChains, verbose = verbose)

mpost2 <- convertToCodaObject(m2)

#saveRDS(mpost, file="burn_coda.out.rds")
#saveRDS(m, file="burn_model.out.rds")

#mpost <- readRDS(file="burn_coda.out.rds")
#m <- readRDS(file="burn_model.out.rds")

# Diagnostics
effectiveSize(mpost2$Beta)
gelman.diag(mpost2$Beta, multivariate=FALSE)$psrf

# To assess the model’s explanatory power, we apply the evaluateModelFit function to the posterior predictive
# distribution simulated by the function computePredictedValues
preds2 = computePredictedValues(m2)
pred_stats2 <- evaluateModelFit(hM = m2, predY = preds2)

R2s2 <- data.frame(colnames(ydat_burned), round(pred_stats2$R2,3)) 
colnames(R2s2) <- c('Variable','R2')
head(R2s2)
# write.csv(R2s, 'model_overall_R2.csv', row.names = F)

# Look at estimates of predictors
beta.estimates2 <- round(summary(mpost2$Beta)$quantiles, 4)
# write.csv(beta.estimates, 'model_beta_estimates.csv')

############## Variance partitoning
VP2 = computeVariancePartitioning(m2)


# Percentage variation explained by fixed and random effects
VP2$vals
#write.csv(VP2$vals, 'variance_partitioning.csv')
vps2 <- VP2$vals

plot.vc2 <- data.frame(VP2$vals)
plot.vc2$predictor <- row.names(plot.vc2)

plot.long2 <- gather(plot.vc2, variable, variance, logTP:WaterTemp_C,factor_key=TRUE)
plot.long2$predictor <- as.factor(plot.long2$predictor)
str(plot.long2)

t2 <- plot.long2 %>% 
  filter(predictor=='ws_vbs_total_burn_pct') %>% 
  arrange(-(variance))
t2

# Sort predictors as a function of % variance explained by Burned
plot.long2 <- plot.long2 %>%
  mutate(variable = fct_relevel(variable, 
                                "logTP", "pH","logTN",
                                "logChloro", "logTSS", "logDOC", "SecchiDepth_m", "WaterTemp_C"))

plot.long2 <- plot.long2 %>%
  mutate(predictor=fct_relevel(predictor,
                               'ws_vbs_total_burn_pct','ConnClass','ws_lake_arearatio','lake_totalarea_ha',
                               'mean_depth','Month','Random: lake','ws_area_ha'))

#jpeg('Figures/VariancePartitioning_BurnedLakes.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(plot.long2, aes(fill=predictor, y=variance, x=variable)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Variance partitioning") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.65, hjust=0.6, color='black'),
        axis.text.y = element_text(size = 12, color='black'),
        axis.title = element_text(size = 12),
        axis.title.x=element_blank()) +
  ylab("Variance explained") +
  #xlab("Lake response variable")+
  scale_x_discrete(breaks=c("logTP","pH","logTN","logChloro","logTSS","logDOC","SecchiDepth_m","WaterTemp_C"),
                   labels=c("TP","pH","TN","Chlorophyll","TSS","DOC","Secchi","Temp"))+
  scale_fill_manual("Predictor", values=c('darkred','khaki','salmon','blue','navy','navajowhite4','gray70','aquamarine'),
                    labels=c('% WS Burn','Connectivity','Drainage ratio','Lake area','Max depth','Month','Random:lake','Watershed area'))
#ggsave("burn_lakes.pdf", height=8, width=8, units="in")
#dev.off()
#write.csv(plot.long2, file="Data/VariancePartitioning/pct_ws_burned.csv", row.names=F)

### trying AGAIN with only burned lakes - watershed high severity veg ###
studyDesign3 = data.frame(lake = as.factor(dat_burned$lagoslakeid) )

rL3 = HmscRandomLevel(units = levels(studyDesign3$lake))

m3 <- Hmsc(Y=ydat_burned, XData=xdat_burned, XFormula = ~ ws_area_ha + ws_lake_arearatio +
             lake_totalarea_ha + ws_vbs_High_pct + Month + ConnClass + mean_depth , 
           distr="normal",
           studyDesign = studyDesign3, ranLevels=list(
             "lake"=rL3)
) 


thin <- 5
samples <- 1000
transient <- 500 * thin
nChains <- 2
verbose <- 500 * thin
m3 <- sampleMcmc(m3, thin = thin, samples = samples, transient = transient, 
                 nChains = nChains, verbose = verbose)

mpost3 <- convertToCodaObject(m3)

#saveRDS(mpost, file="burn_coda.out.rds")
#saveRDS(m, file="burn_model.out.rds")

#mpost <- readRDS(file="burn_coda.out.rds")
#m <- readRDS(file="burn_model.out.rds")

# Diagnostics
effectiveSize(mpost3$Beta)
gelman.diag(mpost3$Beta, multivariate=FALSE)$psrf

# To assess the model’s explanatory power, we apply the evaluateModelFit function to the posterior predictive
# distribution simulated by the function computePredictedValues
preds3 = computePredictedValues(m3)
pred_stats3 <- evaluateModelFit(hM = m3, predY = preds3)

R2s3 <- data.frame(colnames(ydat_burned), round(pred_stats3$R2,3)) 
colnames(R2s3) <- c('Variable','R2')
head(R2s3)
# write.csv(R2s, 'model_overall_R2.csv', row.names = F)

# Look at estimates of predictors
beta.estimates3 <- round(summary(mpost3$Beta)$quantiles, 4)
# write.csv(beta.estimates, 'model_beta_estimates.csv')

############## Variance partitoning
VP3 = computeVariancePartitioning(m3)


# Percentage variation explained by fixed and random effects
VP3$vals
#write.csv(VP2$vals, 'variance_partitioning.csv')
vps3 <- VP3$vals

plot.vc3 <- data.frame(VP3$vals)
plot.vc3$predictor <- row.names(plot.vc3)

plot.long3 <- gather(plot.vc3, variable, variance, logTP:WaterTemp_C,factor_key=TRUE)
plot.long3$predictor <- as.factor(plot.long3$predictor)
str(plot.long3)

t3 <- plot.long3 %>% 
  filter(predictor=='ws_vbs_High_pct') %>% 
  arrange(-(variance))
t3

# Sort predictors as a function of % variance explained by Burned
plot.long3 <- plot.long3 %>%
  mutate(variable = fct_relevel(variable, 
                                "logTP", "logTSS","logTN",
                                "logChloro", "pH", "logDOC", "SecchiDepth_m", "WaterTemp_C"))

plot.long3 <- plot.long3 %>%
  mutate(predictor=fct_relevel(predictor,
                               'ws_vbs_High_pct','ConnClass','ws_lake_arearatio','lake_totalarea_ha',
                               'mean_depth','Month','Random: lake','ws_area_ha'))

#jpeg('Figures/VariancePartitioning_HSBurnedLakes.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(plot.long3, aes(fill=predictor, y=variance, x=variable)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Variance partitioning") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.65, hjust=0.6, color='black'),
        axis.text.y = element_text(size = 12, color='black'),
        axis.title = element_text(size = 12),
        axis.title.x=element_blank()) +
  ylab("Variance explained") +
  #xlab("Lake response variable")+
  scale_x_discrete(breaks=c("logTP","logTSS","logTN","logChloro","pH","logDOC","SecchiDepth_m","WaterTemp_C"),
                   labels=c("TP","TSS","TN","Chlorophyll","pH","DOC","Secchi","Temp"))+
  scale_fill_manual("Predictor", values=c('darkred','khaki','salmon','blue','navy','navajowhite4','gray70','aquamarine'),
                    labels=c('% WS HS veg','Connectivity','Drainage ratio','Lake area','Max depth','Month','Random:lake','Watershed area'))
#ggsave("burn_lakes.pdf", height=8, width=8, units="in")
#dev.off()
#write.csv(plot.long3, file="Data/VariancePartitioning/pct_ws_burned_HSveg.csv", row.names=F)

### trying again with only burned lakes: 100m buffer % burned ###
studyDesign4 = data.frame(lake = as.factor(dat_burned$lagoslakeid) )

rL4 = HmscRandomLevel(units = levels(studyDesign4$lake))

m4 <- Hmsc(Y=ydat_burned, XData=xdat_burned, XFormula = ~ ws_area_ha + ws_lake_arearatio +
             lake_totalarea_ha + buff100m_vbs_total_burn_pct + Month + ConnClass + mean_depth , 
           distr="normal",
           studyDesign = studyDesign4, ranLevels=list(
             "lake"=rL4)
) 

thin <- 5
samples <- 1000
transient <- 500 * thin
nChains <- 2
verbose <- 500 * thin
m4 <- sampleMcmc(m4, thin = thin, samples = samples, transient = transient, 
                 nChains = nChains, verbose = verbose)

mpost4 <- convertToCodaObject(m4)

#saveRDS(mpost, file="burn_coda.out.rds")
#saveRDS(m, file="burn_model.out.rds")

#mpost <- readRDS(file="burn_coda.out.rds")
#m <- readRDS(file="burn_model.out.rds")

# Diagnostics
effectiveSize(mpost4$Beta)
gelman.diag(mpost4$Beta, multivariate=FALSE)$psrf

# To assess the model’s explanatory power, we apply the evaluateModelFit function to the posterior predictive
# distribution simulated by the function computePredictedValues
preds4 = computePredictedValues(m4)
pred_stats4 <- evaluateModelFit(hM = m4, predY = preds4)

R2s4 <- data.frame(colnames(ydat_burned), round(pred_stats4$R2,3)) 
colnames(R2s4) <- c('Variable','R2')
head(R2s4)
# write.csv(R2s, 'model_overall_R2.csv', row.names = F)

# Look at estimates of predictors
beta.estimates4 <- round(summary(mpost4$Beta)$quantiles, 4)
# write.csv(beta.estimates, 'model_beta_estimates.csv')

############## Variance partitoning
VP4 = computeVariancePartitioning(m4)


# Percentage variation explained by fixed and random effects
VP4$vals
#write.csv(VP2$vals, 'variance_partitioning.csv')
vps4 <- VP4$vals

plot.vc4 <- data.frame(VP4$vals)
plot.vc4$predictor <- row.names(plot.vc4)

plot.long4 <- gather(plot.vc4, variable, variance, logTP:WaterTemp_C,factor_key=TRUE)
plot.long4$predictor <- as.factor(plot.long4$predictor)
str(plot.long4)

t4 <- plot.long4 %>% 
  filter(predictor=='buff100m_vbs_total_burn_pct') %>% 
  arrange(-(variance))
t4

# Sort predictors as a function of % variance explained by Burned
plot.long4 <- plot.long4 %>%
  mutate(variable = fct_relevel(variable, 
                                "logTP", "logTN","logTSS",
                                "pH", "logChloro", "SecchiDepth_m", "logDOC", "WaterTemp_C"))

plot.long4 <- plot.long4 %>%
  mutate(predictor=fct_relevel(predictor,
                               'buff100m_vbs_total_burn_pct','ConnClass','ws_lake_arearatio','lake_totalarea_ha',
                               'mean_depth','Month','Random: lake','ws_area_ha'))

# rename predictors for plotting
#plot.long$plot_predictor <- ifelse(plot.long$predictor=='ws_area_ha', 'Watershed area (ha)', plot.long$predictor)
#plot.long$plot_predictor <- ifelse(plot.long$predictor=='mean_depth', 'Max depth (m)', plot.long$predictor)

#jpeg('Figures/VariancePartitioning_BuffBurnedLakes.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(plot.long4, aes(fill=predictor, y=variance, x=variable)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Variance partitioning") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.65, hjust=0.6, color='black'),
        axis.text.y = element_text(size = 12, color='black'),
        axis.title = element_text(size = 12),
        axis.title.x=element_blank()) +
  ylab("Variance explained") +
  #xlab("Lake response variable")+
  scale_x_discrete(breaks=c("logTP","logTN","logTSS","pH","logChloro","SecchiDepth_m","logDOC","WaterTemp_C"),
                   labels=c("TP","TN","TSS","pH","Chlorophyll","Secchi","DOC","Temp"))+
  scale_fill_manual("Predictor", values=c('darkred','khaki','salmon','blue','navy','navajowhite4','gray70','aquamarine'),
                    labels=c('% Buff Burn','Connectivity','Drainage ratio','Lake area','Max depth','Month','Random:lake','Watershed area'))
#ggsave("burn_lakes.pdf", height=8, width=8, units="in")
#dev.off()
#write.csv(plot.long4, file="Data/VariancePartitioning/pct_buffer_burned.csv", row.names=F)

### trying again with only burned lakes - HS burn 100m buff (veg) ###
studyDesign5 = data.frame(lake = as.factor(dat_burned$lagoslakeid) )

rL5 = HmscRandomLevel(units = levels(studyDesign5$lake))

m5 <- Hmsc(Y=ydat_burned, XData=xdat_burned, XFormula = ~ ws_area_ha + ws_lake_arearatio +
             lake_totalarea_ha + buff100m_vbs_High_pct + Month + ConnClass + mean_depth , 
           distr="normal",
           studyDesign = studyDesign5, ranLevels=list(
             "lake"=rL5)
) 


thin <- 5
samples <- 1000
transient <- 500 * thin
nChains <- 2
verbose <- 500 * thin
m5 <- sampleMcmc(m5, thin = thin, samples = samples, transient = transient, 
                 nChains = nChains, verbose = verbose)

mpost5 <- convertToCodaObject(m5)

#saveRDS(mpost, file="burn_coda.out.rds")
#saveRDS(m, file="burn_model.out.rds")

#mpost <- readRDS(file="burn_coda.out.rds")
#m <- readRDS(file="burn_model.out.rds")

# Diagnostics
effectiveSize(mpost5$Beta)
gelman.diag(mpost5$Beta, multivariate=FALSE)$psrf

# To assess the model’s explanatory power, we apply the evaluateModelFit function to the posterior predictive
# distribution simulated by the function computePredictedValues
preds5 = computePredictedValues(m5)
pred_stats5 <- evaluateModelFit(hM = m5, predY = preds5)

R2s5 <- data.frame(colnames(ydat_burned), round(pred_stats5$R2,3)) 
colnames(R2s5) <- c('Variable','R2')
head(R2s5)
# write.csv(R2s, 'model_overall_R2.csv', row.names = F)

# Look at estimates of predictors
beta.estimates5 <- round(summary(mpost5$Beta)$quantiles, 4)
# write.csv(beta.estimates, 'model_beta_estimates.csv')

############## Variance partitoning
VP5 = computeVariancePartitioning(m5)

# Percentage variation explained by fixed and random effects
VP5$vals
#write.csv(VP2$vals, 'variance_partitioning.csv')
vps5 <- VP5$vals

plot.vc5 <- data.frame(VP5$vals)
plot.vc5$predictor <- row.names(plot.vc5)

plot.long5 <- gather(plot.vc5, variable, variance, logTP:WaterTemp_C,factor_key=TRUE)
plot.long5$predictor <- as.factor(plot.long5$predictor)
str(plot.long5)

t5 <- plot.long5 %>% 
  filter(predictor=='buff100m_vbs_High_pct') %>% 
  arrange(-(variance))
t5

# Sort predictors as a function of % variance explained by Burned
plot.long5 <- plot.long5 %>%
  mutate(variable = fct_relevel(variable, 
                                "logTP", "logTSS","logTN",
                                "logChloro", "pH", "logDOC", "SecchiDepth_m", "WaterTemp_C"))

plot.long5 <- plot.long5 %>%
  mutate(predictor=fct_relevel(predictor,
                               'buff100m_vbs_High_pct','ConnClass','ws_lake_arearatio','lake_totalarea_ha',
                               'mean_depth','Month','Random: lake','ws_area_ha'))

#jpeg('Figures/VariancePartitioning_BuffHSBurnedLakes.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(plot.long5, aes(fill=predictor, y=variance, x=variable)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Variance partitioning") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.65, hjust=0.6, color='black'),
        axis.text.y = element_text(size = 12, color='black'),
        axis.title = element_text(size = 12),
        axis.title.x=element_blank()) +
  ylab("Variance explained") +
  #xlab("Lake response variable")+
  scale_x_discrete(breaks=c("logTP","logTSS","logTN","logChloro","pH","logDOC","SecchiDepth_m","WaterTemp_C"),
                   labels=c("TP","TSS","TN","Chlorophyll","pH","DOC","Secchi","Temp"))+
  scale_fill_manual("Predictor", values=c('darkred','khaki','salmon','blue','navy','navajowhite4','gray70','aquamarine'),
                    labels=c('% Buff HS veg','Connectivity','Drainage ratio','Lake area','Max depth','Month','Random:lake','Watershed area'))
#ggsave("burn_lakes.pdf", height=8, width=8, units="in")
#dev.off()
#write.csv(plot.long5, file="Data/VariancePartitioning/pct_buffer_burned_HSveg.csv", row.names=F)


### trying again with only burned lakes - 100m buff HS burn (soil) ###
studyDesign6 = data.frame(lake = as.factor(dat_burned$lagoslakeid) )

rL6 = HmscRandomLevel(units = levels(studyDesign6$lake))

m6 <- Hmsc(Y=ydat_burned, XData=xdat_burned, XFormula = ~ ws_area_ha + ws_lake_arearatio +
             lake_totalarea_ha + buff100m_sbs_High_pct + Month + ConnClass + mean_depth , 
           distr="normal",
           studyDesign = studyDesign6, ranLevels=list(
             "lake"=rL6)
) 


thin <- 5
samples <- 1000
transient <- 500 * thin
nChains <- 2
verbose <- 500 * thin
m6 <- sampleMcmc(m6, thin = thin, samples = samples, transient = transient, 
                 nChains = nChains, verbose = verbose)

mpost6 <- convertToCodaObject(m6)

#saveRDS(mpost, file="burn_coda.out.rds")
#saveRDS(m, file="burn_model.out.rds")

#mpost <- readRDS(file="burn_coda.out.rds")
#m <- readRDS(file="burn_model.out.rds")

# Diagnostics
effectiveSize(mpost6$Beta)
gelman.diag(mpost6$Beta, multivariate=FALSE)$psrf

# To assess the model’s explanatory power, we apply the evaluateModelFit function to the posterior predictive
# distribution simulated by the function computePredictedValues
preds6 = computePredictedValues(m6)
pred_stats6 <- evaluateModelFit(hM = m6, predY = preds6)

R2s6 <- data.frame(colnames(ydat_burned), round(pred_stats6$R2,3)) 
colnames(R2s6) <- c('Variable','R2')
head(R2s6)
# write.csv(R2s, 'model_overall_R2.csv', row.names = F)

# Look at estimates of predictors
beta.estimates6 <- round(summary(mpost6$Beta)$quantiles, 4)
# write.csv(beta.estimates, 'model_beta_estimates.csv')

############## Variance partitoning
VP6 = computeVariancePartitioning(m6)

# Percentage variation explained by fixed and random effects
VP6$vals
#write.csv(VP2$vals, 'variance_partitioning.csv')
vps6 <- VP6$vals

plot.vc6 <- data.frame(VP6$vals)
plot.vc6$predictor <- row.names(plot.vc6)

plot.long6 <- gather(plot.vc6, variable, variance, logTP:WaterTemp_C,factor_key=TRUE)
plot.long6$predictor <- as.factor(plot.long6$predictor)
str(plot.long6)

t6 <- plot.long6 %>% 
  filter(predictor=='buff100m_sbs_High_pct') %>% 
  arrange(-(variance))
t6

# Sort predictors as a function of % variance explained by Burned
plot.long6 <- plot.long6 %>%
  mutate(variable = fct_relevel(variable, 
                                "logTP", "logTSS","logDOC",
                                "pH", "logChloro","logTN", "SecchiDepth_m", "WaterTemp_C"))

plot.long6 <- plot.long6 %>%
  mutate(predictor=fct_relevel(predictor,
                               'buff100m_sbs_High_pct','ConnClass','ws_lake_arearatio','lake_totalarea_ha',
                               'mean_depth','Month','Random: lake','ws_area_ha'))

#jpeg('Figures/VariancePartitioning_BuffHSBurnedsoilLakes.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(plot.long6, aes(fill=predictor, y=variance, x=variable)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Variance partitioning") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.65, hjust=0.6, color='black'),
        axis.text.y = element_text(size = 12, color='black'),
        axis.title = element_text(size = 12),
        axis.title.x=element_blank()) +
  ylab("Variance explained") +
  #xlab("Lake response variable")+
  scale_x_discrete(breaks=c("logTP","logTSS","logDOC","pH","logChloro", "logTN", "SecchiDepth_m","WaterTemp_C"),
                   labels=c("TP","TSS","DOC","pH","Chlorophyll","TN", "Secchi","Temp"))+
  scale_fill_manual("Predictor", values=c('darkred','khaki','salmon','blue','navy','navajowhite4','gray70','aquamarine'),
                    labels=c('% Buff HS soil','Connectivity','Drainage ratio','Lake area','Max depth','Month','Random:lake','Watershed area'))
#ggsave("burn_lakes.pdf", height=8, width=8, units="in")
#dev.off()
#write.csv(plot.long6, file="Data/VariancePartitioning/pct_buffer_burned_HSsoil.csv", row.names=F)

### trying again with only burned lakes: % watershed burned HS (soil) ###
studyDesign7 = data.frame(lake = as.factor(dat_burned$lagoslakeid) )

rL7 = HmscRandomLevel(units = levels(studyDesign7$lake))

m7 <- Hmsc(Y=ydat_burned, XData=xdat_burned, XFormula = ~ ws_area_ha + ws_lake_arearatio +
             lake_totalarea_ha + ws_sbs_High_pct + Month + ConnClass + mean_depth , 
           distr="normal",
           studyDesign = studyDesign7, ranLevels=list(
             "lake"=rL7)
) 


thin <- 5
samples <- 1000
transient <- 500 * thin
nChains <- 2
verbose <- 500 * thin
m7 <- sampleMcmc(m7, thin = thin, samples = samples, transient = transient, 
                 nChains = nChains, verbose = verbose)

mpost7 <- convertToCodaObject(m7)

#saveRDS(mpost, file="burn_coda.out.rds")
#saveRDS(m, file="burn_model.out.rds")

#mpost <- readRDS(file="burn_coda.out.rds")
#m <- readRDS(file="burn_model.out.rds")

# Diagnostics
effectiveSize(mpost7$Beta)
gelman.diag(mpost7$Beta, multivariate=FALSE)$psrf

# To assess the model’s explanatory power, we apply the evaluateModelFit function to the posterior predictive
# distribution simulated by the function computePredictedValues
preds7 = computePredictedValues(m7)
pred_stats7 <- evaluateModelFit(hM = m7, predY = preds7)

R2s7 <- data.frame(colnames(ydat_burned), round(pred_stats7$R2,3)) 
colnames(R2s7) <- c('Variable','R2')
head(R2s7)
# write.csv(R2s, 'model_overall_R2.csv', row.names = F)

# Look at estimates of predictors
beta.estimates7 <- round(summary(mpost7$Beta)$quantiles, 4)
# write.csv(beta.estimates, 'model_beta_estimates.csv')

############## Variance partitoning
VP7 = computeVariancePartitioning(m7)

# Percentage variation explained by fixed and random effects
VP7$vals
#write.csv(VP2$vals, 'variance_partitioning.csv')
vps7 <- VP7$vals

plot.vc7<- data.frame(VP7$vals)
plot.vc7$predictor <- row.names(plot.vc7)

plot.long7 <- gather(plot.vc7, variable, variance, logTP:WaterTemp_C,factor_key=TRUE)
plot.long7$predictor <- as.factor(plot.long7$predictor)
str(plot.long7)

t7 <- plot.long7 %>% 
  filter(predictor=='ws_sbs_High_pct') %>% 
  arrange(-(variance))
t7

# Sort predictors as a function of % variance explained by Burned
plot.long7 <- plot.long7 %>%
  mutate(variable = fct_relevel(variable, 
                                "logTP", "logTSS","logDOC",
                                "pH", "logTN","logChloro", "SecchiDepth_m", "WaterTemp_C"))

plot.long7 <- plot.long7 %>%
  mutate(predictor=fct_relevel(predictor,
                               'ws_sbs_High_pct','ConnClass','ws_lake_arearatio','lake_totalarea_ha',
                               'mean_depth','Month','Random: lake','ws_area_ha'))

#jpeg('Figures/VariancePartitioning_HSBurnedsoilLakes.jpeg',width = 7,height = 5,units = 'in',res=600)
ggplot(plot.long7, aes(fill=predictor, y=variance, x=variable)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Variance partitioning") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.65, hjust=0.6, color='black'),
        axis.text.y = element_text(size = 12, color='black'),
        axis.title = element_text(size = 12),
        axis.title.x=element_blank()) +
  ylab("Variance explained") +
  #xlab("Lake response variable")+
  scale_x_discrete(breaks=c("logTP","logTSS","logDOC","pH","logTN", "logChloro", "SecchiDepth_m","WaterTemp_C"),
                   labels=c("TP","TSS","DOC","pH","TN","Chlorophyll", "Secchi","Temp"))+
  scale_fill_manual("Predictor", values=c('darkred','khaki','salmon','blue','navy','navajowhite4','gray70','aquamarine'),
                    labels=c('% WS HS soil','Connectivity','Drainage ratio','Lake area','Max depth','Month','Random:lake','Watershed area'))
#ggsave("burn_lakes.pdf", height=8, width=8, units="in")
#dev.off()
#write.csv(plot.long7, file="Data/VariancePartitioning/pct_ws_burned_HSsoil.csv", row.names=F)


