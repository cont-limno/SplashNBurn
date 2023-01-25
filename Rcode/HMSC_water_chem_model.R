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

# load data ---------------------------------------------------------------
chem_dat = read_csv("../data/combined_lab_field_may_sep_NEWJan13.csv")
str(chem_dat)
dim(chem_dat)

burn_dat = read_csv("../data/all_burn_severity_variables.csv")
str(burn_dat)
dim(burn_dat)


locus_dat = read_csv("../data/LAGOS_LOCUS_GEO_DEPTH_combined.csv")
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
            lake_totalarea_ha + Burned + Month + ConnClass, 
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

saveRDS(mpost, file="burn_coda.out.rds")
saveRDS(m, file="burn_model.out.rds")

mpost <- readRDS(file="burn_coda.out.rds")
m <- readRDS(file="burn_model.out.rds")

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
                                 "SecchiDepth_m", "logDOC","logTN",
                                 "logTP", "logTSS", "logChloro", "pH", "WaterTemp_C"))

ggplot(plot.long, aes(fill=predictor, y=variance, x=variable)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Variance partitioning") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.65, hjust=0.6),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12)) +
  ylab("Variance explained") +
  xlab("Lake response variable") 
ggsave("burn_lakes.pdf", height=8, width=8, units="in")


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

