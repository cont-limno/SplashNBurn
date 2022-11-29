########################### CART analysis #####################################
# Date: 11-28-22
# updated: 11-29-22
# Author: Ian McCullough, immccull@gmail.com
###############################################################################

#### R libraries ####
library(rpart)
library(rpart.plot)
library(dplyr)

#### Input data ####
setwd("C:/Users/immcc/Documents/SplashNBurn")

# water quality
waterquality <- read.csv("Data/WaterQuality/combined_lab_field_may_sep.csv")

# LAGOS variables
LAGOS <- read.csv("Data/LAGOS/LAGOS_LOCUS_GEO_DEPTH_combined.csv")

# fire variables
fire <- read.csv("Data/BurnSeverity/Ian_calculations/all_burn_severity_variables.csv")

LAGOS_fire <- merge(LAGOS, fire, by='lagoslakeid', all=T)

##### Main program ######
# with help from: https://rpubs.com/camguild/803096, https://www.statmethods.net/advstats/cart.html
# some explanation on pitfalls of overfitting: https://fengkehh.github.io/post/2017-06-30-overfitting/

# prepare water quality by subsetting across months
# note one missing lake in all but one month
may <- subset(waterquality, Month==5)
jun <- subset(waterquality, Month==6)
jul <- subset(waterquality, Month==7)
aug <- subset(waterquality, Month==8)
sep <- subset(waterquality, Month==9)

# bring in LAGOS and fire variables (do not need to use all in analysis)
maydf <- merge(may, LAGOS_fire, by='Site')
jundf <- merge(jun, LAGOS_fire, by='Site')
juldf <- merge(jul, LAGOS_fire, by='Site')
augdf <- merge(aug, LAGOS_fire, by='Site')
sepdf <- merge(sep, LAGOS_fire, by='Site')

maydf_burned <- subset(maydf, Type=='Burned')

## Try basic regression tree
test_tree <- rpart(TP_ppb ~ zMax_m + lake_waterarea_ha + ws_lake_arearatio + 
                     ws_vbs_High_pct + streams_all_mperha + lake_elevation_m + SecchiDepth_m,
                   method="anova", data=maydf_burned, minsplit=9)

summary(test_tree)
printcp(test_tree)
test_tree$variable.importance

# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(test_tree) # visualize cross-validation results  

# plot tree
par(mfrow=c(1,1))
par(xpd = NA)
plot(test_tree, uniform=T,
     main="Regression Tree for TP")
text(test_tree, use.n=T, all=T, cex=0.8)

rpart.plot(test_tree)

# try with just burned lakes/fire variables; doesn't seem to yield anything
fire_tree <- rpart(logTP ~ ws_vbs_High_pct + ws_vbs_total_burn_pct + ws_vbs_Low_pct,
                   method="anova", data=maydf, minsplit=3) #if set minsplit to 3, minbucket defaults to 1
summary(fire_tree)
plot(fire_tree, uniform=T)
text(fire_tree, use.n=T, all=T)
rpart.plot(fire_tree)


## Try basic classification tree
# use method='class', but would this even make sense?
# could try classifying burned vs. control, but not sure how much we'd learn from that
# because analysis would say something like, below xx ppb TP, lakes are mostly control; 
# wouldn't provide burn threshold that triggers differences between burned and control

test_tree <- rpart(Type ~ TP_ppb + ws_lake_arearatio + zMax_m + lake_waterarea_ha,
                   method="class", data=maydf)
test_tree
rpart.plot(test_tree)
par(xpd=NA)
plot(test_tree)
text(test_tree, use.n=T, all=T)
summary(test_tree)

