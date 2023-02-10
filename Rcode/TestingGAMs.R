### GAMs ###

library(mgcv)

setwd("C:/Users/immcc/Documents/SplashNBurn")

TP_all <- read.csv('Data/WaterQuality/TP_all.csv')

# predictor variables to use
firevars <- c('ws_vbs_High_pct','ws_sbs_High_pct','ws_vbs_total_burn_pct',
              'buff100m_vbs_High_pct','buff100m_sbs_High_pct',
              'buff100m_vbs_total_burn_pct')

##########
## show basic linear model (no smoothing term)
plot(meanlogTP ~ ws_vbs_High_pct, data=TP_all, pch=16)
tp_gam <- gam(meanlogTP ~ ws_vbs_High_pct, data=TP_all,
              main='Mean TP')
summary(tp_gam)

## execute GAMS
# some run fine with defaults
gam1 <- gam(meanlogTP ~ s(ws_vbs_High_pct), data=TP_all)
summary(gam1)
plot(gam1)

gam2 <- gam(meanlogTP ~ s(ws_sbs_High_pct), data=TP_all)
summary(gam2)
plot(gam2)

gam3 <- gam(meanlogTP ~ s(ws_vbs_total_burn_pct), data=TP_all)
summary(gam3)
plot(gam3)

gam4 <- gam(meanlogTP ~ s(buff100m_vbs_High_pct), data=TP_all)
summary(gam4)
plot(gam4)

# this one returns an error with defaults
gam5 <- gam(meanlogTP ~ s(buff100m_sbs_High_pct), data=TP_all)
summary(gam5)
plot(gam5)

# seems OK
gam6 <- gam(meanlogTP ~ s(buff100m_vbs_total_burn_pct), data=TP_all)
summary(gam6)
plot(gam6)
