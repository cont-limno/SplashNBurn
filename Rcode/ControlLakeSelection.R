### 1-31-22 ###

library(dplyr)
setwd("C:/Users/immcc/Documents/SplashNBurn")

#### Input data ####
burned_lakes <- read.csv("Data/attribute_data.csv")
burned_lakes <- subset(burned_lakes, lake_connectivity_class %in% c('Isolated','Drainage','DrainageLk'))#get rid of headwaters; we are also lumping drainage lakes
control <- read.csv("Data/ControlLakes/Lakes10kmBuff.csv")

depths <- read.csv("Data/lake_depth.csv")
depths <- depths[,c('lagoslakeid','lake_maxdepth_m','lake_meandepth_m')]

control <- merge(control, depths, by='lagoslakeid', all.x=T)
#write.csv(control, "C:/Users/immcc/Documents/SplashNBurn/Data/ControlLakes/Lakes10kmBuff_wDepth.csv", row.names=F)

# LAGOS LOCUS v1.0
#lake_char <- read.csv("C:/Users/immcc/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LOCUS/LOCUS_v1.0/lake_characteristics.csv")
lake_info <- read.csv("C:/Users/immcc/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LOCUS/LOCUS_v1.0/lake_information.csv")


#### Main program ####
hist(control$Hectares, main='Lakes within 10km of burn', xlab='Hectares')

conn_sum <- control %>%
  group_by(Connectivi) %>%
  summarize(n=n())

control_isol <- subset(control, Connectivi=='Isolated')
control_drain <- subset(control, Connectivi %in% c('Drainage','DrainageLk'))

par(mfrow=c(2,1))
hist(control_isol$Hectares, xlim=c(0,80), xlab='Hectares', breaks=seq(0,80,5),
     main='Isolated')
hist(control_drain$Hectares, xlim=c(0,80), xlab='Hectares', breaks=seq(0,80,5),
     main='Drainage')

#### 2-7-22 ####
# Prioritizing control lakes with max depth (n=13) from LAGOS-US-DEPTH v1,0
maxdepth_lakes <- c(58010, 20349, 127093, 57275, 30871, 3839, 59050, 
                    121215, 37142, 2867, 63, 21441, 3550) #lagoslakeids

control_wDepth <- subset(control, lagoslakeid %in% maxdepth_lakes)
hist(control_wDepth$Hectares)
hist(control_wDepth$lake_maxdepth_m)
summary(control_wDepth$Hectares)
summary(control_wDepth$lake_maxdepth_m)

# comparative summary of burned lakes
burned_lakes <- merge(burned_lakes, depths, by='lagoslakeid', all.x=T)
burned_lakes <- burned_lakes[!(burned_lakes$lagoslakeid==259),] #BirchLake

hist(burned_lakes$lake_waterarea_ha)
hist(burned_lakes$lake_maxdepth_m)
summary(burned_lakes$lake_waterarea_ha)
summary(burned_lakes$lake_maxdepth_m)

par(mfrow=c(1,2))
hist(burned_lakes$lake_waterarea_ha, main='Burned lakes',xlab='ha', breaks=seq(0,600,10),
     ylim=c(0,15))
hist(control_wDepth$Hectares, main='Control', xlab='ha', breaks=seq(0,600,10),
     ylim=c(0,15))

## summarize burned lakes by area and conn class
burned_drainage <- subset(burned_lakes, lake_connectivity_class %in% c('Drainage','DrainageLk'))
burned_isolated <- subset(burned_lakes, lake_connectivity_class =='Isolated')

nrow(burned_drainage)/nrow(burned_lakes)
nrow(burned_isolated)/nrow(burned_lakes)

summary(burned_drainage$lake_waterarea_ha)
summary(burned_isolated$lake_waterarea_ha)

## summarize prioritized control lakes (ones with depth) by area and conn class
control_wDepth_drainage <- subset(control_wDepth, Connectivi %in% c('Drainage','DrainageLk'))
control_wDepth_isolated <- subset(control_wDepth, Connectivi %in% c('Isolated'))

nrow(control_wDepth_drainage)/nrow(control_wDepth)
nrow(control_wDepth_isolated)/nrow(control_wDepth)

summary(control_wDepth_drainage$Hectares)
summary(control_wDepth_isolated$Hectares)

hist(burned_drainage$lake_waterarea_ha)
hist(control_wDepth_drainage$Hectares)

hist(burned_isolated$lake_waterarea_ha)
hist(control_wDepth_isolated$Hectares)

#### random selection of remaining control lakes, proportional to proportions of drainage and isolated lakes in burned dataset 
## ISOLATED
control_isolated <- subset(control, Connectivi=='Isolated')
control_isolated <- subset(control_isolated, !(lagoslakeid %in% maxdepth_lakes))#make sure don't choose ones already chose
set.seed(100)
control_isolated_random <- control_isolated[sample(nrow(control_isolated), 3),] #need 3 more to make right proportion (already accounting for ones selected nonrandomly due to depth)

control_isolated_full <- rbind.data.frame(control_wDepth_isolated, control_isolated_random)

hist(burned_isolated$lake_waterarea_ha, main='Burned', xlab='ha', breaks=seq(0,60,2),
     ylim=c(0,2))
mtext(side=3,'Isolated')
hist(control_isolated_full$Hectares, main='Control', xlab='ha', breaks=seq(0,60,2),
     ylim=c(0,2))
mtext(side=3,'Isolated')

summary(burned_isolated$lake_waterarea_ha)
summary(control_isolated_full$Hectares)

## DRAINAGE
control_drainage <- subset(control, Connectivi %in% c('Drainage','DrainageLk'))
control_drainage <- subset(control_drainage, !(lagoslakeid %in% maxdepth_lakes))#make sure don't choose ones already chose
set.seed(95)
control_drainage_random <- control_drainage[sample(nrow(control_drainage), 8),] #need 8 more to make right proportion (already accounting for ones selected nonrandomly due to depth)

control_drainage_full <- rbind.data.frame(control_wDepth_drainage, control_drainage_random)

hist(burned_drainage$lake_waterarea_ha, main='Burned', xlab='ha', breaks=seq(0,600,10),
     ylim=c(0,10))
mtext(side=3,'Drainage')
hist(control_drainage_full$Hectares, main='Control', xlab='ha', breaks=seq(0,600,10),
     ylim=c(0,10))
mtext(side=3,'Drainage')

summary(burned_drainage$lake_waterarea_ha)
summary(control_drainage_full$Hectares)

# combine all control lakes
all_control <- rbind.data.frame(control_drainage_full, control_isolated_full)

# get coordinates
coords <- lake_info[,c('lagoslakeid','lake_lat_decdeg','lake_lon_decdeg')]
all_control <- merge(all_control, coords, by='lagoslakeid', all.x=T)
#write.csv(all_control, file='Data/ControlLakes/control_candidates_2.7.22.csv', row.names=F)
