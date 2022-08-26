
## set working directory ##
setwd("c:/Users/Owner/Desktop/RAPID/R/GRADIENTS")


## R libraries ##
library(dplyr)
library(readr)
library(ggplot2)


## lake id ##     15 burn / 15 control - 30 lakes
burn_lakes_lagoslakeid <- c(113774, 133329, 14101, 15139, 223, 27717, 29814, 38199, 414, 430, 50749, 51824, 66287, 66295, 73430)
control_lakes_lagoslakeid<-c(63, 2867, 3550, 3839, 20349, 21441, 30871, 37142, 57275, 58010, 59050, 102493, 121215, 127093, 139473)

burn_sites <- c('West Chub Lake', 'Wampas Lake', 'Fishtrap Lake', 'Fourth McDougal', 'Slate Lake', 'Middle McDougal', 'South McDougal', 'North McDougal', 'Lake Gegoka', 'Greenwood Lake', 'Unnamed Lake', 'Teamster Lake', 'Stony Lake', 'Sand Lake', 'Lil Chub Lake')
control_sites <- c('August Lake', 'Baird Lake', 'Two Deer Lake', 'Gypsy Lake', 'Grouse Lake', 'Dunnigan Lake', 'Gander Lake', 'Wadop Lake', 'Eighteen Lake', 'Highlife Lake', 'Cloquet Lake', 'Kitigan Lake', 'Mitiwan Lake', 'Rat Lake', 'Flathorn Lake')

burn_drainage_lagoslakeid <- c(14101, 223, 113774, 27717, 38199, 66295, 50749, 15139, 133329)
burn_isolated_lagoslakeid <- c(66287, 414, 29814, 51824, 73430)
control_drainage_lagoslakeid <- c(63, 57275, 121215, 20349, 3839, 58010, 21441, 127093, 430, 30871, 102493)
control_isolated_lagoslakeid <-c(139473, 2867, 37142, 59050, 3550)




## Data Sets ##
LAGOS_US_Table <- read.csv('c:/Users/HP/RAPID/LAGOS-US-Locus/LAGOS-US-Table.csv')                      #28 lakes
LAGOS_GEO_Table <- read.csv('c:/Users/HP/RAPID/LAGOS-GEO/LAGOS-GEO-Table.csv')                    #29 lakes
MasterTable <- read.csv('c:/Users/HP/RAPID/Datasets/Mastertable.csv')                      #28 lakes
soil_ws <- read.csv('c:/Users/HP/RAPID/Datasets/soil_ws_area_new.csv')                          #14 lakes
soil_buffer <- read.csv('c:/Users/HP/RAPID/Datasets/soil_buffer_area_new.csv')                  #8 lakes
vegetation_ws <- read.csv('c:/Users/HP/RAPID/Datasets/vegetation_ws_area_new.csv')              #14 lakes
vegetation_buffer <- read.csv('c:/Users/HP/RAPID/Datasets/vegetation_buffer_area_new.csv')      #8 lakes

may_june_july <- read.csv('c:/Users/HP/RAPID/May_June_July/may_june_july.csv')


## filter lake data ## 
may <- filter(may_june_july, Month == "may")
june <- filter(may_june_july, Month == "june")
july <- filter(may_june_july, Month == "july")

## merge tables ##
soil_ws_master <- merge(MasterTable, soil_ws, by = 'lagoslakeid', all = TRUE)                              #30 lakes
soil_buffer_master <- merge(MasterTable, soil_buffer, by = 'lagoslakeid', all = TRUE)                      #30 lakes
vegetation_ws_master <- merge(MasterTable, vegetation_ws, by = 'lagoslakeid', all = TRUE)                  #30 lakes
vegetation_buffer_master <- merge(MasterTable, vegetation_buffer, by = 'lagoslakeid', all = TRUE)          #30 lakes                                                                 #29 lakes

may_master <- merge(MasterTable, may, by = 'Site', all = TRUE)                                             #30 lakes
june_master <- merge(may_master, june, by = 'Site', all = TRUE)                                            #30 lakes
MegaTable <- merge(june_master, july, by = 'Site', all = TRUE)                                             #30 lakes
 
## Table Subsets ##
MegaTable_burned <- subset(MegaTable, lagoslakeid %in% burn_lakes_lagoslakeid, all = TRUE)                 #15 lakes
MegaTable_control <- subset(MegaTable, lagoslakeid %in% control_lakes_lagoslakeid, all = TRUE)             #15 lakes

may_burn <- subset(may, Site %in% burn_sites, all = TRUE)                                                  #14 lakes
may_cont <- subset(may, Site %in% control_sites, all = TRUE)                                               #14 lakes

june_burn <- subset(june, Site %in% burn_sites, all = TRUE)                                                #14 lakes
june_cont <- subset(june, Site %in% control_sites, all = TRUE)                                             #14 lakes

july_burn <- subset(july, Site %in% burn_sites, all = TRUE)                                                #13 lakes
july_cont <- subset(july, Site %in% control_sites, all = TRUE)                                             #14 lakes


## merge tables ##
soil_ws_may <- merge(soil_ws_master, may_burn, by = 'Site', all = TRUE)                                    #30 lakes
soil_buffer_may <- merge(soil_buffer_master, may_burn, by = 'Site', all = TRUE)                            #30 lakes
vegetation_ws_may <- merge(vegetation_ws_master, may_burn, by = 'Site', all = TRUE)                        #30 lakes
vegetation_buffer_may <- merge(vegetation_buffer_master, may_burn, by = 'Site', all = TRUE)                #30 lakes

soil_ws_june <- merge(soil_ws_master, june_burn, by = 'Site', all = TRUE)                                  #30 lakes
soil_buffer_june <- merge(soil_buffer_master, june_burn, by = 'Site', all = TRUE)                          #30 lakes
vegetation_ws_june <- merge(vegetation_ws_master, june_burn, by = 'Site', all = TRUE)                      #30 lakes
vegetation_buffer_june <- merge(vegetation_buffer_master, june_burn, by = 'Site', all = TRUE)              #30 lakes

soil_ws_july <- merge(soil_ws_master, july_burn, by = 'Site', all = TRUE)                                  #28 lakes
soil_buffer_july <- merge(soil_buffer_master, july_burn, by = 'Site', all = TRUE)                          #30 lakes
vegetation_ws_july <- merge(vegetation_ws_master, july_burn, by = 'Site', all = TRUE)                      #28 lakes
vegetation_buffer_july <- merge(vegetation_buffer_master, july_burn, by = 'Site', all = TRUE)              #30 lakes





## Gradients looking at TP ##
# May #
# Vegetation WS #
# Low Severity #
plot(vegetation_ws_may$low_severity_pct, vegetation_ws_may$TP, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'Total Phosphorus (ppb)', xlab = '% Watershed Low Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$low_severity_pct~vegetation_ws_may$TP), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$low_severity_pct, vegetation_ws_may$TP)
mvwsls.r <- cor(vegetation_ws_may$low_severity_pct, vegetation_ws_may$TP, use = 'pairwise.complete.obs')
mvwsls.p <- 0.05501
mylabel = bquote(r == .(format(mvwsls.r, digits = 4)))
text(x = 40, y = 40, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsls.p, digits = 4)))
text(x = 40, y = 37, labels = mylabel, col = 'black', cex = 0.8)

# Moderate Low Severity #
plot(vegetation_ws_may$moderate_low_severity_pct, vegetation_ws_may$TP, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'Total Phosphorus (ppb)', xlab = '% Watershed Moderate Low Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$moderate_low_severity_pct~vegetation_ws_may$TP), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$moderate_low_severity_pct, vegetation_ws_may$TP)
mvwsmls.r <- cor(vegetation_ws_may$moderate_low_severity_pct, vegetation_ws_may$TP, use = 'pairwise.complete.obs')
mvwsmls.p <- 0.0704
mylabel = bquote(r == .(format(mvwsmls.r, digits = 4)))
text(x = 40, y = 40, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsmls.p, digits = 4)))
text(x = 40, y = 37, labels = mylabel, col = 'black', cex = 0.8)

# Moderate High Severity #
plot(vegetation_ws_may$moderate_high_severity_pct, vegetation_ws_may$TP, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'Total Phosphorus (ppb)', xlab = '% Watershed Moderate High Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$moderate_high_severity_pct~vegetation_ws_may$TP), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$moderate_high_severity_pct, vegetation_ws_may$TP)
mvwsmhs.r <- cor(vegetation_ws_may$moderate_high_severity_pct, vegetation_ws_may$TP, use = 'pairwise.complete.obs')
mvwsmhs.p <- 0.04796
mylabel = bquote(r == .(format(mvwsmhs.r, digits = 4)))
text(x = 40, y = 40, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsmhs.p, digits = 4)))
text(x = 40, y = 37, labels = mylabel, col = 'black', cex = 0.8)

# High Severity #
plot(vegetation_ws_may$high_severity_pct, vegetation_ws_may$TP, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'Total Phosphorus (ppb)', xlab = '% Watershed High Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$high_severity_pct~vegetation_ws_may$TP), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$high_severity_pct, vegetation_ws_may$TP)
mvwshs.r <- cor(vegetation_ws_may$high_severity_pct, vegetation_ws_may$TP, use = 'pairwise.complete.obs')
mvwshs.p <- 0.004238
mylabel = bquote(r == .(format(mvwshs.r, digits = 4)))
text(x = 40, y = 37, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwshs.p, digits = 4)))
text(x = 40, y = 34, labels = mylabel, col = 'black', cex = 0.8)

# Total WS Burned #
plot(vegetation_ws_may$total_ws_burn_pct, vegetation_ws_may$TP, col = vegetation_ws_may$Color, xlim = c(0,100), ylim = c(15,50), ylab = 'Total Phosphorus (ppb)', xlab = 'Total % Watershed Burn', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$total_ws_burn_pct~vegetation_ws_may$TP), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$total_ws_burn_pct, vegetation_ws_may$TP)
mvwstb.r <- cor(vegetation_ws_may$total_ws_burn_pct, vegetation_ws_may$TP, use = 'pairwise.complete.obs')
mvwstb.p <- 0.01599
mylabel = bquote(r == .(format(mvwstb.r, digits = 4)))
text(x = 90, y = 37, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwstb.p, digits = 4)))
text(x = 90, y = 34, labels = mylabel, col = 'black', cex = 0.8)


# June #
# Vegetation WS #
# Low Severity #
plot(vegetation_ws_june$low_severity_pct, vegetation_ws_june$TP, col = vegetation_ws_june$Color, xlim = c(0,50), ylim = c(15,50), ylab = 'Total Phosphorus (ppb)', xlab = '% Watershed Low Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$low_severity_pct~vegetation_ws_june$TP), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$low_severity_pct, vegetation_ws_june$TP)
jvwsls.r <- cor(vegetation_ws_june$low_severity_pct, vegetation_ws_june$TP, use = 'pairwise.complete.obs')
jvwsls.p <- 0.0713
mylabel = bquote(r == .(format(jvwsls.r, digits = 4)))
text(x = 40, y = 37, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(jvwsls.p, digits = 4)))
text(x = 40, y = 34, labels = mylabel, col = 'black', cex = 0.8)

# Moderate Low Severity #
plot(vegetation_ws_june$moderate_low_severity_pct, vegetation_ws_june$TP, col = vegetation_ws_june$Color, xlim = c(0,50), ylim = c(15,50), ylab = 'Total Phosphorus (ppb)', xlab = '% Watershed Moderate Low Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$moderate_low_severity_pct~vegetation_ws_june$TP), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$moderate_low_severity_pct, vegetation_ws_june$TP)
mvwsmls.r <- cor(vegetation_ws_june$moderate_low_severity_pct, vegetation_ws_june$TP, use = 'pairwise.complete.obs')
mvwsmls.p <- 0.2545
mylabel = bquote(r == .(format(mvwsmls.r, digits = 4)))
text(x = 40, y = 37, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsmls.p, digits = 4)))
text(x = 40, y = 34, labels = mylabel, col = 'black', cex = 0.8)

# Moderate High Severity #
plot(vegetation_ws_june$moderate_high_severity_pct, vegetation_ws_june$TP, col = vegetation_ws_june$Color, xlim = c(0,50), ylim = c(15,50), ylab = 'Total Phosphorus (ppb)', xlab = '% Watershed Moderate High Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$moderate_high_severity_pct~vegetation_ws_june$TP), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$moderate_high_severity_pct, vegetation_ws_june$TP)
mvwsmhs.r <- cor(vegetation_ws_june$moderate_high_severity_pct, vegetation_ws_june$TP, use = 'pairwise.complete.obs')
mvwsmhs.p <- 0.08607
mylabel = bquote(r == .(format(mvwsmhs.r, digits = 4)))
text(x = 40, y = 37, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsmhs.p, digits = 4)))
text(x = 40, y = 34, labels = mylabel, col = 'black', cex = 0.8)

# High Severity #
plot(vegetation_ws_june$high_severity_pct, vegetation_ws_june$TP, col = vegetation_ws_june$Color, xlim = c(0,50), ylim = c(15,50), ylab = 'Total Phosphorus (ppb)', xlab = '% Watershed High Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$high_severity_pct~vegetation_ws_june$TP), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$high_severity_pct, vegetation_ws_june$TP)
mvwshs.r <- cor(vegetation_ws_june$high_severity_pct, vegetation_ws_june$TP, use = 'pairwise.complete.obs')
mvwshs.p <- 0.0636
mylabel = bquote(r == .(format(mvwshs.r, digits = 4)))
text(x = 45, y = 20, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwshs.p, digits = 4)))
text(x = 45, y = 17, labels = mylabel, col = 'black', cex = 0.8)

# Total WS Burned #
plot(vegetation_ws_june$total_ws_burn_pct, vegetation_ws_june$TP, col = vegetation_ws_june$Color, xlim = c(0,100), ylim = c(15,50), ylab = 'Total Phosphorus (ppb)', xlab = 'Total % Watershed Burn', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$total_ws_burn_pct~vegetation_ws_june$TP), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$total_ws_burn_pct, vegetation_ws_june$TP)
mvwstb.r <- cor(vegetation_ws_june$total_ws_burn_pct, vegetation_ws_june$TP, use = 'pairwise.complete.obs')
jvwstb.p <- 0.07036
mylabel = bquote(r == .(format(mvwstb.r, digits = 4)))
text(x = 90, y = 37, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwstb.p, digits = 4)))
text(x = 90, y = 34, labels = mylabel, col = 'black', cex = 0.8)





## Gradients looking at ANC (mg CaCO3/L) ##
# May #
# Vegetation WS #
# Low Severity #
plot(vegetation_ws_may$low_severity_pct, vegetation_ws_may$ANCmg, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'Acid Neutralizing Capacity (mg CaCO3/L)', xlab = '% Watershed Low Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$low_severity_pct~vegetation_ws_may$ANCmg), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$low_severity_pct, vegetation_ws_may$ANCmg)
mvwsls.r <- cor(vegetation_ws_may$low_severity_pct, vegetation_ws_may$ANCmg, use = 'pairwise.complete.obs')
mvwsls.p <- 0.6861
mylabel = bquote(r == .(format(mvwsls.r, digits = 4)))
text(x = 40, y = 17, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsls.p, digits = 4)))
text(x = 40, y = 14, labels = mylabel, col = 'black', cex = 0.8)

# Moderate Low Severity #
plot(vegetation_ws_may$moderate_low_severity_pct, vegetation_ws_may$ANCmg, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'Acid Neutralizing Capacity (mg CaCO3/L)', xlab = '% Watershed Moderate Low Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$moderate_low_severity_pct~vegetation_ws_may$ANCmg), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$moderate_low_severity_pct, vegetation_ws_may$ANCmg)
mvwsmls.r <- cor(vegetation_ws_may$moderate_low_severity_pct, vegetation_ws_may$ANCmg, use = 'pairwise.complete.obs')
mvwsmls.p <- 0.6357
mylabel = bquote(r == .(format(mvwsmls.r, digits = 4)))
text(x = 40, y = 17, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsmls.p, digits = 4)))
text(x = 40, y = 14, labels = mylabel, col = 'black', cex = 0.8)

# Moderate High Severity #
plot(vegetation_ws_may$moderate_high_severity_pct, vegetation_ws_may$ANCmg, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'Acid Neutralizing Capacity (mg CaCO3/L)', xlab = '% Watershed Moderate High Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$moderate_high_severity_pct~vegetation_ws_may$ANCmg), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$moderate_high_severity_pct, vegetation_ws_may$ANCmg)
mvwsmhs.r <- cor(vegetation_ws_may$moderate_high_severity_pct, vegetation_ws_may$ANCmg, use = 'pairwise.complete.obs')
mvwsmhs.p <- 0.5226
mylabel = bquote(r == .(format(mvwsmhs.r, digits = 4)))
text(x = 40, y = 17, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsmhs.p, digits = 4)))
text(x = 40, y = 14, labels = mylabel, col = 'black', cex = 0.8)

# High Severity #
plot(vegetation_ws_may$high_severity_pct, vegetation_ws_may$ANCmg, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'Acid Neutralizing Capacity (mg CaCO3/L)', xlab = '% Watershed High Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$high_severity_pct~vegetation_ws_may$ANCmg), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$high_severity_pct, vegetation_ws_may$ANCmg)
mvwshs.r <- cor(vegetation_ws_may$high_severity_pct, vegetation_ws_may$ANCmg, use = 'pairwise.complete.obs')
mvwshs.p <- 0.362
mylabel = bquote(r == .(format(mvwshs.r, digits = 4)))
text(x = 40, y = 17, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwshs.p, digits = 4)))
text(x = 40, y = 14, labels = mylabel, col = 'black', cex = 0.8)

# Total WS Burned #
plot(vegetation_ws_may$total_ws_burn_pct, vegetation_ws_may$ANCmg, col = vegetation_ws_may$Color, xlim = c(0,100), ylab = 'Acid Neutralizing Capacity (mg CaCO3/L)', xlab = 'Total % Watershed Burn', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$total_ws_burn_pct~vegetation_ws_may$ANCmg), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$total_ws_burn_pct, vegetation_ws_may$ANCmg)
mvwstb.r <- cor(vegetation_ws_may$total_ws_burn_pct, vegetation_ws_may$ANCmg, use = 'pairwise.complete.obs')
mvwstb.p <- 0.4729
mylabel = bquote(r == .(format(mvwstb.r, digits = 4)))
text(x = 90, y = 17, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwstb.p, digits = 4)))
text(x = 90, y = 14, labels = mylabel, col = 'black', cex = 0.8)


# June #
# Vegetation WS #
# Low Severity #
plot(vegetation_ws_june$low_severity_pct, vegetation_ws_june$ANCmg, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'Acid Neutralizing Capacity (mg CaCO3/L)', xlab = '% Watershed Low Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$low_severity_pct~vegetation_ws_june$ANCmg), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$low_severity_pct, vegetation_ws_june$ANCmg)
jvwsls.r <- cor(vegetation_ws_june$low_severity_pct, vegetation_ws_june$ANCmg, use = 'pairwise.complete.obs')
jvwsls.p <- 0.2718
mylabel = bquote(r == .(format(jvwsls.r, digits = 4)))
text(x = 40, y = 17, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(jvwsls.p, digits = 4)))
text(x = 40, y = 14, labels = mylabel, col = 'black', cex = 0.8)

# Moderate Low Severity #
plot(vegetation_ws_june$moderate_low_severity_pct, vegetation_ws_june$ANCmg, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'Acid Neutralizing Capacity (mg CaCO3/L)', xlab = '% Watershed Moderate Low Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$moderate_low_severity_pct~vegetation_ws_june$ANCmg), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$moderate_low_severity_pct, vegetation_ws_june$ANCmg)
jvwsmls.r <- cor(vegetation_ws_june$moderate_low_severity_pct, vegetation_ws_june$ANCmg, use = 'pairwise.complete.obs')
jvwsmls.p <- 0.3847
mylabel = bquote(r == .(format(jvwsmls.r, digits = 4)))
text(x = 40, y = 17, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(jvwsmls.p, digits = 4)))
text(x = 40, y = 14, labels = mylabel, col = 'black', cex = 0.8)

# Moderate High Severity #
plot(vegetation_ws_june$moderate_high_severity_pct, vegetation_ws_june$ANCmg, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'Acid Neutralizing Capacity (mg CaCO3/L)', xlab = '% Watershed Moderate High Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$moderate_high_severity_pct~vegetation_ws_june$ANCmg), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$moderate_high_severity_pct, vegetation_ws_june$ANCmg)
jvwsmhs.r <- cor(vegetation_ws_june$moderate_high_severity_pct, vegetation_ws_june$ANCmg, use = 'pairwise.complete.obs')
jvwsmhs.p <- 0.3018
mylabel = bquote(r == .(format(jvwsmhs.r, digits = 4)))
text(x = 40, y = 17, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(jvwsmhs.p, digits = 4)))
text(x = 40, y = 14, labels = mylabel, col = 'black', cex = 0.8)

# High Severity #
plot(vegetation_ws_june$high_severity_pct, vegetation_ws_june$ANCmg, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'Acid Neutralizing Capacity (mg CaCO3/L)', xlab = '% Watershed High Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$high_severity_pct~vegetation_ws_june$ANCmg), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$high_severity_pct, vegetation_ws_june$ANCmg)
jvwshs.r <- cor(vegetation_ws_june$high_severity_pct, vegetation_ws_june$ANCmg, use = 'pairwise.complete.obs')
jvwshs.p <- 0.2373
mylabel = bquote(r == .(format(jvwshs.r, digits = 4)))
text(x = 40, y = 22, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(jvwshs.p, digits = 4)))
text(x = 40, y = 19, labels = mylabel, col = 'black', cex = 0.8)

# Total WS Burned #
plot(vegetation_ws_june$total_ws_burn_pct, vegetation_ws_june$ANCmg, col = vegetation_ws_june$Color, xlim = c(0,100), ylab = 'Acid Neutralizing Capacity (mg CaCO3/L)', xlab = 'Total % Watershed Burn', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$total_ws_burn_pct~vegetation_ws_june$ANCmg), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$total_ws_burn_pct, vegetation_ws_june$ANCmg)
jvwstb.r <- cor(vegetation_ws_june$total_ws_burn_pct, vegetation_ws_june$ANCmg, use = 'pairwise.complete.obs')
jvwstb.p <- 0.249
mylabel = bquote(r == .(format(jvwstb.r, digits = 4)))
text(x = 90, y = 23, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(jvwstb.p, digits = 4)))
text(x = 90, y = 20, labels = mylabel, col = 'black', cex = 0.8)


# July #
# Vegetation WS #
# Low Severity #
plot(vegetation_ws_july$low_severity_pct, vegetation_ws_july$ANCmg, col = vegetation_ws_july$Color, xlim = c(0,50), ylab = 'Acid Neutralizing Capacity (mg CaCO3/L)', xlab = '% Watershed Low Burn Severity', main = 'July', pch = 20)
abline(lm(vegetation_ws_july$low_severity_pct~vegetation_ws_july$ANCmg), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_july$low_severity_pct, vegetation_ws_july$ANCmg)
juvwsls.r <- cor(vegetation_ws_july$low_severity_pct, vegetation_ws_july$ANCmg, use = 'pairwise.complete.obs')
juvwsls.p <- 0.08681
mylabel = bquote(r == .(format(juvwsls.r, digits = 4)))
text(x = 40, y = 27, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(juvwsls.p, digits = 4)))
text(x = 40, y = 24, labels = mylabel, col = 'black', cex = 0.8)

# Moderate Low Severity #
plot(vegetation_ws_july$moderate_low_severity_pct, vegetation_ws_july$ANCmg, col = vegetation_ws_july$Color, xlim = c(0,50), ylab = 'Acid Neutralizing Capacity (mg CaCO3/L)', xlab = '% Watershed Moderate Low Burn Severity', main = 'July', pch = 20)
abline(lm(vegetation_ws_july$moderate_low_severity_pct~vegetation_ws_july$ANCmg), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_july$moderate_low_severity_pct, vegetation_ws_july$ANCmg)
juvwsmls.r <- cor(vegetation_ws_july$moderate_low_severity_pct, vegetation_ws_july$ANCmg, use = 'pairwise.complete.obs')
juvwsmls.p <- 0.3108
mylabel = bquote(r == .(format(juvwsmls.r, digits = 4)))
text(x = 40, y = 27, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(juvwsmls.p, digits = 4)))
text(x = 40, y = 24, labels = mylabel, col = 'black', cex = 0.8)

# Moderate High Severity #
plot(vegetation_ws_july$moderate_high_severity_pct, vegetation_ws_july$ANCmg, col = vegetation_ws_july$Color, xlim = c(0,50), ylab = 'Acid Neutralizing Capacity (mg CaCO3/L)', xlab = '% Watershed Moderate High Burn Severity', main = 'July', pch = 20)
abline(lm(vegetation_ws_july$moderate_high_severity_pct~vegetation_ws_july$ANCmg), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_july$moderate_high_severity_pct, vegetation_ws_july$ANCmg)
juvwsmhs.r <- cor(vegetation_ws_july$moderate_high_severity_pct, vegetation_ws_july$ANCmg, use = 'pairwise.complete.obs')
juvwsmhs.p <- 0.1725
mylabel = bquote(r == .(format(juvwsmhs.r, digits = 4)))
text(x = 40, y = 27, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(juvwsmhs.p, digits = 4)))
text(x = 40, y = 24, labels = mylabel, col = 'black', cex = 0.8)

# High Severity #
plot(vegetation_ws_july$high_severity_pct, vegetation_ws_july$ANCmg, col = vegetation_ws_july$Color, xlim = c(0,50), ylab = 'Acid Neutralizing Capacity (mg CaCO3/L)', xlab = '% Watershed High Burn Severity', main = 'July', pch = 20)
abline(lm(vegetation_ws_july$high_severity_pct~vegetation_ws_july$ANCmg), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_july$high_severity_pct, vegetation_ws_july$ANCmg)
juvwshs.r <- cor(vegetation_ws_july$high_severity_pct, vegetation_ws_july$ANCmg, use = 'pairwise.complete.obs')
juvwshs.p <- 0.1646
mylabel = bquote(r == .(format(juvwshs.r, digits = 4)))
text(x = 40, y = 27, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(juvwshs.p, digits = 4)))
text(x = 40, y = 24, labels = mylabel, col = 'black', cex = 0.8)

# Total WS Burned #
plot(vegetation_ws_july$total_ws_burn_pct, vegetation_ws_july$ANCmg, col = vegetation_ws_july$Color, xlim = c(0,100), ylab = 'Acid Neutralizing Capacity (mg CaCO3/L)', xlab = 'Total % Watershed Burn', main = 'July', pch = 20)
abline(lm(vegetation_ws_july$total_ws_burn_pct~vegetation_ws_july$ANCmg), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_july$total_ws_burn_pct, vegetation_ws_july$ANCmg)
juvwstb.r <- cor(vegetation_ws_july$total_ws_burn_pct, vegetation_ws_july$ANCmg, use = 'pairwise.complete.obs')
juvwstb.p <- 0.1413
mylabel = bquote(r == .(format(juvwstb.r, digits = 4)))
text(x = 90, y = 27, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(juvwstb.p, digits = 4)))
text(x = 90, y = 24, labels = mylabel, col = 'black', cex = 0.8)




## Gradients looking at TN ##
# May #
# Vegetation WS #
# Low Severity #
plot(vegetation_ws_may$low_severity_pct, vegetation_ws_may$TN, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'Total Nitrogen (ppb)', xlab = '% Watershed Low Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$low_severity_pct~vegetation_ws_may$TN), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$low_severity_pct, vegetation_ws_may$TN)
mvwsls.r <- cor(vegetation_ws_may$low_severity_pct, vegetation_ws_may$TN, use = 'pairwise.complete.obs')
mvwsls.p <- 0.1939
mylabel = bquote(r == .(format(mvwsls.r, digits = 4)))
text(x = 40, y = 750, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsls.p, digits = 4)))
text(x = 40, y = 710, labels = mylabel, col = 'black', cex = 0.8)

# Moderate Low Severity #
plot(vegetation_ws_may$moderate_low_severity_pct, vegetation_ws_may$TN, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'Total Nitrogen (ppb)', xlab = '% Watershed Moderate Low Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$moderate_low_severity_pct~vegetation_ws_may$TN), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$moderate_low_severity_pct, vegetation_ws_may$TN)
mvwsmls.r <- cor(vegetation_ws_may$moderate_low_severity_pct, vegetation_ws_may$TN, use = 'pairwise.complete.obs')
mvwsmls.p <- 0.1457
mylabel = bquote(r == .(format(mvwsmls.r, digits = 4)))
text(x = 40, y = 750, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsmls.p, digits = 4)))
text(x = 40, y = 710, labels = mylabel, col = 'black', cex = 0.8)

# Moderate High Severity #
plot(vegetation_ws_may$moderate_high_severity_pct, vegetation_ws_may$TN, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'Total Nitrogen (ppb)', xlab = '% Watershed Moderate High Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$moderate_high_severity_pct~vegetation_ws_may$TN), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$moderate_high_severity_pct, vegetation_ws_may$TN)
mvwsmhs.r <- cor(vegetation_ws_may$moderate_high_severity_pct, vegetation_ws_may$TN, use = 'pairwise.complete.obs')
mvwsmhs.p <- 0.1001
mylabel = bquote(r == .(format(mvwsmhs.r, digits = 4)))
text(x = 40, y = 750, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsmhs.p, digits = 4)))
text(x = 40, y = 710, labels = mylabel, col = 'black', cex = 0.8)

# High Severity #
plot(vegetation_ws_may$high_severity_pct, vegetation_ws_may$TN, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'Total Nitrogen (ppb)', xlab = '% Watershed High Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$high_severity_pct~vegetation_ws_may$TN), col = 'red')
legend('bottomright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$high_severity_pct, vegetation_ws_may$TN)
mvwshs.r <- cor(vegetation_ws_may$high_severity_pct, vegetation_ws_may$TN, use = 'pairwise.complete.obs')
mvwshs.p <- 0.06124
mylabel = bquote(r == .(format(mvwshs.r, digits = 4)))
text(x = 40, y = 750, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwshs.p, digits = 4)))
text(x = 40, y = 710, labels = mylabel, col = 'black', cex = 0.8)

# Total WS Burned #
plot(vegetation_ws_may$total_ws_burn_pct, vegetation_ws_may$TN, col = vegetation_ws_may$Color, ylab = 'Total Nitrogen (ppb)', xlim = c(0,100), xlab = 'Total % Watershed Burn', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$total_ws_burn_pct~vegetation_ws_may$TN), col = 'red')
legend('bottomright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$total_ws_burn_pct, vegetation_ws_may$TN)
mvwstb.r <- cor(vegetation_ws_may$total_ws_burn_pct, vegetation_ws_may$TN, use = 'pairwise.complete.obs')
mvwstb.p <- 0.08295
mylabel = bquote(r == .(format(mvwstb.r, digits = 4)))
text(x = 90, y = 750, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwstb.p, digits = 4)))
text(x = 90, y = 710, labels = mylabel, col = 'black', cex = 0.8)


# June #
# Vegetation WS #
# Low Severity #
plot(vegetation_ws_june$low_severity_pct, vegetation_ws_june$TN, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'Total Nitrogen (ppb)', xlab = '% Watershed Low Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$low_severity_pct~vegetation_ws_june$TN), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$low_severity_pct, vegetation_ws_june$TN)
jvwsls.r <- cor(vegetation_ws_june$low_severity_pct, vegetation_ws_june$TN, use = 'pairwise.complete.obs')
jvwsls.p <- 0.4954
mylabel = bquote(r == .(format(jvwsls.r, digits = 4)))
text(x = 40, y = 800, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(jvwsls.p, digits = 4)))
text(x = 40, y = 750, labels = mylabel, col = 'black', cex = 0.8)

# Moderate Low Severity #
plot(vegetation_ws_june$moderate_low_severity_pct, vegetation_ws_june$TN, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'Total Nitrogen (ppb)', xlab = '% Watershed Moderate Low Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$moderate_low_severity_pct~vegetation_ws_june$TN), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$moderate_low_severity_pct, vegetation_ws_june$TN)
jvwsmls.r <- cor(vegetation_ws_june$moderate_low_severity_pct, vegetation_ws_june$TN, use = 'pairwise.complete.obs')
jvwsmls.p <- 0.4975
mylabel = bquote(r == .(format(jvwsmls.r, digits = 4)))
text(x = 40, y = 800, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(jvwsmls.p, digits = 4)))
text(x = 40, y = 750, labels = mylabel, col = 'black', cex = 0.8)

# Moderate High Severity #
plot(vegetation_ws_june$moderate_high_severity_pct, vegetation_ws_june$TN, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'Total Nitrogen (ppb)', xlab = '% Watershed Moderate High Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$moderate_high_severity_pct~vegetation_ws_june$TN), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$moderate_high_severity_pct, vegetation_ws_june$TN)
mvwsmhs.r <- cor(vegetation_ws_june$moderate_high_severity_pct, vegetation_ws_june$TN, use = 'pairwise.complete.obs')
mvwsmhs.p <- 0.4982
mylabel = bquote(r == .(format(mvwsmhs.r, digits = 4)))
text(x = 40, y = 800, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsmhs.p, digits = 4)))
text(x = 40, y = 750, labels = mylabel, col = 'black', cex = 0.8)

# High Severity #
plot(vegetation_ws_june$high_severity_pct, vegetation_ws_june$TN, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'Total Nitrogen (ppb)', xlab = '% Watershed High Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$high_severity_pct~vegetation_ws_june$TN), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$high_severity_pct, vegetation_ws_june$TN)
mvwshs.r <- cor(vegetation_ws_june$high_severity_pct, vegetation_ws_june$TN, use = 'pairwise.complete.obs')
mvwshs.p <- 0.2703
mylabel = bquote(r == .(format(mvwshs.r, digits = 4)))
text(x = 45, y = 800, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwshs.p, digits = 4)))
text(x = 45, y = 750, labels = mylabel, col = 'black', cex = 0.8)

# Total WS Burned #
plot(vegetation_ws_june$total_ws_burn_pct, vegetation_ws_june$TN, col = vegetation_ws_june$Color, xlim = c(0,100), ylab = 'Total Nitrogen (ppb)', xlab = 'Total % Watershed Burn', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$total_ws_burn_pct~vegetation_ws_june$TN), col = 'red')
legend('bottomright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$total_ws_burn_pct, vegetation_ws_june$TN)
jvwstb.r <- cor(vegetation_ws_june$total_ws_burn_pct, vegetation_ws_june$TN, use = 'pairwise.complete.obs')
jvwstb.p <- 0.3665
mylabel = bquote(r == .(format(jvwstb.r, digits = 4)))
text(x = 90, y = 800, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(jvwstb.p, digits = 4)))
text(x = 90, y = 750, labels = mylabel, col = 'black', cex = 0.8)



## Gradients looking at DOC ##
# May #
# Vegetation WS #
# Low Severity #
plot(vegetation_ws_may$low_severity_pct, vegetation_ws_may$DOC, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'DOC (ppm)', xlab = '% Watershed Low Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$low_severity_pct~vegetation_ws_may$DOC), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$low_severity_pct, vegetation_ws_may$DOC)
mvwsls.r <- cor(vegetation_ws_may$low_severity_pct, vegetation_ws_may$DOC, use = 'pairwise.complete.obs')
mvwsls.p <- 0.3576
mylabel = bquote(r == .(format(mvwsls.r, digits = 4)))
text(x = 40, y = 20, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsls.p, digits = 4)))
text(x = 40, y = 17, labels = mylabel, col = 'black', cex = 0.8)

# Moderate Low Severity #
plot(vegetation_ws_may$moderate_low_severity_pct, vegetation_ws_may$DOC, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'DOC (ppm)', xlab = '% Watershed Moderate Low Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$moderate_low_severity_pct~vegetation_ws_may$DOC), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$moderate_low_severity_pct, vegetation_ws_may$DOC)
mvwsmls.r <- cor(vegetation_ws_may$moderate_low_severity_pct, vegetation_ws_may$DOC, use = 'pairwise.complete.obs')
mvwsmls.p <- 0.3161
mylabel = bquote(r == .(format(mvwsmls.r, digits = 4)))
text(x = 40, y = 20, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsmls.p, digits = 4)))
text(x = 40, y = 17, labels = mylabel, col = 'black', cex = 0.8)

# Moderate High Severity #
plot(vegetation_ws_may$moderate_high_severity_pct, vegetation_ws_may$DOC, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'DOC (ppm)', xlab = '% Watershed Moderate High Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$moderate_high_severity_pct~vegetation_ws_may$DOC), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$moderate_high_severity_pct, vegetation_ws_may$DOC)
mvwsmhs.r <- cor(vegetation_ws_may$moderate_high_severity_pct, vegetation_ws_may$DOC, use = 'pairwise.complete.obs')
mvwsmhs.p <- 0.3654
mylabel = bquote(r == .(format(mvwsmhs.r, digits = 4)))
text(x = 40, y = 20, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsmhs.p, digits = 4)))
text(x = 40, y = 17, labels = mylabel, col = 'black', cex = 0.8)

# High Severity #
plot(vegetation_ws_may$high_severity_pct, vegetation_ws_may$DOC, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'DOC (ppm)', xlab = '% Watershed High Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$high_severity_pct~vegetation_ws_may$DOC), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$high_severity_pct, vegetation_ws_may$DOC)
mvwshs.r <- cor(vegetation_ws_may$high_severity_pct, vegetation_ws_may$DOC, use = 'pairwise.complete.obs')
mvwshs.p <- 0.3226
mylabel = bquote(r == .(format(mvwshs.r, digits = 4)))
text(x = 40, y = 19, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwshs.p, digits = 4)))
text(x = 40, y = 16, labels = mylabel, col = 'black', cex = 0.8)

# Total WS Burned #
plot(vegetation_ws_may$total_ws_burn_pct, vegetation_ws_may$DOC, col = vegetation_ws_may$Color, ylab = 'DOC (ppm)', xlim = c(0,100), xlab = 'Total % Watershed Burn', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$total_ws_burn_pct~vegetation_ws_may$DOC), col = 'red')
legend('bottomright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$total_ws_burn_pct, vegetation_ws_may$DOC)
mvwstb.r <- cor(vegetation_ws_may$total_ws_burn_pct, vegetation_ws_may$DOC, use = 'pairwise.complete.obs')
mvwstb.p <- 0.3149
mylabel = bquote(r == .(format(mvwstb.r, digits = 4)))
text(x = 80, y = 20, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwstb.p, digits = 4)))
text(x = 80, y = 17, labels = mylabel, col = 'black', cex = 0.8)


# June #
# Vegetation WS #
# Low Severity #
plot(vegetation_ws_june$low_severity_pct, vegetation_ws_june$DOC, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'DOC (ppm)', xlab = '% Watershed Low Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$low_severity_pct~vegetation_ws_june$DOC), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$low_severity_pct, vegetation_ws_june$DOC)
jvwsls.r <- cor(vegetation_ws_june$low_severity_pct, vegetation_ws_june$DOC, use = 'pairwise.complete.obs')
jvwsls.p <- 0.955
mylabel = bquote(r == .(format(jvwsls.r, digits = 4)))
text(x = 40, y = 20, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(jvwsls.p, digits = 4)))
text(x = 40, y = 17, labels = mylabel, col = 'black', cex = 0.8)

# Moderate Low Severity #
plot(vegetation_ws_june$moderate_low_severity_pct, vegetation_ws_june$DOC, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'DOC (ppm)', xlab = '% Watershed Moderate Low Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$moderate_low_severity_pct~vegetation_ws_june$DOC), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$moderate_low_severity_pct, vegetation_ws_june$DOC)
jvwsmls.r <- cor(vegetation_ws_june$moderate_low_severity_pct, vegetation_ws_june$DOC, use = 'pairwise.complete.obs')
jvwsmls.p <- 0.5661
mylabel = bquote(r == .(format(jvwsmls.r, digits = 4)))
text(x = 40, y = 20, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(jvwsmls.p, digits = 4)))
text(x = 40, y = 17, labels = mylabel, col = 'black', cex = 0.8)

# Moderate High Severity #
plot(vegetation_ws_june$moderate_high_severity_pct, vegetation_ws_june$DOC, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'DOC (ppm)', xlab = '% Watershed Moderate High Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$moderate_high_severity_pct~vegetation_ws_june$DOC), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$moderate_high_severity_pct, vegetation_ws_june$DOC)
mvwsmhs.r <- cor(vegetation_ws_june$moderate_high_severity_pct, vegetation_ws_june$DOC, use = 'pairwise.complete.obs')
mvwsmhs.p <- 0.8783
mylabel = bquote(r == .(format(mvwsmhs.r, digits = 4)))
text(x = 40, y = 20, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsmhs.p, digits = 4)))
text(x = 40, y = 17, labels = mylabel, col = 'black', cex = 0.8)

# High Severity #
plot(vegetation_ws_june$high_severity_pct, vegetation_ws_june$DOC, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'DOC (ppm)', xlab = '% Watershed High Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$high_severity_pct~vegetation_ws_june$DOC), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$high_severity_pct, vegetation_ws_june$DOC)
mvwshs.r <- cor(vegetation_ws_june$high_severity_pct, vegetation_ws_june$DOC, use = 'pairwise.complete.obs')
mvwshs.p <- 0.7412
mylabel = bquote(r == .(format(mvwshs.r, digits = 4)))
text(x = 45, y = 24, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwshs.p, digits = 4)))
text(x = 45, y = 21, labels = mylabel, col = 'black', cex = 0.8)

# Total WS Burned #
plot(vegetation_ws_june$total_ws_burn_pct, vegetation_ws_june$DOC, col = vegetation_ws_june$Color, xlim = c(0,100), ylab = 'DOC (ppm)', xlab = 'Total % Watershed Burn', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$total_ws_burn_pct~vegetation_ws_june$DOC), col = 'red')
legend('bottomright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$total_ws_burn_pct, vegetation_ws_june$DOC)
jvwstb.r <- cor(vegetation_ws_june$total_ws_burn_pct, vegetation_ws_june$DOC, use = 'pairwise.complete.obs')
jvwstb.p <- 0.7869
mylabel = bquote(r == .(format(jvwstb.r, digits = 4)))
text(x = 90, y = 25, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(jvwstb.p, digits = 4)))
text(x = 90, y = 21, labels = mylabel, col = 'black', cex = 0.8)




## Gradients looking at NH4N ##
# May #
# Vegetation WS #
# Low Severity #
plot(vegetation_ws_may$low_severity_pct, vegetation_ws_may$NH4N, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'NH4N (ppb)', xlab = '% Watershed Low Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$low_severity_pct~vegetation_ws_may$NH4N), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$low_severity_pct, vegetation_ws_may$NH4N)
mvwsls.r <- cor(vegetation_ws_may$low_severity_pct, vegetation_ws_may$NH4N, use = 'pairwise.complete.obs')
mvwsls.p <- 0.4135
mylabel = bquote(r == .(format(mvwsls.r, digits = 4)))
text(x = 40, y = 18, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsls.p, digits = 4)))
text(x = 40, y = 15, labels = mylabel, col = 'black', cex = 0.8)

# Moderate Low Severity #
plot(vegetation_ws_may$moderate_low_severity_pct, vegetation_ws_may$NH4N, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'NH4N (ppb)', xlab = '% Watershed Moderate Low Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$moderate_low_severity_pct~vegetation_ws_may$NH4N), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$moderate_low_severity_pct, vegetation_ws_may$NH4N)
mvwsmls.r <- cor(vegetation_ws_may$moderate_low_severity_pct, vegetation_ws_may$NH4N, use = 'pairwise.complete.obs')
mvwsmls.p <- 0.1998
mylabel = bquote(r == .(format(mvwsmls.r, digits = 4)))
text(x = 45, y = 15, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsmls.p, digits = 4)))
text(x = 45, y = 12, labels = mylabel, col = 'black', cex = 0.8)

# Moderate High Severity #
plot(vegetation_ws_may$moderate_high_severity_pct, vegetation_ws_may$NH4N, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'NH4N (ppb)', xlab = '% Watershed Moderate High Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$moderate_high_severity_pct~vegetation_ws_may$NH4N), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$moderate_high_severity_pct, vegetation_ws_may$NH4N)
mvwsmhs.r <- cor(vegetation_ws_may$moderate_high_severity_pct, vegetation_ws_may$NH4N, use = 'pairwise.complete.obs')
mvwsmhs.p <- 0.1087
mylabel = bquote(r == .(format(mvwsmhs.r, digits = 4)))
text(x = 40, y = 15, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsmhs.p, digits = 4)))
text(x = 40, y = 12, labels = mylabel, col = 'black', cex = 0.8)

# High Severity #
plot(vegetation_ws_may$high_severity_pct, vegetation_ws_may$NH4N, col = vegetation_ws_may$Color, xlim = c(0,50), ylab = 'NH4N (ppb)', xlab = '% Watershed High Burn Severity', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$high_severity_pct~vegetation_ws_may$NH4N), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$high_severity_pct, vegetation_ws_may$NH4N)
mvwshs.r <- cor(vegetation_ws_may$high_severity_pct, vegetation_ws_may$NH4N, use = 'pairwise.complete.obs')
mvwshs.p <- 0.3328
mylabel = bquote(r == .(format(mvwshs.r, digits = 4)))
text(x = 45, y = 17, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwshs.p, digits = 4)))
text(x = 45, y = 14, labels = mylabel, col = 'black', cex = 0.8)

# Total WS Burned #
plot(vegetation_ws_may$total_ws_burn_pct, vegetation_ws_may$NH4N, col = vegetation_ws_may$Color, ylab = 'NH4N (ppb)', xlim = c(0,100), xlab = 'Total % Watershed Burn', main = 'May', pch = 20)
abline(lm(vegetation_ws_may$total_ws_burn_pct~vegetation_ws_may$NH4N), col = 'red')
legend('bottomright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_may$total_ws_burn_pct, vegetation_ws_may$NH4N)
mvwstb.r <- cor(vegetation_ws_may$total_ws_burn_pct, vegetation_ws_may$NH4N, use = 'pairwise.complete.obs')
mvwstb.p <- 0.2284
mylabel = bquote(r == .(format(mvwstb.r, digits = 4)))
text(x = 80, y = 22, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwstb.p, digits = 4)))
text(x = 80, y = 19, labels = mylabel, col = 'black', cex = 0.8)


# June #
# Vegetation WS #
# Low Severity #
plot(vegetation_ws_june$low_severity_pct, vegetation_ws_june$NH4N, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'NH4N (ppb)', xlab = '% Watershed Low Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$low_severity_pct~vegetation_ws_june$NH4N), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$low_severity_pct, vegetation_ws_june$NH4N)
jvwsls.r <- cor(vegetation_ws_june$low_severity_pct, vegetation_ws_june$NH4N, use = 'pairwise.complete.obs')
jvwsls.p <- 0.418
mylabel = bquote(r == .(format(jvwsls.r, digits = 4)))
text(x = 40, y = 30, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(jvwsls.p, digits = 4)))
text(x = 40, y = 25, labels = mylabel, col = 'black', cex = 0.8)

# Moderate Low Severity #
plot(vegetation_ws_june$moderate_low_severity_pct, vegetation_ws_june$NH4N, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'NH4N (ppb)', xlab = '% Watershed Moderate Low Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$moderate_low_severity_pct~vegetation_ws_june$NH4N), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$moderate_low_severity_pct, vegetation_ws_june$NH4N)
jvwsmls.r <- cor(vegetation_ws_june$moderate_low_severity_pct, vegetation_ws_june$NH4N, use = 'pairwise.complete.obs')
jvwsmls.p <- 0.1646
mylabel = bquote(r == .(format(jvwsmls.r, digits = 4)))
text(x = 40, y = 30, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(jvwsmls.p, digits = 4)))
text(x = 40, y = 25, labels = mylabel, col = 'black', cex = 0.8)

# Moderate High Severity #
plot(vegetation_ws_june$moderate_high_severity_pct, vegetation_ws_june$NH4N, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'NH4N (ppb)', xlab = '% Watershed Moderate High Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$moderate_high_severity_pct~vegetation_ws_june$NH4N), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$moderate_high_severity_pct, vegetation_ws_june$NH4N)
mvwsmhs.r <- cor(vegetation_ws_june$moderate_high_severity_pct, vegetation_ws_june$NH4N, use = 'pairwise.complete.obs')
mvwsmhs.p <- 0.2303
mylabel = bquote(r == .(format(mvwsmhs.r, digits = 4)))
text(x = 40, y = 30, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwsmhs.p, digits = 4)))
text(x = 40, y = 25, labels = mylabel, col = 'black', cex = 0.8)

# High Severity #
plot(vegetation_ws_june$high_severity_pct, vegetation_ws_june$NH4N, col = vegetation_ws_june$Color, xlim = c(0,50), ylab = 'NH4N (ppb)', xlab = '% Watershed High Burn Severity', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$high_severity_pct~vegetation_ws_june$NH4N), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$high_severity_pct, vegetation_ws_june$NH4N)
mvwshs.r <- cor(vegetation_ws_june$high_severity_pct, vegetation_ws_june$NH4N, use = 'pairwise.complete.obs')
mvwshs.p <- 0.448
mylabel = bquote(r == .(format(mvwshs.r, digits = 4)))
text(x = 45, y = 32, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(mvwshs.p, digits = 4)))
text(x = 45, y = 28, labels = mylabel, col = 'black', cex = 0.8)

# Total WS Burned #
plot(vegetation_ws_june$total_ws_burn_pct, vegetation_ws_june$NH4N, col = vegetation_ws_june$Color, xlim = c(0,100), ylab = 'NH4N (ppb)', xlab = 'Total % Watershed Burn', main = 'June', pch = 20)
abline(lm(vegetation_ws_june$total_ws_burn_pct~vegetation_ws_june$NH4N), col = 'red')
legend('topright', legend=c('Burned Drainage Lakes', 'Burn Isolated Lakes'), pch=c(20, 20), col=c('red', 'blue'))
cor.test(vegetation_ws_june$total_ws_burn_pct, vegetation_ws_june$NH4N)
jvwstb.r <- cor(vegetation_ws_june$total_ws_burn_pct, vegetation_ws_june$NH4N, use = 'pairwise.complete.obs')
jvwstb.p <- 0.3
mylabel = bquote(r == .(format(jvwstb.r, digits = 4)))
text(x = 90, y = 22, labels = mylabel, col = 'black', cex = 0.8)
mylabel = bquote(p == .(format(jvwstb.p, digits = 4)))
text(x = 90, y = 18, labels = mylabel, col = 'black', cex = 0.8)
