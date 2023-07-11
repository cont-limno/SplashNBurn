## Get lat/lon coordinates for study lakes
# Date: 7-11-23

setwd("C:/Users/immcc/Documents/SplashNBurn")

lake_info <- read.csv("C:/Users/immcc/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LOCUS/LOCUS_v1.0/lake_information.csv")
waterquality <- read.csv("Data/WaterQuality/combined_lab_field_may_sep.csv")

lagoslakeids <- waterquality$lagoslakeid

lake_info_RAPID <- lake_info[,c("lagoslakeid", "lake_lat_decdeg","lake_lon_decdeg")]
lake_info_RAPID <- subset(lake_info_RAPID, lagoslakeid %in% lagoslakeids)

lakenames <- waterquality[,c('lagoslakeid','Site')]
lakenames <- unique(lakenames)

lake_info_RAPID <- merge(lake_info_RAPID, lakenames, by='lagoslakeid')
#write.csv(lake_info_RAPID , "Data/lake_locations.csv", row.names=F)
