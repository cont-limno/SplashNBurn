library(LAGOSNE)

#large <- read.csv('c:/Users/andre/OneDrive/Rapid/Megatables/LOCUS_GEO_DEPTH.csv')
large <- read.csv('Data/LAGOS/LAGOS_LOCUS_GEO_DEPTH_combined.csv')

burn <- subset(large, type %in% 'burned')
cont <- subset(large, type %in% 'control')

lakenames <- large[,c('lagoslakeid','type','Site')]

#### Main program ####
## LAGOSNE
#lagosne_get(version = '1.087.3', dest_folder = LAGOSNE:::lagos_path())
dt <- lagosne_load(version='1.087.3') 

# lake id (lagoslakeid) numbers for our study lakes (lakes we will sample)
sample_lakes_lagoslakeid <- c(223, 414, 430, 14101, 15139, 27717, 29814, 38199, 50749, 51824, 66287, 66295, 73430, 113774, 133329)
control_lakes_lagoslakeid<-c(63, 2867, 3550, 3839, 20349, 21441, 30871, 37142, 57275, 58010, 59050, 102493, 121215, 127093, 139473)

## epi_nutr
epi_nutr <- dt$epi_nutr

epi_nutr_burned <- subset(epi_nutr, lagoslakeid %in% sample_lakes_lagoslakeid)
epi_nutr_control <- subset(epi_nutr, lagoslakeid %in% control_lakes_lagoslakeid)

unique(epi_nutr_burned$lagoslakeid)
unique(epi_nutr_burned$programname)

unique(epi_nutr_control$lagoslakeid)
unique(epi_nutr_control$programname)

## check what's in preliminary LAGOS-US-LIMNO
# US NEW Exports and final tables v3
LAGOSUS <- readRDS("C:/Users/immcc/Downloads/Exports and final tables v3/Exports and final tables v3/allsites_alldepths_limno.rds")
LAGOSUS_burned <- subset(LAGOSUS, lagoslakeid %in% sample_lakes_lagoslakeid)
LAGOSUS_control <- subset(LAGOSUS, lagoslakeid %in% control_lakes_lagoslakeid)

LAGOSUS_burned$Year <- lubridate::year(LAGOSUS_burned$sample_date)

unique(LAGOSUS_burned$lagoslakeid)
unique(LAGOSUS_control$lagoslakeid)
table(LAGOSUS_burned$lagoslakeid)
table(LAGOSUS_control$lagoslakeid)

unique(LAGOSUS_burned$source_name)
unique(LAGOSUS_control$source_name)
table(LAGOSUS_burned$source_name)
table(LAGOSUS_control$source_name)

unique(LAGOSUS_burned$parameter_name)
unique(LAGOSUS_control$parameter_name)
table(LAGOSUS_burned$parameter_name)
table(LAGOSUS_control$parameter_name)

hist(LAGOSUS_burned$Year)
summary(LAGOSUS_burned$Year)
table(LAGOSUS_burned$Year)

LAGOSUS_burned_2019 <- subset(LAGOSUS_burned, Year==2019)
LAGOSUS_burned_20172019 <- subset(LAGOSUS_burned, Year %in% c(2017,2018,2019))
unique(LAGOSUS_burned_20172019$lagoslakeid)

# create a more readable table
LAGOSUS_burned_2019X <- LAGOSUS_burned_2019[,c(1,2,5,37,8,13,15,38)]
