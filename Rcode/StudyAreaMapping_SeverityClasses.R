############## Study area mapping + burn severity classification ############
# Date created: 4-28-22
# updated: 5-10-22
# author: Ian McCullough (immccull@gmail.com)
#############################################################################

#### R libraries ####
library(dplyr)
library(raster)
library(rgdal)
library(rasterVis)
library(ggplot2)
setwd("C:/Users/immcc/Documents/SplashNBurn")

#### Input data ####
burned_lakes <- read.csv("Data/BurnedLakes/burned_lake_accessibility-12-3-21 CTF2-8-22-1500.csv")
control <- read.csv("Data/ControlLakes/control_candidates_2.8.22_attributes.csv")

depths <- read.csv("Data/lake_depth.csv")
depths <- depths[,c('lagoslakeid','lake_maxdepth_m','lake_meandepth_m')]

control <- merge(control, depths, by='lagoslakeid', all.x=T)
#write.csv(control, "C:/Users/immcc/Documents/SplashNBurn/Data/ControlLakes/Lakes10kmBuff_wDepth.csv", row.names=F)

# preliminary BAER burn severity
dNBR <- raster("Data/GTAC/greenwood_mn_preliminary_20210923/mn4755309164820210815_20210814_20210923_dnbr.tif")

# soil burn severity
sbs <- raster("Data/GTAC/greenwood_sbs/GREENWOOD_sbs.tif")

# LAGOS LOCUS v1.0
#lake_char <- read.csv("C:/Users/immcc/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LOCUS/LOCUS_v1.0/lake_characteristics.csv")
lake_info <- read.csv("C:/Users/immcc/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LOCUS/LOCUS_v1.0/lake_information.csv")

minnesota_lakes <- shapefile("Data/Minnesota_lakes_1ha/Minnesota_lakes_1ha.shp")
minnesota <- shapefile("Data/Minnesota_outline/Minnesota.shp")

burn_perimeter <- shapefile("Data/GreenwoodFirePolygon/GreenwoodFirePolygon.shp")
# not sample lakes; all lakes with watersheds intersecting burn zone
burned_watersheds <- shapefile("Data/BurnedWatersheds/BurnedWatersheds_9.13_intersect.shp")

#### Main program ####
#plot(minnesota)
#plot(burn_perimeter)

hist(control$Hectares, breaks=seq(0,80,10))
summary(control$Hectares)
hist(burned_lakes$lake_waterarea_ha)
summary(burned_lakes$lake_waterarea_ha)

hist(burned_lakes$ws_burn_pct, main='Proportion watershed burned',
     xlab='Proportion', breaks=seq(0,1,0.1))

# get burned and control lake polygons
burned_lakes_shp <- subset(minnesota_lakes, lagoslakei %in% burned_lakes$lagoslakeid)
plot(burned_lakes_shp, add=T, col='red')

control_lakes_shp <- subset(minnesota_lakes, lagoslakei %in% control$lagoslakeid)
plot(control_lakes_shp, add=T, col='dodgerblue')


plot(control_lakes_shp, col='dodgerblue')
plot(burn_perimeter, add=T)
plot(burned_lakes_shp, add=T, col='red')
#legend('topright', legend=c('Burned','Control'), col=c('red','dodgerblue'), pch=c(16,16))

# save burned and control lake shapefiles:
dsnname <- "Data/ControlLakes"
layername <- 'control_lakes'
#writeOGR(control_lakes_shp, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)

dsnname <- "Data/BurnedLakes"
layername <- 'burned_lakes'
#writeOGR(burned_lakes_shp, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)

# get burned and control lake watersheds
control_lakes_lagoslakeid <- control$lagoslakeid
burned_lakes_lagoslakeid <- burned_lakes$lagoslakeid

burned_lakes_watersheds <- subset(burned_watersheds, lagoslakei %in% burned_lakes_lagoslakeid)
dsnname <- "Data/BurnedWatersheds"
layername <- 'BurnedWatersheds_sample'
#writeOGR(burned_lakes_watersheds, dsn=dsnname, layer=layername, driver="ESRI Shapefile", overwrite_layer = T)

plot(burned_lakes_watersheds)
plot(burned_lakes_shp, add=T, col='dodgerblue')


### Burn severity ###

plot(dNBR)
hist(dNBR)

dNBR_unscaled <- dNBR/1000

jpeg('Figures/preliminary_dNBR.jpeg',width = 7,height = 5,units = 'in',res=600) 
  par(mfrow=c(1,2))
  par(mar=c(2,3,1,0.5)) #bot,left,top,right
  plot(dNBR_unscaled, main='dNBR')
  hist(dNBR_unscaled, main='dNBR', xlab='dNBR', xlim=c(-0.4, 1.7), breaks=seq(-0.4,1.7,0.1))
dev.off()

# reclassification into burn severity classes (using 1 for enhanced regrowth-high and 7 for high severity per Key and Benson 2006)

# reclass_table <- c(-0.5, -0.251, 1,
#                    -0.250, -0.101, 2,
#                    -0.100, 0.099, 3,
#                    0.100, 0.269, 4,
#                    0.270, 0.439, 5,
#                    0.440, 0.659, 6,
#                    0.660, 1.300, 7)

# use this table to deal with exact values not being reclassified
reclass_table <- c(-0.5, -0.250, 1,
                   -0.250, -0.100, 2,
                   -0.100, 0.100, 3,
                   0.100, 0.270, 4,
                   0.270, 0.440, 5,
                   0.440, 0.660, 6,
                   0.660, 1.300, 7)


reclass_matrix <- matrix(reclass_table, ncol=3, byrow=T)
dNBR_reclass <- reclassify(dNBR_unscaled, reclass_matrix)
dNBR_reclass_mask <- mask(dNBR_reclass, mask=burn_perimeter, inverse=F)
#writeRaster(dNBR_reclass_mask, filename='Data/GTAC/greenwood_mn_preliminary_20210923/dNBR_classified.tif', overwrite=T)

plot(dNBR_reclass)
plot(dNBR_reclass_mask)
plot(burn_perimeter, add=T)

## Nicer map, with help from: https://erinbecker.github.io/r-raster-vector-geospatial/02-raster-plot/index.html 

# First, to a SpatialPointsDataFrame
raster_pts <- rasterToPoints(dNBR_reclass_mask, spatial=T)
# Then to a 'conventional' dataframe
raster_df  <- data.frame(raster_pts)
names(raster_df) <- c('value','x','y','optional')
raster_df$severity_class <- NA

raster_df$severity_class <- ifelse(raster_df$value==1, 'ERH',NA)
raster_df$severity_class <- ifelse(raster_df$value==2, 'ERL',raster_df$severity_class)
raster_df$severity_class <- ifelse(raster_df$value==3, 'Unburned',raster_df$severity_class)
raster_df$severity_class <- ifelse(raster_df$value==4, 'Low',raster_df$severity_class)
raster_df$severity_class <- ifelse(raster_df$value==5, 'ModLow',raster_df$severity_class)
raster_df$severity_class <- ifelse(raster_df$value==6, 'ModHigh',raster_df$severity_class)
raster_df$severity_class <- ifelse(raster_df$value==7, 'High',raster_df$severity_class)
raster_df$severity_class_fac <- as.factor(raster_df$severity_class)
raster_df$severity_class_fac <- factor(raster_df$severity_class_fac, levels = c("ERH", "ERL", "Unburned", "Low", "ModLow","ModHigh", "High"))

colorz <- c("forestgreen", "lightgreen", "gray", "khaki","gold","orange","firebrick")
class_labels <- c('ERH','ERL','Unburned','Low','ModLow','ModHigh','High')

ggplot() +
  geom_raster(data = raster_df , aes(x = x, y = y, fill = severity_class_fac)) + 
  scale_fill_manual(values=colorz, name='Severity class', na.translate=F)+
  ggtitle("Greenwood Fire: Burn Severity Classes (dNBR)")+
  xlab('')+
  ylab('')+
  #geom_path(data=burned_lakes_shp, aes(long, lat, group=group), color='dodgerblue') + coord_equal()+
  geom_polygon(data=burned_lakes_shp, aes(long, lat, group=group), color='dodgerblue', fill='dodgerblue') + coord_equal()+
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())

## calculate zonal stats of burn severity for watersheds
#test <- subset(burned_lakes_watersheds, lagoslakei==burned_lakes_lagoslakeid[1])
#xx <- extract(dNBR_reclass_mask, test, fun=mean, na.rm=T) #works, but not necessarily what we want

# with help from: http://zevross.com/blog/2015/03/30/map-and-analyze-raster-data-in-r/
ext<-extract(dNBR_reclass_mask, burned_lakes_watersheds, method='simple') #returns large list
length(ext) # 15 elements (15 watersheds)

# Function to tabulate land use by region and return 
# a data.frame
tabFunc<-function(indx, extracted, region, regname) {
  dat<-as.data.frame(table(extracted[[indx]]))
  dat$lagoslakeid<-burned_lakes_watersheds[["lagoslakei"]][[indx]]
  return(dat)
}

# run through each region and compute a table of the count
# of raster cells by land use. Produces a list (see below)
tabs<-lapply(seq(ext), tabFunc, ext, burned_lakes_watersheds, "lagoslakeid")
tabs
tabs<-do.call("rbind",tabs )

tabs$Var1<-factor(tabs$Var1, levels=c(1,2,3,4,5,6,7), labels=class_labels)

# use the spread function from tidyr to make nicer
severity_pct <- tabs%>%
  group_by(lagoslakeid) %>% # group by watershed
  mutate(totcells=sum(Freq), # how many cells overall
         percent.area=round(Freq/totcells,2)) %>% #cells by severity class/total cells
  dplyr::select(-c(Freq, totcells)) %>% # there is a select func in raster so need to specify
  tidyr::spread(key=Var1, value=percent.area, fill=0) # make wide format

# check that rows sum to near 100%
rowSums(severity_pct[,c(2:7)])

par(mfrow=c(2,3))
for (i in 2:ncol(severity_pct)){
  hist(severity_pct[[i]], breaks=seq(0,1,0.1), main='Proportion class', xlab=class_labels[i], ylim=c(0,15))
}

#### soil burn severity 
sbs_mask <- mask(sbs, burn_perimeter, inverse=F)
plot(sbs_mask) #masking may not really be necessary; very similar to our burn perimeter

# First, to a SpatialPointsDataFrame
sbs_raster_pts <- rasterToPoints(sbs_mask, spatial=T)
# Then to a 'conventional' dataframe
sbs_raster_df  <- data.frame(sbs_raster_pts)
names(sbs_raster_df) <- c('value','x','y','optional')

sbs_raster_df$value <- ifelse(sbs_raster_df$value %in% c(1,2,3,4), sbs_raster_df$value, NA)
sbs_raster_df$severity_class <- NA
sbs_raster_df$severity_class <- ifelse(sbs_raster_df$value==1, 'Unburned', NA)
sbs_raster_df$severity_class <- ifelse(sbs_raster_df$value==2, 'Low', sbs_raster_df$severity_class)
sbs_raster_df$severity_class <- ifelse(sbs_raster_df$value==3, 'Moderate', sbs_raster_df$severity_class)
sbs_raster_df$severity_class <- ifelse(sbs_raster_df$value==4, 'High', sbs_raster_df$severity_class)
sbs_raster_df$severity_class_fac <- as.factor(sbs_raster_df$severity_class)

# reorder factor levels
sbs_raster_df$severity_class_fac <- factor(sbs_raster_df$severity_class_fac, levels = c("Unburned", "Low", "Moderate","High"))

colorz <- c("gray", "khaki", "orange","firebrick")

ggplot() +
  geom_raster(data = sbs_raster_df , aes(x = x, y = y, fill = severity_class_fac)) + 
  scale_fill_manual(values=colorz, name='Severity class', na.translate=F)+
  ggtitle("Greenwood Fire: Soil Burn Severity Classes")+
  xlab('')+
  ylab('')+
  #geom_path(data=burned_lakes_shp, aes(long, lat, group=group), color='dodgerblue') + coord_equal()+
  geom_polygon(data=burned_lakes_shp, aes(long, lat, group=group), color='dodgerblue', fill='dodgerblue') + coord_equal()+
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        #panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())

# with help from: http://zevross.com/blog/2015/03/30/map-and-analyze-raster-data-in-r/
ext<-extract(sbs_mask, burned_lakes_watersheds, method='simple') #returns large list
length(ext) # 15 elements (15 watersheds)

# run through each region and compute a table of the count
# of raster cells by land use. Produces a list (see below)
tabs<-lapply(seq(ext), tabFunc, ext, burned_lakes_watersheds, "lagoslakeid")
tabs
tabs<-do.call("rbind",tabs )
tabs <- subset(tabs, Var1 %in% c(1,2,3,4))

tabs$severity_class <- factor(tabs$Var1, levels=c(1,2,3,4), labels=c("Unburned", "Low", "Moderate","High"))

# use the spread function from tidyr to make nicer
severity_pct_soil <- tabs[,c(2:4)]%>%
  group_by(lagoslakeid) %>% # group by watershed
  mutate(totcells=sum(Freq), # how many cells overall
         percent.area=round(Freq/totcells,2)) %>% #cells by severity class/total cells
  dplyr::select(-c(Freq, totcells)) %>% # there is a select func in raster so need to specify
  tidyr::spread(key=severity_class, value=percent.area, fill=0) # make wide format

# check that rows sum to near 100%
rowSums(severity_pct_soil[,c(2:5)])

par(mfrow=c(2,2))
for (i in 2:ncol(severity_pct_soil)){
  hist(severity_pct[[i]], breaks=seq(0,1,0.1), main='Proportion class', ylim=c(0,15))
}



