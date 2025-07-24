#' @author Sabrina Madsen-Colford
#' 
#' This code combines data from the Global Man-made Impervious Surfaces (GMIS), 
#' the City of Toronto impermeable surface (Toronto ISA), the Canadian Annual Crop 
#' Inventory (ACI), and the Southern Ontario Land Resource Information System
#' (SOLRIS V.3) to estimate impervious surfaces in the Greater Ontario Area
#' 
#' GMIS data:
#'  https://gis.earthdata.nasa.gov/portal/home/item.html?id=38f233d35da34e0cada61bc19faa0819
#' Toronto ISA data: 
#'  https://open.toronto.ca/dataset/topographic-mapping-impermeable-surface/
#' ACI data:
#'  https://www.agr.gc.ca/atlas/apps/aef/main/index_en.html?AGRIAPP=23
#' SOLRIS data:
#'  https://www.arcgis.com/home/item.html?id=0279f65b82314121b5b5ec93d76bc6ba
#' 
#' Portion of the codes with *** are sections the user should change

memory.limit(size=5e5)
library("raster")
library("ggplot2")
library("shapefiles")
library("rgdal")

# *** CHANGE PATHS & FILE NAMES
imported_raster_GMIS=raster('E:/Research/Impermeable_Surface_data/GMIS_HBASE/gmis_impervious_surface_percentage/17T_gmis_impervious_surface_percentage_geographic_30m.tif')
imported_raster_GMIS_err=raster('E:/Research/Impermeable_Surface_data/GMIS_HBASE/gmis_standard_error_of_impervious_surface_percentage/17T_gmis_standard_error_of_impervious_surface_percentage_geographic_30m.tif')

imported_raster_Toronto=raster('permeability_wgs84.tif') 
#last updated 2019 not sure what year's data it was created from
imported_raster_aci_2021=raster('E:/Research/Impermeable_Surface_data/ACI/aci_2021_on.tif') #updated yearly
imported_raster_SOLRIS=raster('E:/Research/Impermeable_Surface_data/SOLRIS_V3/SOLRIS_Version_3_0_LAMBERT.tif')
#Only covers data from 2000-2015

# *** Optional: plots to visualize datasets:
#plot(imported_raster_GMIS)
#plot(imported_raster_GMIS_err)
#plot(imported_raster_Toronto)
#plot(imported_raster_aci_2021)
#plot(imported_raster_SOLRIS)

# *** End of optional

GMIS_crs = crs(imported_raster_GMIS) #same as MODIS!
Toronto_crs='+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs'
aci_crs = '+proj=aea +lat_0=40 +lon_0=-96 +lat_1=44.75 +lat_2=55.75 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'
SOLRIS_crs = '+proj=lcc +lat_0=0 +lon_0=-85 +lat_1=44.5 +lat_2=53.5 +x_0=930000 +y_0=6430000 +ellps=GRS80 +units=m +no_defs'
MODIS_crs = '+proj=longlat +datum=WGS84 +no_defs'

# Import bounding box shape file of your region *** CHANGE PATH/FILE NAME ***
bound_box_0 = readOGR(dsn="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/shapefiles", layer='Entire_GTA_500m_BB')
# *** OR UNCOMMENT LINE BELOW TO MAKE A NEW BOUNDING BOX
#bound_box_0 = raster::extent(-80.9, -78.3, 42.4, 44.7) # regional extent

# Transform bounding box to other CRS's
bound_box_Toronto = spTransform(bound_box_0, Toronto_crs)
bound_box_GMIS = spTransform(bound_box_0, GMIS_crs)
bound_box_SOLRIS = spTransform(bound_box_0, SOLRIS_crs)
bound_box_aci = spTransform(bound_box_0, aci_crs)

# Crop data to bounding box
Torontocrop = crop(imported_raster_Toronto,bound_box_Toronto)
GMIScrop = crop(imported_raster_GMIS,bound_box_GMIS)
GMISerr_crop = crop(imported_raster_GMIS_err,bound_box_GMIS)
ACIcrop_2021 = crop(imported_raster_aci_2021,bound_box_aci)
SOLRIScrop = crop(imported_raster_SOLRIS,bound_box_SOLRIS)


# No difference between cropping first then aggregating and vice-versa except 
# faster to crop first

# *** If this is your first time running aggregate Toronto fluxes
Toronto_aggregated<-aggregate(Torontocrop,60,fun=mean) #aggregate to 30m resolution so it is easier to work with
Toronto_aggregated<-(1-Toronto_aggregated)*100 #change permeable area to % impermeable

# *** Otherwise load in Toronto fluxes (saves a lot of time):
Toronto_aggregated <- raster("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/Toronto_ISA_30m.tif")

GMIS_imperm=GMIScrop
GMIS_imperm[GMIS_imperm==200]=0
GMIS_imperm[GMIS_imperm==255]=NA

GMISerr_imperm=GMISerr_crop
GMISerr_imperm[GMISerr_imperm==255]=NA

# *** Optional: plot GMIS impermeable area:
#par(mar=c(3,3,3,0))
#plot(GMIS_imperm, main='GMIS Impermeable Area')

#par(mar=c(3,3,3,0))
#plot(GMISerr_imperm, main='GMIS Impermeable Area Standard Error')
# *** End optional

GMIS_frac_err=GMIS_imperm
GMIS_frac_err[GMIS_imperm==0]=NA
GMIS_frac_err=GMISerr_imperm/GMIS_frac_err
# *** Optional: plot GMIS fractional error:
#plot(GMIS_frac_err) # Average fractional error of non-zero GMIS data is 0.68, 
                    # median=0.61, but values are very variable
# *** End of optional

SOLRIS_imperm=SOLRIScrop
SOLRIS_imperm[SOLRIScrop==203]=100
SOLRIS_imperm[SOLRIScrop==201]=100
SOLRIS_imperm[SOLRIScrop==250]=SOLRIS_imperm[SOLRIScrop==250]*NA
SOLRIS_imperm[SOLRIS_imperm!=100 & SOLRIScrop!=250]=0
SOLRIS_imperm[is.na(SOLRIScrop)]=-999 #set US values to -999 to identify them

# *** Optional: plot SOLRIS Impermeable urban areas:
#par(mar=c(3,3,3,0))
#plot(SOLRIS_imperm, main='SOLRIS Impermeable Area')
#plot(SOLRIScrop==250, main='SOLRIS Unclassified Area')
#plot(SOLRIScrop==202, main='SOLRIS Previous Urban')
# *** End optional

#create a raster
x <-raster()
#set the number of columns, rows, and extent
#GTA500m
x <- raster(ncol=ncol(ACIcrop_2021), nrow=nrow(ACIcrop_2021), xmn=xmin(ACIcrop_2021),
            xmx=xmax(ACIcrop_2021), ymn=ymin(ACIcrop_2021), ymx=ymax(ACIcrop_2021))
res(x)==30 # Check the resolution is 30m
# Check the number of cells is 85412112 for the GTA
ncell(x) == ncell(ACIcrop_2021)
# set the coordinate reference system (CRS) (define the projection)
projection(x) <- "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=44.75 +lat_2=55.75 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# give x the same values as ACIcrop
values(x)<-values(ACIcrop_2021)
x  # Optional: print raster info

ACI_imperm_2021=x
ACI_imperm_2021[x==34]=100
ACI_imperm_2021[x==35]=100 #Greenhouses, not sure if this should be impermeable or not...
ACI_imperm_2021[ACI_imperm_2021!=100]=0
rm(x)

# *** Optional: Plot ACI Urban
#par(mar=c(3,3,3,0))
#plot(ACI_imperm_2021, main='ACI Urban and Greenhouses')
#plot(ACIcrop_2021==35, main='ACI Greenhouses') 
# *** End optional

# Project ACI to MODIS CRS
#aci_crs2="+proj=aea +datum=WGS84 +units=m +no_defs"
ACI_imperm_WGS84_2021 = projectRaster(ACI_imperm_2021, crs=MODIS_crs, method='ngb')

# *** Optional: Plot ACI urban in lat/lon
#par(mar=c(3,3,3,0))
#plot(ACI_imperm_WGS84_2021, main='ACI 2021 Impermeable Surface (proj=WGS84)')
# *** End Optional


SOLRIS_imperm_WGS84 = projectRaster(SOLRIS_imperm, crs=MODIS_crs, method='ngb')
# *** If using GMIS uncomment this line:
SOLRIS_resample_WGS84_GMIS <- resample(SOLRIS_imperm_WGS84,GMIScrop,method='ngb')
# ***If not using GMIS uncomment this line:
#SOLRIS_resample_WGS84 <- resample(SOLRIS_imperm_WGS84,ACI_imperm_WGS84,method='ngb')

# *** Optional plot SOLRIS Impervious urban in lat/lon
#plot(SOLRIS_resample_WGS84_GMIS, main='SOLRIS impermeable Surface (proj=WGS84)')
# ** End optional

#ACI does not have any NA values, only 0's

ACI_resample_WGS84_GMIS_2021 <- resample(ACI_imperm_WGS84_2021,GMIScrop,method='ngb')
# *** Optional plot:
#plot(ACI_resample_WGS84_GMIS_2021, main='ACI 2021 impermeable Surface (proj=WGS84)')

Toronto_imperm_wgs84 = projectRaster(Toronto_aggregated, crs=MODIS_crs, method='ngb')
# *** If using GMIS:
Toronto_resample_wgs84_GMIS<-resample(Toronto_imperm_wgs84,GMIScrop,method='ngb')
# *** If not using GMIS:
#Toronto_resample_wgs84<-resample(Toronto_imperm_wgs84,ACI_imperm_WGS84,method='ngb')

# *** Optional plot:
#plot(Toronto_resample_wgs84_GMIS,main='Toronto Impermeable Area (proj=WGS84)')

#Anywhere considered urban by ACI is replaced by SOLRIS urban pervious or urban impervious
ACI_SOLRIS_WGS84_GMIS_2021 <- ACI_resample_WGS84_GMIS_2021 #ACI_imperm_WGS84
ACI_SOLRIS_WGS84_GMIS_2021[SOLRIS_resample_WGS84_GMIS==-999]=NA
ACI_SOLRIS_WGS84_GMIS_2021[ACI_resample_WGS84_GMIS_2021==100 & is.na(SOLRIS_resample_WGS84_GMIS)==FALSE]=SOLRIS_resample_WGS84_GMIS[ACI_resample_WGS84_GMIS_2021==100 & is.na(SOLRIS_resample_WGS84_GMIS)==FALSE]
ACI_SOLRIS_WGS84_GMIS_2021[ACI_SOLRIS_WGS84_GMIS_2021==-999]=NA

# *** Optional plot:
#plot(ACI_SOLRIS_WGS84_GMIS_2021,main='2021 ACI & SOLRIS ISA')

# *** Optional plots:
#Toronto_imperm_WGS84 <- projectRaster(Toronto_aggregated, crs=MODIS_crs)
#GMIS_resample_WGS84 <- resample(GMIS_imperm, SOLRIS_resample_WGS84_GMIS, method='ngb')
#plot(Toronto_resample_wgs84_GMIS, main='Toronto Impermeable Percentage (proj=WGS84)')
#plot(GMIS_resample_WGS84, main='GMIS Percentage (proj=WGS84)')
#plot((Toronto_resample_wgs84-GMIS_resample_WGS84),main='Toronto impermeable - GMIS %')
##On average Toronto impermeable surface pixels are 6.59% more impervious
## Median difference is 3.59%
# *** End of optional plots

# Calculate the median & mean ISA values when ACI & SOLRIS say urban impervious
med_Toronto_SOLRIS_2021<-median(Toronto_resample_wgs84_GMIS[ACI_SOLRIS_WGS84_GMIS_2021==100],na.rm=TRUE) #63.97% #sd=29.27%
mean_Toronto_SOLRIS_2021<-mean(Toronto_resample_wgs84_GMIS[ACI_SOLRIS_WGS84_GMIS_2021==100],na.rm=TRUE) #60.52%

med_GMIS_SOLRIS_2021<-median(GMIS_imperm[ACI_SOLRIS_WGS84_GMIS_2021==100],na.rm=TRUE) #31% #sd=32.09%
mean_GMIS_SOLRIS_2021<-mean(GMIS_imperm[ACI_SOLRIS_WGS84_GMIS_2021==100],na.rm=TRUE) #35.13% 

mean_GMIS_ACI<-mean(GMIS_imperm[ACI_resample_WGS84_GMIS_2021==100],na.rm=TRUE) #30.18% #sd=31.82%
med_GMIS_ACI<-median(GMIS_imperm[ACI_resample_WGS84_GMIS_2021==100],na.rm=TRUE) #22%


# *** Uncomment to calculate Toronto-ACI-SOLRIS ISA (no GMIS)
#Toronto_ACI_SOLRIS_2021<-ACI_SOLRIS_WGS84_2021
#Toronto_ACI_SOLRIS_2021[ACI_SOLRIS_WGS84_2021==100 & is.na(Toronto_resample_wgs84)]=med_Toronto_SOLRIS_2021
#Toronto_ACI_SOLRIS_2021[is.na(Toronto_resample_wgs84)==FALSE]<-Toronto_resample_wgs84[is.na(Toronto_resample_wgs84)==FALSE]
## *** Optional plot: ISA using Toronto ACI & SOLRIS but not GMIS
#plot(Toronto_ACI_SOLRIS_2021,main='2021 Toronto, ACI, & SOLRIS Impervious (proj=WGS84)')
# *** End of uncomment

# IF ACI and SOLRIS say it is urban impervious but GMIS says 0% ISA, 
# replace with median GMIS ISA for urban impervious pixels in the domain 
GMIS_ACI_SOLRIS_2021<-ACI_SOLRIS_WGS84_GMIS_2021
GMIS_ACI_SOLRIS_2021[ACI_SOLRIS_WGS84_GMIS_2021==100 & GMIS_imperm==0]=med_GMIS_SOLRIS_2021
GMIS_ACI_SOLRIS_2021[ACI_SOLRIS_WGS84_GMIS_2021==100 & GMIS_imperm>0]=GMIS_imperm[ACI_SOLRIS_WGS84_GMIS_2021==100 & GMIS_imperm>0]
GMIS_ACI_SOLRIS_2021[is.na(ACI_SOLRIS_WGS84_GMIS_2021)]<-GMIS_imperm[is.na(ACI_SOLRIS_WGS84_GMIS_2021)]
GMIS_ACI_SOLRIS_2021[GMIS_ACI_SOLRIS_2021==-999]=NA
## *** Optional plot: ISA using ACI, SOLRIS & GMIS but not Toronto ISA:
#plot(GMIS_ACI_SOLRIS_2021,main='GMIS, 2021 ACI, & SOLRIS Impervious (proj=WGS84)')

# *** Uncomment to calculate GMIS-ACI impervious surface (no Toronto or SOLRIS)
#GMIS_ACI<-ACI_resample_WGS84
#GMIS_ACI[ACI_resample_WGS84==100 & GMIS_imperm==0]=med_GMIS_ACI
#GMIS_ACI[ACI_resample_WGS84==100 & GMIS_imperm>0]=GMIS_imperm[ACI_resample_WGS84==100 & GMIS_imperm>0]
#GMIS_ACI[is.na(ACI_SOLRIS_WGS84)]<-GMIS_imperm[is.na(ACI_SOLRIS_WGS84)]
#plot(GMIS_ACI,main='GMIS & ACI Impervious (proj=WGS84)')
# *** End of uncomment

# Replace GMIS-ACI-SOLRIS with Toronto ISA where it is available
GMIS_Toronto_ACI_SOLRIS_2021<-GMIS_ACI_SOLRIS_2021
GMIS_Toronto_ACI_SOLRIS_2021[is.na(Toronto_resample_wgs84_GMIS)==FALSE]<-Toronto_resample_wgs84_GMIS[is.na(Toronto_resample_wgs84_GMIS)==FALSE]
plot(GMIS_Toronto_ACI_SOLRIS_2021,main='GMIS-Toronto-ACI2021-SOLRIS ISA')
#Average difference between GMIS_ACI & GMIS_ACI_SOLRIS is 0.18% with sd:3.60%

LC = raster('C:/Users/kitty/Documents/Research/SIF/SMUrF/data/MCD12Q1/Toronto/MCD12Q1.061_LC_Type1_doy2021001_aid0001.tif')
crs(LC)<-MODIS_crs

# *** Optional (only needed if using Toronto dataset without GMIS)

#Toronto_vals<-values(Toronto_resample_wgs84)#values(Toronto_WGS84_crop)
#ACI_vals<-values(ACI_resample_WGS84) #values(ACI_lcc_crop)
#plot(Toronto_vals[ACI_vals==100],ACI_vals[ACI_vals==100])
##check the median value of ISA from the Toronto dataset when the ISA from ACI is 100
#median(Toronto_vals[ACI_vals==100],na.rm=TRUE) #63.47 (mean=59.85, sd=29.55)
#median(Toronto_vals[ACI_vals==0],na.rm=TRUE) #1.53 (mean=29.55, sd=30.9)

##Set ACI 100% ISA values to the median Toronto values
#ACI_resample[ACI_resample==100]<-63
#plot(ACI_resample,main='ACI & SOLRIS Impermeable Area with 63% impervious')

#plot(Toronto_resample-ACI_resample,main='Toronto ISA - ACI & SOLRIS ISA with 63% impervious')
##The median difference in values is 1.17 (mean=1.25, sd=31.04)
##If use 75% instead get median difference of -8.81 (mean=-14.29,SD=34.56)
##If use 60% instead get median difference of 0.61 (mean=-1.21, SD=33.04)

# *** end of optional

# Resample data to 500m resolution:

LC_crop <- crop(LC,bound_box_0)

# *** Uncomment If using GMIS only
#GMIS_resample_mod <- resample(GMIS_imperm,LC_crop) 
#GMIS_resample_mod[GMIS_resample_mod<0]<-NA
#GMISerr_resample_mod <- resample(GMISerr_imperm,LC_crop)
#GMISerr_resample_mod[GMISerr_resample_mod<0] <- NA
#plot(GMIS_resample_mod, main='Aggregated GMIS ISA %')
#plot(GMISerr_resample_mod, main='Aggregated GMIS ISA % Error')
# *** End of uncomment

# *** Uncomment if just want aggregated Toronto ISA
Toronto_resample_mod <- resample(Toronto_resample_wgs84_GMIS,LC_crop)
# *** Optional plot:
plot(Toronto_resample_mod, main='Aggregated City of Toronto Impermeable Surface area %')
# *** End of uncomment

# *** Uncomment if not using GMIS:
#Toronto_ACI_SOLRIS_mod_2021 <- resample(Toronto_ACI_SOLRIS_2021,LC_crop)
## *** Optional plot:
#plot(Toronto_ACI_SOLRIS_mod_2021, main='Aggregated 2021 Toronto-ACI-SOLRIS Impermeable Surface area %')
# *** End of uncomment

# *** Uncomment for GMIS-ACI-SOLRIS (not using TOronto ISA):
GMIS_ACI_SOLRIS_resample_mod <- resample(GMIS_ACI_SOLRIS_2021,LC_crop)
GMIS_ACI_SOLRIS_resample_mod[GMIS_ACI_SOLRIS_resample_mod<0]<-NA
## *** Optional plot:
#plot(GMIS_ACI_SOLRIS_resample_mod_2021, main='Aggregated GMIS-2021ACI-SOLRIS ISA %')
# *** End of uncomment

# *** Uncomment if only using GMIS & ACI:
#GMIS_ACI_resample_mod <- resample(GMIS_ACI, LC_crop)
#GMIS_ACI_resample_mod[GMIS_ACI_resample_mod<0]<-NA
## *** Optional plot:
#plot(GMIS_ACI_resample_mod, main='Aggregated GMIS-ACI Impermeable Surface area %')
# *** End of uncomment

# Using Toronto GMIS ACI & SOLRIS (version used in Madsen-Colford et al. 2025):
GMIS_Toronto_ACI_SOLRIS_resample_mod_2021 <- resample(GMIS_Toronto_ACI_SOLRIS_2021, LC_crop)
GMIS_Toronto_ACI_SOLRIS_resample_mod_2021[GMIS_Toronto_ACI_SOLRIS_resample_mod_2021<0]<-0
# *** Optionoal plot:
plot(GMIS_Toronto_ACI_SOLRIS_resample_mod_2021, main='Aggregated GMIS-Toronto-2021ACI-SOLRIS ISA %')

##When Toronto impermeable surface is available use it otherwise use GMIS-ACI-SOLRIS
all_resample_mod<-GMIS_ACI_SOLRIS_resample_mod
all_resample_mod[!is.na(Toronto_resample_mod)]=Toronto_resample_mod[!is.na(Toronto_resample_mod)]
all_resample_mod[all_resample_mod<0]<-NA
plot(all_resample_mod, main='Combined Impermeable Surface Area %')
#I am not happy with the area surrounding Toronto, 
# it appears too impervious compared to the city

# *** Uncomment to save resampled GMIS ISA:
#writeRaster(GMIS_resample_mod,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/GMIS_impervious_GTA.tif",
#            overwrite=TRUE)
#writeRaster(GMISerr_resample_mod,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/GMISerr_impervious_GTA.tif",
#            overwrite=TRUE)
# *** End of uncomment

# *** Uncomment to save Toronto-ACI-SOLRIS ISA (no GMIS):
#writeRaster(Toronto_ACI_SOLRIS_mod_2018,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/all_30m_aggregated_2018_impervious_63_GTA.tif",
#            overwrite=TRUE)
#writeRaster(Toronto_ACI_SOLRIS_mod,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/all_30m_aggregated_impervious_63_GTA.tif",
#            overwrite=TRUE)
#writeRaster(Toronto_ACI_SOLRIS_mod_2020,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/all_30m_aggregated_2020_impervious_63_GTA.tif",
#            overwrite=TRUE)

#
# *** Uncomment to save GMIS-ACI-SOLRIS ISA (no Toronto ISA):
#writeRaster(GMIS_ACI_SOLRIS_resample_mod_2018,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/GMIS_ACI_SOLRIS_2018_impervious_GTA.tif",
#            overwrite=TRUE)

# *** To save GMIS-Toronto-ACI-SOLRIS ISA (used in Madsen-Colford et al. 2025)
writeRaster(GMIS_Toronto_ACI_SOLRIS_resample_mod_2021,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/GMIS_Toronto_ACI_SOLRIS_2021_impervious_GTA.tif",
            overwrite=TRUE)
#writeRaster(all_resample_mod,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/Toronto_GMIS_ACI_SOLRIS_impervious_GTA.tif",
#            overwrite=TRUE)


# *** Uncomment to save GMIS-ACI ISA (no Toronto or SOLRIS data):
#writeRaster(GMIS_ACI_resample_mod,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/GMIS_ACI_impervious_GTA.tif",
#            overwrite=TRUE)

# *** Uncomment to save 30m GMIS-ACI-SOLRIS (no Toronto)
#writeRaster(GMIS_ACI_SOLRIS,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/GMIS_ACI_SOLRIS_30m_impervious_GTA.tif",
#            overwrite=TRUE)

# Check the data saved properly by loading it in and plotting it:
GMIS_Toronto_ACI_SOLRIS <- raster("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/GMIS_Toronto_ACI_SOLRIS_2021_impervious_GTA.tif")
plot(GMIS_Toronto_ACI_SOLRIS,main='Toronto, GMIS, SOLRIS, & ACI Impermeable Surface %')


#Look at ISA without GMIS data: 
all_resample<-raster("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/all_30m_aggregated_2018_impervious_63_GTA.tif")
plot(all_resample,main='Toronto, SOLRIS, & ACI Impermeable Surface %')
