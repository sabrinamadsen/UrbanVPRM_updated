library("sp")
library("raster")
library("ggplot2")
library("shapefiles")
library("rgdal")

#imported_raster=raster('permeability_wgs84.tif')
imported_raster_aci=raster('aci_2018_on.tif')
imported_raster_SOLRIS=raster('SOLRIS_V3/SOLRIS_Version_3_0_LAMBERT.tif')

#imported_raster$permeability_wgs84
plot(imported_raster_aci)
plot(imported_raster_SOLRIS)

aci_crs = '+proj=aea +lat_0=40 +lon_0=-96 +lat_1=44.75 +lat_2=55.75 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'
SOLRIS_crs = '+proj=lcc +lat_0=0 +lon_0=-85 +lat_1=44.5 +lat_2=53.5 +x_0=930000 +y_0=6430000 +ellps=GRS80 +units=m +no_defs'
MODIS_crs = '+proj=longlat +datum=WGS84 +no_defs'

#bound_box_0 = readOGR(dsn="E:/Research/UrbanVPRM/dataverse_files/TP39/shapefiles", layer='TP39_30m_BB_4km')

bound_box = readOGR(dsn="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_500m/shapefiles",layer='GTA_500m_BB') # shapefile data in /urbanVPRM_30m/shapefiles/
#bound_box_aci = spTransform(bound_box, aci_crs)
bound_box_SOLRIS = spTransform(bound_box, SOLRIS_crs)
#bound_box_3km = readOGR(dsn="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/TPD/shapefiles",layer='TPD_30m_BB_2km') # shapefile data in /urbanVPRM_30m/shapefiles/
bound_box_aci = spTransform(bound_box, aci_crs)

ACIcrop = crop(imported_raster_aci,bound_box_aci)
SOLRIScrop = crop(imported_raster_SOLRIS,bound_box_SOLRIS)

SOLRIS_imperm=SOLRIScrop
#SOLRIS_imperm[SOLRIScrop==131]=0.5 #Set wetlands to 50% water coverage????
#SOLRIS_imperm[SOLRIScrop==135]=0.5
#SOLRIS_imperm[SOLRIScrop==140]=0.5
#SOLRIS_imperm[SOLRIScrop==150]=0.5
#SOLRIS_imperm[SOLRIScrop==160]=0.5

SOLRIS_imperm[SOLRIScrop==170]=1 #Open water
SOLRIS_imperm[SOLRIS_imperm > 1]=0

par(mar=c(3,3,3,0))
plot(SOLRIS_imperm, main='SOLRIS Water coverage')
#plot(SOLRIScrop==250, main='SOLRIS Unclassified Area')

#create a raster
x <-raster()
#set the number of columns, rows, and extent
# 2km res x <- raster(ncol=210, nrow=170, xmn=1270260, xmx=1275360, ymn=611160, ymx=617460)
#x <- raster(ncol=115, nrow=140, xmn=1260510, xmx=1263960, ymn=419340, ymx=423540)
#GTA 500m
x <- raster(ncol=1798, nrow=1901, xmn=1300980, xmx=1358010, ymn=529830, ymx=583770)
#TP39 500m
#x <- raster(ncol=230, nrow=279, xmn=1273200, xmx=1280100, ymn=428760, ymx=437130)
#TPD 500m
#x <- raster(ncol=230, nrow=278, xmn=1258800, xmx=1265700, ymn=417270, ymx=425610)
#Borden 4km
#x <- raster(ncol=226, nrow=278, xmn=1269420, xmx=1276200, ymn=610140, ymx=618480)
res(x)
#change the resolution
res(x) <- 30
res(x)
#check the number of cells is 35700 or 62828 for 4km res
#For TPD 35776 for 3km or 16100 for 2km
# For GTA: 3417998,  TP39: 64170, TPD:63940
ncell(x)

# set the coordinate reference system (CRS) (define the projection)
projection(x) <- "+proj=aea +lat_0=40 +lon_0=-96 +lat_1=44.75 +lat_2=55.75 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
# give x the same values as ACIcrop
values(x)<-values(ACIcrop)
x

ACI_imperm=x
ACI_imperm[x==20]=1 #open water
#ACI_imperm[x==80]=0.5 #wetland
#ACI_imperm[x==85]=0.5 #peatland

ACI_imperm[ACI_imperm > 1]=0

par(mar=c(3,3,3,0))
plot(ACI_imperm, main='ACI Water Coverage')
plot(ACIcrop==20, main='ACI Open Water') 
#(includes rock,mines,rubble, and natural non-vegetated surfaces)')

aci_crs2="+proj=aea +datum=WGS84 +units=m +no_defs"

#SOLRIS_imperm_AEA = projectRaster(SOLRIS_imperm, crs=aci_crs, method='ngb')
#SOLRIS_imperm_crop = mask(SOLRIS_imperm_AEA, bound_box_aci, method='ngb')
ACI_imperm_lcc = projectRaster(ACI_imperm, crs=SOLRIS_crs, method='ngb')


par(mar=c(3,3,3,0))
#plot(SOLRIS_imperm_AEA, main='SOLRIS Impermeable Surface (proj=AEA)')
plot(ACI_imperm_lcc, main='ACI Water Coverage (proj=lcc)')


#ACI_imperm_AEA = projectRaster(ACI_imperm, crs=aci_crs, method='ngb')
#ACI_imperm_crop = mask(ACI_imperm_AEA, bound_box_aci, method='ngb')
#par(mar=c(3,3,3,0))
#plot(ACI_imperm_crop, main='ACI Impermeable Surface (proj=AEA)')

#ACI_resample<-resample(ACI_imperm,ACIcrop, method='ngb')
#plot(ACI_resample,main='ACI Impermeable Surface (proj=AEA)')

#SOLRIS_resample<-resample(SOLRIS_imperm_AEA,ACIcrop, method='ngb')
#plot(SOLRIS_resample, main='SOLRIS impermeable Surface (proj=AEA)')
##SOLRIS_imperm_crop = crop(SOLRIS_resample, bound_box_aci, method='ngb')
##plot(SOLRIS_imperm_crop,main='SOLRIS Impermeable Surface (proj=AEA)')

ACI_resample<-resample(ACI_imperm_lcc,SOLRIScrop, method='ngb')
plot(ACI_resample, main='ACI Water Coverage (proj=lcc)')

SOLRIS_imperm[SOLRIScrop==250]=ACI_resample[SOLRIScrop==250]
SOLRIS_imperm[is.na(SOLRIScrop)]=ACI_resample[is.na(SOLRIScrop)]
SOLRIS_imperm[is.na(SOLRIScrop)]=1 #set remaining to 1 (over the border on Lake Ontario are NA)
plot(SOLRIS_imperm, main='SOLRIS Water Coverage')

LC = raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_500m/LandCover/MODIS_LC_GTA_500m.tif') # Land cover data in /urbanVPRM_30m/driver_data/lc_isa/

SOLRIS_resample_mod<-projectRaster(SOLRIS_imperm, crs=crs(LC))
SOLRIS_resample_mod<-crop(SOLRIS_resample_mod,LC)
SOLRIS_resample_mod <- resample(SOLRIS_resample_mod,LC)
plot(SOLRIS_resample_mod, main='Aggregated SOLRIS Fractional Water Coverage')

ACI_resample_mod<-projectRaster(ACI_imperm, crs=crs(LC))
ACI_resample_mod<-crop(ACI_resample_mod,LC)
ACI_resample_mod <- resample(ACI_resample_mod,LC)
plot(ACI_resample_mod, main='Aggregated ACI Fractional Water Coverage')


writeRaster(ACI_resample_mod,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/ACI_aggregated_water_cover_GTA.tif",
            overwrite=TRUE)
writeRaster(SOLRIS_resample_mod,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/SOLRIS_aggregated_water_cover_GTA.tif",
            overwrite=TRUE)
