## Updates to code by Sabrina Madsen-Colford
## smadsen@physics.utoronto.ca

## Original code by IAN SMITH
## iasmith [at] bu.edu

# This script extracts hourly surface solar irradiance data for the study domain
# from the Geostationary Operational Environmental Satellite 16 (GOES16; EUMETSAT OSI SAF)

# This script creates the file rap_goes_GTA_V061_500m_<yr>_hourly_fixed.rds
# used in Madsen-Colford et al. 2025

# To run this code, file paths and directories will need to be updated to 
# import/write files. Portions of the code to be modified by the user are 
# marked above by '***'

# Hourly GOES data were downloaded from 
# ftp://eftp.ifremer.fr/cersat-rt/project/osi-saf/data/radflux/
# Need to pre-process using 'pre_process_GOES.R' 

memory.limit(size=5e8)
library(StreamMetabolism)
library(rgdal)
library(zoo)
library(purrr)
library(data.table)
library(ncdf4)
library(raster)
library(sp)
library(parallel)
library(lubridate)
library(ggplot2)

# *** Change path
setwd('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files')

# *** Choose domain from list below or define your own!
# define study domain, city and year
#xmin = -79.9333-4/240
#xmax = -79.9333+4/240
#ymin = 44.3167-4/240
#ymax = 44.3167+4/240
#city = 'Borden_V061_500m_2020'

#xmin = -80.5577-4/240
#xmax = -80.5577+4/240
#ymin =  42.6353-4/240
#ymax =  42.6353+4/240
#city = 'TPD_V061_500m_2019'

#xmin = -80.3574-4/240
#xmax = -80.3574+4/240
#ymin =  42.7102-4/240
#ymax =  42.7102+4/240
#city = 'TP39_V061_500m_2019'

xmin = -79.7
xmax = -79.1
ymin =  43.5
ymax =  43.9
city = 'GTA_V061_500m_2021'

# *** Ghange year
yr = 2021

# Set input and create output files directories
# *** Change paths
inDIR <- paste0('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GOES/2021/origTIFF/')
outDIR <- paste0('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2021/',yr)

# Time file
# *** Change path
times <- fread(paste0('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/RAP/2021/times',yr,'.csv')) # times data in /urbanVPRM_30m/driver_data/times/
setkey(times,chr)

# CRS list
RAP_CRS = "+proj=lcc +lat_1=25 +lat_2=25 +lat_0=25 +lon_0=265 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs"
GOES_CRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "
MODIS_CRS = "+proj=longlat +datum=WGS84 +no_defs"

# Import raster of study domain and convert to SpatialPoints object for resampling
# *** Change path/file name
ls <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2021/LandCover/MODIS_V061_LC_GTA_500m_2021.tif')
npixel <- ncell(ls)
values(ls) <- 1
ls.spdf <- as(ls,'SpatialPointsDataFrame')

# Create slightly larger bounding box to crop to (avoids dropped pixels at edge when reprojecting / resampling)
bbox <- extent(xmin,xmax,ymin,ymax)
buff=0.05
bbox.extra <- extent(xmin - buff, xmax + buff, ymin - buff, ymax + buff)

# Create extended bounding box raster in GOES projection 
gridXY = as(raster::extent(bbox.extra), "SpatialPolygons")
proj4string(gridXY) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
GOES.XY <- projectRaster(raster(gridXY),crs=GOES_CRS)

print("done! 1")

# Create file list of data to project / crop / resample
rl <- list.files(path=inDIR,pattern='GOES') # GOES data downloaded from ftp://eftp.ifremer.fr/cersat-rt/project/osi-saf/data/radflux/


# *** Change path/file name
goes_data<-readRDS("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2021/pre_processed_GOES/goes_GTA_2021_pre_processed_mean_filling_NA_rm.rds")

goes_data<-goes_data[,.(x,y,datetime,sw_test)]

outlist<-NULL
for (d in unique(goes_data$datetime)){
  m<-copy(ls)
  #print(paste0("Processing ",d))
  godat<-rasterFromXYZ(goes_data[goes_data$datetime==d])
  godat<-godat$sw_test
  crs(godat)<-GOES_CRS
  vals<- extract(godat,ls.spdf,method='bilinear')
  values(m)<-vals
  outlist<-append(outlist,m)
}

print("done! 2")

rm(ls,ls.spdf,GOES.XY)

# Compile all cropped and reprojected hourly rasters into single "long" data.table
cnames <- as.character(times$datetime)#substr(rl,1,10)
st = stack(outlist) 
dt <- as.data.table(as.data.frame(st,xy=T))
setnames(dt,c('x','y',cnames))    
print("saved dt")
dt <- setDT(dt)        
dm <- melt.data.table(dt,id.vars=c('x','y'),variable.name='datetime',value.name='swrad',variable.factor=F)
dm[,datetime:=as.character(datetime)][,swrad:=as.numeric(swrad)]
setkey(dm,x,y,datetime)


# Load RAP .rds file to join with GOES completed data.table
rap2 <- readRDS(paste0(outDIR,'/rap_',city,'.rds'))#'_',yr,'.rds'))
setkey(rap2,x,y,datetime)
rap2 <- dm[rap2]
td <- times[,.(datetime,hour)]
td$datetime <- as.character(td$datetime)
setkey(td,datetime)
setkey(rap2,datetime)
rap2 <- td[rap2]

rap2[,chr := .GRP, by = .(datetime)]
setkey(rap2,chr)
invisible(gc())

setorder(rap2,y,x,chr)
len <- dim(rap2[chr==1])[1]
rap2[,Index := .GRP, by = .(x,y)]
rap2 <- rap2[,.(Index,x,y,datetime,chr,tempK,swrad)]
rap2[,tempK:=round(tempK,2)][,swrad:=round(swrad,2)]
setnames(rap2,'chr','HOY')


## Convert Temperature to °C
rap2$tempK = rap2$tempK-273.15
colnames(rap2) = c("Index","x","y","datetime","HoY","tmpC","swRad")

# Assign Index values that correspond to all other driver data
rap2 <- rap2[,-1]

# import raster used for indexing
# *** Change path/ file name
ls <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2021/LandCover/MODIS_V061_LC_GTA_500m_2021.tif')

## Function to convert tif into a datatable..
tifdt_fun = function(raster,name){
  dt = as.data.table(as.data.frame(raster, xy=T))
  dt = cbind(1:ncell(raster), dt)
  setnames(dt,c("Index","x","y",name))
  setkey(dt,Index,x,y)
  return(dt)
}

Idx.dt = tifdt_fun(ls,"Indexing")
Idx.dt <- Idx.dt[,-4]
colnames(Idx.dt) <- c('Index','x','y')

# Add new Index to the rap/goes data table
rap2 <- Idx.dt[rap2,on = c('x','y'), roll = 'nearest']
setkey(Idx.dt, x,y)
setkey(rap2,x,y)

# Save in RDS binary format to preserve space
saveRDS(rap2,paste0(outDIR,'/rap_goes_',city,'_hourly_fixed.rds'))
