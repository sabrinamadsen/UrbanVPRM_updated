## Code by Sabrina Madsen-Colford
## smadsen@physics.utoronto.ca

# This script pre-processes hourly surface solar irradiance data from the 
# Geostationary Operational Environmental Satellite 16 (GOES16; EUMETSAT OSI SAF)
# Added pre-processing of GOES data to help fix missing SWRad values

# This script creates the file goes_GTA_<yr>_pre_processed_mean_filling_NA_rm.rds
# (where <yr> is the year of interest) used in Madsen-Colford et al. 2025

# To run this code, file paths and directories will need to be updated to 
# import/write files. Portions of the code to be modified by the user are 
# marked above by '***'

# Hourly GOES data were downloaded from 
#           ftp://eftp.ifremer.fr/cersat-rt/project/osi-saf/data/radflux/

library(raster)
library(dplyr)
library(ncdf4)
library(data.table)
library(ggplot2)
library(parallel)
library(StreamMetabolism) #for sunrise/sunset times
library(lubridate) #for converting date formats
library(zoo)

# define study domain, city and year
# *** Select domain from list below or create your own!
#xmin = -79.9333-4/240
#xmax = -79.9333+4/240
#ymin = 44.3167-4/240
#ymax = 44.3167+4/240
#city = 'Borden_V061_500m_2020'

#xmin = -80.3574-4/240
#xmax = -80.3574+4/240
#ymin =  42.7102-4/240
#ymax =  42.7102+4/240
#city = "TP39_V061_500m_2019"

#xmin = -80.5577-4/240
#xmax = -80.5577+4/240
#ymin =  42.6353-4/240
#ymax =  42.6353+4/240
#city = "TPD_V061_500m_2019"

#Toronto:
xmin = -79.7
xmax = -79.1
ymin =  43.5
ymax =  43.9

yr = 2018

# Create slightly larger bounding box to crop to (avoids dropped pixels at edge when reprojecting / resampling)
bbox <- extent(xmin,xmax,ymin,ymax)
buff=0.05
bbox.extra <- extent(xmin - buff, xmax + buff, ymin - buff, ymax + buff)


GOES_CRS = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "

# Create extended bounding box raster in GOES projection 
gridXY = as(raster::extent(bbox.extra), "SpatialPolygons")
proj4string(gridXY) = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
GOES.XY <- projectRaster(raster(gridXY),crs=GOES_CRS)

# *** Change Path
inDIR <-'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GOES/2018/origTIFF/'
rl <- list.files(path=inDIR,pattern='GOES') # GOES data downloaded from ftp://eftp.ifremer.fr/cersat-rt/project/osi-saf/data/radflux/

# *** Change path
times <- fread(paste0('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/RAP/2018/times',yr,'.csv')) # times data in /driver_data/

GOES_raster<- function(dir,dt){
  name_list<-NULL
  file<-rl[grep(as.character(dt),rl)]
  if(file.exists(paste0(dir,file))){
    print(paste0("Processing file ", file))
    rs <- raster(paste0(dir,file),varname = 'ssi')
    rs_c <- raster(paste0(dir,file),varname = 'ssi_confidence_level')
    goes.crop <- crop(rs,bbox.extra)
    goes_QF <-copy(goes.crop)
    goes_QF[goes_QF<3]<-NA #remove any data with confidence level less than acceptable
    names(goes_QF)<-substr(file,1,10)
  }else{
    print(paste0("Accounting for missing file ", dt))
    rs <- raster(paste0(dir,rl[1]),varname = 'ssi')
    goes.crop <- crop(rs,bbox.extra)
    goes_QF <-copy(goes.crop)
    goes_QF<-goes_QF*NA
    names(goes_QF)<-as.character(dt)
  }
  return(goes_QF)
}


outlist <- mcmapply(GOES_raster, dir=inDIR, dt=times$datetime, mc.cores=1)

names(outlist)<-as.character(times$datetime)
st<-stack(outlist)
cnames<-names(outlist)

dt<-as.data.table(as.data.frame(st,xy=T))
setnames(dt,c('x','y',cnames))
dt <-setDT(dt)
dm <- melt.data.table(dt,id.vars=c('x','y'),variable.name='datetime',value.name='swrad',variable.factor=F)
dm[,datetime:=as.character(datetime)][,swrad:=as.numeric(swrad)]
setkey(dm,x,y,datetime)

centX <- mean(xmin-buff,xmax+buff)
centY <- mean(ymin-buff,ymax+buff)

sun.rise <- function(x){
  y <- sunrise.set(centY,centX,paste(substr(x,1,4),substr(x,5,6),substr(x,7,8),sep='/'),timezone='UTC')[1]
  return(y)
}
sun.set <- function(x){
  y <- sunrise.set(centY,centX,paste(substr(x,1,4),substr(x,5,6),substr(x,7,8),sep='/'),timezone='UTC')[2]
  return(y)
}

#Deal with missing night time data
sunrise <- melt.data.table(as.data.table(lapply(times$datetime,sun.rise)),variable.name = 'date', value.name='posTime')
sunrise[,chr:=seq(length(unique(times$chr)))][,riseTime:=as.numeric(substr(as.character(ymd_hms(posTime)),12,13))]
sunrise$date<-substr(sunrise$posTime,1,10)
sunrise<-sunrise[,.(chr,riseTime)]
setkey(sunrise,chr)

sunset <- melt.data.table(as.data.table(lapply(times$datetime,sun.set)),variable.name = 'date',value.name='posTime')
sunset[,chr:=seq(length(unique(times$chr)))][,setTime:=as.numeric(substr(as.character(ymd_hms(posTime)),12,13))]
sunset$date<-substr(sunset$posTime,1,10)
sunset<-sunset[,.(chr,setTime)]
setkey(sunset,chr)

dm[,chr := .GRP, by = .(datetime)]
setkey(dm,chr)
dm <- sunrise[dm]
dm <- sunset[dm]
invisible(gc())

dm[swrad<=0,swrad:=NA]

dm[is.na(swrad) & setTime<riseTime & as.numeric(substr(datetime,9,10))<=riseTime+1 & as.numeric(substr(datetime,9,10))>=setTime-1,swrad:=0]
dm[is.na(swrad) & setTime>riseTime & as.numeric(substr(datetime,9,10))<=riseTime+1,swrad:=0]
dm[is.na(swrad) & setTime>riseTime & as.numeric(substr(datetime,9,10))>=setTime-1,swrad:=0]

setorder(dm,y,x,chr)
len <- dim(dm[chr==1])[1]
dm[,Index := .GRP, by = .(x,y)]
dm <- dm[,.(Index,x,y,datetime,chr,swrad)]
setnames(dm,'chr','HOY')

dm$sw_inter<-dm$swrad

# Interpolate missing data
for (i in unique(dm$Index)){
  ind<-which(dm$Index==i)
  dm$sw_inter[ind]<-na.spline(dm$sw_inter[ind])
  print(i)
} 


dm$sw_test<-dm$sw_inter

#Remove interpolated values that are above or below measured values
hrs<-unique(dm$HOY[dm$sw_inter < min(dm$swrad,na.rm=TRUE) | dm$sw_inter > max(dm$swrad,na.rm=TRUE)])
for (h in hrs) {
  idx<-which((dm$sw_inter< min(dm$swrad,na.rm=TRUE) | dm$sw_inter> max(dm$swrad,na.rm=TRUE)) & dm$HOY==h)
  for (i in idx){
    id<-which(dm$x <= dm$x[i]+0.06 & dm$x >= dm$x[i]-0.06 & dm$y <= dm$y[i]+0.06 & dm$y >= dm$y[i]-0.06 & dm$HOY==h)
    n<-sum(is.na(dm$swrad[id]))
    if(n>=length(id)-1){#if there is 1 or less pixel that is not NA
      dm$sw_test[i]<-mean(dm$swrad[dm$HOY==h],na.rm=TRUE) #take the mean of the entire scene
      #print(paste(h,i,'scene',sep=' '))
    }else{
      dm$sw_test[i]<-mean(dm$swrad[id],na.rm=TRUE)
    }
  }
  print(h)
  #for (i in length(dm$x[idx])){
  #  print(paste0("x: "+str(dm$x[idx][i])+"surrounding: "+str(vals[i])))
  #}
}

#replace any remaining values outside of the range with the minimum and maximum of the range
dm$sw_test[dm$sw_inter < min(dm$swrad,na.rm=TRUE) & is.na(dm$sw_test)] <-0 
dm$sw_test[dm$sw_inter > max(dm$swrad,na.rm=TRUE) & is.na(dm$sw_test)] <- max(dm$swrad,na.rm=TRUE)


# *** Change path
dir.create('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2018/pre_processed_GOES/',showWarnings=FALSE)
# *** Change path & file name
saveRDS(dm,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2018/pre_processed_GOES/goes_GTA_2018_pre_processed_mean_filling_test_NA_rm.rds')