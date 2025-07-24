memory.limit(size=5e5)
#This code converts greenup and dormancy from MODIS's .nc file to tif files

library(raster)
library(ncdf4) # for loading in MSLSP greenup file

#Import land cover data for cropping to domain (generated from processLC_ISA_script1.R)
# *** CHANGE PATH/FILENAME FOR YOUR LOCATION OF INTEREST ***
#LC = raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/Borden_V061_500m_2018/LandCover/MODIS_V061_LC_Borden_500m_2018.tif') # Land cover data in /urbanVPRM_30m/driver_data/lc_isa/
LC = raster('E:/Research/UrbanVPRM/dataverse_files/GTA_V061_500m_2018/LandCover/MODIS_LC_GTA_500m_2018.tif') # Land cover data in /urbanVPRM_30m/driver_data/lc_isa/

#Import MODIS data *** CHANGE PATHS ****
phen_MODIS <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Greenup_0_doy2020001_aid0001.tif')
QA_MODIS <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_QA_Overall_0_doy2020001_aid0001.tif')

phen_MODIS[QA_MODIS==3]<-NA #remove 'poor' data

# *** CHANGE TO YEAR OF INTEREST ***
yr <- '2020'
#convert from days since 1970-01-01 to day of year
numdays <- as.numeric(substr(as.Date(paste0(yr,'-01-01'),format='%Y-%m-%d')-as.Date('1970-01-01',format='%Y-%m-%d'),1,6))
#numdays= 18262 #18628 #18262 #17897 #17532 #number of days since 1970 for each year, 2021, 2020, 2019, 2018
phen_MODIS <-phen_MODIS-numdays
plot(phen_MODIS, main="MODIS Greenup, Cycle 1")

# *** CHANGE PATHS ***
phen_MODIS_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Greenup_1_doy2020001_aid0001.tif')
QA_MODIS_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_QA_Overall_1_doy2020001_aid0001.tif')

phen_MODIS_2[QA_MODIS_2==3]<-NA #remove 'poor' data
#convert from days since 1970-01-01 to day of year
phen_MODIS_2 <-phen_MODIS_2-numdays
plot(phen_MODIS_2,main="MODIS Greenup, Cycle 2")

#Try replacing start of season values that are <20 or missing with the second start of season (if second start of season is <200)
phen_MODIS_test<-phen_MODIS
phen_MODIS_test[(phen_MODIS<20 | is.na(phen_MODIS)) & phen_MODIS_2<200]<-phen_MODIS_2[(phen_MODIS<20 | is.na(phen_MODIS)) & phen_MODIS_2<200]
plot(phen_MODIS_test-phen_MODIS,main="MODIS Greenup difference 2020")


#look at mean over several years
#2022
# *** CHANGE PATHS ***
phen_MODIS_2022 <- crop(raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Greenup_0_doy2022001_aid0001.tif'),phen_MODIS)
QA_MODIS_2022 <- crop(raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_QA_Overall_0_doy2022001_aid0001.tif'),pehn_MODIS)

phen_MODIS_2022[QA_MODIS_2022==3]<-NA #remove 'poor' data
as.Date('2022-01-01',format='%Y-%m-%d')-as.Date('1970-01-01',format='%Y-%m-%d')
numdays_2022=18993
phen_MODIS_2022 <-phen_MODIS_2022-numdays_2022

# *** CHANGE PATHS ***
phen_MODIS_2022_2 <- crop(raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Greenup_1_doy2022001_aid0001.tif'),phen_MODIS)
QA_MODIS_2022_2 <- crop(raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_QA_Overall_1_doy2022001_aid0001.tif'),phen_MODIS)

phen_MODIS_2022_2[QA_MODIS_2022_2==3]<-NA #remove 'poor' data
phen_MODIS_2022_2 <-phen_MODIS_2022_2-numdays_2022

phen_MODIS_2022_test<-phen_MODIS_2022
phen_MODIS_2022_test[(phen_MODIS_2022<20 | is.na(phen_MODIS_2022)) & phen_MODIS_2022_2<200]<-phen_MODIS_2022_2[(phen_MODIS_2022<20 | is.na(phen_MODIS_2022)) & phen_MODIS_2022_2<200]
plot(phen_MODIS_2022_test-phen_MODIS_2022,main="MODIS Greenup difference 2022")
length(phen_MODIS_2022_test[phen_MODIS_2022_test-phen_MODIS_2022>0])
plot(phen_MODIS_2022_test,main="MODIS 2022 Greenups Combined")

#2021
# *** CHANGE PATHS ***
phen_MODIS_2021 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Greenup_0_doy2021001_aid0001.tif')
QA_MODIS_2021 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_QA_Overall_0_doy2021001_aid0001.tif')

phen_MODIS_2021[QA_MODIS_2021==3]<-NA #remove 'poor' data
as.Date('2021-01-01',format='%Y-%m-%d')-as.Date('1970-01-01',format='%Y-%m-%d')
numdays_2021=18628
phen_MODIS_2021 <-phen_MODIS_2021-numdays_2021

# *** CHANGE PATHS ***
phen_MODIS_2021_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Greenup_1_doy2021001_aid0001.tif')
QA_MODIS_2021_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_QA_Overall_1_doy2021001_aid0001.tif')

phen_MODIS_2021_2[QA_MODIS_2021_2==3]<-NA #remove 'poor' data
phen_MODIS_2021_2 <-phen_MODIS_2021_2-numdays_2021

phen_MODIS_2021_test<-phen_MODIS_2021
phen_MODIS_2021_test[(phen_MODIS_2021<20 | is.na(phen_MODIS_2021)) & phen_MODIS_2021_2<200]<-phen_MODIS_2021_2[(phen_MODIS_2021<20 | is.na(phen_MODIS_2021)) & phen_MODIS_2021_2<200]
plot(phen_MODIS_2021_test-phen_MODIS_2021,main="MODIS Greenup difference 2021")
length(phen_MODIS_2021_test[phen_MODIS_2021_test-phen_MODIS_2021>0])
plot(phen_MODIS_2021_test,main="MODIS 2021 Greenups Combined 2021")

#2020
# *** CHANGE PATHS ***
phen_MODIS_2020 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Greenup_0_doy2020001_aid0001.tif')
QA_MODIS_2020 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_QA_Overall_0_doy2020001_aid0001.tif')

phen_MODIS_2020[QA_MODIS_2020==3]<-NA #remove 'poor' data
as.Date('2020-01-01',format='%Y-%m-%d')-as.Date('1970-01-01',format='%Y-%m-%d')
numdays_2020=18262
phen_MODIS_2020 <-phen_MODIS_2020-numdays_2020

# *** CHANGE PATHS ***
phen_MODIS_2020_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Greenup_1_doy2020001_aid0001.tif')
QA_MODIS_2020_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_QA_Overall_1_doy2020001_aid0001.tif')

phen_MODIS_2020_2[QA_MODIS_2020_2==3]<-NA #remove 'poor' data
phen_MODIS_2020_2 <-phen_MODIS_2020_2-numdays_2020

phen_MODIS_2020_test<-phen_MODIS_2020
phen_MODIS_2020_test[(phen_MODIS_2020<20 | is.na(phen_MODIS_2020)) & phen_MODIS_2020_2<200]<-phen_MODIS_2020_2[(phen_MODIS_2020<20 | is.na(phen_MODIS_2020)) & phen_MODIS_2020_2<200]
plot(phen_MODIS_2020_test-phen_MODIS_2020,main="MODIS Greenup difference 2020")
length(phen_MODIS_2020_test[phen_MODIS_2020_test-phen_MODIS_2020>0])
plot(phen_MODIS_2020_test,main="MODIS 2020 Greenups Combined 2020")

#2019
# *** CHANGE PATHS ***
phen_MODIS_2019 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Greenup_0_doy2019001_aid0001.tif')
QA_MODIS_2019 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_QA_Overall_0_doy2019001_aid0001.tif')

phen_MODIS_2019[QA_MODIS_2019==3]<-NA #remove 'poor' data
as.Date('2019-01-01',format='%Y-%m-%d')-as.Date('1970-01-01',format='%Y-%m-%d')
numdays_2019=17897
phen_MODIS_2019 <-phen_MODIS_2019-numdays_2019

# *** CHANGE PATHS ***
phen_MODIS_2019_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Greenup_1_doy2019001_aid0001.tif')
QA_MODIS_2019_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_QA_Overall_1_doy2019001_aid0001.tif')

phen_MODIS_2019_2[QA_MODIS_2019_2==3]<-NA #remove 'poor' data
phen_MODIS_2019_2 <-phen_MODIS_2019_2-numdays_2019

phen_MODIS_2019_test<-phen_MODIS_2019
phen_MODIS_2019_test[(phen_MODIS_2019<20 | is.na(phen_MODIS_2019)) & phen_MODIS_2019_2<200]<-phen_MODIS_2019_2[(phen_MODIS_2019<20 | is.na(phen_MODIS_2019)) & phen_MODIS_2019_2<200]
plot(phen_MODIS_2019_test-phen_MODIS_2019,main="MODIS Greenup difference 2019")
length(phen_MODIS_2019_test[phen_MODIS_2019_test-phen_MODIS_2019>0])
plot(phen_MODIS_2019_test,main="MODIS 2019 Greenups Combined 2019")


#2018
# *** CHANGE PATHS ***
phen_MODIS_2018 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Greenup_0_doy2018001_aid0001.tif')
QA_MODIS_2018 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_QA_Overall_0_doy2018001_aid0001.tif')

phen_MODIS_2018[QA_MODIS_2018==3]<-NA #remove 'poor' data
as.Date('2018-01-01',format='%Y-%m-%d')-as.Date('1970-01-01',format='%Y-%m-%d')
numdays_2018=17532
phen_MODIS_2018 <-phen_MODIS_2018-numdays_2018

# *** CHANGE PATHS ***
phen_MODIS_2018_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Greenup_1_doy2018001_aid0001.tif')
QA_MODIS_2018_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_QA_Overall_1_doy2018001_aid0001.tif')

phen_MODIS_2018_2[QA_MODIS_2018_2==3]<-NA #remove 'poor' data
phen_MODIS_2018_2 <-phen_MODIS_2018_2-numdays_2018

phen_MODIS_2018_test<-phen_MODIS_2018
phen_MODIS_2018_test[(phen_MODIS_2018<20 | is.na(phen_MODIS_2018)) & phen_MODIS_2018_2<200]<-phen_MODIS_2018_2[(phen_MODIS_2018<20 | is.na(phen_MODIS_2018)) & phen_MODIS_2018_2<200]
plot(phen_MODIS_2018_test-phen_MODIS_2018,main="MODIS Greenup difference 2018")
length(phen_MODIS_2018_test[phen_MODIS_2018_test-phen_MODIS_2018>0])
plot(phen_MODIS_2018_test,main="MODIS 2018 Greenups Combined 2018")

#2017
# *** CHANGE PATHS ***
phen_MODIS_2017 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Greenup_0_doy2017001_aid0001.tif')
QA_MODIS_2017 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_QA_Overall_0_doy2017001_aid0001.tif')

phen_MODIS_2017[QA_MODIS_2017==3]<-NA #remove 'poor' data
as.Date('2017-01-01',format='%Y-%m-%d')-as.Date('1970-01-01',format='%Y-%m-%d')
numdays_2017=17167
phen_MODIS_2017 <-phen_MODIS_2017-numdays_2017

# *** CHANGE PATHS ***
phen_MODIS_2017_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Greenup_1_doy2017001_aid0001.tif')
QA_MODIS_2017_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_QA_Overall_1_doy2017001_aid0001.tif')

phen_MODIS_2017_2[QA_MODIS_2017_2==3]<-NA #remove 'poor' data
phen_MODIS_2017_2 <-phen_MODIS_2017_2-numdays_2017

phen_MODIS_2017_test<-phen_MODIS_2017
phen_MODIS_2017_test[(phen_MODIS_2017<20 | is.na(phen_MODIS_2017)) & phen_MODIS_2017_2<200]<-phen_MODIS_2017_2[(phen_MODIS_2017<20 | is.na(phen_MODIS_2017)) & phen_MODIS_2017_2<200]
plot(phen_MODIS_2017_test-phen_MODIS_2017,main="MODIS Greenup difference, 2017")
length(phen_MODIS_2017_test[phen_MODIS_2017_test-phen_MODIS_2017>0])
phen_MODIS_2017_test[phen_MODIS_2017_test-phen_MODIS_2017>0]
plot(phen_MODIS_2017_test,main="MODIS 2017 Greenups Combined")


phen_MODIS[phen_MODIS<20]<-NA
phen_MODIS_2017[phen_MODIS_2017<20]<-NA
phen_MODIS_2018[phen_MODIS_2018<20]<-NA
phen_MODIS_2019[phen_MODIS_2019<20]<-NA
#phen_MODIS_2020[phen_MODIS_2020<20]<-NA
phen_MODIS_2021[phen_MODIS_2021<20]<-NA
phen_MODIS_2022[phen_MODIS_2022<20]<-NA
phen_MODIS[phen_MODIS>200]<-NA
phen_MODIS_2017[phen_MODIS_2017>200]<-NA
phen_MODIS_2018[phen_MODIS_2018>200]<-NA
phen_MODIS_2019[phen_MODIS_2019>200]<-NA
#phen_MODIS_2020[phen_MODIS_2020>200]<-NA
phen_MODIS_2021[phen_MODIS_2021>200]<-NA
phen_MODIS_2022[phen_MODIS_2022>200]<-NA
weight_current<-phen_MODIS
weight_current[weight_current>0]<-1 #weight the current year more heavily
weight_current[is.na(weight_current)]<-1
weight_2022<-phen_MODIS_2022
weight_2022[weight_2022>0]<-0.25
weight_2022[is.na(weight_2022)]<-0.25
weight_2021<-phen_MODIS_2021
weight_2021[weight_2021>0]<-0.5 
weight_2021[is.na(weight_2021)]<-0.5
#weight_2020<-phen_MODIS_2020
#weight_2020[weight_2020>0]<-0.5 
#weight_2020[is.na(weight_2020)]<-0.5
weight_2019<-phen_MODIS_2019
weight_2019[weight_2019>0]<-0.5 
weight_2019[is.na(weight_2019)]<-0.5
weight_2018<-phen_MODIS_2018
weight_2018[weight_2018>0]<-0.25 
weight_2018[is.na(weight_2018)]<-0.25
weight_2017<-phen_MODIS_2017
weight_2017[weight_2017>0]<-0.25
weight_2017[is.na(weight_2017)]<-0.25

phen_MODIS_avg<-raster::weighted.mean(stack(phen_MODIS_2019,phen_MODIS,phen_MODIS_2021),stack(weight_2019,weight_current,weight_2021), na.rm=TRUE)
phen_MODIS_avg
phen_MODIS_avg<-round(phen_MODIS_avg)
plot(phen_MODIS_avg, main='Average Greenup, 2020')
plot(crop(phen_MODIS_avg,LC),main='Average Greenup Toronto, 2020')

phen_MODIS_avg_all<-raster::weighted.mean(stack(phen_MODIS_2017,phen_MODIS_2018,phen_MODIS_2019,phen_MODIS,phen_MODIS_2021,phen_MODIS_2022),stack(weight_2017,weight_2018,weight_2019,weight_current,weight_2021,weight_2022), na.rm=TRUE)
phen_MODIS_avg_all
phen_MODIS_avg_all<-round(phen_MODIS_avg_all)
plot(phen_MODIS_avg_all, main='2017-2022 Weighted Average Greenup, 2020')
plot(crop(phen_MODIS_avg_all,LC),main='2017-2022 Weighted Average Greenup Toronto, 2020')

#If there are missing values for all of 2020-2022, fill with 2017-2019 Greenup
phen_MODIS_avg[is.na(phen_MODIS_avg) & !is.na(phen_MODIS_avg_all)]=phen_MODIS_avg_all[is.na(phen_MODIS_avg) & !is.na(phen_MODIS_avg_all)]

phen_MODIS_test[phen_MODIS_test<20]<-NA
phen_MODIS_2017_test[phen_MODIS_2017_test<20]<-NA
phen_MODIS_2018_test[phen_MODIS_2018_test<20]<-NA
phen_MODIS_2019_test[phen_MODIS_2019_test<20]<-NA
#phen_MODIS_2020_test[phen_MODIS_2020_test<20]<-NA
phen_MODIS_2021_test[phen_MODIS_2021_test<20]<-NA
phen_MODIS_2022_test[phen_MODIS_2022_test<20]<-NA
phen_MODIS_test[phen_MODIS_test>200]<-NA
phen_MODIS_2017_test[phen_MODIS_2017_test>200]<-NA
phen_MODIS_2018_test[phen_MODIS_2018_test>200]<-NA
phen_MODIS_2019_test[phen_MODIS_2019_test>200]<-NA
#phen_MODIS_2020_test[phen_MODIS_2020_test>200]<-NA
phen_MODIS_2021_test[phen_MODIS_2021_test>200]<-NA
phen_MODIS_2022_test[phen_MODIS_2022_test>200]<-NA

phen_MODIS_test_avg<-raster::weighted.mean(stack(phen_MODIS_2019_test,phen_MODIS_test,phen_MODIS_2021_test),stack(weight_2019,weight_current,weight_2021), na.rm=TRUE)
phen_MODIS_test_avg
phen_MODIS_test_avg<-round(phen_MODIS_test_avg)
phen_MODIS_test_avg
plot(phen_MODIS_test_avg, main='Average Combined Greenup')

plot(phen_MODIS_test_avg-phen_MODIS_avg, main='Average Greenup Difference')
phen_MODIS_test_avg[abs(phen_MODIS_test_avg-phen_MODIS_avg)>0]
phen_MODIS_avg[abs(phen_MODIS_test_avg-phen_MODIS_avg)>0]
# 17 of the 333797 GTA pixels are different. Maximum difference is -17 days, average and median difference is -3.65 and -5 days for 2021
# 28 of the 333797 GTA pixels are different. Maximum difference is -14 days, average and median difference is -4.5 days

#Using the combined greenup (using second greenup to fill unreasonable values) does not affect Borden, TPD, TP39, or Toronto
plot(crop(phen_MODIS_test_avg,LC),main='Average Combined Greenup Toronto, 2020')
plot(crop(phen_MODIS_avg-phen_MODIS_test_avg,LC),main='Average Greenup difference Toronto, 2020') #Does not affect Borden,TPD, or TP39, affects 2 points in Toronto (9 days) in 2020, and (-17 days) in 2021

writeRaster(phen_MODIS_test_avg,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MODIS_V061_avg_greenup_2020.nc', overwrite=TRUE, varname="SOS", varunit="", 
            longname="Start of Season", xname="Longitude",   yname="Latitude")

#Deal with missing values by filling with MSLSP
#MSLSP data only available from 2016-2019 (as of Feb 2024)
phen_TP_2019 <- raster('E:/Research/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TNH_2019_TP.nc', varname = 'OGI')
phen_Borden_2019 <- raster('E:/Research/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TNK_2019_Borden.nc', varname = 'OGI')
phen_left_2019 <- raster('E:/Research/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TNJ_2019_Toronto.nc', varname = 'OGI')
phen_right_2019 <- raster('E:/Research/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TPJ_2019_Toronto.nc', varname = 'OGI')
#data from https://search.earthdata.nasa.gov/search/granules?p=C2102664483-LPDAAC_ECS&pg[0][v]=f&pg[0][qt]=2018-01-01T00%3A00%3A00.000Z%2C2020-01-01T23%3A59%3A59.999Z&pg[0][gsk]=-start_date&g=G2108838872-LPDAAC_ECS&q=MSLSP&sb[0]=-80.62646%2C42.5256%2C-80.08154%2C42.81998&tl=1645222323!3!!&m=43.1892489966389!-81.61083984375!7!1!0!0%2C2 
# ^ CRS error is OK (CRS added in next 2 lines)

crs(phen_TP_2019)<-'+init=epsg:32617' #projection given in the file (utm in meters)
crs(phen_Borden_2019)<-'+init=epsg:32617' #projection given in the file (utm in meters)
crs(phen_left_2019)<-'+init=epsg:32617' #projection given in the file (utm in meters)
crs(phen_right_2019)<-'+init=epsg:32617' #projection given in the file (utm in meters)
phen_2019<-mosaic(phen_left_2019,phen_right_2019,phen_TP_2019,phen_Borden_2019,fun=mean) #merge the left and right rasters
#the function only averages pixels if they overlap (should not be an issue here)

phen_reproj_2019 <- projectRaster(phen_2019, crs=crs(phen_MODIS_avg)) #reproject to MODIS crs
phen_resample_2019 <-resample(phen_reproj_2019,phen_MODIS_avg)#resample to MODIS pixels
plot(crop(phen_resample_2019,LC)) #crop to just have Toronto
plot(phen_resample_2019,main='MSLSP Greenup, 2019')

#Bring in 2018 data
phen_TP_2018 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TNH_2018_TP.nc', varname = 'OGI')
phen_Borden_2018 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TNK_2018_Borden.nc', varname = 'OGI')
phen_left_2018 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TNJ_2018_Toronto.nc', varname = 'OGI')
phen_right_2018 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TPJ_2018_Toronto.nc', varname = 'OGI')
#data from https://search.earthdata.nasa.gov/search/granules?p=C2102664483-LPDAAC_ECS&pg[0][v]=f&pg[0][qt]=2018-01-01T00%3A00%3A00.000Z%2C2020-01-01T23%3A59%3A59.999Z&pg[0][gsk]=-start_date&g=G2108838872-LPDAAC_ECS&q=MSLSP&sb[0]=-80.62646%2C42.5256%2C-80.08154%2C42.81998&tl=1645222323!3!!&m=43.1892489966389!-81.61083984375!7!1!0!0%2C2 
# ^ CRS error is OK (CRS added in next 2 lines)

crs(phen_TP_2018)<-'+init=epsg:32617' #projection given in the file (utm in meters)
crs(phen_Borden_2018)<-'+init=epsg:32617' #projection given in the file (utm in meters)
crs(phen_left_2018)<-'+init=epsg:32617' #projection given in the file (utm in meters)
crs(phen_right_2018)<-'+init=epsg:32617' #projection given in the file (utm in meters)
phen_2018<-mosaic(phen_left_2018,phen_right_2018,phen_TP_2018,phen_Borden_2018,fun=mean) #merge the left and right rasters
#the function only averages pixels if they overlap (should not be an issue here)

phen_reproj_2018 <- projectRaster(phen_2018, crs=crs(phen_MODIS_avg)) #reproject to MODIS crs
phen_resample_2018 <-resample(phen_reproj_2018,phen_MODIS_avg)#resample to MODIS pixels
plot(crop(phen_resample_2018,LC),main='MSLSP Greenup, 2018 Toronto') #crop to just have Toronto
plot(phen_resample_2018,main='MSLSP Greenup, 2018')

#If MODIS data is NA use MSLSP Greenup
phen_MODIS_test_avg2 <- phen_MODIS_test_avg
phen_MODIS_avg2 <- phen_MODIS_avg

phen_resample<-mean(phen_resample_2018,phen_resample_2019)
phen_resample<-round(phen_resample)
plot(phen_resample,main='MSLSP Greenup, 2018-2019 average')
plot(crop(phen_resample,LC),main='MSLSP Greenup, 2018-2019 average Toronto')

phen_MODIS_test_avg2[is.na(phen_MODIS_test_avg) & phen_resample>20 & phen_resample<200] <- phen_resample[is.na(phen_MODIS_test_avg) & phen_resample>20 & phen_resample<200]
phen_MODIS_avg2[is.na(phen_MODIS_avg) & phen_resample>20 & phen_resample<200] <- phen_resample[is.na(phen_MODIS_avg) & phen_resample>20 & phen_resample<200]

plot(crop(phen_MODIS_test_avg2,LC),main='MODIS-MSLSP combined Greenup, 2020')

phen_MODIS_test_avg2<-round(phen_MODIS_test_avg2) #Round to nearest day
phen_MODIS_avg2<-round(phen_MODIS_avg2)

plot(phen_MODIS_test_avg2-phen_MODIS_avg2, main='Average Greenup Difference')
phen_MODIS_test_avg2[abs(phen_MODIS_test_avg2-phen_MODIS_avg2)>0] #no significant difference whether use second or primary greenup
phen_MODIS_avg2[abs(phen_MODIS_test_avg2-phen_MODIS_avg2)>0] 
# There is one pixel (near lake Erie) that is NAN in one MODIS dataset and 188 in another, the NAN is being replaced by MSLSP with is 122! 
# but all others have a much smaller difference (mean difference of 6.6 days), mean difference -9.4 days for 2021
# No difference at Borden, TPD, TP39, or Toronto

plot(crop(phen_MODIS_avg2-phen_MODIS_test_avg2,LC),main='Average Greenup difference GTA, 2020') 
#Several significant differences over Toronto in 2021 (max diff 51, min -49, mean -8.89, median -9 days)
#- LOOK INTO THIS!!!
# I think I trust the combined Greenup more, 
# the non-combined Greenup has spots where there is start of season significantly later than its surroundings


writeRaster(phen_MODIS_test_avg2,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MODIS_V061_avg_greenup_2020.nc', overwrite=TRUE, varname="SOS", varunit="", 
            longname="Start of Season", xname="Longitude",   yname="Latitude")

#writeRaster(phen_MODIS_avg,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MODIS_avg_greenup_2019.tif', overwrite=TRUE)

writeRaster(phen_MODIS_test_avg2,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MODIS_V061_avg_greenup_2020.tif', overwrite=TRUE)

remove(phen_MODIS, phen_MODIS_2, phen_MODIS_test, phen_MODIS_2017, phen_MODIS_2017_2, phen_MODIS_2019, phen_MODIS_2019_2, phen_MODIS_2017_test, phen_MODIS_2019_test, phen,phen_reproj_2019,phen_resample_2019,phen_reproj_2018,phen_resample_2018,phen_resample)
remove(phen_right_2018,phen_left_2018,phen_TP_2018,phen_Borden_2018,phen_right_2019,phen_left_2019,phen_TP_2019,phen_Borden_2019)
#Repeat for dormancy

#Import MODIS data
dorm_MODIS <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Dormancy_0_doy2021001_aid0001.tif')

dorm_MODIS[QA_MODIS==3]<-NA #remove 'poor' data
#convert from days since 1970-01-01 to day of year
as.Date('2021-01-01',format='%Y-%m-%d')-as.Date('1970-01-01',format='%Y-%m-%d')
numdays=18628
dorm_MODIS <-dorm_MODIS-numdays
plot(dorm_MODIS, main="MODIS Dormancy, Cycle 1")

dorm_MODIS_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Dormancy_1_doy2021001_aid0001.tif')
dorm_MODIS_2[QA_MODIS_2==3]<-NA #remove 'poor' data
#convert from days since 1970-01-01 to day of year
dorm_MODIS_2 <-dorm_MODIS_2-numdays
plot(dorm_MODIS_2,main="MODIS Dormancy, Cycle 2")

dorm_MODIS_test<-dorm_MODIS
dorm_MODIS_test[(dorm_MODIS<200 | is.na(dorm_MODIS)) & dorm_MODIS_2<365]<-dorm_MODIS_2[(dorm_MODIS<200 | is.na(dorm_MODIS)) & dorm_MODIS_2<365]
plot(dorm_MODIS_test-dorm_MODIS,main="MODIS Dormancy difference")
length(dorm_MODIS_test[abs(dorm_MODIS_test-dorm_MODIS)>0])
dorm_MODIS_test[abs(dorm_MODIS_test-dorm_MODIS)>0]
dorm_MODIS[abs(dorm_MODIS_test-dorm_MODIS)>0]
plot(dorm_MODIS_test,main="MODIS Dormancys Combined")

#look at mean over several years
#2022
dorm_MODIS_2022 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Dormancy_0_doy2022001_aid0001.tif')
dorm_MODIS_2022[QA_MODIS_2022==3]<-NA #remove 'poor' data
as.Date('2022-01-01',format='%Y-%m-%d')-as.Date('1970-01-01',format='%Y-%m-%d')
numdays_2022=18993
dorm_MODIS_2022 <-dorm_MODIS_2022-numdays_2022

dorm_MODIS_2022_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Dormancy_1_doy2022001_aid0001.tif')
dorm_MODIS_2022_2[QA_MODIS_2022_2==3]<-NA #remove 'poor' data
dorm_MODIS_2022_2 <-dorm_MODIS_2022_2-numdays_2022

dorm_MODIS_2022_test<-dorm_MODIS_2022
dorm_MODIS_2022_test[(dorm_MODIS_2022<200 | is.na(dorm_MODIS_2022)) & dorm_MODIS_2022_2<365]<-dorm_MODIS_2022_2[(dorm_MODIS_2022<200 | is.na(dorm_MODIS_2022)) & dorm_MODIS_2022_2<365]
plot(dorm_MODIS_2022_test-dorm_MODIS_2022,main="MODIS Dormancy difference")
length(dorm_MODIS_2022_test[dorm_MODIS_2022_test-dorm_MODIS_2022>0])
plot(dorm_MODIS_2022_test,main="MODIS 2022 Dormancys Combined")

#2021
dorm_MODIS_2021 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Dormancy_0_doy2021001_aid0001.tif')
dorm_MODIS_2021[QA_MODIS_2021==3]<-NA #remove 'poor' data
as.Date('2021-01-01',format='%Y-%m-%d')-as.Date('1970-01-01',format='%Y-%m-%d')
numdays_2021=18628
dorm_MODIS_2021 <-dorm_MODIS_2021-numdays_2021

dorm_MODIS_2021_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Dormancy_1_doy2021001_aid0001.tif')
dorm_MODIS_2021_2[QA_MODIS_2021_2==3]<-NA #remove 'poor' data
dorm_MODIS_2021_2 <-dorm_MODIS_2021_2-numdays_2021

dorm_MODIS_2021_test<-dorm_MODIS_2021
dorm_MODIS_2021_test[(dorm_MODIS_2021<200 | is.na(dorm_MODIS_2021)) & dorm_MODIS_2021_2<365]<-dorm_MODIS_2021_2[(dorm_MODIS_2021<200 | is.na(dorm_MODIS_2021)) & dorm_MODIS_2021_2<365]
plot(dorm_MODIS_2021_test-dorm_MODIS_2021,main="MODIS Dormancy difference")
length(dorm_MODIS_2021_test[dorm_MODIS_2021_test-dorm_MODIS_2021>0])
plot(dorm_MODIS_2021_test,main="MODIS 2021 Dormancys Combined")

#2020
dorm_MODIS_2020 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Dormancy_0_doy2020001_aid0001.tif')
dorm_MODIS_2020[QA_MODIS_2020==3]<-NA #remove 'poor' data
as.Date('2020-01-01',format='%Y-%m-%d')-as.Date('1970-01-01',format='%Y-%m-%d')
numdays_2020=18262
dorm_MODIS_2020 <-dorm_MODIS_2020-numdays_2020

dorm_MODIS_2020_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Dormancy_1_doy2020001_aid0001.tif')
dorm_MODIS_2020_2[QA_MODIS_2020_2==3]<-NA #remove 'poor' data
dorm_MODIS_2020_2 <-dorm_MODIS_2020_2-numdays_2020

dorm_MODIS_2020_test<-dorm_MODIS_2020
dorm_MODIS_2020_test[(dorm_MODIS_2020<200 | is.na(dorm_MODIS_2020)) & dorm_MODIS_2020_2<365]<-dorm_MODIS_2020_2[(dorm_MODIS_2020<200 | is.na(dorm_MODIS_2020)) & dorm_MODIS_2020_2<365]
plot(dorm_MODIS_2020_test-dorm_MODIS_2020,main="MODIS Dormancy difference")
length(dorm_MODIS_2020_test[dorm_MODIS_2020_test-dorm_MODIS_2020>0])
plot(dorm_MODIS_2020_test,main="MODIS 2020 Dormancys Combined")

#2019
dorm_MODIS_2019 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Dormancy_0_doy2019001_aid0001.tif')
dorm_MODIS_2019[QA_MODIS_2019==3]<-NA #remove 'poor' data
as.Date('2019-01-01',format='%Y-%m-%d')-as.Date('1970-01-01',format='%Y-%m-%d')
numdays_2019=17897
dorm_MODIS_2019 <-dorm_MODIS_2019-numdays_2019

dorm_MODIS_2019_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Dormancy_1_doy2019001_aid0001.tif')
dorm_MODIS_2019_2[QA_MODIS_2019_2==3]<-NA #remove 'poor' data
dorm_MODIS_2019_2 <-dorm_MODIS_2019_2-numdays_2019

dorm_MODIS_2019_test<-dorm_MODIS_2019
dorm_MODIS_2019_test[(dorm_MODIS_2019<200 | is.na(dorm_MODIS_2019)) & dorm_MODIS_2019_2<365]<-dorm_MODIS_2019_2[(dorm_MODIS_2019<200 | is.na(dorm_MODIS_2019)) & dorm_MODIS_2019_2<365]
plot(dorm_MODIS_2019_test-dorm_MODIS_2019,main="MODIS Dormancy difference")
length(dorm_MODIS_2019_test[dorm_MODIS_2019_test-dorm_MODIS_2019>0])
plot(dorm_MODIS_2019_test,main="MODIS 2019 Dormancys Combined")

#2018
dorm_MODIS_2018 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Dormancy_0_doy2018001_aid0001.tif')
dorm_MODIS_2018[QA_MODIS_2018==3]<-NA #remove 'poor' data
as.Date('2018-01-01',format='%Y-%m-%d')-as.Date('1970-01-01',format='%Y-%m-%d')
numdays_2018=17532
dorm_MODIS_2018 <-dorm_MODIS_2018-numdays_2018

dorm_MODIS_2018_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Dormancy_1_doy2018001_aid0001.tif')
dorm_MODIS_2018_2[QA_MODIS_2018_2==3]<-NA #remove 'poor' data
dorm_MODIS_2018_2 <-dorm_MODIS_2018_2-numdays_2018

dorm_MODIS_2018_test<-dorm_MODIS_2018
dorm_MODIS_2018_test[(dorm_MODIS_2018<200 | is.na(dorm_MODIS_2018)) & dorm_MODIS_2018_2<365]<-dorm_MODIS_2018_2[(dorm_MODIS_2018<200 | is.na(dorm_MODIS_2018)) & dorm_MODIS_2018_2<365]
plot(dorm_MODIS_2018_test-dorm_MODIS_2018,main="MODIS Dormancy difference")
length(dorm_MODIS_2018_test[dorm_MODIS_2018_test-dorm_MODIS_2018>0])
dorm_MODIS_2018_test[abs(dorm_MODIS_2018_test-dorm_MODIS_2018)>0]
dorm_MODIS_2018[abs(dorm_MODIS_2018_test-dorm_MODIS_2018)>0]
plot(dorm_MODIS_2018_test,main="MODIS 2018 Dormancys Combined")

#2017
dorm_MODIS_2017 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Dormancy_0_doy2017001_aid0001.tif')
dorm_MODIS_2017[QA_MODIS_2017==3]<-NA #remove 'poor' data
as.Date('2017-01-01',format='%Y-%m-%d')-as.Date('1970-01-01',format='%Y-%m-%d')
numdays_2017=17167
dorm_MODIS_2017 <-dorm_MODIS_2017-numdays_2017

dorm_MODIS_2017_2 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MCD12Q2.061_Dormancy_1_doy2017001_aid0001.tif')
dorm_MODIS_2017_2[QA_MODIS_2017_2==3]<-NA #remove 'poor' data
dorm_MODIS_2017_2 <-dorm_MODIS_2017_2-numdays_2017

dorm_MODIS_2017_test<-dorm_MODIS_2017
dorm_MODIS_2017_test[(dorm_MODIS_2017<200 | is.na(dorm_MODIS_2017)) & dorm_MODIS_2017_2<365]<-dorm_MODIS_2017_2[(dorm_MODIS_2017<200 | is.na(dorm_MODIS_2017)) & dorm_MODIS_2017_2<365]
plot(dorm_MODIS_2017_test-dorm_MODIS_2017,main="MODIS Dormancy difference")
length(dorm_MODIS_2017_test[dorm_MODIS_2017_test-dorm_MODIS_2017>0])
dorm_MODIS_2017_test[dorm_MODIS_2017_test-dorm_MODIS_2017>0]
dorm_MODIS_2017[dorm_MODIS_2017_test-dorm_MODIS_2017>0]
plot(dorm_MODIS_2017_test,main="MODIS 2017 Dormancys Combined")


dorm_MODIS[dorm_MODIS<200]<-NA
dorm_MODIS_2017[dorm_MODIS_2017<200]<-NA
dorm_MODIS_2018[dorm_MODIS_2018<200]<-NA
dorm_MODIS_2019[dorm_MODIS_2019<200]<-NA
dorm_MODIS_2020[dorm_MODIS_2020<200]<-NA
#dorm_MODIS_2021[dorm_MODIS_2021<200]<-NA
dorm_MODIS_2022[dorm_MODIS_2022<200]<-NA
dorm_MODIS[dorm_MODIS>365]<-NA
dorm_MODIS_2017[dorm_MODIS_2017>365]<-NA
dorm_MODIS_2018[dorm_MODIS_2018>365]<-NA
dorm_MODIS_2019[dorm_MODIS_2019>365]<-NA
dorm_MODIS_2020[dorm_MODIS_2020>365]<-NA
#dorm_MODIS_2021[dorm_MODIS_2021>365]<-NA
dorm_MODIS_2022[dorm_MODIS_2022>365]<-NA

weight_current<-dorm_MODIS
weight_current[weight_current>0]<-1 #weight the current year more heavily
weight_current[is.na(weight_current)]<-1
weight_2022<-dorm_MODIS_2022
weight_2022[weight_2022>0]<-0.5
weight_2022[is.na(weight_2022)]<-0.5
#weight_2021<-dorm_MODIS_2021
#weight_2021[weight_2021>0]<-0.5
#weight_2021[is.na(weight_2021)]<-0.5
weight_2020<-dorm_MODIS_2020
weight_2020[weight_2020>0]<-0.5
weight_2020[is.na(weight_2020)]<-0.5
weight_2019<-dorm_MODIS_2019
weight_2019[weight_2019>0]<-0.25
weight_2019[is.na(weight_2019)]<-0.25
weight_2018<-dorm_MODIS_2018
weight_2018[weight_2018>0]<-0.25
weight_2018[is.na(weight_2018)]<-0.25
weight_2017<-dorm_MODIS_2017
weight_2017[weight_2017>0]<-0.25
weight_2017[is.na(weight_2017)]<-0.25

dorm_MODIS_avg<-raster::weighted.mean(stack(dorm_MODIS_2020,dorm_MODIS,dorm_MODIS_2022),stack(weight_2020,weight_current,weight_2022), na.rm=TRUE)
dorm_MODIS_avg
dorm_MODIS_avg<-round(dorm_MODIS_avg)
plot(dorm_MODIS_avg, main='Average Dormancy')


dorm_MODIS_test[dorm_MODIS_test<200]<-NA
dorm_MODIS_2017_test[dorm_MODIS_2017_test<200]<-NA
dorm_MODIS_2018_test[dorm_MODIS_2018_test<200]<-NA
dorm_MODIS_2019_test[dorm_MODIS_2019_test<200]<-NA
dorm_MODIS_2020_test[dorm_MODIS_2020_test<200]<-NA
#dorm_MODIS_2021_test[dorm_MODIS_2021_test<200]<-NA
dorm_MODIS_2022_test[dorm_MODIS_2022_test<200]<-NA
dorm_MODIS_test[dorm_MODIS_test>365]<-NA
dorm_MODIS_2017_test[dorm_MODIS_2017_test>365]<-NA
dorm_MODIS_2018_test[dorm_MODIS_2018_test>365]<-NA
dorm_MODIS_2019_test[dorm_MODIS_2019_test>365]<-NA
dorm_MODIS_2020_test[dorm_MODIS_2020_test>365]<-NA
#dorm_MODIS_2021_test[dorm_MODIS_2021_test>365]<-NA
dorm_MODIS_2022_test[dorm_MODIS_2022_test>365]<-NA

dorm_MODIS_test_avg<-raster::weighted.mean(stack(dorm_MODIS_2020_test,dorm_MODIS_test,dorm_MODIS_2022_test),stack(weight_2020,weight_current,weight_2022), na.rm=TRUE)
dorm_MODIS_test_avg
dorm_MODIS_test_avg<-round(dorm_MODIS_test_avg)
dorm_MODIS_test_avg
plot(dorm_MODIS_test_avg, main='Average Combined Dormancy')

plot(dorm_MODIS_test_avg-dorm_MODIS_avg, main='Average Dormancy Difference')
dorm_MODIS_test_avg[abs(dorm_MODIS_test_avg-dorm_MODIS_avg)>0]
dorm_MODIS_avg[abs(dorm_MODIS_test_avg-dorm_MODIS_avg)>0]
#Largest difference is 19 days, average difference is -0.5 days
# Largest difference in 2021 is 124 days!!, average difference is 25.5, median 23
# I trust combined average more (the one with the largest difference has dormancy DOY=211-July while the combined has dormancy 335-Dec 1st)

#Deal with missing values by filling with MSLSP data
#2019
phen_dorm_TP_2019 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TNH_2019_TP.nc', varname = 'OGMn')
phen_dorm_Borden_2019 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TNK_2019_Borden.nc', varname = 'OGMn')
phen_dorm_left_2019 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TNJ_2019_Toronto.nc', varname = 'OGMn')
phen_dorm_right_2019 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TPJ_2019_Toronto.nc', varname = 'OGMn')
#data from https://search.earthdata.nasa.gov/search/granules?p=C2102664483-LPDAAC_ECS&pg[0][v]=f&pg[0][qt]=2018-01-01T00%3A00%3A00.000Z%2C2020-01-01T23%3A59%3A59.999Z&pg[0][gsk]=-start_date&g=G2108838872-LPDAAC_ECS&q=MSLSP&sb[0]=-80.62646%2C42.5256%2C-80.08154%2C42.81998&tl=1645222323!3!!&m=43.1892489966389!-81.61083984375!7!1!0!0%2C2 
# ^ CRS error is OK (CRS added in next 2 lines)

crs(phen_dorm_TP_2019)<-'+init=epsg:32617' #projection given in the file (utm in meters)
crs(phen_dorm_Borden_2019)<-'+init=epsg:32617' #projection given in the file (utm in meters)
crs(phen_dorm_left_2019)<-'+init=epsg:32617' #projection given in the file (utm in meters)
crs(phen_dorm_right_2019)<-'+init=epsg:32617' #projection given in the file (utm in meters)
phen_dorm_2019<-mosaic(phen_dorm_left_2019,phen_dorm_right_2019,phen_dorm_TP_2019,phen_dorm_Borden_2019,fun=mean) #merge the left and right rasters
#the function only averages pixels if they overlap (should not be an issue here)

#phen_dorm <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TNH_2019_TP.nc', varname = 'OGMn')
#crs(phen_dorm)<-'+init=epsg:32617' #projection given in the file (utm in meters)

phen_dorm_reproj_2019 <- projectRaster(phen_dorm_2019, crs=crs(dorm_MODIS_avg))
phen_dorm_resample_2019 <-resample(phen_dorm_reproj_2019,dorm_MODIS_avg)
plot(crop(phen_dorm_resample_2019,LC))
plot(phen_dorm_resample_2019,main='MSLSP Dormancy, 2019')

#2018
phen_dorm_TP_2018 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TNH_2018_TP.nc', varname = 'OGMn')
phen_dorm_Borden_2018 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TNK_2018_Borden.nc', varname = 'OGMn')
phen_dorm_left_2018 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TNJ_2018_Toronto.nc', varname = 'OGMn')
phen_dorm_right_2018 <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TPJ_2018_Toronto.nc', varname = 'OGMn')
#data from https://search.earthdata.nasa.gov/search/granules?p=C2102664483-LPDAAC_ECS&pg[0][v]=f&pg[0][qt]=2018-01-01T00%3A00%3A00.000Z%2C2020-01-01T23%3A59%3A59.999Z&pg[0][gsk]=-start_date&g=G2108838872-LPDAAC_ECS&q=MSLSP&sb[0]=-80.62646%2C42.5256%2C-80.08154%2C42.81998&tl=1645222323!3!!&m=43.1892489966389!-81.61083984375!7!1!0!0%2C2 
# ^ CRS error is OK (CRS added in next 2 lines)

crs(phen_dorm_TP_2018)<-'+init=epsg:32617' #projection given in the file (utm in meters)
crs(phen_dorm_Borden_2018)<-'+init=epsg:32617' #projection given in the file (utm in meters)
crs(phen_dorm_left_2018)<-'+init=epsg:32617' #projection given in the file (utm in meters)
crs(phen_dorm_right_2018)<-'+init=epsg:32617' #projection given in the file (utm in meters)
phen_dorm_2018<-mosaic(phen_dorm_left_2018,phen_dorm_right_2018,phen_dorm_TP_2018,phen_dorm_Borden_2018,fun=mean) #merge the left and right rasters
#the function only averages pixels if they overlap (should not be an issue here)

#phen_dorm <- raster('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/driver_data/ms_lsp/MSLSP_17TNH_2018_TP.nc', varname = 'OGMn')
#crs(phen_dorm)<-'+init=epsg:32617' #projection given in the file (utm in meters)

phen_dorm_reproj_2018 <- projectRaster(phen_dorm_2018, crs=crs(dorm_MODIS_avg))
phen_dorm_resample_2018 <-resample(phen_dorm_reproj_2018,dorm_MODIS_avg)
plot(crop(phen_dorm_resample_2018,LC))
plot(phen_dorm_resample_2018,main='MSLSP Dormancy, 2018')

phen_dorm_resample<-mean(phen_dorm_resample_2018,phen_dorm_resample_2019)

phen_dorm_resample <- round(phen_dorm_resample)
plot(phen_dorm_resample,main='MSLSP Dormancy, 2018-2019 average')

dorm_MODIS_test_avg[is.na(dorm_MODIS_test_avg) & phen_dorm_resample>200] <- phen_dorm_resample[is.na(dorm_MODIS_test_avg) & phen_dorm_resample>200]
dorm_MODIS_avg[is.na(dorm_MODIS_avg) & phen_dorm_resample>200] <- phen_dorm_resample[is.na(dorm_MODIS_avg) & phen_dorm_resample>200]

dorm_MODIS_avg[dorm_MODIS_avg>365]<-365 #any dormancy into the next year set to 365 (otherwise VPRM can't handle it)
dorm_MODIS_test_avg[dorm_MODIS_test_avg>365]<-365

plot(crop(dorm_MODIS_avg,LC))
plot(dorm_MODIS_avg)
plot(dorm_MODIS_test_avg-dorm_MODIS_avg)
plot(crop(dorm_MODIS_test_avg-dorm_MODIS_avg,LC))

writeRaster(dorm_MODIS_test_avg,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MODIS_V061_avg_Dormancy_2021.tif', overwrite=TRUE)
writeRaster(dorm_MODIS_test_avg,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MODIS_V061_avg_Dormancy_2021.nc', overwrite=TRUE, varname="EOS", varunit="",longname="End of Season", xname="Longitude", yname="Latitude")

#writeRaster(dorm_MODIS_test_avg,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/MODIS_phenology/MODIS_combined_avg_Dormancy_2019.tif', overwrite=TRUE)
