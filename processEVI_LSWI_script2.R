## Updates to code by Sabrina Madsen-Colford
## smadsen@physics.utoronto.ca

## Original code by IAN SMITH
## iasmith [at] bu.edu

# This script calculates, and interpolates daily EVI and LSWI values for each 
# pixel in the study domain from MODIS surface reflectance data

# This script creates the file adjusted_evi_lswi_interpolated_modis.csv' used in 
# Madsen-Colford et al. 2025.

# To run this code, file paths and directories will need to be updated to 
# import/write files. Portions of the code to be modified by the user are 
# marked above by '***'

# MODIS surface reflectance (MOD09A1) V6.1 data were downloaded from: 
#     https://appeears.earthdatacloud.nasa.gov/task/area

# import packages
library("data.table")
library("raster")
library("sp")
library("rgdal")
library("lubridate")

#MODIS Landcover:
# *** Change path/file name
LC_dat<-raster("E:/Research/UrbanVPRM/dataverse_files/GTA_V061_500m_2021/LandCover/MODIS_V061_LC_GTA_500m_2021.tif")

#MODIS reflectance files
# *** Change path/file name
setwd('C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/MODIS_reflectance/MODIS_V061_GTA_AppEEARS_2021')
mod_files <- list.files(pattern = 'MOD09A1.061_sur_refl_b01')

# *** Change year of interest:
yr<-2021

#QC values where red,NIR, and blue pass the quality test:
EVI_qc<-c(1073741824,1073954817,1075838976,1075838977,1075838979,1076051969, 1075576832,
          1076625411,1077149697,1077411843,1119879171,1121976323,1123287043,
          1123549187,1128267777,1130364929,1130364931,1131151363,1131675649,
          1131937795,1132462083,1134559235,1135345667,1135869955,1136132099,
          1811939331,1814036483,1814822915,1815347203,1815609347,1858076675,
          1860173827,1861484547,1861746691,1866465283,1868562435,1869873155,
          1870135299,1870659587,1872756739,1874067459,1874329603,1946157057,
          1946370049,1948254209,1948254211,1948467201,1949040643,1949564929,
          1949827075,1992294403,1994391555,1995702275,1995964419,2000683009,
          2002780161,2002780163,2003566595,2004090881,2004303873,2004353027,
          2004877315,2006974467,2007760899,2008285187,2008547331,2013265923,
          2013478915,2015363075,2015576067,2016149507,2016673795,2016886787,
          2016935939,2059403267,2061500419,2062811139,2063073283,2067791875,
          2069889027,2070675459,2071199747,2071461891,2071986179,2074083331,
          2074869763,2075394051,2075656195)
#QC values where NIR and SWIR pass the quality test:
LSWI_qc<-c(1073741824,1073741877,1073954817,1073954869,1075838976,1075838977, 1075576832,
           1075838979,1075839029,1076051969,1076052021,1076625411,1077149697,
           1077149749,1077362741,1077411843,1077411895,1119879171,1119879223,
           1121976323,1121976375,1123287043,1123287095,1123549187,1128267777,
           1128267829,1128480821,1130364929,1130364931,1130364981,1131151363,
           1131675649,1131675701,1131937795,1131937847,1132462083,1132462135,
           1132675127,1134559235,1134559287,1135345667,1135345719,1135869955,
           1135870007,1136132099,1136132151)

for (i in 1:length(mod_files)){
  ## read in raster stack
  mod_file_red <- crop(stack(mod_files[i]),LC_dat)
  mod_file_NIR <-crop(stack(paste(substr(mod_files[i],1,23),"2",substr(mod_files[i],25,47),sep = "")),LC_dat)
  mod_file_blue <-crop(stack(paste(substr(mod_files[i],1,23),"3",substr(mod_files[i],25,47),sep = "")),LC_dat)
  mod_file_SWIR <-crop(stack(paste(substr(mod_files[i],1,23),"7",substr(mod_files[i],25,47),sep = "")),LC_dat)
  mod_file_QC <- crop(stack(paste(substr(mod_files[i],1,21),"qc_500m",substr(mod_files[i],25,47),sep = "")),LC_dat)
  
  file <- stack(mod_file_red,mod_file_NIR,mod_file_blue,mod_file_SWIR,mod_file_QC)
  
  # Filter out values with bad quality flags
  EVI_bad_pixels <- which(!(values(file[[5]])) %in% EVI_qc) 
  LSWI_bad_pixels <- which(!(values(file[[5]])) %in% LSWI_qc)
  file[[1]][EVI_bad_pixels]<-NA
  file[[3]][EVI_bad_pixels]<-NA
  file[[4]][LSWI_bad_pixels]<-NA
  
  ### calculate EVI
  file[[6]] <- 2.5 * ((file[[2]] - file[[1]]) / (file[[2]] + 6 * file[[1]] - 7.5 * file[[3]] + 1))
  ## calculate LSWI
  file[[7]] <- (file[[2]] - file[[4]]) / (file[[2]] + file[[4]])
  
  ## convert raster data to data.table and assign Day of Year values
  ## EVI
  EVI.dt = as.data.table(as.data.frame(file[[6]], xy=T))
  EVI.dt = cbind(1:ncell(file[[6]]), EVI.dt)
  setnames(EVI.dt,c("Index","x","y", "EVI"))
  setkey(EVI.dt,Index,x,y)
  ## LSWI
  LSWI.dt = as.data.table(as.data.frame(file[[7]], xy=T))
  LSWI.dt = cbind(1:ncell(file[[7]]), LSWI.dt)
  
  # *** May need to change the start and end of the substrings depending on
  #     the naming convention of the files.
  # If it is the year before the year of interest assign it negative Day of Year (DOY)
  if (as.numeric((substr(mod_files[i],29,32)))<yr){
    LSWI.dt$DOY = as.numeric((substr(mod_files[i],33,35)))-365+1 #extract day of year from file name and subtract 364
  }else if (as.numeric((substr(mod_files[i],29,32)))>yr){ #if after add 365 days
    LSWI.dt$DOY = as.numeric((substr(mod_files[i],33,35)))+365 # *** Change to 366 for leap years
  }else{
    LSWI.dt$DOY = as.numeric((substr(mod_files[i],33,35)))
  }
  setnames(LSWI.dt,c("Index","x","y", "LSWI","DOY"))
  setkey(LSWI.dt,Index,x,y)
  ## save a data.table for each image
  assign(paste0('EVI_LSWI',i,'.dt'), merge(EVI.dt,LSWI.dt,by=c("Index","x","y")))
}

# Combine the data
EVI_LSWI.dt <- rbind(EVI_LSWI1.dt,EVI_LSWI2.dt,EVI_LSWI3.dt,EVI_LSWI4.dt,EVI_LSWI5.dt,EVI_LSWI6.dt,
                     EVI_LSWI7.dt,EVI_LSWI8.dt,EVI_LSWI9.dt,EVI_LSWI10.dt,EVI_LSWI11.dt, EVI_LSWI12.dt,
                     EVI_LSWI13.dt,EVI_LSWI14.dt,EVI_LSWI15.dt,EVI_LSWI16.dt,EVI_LSWI17.dt,EVI_LSWI18.dt,
                     EVI_LSWI19.dt,EVI_LSWI20.dt,EVI_LSWI21.dt,EVI_LSWI22.dt,EVI_LSWI23.dt,EVI_LSWI24.dt,
                     EVI_LSWI25.dt,EVI_LSWI26.dt,EVI_LSWI27.dt,EVI_LSWI28.dt,EVI_LSWI29.dt,EVI_LSWI30.dt,
                     EVI_LSWI31.dt,EVI_LSWI32.dt,EVI_LSWI33.dt,EVI_LSWI34.dt,EVI_LSWI35.dt,EVI_LSWI36.dt,
                     EVI_LSWI37.dt,EVI_LSWI38.dt,EVI_LSWI39.dt,EVI_LSWI40.dt,EVI_LSWI41.dt,EVI_LSWI42.dt,
                     EVI_LSWI43.dt,EVI_LSWI44.dt,EVI_LSWI45.dt,EVI_LSWI46.dt,EVI_LSWI47.dt,EVI_LSWI48.dt,
                     EVI_LSWI49.dt)

## order by DOY
EVI_LSWI.dt <- EVI_LSWI.dt[order(EVI_LSWI.dt$DOY),]

## convert to dataframe
EVI_LSWI.df <- as.data.frame(EVI_LSWI.dt)

## There are a few erroneous values e.g. EVI = 2.5. These are omitted from the dataset 
EVI_LSWI.df[which(EVI_LSWI.df$EVI > 1),'EVI'] <- NA
EVI_LSWI.df[which(EVI_LSWI.df$EVI < -1),'EVI'] <- NA

EVI_LSWI.df[which(EVI_LSWI.df$LSWI > 1),'LSWI'] <- NA
EVI_LSWI.df[which(EVI_LSWI.df$LSWI < -1),'LSWI'] <- NA

## Set up new dataframe for EVI/LSWI Interpolation
npixel <- length(unique(EVI_LSWI.df$Index))
inter_evi_lswi <- as.data.frame(matrix(ncol = 2, nrow = npixel*378))
colnames(inter_evi_lswi) <- c('Index', 'DOY')
inter_evi_lswi[,1] <- rep(seq(1,npixel,1), 378)
inter_evi_lswi <- inter_evi_lswi[order(inter_evi_lswi[,1]),]
inter_evi_lswi[,2] <- rep(seq(-3,374,1), npixel)
inter_evi_lswi <- merge(inter_evi_lswi, EVI_LSWI.df[,c('Index', 'EVI', 'LSWI', 'DOY')], by = c('Index', 'DOY'), all = TRUE)
inter_evi_lswi  <- inter_evi_lswi[order(inter_evi_lswi[,'Index'],inter_evi_lswi [,'DOY']),]

# add coordinates
x <- as.data.frame(tapply(EVI_LSWI.df$x, EVI_LSWI.df$Index, mean, na.rm = T))
y <- as.data.frame(tapply(EVI_LSWI.df$y, EVI_LSWI.df$Index, mean, na.rm = T))
xy <- cbind(as.numeric(rownames(x)), x, y)
colnames(xy) <- c('Index', 'x', 'y')

inter_evi_lswi <- merge(inter_evi_lswi, xy, by = 'Index', all = TRUE)
inter_evi_lswi <- inter_evi_lswi[,c('Index', 'DOY', 'x', 'y', 'EVI', 'LSWI')]

# Interpolate daily EVI values for each pixel
inter_evi_lswi$EVI_inter <- NA

for(i in unique(inter_evi_lswi$Index)){
  if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$EVI)),'EVI']) > 10){
    pix <- inter_evi_lswi[which(inter_evi_lswi$Index == i),]
    spl <- with(pix[!is.na(pix$EVI),],smooth.spline(DOY,EVI, spar = .25)) 
    inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI_inter'] <- predict(spl, c(-3:374))$y
  } else {inter_evi_lswi[which(inter_evi_lswi$Index == i),'EVI_inter'] <- NA}
  print(round(i/npixel*100, 1))
}


# Interpolate daily LSWI values for each pixel
inter_evi_lswi$LSWI_inter <- NA

for(i in unique(inter_evi_lswi$Index)){
  if(is.finite(mean(inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI'], na.rm = T)) & length(inter_evi_lswi[which(inter_evi_lswi$Index == i & is.finite(inter_evi_lswi$LSWI)),'LSWI']) > 10){
    pix <- inter_evi_lswi[which(inter_evi_lswi$Index == i),]
    spl <- with(pix[!is.na(pix$LSWI),],smooth.spline(DOY,LSWI, spar = .25)) 
    inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter'] <- predict(spl, c(-3:374))$y
  } else {inter_evi_lswi[which(inter_evi_lswi$Index == i),'LSWI_inter'] <- NA}
  print(round(i/npixel*100, 1))
}

inter_evi_lswi.dt<-as.data.table(inter_evi_lswi)
inter_evi_lswi.dt<-inter_evi_lswi.dt[inter_evi_lswi.dt$DOY>0 & inter_evi_lswi.dt$DOY<366] #select only data that falls within the year
inter_evi_lswi.df<-as.data.frame(inter_evi_lswi.dt)#convert back to dataframe


# Save the interpolated EVI & LSWI data
# *** Change path/file name
write.table(inter_evi_lswi.df,'C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_500m_V061_2018/adjusted_evi_lswi_interpolated_modis.csv',row.names = F,
            sep=',')