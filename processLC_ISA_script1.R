## Updates to code by Sabrina Madsen-Colford
## smadsen@physics.utoronto.ca

## Original code by IAN SMITH
## iasmith [at] bu.edu

# This script processes 500 m resolution land cover from MODI and impervious
# surface area (ISA) from the Global Man-made Impervious Surfaces (GMIS), the 
# City of Toronto impermeable surface (Toronto ISA), the Canadian Annual Crop 
# Inventory (ACI), and the Southern Ontario Land Resource Information System
# (SOLRIS V.3) (See the Toronto_permeability_plot_GMIS.R code to generate the
# ISA dataset), and C3:C4 ratios from the Annual Crop Inventory (see the 
# ACI_C3C4_fraction.R code to generate C3:C4 fraction code) for the UrbanVPRM. 

# This script creates the files /LandCover/MODIS_LC_GTA_500m_V061_<yr>.tif and
# /ISA/ISA_GTA_500m_<yr>.tif and /LandCover/C4_frac_GTA_500m_<yr>.tif (where
# <yr> is the specified year) used in Madsen-Colford et al. 2025.

# To run this code, file paths and directories will need to be updated to 
# import/write files. Portions of the code to be modified by the user are 
# marked above by '***'

# MODIS land cover data can be downloaded from:
#         https://appeears.earthdatacloud.nasa.gov/task/area

# ISA can be generated using the code: Toronto_permeability_plot_GMIS.R

# import packages
library("data.table")
library("raster")
library("sp")
library("rgdal")
library("shapefiles")

## Create Directories
# *** Change path
setwd("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files")

# *** Change city name/year
city = 'GTA_V061_500m_2021'

print("Create directories for LC data")
dir.create(paste0(city),showWarnings = FALSE)
dir.create(paste0(city,"/LandCover"),showWarnings = FALSE)
dir.create(paste0(city,"/ISA"),showWarnings = FALSE)

## Define CRS of LANDSAT and NLCD data
LANDSAT_CRS = "+proj=utm +zone=17 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
NLCD_CRS = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80
              +towgs84=0,0,0,0,0,0,0 +units=m +no_defs" 
MODIS_CRS = '+proj=longlat +datum=WGS84 +no_defs'


# Crop the data to the bounds of Toronto
# *** For Toronto uncomment this line:
bound_box_0 = readOGR(dsn="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/shapefiles", layer='Toronto_500m_BB')

# *** For fluxtowers uncomment this line:
# *** Change bounding box depending on flux tower being used 
#     (format: '<FT>_30m_BB_4km', where <FT> is the name of the fluxtower: 
#     choose from Borden, TPD, or TP39)
#bound_box_0 = readOGR(dsn="E:/Research/UrbanVPRM/dataverse_files/Borden/shapefiles", layer='Borden_30m_BB_4km') #TPD/shapefiles",layer='TPD_30m_BB_4km')

bound_box_MODIS = spTransform(bound_box_0, '+proj=longlat +datum=WGS84 +no_defs')

## Import, reproject, and crop LC and ISA data
# Land Cover
# *** Change path/file name:
LC_ON = raster("C:/Users/kitty/Documents/Research/SIF/SMUrF/data/MCD12Q1/MCD12Q1.061_LC_Type1_doy2021001_aid0001.tif")
LC_crop = crop(LC_ON, bound_box_MODIS)


# ISA ***NEED to generate using Toronto_permeability_plot_GMIS.R
# *** Change path/file name
ISA_ON = raster("C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/Impermeable_Surface/GMIS_Toronto_ACI_SOLRIS_2021_impervious_GTA.tif")#all_aggregated_impervious_63_TPD.tif") # Impervious data from https://www.mrlc.gov/data
ISA_ONcrop = crop(ISA_ON,bound_box_MODIS)

# Load in C3:C4 fraction from the Annual Crop inventory (ACI) of Canada
# *** Generate using 'ACI_C3C4_fraction.R'
# *** Change path/file name
C4_ON = raster("C:/Users/kitty/Documents/Research/SIF/SMUrF/data/ACI_C4_fraction_GTA_500m_2021.tif") # C3/C4 data from ACI aggregated to 500m res
C4_ONcrop = crop(C4_ON,bound_box_MODIS)

# Write Rasters
# *** Change Paths/file names:
writeRaster(LC_crop,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2021/LandCover/MODIS_V061_LC_GTA_500m_2021.tif",
            overwrite=TRUE)
writeRaster(ISA_ONcrop,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2021/ISA/ISA_GTA_GMIS_Toronto_ACI_SOLRIS_500m_2021.tif",
            overwrite=TRUE)
writeRaster(C4_ONcrop,filename="C:/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/GTA_V061_500m_2021/LandCover/C4_frac_GTA_500m_2021.tif",
            overwrite=TRUE)
