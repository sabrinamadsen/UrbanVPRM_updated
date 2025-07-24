#!/bin/bash -l

# Set directories and shell variables
cd dataverse_files/RAP/2023/202312/
varname='TMP_2maboveground'

# Convert .grb2 to .nc

for grbfile in rap_130_2023123*.grb2
do
tmp=${grbfile/.grb2/}
outfile=$tmp\.nc
/mnt/c/Users/kitty/Documents/Research/SIF/UrbanVPRM/grib2/wgrib2/wgrib2 $grbfile -netcdf $outfile
#done

# Convert .nc to .tif

for ncfile in *.nc
do
tmp=${ncfile/.nc/}
outDIR=/mnt/c/Users/kitty/Documents/Research/SIF/UrbanVPRM/UrbanVPRM/dataverse_files/RAP/2023/origTIFF/
outfile=$outDIR$tmp\.tif
echo $tmp
gdal_translate -a_srs "+proj=lcc +lat_1=25 +lat_2=25 +lat_0=25 +lon_0=-95 +x_0=0 +y_0=0 +a=6371229 +b=6371229 +units=m +no_defs" NETCDF:$ncfile:$varname $outfile
rm $ncfile  #I added this to remove netcdf files after they have been converted to tif files
done
done
