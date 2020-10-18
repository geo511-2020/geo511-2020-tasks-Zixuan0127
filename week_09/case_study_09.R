library(sf)
library(tidyverse)
library(ggmap)
library(rnoaa)
library(spData)
data(world)
data(us_states)
# 2020 update - it appears NOAA changed the URL which broke the R function.  Use the following instead of storm_shp().
dataurl="https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/shapefile/IBTrACS.NA.list.v04r00.points.zip"
tdir="E:/UB/Spatial Data Science/geo511-2020-tasks-Zixuan0127/week_09"
download.file(dataurl,destfile=file.path(tdir,"temp.zip"))
unzip(file.path(tdir,"temp.zip"),exdir = tdir)
list.files(tdir)
storm_data <- read_sf(list.files(tdir,pattern=".shp"))

