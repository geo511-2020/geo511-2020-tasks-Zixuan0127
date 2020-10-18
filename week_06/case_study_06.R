library(raster)
library(sp)
library(spData)
library(tidyverse)
library(sf)
library(ncdf4)


data(world)  #load 'world' data from spData package



tmean=raster("absolute.nc")

world_no_ant = world %>%
  filter(continent != "Antarctica")

world_sp = as(world_no_ant, "Spatial")
max_tem = st_as_sf(raster::extract(tmean,world_sp,fun=max,na.rm=T,small=T,sp=T))
max_tem = max_tem%>%
  mutate(tmax=CRU_Global_1961.1990_Mean_Monthly_Surface_Temperature_Climatology,
         .keep = "unused")

map1=ggplot(max_tem)+
      geom_sf(aes(fill=tmax))+
      scale_fill_viridis_c(name="Annual\nMaximum\nTemperature (C)")+
      theme(legend.position = "bottom")

plot(map1)

hottest_country = st_set_geometry(max_tem,value = NULL)
hottest_country = hottest_country%>%
  select(continent,name_long,tmax)%>%
  group_by(continent)%>%
  top_n(1)%>%
  arrange(desc(tmax))
print(hottest_country)
