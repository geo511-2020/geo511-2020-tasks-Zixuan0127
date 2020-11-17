library(tidyverse)
library(spData)
library(sf)

## New Packages
library(mapview) # new package that makes easy leaflet maps
library(foreach)
library(doParallel)
registerDoParallel(4)
getDoParWorkers() # check registered cores
library(tidycensus)



racevars <- c(White = "P005003", 
              Black = "P005004", 
              Asian = "P005006", 
              Hispanic = "P004003")

options(tigris_use_cache = TRUE)
erie <- get_decennial(geography = "block", variables = racevars, 
                      state = "NY", county = "Erie County", geometry = TRUE,
                      summary_var = "P001001", cache_table=T) 

erie = st_crop(erie,c(xmin=-78.9,xmax=-78.85,ymin=42.888,ymax=42.92))

erie$variable = as.factor(erie$variable)

erie_all = NULL
for (i in levels(erie$variable)){
erie_s = erie %>%
  filter(variable == i)%>%
  st_sample(size = .$value)%>%
  st_as_sf()%>%
  mutate(variable = i)
erie_all = rbind(erie_all,erie_s)
}

mapview(erie_all,zcol = "variable", cex = 2)