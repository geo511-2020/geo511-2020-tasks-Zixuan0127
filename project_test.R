library(leaflet)
library(ggplot2)
library(tidyverse)
library(mapview)
library(leafpop)
library(sf)
library(lubridate)

#Import data and transform them to Spatial data
station = read_csv("E:/UB/Spatial Data Science/info.csv")
station = st_as_sf(station[1,], coords = c("x", "y"), crs = 4326)

#Import and clean precipitation data
##S1
S1 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01359165&period=&begin_date=2018-12-14&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S1 = S1%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  group_by(Date)%>%
  summarise(sum= sum(P))
S1[S1 == 0]=NA

##S2
S2 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01511000&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S2 = S2%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  group_by(Date)%>%
  summarise(sum= sum(P))
S2[S2 == 0]=NA

#Plot
##S1
S1_plot = ggplot(S1,aes(Date,sum))+
  geom_point(col = "red")+
  geom_smooth()+
  labs(x="Date",
       y="Daily Precipitation (inches)",
       title = "Rainfall Plot",
       subtitle = "Hudson River at Port of Albany, NY.")+
  theme_light()

plot(S1_plot)
ggsave("S1.png",S1_plot)
#mapview(station)

#m = leaflet() %>%
#      addTiles() %>%
#      addCircleMarkers(data = station,
#                     popup = station$Station_Name,
#                     radius = 2)

m = mapview(station, map.types = "Esri.WorldImagery",
            popup = popupImage("E:/UB/Spatial Data Science/geo511-2020-tasks-Zixuan0127/S1.png", src = "remote"))
print(m)