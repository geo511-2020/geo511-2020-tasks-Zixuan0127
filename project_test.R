library(leaflet)
library(ggplot2)
library(tidyverse)
library(mapview)
library(leafpop)
library(sf)
library(lubridate)


################################################################################

#Import and clean precipitation data
##S1
S1 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01359165&period=&begin_date=2018-12-14&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S1 = S1%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)

##S2
S2 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01511000&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S2 = S2%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S3
S3 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=03010820&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S3 = S3%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S4
S4 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=03011020&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S4 = S4%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S5
S5 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=421932078513701&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S5 = S5%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S6
S6 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=421946078274901&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S6 = S6%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S7
S7 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=420703079442501&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S7 = S7%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S8
S8 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=421429079295601&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S8 = S8%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S9
S9 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=421511079161701&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S9 = S9%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S10
S10 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01510000&period=&begin_date=2020-06-23&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S10 = S10%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##11
S11 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01499500&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S11 = S11%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S12
S12 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01200000&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S12 = S12%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S13
S13 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=04294413&period=&begin_date=2015-04-16&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S13 = S13%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S14
S14 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=04256500&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S14 = S14%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S15
S15 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=04260500&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S15 = S15%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S16
S16 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=411405074141501&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S16 = S16%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S17
S17 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=0135980207&period=&begin_date=2018-12-15&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S17 = S17%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S18
S18 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01376269&period=&begin_date=2019-02-22&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S18 = S18%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S19
S19 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=410518074020300&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S19 = S19%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S20
S_20 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=424133077495701&period=&begin_date=2016-04-19&end_date=2020-10-17",
                skip = 29,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))

S_20 = S_20%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S21
S_21 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=424421077495301&period=&begin_date=2015-10-15&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S_21 = S_21%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S22
S_22 = read.delim("https://waterdata.usgs.gov/ny/nwis/uv?cb_00045=on&format=rdb&site_no=425844077532901&period=&begin_date=2020-09-10&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S_22 = S_22%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S23
S_23 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=04229500&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S_23 = S_23%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S24
S_24 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=410828074065801&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S_24 = S_24%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S25
S_25 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01354330&period=&begin_date=2018-12-11&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S_25 = S_25%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S26
S_26 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01521000&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S_26 = S_26%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S27
S_27 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01523000&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S_27 = S_27%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S28
S_28 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01305575&period=&begin_date=2020-06-23&end_date=2020-10-17",
                skip = 29,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S_28 = S_28%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S29
S_29 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=405925072165601&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S_29 = S_29%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S30
S_30 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01434498&period=&begin_date=2019-08-21&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S_30 = S_30%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S31
S_31 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01310740&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S_31 = S_31%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S32
S_32 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01376515&period=&begin_date=2019-05-16&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S_32 = S_32%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(P = as.numeric(P))%>%
  mutate(Station_N = as.character(Station_N))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S33
S_33 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01376520&period=&begin_date=2019-03-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S_33 = S_33%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(Station_N = as.character(Station_N))%>%
  mutate(P = as.numeric(P))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S34
S_34 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=01435000&period=&begin_date=2019-09-20&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S_34 = S_34%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(Station_N = as.character(Station_N))%>%
  mutate(P = as.numeric(P))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)
##S35
S_35 = read.delim("https://nwis.waterdata.usgs.gov/ny/nwis/uv/?cb_00045=on&format=rdb&site_no=04230380&period=&begin_date=2020-06-19&end_date=2020-10-17",
                skip = 28,
                col.names = c("USGS","Station_N","Date","EST","P","Type"))
S_35 = S_35%>%
  mutate(Date = floor_date(ymd_hm(Date),unit = "day"),.keep="unused")%>%
  mutate(Station_N = as.character(Station_N))%>%
  mutate(P = as.numeric(P))%>%
  group_by(Date,Station_N)%>%
  summarise(sum= sum(P))%>%
  filter(sum != 0)


################################################################################

#Plot
all_sta_list = list(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,
                    S11,S12,S13,S14,S15,S16,S17,S18,
                    S19,S_20,S_21,S_22,S_23,S_24,S_25,
                    S_26,S_27,S_28,S_29,S_30,S_31,S_32,S_33,
                    S_34,S_35)
all_pt = list()
for (i in 1:35){
  pt=as.data.frame(all_sta_list[i])
  m=ggplot(pt,aes(as.Date(Date),sum))+
    geom_point(col = "red")+
    geom_smooth()+
    scale_x_date(date_labels = "%m-%Y")+
    labs(x="Date",
         y="Daily Precipitation (inches)",
         title = "Rainfall Plot")+
    theme_light()
  all_pt[[i]] <- m
  file_name = paste("S",i,".png",sep = "")
  #ggsave(file_name)
}
################################################################################
all_sta = do.call("rbind",list(S1,S2,S3,S4,S5,S6,S7,S8,S9,S10,
                               S11,S12,S13,S14,S15,S16,S17,S18,
                               S19,S_20,S_21,S_22,S_23,S_24,S_25,
                               S_26,S_27,S_28,S_29,S_30,S_31,S_32,S_33,
                               S_34,S_35))

#Import data and transform them to Spatial data
station = read_csv("E:/UB/Spatial Data Science/info.csv",
                   col_types = "cddd")

station = station%>%
  mutate(Station_N = as.character(Station_N))
all_sta_info = all_sta%>%
  group_by(Station_N)%>%
  summarise(Ave = mean(sum),
            Max = max(sum),
            Min = min(sum))%>%
  left_join(station,.by="Station_N")

all_sta_geo = st_as_sf(all_sta_info, coords = c("x", "y"), crs = 4326)

all_sta_info = all_sta_info%>%
  select(-x,-y)

m = leaflet() %>%
      addTiles() %>%
      addCircleMarkers(data = all_sta_geo,
                       group = "Information",
                       popup = popupTable(all_sta_info))%>%
      addCircleMarkers(data =all_sta_geo ,
                     group = "Plot",
                     popup = popupGraph(all_pt,type = "png"))%>%
      addLayersControl(overlayGroups = c("Information","Plot"))

#m = mapview(station, map.types = "Esri.WorldImagery",
#            popup = popupImage("E:/UB/Spatial Data Science/geo511-2020-tasks-Zixuan0127/S1.png", src = "remote"))
print(m)

