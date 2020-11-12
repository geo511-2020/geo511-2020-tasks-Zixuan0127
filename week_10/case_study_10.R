library(raster)
library(rasterVis)
library(rgdal)
library(ggmap)
library(tidyverse)
library(knitr)
library(ncdf4)

# Create afolder to hold the downloaded data
dir.create("data",showWarnings = F) #create a folder to hold the data

lulc_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MCD12Q1.051_aid0001.nc?raw=true"
lst_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MOD11A2.006_aid0001.nc?raw=true"

# download them
download.file(lulc_url,destfile="data/MCD12Q1.051_aid0001.nc", mode="wb")
download.file(lst_url,destfile="data/MOD11A2.006_aid0001.nc", mode="wb")

lulc=stack("data/MCD12Q1.051_aid0001.nc",varname="Land_Cover_Type_1")
lst=stack("data/MOD11A2.006_aid0001.nc",varname="LST_Day_1km")

plot(lulc)

lulc=lulc[[13]]
plot(lulc)

Land_Cover_Type_1 = c(
  Water = 0, 
  `Evergreen Needleleaf forest` = 1, 
  `Evergreen Broadleaf forest` = 2,
  `Deciduous Needleleaf forest` = 3, 
  `Deciduous Broadleaf forest` = 4,
  `Mixed forest` = 5, 
  `Closed shrublands` = 6,
  `Open shrublands` = 7,
  `Woody savannas` = 8, 
  Savannas = 9,
  Grasslands = 10,
  `Permanent wetlands` = 11, 
  Croplands = 12,
  `Urban & built-up` = 13,
  `Cropland/Natural vegetation mosaic` = 14, 
  `Snow & ice` = 15,
  `Barren/Sparsely vegetated` = 16, 
  Unclassified = 254,
  NoDataFill = 255)

lcd=data.frame(
  ID=Land_Cover_Type_1,
  landcover=names(Land_Cover_Type_1),
  col=c("#000080","#008000","#00FF00", "#99CC00","#99FF99", "#339966", "#993366", "#FFCC99", "#CCFFCC", "#FFCC00", "#FF9900", "#006699", "#FFFF00", "#FF0000", "#999966", "#FFFFFF", "#808080", "#000000", "#000000"),
  stringsAsFactors = F)
# colors from https://lpdaac.usgs.gov/about/news_archive/modisterra_land_cover_types_yearly_l3_global_005deg_cmg_mod12c1
kable(head(lcd))

# convert to raster (easy)
lulc=as.factor(lulc)

# update the RAT with a left join
levels(lulc)=left_join(levels(lulc)[[1]],lcd)

# plot it
gplot(lulc)+
  geom_raster(aes(fill=as.factor(value)))+
  scale_fill_manual(values=levels(lulc)[[1]]$col,
                    labels=levels(lulc)[[1]]$landcover,
                    name="Landcover Type")+
  coord_equal()+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(ncol=1,byrow=TRUE))

plot(lst[[1:12]])

offs(lst)=-273.15
plot(lst[[1:10]])

lstqc=stack("data/MOD11A2.006_aid0001.nc",varname="QC_Day")
plot(lstqc[[1:2]])

values(lstqc[[1:2]])%>%table()

intToBits(65)

intToBits(65)[1:8]

as.integer(intToBits(65)[1:8])

rev(as.integer(intToBits(65)[1:8]))


## set up data frame to hold all combinations
QC_Data <- data.frame(Integer_Value = 0:255,
                      Bit7 = NA, Bit6 = NA, Bit5 = NA, Bit4 = NA,
                      Bit3 = NA, Bit2 = NA, Bit1 = NA, Bit0 = NA,
                      QA_word1 = NA, QA_word2 = NA, QA_word3 = NA,
                      QA_word4 = NA)

## 
for(i in QC_Data$Integer_Value){
  AsInt <- as.integer(intToBits(i)[1:8])
  QC_Data[i+1,2:9]<- AsInt[8:1]
}

QC_Data$QA_word1[QC_Data$Bit1 == 0 & QC_Data$Bit0==0] <- "LST GOOD"
QC_Data$QA_word1[QC_Data$Bit1 == 0 & QC_Data$Bit0==1] <- "LST Produced,Other Quality"
QC_Data$QA_word1[QC_Data$Bit1 == 1 & QC_Data$Bit0==0] <- "No Pixel,clouds"
QC_Data$QA_word1[QC_Data$Bit1 == 1 & QC_Data$Bit0==1] <- "No Pixel, Other QA"

QC_Data$QA_word2[QC_Data$Bit3 == 0 & QC_Data$Bit2==0] <- "Good Data"
QC_Data$QA_word2[QC_Data$Bit3 == 0 & QC_Data$Bit2==1] <- "Other Quality"
QC_Data$QA_word2[QC_Data$Bit3 == 1 & QC_Data$Bit2==0] <- "TBD"
QC_Data$QA_word2[QC_Data$Bit3 == 1 & QC_Data$Bit2==1] <- "TBD"

QC_Data$QA_word3[QC_Data$Bit5 == 0 & QC_Data$Bit4==0] <- "Emiss Error <= .01"
QC_Data$QA_word3[QC_Data$Bit5 == 0 & QC_Data$Bit4==1] <- "Emiss Err >.01 <=.02"
QC_Data$QA_word3[QC_Data$Bit5 == 1 & QC_Data$Bit4==0] <- "Emiss Err >.02 <=.04"
QC_Data$QA_word3[QC_Data$Bit5 == 1 & QC_Data$Bit4==1] <- "Emiss Err > .04"

QC_Data$QA_word4[QC_Data$Bit7 == 0 & QC_Data$Bit6==0] <- "LST Err <= 1"
QC_Data$QA_word4[QC_Data$Bit7 == 0 & QC_Data$Bit6==1] <- "LST Err > 2 LST Err <= 3"
QC_Data$QA_word4[QC_Data$Bit7 == 1 & QC_Data$Bit6==0] <- "LST Err > 1 LST Err <= 2"
QC_Data$QA_word4[QC_Data$Bit7 == 1 & QC_Data$Bit6==1] <- "LST Err > 4"
kable(head(QC_Data))

keep=QC_Data[QC_Data$Bit1 == 0,]
keepvals=unique(keep$Integer_Value)
keepvals

qcvals=table(values(lstqc))  # this takes a minute or two


QC_Data%>%
  dplyr::select(everything(),-contains("Bit"))%>%
  mutate(Var1=as.character(Integer_Value),
         keep=Integer_Value%in%keepvals)%>%
  inner_join(data.frame(qcvals))%>%
  kable()

lstkeep=calc(lstqc,function(x) x%in%keepvals)

gplot(lstkeep[[4:8]])+
  geom_raster(aes(fill=as.factor(value)))+
  facet_grid(variable~.)+
  scale_fill_manual(values=c("blue","red"),name="Keep")+
  coord_equal()+
  theme(legend.position = "bottom")

lst=mask(lst,mask=lstkeep,maskval=0)

names(lst)[1:5]

tdates=names(lst)%>%
  sub(pattern="X",replacement="")%>%
  as.Date("%Y.%m.%d")

names(lst)=1:nlayers(lst)
lst=setZ(lst,tdates)

#Part 1
lw=SpatialPoints(data.frame(x= -78.791547,y=43.007211))
projection(lw) = "+proj=longlat"
lw = spTransform(lw,"+proj=longlat")
lw_lst = raster::extract(lst,lw,buffer=1000,fun=mean,na.rm=T)%>%
  t()
lst_date = getZ(lst)
lst_re = cbind.data.frame(lst = lw_lst, date = lst_date)

p1 = ggplot(lst_re,aes(date,lst))+
  geom_point()+
  geom_smooth(span = 0.02,color = 'blue',n=500)
plot(p1)

# Part 2
tmonth = as.numeric(format(getZ(lst),"%m"))
lst_month = stackApply(lst,tmonth,fun = mean)
names(lst_month) = month.name
gplot(lst_month)+
  geom_tile(aes(fill = value))+
  facet_wrap(~variable)+
  scale_fill_gradientn(colours=c("blue",mid="grey","red"))+
  theme(axis.text = element_blank())

cellStats(lst_month,mean)%>%
  kable(digits = round(2),format = 'rst',col.names = "Average")


# Part 3
lc = resample(lulc,lst,method = 'ngb')

lcds1=cbind.data.frame(
  values(lst_month),
  ID=values(lc[[1]]))%>%
  na.omit()
g_data = lcds1%>%
  gather(key='month',value='value',-ID)%>%
  mutate(ID = as.numeric(ID),
         month=factor(month,levels=month.name,ordered=T))%>%
  left_join(lcd,by='ID')%>%
  filter(landcover %in% c("Urban & built-up","Deciduous Broadleaf forest"))
  
p3 = ggplot(g_data,aes(month,value))+
  geom_point(position = position_jitter(w=0.3,h=0),alpha = 0.7)+
  geom_violin(fill = 'gray',color = 'red',alpha = 0.5)+
  facet_wrap(~landcover)+
  labs(x = "Month",
       y = "Monthly Mean Land Surface Temperature (C)",
       title = "Land Surface Temperature in Urban and Forest")+
  theme(axis.text = element_text(angle = 90))

plot(p3)