library(ggplot2)
library(readr)
dataurl = "https://raw.githubusercontent.com/AdamWilsonLab/SpatialDataScience/master/docs/02_assets/buffaloweather.csv"

temp <- read_csv(dataurl,
                 skip = 1,
                 na = '999.90',
                 col_names = c("YEAR","JAN","FEB","MAR", 
                               "APR","MAY","JUN","JUL",  
                               "AUG","SEP","OCT","NOV",  
                               "DEC","DJF","MAM","JJA",  
                               "SON","metANN"))
summary(temp)

JJA_mean = ggplot(temp,aes(YEAR,JJA))+
  geom_smooth(color ='red')+
  geom_line()+
  labs(x = "Year",
       y = "Mean Summer Temperatures (C)",
       title = "Mean Summer Temperatures in Buffalo, NY",
       subtitle = "Summer inclueds June, July, and August
Data from the Global Historical Climate Network 
Red line is a LOESS smooth")+
  theme(axis.title = element_text(size = 16))
ggsave('Case_study_2.png',plot = JJA_mean)
print(JJA_mean)
#JJA_indiv = JJA_mean +
# geom_line(aes(y=JUN),color = 'coral' )+
#  geom_line(aes(y=JUL),color = 'darkorchid' )+
#  geom_line(aes(y=AUG),color = 'hotpink' )
