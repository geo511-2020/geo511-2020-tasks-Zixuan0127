library(gapminder)
library(ggplot2)
library(dplyr)
gapminder = gapminder
summary(gapminder)
# remove "Kuwait" from dataset.
gapminder_noKuwait = gapminder %>%
  filter(country != "Kuwait")
# Plot 1
fig1 = ggplot(gapminder_noKuwait,aes(lifeExp,gdpPercap, color = continent,
                                     size = pop/100000))+
  geom_point()+
  facet_wrap( ~ year, nrow =1)+
  scale_y_continuous(trans = "sqrt")+
  labs(x= "Life Expectancy",
       y= "GDP Per Capita",
       color = "Continent",
       size = "Population (100k)")+
  theme_bw()
print(fig1)

# Data for fig 2
gapminder_con_year = gapminder %>%
  group_by(continent,year) %>%
  summarize(gdpPercapweighted = weighted.mean(x=gdpPercap,w=pop), 
            pop = sum(as.numeric(pop)))
# Plot 2
fig2 = 
  ggplot(gapminder_con_year,aes(year,gdpPercapweighted))+
  geom_line(data = gapminder_noKuwait,aes(year,gdpPercap,color = continent,group = country))+
  geom_point(data = gapminder_noKuwait,aes(year,gdpPercap,color = continent,size = pop/100000))+
  geom_line()+
  geom_point(aes(size = pop/100000))+
  facet_wrap(~ continent, nrow =1)+
  labs(x="Year",
       y= "GDP Per Capita",
       col = "Continent",
       size= "Population (100K)")+
  theme_bw()

print(fig2)

# Save the figs
ggsave("case_study_3_fig_1.png",plot = fig1,width = 15, unit = "in",
       path = "E:/UB/Spatial Data Science/geo511-2020-tasks-Zixuan0127/week_03")
ggsave("case_study_3_fig_2.png",plot = fig2,width = 15, unit = "in",
       path = "E:/UB/Spatial Data Science/geo511-2020-tasks-Zixuan0127/week_03")
