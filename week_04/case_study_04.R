library(tidyverse)
library(nycflights13)
# Create keys same as flights
airports_dest = airports %>%
  mutate(dest = faa,
         dest_name = name)%>%
  select(dest_name,dest)
airports_ori = airports %>%
  mutate(origin = faa,
         ori_name = name) %>%
  select(ori_name,origin)
# Joint table
all = flights %>%
  left_join(airlines,by = "carrier")
# Filter the airports at NYC
all_nyc = all %>%
  filter(origin == "JFK"| origin == "LGA" | origin == "EWR")%>%
  arrange(desc(distance))
# replace the Abb. by full name
all_nyc = all_nyc %>%
  left_join(airports_dest, by = "dest")%>%
  mutate(dest = dest_name,.keep = 'unused')%>%
  left_join(airports_ori, by ="origin")%>%
  mutate(origin = ori_name, .keep = 'unused')
# full name for airline. 
all_nyc = all_nyc%>%
  mutate(carrier = name,.keep = 'unused')
# find the farthest airport 
# JFK
jfk = all_nyc %>%
  filter(origin == "John F Kennedy Intl")
jfk = jfk%>%
  distinct(distance, .keep_all = TRUE) %>%
  arrange(desc(distance))%>%
  select(origin,dest,distance)
f_jfk = head(jfk,n = 1)
# LGA
lga = all_nyc %>%
  filter(origin == "La Guardia")
lga = lga%>%
  distinct(distance, .keep_all = TRUE) %>%
  arrange(desc(distance))%>%
  select(origin,dest,distance)
f_lga = head(lga,n = 1)
# EWR
ewr = all_nyc %>%
  filter(origin == "Newark Liberty Intl")
ewr = ewr%>%
  distinct(distance, .keep_all = TRUE) %>%
  arrange(desc(distance))%>%
  select(origin,dest,distance)
f_ewr = head(ewr,n = 1)
# Combine the farthest distance
nyc_far = rbind(f_ewr,f_jfk,f_lga)
nyc_far = nyc_far %>%
  arrange(desc(distance))
# Print the results
print(paste("The farest airport from NYC is ",head(nyc_far$dest,n=1),
            ", and the distance is ",head(nyc_far$distance, n=1 ),
            "KM (I guess,there was no unit in tables.)."))