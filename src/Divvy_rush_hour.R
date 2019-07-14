library(pacman)
library(tidyverse)
library(plotly)
library(lubridate)
library(magrittr)

p_load(dplyr, ggvis, here, conflicted, data.table, jsonlite, geosphere,
       geojsonio, sp, eeptools, ggridges, viridis, ggthemes, ggExtra)

conflict_prefer("filter", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("hour", "lubridate")
conflict_prefer("wday", "lubridate")

#here::here()








#stations <- fromJSON("https://feeds.divvybikes.com/stations/stations.json")$stationBeanList

#Divvy_Stations <- fread("/Users/zacklarsen/Zack_Master/Datasets/Chicago/DivvyStationsMap.csv")





station_information_url <- "https://gbfs.divvybikes.com/gbfs/en/station_information.json"
#system_information_url <-  "https://gbfs.divvybikes.com/gbfs/en/system_information.json"
#system_regions_url <- "https://gbfs.divvybikes.com/gbfs/en/system_regions.json"
#system_alerts_url <- "https://gbfs.divvybikes.com/gbfs/en/system_alerts.json"
station_status_url <- "https://gbfs.divvybikes.com/gbfs/en/station_status.json"

stations <- fromJSON(station_information_url)$data$stations
status <- fromJSON(station_status_url)$data$stations

fromJSON(station_information_url)$data$stations %>% 
  View("Stations")

fromJSON(station_status_url)$data$stations %>% 
  View("Station Status")








divvy_trips <- fread(here("data",'Divvy_trips_sample.csv'))

divvy_trips %>% 
  View()




Divvy_q4_trips <- fread("/Users/zacklarsen/Zack_Master/Datasets/Chicago/Divvy_Trips_2018_Q4.csv")
colnames(Divvy_q4_trips)


Divvy_q4_trips %<>% 
  mutate(
    start_hour = hour(start_time),
    start_day = day(start_time),
    start_weekday = wday(start_time, label = TRUE, abbr = FALSE)
  ) %>% 
  mutate(
    birthyear_char = as.character(birthyear),
    birthyear_date_col = as.POSIXct(birthyear_char, format = '%Y'),
    age = calc_age(birthyear_date_col)
    ) %>% 
  select(-birthyear_char, -birthyear_date_col)



Divvy_q4_trips %>% 
  head()













# Rush Hour ---------------------------------------------------------------

# Inspired by: https://stephenslab.github.io/wflow-divvy/time-of-day-trends.html

ggplot(Divvy_q4_trips,
       aes(start_hour)) +
  geom_bar(fill = "dodgerblue",width = 0.75) +
  facet_wrap(~start_weekday, ncol = 2) +
  scale_x_discrete(breaks = seq(0,24,2)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank()
    ) + 
  labs(
    title = "Divvy rush hour trends",
    caption = "Source: https://stephenslab.github.io/wflow-divvy/time-of-day-trends.html"
    )


ggplot(subset(Divvy_q4_trips, from_station_name == "University Ave & 57th St"),
       aes(start_hour)) +
  geom_bar(fill = "dodgerblue",width = 0.75) +
  facet_wrap(~start_weekday, ncol = 2) +
  scale_x_discrete(breaks = seq(0,24,2)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Age and gender ----------------------------------------------------------

distances <- fread(here("data","distances.csv"))

# Add in distance
Divvy_q4_trips %<>% 
  inner_join(distances, by = c("from_station_id", "to_station_id"))


# Ridgeline plot of distance by age decile:

Divvy_q4_trips %>% 
  na.omit() %>% 
  mutate(age_decile = cut(age, seq(0, 100, by = 10))) %>% 
  head(5) %>% 
  select(age, age_decile)





ggplot(Divvy_q4_trips %>% 
         na.omit() %>% 
         mutate(age_decile = cut(age, seq(0, 100, by = 10))), 
       aes(x = distance_miles, y = age_decile, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Distance (Miles)", option = "C") +
  labs(title = 'Distance of Divvy bike trips in Q4 2018') +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) 





ggplot(Divvy_q4_trips %>% 
         na.omit() %>% 
         mutate(age_decile = cut(age, seq(0, 100, by = 10))), 
       aes(x = tripduration, y = age_decile, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Duration", option = "C") +
  labs(title = 'Duration of Divvy bike trips in Q4 2018') +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) 


















# Station and status ------------------------------------------------------

station_information_url <- "https://gbfs.divvybikes.com/gbfs/en/station_information.json"
station_status_url <- "https://gbfs.divvybikes.com/gbfs/en/station_status.json"

# Reading in from live data json feed url:
stations <- fromJSON(station_information_url)$data$stations
status <- fromJSON(station_status_url)$data$stations




# Reading in from json text file
#station <- fromJSON(here("data","station_information.json"))
#status <- read_json(here("data","station_status.json"))








