library(pacman)
library(tidyverse)
library(plotly)
library(lubridate)

p_load(flexdashboard, leaflet, leaflet.extras, dplyr, ggvis, here,
       conflicted, data.table, jsonlite, kableExtra, glue, geosphere,
       DT, networkD3, htmltools, geojsonio, data.table, magrittr,
       circlize, chorddiag, sp, crayon)

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


Divvy_q4_trips %>% 
  mutate(
    start_hour = hour(start_time),
    start_day = day(start_time),
    start_weekday = wday(start_time, label = TRUE, abbr = FALSE)
  ) %>% 
  select(trip_id, start_time, start_hour, start_day, start_weekday) %>% 
  head()


Divvy_q4_trips %<>% 
  mutate(
    start_hour = hour(start_time),
    start_day = wday(start_time, label = TRUE, abbr = FALSE)
    ) 


Divvy_q4_trips %>% 
  select(start_time, start_hour, start_day) %>% 
  tail(50)








# Inspired by: https://stephenslab.github.io/wflow-divvy/time-of-day-trends.html

ggplot(Divvy_q4_trips,
       aes(start_hour)) +
  geom_bar(fill = "dodgerblue",width = 0.75) +
  facet_wrap(~start_day, ncol = 2) +
  scale_x_discrete(breaks = seq(0,24,2)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank()
        ) + 
  labs(
    title = "Divvy rush hour trends",
    caption = "Source: https://stephenslab.github.io/wflow-divvy/time-of-day-trends.html"
    )

ggplot(subset(Divvy_q4_trips, from_station_name == "University Ave & 57th St"),
       aes(start_hour)) +
  geom_bar(fill = "dodgerblue",width = 0.75) +
  facet_wrap(~start_day, ncol = 2) +
  scale_x_discrete(breaks = seq(0,24,2)) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
