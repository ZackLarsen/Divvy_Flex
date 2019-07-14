##################################################
## Project: Divvy Flex Dashboard
## Script purpose: Pull in data from Divvy
## Date: July 14, 2019
## Author: Zack Larsen
##################################################

library(pacman)
library(tidyverse)
library(plotly)
library(lubridate)
library(magrittr)

p_load(dplyr, here, conflicted, data.table, jsonlite, geosphere,
       geojsonio, sp)

conflict_prefer("filter", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("hour", "lubridate")
conflict_prefer("wday", "lubridate")
conflict_prefer("here", "here")
#here::here()





# Divvy Site --------------------------------------------------------------

# Below are the former urls - they no longer work:

#stations <- fromJSON("https://feeds.divvybikes.com/stations/stations.json")$stationBeanList

#Divvy_Stations <- fread("/Users/zacklarsen/Zack_Master/Datasets/Chicago/DivvyStationsMap.csv")




# These are the urls from the UPDATED Divvy data site as of July 14, 2019:

station_information_url <- "https://gbfs.divvybikes.com/gbfs/en/station_information.json"
#system_information_url <-  "https://gbfs.divvybikes.com/gbfs/en/system_information.json"
#system_regions_url <- "https://gbfs.divvybikes.com/gbfs/en/system_regions.json"
#system_alerts_url <- "https://gbfs.divvybikes.com/gbfs/en/system_alerts.json"
station_status_url <- "https://gbfs.divvybikes.com/gbfs/en/station_status.json"


# Save JSON data from the url to a JSON text file in the data folder:
write_lines(fromJSON(station_information_url), here("data","station_information.json"))
write_lines(fromJSON(station_status_url), here("data","station_status.json"))


stations <- fromJSON(station_information_url)$data$stations
status <- fromJSON(station_status_url)$data$stations






# Divvy_q4_trips ----------------------------------------------------------

Divvy_q4_trips <- fread("/Users/zacklarsen/Zack_Master/Datasets/Chicago/Divvy_Trips_2018_Q4.csv")
colnames(Divvy_q4_trips)


Divvy_q4_trips %>% 
  mutate(
    start_hour = hour(start_time),
    start_day = day(start_time),
    start_weekday = wday(start_time, label = TRUE, abbr = FALSE)
  ) %>% 
  select(trip_id, start_time, start_hour, start_day, start_weekday) %>% 
  head()


Divvy_q4_trips %<>% 
  select() %>% 
  mutate(
  start_hour = hour(start_time),
  start_day = wday(start_time, label = TRUE, abbr = FALSE)
) 


Divvy_q4_trips %>% 
  select(start_time, start_hour, start_day) %>% 
  tail(50)



# Distance ----------------------------------------------------------------

# Distance calculations - we need only perform once for each possible 
# connection rather than for each trip:
stations %<>% 
  select(station_id, name, capacity, lat, lon) %>% 
  arrange(as.numeric(station_id))

nrow(stations) # 608

608 ** 2 # This is how many rows will be in the data frame that computes
# distances between all possible stations

# First, do crossing between all stations (from tidyr):
cartesian <- crossing(stations %>% select(station_id, lat, lon), 
                      stations %>% select(station_id, lat, lon)) %>% 
  rename(
    "from_station_id" = station_id,
    "from_lat" = lat,
    "from_lon" = lon,
    "to_station_id" = station_id1,
    "to_lat" = lat1,
    "to_lon" = lon1
  )

cartesian %>% 
  head()

distances <- cartesian %>% 
  mutate(
    distance = by(cartesian, 1:nrow(cartesian), function(row) { 
      distGeo(c(row$from_lat, row$from_lon), c(row$to_lat, row$to_lon), a=6378137, f=1/298.257223563) 
    }
    )
  ) %>% 
  arrange(-distance) %>% 
  mutate(distance_miles = distance / 1609.344) %>% 
  select(from_station_id, to_station_id, distance_miles)

distances %>% 
  head()

# Garbage collect
rm(cartesian)

fwrite(distances, here("data","distances.csv"))

