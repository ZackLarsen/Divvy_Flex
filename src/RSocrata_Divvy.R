
# Created on 3/3/2018 by Zack Larsen

# Using leaflet and RSocrata to visualize City of Chicago Data Portal data

library(pacman)
p_load(flexdashboard)

library(flexdashboard)





## Install the required package with:
## install.packages("RSocrata")
library(RSocrata)
library(ggvis)
library(leaflet)
library(dplyr)
library(magrittr)
library(jsonlite)

# https://opendata.socrata.com/profile/Zack/g43r-uxjx/app_tokens
app_token <- 'h1ubFU6RLd3Oqttcgl9FaloPH'

# LIVE JSON feed for all stations
# Divvy
JSON_FEED <- 'https://feeds.divvybikes.com/stations/stations.json'
# Citibike NYC
NYC_Citibike_JSON <- "http://citibikenyc.com/stations/json"


# Station map for Divvy
Stationsdf <- read.socrata(
  "https://data.cityofchicago.org/resource/aavc-b2wj.json",
  app_token = 'h1ubFU6RLd3Oqttcgl9FaloPH',
  email     = NULL,
  password  = NULL
)
# Let's use leaflet to plot the stations
# First, we have to get the coordinates as a dataframe:
DEDUP_FULLlocationDF <- select(Stationsdf, latitude, longitude)
# We need to convert the coordinates to numeric
typeof(DEDUP_FULLlocationDF$latitude) # character
DEDUP_FULLlocationDF  %<>% 
  mutate_if(is.character,as.numeric)
typeof(DEDUP_FULLlocationDF$latitude) # double
# This bunches the points together into clusters and then
# splits the clusters apart on zoom. Very cool!!
DEDUP_FULLlocationDF %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())





# Divvy live JSON feed
Divvy_JSON <- fromJSON(JSON_FEED)
Divvy_JSON_stations <- data.frame(Divvy_JSON$stationBeanList)
colnames(Divvy_JSON_stations)
# Let's visualize the number of available bikes per station on the map
Divvy_JSON_stations <- select(Divvy_JSON_stations, latitude, longitude, availableBikes)

Divvy_JSON_stations %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(radius = Divvy_JSON_stations$availableBikes, color = "red") %>%
  addMarkers(clusterOptions = markerClusterOptions())


# Plot the symbol as icon
Divvy_icon <- makeIcon(iconUrl = '/Users/zacklarsen/Desktop/Learning/Projects/R_Viz/Divvy-icon.png',
                      iconWidth = 20, iconHeight = 20,
                      iconAnchorX = 0, iconAnchorY = 0)
Divvy_JSON_stations %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(radius = Divvy_JSON_stations$availableBikes, color = "red") %>%
  addMarkers(clusterOptions = markerClusterOptions(), icon = Divvy_icon)

# Plot the bike as icon
Divvy_bike_icon <- makeIcon(iconUrl = '/Users/zacklarsen/Desktop/Learning/Projects/R_Viz/divvybike.png',
                       iconWidth = 50, iconHeight = 50,
                       iconAnchorX = 0, iconAnchorY = 0)
Divvy_JSON_stations %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(radius = Divvy_JSON_stations$availableBikes, color = "red") %>%
  addMarkers(clusterOptions = markerClusterOptions(), icon = Divvy_bike_icon)








##############################
# Citibike NYC live JSON feed
citibike <- fromJSON(NYC_Citibike_JSON)
citibike_stations <- data.frame(citibike$stationBeanList)
colnames(citibike_stations)

# Let's visualize the number of available bikes per station on the map
Citibike_JSON_stations <- select(citibike_stations, latitude, longitude, availableBikes)

Citibike_JSON_stations %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(radius = Citibike_JSON_stations$availableBikes, color = "red") %>%
  addMarkers(clusterOptions = markerClusterOptions())

# Plot the bike as icon
Citibike_icon <- makeIcon(iconUrl = '/Users/zacklarsen/Desktop/Learning/Projects/R_Viz/Citibike_icon.png',
                            iconWidth = 35, iconHeight = 35,
                            iconAnchorX = 0, iconAnchorY = 0)
Citibike_JSON_stations %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(radius = Citibike_JSON_stations$availableBikes, color = "red") %>%
  addMarkers(clusterOptions = markerClusterOptions(), icon = Citibike_icon)








##############################################################################
# Let's try using mapbox or google maps as the background image for plotting.

# First, using leaflet's providers()
Citibike_JSON_stations %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addCircleMarkers(radius = Citibike_JSON_stations$availableBikes, color = "red") %>%
  addMarkers(clusterOptions = markerClusterOptions(), icon = Citibike_icon)

Citibike_JSON_stations %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addCircleMarkers(radius = Citibike_JSON_stations$availableBikes, color = "red") %>%
  addMarkers(clusterOptions = markerClusterOptions(), icon = Citibike_icon)

Citibike_JSON_stations %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(radius = Citibike_JSON_stations$availableBikes, color = "red") %>%
  addMarkers(clusterOptions = markerClusterOptions(), icon = Citibike_icon)

Citibike_JSON_stations %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.Watercolor) %>%
  addCircleMarkers(radius = Citibike_JSON_stations$availableBikes, color = "red") %>%
  addMarkers(clusterOptions = markerClusterOptions(), icon = Citibike_icon)

# Without the clusters
Citibike_JSON_stations %>%
  leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addCircleMarkers(radius = Citibike_JSON_stations$availableBikes, color = "red") %>%
  addMarkers(icon = Citibike_icon)





#########
# Mapbox
mapbox_pk <- 'pk.eyJ1IjoiemxhcnNlbiIsImEiOiJjamNpMzM5ZzIzZmdzMnFwNWd2eXJ6YzY1In0.r8LhaHRRUpdGewvOWE_jrA'
tile_ID <- 'mapbox.mapbox-streets-v7'
mapbox_URL <- 'https://api.mapbox.com/styles/v1/zlarsen/cjci4ctzb8tts2slht06hftyy.html?fresh=true&title=true&access_token=pk.eyJ1IjoiemxhcnNlbiIsImEiOiJjamNpMzM5ZzIzZmdzMnFwNWd2eXJ6YzY1In0.r8LhaHRRUpdGewvOWE_jrA#14.3/39.295192/-76.609623/0'
mapbox_leaflet_URL <- 'https://api.mapbox.com/styles/v1/zlarsen/cjci4ctzb8tts2slht06hftyy/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiemxhcnNlbiIsImEiOiJjamNpMzM5ZzIzZmdzMnFwNWd2eXJ6YzY1In0.r8LhaHRRUpdGewvOWE_jrA'
map_attr <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> Basemap © <a href='http://www.tcu.edu'>Zack Larsen</a>"

Citibike_JSON_stations %>%
  leaflet() %>%
  addMarkers(icon = Citibike_icon) %>%
  addTiles(urlTemplate = mapbox_leaflet_URL, attribution = map_attr) %>%
  addCircleMarkers(radius = Citibike_JSON_stations$availableBikes, color = "red")

# With clusters
Citibike_JSON_stations %>%
  leaflet() %>%
  addTiles(urlTemplate = mapbox_leaflet_URL, attribution = map_attr) %>%
  addCircleMarkers(radius = Citibike_JSON_stations$availableBikes, color = "red") %>%
  addMarkers(clusterOptions = markerClusterOptions(), icon = Citibike_icon)




# Really cool viz of google location history data
# https://www.cultureofinsight.com/blog/2018/01/31/2018-01-31-map-your-google-location-data-with-r-shiny/














# List all Datasets available on City of Chicago Data Portal
allSitesDataFrame <- ls.socrata("https://data.cityofchicago.org/resource/")
nrow(allSitesDataFrame) # Number of datasets
allSitesDataFrame$title # Names of each dataset
allSitesDataFrame$description # Description of datasets
allSitesDataFrame$identifier
allSitesDataFrame$landingPage

# Careful - this will download ALL 13.8 MILLION rows
trips_df <- read.socrata(
  "https://data.cityofchicago.org/resource/fg6s-gzvg.json",
  app_token = 'h1ubFU6RLd3Oqttcgl9FaloPH',
  email = NULL, 
  password = NULL
)






















# Cumulative history of JSON_FEED updates, every 10 minutes since beginning of program!
# Careful - this will download ALL 104 MILLION rows!
stations_df <- read.socrata(
  "https://data.cityofchicago.org/resource/eq45-8inv.json",
  app_token = 'h1ubFU6RLd3Oqttcgl9FaloPH',
  email = NULL, 
  password = NULL
)

"address": "2112 W Peterson Ave"





