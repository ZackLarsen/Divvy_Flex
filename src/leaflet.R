##################################################
## Project: Divvy_Flex
## Script purpose: Test out leaflet plots using Divvy data
## Date: May 15, 2019
## Author: Zack Larsen
##################################################



# https://allthisblog.wordpress.com/2016/10/12/r-311-with-leaflet-tutorial/


library(pacman)
p_load(leaflet, dplyr, ggvis, here, conflicted, data.table, jsonlite)

conflict_prefer("filter", "dplyr")

getwd()
setwd('/Users/zacklarsen/Zack_Master/Projects/Dataviz/R/Divvy_Flex')







# Data feed ---------------------------------------------------------------

stations <- fromJSON("https://feeds.divvybikes.com/stations/stations.json")
stations$executionTime
stations$stationBeanList %>% 
  filter(
    #id == 2,
    is_renting == TRUE,
    #availableDocks < 5,
    stAddress1 %like% 'Damen'
    )




my_map <- leaflet() %>%
  addTiles() %>%
  addMarkers(lat = stations$stationBeanList$latitude, 
             lng = stations$stationBeanList$longitude,
             popup = stations$stationBeanList$stationName)
my_map 








my_map <- leaflet() %>%
  addTiles() %>%
  addMarkers(lat = 41.80772701677172, 
             lng = -87.74322936566276,
             popup = 'Random red light')
my_map 




locationDF <- data.frame(violations[,c("LATITUDE","LONGITUDE")])
FULLlocationDF <- na.omit(locationDF)


DEDUP_FULLlocationDF <- group_by(FULLlocationDF, LATITUDE, LONGITUDE) %>% 
  slice(1)


DEDUP_FULLlocationDF %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers()


# Make an icon to plot instead of the default pointer
stoplight <- makeIcon(iconUrl = '/Users/zacklarsen/Desktop/traffic-symbol-icon-png-5885.png',
                      iconWidth = 20, iconHeight = 20,
                      iconAnchorX = 20, iconAnchorY = 20)
DEDUP_FULLlocationDF %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(icon = stoplight)





# This bunches the points together into clusters and then
# splits the clusters apart on zoom. Very cool!!
DEDUP_FULLlocationDF %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())



# Randomly generate a count of tickets per light, then map that to the radius of the circle
ticketCountDF <- DEDUP_FULLlocationDF
ticketCountDF$ticketCount <- runif(nrow(ticketCountDF),min = 5,max = 100)
ticketCountDF

ticketCountDF %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(radius = ticketCountDF$ticketCount/10)
  #addLegend(labels = ,colors = )
  # addCircleMarkers(color = ticketCountDF$ticketCount)







