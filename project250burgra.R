library(readr)
library(readxl)
libraries <- c("ggplot2","sf","rworldmap","tidyverse","magrittr",
               "leaflet", "dplyr", "rvest", "xml2","rvest",
               "maps","mapdata","RgoogleMaps","lubridate","rnaturalearth","dplyr","rnaturalearthdata","RColorBrewer","httr")
lapply(libraries, require, character.only = TRUE)
library(dplyr)
#
uscounties <- read_excel("uscounties.xlsx")
data_final <- read_csv("data_final.csv")
str(uscounties)
str(data_final)
uscounties$state <- uscounties$state_id
uscounties <- uscounties %>% rename(state = state_id)
uscounties <- subset(uscounties, select = -state_id)
merged_data <- merge(data_final, uscounties, by = c("county", "state"))
#mapping
str(merged_data)
mapping <- function(data=merged_data, colorx=merged_data$pop.change){
  map <- leaflet() %>%
    addTiles() %>%
    setView(lng = mean(data$lng), lat = mean(data$lat), zoom = 3)
  map <- map %>%
    addCircleMarkers(data = data, lng = ~lng, lat = ~lat, color = ~colorx, radius = 5)
  
  map
}
mapping()
