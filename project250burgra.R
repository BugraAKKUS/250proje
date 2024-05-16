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

map <- leaflet() %>%
  addTiles() %>%
  setView(lng = mean(merged_data$lng), lat = mean(merged_data$lat), zoom = 3)
map <- map %>%
  addCircleMarkers(data = merged_data, lng = ~lng, lat = ~lat, color = ~pop.change, radius = 5)
  
map

### demokrat mi republican mi?


color_palette <- c(democrat = "red", republican = "blue")

merged_data$marker_color <- ifelse(merged_data$democrat > merged_data$republican, color_palette["democrat"], color_palette["republican"])

map <- leaflet() %>%
  addTiles() %>%
  setView(lng = mean(merged_data$lng), lat = mean(merged_data$lat), zoom = 3)

map <- map %>%
  addCircleMarkers(data = merged_data, lng = ~lng, lat = ~lat, color = ~marker_color, radius = 5)

map


