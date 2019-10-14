#Deprivation Divides - the places where some of Britain's most deprived neighbourhoods border some of the least deprived.
library(tidyverse)
library(rgdal)
library(spdplyr)
library(leaflet)
library(RColorBrewer)
library(spdep)

#Download IMD shapefile here: http://data-communities.opendata.arcgis.com/datasets/indices-of-multiple-deprivation-imd-2019-1
imd <- readOGR("IMD.geojson")

#filter to most and least deprived
top_and_bottom <- imd %>%
  filter(IMD_Decile == 1| IMD_Decile == 10) %>%
  mutate(opposite_decile = case_when(IMD_Decile == 10 ~ 1,IMD_Decile == 1 ~ 10))

#---- Neighbour analysis ----
# neighbors_nb <- poly2nb(top_and_bottom)
# 
# wts <- nb2listw(neighbors_nb, style="W",zero.policy=T)
# top_and_bottom$dec_weight <- lag(wts,top_and_bottom$IMD_Decile,zero.policy=T)

neighbors_list <- rgeos::gTouches(top_and_bottom, returnDense = FALSE)
list_lsoas <- lapply(neighbors_list, function(x) top_and_bottom$IMD_Decile)
top_and_bottom$neighbour_deciles <- list_lsoas

top_and_bottom_neighbors <- top_and_bottom %>%
  mutate()

top_and_bottom_neighbors_sf <- st_as_sf(top_and_bottom_neighbors) %>%
  rowwise() %>%
  filter(opposite_decile %in% neighbour_deciles)


#Map in rleaflet----
#set color bins
bins <- 2
pal <- colorBin("YlOrRd", domain = top_and_bottom$IMD_Decile, bins = 2)

#set labels
labels <- sprintf(
  "<strong>%s</strong><br/>IMD Decile: %g",
  top_and_bottom$lsoa11nm, top_and_bottom$IMD_Decile
) %>% lapply(htmltools::HTML)

#generate map
map <- leaflet(top_and_bottom) %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  setView(lat=53, lng=-2, zoom=7) %>%
  addPolygons(smoothFactor = 0.5, 
              fillOpacity = 0.8, 
              fillColor = ~pal(IMD_Decile),
              color = "",
              weight = 0.1,
              opacity = 0,
              dashArray = "3",
              highlight = highlightOptions(weight = 5,
                                           color = "#666",
                                           dashArray = "",
                                           fillOpacity = 0.7,
                                           bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                          textsize = "15px",
                                          direction = "auto"))





