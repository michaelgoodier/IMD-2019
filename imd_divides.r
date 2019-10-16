#Deprivation Divides - the places where some of Britain's most deprived neighbourhoods border some of the least deprived.
library(tidyverse)
library(rgdal)
library(spdplyr)
library(leaflet)
library(RColorBrewer)

#download IMD shapefile here: http://data-communities.opendata.arcgis.com/datasets/indices-of-multiple-deprivation-imd-2019-1
imd <- readOGR("IMD.geojson")

#filter to most and least deprived areas only
top_and_bottom <- imd %>%
  filter(IMD_Decile == 1| IMD_Decile == 10) %>%
  mutate(opposite_decile = case_when(IMD_Decile == 10 ~ 1,IMD_Decile == 1 ~ 10))

#---- neighbour analysis ----
#calculate neighbours by polygons which touch each other
neighbours_list <- rgeos::gTouches(top_and_bottom, returnDense = FALSE)

#add in deciles of neighbours as a list of lists to our most / least deprived shapefile
top_and_bottom$neighbour_deciles <- lapply(neighbours_list, function(x) top_and_bottom$IMD_Decile[x])
                                           
#covert to sf so we can do a rowwise filter to lsoas with the "opposite decile" in their neighbours list
top_and_bottom_neighbours_sf <- st_as_sf(top_and_bottom) %>%
  rowwise() %>%
  filter(opposite_decile %in% neighbour_deciles) %>%
  ungroup()

#filter original shapefile by the results of the above filter so we can map it
final_areas <- top_and_bottom %>%
  filter(lsoa11nm %in% top_and_bottom_neighbours_sf$lsoa11nm)
                     
#---- map in rleaflet ----
#set color bins
bins <- 2
pal <- colorBin("YlOrRd", domain = final_areas$IMD_Decile, bins = 2)

#set labels
labels <- sprintf(
  "<strong>%s</strong><br/>IMD Decile: %g",
  final_areas$lsoa11nm, final_areas$IMD_Decile
) %>% lapply(htmltools::HTML)

legend_labels <- c("1 (Most Deprived)","10 (Least Deprived)")
                                           
#generate map
map <- leaflet(final_areas) %>%
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
                                          direction = "auto")) %>%
  addLegend(pal = pal, 
            values = ~IMD_Decile, 
            opacity = 1,
            title = "Deprivation Decile",
            labFormat = function(type, cuts, p) {paste0(legend_labels)})
                                           
#export as csv to aid with writing
write_csv(select(top_and_bottom_neighbours_sf,-neighbour_deciles,-neighbours,-geometry),"imd_divides.csv")
