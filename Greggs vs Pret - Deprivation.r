#greggs vs pret: deprivation 
#Michael Goodier @michaelgoodier

#packages----
library(tidyverse)
library(rvest)
library(rgdal)
library(spdplyr)
library(ggplot2)

#functions----
extract_postcode <- function(addresses) {
  # 1. Convert addresses to upper case
  addresses = toupper(addresses)
  # 2. Regular expression for UK postcodes: (DOESN'T CATCH THEM ALL)
  pcd_regex = "\\b(?:([Gg][Ii][Rr] 0[Aa]{2})|((([A-Za-z][0-9]{1,2})|(([A-Za-z][A-Ha-hJ-Yj-y][0-9]{1,2})|(([A-Za-z][0-9][A-Za-z])|([A-Za-z][A-Ha-hJ-Yj-y][0-9]?[A-Za-z]))))\\s?[0-9][A-Za-z]{2}))\\b"
  # 3. Check if a postcode is present in each address or not (return TRUE if present, else FALSE)
  present <- grepl(pcd_regex, addresses)
  # 4. Extract postcodes matching the regular expression for a valid UK postcode
  postcodes <- regmatches(addresses, regexpr(pcd_regex, addresses))
  # 5. Return NA where an address does not contain a (valid format) UK postcode
  postcodes_out <- list()
  postcodes_out[present] <- postcodes
  postcodes_out[!present] <- NA
  # 6. Return the results in a vector (should be same length as input vector)
  return(do.call(c, postcodes_out))
}

scrape_prets <- function(postcodes){
  pbapply::pblapply(postcodes,function(postcode){
    cat("\r Scraping Prets in ",postcode)
    page <- try(read_html(paste0("https://www.pret.co.uk/en-gb/find-a-pret/",postcode)))
    if(!"try-error" %in% class(page)){
      name <- page %>% html_nodes('div.addr-box') %>% html_nodes('h3') %>% html_text
      contact <- page %>% 
        html_nodes('address') %>% 
        html_text(trim=T) %>% 
        str_replace_all("[\r\n  ]"," ") %>% 
        str_replace_all("  "," ") %>% 
        as_tibble %>%
        separate(value, into = c("address","telephone"),sep="Telephone: ") %>%
        mutate_at(c("address","telephone"),trimws)
      geolocation <- page %>% 
        html_nodes("div.map-canvas") %>%
        html_attr("data-position")%>% 
        as_tibble %>%
        separate(value, into = c("lat","lon"),sep=", ")
      pret <- bind_cols(contact,geolocation) %>% mutate(name = name)
      return(pret)}else{
        num <- 0
        repeat{
          Sys.sleep(1)
          num <<- num + 1
          cat("\nwaiting and retrying",postcode," - ",num) 
          page <- try(read_html(paste0("https://www.pret.co.uk/en-gb/find-a-pret/",postcode)))
          if(!"try-error" %in% class(page)){
            name <- page %>% html_nodes('div.addr-box') %>% html_nodes('h3') %>% html_text
            contact <- page %>% 
              html_nodes('address') %>% 
              html_text(trim=T) %>% 
              str_replace_all("[\r\n  ]"," ") %>% 
              str_replace_all("  "," ") %>% 
              as_tibble %>%
              separate(value, into = c("address","telephone"),sep="Telephone: ") %>%
              mutate_at(c("address","telephone"),trimws)
            geolocation <- page %>% 
              html_nodes("div.map-canvas") %>%
              html_attr("data-position")%>% 
              as_tibble %>%
              separate(value, into = c("lat","lon"),sep=", ")
            pret <- bind_cols(contact,geolocation) %>% mutate(name = name)
            return(pret)}
          if(num == 10){pret <- data.frame(name=postcode)
          break}}
        return(pret)
      }
  }) %>% bind_rows %>% unique
}
#Import data ----

#read IMD shapefile (takes a while as a lot of data)
#Download IMD shapefile here: http://data-communities.opendata.arcgis.com/datasets/indices-of-multiple-deprivation-imd-2019-1
imd <- readOGR("../IMD 2019/IMD.geojson") %>% spTransform(CRS(wgs84))

#greggs
greggs <- jsonlite::fromJSON("https://api.greggs.co.uk/1.0/stores/55.378051/-3.43597299999999/1000000")

#run pret scraper
#get one of these to scrape every postal area http://geoportal1-ons.opendata.arcgis.com/datasets/823ece4a492a40298fb74ba4b5a14c46_0
postcodes <- read_csv("~/Lookups/Postcode_to_Output_Area_to_Lower_Layer_Super_Output_Area_to_Middle_Layer_Super_Output_Area_to_Local_Authority_District_August_2018_Lookup_in_the_UK.csv") %>%
  select(pcds) %>%
  separate(pcds, into = c("pcd",NA)) %>%
  unique %>%
  unlist

pret <- scrape_prets(postcodes)

#save our results
write_csv(pret,"UK Prets 17-10-2019.csv")
write_csv(greggs,"UK Greggs 17-10-2019.csv")

#analysis ----
#remove non uk prets, clean a bit, and merge with greggs into one table

uk_pret <- pret %>%
  mutate(type="Pret") %>%
  filter(str_detect(telephone,"^0")) %>%
  rename(phone = telephone, latitude = lat, longitude = lon, street = address) %>%
  mutate(postcode = extract_postcode(street), street = str_remove(street,postcode)) %>%
  mutate_at(c("latitude","longitude"),as.numeric)

uk_greggs <- greggs %>%
  mutate_at(c("latitude","longitude"),as.numeric) %>%
  select(-id,-distance,-town)

uk_pret_greggs <- bind_rows(uk_pret,uk_greggs)

#make into a spatial points data frame
wgs84 <- "+proj=longlat +datum=WGS84"
coords <- uk_pret_greggs %>% select(longitude,latitude)
locations_sp <- SpatialPointsDataFrame(coords,
                                       data = uk_pret_greggs,
                                       proj4string = CRS(wgs84)) 

#get lsoa data for each store and join to our preggs data
extracted_data <- over(locations_sp, imd)
final <- bind_cols(uk_pret_greggs,extracted_data)
write_csv(final,"UK Greggs and Pret with Deprivation 23-10-2019.csv")

#plot and see findings etc ----
england <- final %>% filter(!is.na(FID))

average_score <- england %>%
  group_by(type) %>%
  summarise(average_deprivation_score = mean(IMDScore))

decile_breakdown <- england %>%
  group_by(type) %>%
  mutate(total = n()) %>%
  group_by(type,IMDDec0,total) %>%
  summarise(number_in_decile = n()) %>%
  mutate(`Percentage of outlets` = (number_in_decile/total) *100)

ggplot(decile_breakdown, 
                       aes(x = IMDDec0, 
                           y = `Percentage of outlets`, 
                           fill = type)) +
  geom_bar(stat="identity", position="dodge") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  theme_minimal() +
  scale_fill_manual(values = c("#00558f", "#9f1b32")) +
  labs(title="Who eats where?",
       subtitle = "Greggs outlets are found in more deprived neighbourhoods than Prets",
       y = "Percentage of outlets",
       x = "IMD Decile (1 = more deprived)",
       fill = NULL)
