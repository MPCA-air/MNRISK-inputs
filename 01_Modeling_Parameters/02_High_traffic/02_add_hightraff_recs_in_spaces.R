##add in receptors along highways where there are missing spots.

library(tidyverse)
library(sf)
library(mapedit)
library(mapview)
library(leaflet)

original_traff_file <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/2. High volume traffic/Without_overlaps_high_traffic.csv")

traff_sf <- st_as_sf(original_traff_file, crs = 4326, coords = c("long", "lat"), remove = FALSE)

traff_sf <- st_transform(traff_sf, crs = 26915)

traff_sf_coords <- st_coordinates(traff_sf) %>% as.data.frame() %>% setNames(c("utm_x", "utm_y"))

traff_sf <- cbind(traff_sf, traff_sf_coords)

counties <- unique(traff_sf$COUNTYF)

for(i in counties){
  high_traff <- filter(traff_sf, COUNTYF == i)
  write_csv(high_traff, paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/2. High volume traffic/split_for_review/high_traff", i, ".csv"))
  }

##Start here
library(tidyverse)
library(sf)
library(mapedit)
library(mapview)
library(leaflet)

setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/2. High volume traffic/finished_for_review_2")

files_to_check <- list.files()

for(i in files_to_check){

   file_to_check <- read_csv(i)

   file_to_check <- file_to_check %>%
     separate(geometry, into = c("long", "lat"), sep = ",") %>%
     mutate(lat = gsub("\\)", "", lat),
            long = gsub("c\\(", "", long),
            lat = as.numeric(lat),
            long = as.numeric(long))

   file_to_check <- st_as_sf(file_to_check, crs = 4326, coords = c("long", "lat"), remove = FALSE)

   new_ht_pts <- editFeatures(file_to_check, map = leaflet() %>% addTiles() %>% addCircles(lng    = file_to_check$long, lat    = file_to_check$lat, radius = 50, stroke = F, fillOpacity = 0.7, color  = "darkorange") %>%
  addProviderTiles(providers$CartoDB.Positron))

write_csv(new_ht_pts, paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/2. High volume traffic/finished_for_review_2/_2", i))

rm(new_ht_pts)
gc()
}
