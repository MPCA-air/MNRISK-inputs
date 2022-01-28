## This script checks if all stacks at a facility are less than 400 meters apart.

library(tidyverse)
library(sf)
library(leaflet)
library(mapedit)

inv_year <- 2017

points_near <- readRDS(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/07_Cleaned_stack_params_with_final_coords_grouped.Rdata"))

# Use if checking final facility source parameter locations
# points_near <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/999. Final prep_source parameters/results/facilities_modeling_params_mnrisks_2017.csv")
points_near$x2 <- points_near$long
points_near$y2 <- points_near$lat

points_near  <- st_as_sf(points_near, coords = c("x2", "y2"), crs = 4326)

points_near <- st_transform(points_near, crs = 26915)

distance <- 400 #meters

sources <- unique(points_near$source_id)

points_near_fac_all <- data.frame()

for (i in sources) {

  print(i)

  points_near_fac <- filter(points_near, source_id == i)

  neighbors <- st_is_within_distance(points_near_fac, points_near_fac, distance)

  points_near_fac$neighbs <- neighbors

  points_near_fac$neighbs <- as.character(points_near_fac$neighbs)


  points_near_fac_all <- rbind(points_near_fac_all, points_near_fac)

  }


points_dupes <- points_near_fac_all %>%
                group_by(neighbs, source_name, source_id) %>%
                mutate(neighbs_n = 1:n(),
                       neighbs_count = n()) %>%
                ungroup() %>%
                group_by(source_name, source_id) %>%
                mutate(n = 1:n(),
                       count = n()) %>%
                filter(neighbs_n > 1 & n > 1 & n_distinct(neighbs) > 1) %>%
                ungroup()


far_stacks <- unique(points_dupes$source_name)



# Look at each facility's stacks that are outside of 400 meters apart.
## Done for 2017
if (FALSE) {

for(i in far_stacks) {

  points_far_stacks <- filter(points_near, source_name == i)

  points_far_stacks <- st_transform(points_far_stacks, crs = 4326)

  new_stack_locs  <-
                editFeatures(points_far_stacks, map = leaflet() %>%
                addProviderTiles(providers$Esri.WorldImagery) %>%
                setView(lng = mean(points_far_stacks$long),
                        lat = mean(points_far_stacks$lat),
                        zoom = 17) %>%
                addMarkers(data  = points_far_stacks,
                           lng   = ~long,
                           lat   = ~lat,
                           label = ~source_name))

  new_i <- gsub("\\/", "", i)
  #new_stack_locs$neighbs <- as.character(new_stack_locs$neighbs)

  write_csv(new_stack_locs, paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/qc_stack_params/final_stack_locs_07/", new_i, "_new_stack_loc.csv"))

}
}


## Moved the individual stacks that were way off property onto property near other stacks, excel Xcel-Sherburne County.
## Now will integrate the new locs into the final data set
new_stacks <- list.files("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/qc_stack_params/final_stack_locs_07", full.names = T)

#setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/qc_stack_params/final_stack_locs_07/")

new_fac_stacks_all <- data.frame()

for (i in new_stacks) {

  data <- read_csv(i)

  data <- data %>%
          rowwise() %>%
          separate(geometry, into = c("long_new", "lat_new"), sep = ",", remove = FALSE) %>%
          mutate(lat_new  = gsub("\\)", "", lat_new),
                 long_new = gsub("c\\(", "", long_new),
                 lat      = coalesce(lat_new, as.character(lat)),
                 long     = coalesce(long_new, as.character(long))) %>%
          select(-c(long_new:count))

  new_fac_stacks_all <- bind_rows(new_fac_stacks_all, data)

  }

new_fac_stacks_all <- new_fac_stacks_all %>%
                      select(source_name, source_id, release_point_id, lat, long) %>%
                      mutate(source_id = as.character(source_id),
                             lat = as.numeric(lat),
                             long = as.numeric(long))

# Load MN stack coordinates
inv_year <- 2017

# Load MN stack coordinates
stacks <- readRDS(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/07_Cleaned_stack_params_with_final_coords_grouped.Rdata"))

all_stacks <- left_join(stacks,
                        new_fac_stacks_all, by = c("source_name", "source_id", "release_point_id"))

all_stacks <- all_stacks %>%
              rowwise() %>%
              mutate(lat  = coalesce(lat.y, lat.x),
                    long = coalesce(long.y, long.x)) %>%
              select(-lat.x, -lat.y, -long.x, -long.y)


# SAVE ---------------------------------------------#
saveRDS(all_stacks, paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/_development/1. Modeling parameters/1. Point sources/07-5_Cleaned_stack_params_with_final_coords_grouped.Rdata"))

