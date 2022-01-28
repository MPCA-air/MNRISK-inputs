library(tidyverse)
library(sf)
library(leaflet)

inv_year <- 2017


# Load updated stack coordinates
stacks <- readRDS(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/04_Cleaned_stack_params_with_mod_coords.Rdata")) %>%
          mutate(stack_id = paste(source_id, release_point_id))


# Load original coordinates with modeled coordinates
orig <- read.csv(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/04_Cleaned_stack_params_with_qcd_coords.csv"), stringsAsFactors = F) %>%
        mutate(original = TRUE) %>%
        filter(!is.na(lat), !is.na(lat_mod)) %>%
        mutate(stack_id = paste(source_id, release_point_id),
               lat2  = lat,
               long2 = long)


# Create spatial object
## Split into 2 for distance comparison
orig2 <- orig %>%
         bind_rows(orig %>% mutate(lat2 = lat_mod, long2 = long_mod)) %>%
         st_as_sf(coords = c("long2", "lat2"), crs = 4326)  # crs=26915 for UTM


# Calculate distance between old and new coords
orig_dist <- tibble()

for (i in unique(orig2$stack_id)) {

    print(i)

    tmp <- filter(orig2, stack_id == i) %>% st_distance() %>% .[1,2] %>% as.numeric() %>% round()

    orig_dist <- bind_rows(tibble(source_id        = strsplit(i, " ")[[1]][1],
                                  release_point_id = strsplit(i, " ")[[1]][2],
                                  stack_id         = i,
                                  distance_delta   = tmp),
                           orig_dist)
}


# Filter to distances > 400 meters
big_movers <- filter(orig_dist, distance_delta > 400)

big_movers <- filter(orig, stack_id %in% big_movers$stack_id) %>%
              left_join(orig_dist %>% group_by(stack_id) %>% slice(1))

# Colors
#color_palette <- colorFactor(topo.colors(9), big_movers$source_id)

col_pal <- colorQuantile(
  RColorBrewer::brewer.pal(9, "Blues"),
  big_movers$distance_delta,
  n = 7,
  na.color = "#808080"
)

# Leaflet map
if(nrow(big_movers) > 0) {

leaflet(big_movers) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(data  = big_movers,
                   lng   = ~long,
                   lat   = ~lat,
                   color = ~col_pal(distance_delta),
                   label = ~paste(source_name, release_point_id, distance_delta, sep = ":   ")) %>%
  addMarkers(data  = filter(big_movers, source_id != 5),
             lng   = ~long_mod,
             lat   = ~lat_mod,
             label = ~paste(source_name, release_point_id, distance_delta, sep = ":   "))

}

# Ignore QC'd sources
qcd_mod_coords <- c("2708500049", "2712300694", "2703700011", "2703700192",
                    "2712900014", "2706100001", "2712300053", "2714100004",
                    "2706100004", "2713700113", "2710900084", "2710900006",
                    "2707900045")

big_movers <- filter(big_movers, !source_id %in% qcd_mod_coords)



#-----------------------------------------------#
# Check for rogue stacks far from a facility
#-----------------------------------------------#
stacks2 <- stacks %>%
           mutate(lat2 = lat, long2 = long) %>%
           st_as_sf(coords = c("long2", "lat2"), crs = 4326)  # crs=26915 for UTM

stack_dist <- tibble()

for (i in unique(stacks2$source_id)) {

  print(i)

  tmp <- filter(stacks2, source_id == i)

  if (nrow(tmp) > 1) {

    distances <- tmp %>% st_distance() %>% as.numeric() %>% round()

    stack_dist <- bind_rows(tibble(source_id        = i,
                                   rel_point_id     = tmp$release_point_id[[1]],
                                   distance_delta   = max(distances, na.rm = T)),
                            stack_dist)
  }
}


big_movers <- filter(stack_dist, distance_delta > 400)

big_movers <- filter(stacks, source_id %in% big_movers$source_id) %>%
              group_by(source_id) %>%
              mutate(lat_med  = median(lat),
                     long_med = median(long),
                     source_name = coalesce(source_name)) %>%
              left_join(stack_dist)


col_pal <- colorQuantile(
  RColorBrewer::brewer.pal(9, "Blues")[-(1:3)],
  big_movers$distance_delta,
  n = 5,
  na.color = "#808080"
)

# Leaflet map
if(nrow(big_movers) > 0) {

leaflet(big_movers) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(data  = big_movers,
                   lng   = ~long,
                   lat   = ~lat,
                   #color = ~col_pal(distance_delta),
                   label = ~paste(source_id, release_point_id, distance_delta, sep = ":   ")) %>%
  addMarkers(data  = filter(big_movers, source_id != 5),
             lng   = ~long_med,
             lat   = ~lat_med,
             label = ~paste(source_name, source_id, distance_delta, sep = ":   "))

}

# Ignore QC'd sources
qcd_mod_coords <- c("2706100004", "2713700345", "2713700113", "2710900006",
                    "2710900084", "2716100035", "2708300038", "2715100026",
                    "2712900014", "2714500008", "2714100004", "2703700115",
                    "2703700011", "2712300053", "2716300002", "2712300694")

big_movers <- filter(big_movers, !source_id %in% qcd_mod_coords)



#-------------------------------------#
# Check w/ 2014 facility locations
#-------------------------------------#
mn_2014 <- read.csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/_development/5. Sent to Lakes/Source parameters/facilities.csv", stringsAsFactors = F)

names(mn_2014)


# Map all stacks
leaflet(stacks) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(data  = filter(stacks, source_id != "5"),
                   lng   = ~long,
                   lat   = ~lat,
                   label = ~paste(source_name, release_point_id, sep = ":   ")) %>%
   addCircleMarkers(data  = filter(mn_2014, source_id != "5"),
                    lng   = ~long,
                    lat   = ~lat,
                    label = ~paste(source_name, rel_point_id, sep = ":   "),
                    color = "yellow",
                    stroke = F,
                    opacity = 0.5)


#---------------------------------#
# Update UTM coords
#---------------------------------#
stacks  <- stacks %>%
           mutate(lat2 = lat, long2 = long) %>%
           st_as_sf(coords = c("long2", "lat2"), crs = 4326) #"+proj=longlat +datum=WGS84 +no_defs"

stacks  <- stacks %>% st_transform(26915)  #crs=26915 for UTM

coords  <- st_coordinates(stacks$geometry)

stacks$x_utm <- coords[ , 1]
stacks$y_utm <- coords[ , 2]

stacks <- stacks %>% st_set_geometry(NULL)


#---------------------------------#
# SAVE QC'd stack coordinates
#---------------------------------#
saveRDS(stacks,
        paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/05_Cleaned_stack_params_with_final_coords.Rdata"))

gc()
