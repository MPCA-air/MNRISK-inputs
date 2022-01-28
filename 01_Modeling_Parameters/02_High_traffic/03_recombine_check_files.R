library(tidyverse)
library(sf)

setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/2. High volume traffic/finished_for_review_2")

checked_files <- list.files()

summary_table <- data.frame()

for(i in checked_files){

  data <- read_csv(i)

  temp <- tibble(lat_nas = TRUE, file = "none")

  temp$lat_nas <- TRUE %in% is.na(data$lat)
  temp$file <- i

  summary_table <- rbind(temp, summary_table)

}

files_to_check_again <- filter(summary_table, lat_nas == FALSE) %>%
  mutate(file = gsub("completecheck", "", file))

check_file_names <- unique(files_to_check_again$file)

library(mapedit)
library(mapview)
library(leaflet)

setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/2. High volume traffic/finished_for_review")

for(i in check_file_names){

  file_to_check <- read_csv(i)

  file_to_check <- file_to_check %>%
    separate(geometry, into = c("long", "lat"), sep = ",")

  file_to_check <- file_to_check %>%
    mutate(long = gsub("c\\(", "", long) %>% as.numeric(),
           lat = gsub("\\)", "", lat) %>% as.numeric())

  file_to_check <- st_as_sf(file_to_check, coords = c("long", "lat"), remove = FALSE, crs = 4326)

  new_ht_pts <- editFeatures(file_to_check, map = leaflet() %>% addTiles() %>% addCircles(lng    = file_to_check$long_new, lat    = file_to_check$lat_new, radius = 50, stroke = F, fillOpacity = 0.7, color  = "darkorange") %>%
        addProviderTiles(providers$CartoDB.Positron))

  write_csv(new_ht_pts, paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/2. High volume traffic/finished_for_review_2/_2", i))

}

##steps
##check that all files have some na columns, that means all were checked prior to saving - this is completed, see above.
##bind all files together
##convert to sf object
##remove leaflet columns
##join to traffic lines shapefile
##remove unneeded columns layer
##redo the source ids



setwd("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/2. High volume traffic/finished_for_review_2")

checked_files <- list.files()

all_hightraffic <- data.frame()

for(i in checked_files){

  data <- read_csv(i)

  data <- data %>%
    mutate(across(everything(), as.character),
           STATEFP = STATEFP[2],
           COUNTYF = COUNTYF[2]) %>%
    select(-X_leaflet_id, -feature_type, -radius)

  all_hightraffic <- bind_rows(data, all_hightraffic)

}

all_hightraffic <- all_hightraffic %>%
  separate(geometry, into = c("long", "lat"), sep = ",")

all_hightraffic <- all_hightraffic %>%
  mutate(long = gsub("c\\(", "", long),
         lat = gsub("\\)", "", lat))

all_hightraffic <- all_hightraffic %>%
  mutate(long = as.numeric(long),
         lat = as.numeric(lat))

all_hightraffic <- st_as_sf(all_hightraffic, crs = 4326, coords = c("long", "lat"), remove = FALSE)

all_hightraffic <- all_hightraffic %>%
  select(STATEFP, geometry, lat, long)

all_hightraffic <- st_transform(all_hightraffic, crs = 26915)

all_hightraffic_coords <- st_coordinates(all_hightraffic) %>% as.data.frame() %>% setNames(c("utm_x", "utm_y"))

all_hightraffic <- cbind(all_hightraffic, all_hightraffic_coords)

aadt_segments_shp <- read_sf("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/2. SCC Groups and Fractions/data_info/Annual_Average_Daily_Traffic_Segments_in_Minnesota.shp")

aadt_segments_shp <- st_zm(aadt_segments_shp)

all_hightraffic_join <- st_nearest_feature(all_hightraffic, aadt_segments_shp)

all_hightraffic_joined <- cbind(all_hightraffic, all_hightraffic_join) %>% rename(join_index = all_hightraffic_join)

aadt_names <- rownames(aadt_segments_shp)

aadt_segments_shp <- cbind(aadt_segments_shp, aadt_names) %>% mutate(aadt_names = as.integer(aadt_names))

all_hightraffic_nearest <- left_join(all_hightraffic_joined %>% as_tibble(), aadt_segments_shp %>% as_tibble(), by = c("join_index" = "aadt_names"))

##This checks
checking <- filter(all_hightraffic_nearest, is.na(SEQUENCE_N))

checking <- st_as_sf(all_hightraffic_nearest, sf_column_name = "geometry.y", crs = 26915)

plot(checking[2])

##Take out neighbors closer than 10 meters.
all_hightraffic <- all_hightraffic_nearest %>%
  select(STATEFP, lat, long, utm_x, utm_y, SEQUENCE_N, ROUTE_LABE, STREET_NAM, LOCATION_D, CURRENT_YE, CURRENT_VO, SHAPE_Leng) #%>%
  #unique() %>%
  #filter(!is.na(SEQUENCE_N))

all_hightraffic <- st_as_sf(all_hightraffic, coords = c("utm_x", "utm_y"), crs = 26915, remove = FALSE)

distance <- 40 #meters

neighbors <- st_is_within_distance(all_hightraffic, all_hightraffic, distance)

all_hightraffic$neighbs <- neighbors

##There are some nearest neighbors, within 20 meters.
checking <- all_hightraffic %>%
  group_by(neighbs) %>%
  mutate(count = n()) %>%
  filter(count > 1)

all_hightraffic <- all_hightraffic %>%
  group_by(neighbs, ROUTE_LABE, SEQUENCE_N, LOCATION_D) %>%
  slice(1) %>%
  ungroup() %>%
  group_by() %>%
  mutate(source_id = paste0("ht_", seq(1:n()))) %>%
  select(-neighbs, -geometry)

checking <- st_as_sf(all_hightraffic, coords = c("long", "lat"), crs = 4326)

plot(checking[2])

##There are some high traffic points in Iowa, we will remove them in later scripts.

write_csv(all_hightraffic, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/2. High volume traffic/high_traffic_pts_final.csv")
