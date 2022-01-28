#####get high traffic points
library(tidyverse)
library(sf)
library(mapedit)
library(mapview)
library(leaflet)
library(units)

##Make a high traffic points csv without overlapping points.
high_traffic_shp <- st_read("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/2. SCC Groups and Fractions/data_info/interim_tables/aadt_out_v3.shp", stringsAsFactors = FALSE)

##high_traffic_df <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/2. SCC Groups and Fractions/data_info/interim_tables/aadt_out_v3.csv")


high_traffic_shp <- st_as_sf(high_traffic_shp, sf_column_name = "geometry", crs = 26915)

high_traffic_pts <- sf::st_line_sample(high_traffic_shp,  density = units::set_units(10, 1/km), type = "regular")

high_traffic_pts <- high_traffic_pts[!st_is_empty(high_traffic_pts), ]

high_traffic_pts <- st_cast(high_traffic_pts, "POINT")

st_distance(high_traffic_pts[1:10, ])


high_traffic_pts <- st_join(high_traffic_shp, st_sf(high_traffic_pts))

high_traffic_pts <- st_cast(high_traffic_pts, "POINT")

quantile(high_traffic_pts$vkt_all, 0.95)

high_traffic_pts <- high_traffic_pts %>%
  select(STATEFP, COUNTYF, GEOID, urban, SEQUENC, ROUTE_L, rod_typ, number, vkt_all, len, geometry) %>%
  filter(grepl("I-", ROUTE_L)|COUNTYF %in% c("123", "053") & rod_typ %in% c("CSAH", "I", "TH")) %>%
  group_by(ROUTE_L) %>%
  mutate(vkt_mean = mean(vkt_all, na.rm = T)) %>%
  filter(vkt_mean > 40000)

##filtered by over 40,000 and in ramsey or hennepin.
unique(high_traffic_pts$ROUTE_L)
high_traffic_pts <- st_cast(high_traffic_pts, "POINT")

high_traffic_pts <- st_as_sf(high_traffic_pts, sf_geometry_column = geometry, crs = 26915, remove = FALSE)

high_traffic_pts_coords <- st_coordinates(high_traffic_pts) %>% as.data.frame() %>% setNames(c("utm_x", "utm_y"))

high_traffic_pts <- bind_cols(high_traffic_pts, high_traffic_pts_coords)

##high_traffic_pts <- high_traffic_pts %>%
  ##group_by(ROUTE_L) %>%
  ##arrange(utm_x) %>%
  ##mutate(order_x =  sqrt((utm_x - lag(utm_x, 1))^2 + (utm_y - lag(utm_y))^2),
    #     sequence_x = 1:n()) %>%
  ##group_by(ROUTE_L) %>%
  ##arrange(utm_y) %>%
  ##mutate(order_y = sqrt((utm_x - lag(utm_x, 1))^2 + (utm_y - lag(utm_y))^2),
    ##     sequnce_y = 1:n()) %>% ungroup()

##high_traffic_pts <- st_as_sf(high_traffic_pts, coords = c("utm_x", "utm_y"), crs = 26915, remove = FALSE)

##high_traffic_pts <- filter(high_traffic_pts, order_x > 0 | order_y > 0)

##plot(high_traffic_pts[1])

high_traffic_pts$num <- 1:nrow(high_traffic_pts)

high_traffic_pts <- high_traffic_pts %>%
  rowwise() %>%
  mutate(source_id = paste0("ht_", num),
         source_name = paste0(source_id, "_", ROUTE_L, "_", SEQUENC)) %>%
  select(-num)

high_traffic_pts$PTYPE <- "HIGHVOLTRAFFIC"

high_traffic_pts <- st_as_sf(high_traffic_pts, coords = c("utm_x", "utm_y"), crs = 26915, remove = FALSE)

high_traffic_pts <- st_transform(high_traffic_pts, crs = 4326)

high_traffic_pts_coords <- st_coordinates(high_traffic_pts) %>% as.data.frame() %>% setNames(c("long", "lat"))

high_traffic_pts <- cbind(high_traffic_pts, high_traffic_pts_coords)

high_traffic_pts <- high_traffic_pts %>%
  select(-geometry) %>%
  rename(seq_number = SEQUENC,
         route_name = ROUTE_L) %>%
  mutate(release_ht = 1.5,
         sz = 1.4,
         sy = 77.2,
         side_length = 166)

write_csv(high_traffic_pts, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/2. High volume traffic/Without_overlaps_high_traffic.csv")
