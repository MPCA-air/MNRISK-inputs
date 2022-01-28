library(tidyverse)
library(sf)

kristies_checks <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/3. Gas stations/service_stations_to_check.csv", col_types = cols(.default = "c"))  %>%
  filter(who_checks %in% c("Kristie", "Dorian")|is.na(who_checks))

monikas_checks <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/3. Gas stations/service_stations_to_check_vml.csv", col_types = cols(.default = "c")) %>%
  filter(who_checks == "Monika")

gas_stations <- bind_rows(monikas_checks, kristies_checks)

gas_stations <- gas_stations %>%
  filter(!lat %in% c("not there", "permanently closed", "not finding this one") | is.na(lat)) %>%
  filter(!grepl("o gas station here", latitude)) %>%
  mutate(latitude = gsub(",", "", latitude)) %>%
  separate(lat, into = c("lat", "long_1"), sep = ",")

gas_stations <- gas_stations %>%
  rowwise() %>%
  mutate(long = coalesce(long_1, long)) %>%
  mutate(lat = coalesce(lat, latitude),
         long = coalesce(long, longitude)) %>%
  select(source_name, ai_id, address, city, status, lat, long)

gas_stations <- gas_stations %>%
  group_by(address, city) %>% slice(1)

checking_1 <- gas_stations %>%
  group_by(source_name, address) %>% summarise(count = n())

checking_2 <- gas_stations %>%
  group_by(address, city) %>% mutate(count = n()) %>% ungroup() %>% filter(count>1)

gas_stations <- st_as_sf(gas_stations, coords = c("long", "lat"), crs = 4326, remove = FALSE)

gas_stations <- st_transform(gas_stations, crs = 26915)

gas_station_coords <- st_coordinates(gas_stations) %>% as.data.frame() %>% setNames(c("utm_x", "utm_y"))

gas_stations <- bind_cols(gas_stations, gas_station_coords)

census_blkgrps <- st_read("R:/demographics/census_2010_blockgroups_usboc.shp")

counties <- st_read("R:/administrative_boundaries/counties_mndot.shp") %>% as.data.frame() %>% select(COUNTYFIPS, COUNTYNAME)

blkgrps_counties <- left_join(census_blkgrps, counties, by = c("COUNTYFP10" = "COUNTYFIPS"))

gas_stations_blkgrps <- st_join(gas_stations, blkgrps_counties)

gas_stations_blockgroups <- gas_stations_blkgrps %>%
  group_by(GEOID10, COUNTYFP10) %>%
  summarise(count_blkgrp = n()) %>%
  ungroup() %>%
  select(-geometry) %>%
  as.data.frame()

gas_stations_counties <- gas_stations_blkgrps %>%
  group_by(COUNTYFP10) %>%
  summarise(count_county = n()) %>%
  ungroup() %>%
  select(-geometry) %>%
  as.data.frame()

gas_stations_frx <- left_join(gas_stations_blockgroups %>% as.data.frame(), gas_stations_counties %>% as.data.frame(), by = c("COUNTYFP10")) %>%
  rowwise() %>%
  mutate(frx_gs = count_blkgrp/count_county) %>%
  select(GEOID10, COUNTYFP10, frx_gs)

checking <- gas_stations_frx %>%
  group_by(COUNTYFP10) %>%
  summarise(county_sum = sum(frx_gs))


gas_stations <- left_join(gas_stations_blkgrps %>% as.data.frame(), gas_stations_frx %>% as.data.frame()) %>% select(source_name, ai_id, address, city, lat, long, utm_x, utm_y, COUNTYFP10, GEOID10, COUNTYNAME, frx_gs) %>% rename(county_fips = COUNTYFP10)


gas_stations <- gas_stations %>%
  mutate(building_ht_meters = 3.9,
         source_width_meters = 30,
         release_ht_meters = 3/2,
         Sz_meters = (building_ht_meters/2.15) %>% round(1),
         Sy_meters = (source_width_meters/4.3) %>% round(1),
         source_id = paste0("gs_", row.names(gas_stations)),
         county_fips = paste0("27", county_fips))

write_csv(gas_stations, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/3. Gas stations/service_stations_final.csv")
