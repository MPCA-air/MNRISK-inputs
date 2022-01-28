library(tidyverse)
library(readxl)
library(sf)

tempo_query <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/3. Gas stations/service_station_tempo_query.csv")

##Number unique names
tempo_query_names <- unique(tempo_query$MASTER_AI_NAME)


tempo_query <- tempo_query %>%
  rowwise() %>%
  mutate(location_check = ifelse(grepl("Centroid", METHOD_DESC), "check", "")) %>%
  select(MASTER_AI_NAME, MASTER_AI_ID, ADDRESS1, CITY_NAME, COUNTY_NAME, STATUS, LATITUDE, LONGITUDE, location_check, METHOD_DESC) %>%
  unique() %>%
  rename(source_name = MASTER_AI_NAME,
         ai_id = MASTER_AI_ID,
         address = ADDRESS1,
         city = CITY_NAME,
         county = COUNTY_NAME,
         status = STATUS,
         latitude = LATITUDE,
         longitude = LONGITUDE,
         location_method = METHOD_DESC) %>%
  mutate(responsible_party = "tempo")

##eliminate tempo locations that are county centroids
county_centroids <- st_read("R:/administrative_boundaries/counties_mndot_centroid.shp")

county_centroids <- st_buffer(county_centroids, dist = 100)

##convert tempo data to an sf object
tempo_query <- st_as_sf(tempo_query, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE, na.fail = FALSE)

tempo_query <- st_transform(tempo_query, crs = 26915)

tempo_query <- st_join(tempo_query, county_centroids)

tempo_query <- tempo_query %>%
  rowwise() %>%
  mutate(location_check = ifelse(is.na(COUNTYNAME), location_check, "check")) %>%
  select(-c(AREA, PERIMETER, COUNTY_, COUNTY_ID, COUNTY_NUM, COUNTYNAME, COUNTYFIPS, FIPS, ORIG_FID, geometry))

tanks_spreadsheet <- read_xlsx("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/3. Gas stations/Minnesota gas stations with UST's_012021.xlsx", sheet = "MN gas station UST")
nrow(tanks_spreadsheet) ##2787

tanks_spreadsheet <- tanks_spreadsheet %>%
  filter(Tank_Status != "Out of Service") %>%
  mutate(location_check = ifelse(grepl("Centroid", Coord_Collect_Meth_Desc), "check", ""))

nrow(tanks_spreadsheet) ##2743

tanks_spreadsheet <- tanks_spreadsheet %>%
  select(AI_Name, AI_ID, Address1, City, County, Tank_Status, Longitude, Latitude, location_check, Coord_Collect_Meth_Desc) %>%
  unique() %>%
  rename_all(tolower) %>%
  rename(source_name = ai_name,
         address = address1,
         status = tank_status,
         location_method = coord_collect_meth_desc) %>%
  mutate(responsible_party = "tanks")

nrow(tanks_spreadsheet) ##2737

##Make an updated table of all of the service stations.

all_service_stations <- bind_rows(tanks_spreadsheet, tempo_query)

all_service_stations <- all_service_stations %>%
  group_by(ai_id) %>%
  mutate(count = n()) %>%
  ungroup()

all_service_stations <- st_as_sf(all_service_stations, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE, na.fail = FALSE)

all_service_stations <- st_transform(all_service_stations, crs = 26915)

all_service_stations_coords <- st_coordinates(all_service_stations) %>% as.data.frame() %>% setNames(c("utm_x", "utm_y"))

all_service_stations <- bind_cols(all_service_stations, all_service_stations_coords) %>% as.data.frame() %>% select(-geometry)

##see how many names/counties/cities from all service stations are in MNRISKS2014. We checked most of the gas stations in MNRISKS2014. So, we'd use those locations.

service_stations_2014 <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/3. Gas stations/service_stations_blkgrps_utm.csv") %>% select(source_name, address, lat, long, county, City)

##Which names/cities/counties are in the MNRISKS2014 data
all_service_stations <- left_join(all_service_stations, service_stations_2014, by = c("address" = "address", "source_name" = "source_name", "city" = "City", "county" = "county"))

all_service_stations <- all_service_stations %>%
  mutate(location_check = ifelse(!is.na(lat), "don't check", location_check))


##remove service stations that are temporarily closed

all_service_stations <- all_service_stations %>%
  group_by(ai_id) %>%
  filter(!any(status %in% "Temporarily Closed"))

##calculate distances between the tanks locations and the tempo query
all_service_stations <- all_service_stations %>%
  group_by(ai_id) %>%
  mutate(distance = ifelse(count == 2&!is.na(latitude), ((utm_x - lag(utm_x))^2 + (utm_y - lag(utm_y))^2)^0.5, 0)) %>% group_by(ai_id) %>% fill(distance, .direction = "updown") %>% ungroup()

all_service_stations <- all_service_stations %>%
  mutate(location_check = ifelse(distance > 500 & count == 2 & location_check != "don't check", "check", location_check))

all_service_stations <- all_service_stations %>%
  group_by(ai_id) %>%
  arrange(lat, desc(location_check), responsible_party) %>%
  slice(1)

howmany <- filter(all_service_stations, location_check != "don't check")

all_service_stations <- all_service_stations %>%
  mutate(location_check = ifelse(location_check == "don't check" | location_method %in% c("Address Matching House Number", "Public Land Survey-Two Quarter", "Digitized - Web Map Google / Yahoo / Microsoft", "Digitized - MPCA online map", "Digitized - MPCA internal map", "GPS - Unknown", "GPS - Recreational Receiver Uncorrected", "GPS - Other", "Digitized-DRG", "GPS - Recr Receiver WAAS enabled real time dif cor", "Digitized-DOQ"), "don't check", "check"))

all_service_stations_check <- all_service_stations %>%
  filter(location_check == "check") %>%
  group_by() %>%
  mutate(who_checks = ifelse(row_number() %in% 1:86, "Monika", ifelse(row_number() %in% 87:173, "Kristie", "Dorian"))) %>%
  rowwise() %>%
  mutate(location_link = paste0("https://maps.google.com/maps?q=", latitude, ", ", longitude, "&hl=es;z=14&amp;output=embed"))

all_service_stations_dontcheck <- all_service_stations %>%
  filter(location_check == "don't check")

all_service_stations <- bind_rows(all_service_stations_check, all_service_stations_dontcheck)

write_csv(all_service_stations, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/3. Gas stations/service_stations_to_check.csv")
