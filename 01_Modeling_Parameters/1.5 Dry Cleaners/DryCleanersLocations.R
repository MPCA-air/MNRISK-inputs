library(RODBC)
library(tidyverse)
library(readxl)
library(sf)

# Set inventory year
year_inv <- 2017


odbcDataSources()

credentials <- read.csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Student Folder/credentials.csv", stringsAsFactors = FALSE)

d_cnx <- odbcConnect("deltaw", uid = credentials$delta_user, pwd = credentials$delta_pwd, believeNRows = FALSE)

odbcGetInfo(d_cnx)

odbcTables(d_cnx, schema = "RAPIDS")  #Show all tables in deltaw database

tables <- sqlTables(d_cnx, tableType = "TABLE", schema = "RAPIDS")  #Show all tables in deltaw database


# Get inventory year code
inv_codes  <- sqlQuery(d_cnx, "SELECT * FROM RAPIDS.INV_INVENTORIES", stringsAsFactors = F)

inv_codes  <- filter(inv_codes, grepl(year_inv, INVENTORY_YEAR))

if(nrow(inv_codes) > 1) print(paste("Multiple inventory codes found: ", paste(inv_codes$INVENTORY_ID, collapse = ", ")))

inv_codes  <- filter(inv_codes, nchar(INVENTORY_ID) == max(nchar(inv_codes$INVENTORY_ID)))$RID

# Get sources
src_names   <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.inv_sources WHERE INVENTORY_RID = ", inv_codes), stringsAsFactors = F)

src_coords  <- sqlQuery(d_cnx, "SELECT * FROM RAPIDS.INV_COORDINATES", stringsAsFactors = F)



# Get processes
src_process <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.inv_processes WHERE INVENTORY_RID = ", inv_codes), stringsAsFactors = F, as.is =T)

# Get operating emission units
src_units   <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.INV_EMISSION_UNITS WHERE INVENTORY_RID = ", inv_codes), stringsAsFactors = F)

## Remove units shutdown during inventory year
src_units <- filter(src_units, STATUS_CODE != "SHUTDOWN" | STATUS_YEAR != year_inv)

## Remove units built after inventory year, or removed before inventory year
src_units <- filter(src_units, is.na(BEGIN_OPERATION_DATE) | as.numeric(substr(BEGIN_OPERATION_DATE, 1, 4)) <= year_inv,  is.na(END_OPERATION_DATE) | as.numeric(substr(END_OPERATION_DATE, 1, 4)) >= year_inv)


# Get emissions for inventory year
emissions   <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.INV_PROCESS_EMISSIONS WHERE INVENTORY_RID = ", inv_codes), stringsAsFactors = F, as.is = T)

# Get modeling parameters for facilities
src_params  <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.INV_RELEASE_POINTS WHERE STATUS_YEAR = ", year_inv), stringsAsFactors = F)

## Remove units shutdown during inventory year
src_params  <- filter(src_params, STATUS_CODE != "SHUTDOWN" | STATUS_YEAR != year_inv)


# Reference codes
scc   <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.REF_SCC_CODES"), stringsAsFactors = F, as.is = T)

counties <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.GEO_COUNTIES"), stringsAsFactors = F, as.is = T)

poll_codes <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.REF_MATERIAL_CODES"), stringsAsFactors = FALSE)

processes <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.REF_SCC_DATA_CATEGORY_CODES"), stringsAsFactors = FALSE)

# Close open connections
odbcCloseAll()

src_process$INVENTORY_RID <- as.character(src_process$INVENTORY_RID)

emissions$INVENTORY_RID <- as.character(emissions$INVENTORY_RID)

sccs_2017 <- left_join(emissions[, c("INVENTORY_RID", "PROCESS_RID", "PERIOD_RID", "MATERIAL_CODE", "EMISSION_AMT", "EMISSION_UNIT_CODE")], src_process[, c("RID", "INVENTORY_RID", "SOURCE_RID", "EMISSION_UNIT_RID", "PROCESS_ID", "SCC_CODE", "SHORT_DESC")],
                       by = c("INVENTORY_RID" = "INVENTORY_RID", "PROCESS_RID" = "RID"))

sccs_2017 <- left_join(sccs_2017, scc, by = c("SCC_CODE" = "SCC_CODE"))

sccs_2017_drycleaners <- sccs_2017 %>%
  filter(grepl("Dry Clean", SHORT_DESC.x))

sccs_2017_drycleaners$SOURCE_RID <- as.character(sccs_2017_drycleaners$SOURCE_RID)
src_coords$ENTITY_RID <- as.character(src_coords$ENTITY_RID)

sccs_2017_drycleaners <- left_join(sccs_2017_drycleaners, src_coords[, c("RID", "ENTITY_TYPE", "ENTITY_RID", "ENTITY_ID", "ENTITY_NAME", "PRIMARY_COORD_SYSTEM", "LONGITUDE", "LATITUDE", "UTM_ZONE", "UTM_EASTING", "UTM_NORTHING", "GEO_STATE_NAME", "GEO_STATE_FIPS", "GEO_COUNTY_NAME", "GEO_COUNTY_FIPS", "GEO_CITY_NAME", "GEO_ZIP_CODE", "GEO_ADDRESS_1")], by = c("SOURCE_RID" = "ENTITY_RID"))

#write_csv(sccs_2017_drycleaners, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.5 Dry Cleaners/dry_cleaners_coords.csv")

latlong_stations <- st_as_sf(sccs_2017_drycleaners, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

plot(latlong_stations)

sccs_2017_drycleaners$SOURCE_RID <- as.character(sccs_2017_drycleaners$SOURCE_RID)

src_names$RID <- as.character(src_names$RID)

sccs_2017_drycleaners <- left_join(select(sccs_2017_drycleaners, -GEO_CITY_NAME, -GEO_ZIP_CODE, -GEO_ADDRESS_1), src_names[, c("RID", "SOURCE_NAME", "GEO_ADDRESS_1", "GEO_CITY_NAME", "GEO_ZIP_CODE")], by = c("SOURCE_RID" = "RID"))

##set up QA file

drycleaners_qa <- sccs_2017_drycleaners %>%
  select(INVENTORY_RID, SOURCE_RID, ENTITY_TYPE, ENTITY_ID, SOURCE_NAME, LONGITUDE, LATITUDE, GEO_COUNTY_NAME, GEO_CITY_NAME, GEO_ZIP_CODE, GEO_ADDRESS_1) %>%
  rename(inventory_id = INVENTORY_RID, county_name = GEO_COUNTY_NAME, entity_type = ENTITY_TYPE,	entity_id = ENTITY_ID,	source_rid = SOURCE_RID, source_name = SOURCE_NAME,	source_type = ENTITY_TYPE,	geo_address_1 = GEO_ADDRESS_1,	geo_city_name = GEO_CITY_NAME,	geo_zip_code = GEO_ZIP_CODE,	lat = LATITUDE,	long = LONGITUDE) %>%
  mutate(geometry = paste0("c(", long, ", ", lat, ")"))

dry_cleaners_qa <- unique(drycleaners_qa)
write_csv(drycleaners_qa, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/QA_facility_locations/Egg Hunt!/drycleaners/bad data/hidden_locations_2017.csv")


##set up dry cleaners for formatting
##Find Dry cleaners that weren't qa'ed
##This was a check from last time and does not need to be repeated.

#not_qaed_dcs <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/QA_facility_locations/Egg Hunt!/drycleaners/bad data/hidden_locations_1.csv")

#dry_cleaners_qaed <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/QA_facility_locations/Egg Hunt!/drycleaners/good data/01_correct_facility_locations_2018-04-13.csv")

#not_qaed <- anti_join(not_qaed_dcs, dry_cleaners_qaed, by = c("inventory_id" = "inventory_id", "source_rid" = "source_rid", "source_type" = "source_type", "entity_id" = "entity_id", "source_name" = "source_name"))

#write_csv(not_qaed, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/QA_facility_locations/Egg Hunt!/drycleaners/bad data/hidden_locations_2017_notqaed.csv")


##Pull in qaed dry cleaner locations
##From the 2014 check for dry cleaner locations

good_drycleaners_1 <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/QA_facility_locations/Egg Hunt!/drycleaners/good data/01_correct_facility_locations_2018-04-13.csv")

good_drycleaners_2 <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/QA_facility_locations/Egg Hunt!/drycleaners/good data/01_correct_facility_locations_2018-04-20.csv")

good_drycleaners <- rbind(good_drycleaners_1, good_drycleaners_2) %>% unique()

##check to see if new pull of Dry Cleaner data
good_drycleaners <- st_as_sf(good_drycleaners, coords = c("long", "lat"), crs = 4326, remove = FALSE)

good_drycleaners <- st_transform(good_drycleaners, crs = 26915)

good_dry_cleaners_coords <- st_coordinates(good_drycleaners) %>% as.data.frame() %>% setNames(c("utm_x", "utm_y"))

good_drycleaners <- bind_cols(good_drycleaners, good_dry_cleaners_coords)

sccs_2017_drycleaners <- st_as_sf(sccs_2017_drycleaners, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

sccs_2017_drycleaners <- st_transform(sccs_2017_drycleaners, crs = 26915)

sccs_2017_drycleaners_coords <- st_coordinates(sccs_2017_drycleaners) %>% as.data.frame() %>% setNames(c("utm_x", "utm_y"))

sccs_2017_drycleaners <- bind_cols(sccs_2017_drycleaners, sccs_2017_drycleaners_coords)

sccs_2017_drycleaners <- mutate(sccs_2017_drycleaners, SOURCE_RID = as.numeric(SOURCE_RID))



dry_cleaners_distance <- left_join(sccs_2017_drycleaners %>% as.data.frame(), good_drycleaners %>% as.data.frame(), by = c("SOURCE_NAME" = "source_name", "ENTITY_ID" = "entity_id"))

dry_cleaners_distance <- dry_cleaners_distance %>%
  mutate(distance_new_good = sqrt((utm_x.x - utm_x.y)^2 + (utm_y.x - utm_y.y)^2))

good_drycleaners <- dry_cleaners_distance %>%
  mutate(utm_x = ifelse(is.na(utm_x.x), utm_x.y, utm_x.x),
         utm_y = ifelse(is.na(utm_y.x), utm_y.y, utm_y.x)) %>%
  select(SOURCE_RID, ENTITY_ID, SOURCE_NAME, GEO_ADDRESS_1, GEO_CITY_NAME, inventory_id, source_type, county_name, utm_x.y, utm_y.y, distance_new_good, utm_x, utm_y) %>% unique()

##write_csv(good_drycleaners, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.5 Dry Cleaners/dry_cleaners_coords_compare.csv")

##The dry cleaners coords compare file was checked for locations and saved as the same name. The correct coords for the dry cleaners are the columns lat_correct and long_correct.
##set up qa-ed data

good_drycleaners <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.5 Dry Cleaners/dry_cleaners_coords_compare.csv")

good_drycleaners <- st_as_sf(good_drycleaners, coords = c("utm_x", "utm_y"), crs = 26915, remove = FALSE)

good_drycleaners <- st_transform(good_drycleaners, crs = 4326)

good_drycleaners_coords <- st_coordinates(good_drycleaners) %>% as_tibble() %>% setNames(c("long", "lat"))

good_drycleaners <- bind_cols(good_drycleaners, good_drycleaners_coords)

good_drycleaners <- good_drycleaners %>%
  mutate(long = ifelse(is.na(long), correct_long, long),
         lat = ifelse(is.na(lat), Correct_lat, lat)) %>%
  select(-correct_long, -Correct_lat)

good_drycleaners <- st_as_sf(good_drycleaners, coords = c("long", "lat"), crs = 4236, remove = FALSE)

good_drycleaners <- st_transform(good_drycleaners, crs = 26915)

good_drycleaners_coords <- st_coordinates(good_drycleaners) %>% as_tibble() %>% setNames(c("utm_x", "utm_y"))

data_spatial <- good_drycleaners %>%
  rename_all(tolower) %>%
  select(source_name, long, lat, county_name, entity_id, utm_x, utm_y) %>%
  rename(source_id = entity_id, county = county_name) %>%
  unique()

census_tracts <- st_read("R:/demographics/census_2010_blockgroups_usboc.shp")

data_spatial_tracts <- st_intersection(data_spatial, census_tracts)

counties <- st_read("R:/administrative_boundaries/counties_mndot.shp")

data_tracts_counties <- st_intersection(data_spatial_tracts, counties)

data_tracts_counties <- as_tibble(data_tracts_counties)

data_tracts_counties <- data_tracts_counties %>%
  select(source_id, source_name, long, lat, utm_x, utm_y, GEOID10, COUNTYNAME) %>%
  rename(bg_geoid = GEOID10,
         county = COUNTYNAME) %>%
  mutate(release_ht_meters = 3/2, ##height of a 1 story building is 3m
         side_length = 13, ##this is the length of a Martinizing in St. Paul MN
         sz = round(3/2.15, digits = 1),
         sy = round(side_length/4.3, digits = 1))


write_csv(data_tracts_counties, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.5 Dry Cleaners/drycleaners_final.csv")

## columns needed ##c("source_id", "source_name", "county", "bg_geoid", "lat", "long", "y_utm", "x_utm", "is_urban", "population", "met_station", "usaf", "year", "stn_name", "upa_id", "upa_name", "elevation", "release_ht_meters", "side_length", "sz", "sy")

