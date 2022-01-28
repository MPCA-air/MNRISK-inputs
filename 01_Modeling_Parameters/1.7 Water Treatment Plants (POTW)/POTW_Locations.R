library(RODBC)
library(tidyverse)
library(rgdal)
library(maptools)
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

##potws are "publically owned treatment works" or "Pokemon of the week"
sccs_2017_potws <- sccs_2017 %>%
  filter(SCC_CODE == 50100701)

src_names$RID <- as.character(src_names$RID)

src_names$INVENTORY_RID <- as.character(src_names$INVENTORY_RID)

sccs_2017_potws <- left_join(sccs_2017_potws, src_names, by = c("INVENTORY_RID" = "INVENTORY_RID", "SOURCE_RID" = "RID"))

sccs_2017_potws <- filter(sccs_2017_potws, SOURCE_TYPE == "ALLOC POINT")

src_coords$ENTITY_RID <- as.character(src_coords$ENTITY_RID)

sccs_2017_potws <- left_join(sccs_2017_potws, src_coords[, c("RID", "ENTITY_TYPE", "ENTITY_RID", "ENTITY_ID", "ENTITY_NAME", "PRIMARY_COORD_SYSTEM", "LONGITUDE", "LATITUDE", "UTM_ZONE", "UTM_EASTING", "UTM_NORTHING", "GEO_STATE_NAME", "GEO_STATE_FIPS", "GEO_COUNTY_NAME", "GEO_COUNTY_FIPS", "GEO_CITY_NAME", "GEO_ZIP_CODE", "GEO_ADDRESS_1")], by = c("SOURCE_RID" = "ENTITY_RID"))

sccs_2017_potws <- sccs_2017_potws %>%
  select(INVENTORY_RID, SOURCE_RID, SOURCE_TYPE, SOURCE_NAME, LONGITUDE, LATITUDE, ENTITY_ID, ENTITY_TYPE, GEO_COUNTY_NAME, `GEO_CITY_NAME.x`, `GEO_ZIP_CODE.x`, `GEO_ADDRESS_1.x`) %>%
  unique() %>%
  rename(lat = LATITUDE,
         long = LONGITUDE,
         entity_id = ENTITY_ID,
         source_name = SOURCE_NAME,
         geo_address_1 = GEO_ADDRESS_1.x,
         geo_city_name = GEO_CITY_NAME.x,
         geo_zip_code = GEO_ZIP_CODE.x) %>%
  mutate(geometry = paste0("c(", long, ", ", lat, ")"))

write_csv(sccs_2017_potws, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/QA_facility_locations/Egg Hunt!/potws/bad data/hidden_locations_2017.csv")

##potws from MNRISKS2014

mnrisks2014_potws <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/_development/1. Modeling parameters/1.7 POTW_Water Treatment Plants/potws_final.csv") %>% unique()

unique(mnrisks2014_potws$source_name)

##check new potws vs checked locations from 2014MNRISKS potw locations
checked_potws <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.7 Water Treatment Plants (POTW)/01_correct_facility_locations_2018-08-22_1.csv") %>% unique()

##checked potws is the same as the final MNRISKS2014 potws file.
check_4_same <- left_join(sccs_2017_potws %>% select(entity_id, source_name, long, lat), checked_potws %>% select(entity_id, source_name, long, lat), by = c("entity_id", "source_name"))

check_4_same <- check_4_same %>%
  mutate(long_diff = long.x - long.y,
         lat_diff = lat.x - lat.y)

##There are about 100 less potws this time compared to last time.
new_potws <- anti_join(sccs_2017_potws %>% select(entity_id, source_name, long, lat), checked_potws %>% select(entity_id, source_name, long, lat), by = c("entity_id", "source_name"))

write_csv(new_potws, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.7 Water Treatment Plants (POTW)/missing_in_2017_potws_coords_check.csv")

##check for significant distances using st_distance()

biggest_distance <- check_4_same %>%
  filter(source_name == "Fountain WWTP")

fountain_1 <- st_as_sf(biggest_distance, coords = c("long.x", "lat.x"), crs = 4326)

fountain_2  <- st_as_sf(biggest_distance, coords = c("long.y", "lat.y"), crs = 4326)

st_distance(fountain_1, fountain_2)
##37km? I don't think these got fixed. We will just use our corrected and checked locations.

check_4_same <- check_4_same %>%
  mutate(check_coords = ifelse(abs(lat_diff) > 0.001|abs(long_diff) > 0.001|is.na(lat.y), "check", "don't check"))

#write_csv(check_4_same, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.7 Water Treatment Plants (POTW)/potws_coords_check.csv")

##Checked to see if we grabbed all POTWs (i.e. not just new ones, etc)
##Checked location of all allocated source POTWs

potws <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.7 Water Treatment Plants (POTW)/potws_coords_check.csv")

potws <- potws %>%
  rowwise() %>%
  mutate(lat_correct = gsub(",", "", lat_correct) %>% as.numeric(),
         lat = coalesce(lat_correct, lat.y, lat.x),
         long = coalesce(long_correct, long.y, long.x)) %>%
  select(entity_id, source_name, lat, long) %>%
  mutate(release_point_type = 	"VERTICAL",
         height = 25.93133651,
         height_units = "FT",
         diameter = 2.119230747,
         dimaeter_unit = "FT",
         exit_gas_flow_rate = 64.03131081,
         exit_gas_flow_rate_unit = "FT3/SEC",
         exit_gas_temperature = 126.0547383,
         exit_gas_temperature_unit = F,
         sy = NA,
         sz = NA) %>%
  ungroup() %>%
  rename(source_id = entity_id)

potws <- st_as_sf(potws, coords = c("long", "lat"), crs = 4326, remove = FALSE)

potws <- st_transform(potws, crs = 26915)

potws_coords <- st_coordinates(potws) %>% as.data.frame() %>% setNames(c("utm_x", "utm_y"))

potws <- bind_cols(potws, potws_coords)


census_blkgrps <- st_read("R:/demographics/census_2010_blockgroups_usboc.shp")

counties <- st_read("R:/administrative_boundaries/counties_mndot.shp")

blkgrps_counties <- st_intersection(census_blkgrps, counties) %>% select(GEOID10, COUNTYNAME)

potws <- st_join(potws, blkgrps_counties)

write_csv(potws, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.7 Water Treatment Plants (POTW)/potws_final.csv")


## columns needed ##c("source_id", "source_name", "county", "bg_geoid", "lat", "long", "y_utm", "x_utm", "is_urban", "population", "met_station", "usaf", "year", "stn_name", "upa_id", "upa_name", "elevation", "release_ht_meters", "side_length", "sz", "sy")

