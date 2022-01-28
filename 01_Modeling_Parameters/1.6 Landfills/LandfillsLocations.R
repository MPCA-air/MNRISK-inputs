library(RODBC)
library(tidyverse)
library(readxl)
library(sf)
library(rgdal)
library(maptools)

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

sccs_2017_landfills <- sccs_2017 %>%
  filter(grepl("Landfill", SHORT_DESC.x)|grepl("Landfill", SHORT_DESC.y))

src_names$RID <- as.character(src_names$RID)

src_names$INVENTORY_RID <- as.character(src_names$INVENTORY_RID)

sccs_2017_landfills <- left_join(sccs_2017_landfills, src_names, by = c("INVENTORY_RID" = "INVENTORY_RID", "SOURCE_RID" = "RID"))

sccs_2017_landfills <- filter(sccs_2017_landfills, SOURCE_TYPE == "ALLOC POINT")

source_flares_fugitives <- sccs_2017_landfills %>%
  select(SOURCE_NAME, DEFAULT_EMISSION_UNIT_TYPE) %>%
  unique()

##checking to see which has more emissions-flares or fugitives
flares_fugitives <- sccs_2017_landfills %>%
  group_by(MATERIAL_CODE, SHORT_DESC.x) %>%
  summarise(EMISSION_AMT = sum(as.numeric(EMISSION_AMT), na.rm = T), Count = n())

write_csv(flares_fugitives, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.6 Landfills/flares_fugitives.csv")

##We looked into this to determine how to model these emissions. Flares would be modeled with a higher exit temperature, where fugitives would be ambient temp. We will re-look at it, and determine can we do both? Are there separate processes that we could model like in regular point sources? Check this.

library(ggplot2)

ggplot(data = flares_fugitives, aes(x = MATERIAL_CODE, y = EMISSION_AMT, fill = `SHORT_DESC.x`)) +
  geom_col(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


sccs_2017_landfills$SOURCE_RID <- as.character(sccs_2017_landfills$SOURCE_RID)

src_coords$ENTITY_RID <- as.character(src_coords$ENTITY_RID)
sccs_2017_landfills <- left_join(sccs_2017_landfills, src_coords[, c("RID", "ENTITY_TYPE", "ENTITY_RID", "ENTITY_ID", "ENTITY_NAME", "PRIMARY_COORD_SYSTEM", "LONGITUDE", "LATITUDE", "UTM_ZONE", "UTM_EASTING", "UTM_NORTHING", "GEO_STATE_NAME", "GEO_STATE_FIPS", "GEO_COUNTY_NAME", "GEO_COUNTY_FIPS", "GEO_CITY_NAME", "GEO_ZIP_CODE", "GEO_ADDRESS_1")], by = c("SOURCE_RID" = "ENTITY_RID"))

sccs_2017_landfills <- sccs_2017_landfills %>%
  select(INVENTORY_RID, SOURCE_RID, SOURCE_TYPE, SOURCE_NAME, LONGITUDE, LATITUDE, ENTITY_ID, ENTITY_TYPE, GEO_COUNTY_NAME, `GEO_CITY_NAME.x`, `GEO_ZIP_CODE.x`, `GEO_ADDRESS_1.x`) %>%
  unique() %>%
  rename(inventory_id = INVENTORY_RID,
         source_rid	= SOURCE_RID,
         source_type = SOURCE_TYPE,
         entity_id = ENTITY_ID,
         source_name = SOURCE_NAME,
         long = LONGITUDE,
         lat = LATITUDE,
         county_name = GEO_COUNTY_NAME,
         geo_city_name = `GEO_CITY_NAME.x`,
         geo_zip_code	= `GEO_ZIP_CODE.x`,
         geo_address_1 = `GEO_ADDRESS_1.x`) %>%
  mutate(geometry = paste0("c(", long, ",", lat, ")"))

write_csv(sccs_2017_landfills, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/QA_facility_locations/Egg Hunt!/landfills/bad data/hidden_locations_2017.csv")

##Check new data vs fixed older data (i.e. from MNRISKS 2014)
landfills_2014 <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/QA_facility_locations/Egg Hunt!/landfills/good data/01_correct_facility_locations_2018-04-20.csv")

check_4_same <- left_join(sccs_2017_landfills %>% select(entity_id, source_name, long, lat), landfills_2014 %>% select(entity_id, source_name, long, lat), by = c("entity_id", "source_name"))

check_4_same <- check_4_same %>%
  mutate(long_diff = long.x - long.y,
         lat_diff = lat.x - lat.y)

write_csv(check_4_same, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.6 Landfills/landfill_coords_check.csv")

##MNRISKS 2011 Sy = 5, Sz = 1
#3Measured three landfills in MN
#Name	X(m)	Y(m)
#Rolling Hills Landfill	540	950
#Waste Management - Elk River Landfill	591	886
#Waste Management - Spruice Ridge Landfill	1256	1300
##side length = 500 meters

good_landfills <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.6 Landfills/landfill_coords_check.csv")

landfills <- good_landfills %>%
  mutate(long = ifelse(is.na(correct_long), long.y, correct_long),
         lat = ifelse(is.na(correct_lat), lat.y, correct_lat)) %>%
  select(entity_id, source_name, long, lat) %>%
  filter(!is.na(lat))

data_spatial <- st_as_sf(landfills, coords = c("long", "lat"), crs = 4326, remove = FALSE)

data_spatial <- st_transform(data_spatial, crs = 26915)

data_spatial_coords <- st_coordinates(data_spatial) %>% as.data.frame() %>% setNames(c("utm_x", "utm_y"))

data_spatial <- bind_cols(data_spatial, data_spatial_coords)

census_tracts <- st_read("R:/demographics/census_2010_blockgroups_usboc.shp")


data_spatial_tracts <- st_intersection(data_spatial, census_tracts)

counties <- st_read("R:/administrative_boundaries/counties_mndot.shp")

data_tracts_counties <- st_intersection(data_spatial_tracts, counties)

data_tracts_counties <- as.data.frame(data_tracts_counties)

data_tracts_counties <- data_tracts_counties %>%
  select(entity_id, source_name, long, lat, utm_x, utm_y, GEOID10, COUNTYNAME) %>%
  rename(source_id = entity_id,
         bg_geoid = GEOID10,
         county = COUNTYNAME) %>%
  mutate(release_ht_meters = 2,
         side_length = 500,
         sz = round(release_ht_meters/2.15, digits = 1),
         sy = round(side_length/4.15, digits = 1))

write_csv(data_tracts_counties, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.6 Landfills/landfills_final.csv")


## columns needed ##c("source_id", "source_name", "county", "bg_geoid", "lat", "long", "y_utm", "x_utm", "is_urban", "population", "met_station", "usaf", "year", "stn_name", "upa_id", "upa_name", "elevation", "release_ht_meters", "side_length", "sz", "sy")
