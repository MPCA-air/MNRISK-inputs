#
library(RODBC)
library(tidyverse)
library(sf)
# View available databases
odbcDataSources()

# Set inventory year
inv_year <- 2017



credentials <- read.csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/R_Camp/Student Folder/credentials.csv", stringsAsFactors = F)

d_cnx <- odbcConnect("deltaw",
                     uid = credentials$delta_user,
                     pwd = credentials$delta_pwd,
                     believeNRows = FALSE)

odbcGetInfo(d_cnx)

tables <- sqlTables(d_cnx, tableType = "TABLE", schema = "RAPIDS")  #Show all tables in deltaw database


# Get inventory year code
inv_codes  <- sqlQuery(d_cnx, "SELECT * FROM RAPIDS.INV_INVENTORIES", max = 40, stringsAsFactors = F)

inv_codes  <- filter(inv_codes, grepl(inv_year, INVENTORY_YEAR))

if (nrow(inv_codes) > 1) print(paste("Multiple inventory codes found: ",
                                    paste(inv_codes$INVENTORY_ID, collapse = ", ")))

inv_codes  <- filter(inv_codes,
                     nchar(INVENTORY_ID) == max(nchar(inv_codes$INVENTORY_ID)))$RID


#---------------------------------#
# Get sources and facility coords
#---------------------------------#
src_names   <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.inv_sources WHERE INVENTORY_RID = ", inv_codes), stringsAsFactors = F)

src_coords  <- sqlQuery(d_cnx, "SELECT * FROM RAPIDS.INV_COORDINATES", max = 100000, stringsAsFactors = F)



# Drop shutdown sources
## Keep stacks if no status update
## If status is NOT shutdown
## Or if status was updated after the MNRISK modeling year
shut_sources <- src_names %>%
                filter(!is.na(STATUS_YEAR),
                       STATUS_CODE == "SHUTDOWN",
                       STATUS_YEAR < inv_year)

src_names <- src_names %>%
             filter(is.na(STATUS_YEAR) |
                    STATUS_CODE != "SHUTDOWN" |
                    STATUS_YEAR >= inv_year)


# View source types
unique(src_names$SOURCE_TYPE)


# Join coords
src_all <- left_join(src_names,
                     select(src_coords, ENTITY_RID, LONGITUDE, LATITUDE),
                     by = c("RID" = "ENTITY_RID"))


#----------------------------------------------------#
# Drop option B facilities
#----------------------------------------------------#
src_all <- filter(src_all, !PERMIT_TYPE %in% c("B"))


#---------------------------------------------------------------#
# Get modeling inputs for facility release points
#---------------------------------------------------------------#
src_params  <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.INV_RELEASE_POINTS"), max = 500000, stringsAsFactors = F)


## Remove units shutdown before inventory year
src_params  <- filter(src_params, # STATUS_CODE != "SHUTDOWN" &
                      (is.na(END_OPERATION_DATE)   | as.Date(END_OPERATION_DATE)   > as.Date(paste0(inv_year,"-01-01"))) &
                      (is.na(BEGIN_OPERATION_DATE) | as.Date(BEGIN_OPERATION_DATE) < as.Date(paste0(inv_year,"-12-31"))))



# Filter parameters to most recent year
src_params <- src_params %>%
              group_by(SOURCE_RID, RID) %>%
              filter(max(STATUS_YEAR, na.rm = T) == -Inf | STATUS_YEAR == max(STATUS_YEAR, na.rm = T)) %>%
              ungroup()


# Join modeling params to sources
src_all <- left_join(src_all,
                     src_params %>% rename(release_point_rid = RID),
                     by = c("RID" = "SOURCE_RID"))


#-----------------------------------------#
# Rename columns
#-----------------------------------------#
names(src_all) <- tolower(names(src_all))

src_all <- rename(src_all,
                  source_rid           = rid,
                  lat                  = latitude,
                  long                 = longitude,
                  geoid_tract          = geo_census_tract_id,
                  height               = height_amt,
                  height_units         = height_unit_code,
                  diameter             = diameter_amt,
                  diameter_units       = diameter_unit_code,
                  exit_velocity        = exit_gas_velocity_amt,
                  exit_velocity_units  = exit_gas_velocity_unit_code,
                  exit_flow_rate       = exit_gas_flow_rate_amt,
                  exit_flow_rate_units = exit_gas_flow_rate_unit_code,
                  exit_temp            = exit_gas_temperature_amt,
                  exit_temp_units      = exit_gas_temperature_unit_code,
                  short_desc           = `short_desc.y`,
                  status_code_fac      = `status_code.x`,
                  status_code_stk      = `status_code.y`,
                  comment              = `comment_text.y`,
                  begin_date_fac       = `begin_operation_date.x`,
                  end_date_fac         = `end_operation_date.x`,
                  begin_date_stk       = `begin_operation_date.y`,
                  end_date_stk         = `end_operation_date.y`
                  )

# Drop most columns
src_all <- select(src_all,
                  county_rid,
                  source_name,
                  source_id,
                  source_rid,
                  source_type,
                  naics_code,
                  release_point_id,
                  release_point_rid,
                  release_point_type,
                  short_desc,
                  status_code_fac, status_code_stk,
                  comment,
                  begin_date_fac, end_date_fac,
                  begin_date_stk, end_date_stk,
                  lat, long,
                  height, height_units,
                  diameter, diameter_units,
                  exit_flow_rate, exit_flow_rate_units,
                  exit_temp, exit_temp_units
                  )


#---------------------------------#
# Join counties
#---------------------------------#
# County references
counties <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.GEO_COUNTIES"), stringsAsFactors = F, as.is = T)

counties <- counties %>%
            mutate(county_fips = ifelse(STATE_RID %in%  c(1011107), paste0(27, COUNTY_FIPS), COUNTY_FIPS),
                   RID = as.integer(RID))

# Join
src_all <- left_join(src_all,
                     counties %>% select(RID, county_fips),
                     by = c("county_rid" = "RID"))

# Put county first
src_all <- select(src_all, -county_rid) %>%
           select(county_fips, everything())



## Correct incorrect longitudes
## K&G coordinates are incorrect as are some funeral homes. These will be corrected here.

# Correct K&G location
kg_lat <- 44.29282
kg_lng <- -93.29392

src_all <- src_all %>%
  rowwise() %>%
  mutate(lat  = ifelse(source_name == "K & G MANUFACTURING CO.", kg_lat, lat),
         long = ifelse(source_name == "K & G MANUFACTURING CO.", kg_lng, long))

# Correct Adams Funeral Home location
ad_lat <- 43.56584
ad_lng <- -92.71789

src_all <- src_all %>%
  rowwise() %>%
  mutate(lat  = ifelse(source_name == "ADAMS FUNERAL HOME", ad_lat, lat),
         long = ifelse(source_name == "ADAMS FUNERAL HOME", ad_lng, long))

# Correct Glend-Nilson Funeral Home location
gn_lat <- 46.28171
gn_lng <- -96.07061


src_all <- src_all %>%
          rowwise() %>%
          mutate(lat  = ifelse(source_name == "GLENDE-NILSON FUNERAL HOME", gn_lat, lat),
                 long = ifelse(source_name == "GLENDE-NILSON FUNERAL HOME", gn_lng, long))


#---------------------------------#
# Save facility locations
#---------------------------------#
src_points <- src_all %>%
              group_by(county_fips, source_name, source_id, source_type) %>%
              summarize(lat  = mean(lat, na.rm = T),
                        long = mean(long, na.rm = T))

wrong_long <- src_points %>% filter(long > 0)

if (nrow(wrong_long) > 0) {
    write_csv(wrong_long,
              paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/01_CEDR facility coords_wrong_longitude.csv"))
}

#---------------------------------#
# Save facility locations
#---------------------------------#
check_facs_no_lat <- src_points %>%
                     filter(is.na(lat),
                            source_type %in% c("POINT", "ALLOC POINT"))

src_points <- src_all %>%
              filter(!is.na(lat)) %>%
              st_as_sf(coords = c("long", "lat"), crs = 4326, remove = FALSE, na.fail = FALSE)


# Find average of stack locations in UTM
src_points <- st_transform(src_points, crs = 26915)

src_points_coords <- st_coordinates(src_points) %>%
                     as.data.frame() %>%
                     setNames(c("x_utm", "y_utm"))

src_points <- bind_cols(src_points, src_points_coords)

src_points <- src_points %>%
              group_by(county_fips, source_name, source_id, source_type) %>%
              summarize(x_utm  = mean(x_utm, na.rm = T),
                        y_utm = mean(y_utm, na.rm = T)) %>%
              ungroup()


src_points <- src_points %>%
              st_as_sf(coords = c("x_utm", "y_utm"), crs = 26915, remove = FALSE, na.fail = FALSE)

# Get stack average location in Lat/Long
src_points <- st_transform(src_points, crs = 4326)

src_points_coords <- st_coordinates(src_points) %>%
                     as.data.frame() %>%
                     setNames(c("long", "lat"))

src_points <- bind_cols(src_points, src_points_coords) %>%
              st_set_geometry(NULL)

# Save coords
write_csv(src_points,
          paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/01_CEDR2017_Facility_coords.csv"))


# Quick map
if (F) {

  library(leaflet)

  leaflet() %>%
    addTiles() %>%
    addMarkers(lng = src_points$long, lat = src_points$lat, popup = src_points$source_name)

}


#-------------------------------------------------#
# Drop events, portables, airports, and non-point
#------------------------------------------------#

## Save portables
portables <-  src_all %>%
              filter(source_type %in% c("POINT", "ALLOC POINT"),
                     is.na(lat)) %>%
              group_by(source_name, source_id) %>%
              #mutate(n = n()) %>%
              slice(1)

unique(portables$source_name) %>% sort()

write_csv(portables,
          paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/01_CEDR portable facilities.csv"))


# Continue W/O portables
src_all <- src_all %>%
           filter(source_type %in% c("POINT", "ALLOC POINT"),
                  !is.na(lat))



#---------------------------------#
# Processes
#---------------------------------#
src_process <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.inv_processes WHERE INVENTORY_RID = ", inv_codes),
                        stringsAsFactors = F,
                        as.is            = T)


process_rls_points_all <- sqlQuery(d_cnx,
                                   paste0("SELECT * FROM RAPIDS.inv_process_release_points"),
                                   stringsAsFactors = F,
                                   as.is            = T)


# St Paul Park Refinery processes
#sppr_process <- filter(src_process, SOURCE_RID == filter(src_names, SOURCE_ID == 2716300003)$RID)


# Rename columns
names(src_process) <- names(src_process) %>% tolower()

src_process$source_rid        <- as.integer(src_process$source_rid)

src_process$emission_unit_rid <- as.integer(src_process$emission_unit_rid)

src_process <- src_process %>%
               rename(short_desc_process = short_desc)


#-----------------------------------------#
# Check for stacks with no emissions
#-----------------------------------------#

# Processes with minimum emissions
min_emissions <- sqlQuery(d_cnx,
                          paste0("SELECT DISTINCT process_rid FROM RAPIDS.INV_PROCESS_EMISSIONS WHERE INVENTORY_RID = ", inv_codes, " AND ((EMISSION_AMT > 0) or (FUGITIVE_EMISSION_AMT > 0))"),
                          max              = 300000,
                          stringsAsFactors = F,
                          as.is            = T) #%>%
                 #filter(EMISSION_AMT > 0 | FUGITIVE_EMISSION_AMT > 0) %>%
                 #select(PROCESS_RID)

chk_emissions <- sqlQuery(d_cnx,
                          paste0("SELECT * FROM RAPIDS.INV_PROCESS_EMISSIONS WHERE",
                                 "INVENTORY_RID = ", inv_codes,
                                 " AND PROCESS_RID = 101152856",
                                 " AND EMISSION_AMT > 0 or FUGITIVE_EMISSION_AMT > 0"),
                           max              = 3000,
                           stringsAsFactors = F,
                           as.is            = T)



# Drop release points without operating processes
process_rls_points <- process_rls_points_all %>%
                      filter(PROCESS_RID %in% src_process$rid)

# Drop release points without emissions
process_rls_points <- process_rls_points_all %>%
                      filter(PROCESS_RID %in% min_emissions$PROCESS_RID) %>%
                      rename_all(tolower) %>%
                      mutate(release_point_rid = as.numeric(release_point_rid))



##-------------------------------------------------#
## Assign release pts to sources with only NA
##-------------------------------------------------#

# Not needed for 2017
## Nate updated CEDR database
if (FALSE) {
# Sources with missing release points
missing_rls_pts <- src_all %>%
                   filter(is.na(release_point_rid),
                          source_rid %in% src_process$source_rid)

# Sources with only missing rls points
only_missing_pts <- src_all %>%
                    group_by(source_id) %>%
                    filter(n_distinct(na.omit(release_point_id)) < 1,
                           source_rid %in% src_process$source_rid)

# Processes for missing pts
only_missing_procs <- src_process %>%
                      filter(source_id %in% only_missing_pts$source_id)

}



# Drop release points from source table not assigned to a process
src_emitting <- src_all %>%
                filter(source_rid %in% process_rls_points$source_rid | is.na(release_point_rid),
                       release_point_rid %in% process_rls_points$release_point_rid)
               #| is.na(release_point_rid))


dropped_facs <- src_all %>%
                filter(!source_name %in% unique(src_emitting$source_name)) %>%
                group_by(source_name) %>%
                slice(1)



# Add process flows
src_emitting <- left_join(src_emitting,
                          select(process_rls_points, process_rid,
                                 flow_pct, release_point_rid))


# Add process SCCs
src_emitting <- left_join(src_emitting,
                          select(src_process, emission_unit_rid, process_id, short_desc_process, scc_code, rid),
                          by = c("process_rid" = "rid"))


#---------------------------------#
# Add emission units
#---------------------------------#
# Get operating emission units
src_units   <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.INV_EMISSION_UNITS WHERE INVENTORY_RID = ", inv_codes), max = 500000, stringsAsFactors = F)


## Remove units built after inventory year or shut down before inventory year
src_units <- src_units %>%
            rename_all(tolower) %>%
            filter(#STATUS_CODE != "SHUTDOWN" &
                              (is.na(end_operation_date) | as.Date(end_operation_date) > as.Date(paste0(inv_year,"-01-01"))) &
                              (is.na(begin_operation_date) | as.Date(begin_operation_date) <= as.Date(paste0(inv_year,"-12-31"))))


names(src_units) <- names(src_units) %>% tolower()

src_units <- rename(src_units,
                    emission_unit_rid = rid,
                    eu_naics_code     = naics_code,
                    eu_desc           = short_desc)


# Join emission units
src_emitting <- left_join(src_emitting,
                          select(src_units, emission_unit_id,
                                 emission_unit_rid, eu_desc,
                                 emission_unit_type, eu_naics_code),
                          by = c("emission_unit_rid" = "emission_unit_rid"))


# Check units w/ different NAICS codes than facility
diff_naics <- src_emitting %>% filter(naics_code != eu_naics_code)



#---------------------------------#
# SCC reference
#---------------------------------#
#scc      <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.REF_SCC_CODES"), stringsAsFactors = F, as.is = T)

#scc2     <- sqlQuery(d_cnx, paste0("SELECT * FROM RAPIDS.REF_CEDR_SCC_OPTD"), stringsAsFactors = F, as.is = T)


#-------------------------------------#
# Save stack params
#-------------------------------------#
saveRDS(src_emitting,
        paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/01_", inv_year, "_CEDR stack params.Rdata"))



#---------------------------------#
# Close open connections
#---------------------------------#
odbcCloseAll()


##
