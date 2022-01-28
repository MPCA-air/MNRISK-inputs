library(tidyverse)
library(sf)

# Facility source parameters from Modeling Platform: ftp://newftp.epa.gov/Air/emismod/2017/2017emissions/

params_folders <- list.dirs("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/Border_state_facilities/2017gb_17j/inputs")

params_folders <- params_folders[params_folders != "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/Border_state_facilities/2017gb_17j/inputs"]

all_params <- tibble()

i <- params_folders[4]

for(i in params_folders){

  setwd(i)

  source_param_files <- list.files()
  source_param_files <- source_param_files[grepl(".csv", source_param_files)]

  j <- source_param_files

  for (j in source_param_files) {

    source_params <- read_csv(j, skip = 6, n_max = 3)

    if (ncol(source_params) == 1) {
      source_params <- read_csv(j, skip = 19)
    } else {
      source_params <- read_csv(j, skip = 6)
    }

    source_params <- source_params %>%
                      filter(substr(region_cd, 1, 2) %in% c(19, 38, 46, 55)) %>%
                      select(country_cd, region_cd, facility_id, facility_name,
                             unit_id, rel_point_id, process_id,
                             facil_category_code, fac_source_type,
                             agy_facility_id, agy_unit_id, agy_rel_point_id,
                             scc, stkhgt, stkdiam, stktemp, stkflow, stkvel,
                             naics, longitude, latitude) %>%
                      unique()

    all_params <- rbind(all_params, source_params)
  }
}

saveRDS(all_params, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/Border_state_facilities/borderstate_facility_parameters_df.Rdata")


## View facilities
fac_params <- readRDS("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/Border_state_facilities/borderstate_facility_parameters_df.Rdata")

fac_params_geo <- st_as_sf(fac_params,
                           coords = c("longitude", "latitude"),
                           crs = 4269,
                           remove = F)

#######Get MN state border
mn_state <- st_read("R:/administrative_boundaries/state_mndot.shp",
                    stringsAsFactors = FALSE)

mn_state_buffer <- st_buffer(mn_state, dist = 20000)

#buffer_select <- st_difference(mn_state_buffer, mn_state)

buffer_latlong <- st_transform(mn_state_buffer, crs = 4269)

fac_params_buffer <- st_join(fac_params_geo, buffer_latlong, left = FALSE)

unique(fac_params_buffer$facility_id)

#rm(buffer_latlong, buffer_select, fac_params_geo, mn_state_buffer, fac_params, mn_state)
gc()


fac_params_buffer$long <- st_coordinates(fac_params_buffer)[,1]
fac_params_buffer$lat <- st_coordinates(fac_params_buffer)[,2]

fac_params_buffer <- st_transform(x = fac_params_buffer, crs = 26915)

fac_params_buffer$utm_x <- st_coordinates(fac_params_buffer)[,1]
fac_params_buffer$utm_y <- st_coordinates(fac_params_buffer)[,2]

fac_params_buffer <- st_set_geometry(fac_params_buffer, NULL) %>%
                     unique()

# SAVE
saveRDS(fac_params_buffer, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/Border_state_facilities/borderstate_facility_parameters.Rdata")


