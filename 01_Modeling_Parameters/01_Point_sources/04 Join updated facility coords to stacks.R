library(tidyverse)
library(sf)
library(leaflet)

inv_year <- 2017


# Load facility Centroids
## This file has not been updated
## Need to compare this file to the final easter egg hunt file. and if identical, this file does not need an update.
fac_coords_orig <- read.csv(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/01_CEDR2017_Facility_coords.csv"), stringsAsFactors = F)


# Easter egg hunt update
fac_coords <- read.csv(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/R/QA_facility_locations/review/2018-04-13_bulk_TEMPO_update_new_air_coordinates.csv"), stringsAsFactors = F)

fac_coords <- fac_coords %>%
                rename_all(tolower) %>%
                rename(source_id = entity_id,
                       lat       = latitude,
                       long      = longitude) %>%
                select(-c(coordinate_system_code:confidential_flag))


fac_coords <- fac_coords %>%
                mutate(source_id = as.character(source_id)) %>%
                bind_rows(filter(fac_coords_orig, !source_id %in% fac_coords$source_id)) %>%
                rename(lat_new  = lat,
                       long_new = long)

rm(fac_coords_orig)


# Coordinates have improved in CEDR
## Virnig Manuf.
## DPC Inc
cedr_better_facs <- c("2700900042", "2703709003")

fac_coords <- fac_coords %>% filter(!source_id %in% cedr_better_facs)


# Updates
## Riverbend Kayak in Winona
## Poet Biorefining Glenville
update_coords_2020 <- tibble(source_id = c("2716900088", "2704700055"),
                             lat_fix   = c(44.03238, 43.575536),
                             long_fix  = c(-91.6123, -93.30205))

fac_coords <- fac_coords %>%
              left_join(update_coords_2020) %>%
              mutate(lat_new  = ifelse(is.na(lat_fix), lat_new, lat_fix),
                     long_new = ifelse(is.na(long_fix), long_new, long_fix)) %>%
              select(-lat_fix, -long_fix)



# Load CEDR stack coordinates
stacks <- readRDS(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/03_Cleaned_stack_params.Rdata")) %>%
          mutate(source_id = as.character(source_id),
                 lat  = ifelse(is.na(lat_mod), lat, lat_mod),
                 long = ifelse(is.na(long_mod), long, long_mod)) %>%
          select(-flow_rate_m3_s)



# Update to Egg-hunt corrected facility centroid coordinates
stacks <- stacks %>%
          left_join(select(fac_coords, source_id, lat_new, long_new)) %>%
          group_by(source_id, release_point_id) %>%
          mutate(lat_mod  = coalesce(lat, lat_new),
                 long_mod = coalesce(long, long_new)) %>%
          ungroup() %>%
          relocate(c(lat, long, lat_new, long_new, lat_mod, long_mod), .after = last_col())


# Drop modeled coords for updated facilities
stacks <- stacks %>%
          rowwise() %>%
          mutate(lat_mod  = ifelse(source_id %in% update_coords_2020$source_id, NA, lat_mod),
                 long_mod = ifelse(source_id %in% update_coords_2020$source_id, NA, long_mod))

# Drop incorrect modeled coords
## 3M Hutchinson SV312
## Marvin Windows stru2
## Hibbing Utilities SV005 & 6
## Heartland Corn wgp00
bad_coord_stacks <- c("2708500049 sv312",
                      "2713500002 stru2",
                      "2713700027 sv005", "2713700027 sv006",
                      "2714300014 wgp00")

stacks <- stacks %>%
          rowwise() %>%
          mutate(stack_id = paste(source_id, release_point_id),
                 lat_mod  = ifelse(paste(source_id, release_point_id) %in% bad_coord_stacks, NA, lat_mod),
                 long_mod = ifelse(paste(source_id, release_point_id) %in% bad_coord_stacks, NA, long_mod))



#---------------------------------#
# SAVE updated coordinate stacks
# before updating centroid
#---------------------------------#
write.csv(stacks,
          paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/04_Cleaned_stack_params_with_qcd_coords.csv"), quote = T, row.names = F)


# Count stacks per source
stacks <- stacks %>%
          group_by(source_id) %>%
          mutate(n_stacks = n())


# Count unique coordinate locations
stacks <- stacks %>%
          group_by(source_id) %>%
          mutate(n_coords     = n_distinct(paste(lat_new, long_new)),
                 n_mod_coords = n_distinct(paste(lat_mod, long_mod))) %>%
          ungroup()


#----------------------------------------------------------------#
# Use detailed 2014 stack params  ----
# if current year only has 1 stack location and past year has more
#----------------------------------------------------------------#
single_locs <- stacks %>%
               group_by(source_id) %>%
               filter(n_stacks > 1,
                      max(n_coords, n_mod_coords, na.rm = T) == 1) %>%
               mutate(priority = 2) %>%
               ungroup()

past_coords <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/_development/5. Sent to Lakes/Source parameters/facilities.csv") %>%
               group_by(source_id) %>%
               mutate(n_stacks = n(),
                      n_coords   = n_distinct(lat),
                      stack_id = paste(source_id, rel_point_id),
                      priority = 1) %>%
               filter(n_stacks > 1,
                      n_coords > 1,
                      stack_id %in% single_locs$stack_id) %>%
               select(-c(urban:elevation), -county, -bg_geoid, -x_utm, -y_utm, -scc_code) %>%
               rename(release_point_id = rel_point_id,
                      height_m    = release_ht_meters,
                      exit_vel_ms = velocity_ms,
                      lat_mod     = lat,
                      long_mod    = long)


# Blank stack modeling inputs for volume sources
unique(past_coords$source_type)

past_vols <- past_coords %>%
            filter(source_type == "VOLUME") %>%
            mutate(height_m    = NA,
                   diameter_m  = NA,
                   exit_vel_ms = NA,
                   temp_k      = NA)

past_coords <- past_coords %>% filter(source_type != "VOLUME") %>%
               bind_rows(past_vols)

# Blank volume modeling inputs for point sources
past_pts <- past_coords %>%
            filter(source_type %in% "POINT") %>%
            mutate(sy = NA,
                   sz = NA,
                   side_length = NA)

past_coords <- past_coords %>% filter(source_type != "POINT") %>%
               bind_rows(past_pts)


# Check name match
names(past_coords)[!names(past_coords) %in% names(single_locs)]
names(single_locs)[!names(single_locs) %in% names(past_coords)]


# Check for ource type changes: Volume source to -> Vertical
type_change <- past_coords %>%
               left_join(single_locs %>% select(source_id, release_point_id, source_type) %>% rename(source_type_2017 = source_type)) %>%
               filter(tolower(source_type) != tolower(source_type_2017))


# Swap in new params
first_val <- function(x) {
  x[which(!is.na(x) & x != 0)[1]]
}

single_locs <- bind_rows(single_locs, past_coords) %>%
               group_by(source_id, release_point_id) %>%
               arrange(priority) %>%
               mutate_all(first_val) %>%
               slice(1)

# Join back
src_chk <- nrow(stacks)

stacks <- stacks %>%
          filter(!stack_id %in% single_locs$stack_id) %>%
          bind_rows(single_locs) %>%
          select(-n_mod_coords, n_coords, -n_stacks, -priority, -stack_id)


# Should be TRUE
print(src_chk == nrow(stacks))


# Facility centroid set to average of all stacks ----
stacks <- stacks %>%
          group_by(source_id) %>%
          mutate(lat_avg  = if_else(is.na(lat_mod), median(lat_mod, na.rm = T), lat_new),
                 long_avg = if_else(is.na(long_mod), median(long_mod, na.rm = T), long_new))


# Set lat/long to modeled coords if available
stacks <- stacks %>%
          rowwise() %>%
          mutate(lat_final   = coalesce(lat_mod, lat_avg, lat_new),
                 long_final  = coalesce(long_mod, long_avg, long_new))


# Check for stacks missing coordinates
miss <- filter(stacks, is.na(lat_final))

nrow(miss)


#---------------------------------#
# View large location changes
#---------------------------------#
loc_changes <- stacks %>%
               filter(round(lat, 2) != round(lat_new, 2) | round(long, 2) != round(long_new, 2)) %>%
               group_by(source_id, lat, long) %>%
               slice(1)


# Map changes
leaflet(loc_changes) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(data  = loc_changes,
             lng   = ~long,
             lat   = ~lat,
             label = ~paste(source_name, release_point_id, sep = ":   "),
             color = "green") %>%
  addMarkers(data  = loc_changes,
             lng   = ~long_new,
             lat   = ~lat_new,
             label = ~paste(source_name, release_point_id, sep = ":   "))


# Drop columns
stacks <- stacks %>%
          select(-c(source_rid,
                    lat:long_avg,
                    release_point_rid,
                    source_group)) %>%
          select(source_name:source_type, everything()) %>%
          rename(lat  = lat_final,
                 long = long_final) %>%
          mutate(lat2  = lat,
                 long2 = long)


# Map all
if(F) {
  leaflet(stacks) %>%
    setView(lat = 43.643, lng = -93.4057, zoom = 12) %>%
    #addWMSTiles(baseUrl = "https://imageserver.gisdata.mn.gov/cgi-bin/wmswm?", layers = "fsa019") %>%
    addProviderTiles(provider = providers$Esri.WorldImagery) %>%
    addCircleMarkers(data  = stacks %>% group_by(source_id, lat, long) %>% slice(1) %>% filter(source_id == 2704700055),
                     lng   = ~long,
                     lat   = ~lat,
                     label = ~paste(source_name, release_point_id, sep = ":   "),
                     color = "hotpink")
}


#---------------------------------#
# SAVE updated coordinate stacks
#---------------------------------#
saveRDS(stacks,
          paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/04_Cleaned_stack_params_with_mod_coords.Rdata"))


#
