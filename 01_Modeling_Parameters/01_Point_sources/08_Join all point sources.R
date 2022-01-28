library(tidyverse)
library(readxl)
library(sf)

inv_year <- 2017


# Check that all processing files were run / updated
## Stop if dates don't match

# Get list of R data files
run_files <- list.files("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources", full.names = T)

# Drop the trains file - That is added by this script
run_files <- run_files[grepl("Rdata", run_files)]

run_files <- run_files[!grepl("trains", run_files)]

run_dates <- file.info(run_files)$mtime %>% as.Date()

if (n_distinct(run_dates) > 1) stop("All .Rdata files not up-to-date. Check the folder to see if a script did not run.")


# Load MN stack coordinates
stacks <- readRDS(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/_development/1. Modeling parameters/1. Point sources/07-5_Cleaned_stack_params_with_final_coords_grouped.Rdata"))

stacks <-  st_as_sf(stacks, coords = c("long", "lat"), crs = 4326, remove = FALSE)

stacks <- st_transform(stacks, crs = 26915)


# Join Block group assignment
blkgrps_shp <- st_read("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/5. Block group polygons/Results/blockgroups_mnrisks_2014.shp") %>%
               select(geoid, geometry) %>%
               rename(bg_geoid = geoid) %>%
               filter(str_sub(bg_geoid, 1, 2) == "27")

blkgrps_shp <- st_transform(blkgrps_shp, crs = 26915)

stacks <- st_join(stacks, blkgrps_shp) %>%
          st_set_geometry(NULL) %>%
          mutate(bg_geoid = as.numeric(bg_geoid),
                 stack_id = NULL)

summary(stacks$bg_geoid)


# Clean names
names(stacks)

names(stacks) <- gsub("release", "rel", names(stacks))


# Load border facilities
bords <- readRDS(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/_development/1. Modeling parameters/1. Point sources/Border_state_facilities/borderstate_facility_parameters_grouped_with_BGs.Rdata")) %>%
         mutate(source_id    = as.character(source_id),
                rel_point_id = as.character(rel_point_id))

names(bords)

bords <- bords %>%
          mutate(height_m = coalesce(height_m, fug_height_m)) %>%
          select(-fug_height_m)

summary(bords$bg_geoid)


# Join all
stacks <- bind_rows(stacks, bords)


# Arrange columns
stacks <- stacks %>%
          rename(velocity_ms = velocity_m_s) %>%
          select(-flow_rate_m3_s) %>%
          select(source_name, source_id, source_type,
                 rel_point_id, rel_point_type, short_desc,
                 bg_geoid,
                 lat, long, height_m, diameter_m, velocity_ms, temp_k,
                 side_length, sy, sz)



#---------------------------------#
# Join railyard sources
#---------------------------------#
rail <- read.csv(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/4. Airports, rail yards/Railyards/railyards.csv"), stringsAsFactors = F) %>%
          rename(height_m       = building_ht,
                 side_length    = source_width) %>%
          select(-county, -geometry, -utm_x, -utm_y) %>%
          mutate(source_id      = as.character(source_id),
                 source_name    = paste0(tools::toTitleCase(tolower(source_name)), " Rail Yard"),
                 rel_point_id   = source_id, #"RELPT1",
                 source_type    = "VOLUME",
                 short_desc     = "Allocated rail yard",
                 rel_point_type = "Fugitive",
                 height_m       = 4,
                 side_length    = 50,
                 sy             = (side_length / 4.3) %>% round(1),
                 sz             = 1.8)


names(rail)

names(rail)[!names(rail) %in% names(stacks)]
names(stacks)[!names(stacks) %in% names(rail)]

stacks <- bind_rows(stacks, rail)


#---------------------------------------#
# Set fugitives to "Volume" source type
#---------------------------------------#
stacks <- stacks %>%
            rowwise() %>%
            mutate(source_type = ifelse(rel_point_type == "Fugitive", "VOLUME", source_type)) %>%
            mutate(source_type = ifelse(source_type == "ALLOC POINT", "POINT", source_type)) %>%
            rename(release_ht_meters = height_m)


#-----------------------------------------------#
# SAVE MN stacks, rail yards, and border stacks
#-----------------------------------------------#
saveRDS(stacks,
        paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/_development/1. Modeling parameters/1. Point sources/08_All_Cleaned_stack_params_with_final_coords_grouped_plus_railyards_because_trains.Rdata"))

