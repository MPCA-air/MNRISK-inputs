library(tidyverse)

inv_year <- 2017


#---------------------------------------#
# Load stack coordinates
#---------------------------------------#
stacks <- readRDS(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/06_Cleaned_stack_params_with_final_coords_landfills.Rdata"))


#---------------------------------------#
# Load QC'd POTW locations
#---------------------------------------#

# Load from Egg-hunt
potw <- read.csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.7 Water Treatment Plants (POTW)/potws_final.csv", stringsAsFactors = F) %>%
          rename(lat_new   = lat,
                 long_new  = long)


#---------------------------------------#
# Landfill checks
#---------------------------------------#

# Check landfills missing from 2017 emission sources
miss_potw <- filter(potw, !source_id %in% stacks$source_id)


# Filter to 2017 emission sources
potw     <- filter(potw, source_id %in% stacks$source_id)


# Select those that moved
movers <- left_join(potw,
                    stacks %>% select(source_id, release_point_id, lat, long),
                    by = "source_id") %>%
          filter(round(long, 5) != round(long_new, 5))


# Join new locations to stack release points
stacks <- left_join(stacks,
                    movers %>% select(source_id, lat_new, long_new),
                    by = "source_id")


#---------------------------------------#
# Update POTW release points
#---------------------------------------#
stacks <- stacks %>%
          mutate(potw          = grepl("POTW", source_id) & is.na(sy),
                 height_m      = ifelse(potw, 4, height_m),
                 lat           = ifelse(is.na(lat_new), lat, lat_new),
                 long          = ifelse(is.na(long_new), long, long_new),
                 x_utm         = ifelse(is.na(lat_new), x_utm, NA),
                 y_utm         = ifelse(is.na(lat_new), y_utm, NA)) %>%
          select(-lat_new, -long_new, -potw)


#---------------------------------------#
# SAVE w/ updated POTWs
#---------------------------------------#
saveRDS(stacks,
        paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/06_Cleaned_stack_params_with_final_coords_landfills_potws.Rdata"))

