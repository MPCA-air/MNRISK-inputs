library(tidyverse)

inv_year <- 2017


#---------------------------------------#
# Load stack coordinates
#---------------------------------------#
stacks <- readRDS(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/05_Cleaned_stack_params_with_final_coords.Rdata"))


#---------------------------------------#
# Load QC'd landfill locations
#---------------------------------------#

# Load from Egg-hunt
waste_qc <- read.csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.6 Landfills/landfills_final.csv", stringsAsFactors = F) %>%
              mutate(lat_new = lat,
                     long_new = long) %>%
              mutate_at(vars(long_new, lat_new), as.numeric) %>%
              filter(lat_new != round(lat, 5)) %>%
              select(-lat, -long)

waste <- read.csv(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.6 Landfills/landfills_final.csv"), stringsAsFactors = F) %>%
         rename(SOURCE_RID = source_id,
                x_utm_new  = utm_x,
                y_utm_new  = utm_y)

waste_ids <- read.csv(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1.6 Landfills/landfill_coords_check.csv"), stringsAsFactors = F) %>%
             rename(source_id = entity_id) %>%
             select(source_id)


# Join source IDs
waste <- left_join(waste_qc, waste_ids, by = c("source_id"))



#---------------------------------------#
# Landfill checks
#---------------------------------------#

# Check landfills missing from 2017 emission sources
miss_waste  <- filter(waste, !source_id %in% stacks$source_id)


# Filter to 2017 emission sources
waste  <- filter(waste, source_id %in% stacks$source_id)


# Join default locations
waste_movers <- left_join(waste, stacks %>% select(source_id, release_point_id, lat, long), by = "source_id")


# Select those that moved
waste_movers <- filter(waste_movers, round(long, 5) != long_new)


# Join new locations to stack release points
stacks <- left_join(stacks, waste %>% select(source_id, lat_new, long_new), by = "source_id")


#---------------------------------------#
# Update landfill release points
#---------------------------------------#
stacks <- stacks %>%
            mutate(landfill      = grepl("SW", source_id) & !is.na(sy),
                   lat           = ifelse(is.na(lat_new), lat, lat_new),
                   long          = ifelse(is.na(long_new), long, long_new),
                   x_utm         = ifelse(is.na(lat_new), x_utm, NA),
                   y_utm         = ifelse(is.na(lat_new), y_utm, NA),
                   side_length   = ifelse(landfill, 500, side_length ),
                   #fug_height_m  = ifelse(landfill, 2, fug_height_m),
                   sy            = ifelse(landfill, 120, sy),
                   sz            = ifelse(landfill, 1, sz)) %>%
            select(-lat_new, -long_new, -landfill)



#---------------------------------------#
# SAVE w/ updated landfills
#---------------------------------------#
saveRDS(stacks,
        paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/06_Cleaned_stack_params_with_final_coords_landfills.Rdata"))

gc()
