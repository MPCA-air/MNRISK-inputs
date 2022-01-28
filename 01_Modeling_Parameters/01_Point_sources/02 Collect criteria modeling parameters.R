library(tidyverse)
library(sf)
library(RODBC)
library(leaflet)
library(measurements)


# Set inventory year
inv_year <- 2017

#--------------------------------------------------------------------------#
# Load params for criteria modeling from exported TEMPO Tableau tool data
#--------------------------------------------------------------------------#

# Point sources
crit_params <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/Criteria modeled facilities/modeled_facilities_subject_items.csv", guess_max = 100000) %>%
                select(-`Default Title Desc`) %>%
                group_by(`Master Ai Id`) %>%
                mutate(`Master Ai Name` = `Master Ai Name`[1],
                       source_type = "POINT") %>%
                janitor::clean_names() %>%
                select(-contains("base"), -contains("rounded"), -contains("flag"), -contains("not_mod"), -contains("pollutant_avg"), -contains("source_mod"), -contains("_area"), -contains("_vol"), -contains("_subj_item"), -contains("emission"), -master_ai_id_agency_interest)



# Check temperatures
summary(crit_params$exit_temp_k)

summary(conv_unit(crit_params$exit_temp_k, "K", "F"))

## Assume temps below 40 are in C, not Kelvin or F
crit_params <- crit_params %>%
               mutate(exit_temp_k = if_else(between(exit_temp_k, 0.1, 40), conv_unit(exit_temp_k, "C", "K"), exit_temp_k))


## Assume temps below 281 and above 40 are in F, not Kelvin or C
crit_params <- crit_params %>%
               mutate(exit_temp_k = if_else(between(exit_temp_k, 0.1, 281), conv_unit(exit_temp_k, "F", "K"), exit_temp_k))


# Calculate effective diameter for rectangle release points
no_diam <- filter(crit_params, is.na(diameter_m))

crit_params <- crit_params %>%
               rowwise() %>%
               mutate(diameter_m = if_else(is.na(diameter_m), sqrt(length_m*width_m / pi) * 2, diameter_m))

# Still 1 problem facility
no_diam <- dplyr::filter(crit_params, is.na(diameter_m))



# Volume sources
vol_params <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/Criteria modeled facilities/Volume_Sources_data.csv", guess_max = 100000) %>%
              select(-`Default Title Desc`) %>%
              group_by(`Master Ai Id (Agency Interest)`) %>%
              mutate(`Master Ai Name` = `Facility`[1],
                     source_type = "VOLUME") %>%
              janitor::clean_names() %>%
              select(-facility_ai_id, -facility, -contains("base"), -contains("rounded"), -contains("flag"), -contains("not_mod"), -contains("pollutant_avg"), -contains("source_mod"), -contains("emission"), -x_y_utm, -contains("activity"), -contains("area"), -model_id) %>%
              rename(master_ai_id = master_ai_id_agency_interest)



# Add length and width
## Calculate height if needed
vol_params <- vol_params %>%
              mutate(width_m  = sigma_y * 4.3,
                     length_m = width_m,
                     height_m = if_else(is.na(height_m) | height_m == 0, sigma_z_m*2.15, height_m))

names(vol_params) <- str_remove_all(names(vol_params), "_vol")

# Check for mismatched columns
names(vol_params)[!names(vol_params) %in% names(crit_params)]
names(crit_params)[!names(crit_params) %in% names(vol_params)]


# Join
crit_params <- bind_rows(crit_params, vol_params)

#-------------------------------------------------------#
# Get most recent, non-zero, non-NA value for each column
# Average parameters for duplicate stacks
#-------------------------------------------------------#
## View(crit_params[, c("master_ai_id", "stack_id", "model_id")])

first_val <- function(x) {

       x[which(!is.na(x) & x != 0)[1]]

  }

# Check for most recent file missing parameters
central_qc <- filter(crit_params,
                     master_ai_name == "Central MN Renewables",
                     master_ai_id == "8059")

missing_new <- crit_params %>%
               group_by(master_ai_id, stack_id, model_id) %>%
               mutate(n = n(),
                      unique_d = sum(!is.na(diameter_m))) %>%
               filter(unique_d < n)

# Take the mean of multiple operating scenarios in the same activity
crit_params <- crit_params %>%
                group_by(master_ai_id, stack_id, model_id, activity) %>%
                mutate_if(is.numeric, mean, na.rm = T) %>%
                slice(1) %>%
                ungroup() %>%
                select(master_ai_id, stack_id, model_id, everything())


# Take most recent non-NA value from all activities
crit_params <- crit_params %>%
               group_by(master_ai_id, stack_id, model_id) %>%
               arrange(-activity_year, -activity_num) %>%
               mutate_if(is.numeric, first_val) %>%
               slice(1) %>%
               ungroup() %>%
               select(master_ai_id, stack_id, model_id, everything())


# Check again for success
central_collapsed <- filter(crit_params,
                            master_ai_name == "Central MN Renewables",
                            master_ai_id == "8059")

# Drop missing stack IDs
crit_params <- crit_params %>% filter(!is.na(stack_id) | !is.na(model_id))


# Find facilities with only 1 stack location
crit_params <- group_by(crit_params, master_ai_id) %>%
               mutate(n_coords = n_distinct(x_y_utm)) %>%
               ungroup()

crit_single_stack <- crit_params %>% filter(n_coords == 1)


# Drop facilities w/ only 1 location?
# crit_params <- crit_params %>% filter(n_coords > 1)

## Determine the columns needed
column_names_check <- readRDS("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/02_criteria_mod_params.Rdata")

names(column_names_check)

## Drop columns
crit_params <- crit_params %>%
               select(-contains("activity"), -n_coords, -contains("emission_rate"), -contains("pollutant"), -contains("operating_scenario"), -facility_ai_id, -exit_vel_m_s)

crit_params <- rename(crit_params,
                      em_unit_id  = stack_id,
                      source_name = master_ai_name,
                      temp_k      = exit_temp_k,
                      exit_vel_ms = exit_velocity_m_s)


# Clean release point ID
crit_params <- crit_params %>%
               rowwise() %>%
               mutate(model_id         = tolower(model_id),
                      release_point_id = if_else(length(str_split(model_id, "sv")[[1]]) > 1,
                                                 paste0("sv", str_split(model_id, "sv")[[1]][2]), substr(model_id, 1, 5)),
                      release_point_id = if_else(grepl("[a-z]", substr(release_point_id, 5, 5)), substr(release_point_id, 1, 4), release_point_id),
                      release_point_id = if_else(nchar(release_point_id) < 5,
                                                 paste0(substr(release_point_id, 1, 2),
                                                 "0",
                                                 substr(release_point_id, 3, 4)),
                                    release_point_id),
                      release_point_id = if_else(nchar(release_point_id) > 5,
                                                 substr(release_point_id, 1, 5),
                                                 release_point_id)) #str_split(model_id, "SV")[[1]][2])


unique(crit_params$release_point_id) %>% sort()


#---------------------------------#
# QC criteria modeling params
#---------------------------------#

# Fix Heartland swapped Temp & diameter
crit_params <- mutate(crit_params,
                      temp_k2    = if_else(master_ai_id == 743 & diameter_m > 100, diameter_m, temp_k),
                      diameter_m = if_else(master_ai_id == 743 & diameter_m > 100, temp_k, diameter_m),
                      temp_k     = temp_k2) %>%
               select(-temp_k2)


#---------------------------------#
# Get CEDR facility IDs
#---------------------------------#

## Connect to deltaw
credentials <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/_development/1. Modeling parameters/delta_credentials.csv")

deltaw <- odbcConnect("deltaw",
                      uid = credentials$delta_user,
                      pwd = credentials$delta_pwd,
                      believeNRows = FALSE)


# TEMPO-ID to Source-ID table
facility_ids <- sqlQuery(deltaw,
                         "SELECT * FROM TEMPO_MN_RAPIDS.ID_TRANSLATION_XREF",
                         stringsAsFactors = F) %>%
                janitor::clean_names()

facility_ids <- facility_ids %>% select(master_ai_id, preferred_id) %>% unique()


# Add missing IDs
# Vande Ag Enterprises - Site 1 (214558)
# Owatonna WWTP
# Georgia Pacific Wood Products LLC (2651)
# Riverview LLP - Riverview Dairy (56297)
# Hardrives Inc - Nonmetallic 2590
# Dairy Farmers
# Bituminous MSP
# Bitumionus IGH
# Austin Utilities - NE Power Station (437)
# Land O'Lakes Inc - Pine Island (4388)
# Melrose Dairy Proteins LLC (2657)
# Order of St Benedict/St John's Abbey (2251)
# Owatonna Public Utilities - West Owatonna (180)
# Waste Management Inc/Burnsville Landfill (7145)

facility_ids <- bind_rows(facility_ids,
                          tibble(master_ai_id = c(214558, 4844, 2651, 56297, 2590, 1520, 446, 445, 437, 4388, 2657, 2251, 180, 7145),
                                 preferred_id = c(2708300068, 2714700080, 2713700031, 2714900023, 2705301077, 2704900062, 2705300315, 2703700264, 2709900001, 2704900029, 2714500003, 2714500008, 2714700040, 2703700192)))


duplicated <- facility_ids %>% group_by(master_ai_id) %>% mutate(n = n()) %>% filter(n > 1)
# Mesabi Metallics 2929 - 06100067


# Join source IDs to modeling params
crit_params <- left_join(crit_params, facility_ids)

crit_params <- crit_params %>%
               select(source_name, master_ai_id, preferred_id, model_id, release_point_id, everything())


# Drop missing stack IDs and Remediation site
## Marshall Generation 228946 Canceled
## Ever Cat Fuels 127194 not in Inventory yet - this facility is in the 2017 emissions inventory
crit_params <- crit_params %>% filter(!master_ai_id %in% c(202300, 228946))

missing_fac_id   <- filter(crit_params, is.na(preferred_id))

crit_params <- crit_params %>%
               rowwise() %>%
               mutate(preferred_id = ifelse(source_name == "Ever Cat Fuels LLC", 2705900035, preferred_id))

missing_fac_id   <- filter(crit_params, is.na(preferred_id))

## There are two facilities with sources without modeling ids. Some of the sources have modeling ids, some do not. So, we can't use these source ids. No change needed when there are some with missing model_ids.
missing_stack_id <- filter(crit_params, is.na(model_id))

crit_params <- filter(crit_params, !is.na(model_id), !is.na(preferred_id))



# Check for duplicates
dupes <- crit_params %>%
         group_by(preferred_id, model_id) %>%
         mutate(count = n()) %>%
         filter(count > 1)


# Collapse duplicated stacks
crit_params <- crit_params %>%
               group_by(preferred_id, model_id) %>%
               mutate_if(is.numeric, mean, na.rm = T) %>%
               slice(1) %>%
               ungroup()


# Convert IDs to character
crit_params <- crit_params %>%
               rowwise() %>%
               mutate(source_id = as.character(preferred_id)) %>%
               select(-preferred_id)

#-------------------------------------------------------#
# Convert modeled UTM to Lat/Long
#-------------------------------------------------------#

# Select criteria modeled stacks
## Split XY Coords into 2 columns
mod_stacks <- filter(crit_params, !is.na(x_y_utm)) %>%
              separate(x_y_utm, c("x_utm", "y_utm"), sep = ",", remove = F, convert = T)

# Make numeric and fix double decimal at Green Plains Otter Tail
mod_stacks <- mod_stacks %>%
              mutate(x_utm = ifelse(x_utm == "259.089.424", "259089.424", x_utm),
                     x_utm = as.numeric(x_utm),
                     y_utm = as.numeric(y_utm))


# Add coordinate columns
mod_stacks$x  <- mod_stacks$x_utm
mod_stacks$y  <- mod_stacks$y_utm


# Create shapefile
mod_stacks <- st_as_sf(mod_stacks, coords = c("x", "y"))

plot(mod_stacks[ , 1])

st_crs(mod_stacks) <- 26915 #4326

mod_stacks <- mod_stacks %>% st_transform(4326)

plot(mod_stacks[ , 1])


# Add Lat/Long
mod_stacks$lat  <- st_coordinates(mod_stacks)[ , 2]
mod_stacks$long <- st_coordinates(mod_stacks)[ , 1]


# Convert to data frame
mod_stacks <- mod_stacks %>%
              st_set_geometry(NULL) %>%
              select(-c(x_y_utm, em_unit_id), -contains("subject_item"), -contains("coord_value"))


#-----------------------------------------------------#
# QC
## Fix American Crystal (2248) in Bayfield Wisconsin
## Dropped most recent activity: AIM20180001
#-----------------------------------------------------#
wisc_crystals <- mod_stacks %>% filter(master_ai_id == 2248 & x_utm > 665500)

mod_stacks <- mod_stacks %>% filter(master_ai_id != 2248 | x_utm < 665500)


# Average duplicate release points
mod_stacks <- mod_stacks %>%
              group_by(source_id, release_point_id) %>%
              mutate_if(is.numeric, mean, na.rm = T) %>%
              slice(1) %>%
              ungroup()


leaflet(mod_stacks) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(data  = mod_stacks,
             lng   = ~long,
             lat   = ~lat,
             label = ~paste(source_name, release_point_id, sep = ":   "))


#--------------------
# Select columns
#--------------------
names(mod_stacks)

# Set names to match MNRISKS
mod_stacks <- mod_stacks %>%
              rename(rel_point_type = discharge_dir_desc,
                     short_desc     = si_description,
                     sy             = sigma_y,
                     sz             = sigma_z_m,
                     side_length    = length_m) %>%
              mutate(priority = 1)


# Collapse
single_mod_stacks <- mod_stacks %>%
                     group_by(source_id, release_point_id) %>%
                     mutate_if(is.numeric, first_val) %>%
                     slice(1)

#-------------------------#
# SAVE modeling params
#-------------------------#
saveRDS(single_mod_stacks, paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/02_criteria_mod_params.Rdata"))


rm(list = ls()) # Clean house
