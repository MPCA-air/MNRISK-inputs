library(tidyverse)
library(measurements)
library(RODBC)

# Set inventory year
inv_year <- 2017


# Default SCC modeling params
## Notes: These have not been updated, but do not need to be updated. They haven't changed in the emissions inventory.
default_scc <- read.csv(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/qc_stack_params/2018_updated_default_release_pt_model_params.csv"), stringsAsFactors = F)


summary(default_scc$exit_gas_temperature)

# Convert to metric
default_scc <- default_scc %>%
                 mutate(height_m       = conv_unit(stack_height, "ft", "m"),
                        diameter_m     = conv_unit(stack_diameter, "ft", "m"),
                        temp_k         = conv_unit(exit_gas_temperature, "F", "K"),
                        exit_vel_ms    = conv_unit(exit_gas_velocity, "ft_per_sec", "m_per_sec"),
                        flow_rate_m3_s = conv_unit(exit_gas_flow_rate, "ft3_per_sec", "m3_per_sec"))

summary(default_scc$temp_k)


# Read modeling params
src_processes <- readRDS(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/01_2017_CEDR stack params.Rdata")) %>%
                 janitor::clean_names() %>%
                 mutate(source_id = as.character(source_id)) %>%
                 rename(rel_point_type = release_point_type)

names(src_processes)


#-- Group all processes at each release point
src_all <- src_processes %>%
            group_by(source_id, release_point_id) %>%
            mutate(n_process           = n(),
                   process_ids         = list(unique(process_id)),
                   process_rids        = list(unique(process_rid)),
                   flow_pcts           = list(flow_pct),
                   emission_unit_ids   = list(unique(emission_unit_id)),
                   emission_unit_rids  = list(unique(emission_unit_rid)),
                   scc_codes           = list(unique(scc_code)),
                   priority            = 3) %>%
            slice(1) %>%
            ungroup() %>%
            select(-scc_code, -process_rid, -process_id, -emission_unit_id, -emission_unit_rid, -flow_pct, -eu_desc, -county_fips, -naics_code)


# Clean source names
src_all$source_name <- gsub("[*]", "", src_all$source_name)

# Check temperatures
summary(src_all$exit_temp)

## Assume temps below 32 are in C, not Kelvin or F
src_all <- src_all %>%
           mutate(exit_temp = if_else(between(exit_temp, 0.1, 33), conv_unit(exit_temp, "C", "F"), exit_temp))


#---------------------------------#
# Drop dry cleaners
## Notes: We have not re-pulled the dry cleaners yet
#---------------------------------#
dryc <- read_csv(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1.5 Dry Cleaners/drycleaners_final.csv"))

src_all <- filter(src_all, !source_name %in% dryc$source_name)


#------------------------------------#
# Check for shutdown stacks
#------------------------------------#
unique(src_all$status_code_fac)

unique(src_all$status_code_stk)

unique(src_all$end_date_fac)

unique(src_all$end_date_stk)

shutdown <- src_all %>%
            filter((status_code_fac == "SHUTDOWN" | status_code_stk == "SHUTDOWN") &
                   ((is.na(end_date_stk) | end_date_stk < paste0(inv_year, "-01-01T05:00:00Z")) &
                    (end_date_fac < paste0(inv_year, "-01-01T05:00:00Z") | is.na(end_date_fac))))


# Drop shutdown stacks
src_all <- filter(src_all,
                  !paste(source_id, release_point_id) %in%
                   paste(shutdown$source_id, shutdown$release_point_id))


#----------------------------------------#
# Sources with no release points
#----------------------------------------#
no_stacks <- filter(src_all, is.na(rel_point_type))
## There are none for 2017 data set.


#----------------------------------------#
# Missing parameters
#----------------------------------------#
missing <- src_all %>%
           rowwise() %>%
           filter(tolower(rel_point_type) != "fugitive",
                  NA %in% c(height, diameter, exit_flow_rate, exit_temp))

rm(missing)



#---------------------------------------------------#
# Facilities with multiple unit types for Flow rate
#---------------------------------------------------#
multi_units <- src_all %>%
                 group_by(source_id) %>%
                 mutate(flow_units_count = length(unique(c(NA, exit_flow_rate_units)))) %>%
                 filter(flow_units_count > 2)

# Select Dura Supreme
multi_units <- filter(multi_units, grepl("Dura Supreme", source_name), exit_flow_rate > 0) %>%
               arrange(-exit_flow_rate) %>%
               select(-c(process_ids:scc_codes))

rm(multi_units)

#---------------------------------------------------#
# Auto body temperatures = human body?
## EI says correct
##37 facilities reported emissions exactly at body temperature
#---------------------------------------------------#
body_temp <- filter(src_all, exit_temp == 98.6) %>% select(-c(process_ids:scc_codes))

rm(body_temp)

#---------------------------------#
# Convert to metric
#---------------------------------#
src_all <- src_all %>%
           rowwise() %>%
           mutate(release_point_id     = tolower(release_point_id),
                  height_units         = tolower(height_units),
                  diameter_units       = tolower(diameter_units),
                  fr_units             = tolower(exit_flow_rate_units),
                  fr_units             = case_when(is.na(fr_units) ~ as.character(NA),
                                                   fr_units == "acfs" ~ "ft3_per_sec",
                                                   fr_units == "ft3/sec" ~ "ft3_per_sec",
                                                   fr_units == "acfm" ~ "ft3_per_min",
                                                   TRUE               ~ fr_units),
                  exit_temp_units      = str_replace_all(exit_temp_units, "FALSE", "F"),

                  height_m             = ifelse(!is.na(height_units),
                                                conv_unit(height, height_units, "m"),
                                                height) %>% round(3),
                  diameter_m           = ifelse(!is.na(diameter_units),
                                                conv_unit(diameter, diameter_units, "m"),
                                                diameter) %>% round(3),
                  flow_rate_m3_s       = ifelse(!is.na(fr_units),
                                                conv_unit(exit_flow_rate, fr_units, "m3_per_sec"),
                                                exit_flow_rate) %>% round(6),
                  exit_vel_ms          = (flow_rate_m3_s / (pi*(diameter_m/2)**2)) %>% round(4),
                  temp_k               = ifelse(!is.na(exit_temp_units) & exit_temp != 0,
                                                conv_unit(exit_temp, exit_temp_units, "K"),
                                                exit_temp) %>% round(1))

names(src_all)


# Check for Zeroes
zeroes <- src_all %>%
          rowwise() %>%
          filter(0 %in% c(height_m, diameter_m, exit_vel_ms, temp_k))


# Drop original unit columns
src_all <- src_all %>%
           select(-(height:exit_temp_units), -fr_units)


# Drop columns not needed until emission processing
names(src_all)

src_all <- src_all %>%
           select(-contains("status_"), -(comment:end_date_stk), -(short_desc_process:flow_pcts))


#----------------------------------------#
# Replace missing parameters with
# 80th percentile default values
#----------------------------------------#
missing <- src_all %>%
            filter(tolower(rel_point_type) != "fugitive",
                   NA %in% c(height_m, diameter_m, exit_vel_ms, temp_k))

# 80th pctile
default_scc %>% summarize_if(is.numeric, quantile, 0.2, na.rm = T)


# Insert new values for NA's
src_all <- src_all %>%
           replace_na(list(height_m     = 8.4,
                           diameter_m   = 0.4,
                           exit_vel_ms  = 2.2,
                           temp_k       = 297.3))


#---------------------------------#
# Join criteria modeling params
#---------------------------------#

# Load params from criteria modeling
## This has been updated
crit_params_all <- readRDS(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/02_criteria_mod_params.Rdata")) %>%
                   rename(lat_mod  = lat,
                          long_mod = long)


# Extra criteria facilities
crit_extra <- crit_params_all %>% filter(!source_id %in% src_all$source_id)

crit_extra %>% {unique(.$source_name)}


# Keep only facilities and release points in CEDR
crit_params <- crit_params_all %>%
               filter(!source_id %in% crit_extra$source_id) %>%
               mutate(stack_id = paste(source_id, release_point_id))


# Release point name mismatch
crit_mismatch <- crit_params %>%
                 filter(!stack_id %in% paste(src_all$source_id, src_all$release_point_id))

# Keep only facilities and release points in CEDR
crit_params <- crit_params %>%
               filter(stack_id %in% paste(src_all$source_id, src_all$release_point_id))


# Dropped facilities
crit_dropped <- crit_params_all %>%
                filter(!source_id %in% crit_params$source_id) %>%
                group_by(source_id) %>%
                slice(1)


# Convert flow rate to metric
crit_params <- crit_params %>%
               mutate(flow_rate_m3_s = conv_unit(m_flow_rate_acfm, "ft3_per_min", "m3_per_sec")) %>%
               select(-m_flow_rate_acfm,
                      -rotation_angle_deg,
                      -width_m,
                      -master_ai_id,
                      -model_id)

# Blank stack modeling inputs for volume sources
crit_vols <- crit_params %>%
             filter(source_type == "VOLUME") %>%
             mutate(height_m    = NA,
                    diameter_m  = NA,
                    exit_vel_ms = NA,
                    temp_k      = NA,
                    flow_rate_m3_s = NA,
                    rel_point_type = "FUGITIVE")

crit_params <- crit_params %>% filter(source_type != "VOLUME") %>%
               bind_rows(crit_vols)


# Blank volume modeling inputs for point sources
unique(crit_params$source_type)

crit_pts <- crit_params %>%
              filter(source_type %in% "POINT") %>%
              mutate(sy = NA,
                     sz = NA,
                     side_length = NA)

crit_params <- crit_params %>% filter(source_type != "POINT") %>%
               bind_rows(crit_pts)

# Drop UTM columns
crit_params <- select(crit_params, -contains("_utm"), -stack_id)


# Check temperatures
## Assume temps below freezing are in Fahrenheit, not Kelvin
summary(conv_unit(crit_params$temp_k, "K", "F"))


#----------------------------------------------#
# Reasonable input range
# Drop values beyond acceptable range
## Should we look at them again? -> Sent to Nick
#----------------------------------------------#
source(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/qc_stack_params/model_param_check.R"))


# Check for beyond values
bad_values <- crit_params %>%
              rowwise() %>%
              filter(height_m     != check_input(height_m, "height")   |
                     diameter_m   != check_input(diameter_m, "diameter") |
                     exit_vel_ms  != check_input(exit_vel_ms, "velocity") |
                     temp_k       != check_input(temp_k, "temp"))


# Set modeled inputs to NA if out of range
crit_params <- crit_params %>%
               rowwise() %>%
               mutate(height_m     = ifelse(height_m != check_input(height_m, "height"), NA, height_m),
                      diameter_m   = ifelse(diameter_m != check_input(diameter_m, "diameter"), NA, diameter_m),
                      exit_vel_ms  = ifelse(exit_vel_ms != check_input(exit_vel_ms, "velocity"), NA, exit_vel_ms),
                      temp_k       = ifelse(temp_k != check_input(temp_k, "temp"), NA, temp_k))


# Constrain default inputs to range
default_scc <- default_scc %>%
               rowwise() %>%
               mutate(height_m     = check_input(height_m, "height"),
                      diameter_m   = check_input(diameter_m, "diameter"),
                      exit_vel_ms  = check_input(exit_vel_ms, "velocity"),
                      temp_k       = check_input(temp_k, "temp"))


#---------------------------------#
# Match names
#---------------------------------#
# Check for mismatched column names
names(src_all)[!names(src_all) %in% names(crit_params)]
names(crit_params)[!names(crit_params) %in% names(src_all)]


crit_params <- crit_params %>%
               mutate(source_id = as.character(source_id)) %>%
               select(-source_name, -short_desc)

# Check source type change: Volume -> Vertical
type_change <- crit_params %>%
               left_join(src_all %>% select(source_id, release_point_id, source_type) %>% rename(source_type_2017 = source_type)) %>%
               filter(tolower(source_type) != tolower(source_type_2017))


# Join model params to CEDR data
new_params <- bind_rows(src_all, crit_params)


# Use the Criteria mod params if available
first_val <- function(x) {

  x[which(!is.na(x) & x != 0)[1]]

}

new_params <- new_params %>%
              select(-emission_unit_ids, -emission_unit_rids, -scc_codes) %>%
              group_by(source_id, release_point_id) %>%
              arrange(priority) %>%
              mutate_all(first_val) %>%
              slice(1)

# Re-join scc_codes
new_params <- new_params %>%
              left_join(src_all %>% select(source_id, release_point_id, scc_codes))


#--------------------------------------------#
# Set fugitive parameters as volume sources
#--------------------------------------------#

# Set as fugitive if description is fugitive
new_params<- new_params%>%
              rowwise() %>%
              mutate(rel_point_type = if_else(grepl("fugitive", tolower(short_desc)),
                                              "FUGITIVE",
                                              rel_point_type))

fugitives <- new_params%>%
              filter(tolower(source_type) == "volume" | tolower(rel_point_type) %in% c("fugitive")) %>%
              mutate(source_type    = "VOLUME",
                     rel_point_type = "FUGITIVE")


# Use 2.1M (7ft) for EI default release heights
fugitives  <- fugitives %>%
              mutate(height_m    = replace_na(height_m, 2.1) %>% round(1),
                     height_m    = ifelse(height_m == 8.4, 2.1, height_m),
                     diameter_m  = NA,
                     exit_vel_ms = NA,
                     temp_k      = NA,
                     flow_rate_m3_s = NA)


# Volume size if missing:
## Assume 13ft(4m) tall building, 20x20 meters footprint, release ht. at 7ft (on a building)
## Gas stations 1.8 meters, implies building ht of 13ft
fugitives <- fugitives %>%
             rowwise() %>%
             mutate(side_length   = replace_na(side_length, 20) %>% round(1),
                    sy            = replace_na(sy, side_length / 4.3) %>% round(1),
                    sz            = replace_na(sz, height_m / 2.15) %>% round())

# Rejoin
new_params<- bind_rows(filter(new_params, tolower(source_type) != "volume", !tolower(rel_point_type) %in% c("fugitive")),
                     fugitives)

new_params_unchanged <- new_params %>%
                        rowwise() %>%
                        mutate(priority = 2)


#---------------------------------------------------------#
# Reasonable input range
# Replace values beyond acceptable range w/ SCC defaults
#---------------------------------------------------------#

# Create temporary input check columns
new_params <- new_params %>%
              mutate(chk_height_m    = check_input(height_m, "height"),
                     chk_diameter_m  = check_input(diameter_m, "diameter"),
                     chk_exit_vel_ms = check_input(exit_vel_ms, "velocity"),
                     chk_temp_k      = check_input(temp_k, "temp"))

# Check for beyond values
bad_values <- new_params %>%
              rowwise() %>%
              filter(!grepl("emergency", tolower(short_desc)),
                     !grepl("fire pump", tolower(short_desc)),
                     rel_point_type != "Fugitive",
                     source_type  != "VOLUME",
                     height_m     != chk_height_m |
                     diameter_m   != chk_diameter_m |
                     exit_vel_ms  != chk_exit_vel_ms |
                     temp_k       != chk_temp_k) %>%
              mutate(scc_codes = paste(scc_codes, collapse = ", "))


# SAVE for review
write.csv(bad_values, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/qc_stack_params/03_bad_stack_values_for_review.csv", quote = T, row.names = F)


#--------------------------------------------------------#
# Replace NA and out of range values with SCC defaults if available,
# Otherwise use check_input default
#--------------------------------------------------------#
# Allow power plant stacks > 100 meters
first_val(c(NA, 50, 2))

new_params <- new_params %>%
              rowwise() %>%
              mutate(height_m     = ifelse(is.na(height_m) | height_m != max(height_m, chk_height_m, na.rm = T),
                                           first_val(c(filter(default_scc, scc_code %in% scc_codes)$height_m %>% median(na.rm = T), chk_height_m)),
                                           height_m),

                     diameter_m   = ifelse(is.na(diameter_m) | diameter_m != chk_diameter_m,
                                           first_val(c(filter(default_scc, scc_code %in% scc_codes)$diameter_m %>% median(na.rm = T), chk_diameter_m)),
                                           diameter_m),

                     exit_vel_ms = if_else(is.na(exit_vel_ms) | exit_vel_ms != chk_exit_vel_ms,
                                           first_val(c(filter(default_scc, scc_code %in% scc_codes)$exit_vel_ms %>% median(na.rm = T), chk_exit_vel_ms)),
                                           exit_vel_ms),
                     temp_k      = ifelse(is.na(temp_k) | temp_k != chk_temp_k,
                                          first_val(c(filter(default_scc, scc_code %in% scc_codes)$temp_k %>% median(na.rm = T), chk_temp_k)),
                                          temp_k))



# Review replaced values
bad_values_fixed <- new_params %>%
                    rowwise() %>%
                    filter(paste0(source_id, release_point_id) %in% paste0(bad_values$source_id, bad_values$release_point_id))



bad_values <- new_params %>%
              rowwise() %>%
              filter(!grepl("emergency", tolower(short_desc)),
                     !grepl("fire pump", tolower(short_desc)),
                     rel_point_type != "Fugitive",
                     source_type  != "VOLUME",
                     height_m     != check_input(height_m, "height")   |
                     diameter_m   != check_input(diameter_m, "diameter") |
                     exit_vel_ms  != check_input(exit_vel_ms, "velocity") |
                     temp_k       != check_input(temp_k, "temp"))


# Insert default 80th pctile values for NA's
new_params <- new_params %>%
              replace_na(list(height_m     = 8.4,
                              diameter_m   = 0.4,
                              exit_vel_ms  = 2.2,
                              temp_k       = 297.3))



#----------------------------------#
# Check for outliers
#----------------------------------#
if(F) {
  collapsed <- select(new_params, -(source_rid:source_type), -(rel_point_type:long)) %>%
    gather(input, value, height_m:temp_k, -(source_name:release_point_id))

  ggplot(collapsed, aes(x = input, y = as.numeric(value))) +
   geom_boxplot(aes(x = input), outlier.alpha = 0.2, outlier.color = "steelblue") +
   geom_jitter(alpha = 0.02) +
   facet_wrap(~input, scales = "free")

}


#------------------------------------------------#
# Set Downward and goose neck stacks to "Capped"
#------------------------------------------------#
new_params <- new_params %>%
              mutate(rel_point_type = if_else(rel_point_type %in% c("VERT W/RAIN CAP", "DOWNWARD VENT", "GOOSE NECK", "Downward", "Upwards with a cap on stack/vent"),
                                              "Capped",
                                              rel_point_type),
                     rel_point_type = gsub("Horizontally", "HORIZONTAL", rel_point_type),
                     rel_point_type = gsub("Upwards with no cap on stack/vent", "VERTICAL", rel_point_type),
                     rel_point_type = tolower(rel_point_type) %>% tools::toTitleCase())

unique(new_params$rel_point_type)


#-----------------------------------------------------#
# Label WWTPs, landfills, cremation, roaming asphalt
#-----------------------------------------------------#
new_params <- new_params %>%
              mutate(source_group = as.character(NA),
                     source_group = if_else(is.na(source_id) & grepl("50100701", short_desc), paste0("WWTP", source_rid), as.character(source_id)),
                     source_group = if_else(is.na(source_id) & grepl("50100410", short_desc), paste0("LANDFILL", source_rid), as.character(source_id)),
                     source_group = if_else(is.na(source_id) & grepl("31502101", short_desc), paste0("CREMATION", source_rid), as.character(source_id)))


#----------------------------------------#
# Check for missing parameters
#----------------------------------------#
missing <- new_params %>%
           filter(source_type != "VOLUME", rel_point_type != "Fugitive",
                  NA %in% c(height_m, diameter_m, exit_vel_ms, temp_k)) %>%
           mutate(priority = 2)


## Grab the original source parameters that have no alternative (ie default, etc.)
if(FALSE) {
  na_params <- bind_rows(new_params_unchanged, missing)

  na_params <- na_params %>%
                group_by(source_name, source_id, source_rid, release_point_id, release_point_rid) %>%
                mutate(count = n()) %>%
                filter(count > 1) %>%
                arrange(priority) %>%
                slice(1)

  new_params <- bind_rows(new_params, na_params %>% mutate(priority = 999))

  new_params <- new_params %>%
                group_by(source_name, source_id, source_rid, release_point_id, release_point_rid) %>%
                arrange(desc(priority)) %>%
                slice(1)

## test again for missing
missing <- new_params %>%
            filter(source_type != "VOLUME", rel_point_type != "Fugitive",
                   NA %in% c(height_m, diameter_m, exit_vel_ms, temp_k))

}


#---------------------------------#
# Save cleaned stack params
#---------------------------------#

# Convert list columns to text
# Drop original unit columns
new_params <- new_params %>%
              #mutate(emission_unit_ids   = paste(emission_unit_ids, collapse = ", "),
              #       emission_unit_rids  = paste(emission_unit_rids, collapse = ", ")) %>%
              #scc_codes           = paste(scc_codes, collapse = ", "))
              select(-scc_codes)


saveRDS(new_params,
          paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/03_Cleaned_stack_params.Rdata"))


#
