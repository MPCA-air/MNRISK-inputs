library(leaflet)
library(sf)
library(tidyverse)
library(measurements)

inv_year <- 2017

# Load all facilities w/in 20KM
bords <- readRDS("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/Border_state_facilities/borderstate_facility_parameters.Rdata")

#------------------------------------#
# Reduce to 1 record per release pt per scc
#------------------------------------#
bords <- bords %>%
         group_by(region_cd, facility_id, rel_point_id, scc) %>%
         slice(1)

#---------------------------------#
# Convert to metric
#---------------------------------#
bords <- bords %>%
         ungroup() %>%
         mutate(stkhgt          = conv_unit(stkhgt, "ft", "m") %>% round(1),

                stkdiam         = conv_unit(stkdiam, "ft", "m") %>% round(1),

                stkvel          = conv_unit(stktemp, "ft_per_sec", "m_per_sec") %>% round(1),

                stktemp         = conv_unit(stktemp, "F", "K") %>% round(1))


#--------------------------------------------------------#
# Replace values out-of-range with SCC defaults
#--------------------------------------------------------#
source("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/qc_stack_params/model_param_check.R")

# Default SCC modeling params
default_scc <- read.csv(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/qc_stack_params/2018_updated_default_release_pt_model_params.csv"), stringsAsFactors = F)


# Convert to metric
default_scc <- default_scc %>%
                mutate(height_m       = conv_unit(stack_height, "ft", "m"),
                       diameter_m     = conv_unit(stack_diameter, "ft", "m"),
                       temp_k         = conv_unit(exit_gas_temperature, "F", "K"),
                       velocity_m_s   = conv_unit(exit_gas_velocity, "ft_per_sec", "m_per_sec"),
                       flow_rate_m3_s = conv_unit(exit_gas_flow_rate, "ft3_per_sec", "m3_per_sec"),
                       scc = as.numeric(scc_code))

## Join defaults
#bords <- left_join(bords, default_scc, by = c("scc"))

# Replace out of range values with SCC defaults
bords <- bords %>%
         rowwise() %>%
         mutate(stkhgt  = if_else(!is.na(stkhgt) & stkhgt != check_input(stkhgt, "height"),
                                 filter(default_scc, scc_code %in% scc)$height_m %>% mean(na.rm = T),
                                 stkhgt),

                stkdiam = if_else(!is.na(stkdiam) & stkdiam != check_input(stkdiam, "diameter"),
                                 filter(default_scc, scc_code %in% scc)$diameter_m %>% mean(na.rm = T),
                                 stkdiam),

                stkvel  = if_else(!is.na(stkvel) & stkvel != check_input(stkvel, "velocity"),
                                  filter(default_scc, scc_code %in% scc)$velocity_m_s %>% mean(na.rm = T),
                                  stkvel),

                stktemp = if_else(!is.na(stktemp) & stktemp != check_input(stktemp, "temp"),
                                 filter(default_scc, scc_code %in% scc)$temp_k %>% mean(na.rm = T),
                                 stktemp))

#-------------------------------------#
# Reduce to 1 record per release pt
# Average stack parameters across SCCs
#-------------------------------------#
bords <- bords %>%
         group_by(region_cd, facility_id, rel_point_id) %>%
         mutate(across(stkhgt:stkvel, mean, na.rm = T),
                n = n()) %>%
         slice(1)


# Set sources as "fugitive" if missing any stack parameters
bords <- bords %>%
         mutate(missing_params = sum(is.na(c(stktemp, stkvel, stkdiam, stkhgt))),
                fugitive = missing_params > 0)


# Blank stack modeling inputs for volume sources
bords <- bords %>%
         mutate(stkhgt       = ifelse(fugitive, NA, stkhgt),
                stkdiam      = ifelse(fugitive, NA, stkdiam),
                stkflow      = ifelse(fugitive, NA, stkflow),
                stkvel       = ifelse(fugitive, NA, stkvel),
                stktemp      = ifelse(fugitive, NA, stktemp),
                side_length  = ifelse(!fugitive, NA, 20),
                fug_height   = ifelse(!fugitive, NA, 2.1),
                sy           = ifelse(!fugitive, NA, 5),
                sz           = ifelse(!fugitive, NA, 1.8)) %>%
          select(-fugitive)


fugis  <- filter(bords, is.na(stkhgt))

verts  <- filter(bords, !is.na(stkhgt))


# Group stacks with identical release parameters and location
# Round modeling params to 1 decimal digit
bords <- bords %>%
           mutate(across(c(stkhgt, stkdiam, stkvel, stktemp, side_length, fug_height, sy, sz), round, digits = 1)) %>%
           mutate(across(c(latitude, longitude), round, digits = 4)) %>%
           group_by(facility_id, latitude, longitude, stkhgt, stkdiam, stktemp, stkvel, fug_height) %>%
           slice(1) %>%
           ungroup() %>%
           select(-latitude, -longitude)


# SAVE
saveRDS(bords, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/Border_state_facilities/borderstate_facility_parameters_grouped.Rdata")
