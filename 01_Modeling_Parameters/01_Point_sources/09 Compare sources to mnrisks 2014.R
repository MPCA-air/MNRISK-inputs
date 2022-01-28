library(tidyverse)
library(readxl)
library(sf)

inv_year <- 2017


# Load stack coordinates
stacks <- read.csv(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/08_All_Cleaned_stack_params_with_final_coords_grouped_plus_railyards_because_trains.csv"))


# Load 2014 emissions
emits_old <- read.csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/_development/5. Sent to Lakes/Emissions/facilities_emissions.csv", stringsAsFactors = F) %>%
         rename_all(tolower)


# Select missing facilities
miss <- emits_old %>% filter(!source_id %in% stacks$source_id) %>% group_by(source_id) %>% slice(1)


# Join names
names_old <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/_development/5. Sent to Lakes/Source parameters/facilities.csv") %>%
             rename_all(tolower)

##this changes the source name to be named source_id. why?
#names(names_old)[1] <- "source_id"

names_old %<>% group_by(source_id) %>%
               slice(1) %>%
               ungroup() %>%
               #mutate(source_id = as.numeric(source_id)) %>%
               dplyr::select(source_id, source_name)

miss <- left_join(miss, names_old) %>% dplyr::select(source_name, everything())


#------------------------------------#
# SAVE missing facilities
#------------------------------------#
write_csv(miss, paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/09_missing_facilities.csv"))

##
