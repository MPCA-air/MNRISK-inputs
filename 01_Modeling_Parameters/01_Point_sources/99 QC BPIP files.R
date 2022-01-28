library(tidyverse)
library(readxl)
library(sf)

inv_year <- 2017


# Load stack coordinates
stacks <- readRDS(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/_development/1. Modeling parameters/1. Point sources/07-5_Cleaned_stack_params_with_final_coords_grouped.Rdata"))


#---------------------------------#
# Check BPIP facilities exist
#---------------------------------#
bpip_sources <- data_frame(source_id = list.files("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2014 MNRISKS/2014_mnrisks_development/1. Modeling parameters/1. Point sources/BPIP files", pattern = ".bpi") %>% gsub("[.]bpi", "", .))

miss_bpip <- filter(bpip_sources, !source_id %in% stacks$source_id)

# Verso(#2700900011) closed; Wausau closed, Georgia-Pacific Hardboard




