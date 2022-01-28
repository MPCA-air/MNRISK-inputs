library(tidyverse)

## Context
# We are reviewing the modeling parameters for stacks/release points in the *2017* Emissions Inventory. When cleared for launch they will be submitted for modeling to generate statewide pollution estimates and packaged as _MNRISKS 2017_.


## Data
# The data below was retrieved from CEDR on December 18, 2020.
data <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/qc_stack_params/2020 - CEDR default SCC stack params.csv")


## Input range
summary(data)


#----------------------------------------------#
# Reasonable input range
#----------------------------------------------#
source("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/qc_stack_params/model_param_check.R")


input_range_metric

input_range

#----------------------------------------------#
# Label unreasonable inputs
#----------------------------------------------#
data <- rename_all(data, tolower)

names(data) <- gsub("_amt", "", names(data))


out_of_range <- data %>%
                rowwise() %>%
                mutate(height_chk   = stack_height      == check_input(stack_height, "height", metric = F),
                       diameter_chk = stack_diameter    == check_input(stack_diameter, "diameter", metric = F),
                       velocity_chk = exit_gas_velocity == check_input(exit_gas_velocity, "velocity", metric = F),
                       temp_chk     = exit_gas_temperature == check_input(exit_gas_temperature, "temp", metric = F))


out_of_range <- filter(out_of_range, !height_chk | !diameter_chk | !velocity_chk | !temp_chk)


#----------------------------------------------#
# Adjust unreasonable inputs
#----------------------------------------------#
data <- data %>%
          rowwise() %>%
          mutate(stack_height          = check_input(stack_height, "height", metric = F),
                 stack_diameter        = check_input(stack_diameter, "diameter", metric = F),
                 exit_gas_velocity     = check_input(exit_gas_velocity, "velocity", metric = F),
                 exit_gas_temperature  = check_input(exit_gas_temperature, "temp", metric = F),
                 exit_gas_flow_rate    = exit_gas_velocity * pi * (stack_diameter/2)**2,
                 out_of_range          = scc_code %in% out_of_range$scc_code) %>%
           arrange(desc(out_of_range)) %>%
           select(-out_of_range)


#---------------------------------#
# Save updated table
#---------------------------------#
write.csv(data, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/qc_stack_params/2020_updated_default_release_pt_model_params.csv", row.names = F, quote = T)



##
