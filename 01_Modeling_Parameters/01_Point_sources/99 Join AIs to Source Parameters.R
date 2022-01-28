##This table makes the source parameter table for the previously modeled facilities.

library(tidyverse)

src_params <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/Criteria modeled facilities/Point_Sources_data.csv")

ais <- read_csv("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/Criteria modeled facilities/Activity_Details_data.csv") %>% select(`Master Ai Id`, `Master Ai Name`) %>% unique()

src_params <- left_join(src_params, ais, by = c("Facility" = "Master Ai Name")) %>%
              rename(`Master Ai Name` = Facility)

write_csv(src_params, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/Criteria modeled facilities/modeled_facilities_subject_items.csv")
