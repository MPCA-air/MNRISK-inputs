library(tidyverse)
library(readxl)
library(leaflet)
library(sf)

inv_year <- 2017


# Load stack coordinates
stacks <- readRDS(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/06_Cleaned_stack_params_with_final_coords_landfills_potws.Rdata"))


# Group stacks with identical release parameters and location
# Round model params to 1 decimal digit
# UTM coords to no decimal (Nearest 1 meter)
stacks <- stacks %>%
          rename(velocity_m_s = exit_vel_ms) %>%
          mutate_at(vars(height_m, diameter_m, velocity_m_s, temp_k, side_length, sy, sz), round, digits = 1) %>%
                    mutate_at(vars(x_utm, y_utm), round, digits = 0)

# Re-join SCCs
sccs <- readRDS(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/01_2017_CEDR stack params.Rdata")) %>%
        select(scc_code, source_id, release_point_id) %>%
        mutate(source_id        = as.character(source_id),
               release_point_id = tolower(release_point_id))

# 1 row per release_point / SCC combination
sccs <- sccs %>%
        filter(!is.na(scc_code)) %>%
        group_by(source_id, release_point_id) %>%
        slice(1)

# Join
## Check count for multiple SCCs per release point
stacks <- stacks %>%
          left_join(sccs) %>%
          group_by(source_id, release_point_id) %>%
          mutate(count = n())


## These are no longer in data set: flow_rate_m3_s, fug_height_m
all_stacks <- stacks  %>%
              group_by(source_id, scc_code, x_utm, y_utm, height_m, diameter_m, velocity_m_s, temp_k, side_length, sy, sz) %>%
              mutate(mnrisks_release_pt_id = release_point_id[1])


stacks_changed <- all_stacks %>%
                  filter(mnrisks_release_pt_id != release_point_id)


# Collapse identical stacks to single row (unless different SCC code)
stacks <- stacks  %>%
          group_by(source_id, scc_code, x_utm, y_utm, height_m, diameter_m, velocity_m_s, temp_k, side_length, sy, sz) %>%
          slice(1) %>%
          ungroup() %>%
          select(-scc_code)


#---------------------------------#
# SAVE updated coordinate stacks
#---------------------------------#
saveRDS(all_stacks,
        paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/07_CEDR stack IDs -  MNRISK IDs.Rdata"))

saveRDS(stacks,
        paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/1. Point sources/07_Cleaned_stack_params_with_final_coords_grouped.Rdata"))


# Leaflet map
if (FALSE) {
  # Quick QC View
  # Load MNRISKs Blockgroups   ####
  bgs <- st_read(paste0("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/", inv_year, " MNRISKS/", "_development/1. Modeling parameters/5. Block group polygons/Results/blockgroups_mnrisks_2014_v2.shp"))

  bgs$geoid <- as.character(bgs$geoid) %>% as.numeric()


  color_palette <- colorFactor(topo.colors(9), stacks$source_id)

  leaflet(bgs) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>%
    #addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(opacity = 0.80)) %>%
    addPolygons(weight = 1, color = "gray") %>%
    addCircleMarkers(data  = stacks,
                     lng   = ~long,
                     lat   = ~lat,
                     radius = 4,
                     color  = ~color_palette(source_id),
                     label  = ~paste(source_name, release_point_id, sep = ":   "))

}

##
