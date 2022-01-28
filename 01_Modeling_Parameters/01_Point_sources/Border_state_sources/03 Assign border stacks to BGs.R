library(tidyverse)
library(leaflet)
library(sf)


# Load all facilities w/in 20KM
bords <- readRDS("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/Border_state_facilities/borderstate_facility_parameters_grouped.Rdata")


bords <-  bords %>% mutate(long2 = long, lat2 = lat) %>%
           st_as_sf(coords = c("long2", "lat2"), crs = 4326)  #crs=26915 for UTM


# Count facilities
n_distinct(bords$facility_id)


# Load MNRISKs Blockgroups   ####
bgs <- st_read("X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/5. Block group polygons/Results/blockgroups_mnrisks_2014.shp")

bgs$geoid <- as.character(bgs$geoid) %>%
             as.numeric()

summary(bgs$geoid)

leaflet(bgs) %>%
      addTiles() %>%
      addPolygons(color  = ~ifelse(str_sub(geoid, 1, 2) == "27", "blue", "darkgray"),
                  weight = 2)

# Assign blockgroups
bg_int <- st_intersects(bords, bgs)

bg_int <- as.character(bg_int)

bords$bg_geoid <- bgs$geoid[as.numeric(bg_int)] %>%
                  as.character() %>%
                  as.numeric()

# Select sources missing a blockgroup
miss <- filter(bords, is.na(bg_geoid))


# Assigning lost sources to nearest BG
miss <- miss %>% st_transform(26915)


for (i in 1:nrow(miss)) {

  buffer_size <- 2000

  dist_i <- st_intersects(miss[i, ] %>% st_buffer(dist = buffer_size),
                          bgs %>% st_transform(26915))

while (length(unlist(dist_i)) < 1) {

  buffer_size <- buffer_size + 1000

  dist_i <- st_intersects(miss[i, ] %>% st_buffer(dist = buffer_size),
                          bgs %>% st_transform(26915))
}

print(paste0("#", i, ":  ", dist_i %>% unlist))

if (length(unlist(dist_i)) < 1) stop("No BG found!")

miss[i, "bg_geoid"] <- bgs[dist_i %>% unlist %>% .[1] %>% as.numeric, ]$geoid

}

summary(miss$bg_geoid)

# Combine nearest with matches
bords <- bords %>%
         filter(!is.na(bg_geoid)) %>%
         bind_rows(miss %>% st_set_geometry(NULL))


# Missing map
color_palette <- colorFactor(topo.colors(9), bords$facility_id)

leaflet(bgs) %>%
  addTiles() %>%
  addPolygons(color  = ~ifelse(str_sub(geoid, 1, 2) == "27", "blue", "tomato"),
              weight = 2) %>%
  addCircleMarkers(data  = bords,
                   lng   = ~long,
                   lat   = ~lat,
                   color = ~ifelse(is.na(bg_geoid), "tomato", "black"),
                   radius = ~ifelse(is.na(bg_geoid), 5, 2),
                   label = ~paste(facility_name, rel_point_id, sep = ":   "),
                   #popup = ~paste(facility_name, rel_point_id, sep = ":   "),
                   weight = 2)


#-------------------------------#
# Check missing
#-------------------------------#
bords %>% filter(is.na(bg_geoid)) %>% nrow

#-------------------------------#
# Clean names
#-------------------------------#
bords <- bords %>%
           st_set_geometry(NULL) %>%
           rename_all(tolower)

names(bords) <- gsub("facility", "source", names(bords)) %>%
                gsub("stk", "", .)

bords <- bords %>%
           rename(x_utm             = utm_x,
                  y_utm             = utm_y,
                  height_m          = hgt,
                  diameter_m        = diam,
                  flow_rate_m3_s    = flow,
                  velocity_m_s      = vel,
                  temp_k            = temp,
                  fug_height_m      = fug_height,
                  scc_code          = scc) %>%
            mutate(source_type      = "POINT",
                   rel_point_type   = ifelse(is.na(fug_height_m), "Vertical", "Fugitive"),
                   short_desc       = "Out of state facility") %>%
            select(source_name,
                   source_id,
                   source_type,
                   scc_code,
                   rel_point_id,
                   rel_point_type,
                   short_desc,
                   bg_geoid,
                   lat,
                   long,
                   x_utm,
                   y_utm,
                   height_m,
                   diameter_m,
                   flow_rate_m3_s,
                   velocity_m_s,
                   temp_k,
                   fug_height_m,
                   side_length,
                   sy,
                   sz)

#-------------------------------#
# SAVE Border stacks
#-------------------------------#
saveRDS(bords, "X:/Agency_Files/Outcomes/Risk_Eval_Air_Mod/_Air_Risk_Evaluation/MNRISKS/2017 MNRISKS/_development/1. Modeling parameters/1. Point sources/Border_state_facilities/borderstate_facility_parameters_grouped_with_BGs.Rdata")


##
