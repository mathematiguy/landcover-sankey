landcover_area <- readRDS("data/landcover_area.RDS")
landcover_area_change <- readRDS("data/landcover_area_change.RDS")
landcover_wide <- readRDS("data/landcover_wide.RDS") %>% 
  ungroup()
colours <- readRDS("data/landcover_colours.RDS")
