library(tidyverse)
library(RColorBrewer)
library(networkD3)

#source Caleb's function 
source("build_sankey_data_fun.R")

#simple practice data
#data tibble requires categorical variables with final column of numeric value
get_data <- 
  tibble::tribble(~state1, ~state2, ~value,
                  "a 1", "a 2", 5,
                  "a 1", "c", 2,
                  "a 2", "a 1", 1,
                  "a 2", "c", 6,
                  "c", "a 1", 3,
                  "c", "a 2", 5
  )

#colour tibble requires 
#column called `name` of categorical variable values and 
#column called `colour` with hex values
get_colour <- 
  tibble::tribble(~name, ~colour, 
                  "a 1", "red",
                  "a 2", "orange",
                  "c", "yellow"
  )

sankey_data <- build_sankey_data(
  data=get_data, 
  colour_data=get_colour)

units <- "units"

sankeyNetwork(Links=sankey_data$links, Nodes=sankey_data$nodes, 
              Source="source_id", Target="target_id", Value="value", 
              NodeID="name", colourScale=sankey_data$colour_scale,
              fontSize=14, fontFamily="Arial", units=units
              )

#more complex landcover example

landcover_wide <- readRDS("landcover_wide.RDS") %>% ungroup()
colours <- readRDS("landcover_colours.RDS")
input_geography <- "Otago"
input_class_type <- "Medium" 
input_period <- c("mediumClass_1996", "mediumClass_2001", "mediumClass_2012")
input_from <- c("Tussock grassland")
input_to <- c("Exotic forest", "Exotic grassland") 

get_data <- 
  landcover_wide %>% 
  filter(geography==input_geography) %>% 
  group_by_at(input_period) %>% 
  summarise(area_ha=sum(area_ha)) %>% 
  ungroup() %>% 
  filter(mediumClass_1996==input_from) %>% 
  filter(mediumClass_2012==input_to) 

get_colour <- colours %>% 
  filter(class_type==input_class_type) %>% 
  rename(name=class_name)  

sankey_data <- build_sankey_data(
  data=get_data, 
  colour_data=get_colour)

units <- "ha"

sankeyNetwork(Links=sankey_data$links, Nodes=sankey_data$nodes, 
              Source="source_id", Target="target_id", Value="value", 
              NodeID="name", colourScale=sankey_data$colour_scale,
              fontSize=14, fontFamily="Arial", units=units)
