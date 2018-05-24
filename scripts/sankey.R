source("scripts/libraries.R")
source("scripts/data.R")

mediumC_palette <- c(
  "Artificial bare surfaces" = "#FF73DF",
  "Cropping/horticulture" = "#A80000",
  "Exotic forest" = "#6A3D9A",
  "Exotic grassland" = "#33A02C",
  "Exotic scrub/shrubland" = "#FFFF99",
  "Indigenous forest" = "#00441B",
  "Indigenous scrub/shrubland" = "#B15928",
  "Natural bare/lightly-vegetated surfaces" = "#FFFFFF",
  "Other herbaceous vegetation" = "#737300",
  "Tussock grassland" = "#FF7F00",
  "Urban area" = "#FB9A99",
  "Water bodies" = "#1F78B4")
  
firstup <- function(x) {
  #' Uppercase first character of each element in a character
  #' vector 
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

landcover_tidy <- landcover_data %>%
  gather(detailedClass_1996:broadClass_2012, key = 'level', value = 'class') %>%
  separate(level, sep = '_', into = c("level", "year")) %>% 
  mutate(level = str_extract(level, "^[a-z]+"),
         year = as.integer(year),
         class = firstup(class))

landcover_from <- landcover_tidy %>%
  filter(level == 'medium', year == 1996, geography == "Wellington") %>%
  mutate(from_class = class) %>% 
  select(-c(level, year, area_ha, class))

landcover_to <- landcover_tidy %>%
  filter(level == 'medium', year == 2012, geography == "Wellington") %>% 
  mutate(to_class = class) %>% 
  select(-c(level, year, class))

link_data <- full_join(landcover_from, landcover_to, by = c('uid', 'geography')) %>%
  group_by(from_class, to_class) %>%
  summarise(area_ha = sum(area_ha)) %>%
  ungroup() %>% 
  arrange(from_class) %>% 
  mutate(from_index = as.numeric(as.factor(from_class)) - 1) %>% 
  arrange(to_class) %>% 
  mutate(to_index   = as.numeric(as.factor(to_class)) + max(from_index)) %>% 
  mutate(from_index = as.integer(from_index),
         to_index   = as.integer(to_index)) %>% 
  arrange(from_class) %>% 
  as.data.frame()

node_data <- tibble(
  name = c(sort(unique(as.character(link_data$from_class))),
           sort(unique(as.character(link_data$to_class))))) %>% 
  mutate(id = seq(nrow(.)) - 1,
         colour = map_chr(name, function(x) mediumC_palette[x]),
         name = name) %>% 
  as.data.frame()

build_d3_palette <- function(names, colours) {
  #' convert a character vector of hex colours into a d3 scaleOrdinal palette
  name_list    <- jsonlite::toJSON(names)
  palette_list <- jsonlite::toJSON(colours)
  palette_text <- paste0('d3.scaleOrdinal()',
                         '.domain(', name_list,   ')',
                         '.range(', palette_list, ");")
  return(palette_text)
}

colour_data <- node_data %>% 
  select(-id) %>% 
  distinct() %>% 
  gather(name:colour, key = 'variable', value = 'value') %>% 
  split(.$variable) %>% 
  map(function(df) df$value)

sankeyNetwork(
  Links = link_data, Nodes = node_data, Source = 'from_index', Target = 'to_index', 
  Value = 'area_ha', NodeID = 'name', units = 'ha', LinkGroup = 'to_class',
  colourScale = JS(build_d3_palette(colour_data$name, colour_data$colour)))

cat(build_d3_palette(colour_data$name, colour_data$colour))











