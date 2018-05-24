library(tidyverse)
library(ggvis)

iris %>% 
  as_tibble() %>% 
  ggvis(x = ~Sepal.Length, y = ~Petal.Width, fill = ~Species) %>% 
  layer_points() %>% 
  bind_shiny("ggvis", "ggvis_ui")
