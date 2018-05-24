energy <- structure(list(nodes = structure(list(name = structure(
  c(15L, 11L, 8L, 17L, 7L, 10L, 2L, 1L, 12L, 4L, 14L, 3L, 9L, 16L, 13L, 6L, 5L),
  .Label = c("Biomass", "Coal", "Commericial", "Electricity", "Energy Services",
             "Exports", "Geothermal", "Hydro", "Industrial", "Natural Gas", 
             "Nuclear", "Petroleum", "Rejected Energy", "Residential", "Solar",
             "Transportation", "Wind"), class = "factor")), .Names = "name",
  class = "data.frame", row.names = c(NA, -17L)), 
  links = structure(list(source = c(0L, 0L, 1L, 2L, 2L, 3L, 4L, 4L, 4L, 5L, 5L,
                                    5L, 5L, 5L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 8L, 8L, 8L, 8L, 8L, 9L, 9L, 9L, 9L, 9L, 9L, 10L, 10L, 11L, 11L, 
                                                                                                                                                                                                                                                                12L, 12L, 13L, 13L), target = c(9L, 10L, 9L, 9L, 12L, 9L, 9L, 
                                                                                                                                                                                                                                                                                                10L, 11L, 9L, 10L, 11L, 12L, 13L, 9L, 12L, 9L, 10L, 11L, 12L, 
                                                                                                                                                                                                                                                                                                13L, 9L, 10L, 11L, 12L, 13L, 10L, 11L, 12L, 13L, 14L, 15L, 14L, 
                                                                                                                                                                                                                                                                                                16L, 14L, 16L, 14L, 16L, 14L, 16L), value = c(0.25, 0.28, 8.34, 
                                                                                                                                                                                                                                                                                                                                              2.38, 0.01, 1.81, 0.16, 0.04, 0.02, 9.99, 4.75, 3.3, 9.36, 0.92, 
                                                                                                                                                                                                                                                                                                                                              14.3, 1.41, 0.52, 0.45, 0.13, 2.28, 1.35, 0.28, 0.98, 0.56, 8.2, 
                                                                                                                                                                                                                                                                                                                                              25.4, 4.78, 4.63, 3.27, 0.03, 25.4, 0.08, 3.95, 7.33, 3.05, 5.66, 
                                                                                                                                                                                                                                                                                                                                              4.91, 19.6, 21.9, 5.81), energy_type = structure(c(12L, 12L, 
                                                                                                                                                                                                                                                                                                                                                                                                 9L, 7L, 7L, 13L, 6L, 6L, 6L, 8L, 8L, 8L, 8L, 8L, 2L, 2L, 1L, 
                                                                                                                                                                                                                                                                                                                                                                                                 1L, 1L, 1L, 1L, 10L, 10L, 10L, 10L, 10L, 3L, 3L, 3L, 3L, 11L, 
                                                                                                                                                                                                                                                                                                                                                                                                 5L, 11L, 4L, 11L, 4L, 11L, 4L, 11L, 4L), .Label = c("Biomass", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Coal", "Electricity", "Energy Services", "Exports", "Geothermal", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Hydro", "Natural", "Nuclear", "Petroleum", "Rejected Energy", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Solar", "Wind"), class = "factor")), .Names = c("source", "target", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "value", "energy_type"), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   -40L))), .Names = c("nodes", "links")) 

library(networkD3)

sankeyNetwork(Links = energy$links, Nodes = energy$nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "Quads", LinkGroup = 'energy_type', colourScale = JS(
                
                'd3.scaleOrdinal()  
                .domain(["Solar","Nuclear","Hydro","Wind","Geothermal","Natural Gas","Coal","Biomass","Petroleum","Electricity","Residential","Commericial","Industrial","Transportation","Rejected Energy","Exports","Energy Services"])
                .range(["#FFFF00","#FF0000","#0000FF","#800080","#A52A2A","#00FFFF","#000000","#00FF00","#008000","#FFA500","#FAAFBE","#FAAFBE","#FAAFBE","#FAAFBE","#C0C0C0","#FFA500","#808080"])'
                
              ), 
              fontSize = 12, nodeWidth = 75, iterations = 100)

# putting in a data.frame might help see problems
color_scale <- data.frame(
  range = c("#FFFF00","#FF0000","#0000FF","#800080","#A52A2A","#00FFFF","#000000","#00FF00","#008000","#FFA500","#FAAFBE","#FAAFBE","#FAAFBE","#FAAFBE","#C0C0C0","#FFA500","#808080"),
  domain = c("Solar","Nuclear","Hydro","Wind","Geothermal","Natural Gas","Coal","Biomass","Petroleum","Electricity","Residential","Commericial","Industrial","Transportation","Rejected Energy","Exports","Energy Services"),
  nodes = energy$nodes,
  stringsAsFactors = FALSE
)

# once corrected color_scale can be used like this
sankeyNetwork(
  Links = energy$links, Nodes = energy$nodes, Source = "source",
  Target = "target", Value = "value", NodeID = "name",
  units = "Quads", LinkGroup = 'energy_type', colourScale = JS(
    sprintf(
      'd3.scaleOrdinal()
  .domain(%s)
  .range(%s)',
      jsonlite::toJSON(color_scale$domain),
      jsonlite::toJSON(color_scale$range)
    )
  ), 
  fontSize = 12, nodeWidth = 75, iterations = 100
)

as_tibble(energy$links)
