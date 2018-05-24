library(networkD3)

source("load_data.R")

space_char <- ' '

landcover_wide <- landcover_wide %>% 
  mutate_if(is.character, funs(str_replace_all(., " ", space_char)))

colour_data <- colours %>% 
  mutate_if(is.character, funs(str_replace_all(., " ", space_char))) %>% 
  rename(name = class_name)

build_sankey_data <- function(data, colour_data = NULL) {
  #' This is a generic function for preparing datasets to be input into
  #' the `networkD3::sankeyNetwork` function.
  #'
  #' The columns are assumed to be in the form of:
  #' var_1 -> var_2 -> ... -> var_n -> value
  #' where var_i is the variable for the ith layer in the sankey chart.
  #' 
  #' Assuming this order, `build_sankey_data` returns the Nodes and Links
  #' `data.frame`s for constructing the sankey chart correctly.
  
  # There is currently a bug in networkD3::sankeyNetwork which assigns
  # edge and vertex colours incorrectly if the regular space character
  # is used, so we replace it with a special character.
  space_char <- ' '
  
  data <- data %>% 
    mutate_if(is.character, funs(str_replace_all(., " ", space_char)))
  
  colour_data <- colour_data %>% 
    mutate_if(is.character, funs(str_replace_all(., " ", space_char)))
  
  # Get the variable name columns
  # Assumes that the values are stored in the last column
  gather_cols <- names(data)[1:(ncol(data) - 1)]
  value_col <- names(data)[ncol(data)]
  
  # Prepare the `node_data` by gathering `variable`/`name` combinations.
  # This is important because sometimes names are duplicated between
  # layers and we still need the ids to be distinct.
  #
  # Of course, `id`s are attached here too so we can inspect the table
  # later to make sure that the concordance is correct.
  node_data <- data %>% 
    select(one_of(gather_cols)) %>% 
    gather(one_of(gather_cols), key = 'variable', value = 'name') %>% 
    distinct() %>% 
    mutate(id = as.numeric(factor(paste(variable, name, sep = "//"))) - 1) %>% 
    arrange(id)
  
  # Prepare `link_data` - first we create a list of dataframes of the
  # form source -> target -> value. These are later concatenated to form
  # the `link_data` for `networkD3::sankeyNetwork`
  link_list <- lapply(1:(ncol(data) - 2), function(i) {
    # The columns of `data` are split into n - 2 dataframes here.
    # The matching looks like:
    # names(df1) = c('var_1', 'var_2', 'value')
    # names(df2) = c('var_2', 'var_3', 'value')
    # ...
    # names(df[n-2]) = c('var_[n-2]', 'var_[n-1]', 'value')
    #
    # Before outputting, the columns are renamed to 'source', 'target'
    # and 'value'. This way the tables can be concatenated with `bind_rows`.
    
    # Extract the column names for this loop
    link_cols   <- names(data)[c(i, i + 1, ncol(data))]
    source <- link_cols[1]
    target <- link_cols[2]
    value  <- link_cols[3]
    
    # Select the columns
    res <- data[, link_cols]
    
    # Rename the columns
    names(res) <- c('source', 'target', 'value')
    
    # Aggregate the `value`s by `source` and `target`.
    # Then attach the `variable` names for each `source` and `target`.
    res <- res %>%
      group_by(source, target) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>% 
      mutate(source_var = link_cols[1],
             target_var = link_cols[2])
    
    return(res)
    
  })
  
  # Bind the tables in `link_list` together, then join with `node_data` to get
  # the correct `id`s for each link. Then, reorder the columns nicely and convert
  # the `id`s to integers and `link_data` to `data.frame` to pass the type checking
  # in `networkD3::sankeyNetwork`.
  link_data <- link_list %>% 
    bind_rows() %>% 
    inner_join(node_data, by = c("source_var" = "variable", "source" = "name")) %>%
    rename(source_id = id) %>% 
    inner_join(node_data, by = c("target_var" = "variable", "target" = "name")) %>%
    rename(target_id = id) %>%
    select(source_var, target_var, source, target, source_id, target_id, value) %>% 
    mutate(source_id = as.integer(source_id),
           target_id = as.integer(target_id)) %>% 
    as.data.frame()
  
  # `node_data` must be a data.frame and the `name` column must be a factor.
  node_data <- node_data %>% 
    mutate(name = factor(name)) %>% 
    left_join(colour_data, by = c('name')) %>% 
    as.data.frame()
  
  build_d3_palette <- function(names, colours) {
    #' convert a character vector of hex colours into a d3 scaleOrdinal palette
    if (length(names) != length(colours)) {
      stop("names and colours lengths do not match")
    }
    name_list    <- jsonlite::toJSON(names)
    palette_list <- jsonlite::toJSON(colours)
    palette_text <- paste0('d3.scaleOrdinal()',
                           '.domain(', name_list,   ')',
                           '.range(', palette_list, ");")
    return(palette_text)
  }
  
  node_colours <- node_data %>% 
    select(name, colour) %>% 
    distinct()
  
  colour_scale <- build_d3_palette(node_colours$name, node_colours$colour)
  
  # package `node_data` and `link_data` together before return
  res <- list(nodes = node_data, links = link_data, colour_scale = JS(colour_scale))
  
  return(res)
  
}

sankey_data <- landcover_wide %>% 
  as_tibble() %>% 
  group_by(mediumClass_1996, mediumClass_2012) %>% 
  summarise(area_ha = sum(area_ha)) %>% 
  ungroup() %>% 
  build_sankey_data(colour_data = filter(colour_data, class_type == "Medium"))

sankeyNetwork(Links = sankey_data$links, Nodes = sankey_data$nodes, Source = 'source_id', 
              Target = 'target_id', Value = 'value', NodeID = 'name', colourScale = sankey_data$colour_scale,
              fontSize = 14, fontFamily = "Open Sans", LinkGroup = 'target', units = 'cases')

