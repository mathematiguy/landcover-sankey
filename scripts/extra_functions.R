theme_shiny_graph <- 
  function(x) {
    list(theme(
      plot.title = element_text(family = "Arial", face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(family = "Arial", size = 12, hjust = 0.5),
      text = element_text(family = "Arial", size = 12, face = "plain"),
      axis.text.x = element_text(family = "Arial", angle = 90, hjust = 1, vjust = 0.5, size = 12),
      axis.text.y = element_text(family = "Arial", hjust = 0.5, size = 12),
      axis.line = element_line(colour = "#d3d3d3"),
      strip.background = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_line(colour = "#d3d3d3", size = 0.25),
      panel.grid.minor = element_line(colour = "#d3d3d3", size = 0.25),
      strip.text = element_text(family = "Arial", size = 12),
      legend.title = element_blank(),
      legend.text = element_text(family = "Arial", size = 12),
      legend.key.width = unit(3, "mm"),
      legend.key.height = unit(10, "mm")

    ))
  }

str_to_sentence <- function(x){
  paste0(str_to_upper(str_sub(x, 1, 1)), str_to_lower(str_sub(x, 2)))
}

str_to_sentence_spaces <- function(x){
  str_replace_all(paste0(str_to_upper(str_sub(x, 1, 1)), str_to_lower(str_sub(x, 2))), "_", " ")
}

add_title <- function(vis, ..., properties=NULL, title="Plot Title") 
{
  merge.lists <- function(a, b) {
    a.names <- names(a)
    b.names <- names(b)
    m.names <- sort(unique(c(a.names, b.names)))
    sapply(m.names, function(i) {
      if (is.list(a[[i]]) & is.list(b[[i]])) merge.lists(a[[i]], b[[i]])
      else if (i %in% b.names) b[[i]]
      else a[[i]]
    }, simplify = FALSE)
  }
  default.props <- axis_props(
    title=list(font="Arial", fontSize=14),
    ticks = list(strokeWidth=0),
    axis = list(strokeWidth=0),
    labels = list(fontSize = 0),
    grid = list(strokeWidth=0)
  )
  axis.props <- do.call(axis_props, merge.lists(default.props, properties))
  vis <- scale_numeric(vis, "title", domain = c(0,1), range = 'width')
  axis <- ggvis:::create_axis('x', 'title', orient = "top",  title = title, properties = axis.props, ...)
  ggvis:::append_ggvis(vis, "axes", axis)
}

str_paste_plus_sign <- 
  function(x){
    ifelse(str_sub(x, 1, 1)=="-", x, paste0("+", x))
  }

build_sankey_data <- function(data, colour_data) {
  #' This is a generic function for preparing datasets to be input into
  #' the `networkD3::sankeyNetwork` function.
  #'
  #' The columns are assumed to be in the form of:
  #' var_1 -> var_2 -> ... -> var_n -> value
  #' where var_i is the variable for the ith layer in the sankey chart.
  #' 
  #' Assuming this order, `build_sankey_data` returns the Nodes and Links
  #' `data.frame`s for constructing the sankey chart correctly.
  
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
  
  colour_scale <- build_d3_palette(node_data$name, node_data$colour)
  
  # package `node_data` and `link_data` together before return
  res <- list(nodes = node_data, links = link_data, colour_scale = JS(colour_scale))
  
  return(res)
  
}