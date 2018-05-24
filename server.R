shinyServer(function(input, output, session) {
  
  #sankey diagrams
  output$sankey1_to_container <- renderUI({
    
    if(input$sankey1_class_type=="Detailed") class_choices <- pal_detailed$domain
    if(input$sankey1_class_type=="Medium") class_choices <- pal_medium$domain
    if(input$sankey1_class_type=="Broad") class_choices <- pal_broad$domain
    
    if(input$sankey1_class_type=="Detailed") selected_choices <- pal_detailed$domain
    if(input$sankey1_class_type=="Medium") selected_choices <- pal_medium$domain
    if(input$sankey1_class_type=="Broad") selected_choices <- pal_broad$domain
    
    selectInput(
      "sankey1_to_filter",
      label="To categories",
      choices=class_choices,
      selected=selected_choices,
      multiple=TRUE)
  })

  output$sankey1_from_container <- renderUI({
    
    if(input$sankey1_class_type=="Detailed") class_choices <- pal_detailed$domain
    if(input$sankey1_class_type=="Medium") class_choices <- pal_medium$domain
    if(input$sankey1_class_type=="Broad") class_choices <- pal_broad$domain
    
    if(input$sankey1_class_type=="Detailed") selected_choices <- pal_detailed$domain
    if(input$sankey1_class_type=="Medium") selected_choices <- pal_medium$domain
    if(input$sankey1_class_type=="Broad") selected_choices <- pal_broad$domain
    
    selectInput(
      "sankey1_from_filter",
      label="From categories",
      choices=class_choices,
      selected=selected_choices,
      multiple=TRUE)
  })

  sankey_data <- reactive({

    if(input$sankey1_class_type=="Detailed") from_year <- paste0("detailedClass_", input$sankey1_year[1])
    if(input$sankey1_class_type=="Detailed") to_year   <- paste0("detailedClass_", input$sankey1_year[2])
    if(input$sankey1_class_type=="Medium") from_year <- paste0("mediumClass_", input$sankey1_year[1])
    if(input$sankey1_class_type=="Medium") to_year   <- paste0("mediumClass_", input$sankey1_year[2])
    if(input$sankey1_class_type=="Broad") from_year <- paste0("broadClass_", input$sankey1_year[1])
    if(input$sankey1_class_type=="Broad") to_year   <- paste0("broadClass_", input$sankey1_year[2])
    
    sankey_selection <- landcover_wide %>% ungroup()

    if (input$sankey1_change) {
      from_col <- landcover_wide[, from_year][[1]]
      to_col   <- landcover_wide[, to_year][[1]]

      sankey_selection <-
        sankey_selection %>%
        filter(from_col != to_col)
    }

      sankey_selection <-
        sankey_selection %>%
        filter(geography == input$sankey1_geography)

    sankey_selection
  })

  output$sankey1 <- networkD3::renderSankeyNetwork({
    
    # get input values
    from_year <- input$sankey1_year[1]
    to_year <- input$sankey1_year[2]
    class_type <- input$sankey1_class_type
    
    get_class <- function(colnames, class_type, year) {
      colnames[
        str_detect(
          colnames, 
          pattern = sprintf(
            "%sClass_%d",
            str_to_lower(class_type),
            year))]
    }
    
    # get the 'from class', 'to class' and 'value column'
    from_class <- get_class(names(landcover_wide), class_type, from_year)
    to_class   <- get_class(names(landcover_wide), class_type, to_year)
    
    # ok to hard-code this for now
    value_col <- "area_ha"
    
    # filter by geography
    landcover_change <- landcover_wide %>% 
      filter(geography == input$sankey1_geography) %>% 
      select(from_class, to_class, value_col)
    
    # Select only requested classes
    landcover_change <- landcover_change %>% 
      filter(.[,from_class][[1]] %in% input$sankey1_from_filter,
             .[,to_class][[1]] %in% input$sankey1_to_filter)
    
    # if requested, filter change only and aggregate area_ha
    if (input$sankey1_change) {
      landcover_change <- landcover_change %>% 
        filter(.[, from_class] != .[, to_class]) %>% 
        group_by_at(vars(one_of(from_class, to_class))) %>% 
        summarise(area_ha = sum(area_ha)) %>% 
        ungroup()
    } else {
      landcover_change <- landcover_change %>% 
        group_by_at(vars(one_of(from_class, to_class))) %>% 
        summarise(area_ha = sum(area_ha)) %>% 
        ungroup()
    }
    
    # filter out small/large links
    if (any(input$sankey1_links != c(0, 1))) {
      landcover_change <- landcover_change %>% 
        arrange(area_ha) %>% 
        mutate(score = 1:nrow(.) / nrow(.)) %>% 
        filter(input$sankey1_links[1] < score,
               score < input$sankey1_links[2]) %>%
        select(-score) %>% 
        arrange_at(vars(from_class, to_class))
    }
    
    # rename column in colour_data to meet requirements of build_sankey_data
    colour_data <- colours %>% 
      rename(name = class_name)
    
    # get sankey_data
    sankey_data <- build_sankey_data(
      data = landcover_change, 
      colour_data = colour_data %>% 
        filter(class_type == input$sankey1_class_type))
    
    sankeyNetwork(Links = sankey_data$links, Nodes = sankey_data$nodes, Source = 'source_id', 
                  Target = 'target_id', Value = 'value', NodeID = 'name', colourScale = sankey_data$colour_scale,
                  fontSize = 14, fontFamily = "Arial", units = 'ha')
    
  })
  
  # iris %>% 
  #   as_tibble() %>% 
  #   ggvis(x = ~Sepal.Length, y = ~Petal.Width, fill = ~Species) %>% 
  #   layer_points() %>% 
  #   bind_shiny("ggvis", "ggvis_ui")
  
  # debug_data <- reactive({
  #   
  #   from_year <- input$sankey1_year[1]
  #   to_year <- input$sankey1_year[2]
  #   class_type <- input$sankey1_class_type
  #   
  #   from_class <- names(landcover_wide)[
  #     str_detect(
  #       names(landcover_wide), 
  #       pattern = sprintf(
  #         "%sClass_%d",
  #         str_to_lower(class_type),
  #         from_year))]
  #   
  #   to_class <- names(landcover_wide)[
  #     str_detect(
  #       names(landcover_wide), 
  #       pattern = sprintf(
  #         "%sClass_%d",
  #         str_to_lower(class_type),
  #         to_year))]
  #   
  #   value_col <- "area_ha"
  #   
  #   landcover_change <- landcover_wide %>% 
  #     filter(geography == input$sankey1_geography) %>% 
  #     select(from_class, to_class, value_col)
  #   
  #   if (input$sankey1_change) {
  #     landcover_change <- landcover_change %>% 
  #       filter(.[, from_class] != .[, to_class]) %>% 
  #       group_by_at(vars(one_of(from_class, to_class))) %>% 
  #       summarise(area_ha = sum(area_ha)) %>% 
  #       ungroup()
  #   }
  #   
  #   landcover_change <- landcover_change %>% 
  #     group_by_at(vars(one_of(from_class, to_class))) %>% 
  #     summarise(area_ha = sum(area_ha)) %>% 
  #     ungroup()
  #   
  #   if (any(input$sankey1_links != c(0, 1))) {
  #     landcover_change <- landcover_change %>% 
  #       arrange(desc(area_ha)) %>% 
  #       mutate(score = 1:nrow(.) / nrow(.)) %>% 
  #       filter(input$sankey1_links[1] < score,
  #              score < input$sankey1_links[2]) %>%
  #       select(-score) %>% 
  #       arrange_at(vars(from_class, to_class))
  #   }
  #   
  #   landcover_change %>% 
  #     filter(.[,from_class][[1]] %in% input$sankey1_from_filter,
  #            .[,to_class][[1]] %in% input$sankey1_to_filter)
  #   
  # })
  # 
  # output$debug_print <- renderPrint({
  #   input$sankey1_from_filter
  # })
  # 
  # output$debug_table <- renderDataTable({
  #   debug_data()
  # })

  # output$sankey1_title <- renderText({  #   if (input$sankey1_change == TRUE) {  #     my_title <- paste("Land cover change by class",  #                       paste0(input$sankey1_geography, " ", input$sankey1_year[1], "\u2013", input$sankey1_year[2]),  #                       sep="\n")  #   }  #   else if (input$sankey1_change == FALSE) my_title <-  #       paste("Land cover by class",  #             paste0(input$sankey1_geography, " ", input$sankey1_year[1], "\u2013", input$sankey1_year[2]),  #             sep="\n")  #   return(my_title)  # })  #
  
  # output$sankey1 <- networkD3::renderSankeyNetwork({
  # 
  #   sankeyNetwork(Links = links, Nodes = nodes,
  #                 Source = "source", Target = "target",
  #                 Value = "value", NodeID = "name",
  #                 fontSize= 12, nodeWidth = 30, iterations = 0)
  # 
  # })
# 
#   #tables
#   dt1 <- landcover_area_change
#   
#   output$table1 <- renderDataTable({
#     dt1 %>% 
#       DT::datatable(
#         filter="top",
#         rownames=F,
#         options=list(
#           autoWidth=T,
#           pageLength=10,
#           columnDefs=list(list(className='dt-center', targets=0:(ncol(dt1)-1)))
#         )
#       )
#   })
#   
#   output$table1_download <- downloadHandler(
#     filename=function() {"table.csv"},
#     content=function(file) {
#       write_csv(dt1, file, na="")
#     })
#   
#   dt2 <- landcover_area
#   
#   output$table2 <- renderDataTable({
#     dt2 %>% 
#       DT::datatable(
#         filter="top",
#         rownames=F,
#         options=list(
#           autoWidth=T,
#           pageLength=10,
#           columnDefs=list(list(className='dt-center', targets=0:(ncol(dt2)-1)))
#         )
#       )
#   })
# 
#   output$table2_download <- downloadHandler(
#     filename=function() {"table.csv"},
#     content=function(file) {
#       write_csv(dt2, file, na="")
#     })

  # output$table3 <- renderDataTable(
  #   landcover_conc,
  #   caption="Land cover concordance of detailed (i.e. lcdb), medium and broad classes",
  #   filter="top",
  #   rownames=F,
  #   options=list(
  #     autoWidth=T,
  #     pageLength=15,
  #     columnDefs=list(list(className='dt-center', targets=0:ncol(landcover_conc)-1))
  #   )
  # )
  # 
  # output$table3_download <- downloadHandler(
  #   filename=function() {"table.csv"},
  #   content=function(file) {
  #     write_csv(landcover_conc, file, na="")
  #   })
  
})