shinyUI(fluidPage(
  tags$head(includeCSS("www/style.css")),
  tags$head(tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js", type="text/javascript")), #https://www.cultureofinsight.com/blog/2018/03/15/2018-03-15-responsive-iframes-for-shiny-apps/
  navbarPage(
    collapsible=T,
    title=HTML("<b>Land cover</b>"), 
    windowTitle="Land cover",
    tabPanel(
      "Flowcharts",
      icon=icon("exchange"),
      sidebarLayout(
        sidebarPanel(width=4,
           shinyWidgets::materialSwitch(
             "sankey1_change",
             label=tags$b("Change only"),
             value=TRUE,
             status="primary"
           ),
           selectInput(
             "sankey1_geography",
             "Geography",
             geography_names,
             selected="New Zealand"
           ),
           sliderTextInput(
             "sankey1_year",
             label="Year range",
             selected=c("1996", "2012"),
             grid=FALSE,
             force_edges=TRUE,
             choices=c("1996", "2001", "2008", "2012"),
             width="50%"
           ),
           radioButtons("sankey1_class_type", "Class type", choices=c("Broad", "Medium", "Detailed"), selected="Medium"),
           sliderInput(
             "sankey1_links",
             label="Links range",
             min=0,
             max=1,
             step=0.1,
             value=c(0, 1),
             width="50%"
           ),
           uiOutput("sankey1_from_container"),
           uiOutput("sankey1_to_container")
           # uiOutput("ggvis_ui")
        ),
        mainPanel(width=8,
                  textOutput("sankey1_title"),
                  networkD3::sankeyNetworkOutput("sankey1", width="100%")
                  # ggvisOutput("ggvis")
                  # verbatimTextOutput("debug_print"),
                  # dataTableOutput("debug_table")
        )
      )
    ),
    #tabPanel("About", icon=icon("info-circle"), includeMarkdown("www/about.Rmd"))
    tags$div(HTML('<div data-iframe-height></div>'))
  )
))