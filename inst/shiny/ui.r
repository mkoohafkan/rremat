shinyUI(navbarPage("Habitat Blueprint",
  # explanation of project and interface
  tabPanel("Description",
    tabsetPanel("stuff",
    tabPanel("welcome",
      includeMarkdown("welcome.md")
    ),
    tabPanel("available data",
      includeMarkdown("data.md")
    ),
    tabPanel("habitat definitions",
      withMathJax(),
      includeCSS("table.css"),
      includeMarkdown("habitat.md")
    ),
    tabPanel("technical details",
      includeMarkdown("technical.md")
    )
  )
  ),
  # explore single transect
  tabPanel("Explore Transect",
    sidebarLayout(
      sidebarPanel(
        uiOutput("transect_date"),
        plotOutput("transect_flows"),
        plotOutput("transect_tides"),
        plotOutput("transect_wll")
      ),
      mainPanel(
        h1("Overall Habitat"),
        plotOutput("grid_plot"),
        fluidRow(
          column(6, 
            plotOutput("category_bar")
          ),
          column(6,
            plotOutput("depth_cat")
          )
        )#,
#        h1("Depth Habitat"),
#        fluidRow(
#          column(6,
#            plotOutput("depth_plot")
#          ),
#          column(6,
#            plotOutput("depth_vol")
#          )
#        )
      )
    )
  ),
  
  # explore period (e.g. single closure
  tabPanel("Explore Period",
    sidebarLayout(
      sidebarPanel(
        uiOutput("period"),
        selectInput("plot_type", "plot type", choices = c("stacked area", 
          "stacked bar"), selected = "stacked bar"),
        plotOutput("period_flows"),
        plotOutput("period_tides"),
        plotOutput("period_wll")
      ),
      mainPanel(
        h1("Overall Habitat"),
        plotOutput("period_overall"),
        plotOutput("period_bydepth")        
      )
    )
  )
))
  
