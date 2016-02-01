# ui.R

shinyUI(pageWithSidebar(
  headerPanel("Runoff prediction for Qilian and Zhamashike"),
  sidebarPanel(
    # Sidebar panels are conditional to the tabs in the main panel.
    # Maps.
    conditionalPanel(
      condition="input.conditionedPanels==1", 
      selectInput(
        "map_data",
        label = "Choose map to display",
        choices = c("Topography"),
        selected = "Topography"
      )
    ),
    # Time series data.
    conditionalPanel(
      condition="input.conditionedPanels==2",
      selectInput(
        "data", 
        label = "Choose data to display",
        choices = c(#"Precipitation",
                    #"Temperature",
                    "Discharge"
                    #"Snow cover"
                    ),
        selected = "Discharge"
      ),
      conditionalPanel(
        condition = "input.data == 'Discharge'",
        #selectInput(
        #  'timeAggregation',
        #  'Choose a time aggregation',
        #  choices = c("daily" = "daily",
        #              "weekly" = "weekly",
        #              "monthly" = "monthly"),
        #  selected = "daily"
        #  #multiple = TRUE
        #),
        selectizeInput(
          'dischargeStation', 
          'Choose a station', 
          choices = c("Qilian" = "Qilian",
                      #"Yingluoxia" = "Yingluoxia",
                      "Zhamashike" = "Zhamashike"),
          #selected = "Qilian"
          #multiple = TRUE,
          options = list(maxItems = 1, 
                         placeholder = "Type a station name, e.g. Qilian")
        )
      )
#      conditionalPanel(
#        condition = "input.data == 'Snow cover'",
#        selectInput(
#          "snowCoverAreas",
#          "Choose a sub-catchment",
#          list("Zhamashike","Qilian")
#        )
#      )
    ),  # Data panel
    # Model.
    conditionalPanel(condition="input.conditionedPanels==3",
                     selectInput("model.type",
                                 label = "Choose a model",
                                 choices = c("Multy-layer perceptron" = "mlpe"),
                                 selected = "Multy-layer perceptron"),
                     selectInput("test.for.fit",
                                 label = "Chose quality criteria",
                                 choices = c("Mean average error" = "MAE"),
                                 selected = "Mean average error"),
                     sliderInput("ensembleSize",
                                 label = "Choose ensemble size",
                                 min = 2,
                                 max = 5,
                                 value = 2)
    ),
    # Forecast.
    conditionalPanel(condition="input.conditionedPanels==4",
                     selectInput("forecastHorizon",
                                 label = "Choose a forecasting horizon",
                                 choices = c(#"day",
                                             #"week",
                                             "month"
                                             #"season"
                                             ),
                                 selected = "day"),
                     helpText("Computing forcast. This may take some time.")
    )
    # Help
#    conditionalPanel(condition="input.conditionedPanels==5",
#                     helpText("Documentation.")
#    ) 
  ),  # sidebar panel
  mainPanel(
    # Add tabs to the main panel. 
    tabsetPanel(
      tabPanel("Maps",
               br(),
               textOutput("mapText"),
               plotOutput("preImage",width="400px",height="400px"),
               value = 1),
      tabPanel("Time series data", 
               br(),
               textOutput("timeSeriesDataText"),
               #plotOutput("plotDailyDischarge",width="600px",height="200px"),
               #plotOutput("plotWeeklyDischarge",width="600px", height="200px"),
               plotOutput("plotMonthlyDischarge",width="600px", height="200px"),
               value=2), 
      tabPanel("Model", 
               br(),
               textOutput("modelTypeText"),
               br(),
               textOutput("qualityMetricText"),
               br(),
               textOutput("ensembleText"),
               value = 3),
      tabPanel("Forecast", 
               br(),
               textOutput("predictionTextQilian"),
               br(),
               textOutput("predictionTextZhamashike"),
               br(),
               textOutput("predictionTextSum"),
               plotOutput("predictionPlotQilian",width="600px",height="200px"),
               plotOutput("predictionPlotZhamashike",width="600px",height="200px"),
               value=4),
#      tabPanel("Help", value = 5)
      id = "conditionedPanels"
    )
  )  # main panel
))  # shinyUI(pageWithSidebar(