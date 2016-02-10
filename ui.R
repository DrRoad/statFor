# ui.R

shinyUI(fluidPage(

# CSS stylesheet ----  
  # Allows plotting of 2 figures next to each other.
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
# HEADER PANEL ----  
  headerPanel("Discharge prediction for Qilian and Zhamashike"),
# SIDEBAR PANEL ----
  sidebarPanel(
    # Sidebar panels are conditional to the tabs in the main panel.
# - Maps tab. ----
    conditionalPanel(
      condition="input.conditionedPanels==1", 
      selectInput(
        "map_data",
        label = "Choose map to display",
        choices = c("Topography"),
        selected = "Topography"
      )
    ),
# - Time series data tab. ----
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
        selectInput(
          'dischargeStation', 
          label = 'Choose a station', 
          choices = c("Choose" = '',
                      "Qilian" = 'Qilian',
                      "Zhamashike" = 'Zhamashike'),
          selectize = TRUE
          # Options for selectizeInput()
          #options = list(maxItems = 1, 
          #               placeholder = "Type a station name, e.g. Qilian")
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
# - Model tab. ----
    conditionalPanel(condition="input.conditionedPanels==3",
                     selectInput("model.type",
                                 label = "Choose a model",
                                 choices = c("Multi-layer perceptron" = "mlpe"),
                                 selected = "mlpe"),
                     selectInput("test.for.fit",
                                 label = "Chose quality criteria",
                                 choices = c("Mean average error" = "MAE"),
                                 selected = "MAE"),
                     sliderInput("ensembleSize",
                                 label = "Choose ensemble size",
                                 min = 5,
                                 max = 100,
                                 value = 90)
    ),
# - Forecast tab. ----
    conditionalPanel(condition="input.conditionedPanels==4",
                     selectInput("forecastHorizon",
                                 label = "Choose a forecasting horizon",
                                 choices = c(#"dayly",
                                             #"weekly",
                                             "monthly"
                                             #"season"
                                             ),
                                 selected = "monthly"),
                     #helpText("Computing forcast. This may take some time.")
                     conditionalPanel(
                       condition="input.forecastHorizon=='monthly'",
                       selectInput("target",
                                   label = "Choose a month to predict average discharge for",
                                   choices = c("June 2010"     = 1,
                                               "July 2010"     = 2,
                                               "August 2010"   = 3,
                                               "September 2010" = 4,
                                               "October 2010"  = 5,
                                               "November 2010" = 6,
                                               "December 2010" = 7,
                                               "January 2011"  = 8,
                                               "February 2011" = 9,
                                               "March 2011"    = 10,
                                               "April 2011"    = 11,
                                               "May 2011"      = 12,
                                               "June 2011"     = 13,
                                               "July 2011"     = 14,
                                               "August 2011"   = 15,
                                               "September 2011" = 16,
                                               "October 2011"  = 17,
                                               "November 2011" = 18,
                                               "December 2011" = 19,
                                               "January 2012"  = 20,
                                               "February 2012" = 21,
                                               "March 2012"    = 22,
                                               "April 2012"    = 23,
                                               "May 2012"      = 24,
                                               "June 2012"     = 25,
                                               "July 2012"     = 26,
                                               "August 2012"   = 27,
                                               "September 2012" = 28,
                                               "October 2012"  = 29,
                                               "November 2012" = 30,
                                               "December 2012" = 31,
                                               "January 2013"  = 32,
                                               "February 2013" = 33,
                                               "March 2013"    = 34,
                                               "April 2013"    = 35,
                                               "May 2013"      = 36,
                                               "June 2013"     = 37,
                                               "July 2013"     = 38,
                                               "August 2013"   = 39,
                                               "September 2013" = 40,
                                               "October 2013"  = 41,
                                               "November 2013" = 42,
                                               "December 2013" = 43,
                                               "January 2014"  = 44,
                                               "February 2014" = 45,
                                               "March 2014"    = 46,
                                               "April 2014"    = 47,
                                               "May 2014"      = 48,
                                               "June 2014"     = 49,
                                               "July 2014"     = 50,
                                               "August 2014"   = 51,
                                               "September 2014" = 52,
                                               "October 2014"  = 53,
                                               "November 2014" = 54,
                                               "December 2014" = 55
                                               ),
                                   selected = 10
                                   )
                     )
    )
    # Help
#    conditionalPanel(condition="input.conditionedPanels==5",
#                     helpText("Documentation.")
#    ) 
  ),  # sidebar panel
# MAIN PANEL ----
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
               plotOutput("plotMonthlyDischarge",width="600px", height="300px"),
               value=2), 
      tabPanel("Model", 
               br(),
               h3("Model description"),
               htmlOutput("modelInfoText"),
               br(),
               h3("Model quality metrics"),
               h4("Scatter plot"),
               textOutput("scatterPlotHeaderText"),
               br(),
               plotOutput("scatterPlotQilian",width="50%"),
               plotOutput("scatterPlotZhamashike",width="50%"),
               br(),
               br(),
               br(),
               br(),
               h4("Time series plot"),
               htmlOutput("testSetTimeSeriesHeaderText"),
               plotOutput("testSetTimeSeriesQilian",width="50%"),
               plotOutput("testSetTimeSeriesZhamashike",width="50%"),
               value = 3),
      tabPanel("Forecast", 
               br(),
               htmlOutput("predictionText"),
               plotOutput("predictionPlotQilian",width="600px",height="300px"),
               plotOutput("predictionPlotZhamashike",width="600px",height="300px"),
               value=4),
#      tabPanel("Help", value = 5)
      id = "conditionedPanels"
    )
  )  # main panel
))  # shinyUI(pageWithSidebar(