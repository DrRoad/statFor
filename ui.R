# ui.R

shinyUI(pageWithSidebar(
  headerPanel("Runoff prediction YinluoXia"),
  sidebarPanel(
    # Sidebar panels are conditional to the tabs in the main panel.
    # Summary.
    conditionalPanel(condition="input.conditionedPanels==1", 
                     helpText("Summary of our best daily forecast.")
    ),
    # Data.
    conditionalPanel(condition="input.conditionedPanels==2",
                     selectInput("data", 
                                 label = "Choose data to display",
                                 choices = c("Topography", 
                                             "Precipitation",
                                             "Temperature",
                                             "Discharge",
                                             "Snow cover"),
                                 selected = "Topography"),
                     conditionalPanel(
                       condition = "input.data == 'Discharge'",
                       selectInput("dischargeStation", "Choose a station",
                                   list("Zhamashike","Qilian","Yinluoxia"))
                     ),
                     conditionalPanel(
                       condition = "input.data == 'Snow cover'",
                       selectInput("snowCoverAreas","Choose a sub-catchment",
                                   list("Zhamashike","Qilian"))
                     )
    ),
    # Model.
    conditionalPanel(condition="input.conditionedPanels==3",
                     selectInput("model",
                                 label = "Choose a model",
                                 choices = c("Linear regression"),
                                 selected = "Linear regression")
    ),
    # Forecast.
    conditionalPanel(condition="input.conditionedPanels==4",
                     selectInput("forecastHorizon",
                                 label = "Choose a forecasting horizon",
                                 choices = c("day","week","month","season"),
                                 selected = "day")
                     
    ),
    # Help
    conditionalPanel(condition="input.conditionedPanels==5",
                     helpText("Documentation.")
    ) 
  ),
  mainPanel(
    # Add tabs to the main panel. 
    tabsetPanel(
      tabPanel("Summary",
               value = 1),
      tabPanel("Data", 
               br(),
               textOutput("text1"),
               plotOutput("preImage",width="200px",height="200px"),
               value=2), 
      tabPanel("Model", value = 3),
      tabPanel("Forecast", value=4),
      tabPanel("Help", value = 5)
      , id = "conditionedPanels"
    )
  )
))