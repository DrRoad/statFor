# ui.R

shinyUI(pageWithSidebar(
  headerPanel("Runoff prediction YinluoXia"),
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
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
    conditionalPanel(condition="input.conditionedPanels==2",
                     helpText("Choose a forecasting horizon")
                     
    ) 
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Data", 
               br(),
               textOutput("text1"),
               plotOutput("preImage",width="200px",height="200px"),
               value=1), 
      tabPanel("Forecast", value=2)
      , id = "conditionedPanels"
    )
  )
))