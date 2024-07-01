################# UI ###############################



ui <- fluidPage(
  titlePanel("Shiny App Calling Python Script and Reading Correlation Output"),
  sidebarLayout(
    sidebarPanel(
      actionButton("run", "Run Correlation Analysis"),
      sliderInput(inputId = "thresh", label= "",min = 0, max = 1, value = c(0,1)),
      
    ),
    mainPanel( div("Correlation Analysis", style = "text-align: left; font-size:150%"),
      plotOutput("corrplot"),
      div("Most correlated variables", style = "text-align: left; font-size:150%"),
      tableOutput("corrtable")
    )
  )
)