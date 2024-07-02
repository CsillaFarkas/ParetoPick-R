################# UI ###############################

ui <- 
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      sidebarMenu(
      menuItem("Data Prep", tabName = "data_prep"),
      menuItem("Correlation Analysis", tabName = "correlation_analysis"),
      menuItem("PCA", tabName = "pca")
    )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "data_prep",),
        
      ## CORRELATION ANALYSIS PANEL
        tabItem(tabName = "correlation_analysis",
         
            tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: darkslateblue}")),
            #https://htmlcolorcodes.com/color-names/
            
            titlePanel("Correlation Analysis"),
           sidebarLayout(
              sidebarPanel(
      # Display message about file status
      textOutput("fileStatusMessage"),
      div("1. Choose variables to be included in the Correlation Analysis:", style = "text-align: left; font-size:150%"),
      checkboxGroupInput("selements", "", 
                         choiceNames = c("Measure share in total catchment area (sit)", 
                                     "Measure share in used catchment area (siim)",
                                     "Moran's I", "Ratio of structural to management options (linE)"),choiceValues=c("sit","siim","moran","linE")),
      
      div("2. Perform the Correlation Analysis", style = "text-align: left; font-size:150%"),
      actionButton("run", "Run Correlation Analysis"),
      shinyWidgets::sliderTextInput(inputId = "thresh", label= "Choose threshold",choices = seq(0.65,0.95,0.05),selected=0.75),
      
    ),
    mainPanel(textOutput("fileStatusMessage"),
              
      # Display the selected elements from the checkbox group
      div("Selected Variables", style = "text-align: left; font-size:150%"),
      tableOutput("selements"),
      
      div("Correlation Analysis", style = "text-align: left; font-size:150%"),
      plotOutput("corrplot"),
      
      div("Most correlated variables", style = "text-align: left; font-size:150%"),
      tableOutput("corrtable"), 
    
      div("3. Choose variables that shall be excluded from the PCA",style = "text-align: left; font-size:150%"),
      selectInput(inputId = "excl",label = "variables to exclude", choices = mes,multiple = TRUE),
      actionButton("confirm_selection", "Confirm Selection"),
      
      # print confirmed selectoin
      textOutput("confirmed_selection")
      # textOutput("excla")
       
      # div("Accepted Correlation under this decision",style = "text-align: left; font-size:150%"),
      # tableOutput("remtab")
      
      )## CORRELATION ANALYSIS MAIN PANEL END
    
    )
  ),
  
  ## PCA PANEL
  tabItem(tabName = "pca",
         
          tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: darkslateblue}")),
          #https://htmlcolorcodes.com/color-names/
          
          titlePanel("Principal Component Analysis"),
          sidebarLayout(sidebarPanel(div("Variables included in the PCA",style = "text-align: left; font-size:150%"),
                          tableOutput("pca_incl"),
                          actionButton("runPCA", "Run Principal Component Analysis")),
                        mainPanel(
                          
                        )## PCA MAIN PANEL END
                        )
  ))))
  