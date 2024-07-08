############################### UI ###############################

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
      useShinyjs(),
      tabItems(
        tabItem(tabName = "data_prep",
                titlePanel("OPTAIN Data Preparation"),
                mainPanel(
                  
                  tags$head(tags$style("#fileStatusMessage{font-size:150%;}")),
                  
                  
                          p("To run the data preparation and the subsequent Correlation and Principal Component Analysis the following files have to be provided in the data folder:",style =  "text-align: left; font-size:150%"),
                         HTML(paste0("<ol>",
                            "<li style=font-size:150%>","pareto_genomes.txt","</li>",
                            "<li style=font-size:150%>","pareto_fitness.txt","</li>",
                            "<li style=font-size:150%>","hru.con","</li>",
                            "<li style=font-size:150%>","measure_location.csv","</li>",
                            "</ol>")),
                         actionButton("files_avail", "Check Files"),
                         
                  uiOutput("fileStatusMessage"),
                  
                  fluidRow(
                    id="sel_obj",
                    column(3, textInput("col1", "Objective 1\n (Column 1)")),
                    column(3, textInput("col2", "Objective 2\n (Column 2)")),
                    column(3, textInput("col3", "Objective 3\n (Column 3)")),
                    column(3, textInput("col4", "Objective 4\n (Column 4)")),
                    actionButton("obj_conf", "Confirm Objective Names")
                  )%>% hidden(), 
                  hr(),
                  tableOutput("obj_conf"),
                  p("Run Preparation Script when ready",style =  "text-align: left; font-size:150%"),
                  actionButton("runprep", "Run Prep"),
                  uiOutput("script_output") 
                  
                )# DATA PREP MAIN PANEL END
        ), 
        
      ## CORRELATION ANALYSIS PANEL
        tabItem(tabName = "correlation_analysis",
         
            # tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: darkslateblue}")),
            # #https://htmlcolorcodes.com/color-names/
            # 
            titlePanel("Correlation Analysis"),
           sidebarLayout(
              sidebarPanel(
      # Display message about file status
      div("1. Choose variables to be included in the Correlation Analysis:", style = "text-align: left; font-size:150%"),
      checkboxGroupInput("selements", "", 
                         choiceNames = c("Measure share in total catchment area (sit)", 
                                     "Measure share in used catchment area (siim)",
                                     "Moran's I", "Ratio of structural to management options (linE)"),choiceValues=c("sit","siim","moran","linE")),
      
      div("2. Perform the Correlation Analysis", style = "text-align: left; font-size:150%"),
      actionButton("run", "Run Correlation Analysis"),
      shinyWidgets::sliderTextInput(inputId = "thresh", label= "Choose threshold",choices = seq(0.65,0.95,0.05),selected=0.75),
      div("3. Choose variables that shall be excluded from the PCA",style = "text-align: left; font-size:150%"),
      selectInput(inputId = "excl",label = "variables to exclude", choices = mes,multiple = TRUE),
      actionButton("confirm_selection", "Confirm Selection"),
      
    ),
    mainPanel(
              
      # Display the selected elements from the checkbox group
      div("Selected Variables", style = "text-align: left; font-size:150%"),
      tableOutput("selements"),
      
      div("Correlation Analysis", style = "text-align: left; font-size:150%"),
      plotOutput("corrplot"),
      
      div("Most correlated variables", style = "text-align: left; font-size:150%"),
      tableOutput("corrtable"), 
    
     
      # print confirmed selectoin
      uiOutput(outputId = "confirmed_selection")
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
                          mainPanel(div("Plot Style: Please select how the objectives should be plotted", style = "text-align: left; font-size:150%"),
                                    selectInput("element1", "X Axis", choices = c("off","A", "B", "C", "D")),
                                    selectInput("element2", "Y Axis", choices = c("off","A", "B", "C", "D")),
                                    selectInput("element3", "Colour", choices = c("off","A", "B", "C", "D")),
                                    selectInput("element4", "Size", choices = c("off","A", "B", "C", "D")),
                                    actionButton("set_choices","SET CHOICES"),
                                    textOutput("selected_elements"),

                                    textOutput("selected_elements"),
                                    
                                    
                          verbatimTextOutput("pca_status")
                          
                        )## PCA MAIN PANEL END
                        )
  ))))
  