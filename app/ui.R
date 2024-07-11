############################### UI ###############################

ui <- 
  dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      sidebarMenu(id = "tabs",
      menuItem("Data Preparation", tabName = "data_prep"),
      menuItem("Correlation Analysis", tabName = "correlation_analysis",selected=TRUE),
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
                  div(id="range_title","Range of objective values",style = "text-align: left; font-size:120%"),
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
      textOutput("numbercorr"),
      div("2. Perform the Correlation Analysis", style = "text-align: left; font-size:150%"),
      actionButton("run", "Run Correlation Analysis"),
      shinyWidgets::sliderTextInput(inputId = "thresh", label= "Choose threshold",choices = seq(0.65,0.95,0.05),selected=0.75),
      div("3. Choose variables that shall be excluded from the PCA",style = "text-align: left; font-size:150%"),
      selectInput(inputId = "excl",label = "variables to exclude", choices = NULL,multiple = TRUE),
      actionButton("confirm_selection", "Confirm Selection"),
      
    ),
    mainPanel(
              
      # Display the selected elements from the checkbox group
      div("Selected Variables", style = "text-align: left; font-size:150%"),
      tableOutput("selements"),
      
      div("Correlation Analysis", style = "text-align: left; font-size:150%"),
      plotOutput("corrplot"),
      
      div("Most correlated variables", style = "text-align: left; font-size:150%"),
      DTOutput("corrtable"), 
    
     
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
                          radioButtons("pcamethod", "Select a clustering option:",
                                       choices = c("k-means", "k-medoids"),
                                       selected = "k-means"),
                          actionButton("runPCA", "Run Principal Component Analysis"),
                          uiOutput("pca_mess")),
                          mainPanel(div("Please select how the objectives should be plotted", style = "text-align: left; font-size:150%"),
                            fluidRow(column(6,
                                    selectInput("element1", "X Axis", choices = NULL),
                                    selectInput("element2", "Y Axis", choices = NULL),
                                    selectInput("element3", "Colour", choices = NULL),
                                    selectInput("element4", "Size", choices = NULL),
                                    actionButton("set_choices","Confirm Choice"),
                                    htmlOutput("selected_elements")),
                                    column(6,
                                           textInput("axisx","X Axis Label",value = ""),
                                           textInput("axisy","Y Axis Label",value = ""),
                                           textInput("colour","Colour Label",value = ""),
                                           textInput("size","Size Label",value = ""),
                                           actionButton("confirm_axis","Confirm Axis Labels"),
                                           htmlOutput("axis_text"))),
                            # PCA Clustering
                            div("Shall several number of clusters be tested?", style = "text-align: left; font-size:150%"),
                            div(style = "margin-top: -15px;",radioButtons("clusyn", "", choices = c("Yes", "No"))),
                            
                            conditionalPanel(
                              condition = "input.clusyn == 'Yes'",
                              h3("Please specify how many clusters to iterate through:"),
                              numericInput("clus_min", "Minimum number of Clusters", value = 0),
                              numericInput("clus_max", "Maximum number of Clusters", value = 0),
                              
                            ),
                            
                            conditionalPanel(
                              condition = "input.clusyn == 'No'",
                              numericInput("clus_fix", "Fixed number of Clusters", value = 0)
                            ),
                            actionButton("write_clust", "Confirm Cluster Number"),
                            
                            # PCA Outlier
                            div("Shall outliers be analysed and potentially removed?", style = "text-align: left; font-size:150%"),
                            div(style = "margin-top: -15px;",radioButtons("outlyn", "", choices = c("Yes", "No"))),
                            
                            conditionalPanel(
                              condition = "input.outlyn == 'Yes'",
                              h3("Please specify the number of standard deviations that shall be tested:"),
                              numericInput("sd_min", "Minimum standard deviation from cluster mean", value = 0),
                              numericInput("sd_max", "Maximum standard deviation from cluster mean", value = 0),
                              h3("Please specify how many extreme variables within a datapoint shall be tested:"),
                              numericInput("count_min", "Minimum number of extreme variables", value = 0),
                              numericInput("count_max", "Maximum number of extreme variables", value = 0),
                              h3("Please specify set a limit for the number of extreme solutions allowed in the final clustering:"),
                              numericInput("outlier_ratio", "Outlier to cluster ratio", value = 0.5, min=0.1, max=0.9),
                              actionButton("write_outl", "Confirm Outlier Testing")
                              ),
                            
                            conditionalPanel(
                              condition = "input.outlyn == 'No'",
                              actionButton("write_outl", "Confirm No Outlier Testing") ),
                           
                            
                            
                            
                            # PCA printing Background Processes
                          conditionalPanel(condition = "output.isElementVisible == true",div("Python Background Processes",style = "text-align: left; font-size:150%"),        
                          verbatimTextOutput("pca_status"))
                          
                        )## PCA MAIN PANEL END
                        )
  ))))
  