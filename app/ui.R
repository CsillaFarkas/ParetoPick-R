############################### UI ###############################

ui <- 
  dashboardPage(
    dashboardHeader(title="OPTAIN"),
    dashboardSidebar(
      sidebarMenu(id = "tabs",
      menuItem("Introduction",tabName = "intro", icon = icon("home")),
      menuItem("Parallel Coordinates",tabName = "play_around",icon = icon("dashboard")),
      menuItem("Data Preparation", icon = icon("th"),tabName = "data_prep"),
      menuItem("Correlation Analysis", tabName = "correlation_analysis",selected=TRUE),
      menuItem("PCA", tabName = "pca"),
      menuItem("Analysis",tabName = "analysis")
      
    )),
    dashboardBody( tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #9eb1cf;
                                }
                                
                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #f4b943;
                                }
                                
                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #f4b943;
                                }
                                
                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #f4b943;
                                }
                                
                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: #9eb1cf;
                                }
                                
                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: #a2a4b6;
                                color: #000000;
                                }
                                
                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: #9ec9cf;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: #ff69b4;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #9eb1cf;
                                }
                                
                                .well {
                                background-color: #c4d0e2;
                                }
                                
                                 /* Style the sidebar input labels */
                                   .well label {
                                   color: #2f353e;
                                   }
                                 
                                '))),
      useShinyjs(),
      tabItems(
        tabItem(tabName = "intro",
                titlePanel("Introduction and Background"),
                mainPanel(p("This application allows to analyse OPTAIN Optimisation outputs. The aim is to support decision making and the analysis of results by reducing the high number of potential solutions provided by the SWAT+ / COMOLA workflow.
                              While all these solutions are pareto-optimal (none of the objectives can be improved without losses in other objectives), choosing among a large number of solutions can be daunting. To reduce complexity while minimising information loss, this application selects a number
                            of optimal solutions using a clustering algorithm based on a Principal Component Analysis (PCA). The user can select variables to be considered in the PCA, decide on the extend of correlation accepted across the considered variables, as well as modify the number of tested clusters and alter the way outliers are handled.",style="text-align; left; font-size:135%"),
                          p("The first tab BLBLA",style="text-align; left; font-size:135%"),p("The second tab BLBLA",style="text-align; left; font-size:135%"),p("The third tab BLBLA",style="text-align; left; font-size:135%")
                          )),
        tabItem(tabName = "play_around",
                titlePanel("Visualising Optimisation Output"),
                mainPanel(
                          )),
        
        
        tabItem(tabName = "data_prep",
                titlePanel("OPTAIN Data Preparation"),
                mainPanel(
                  
                  tags$head(tags$style("#fileStatusMessage{font-size:150%;}")),
                  
                  
                          p("To run the data preparation and the subsequent Correlation and Principal Component Analysis, please provide the following files:",style =  "text-align: left; font-size:150%"),
                         
                  div("1. pareto_genomes.txt",style = "text-align: left; font-size:115%"),
                  div(style = "margin-top: -15px;",fileInput("file1", "", accept = ".txt")), 
                  
                  div("2. pareto_fitness.txt",style = "text-align: left; font-size:115%"),
                  div(style = "margin-top: -15px;",fileInput("file2", "", accept = ".txt")), 
                  
                  div("3. measure_location.csv",style = "text-align: left; font-size:115%"),
                  div(style = "margin-top: -15px;",fileInput("file3", "", accept = ".csv")), 
                  
                  div("4. hru.con",style = "text-align: left; font-size:115%"),
                  div(style = "margin-top: -15px;",fileInput("file4", "", accept = ".con")),
                  
                  div("5. hru.shp",style = "text-align: left; font-size:115%"),
                  div(style = "margin-top: -15px;",fileInput("file5", "", accept = ".shp")),
                  
                  div("6. hru.shx",style = "text-align: left; font-size:115%"),
                  div(style = "margin-top: -15px;",fileInput("file6", "", accept = ".shx")),
                  
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
                                     "Moran's I", "Ratio of structural to management options (linE)"),choiceValues=c("sit","siim","moran","linE"),selected = c("sit","siim","moran","linE")),
      textOutput("numbercorr"),
      div("2. Perform the Correlation Analysis", style = "text-align: left; font-size:150%"),
      actionButton("run", "Run Correlation Analysis"),
      div("3. Choose threshold for correlation",style = "text-align: left; font-size:150%"),
      div(style = "margin-top: -15px;",shinyWidgets::sliderTextInput(inputId = "thresh", label= "",choices = seq(0.65,0.95,0.05),selected=0.75)),
      div("4. Choose variables that shall be excluded from the PCA",style = "text-align: left; font-size:150%"),
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
                                     div("to change these variables please return to the previous tab and choose variables to remove",style = "text-align: left; font-size:100%"),
                          tableOutput("pca_incl"),
                          div("PCA Settings (please specify on the right)", style = "text-align: left; font-size:150%"),
                          htmlOutput("pca_settings_summary"),
                          div("5. Select a clustering method", style = "text-align: left; font-size:150%"),
                          
                          div(style = "margin-top: -15px;",radioButtons("pcamethod", "",
                                       choices = c("k-means", "k-medoids"),
                                       selected = "k-means")),
                          actionButton("runPCA", "Run Principal Component Analysis"),
                          uiOutput("pca_mess")),
                        
                        # PCA Main Panel
                          mainPanel(
                            div("Refine PCA Settings here and then click Run Principal Component Analysis on the left", style = "text-align: left; font-size:150%"),
                            
                            div("1. Please select how the objectives should be plotted", style = "margin-top: 10px; text-align: left; font-size:150%"),
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
                            div("2. Shall several number of clusters be tested?", style = "text-align: left; font-size:150%"),
                            div(style = "margin-top: -15px;",radioButtons("clusyn", "", choices = c("Yes", "No"),selected = "No")),
                            
                            conditionalPanel(
                              condition = "input.clusyn == 'Yes'",
                              h4("Please specify how many clusters to iterate through:"),
                              numericInput("clus_min", "Minimum number of Clusters", value = 3),
                              numericInput("clus_max", "Maximum number of Clusters", value = 3),
                              
                            ),
                            
                            conditionalPanel(
                              condition = "input.clusyn == 'No'",
                              numericInput("clus_fix", "Fixed number of Clusters", value = 15)
                            ),
                            actionButton("write_clust", "Confirm Cluster Number"),
                            
                            # PCA Outlier
                            div("3. Shall outliers be analysed and potentially removed?", style = "text-align: left; font-size:150%"),
                            div(style = "margin-top: -15px;",radioButtons("outlyn", "", choices = c("Yes", "No"),selected = "No")),
                            
                            conditionalPanel(
                              condition = "input.outlyn == 'Yes'",
                              h4("3.1 Please specify the number of standard deviations that shall be tested:"),
                              numericInput("sd_min", "Minimum standard deviation from cluster mean", value = 3),
                              numericInput("sd_max", "Maximum standard deviation from cluster mean", value = 3),
                              h4("3.2 Please specify how many extreme variables within a datapoint shall be tested:"),
                              numericInput("count_min", "Minimum number of extreme variables", value = 3),
                              numericInput("count_max", "Maximum number of extreme variables", value = 3),
                              h4("3.3 Please specify set a limit for the number of extreme solutions allowed in the final clustering:"),
                              numericInput("outlier_ratio", "Outlier to cluster ratio", value = 0.5, min=0.1, max=0.9),
                              actionButton("write_outl", "Confirm Outlier Testing")
                              ),
                            
                            conditionalPanel(
                              condition = "input.outlyn == 'No'",
                              actionButton("write_outl", "Confirm No Outlier Testing") ),
                           
                            div("4. Please specify the number of principal components that shall be tested", style = "text-align: left; font-size:150%"),
                            numericInput("pca_min", "Minimum number of PCs", value = 7),
                            numericInput("pca_max", "Maximum number of PCs", value = 7),
                            actionButton("pcaminmax", "Confirm PC Testing"),
                            
                            # PCA printing Background Processes
                          conditionalPanel(condition = "output.isElementVisible == true",div("Python Background Processes",style = "text-align: left; font-size:150%"),        
                          verbatimTextOutput("pca_status"))
                          
                          ), ## PCA MAIN PANEL END
                        
                        
          )), tabItem(tabName = "analysis",
                      titlePanel("Analysing the remaining optima"),
                      mainPanel(DTOutput("antab"),
                                actionButton("plt_opti","Plot Optimum"),
                                leafletOutput("map")
                                ))
      )))
  