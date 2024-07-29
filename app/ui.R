############################### UI #################################
# comments: 
# Project: Clustering Pareto Solutions/Multi-objective visualisation
####################################################################
ui <- 
  dashboardPage(
    dashboardHeader(title="OPTAIN"),
    dashboardSidebar(
      sidebarMenu(id = "tabs",
      menuItem("Introduction",tabName = "intro", icon = icon("home")),
      menuItem("Visualising the Pareto Front",tabName = "play_around",icon = icon("dashboard")),
      menuItem("Data Preparation", icon = icon("th"),tabName = "data_prep"),
      menuItem("Correlation Analysis", tabName = "correlation_analysis",selected=TRUE),
      menuItem("Clustering", tabName = "pca"),
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
                mainPanel( div(
                  style = "width: 100%; text-align: justify; font-size:135%;",
                  p("This application analyses OPTAIN Optimisation outputs and shall support decision making.  
                              While all solutions provided by the SWAT+ / COMOLA workflow are pareto-optimal (none of the objectives can be improved without losses 
                            in other objectives), choosing among a large number of solutions can be daunting."),
                              br(), p("To reduce complexity while minimising information loss, this application provides a clustering algorithm based on a Principal Component Analysis (PCA).
                              The user can modify the clustering process, alter the number of tested clusters and the way outliers are handled or how much correlation is accepted across the considered variables.
                              Finally, those optima representative for different clusters can be plotted.
                                "),br(),br(),
                          p(" The application is structured the following way:"),
                          p(HTML("The second tab <strong>Visualising the Pareto Front</strong> plots the optimisation results. The user can select preferred objective ranges and assess how a reduced objective space affects the pareto front.")),
                          p(HTML("The third tab <b>Data Preparation</b>is needed to produce the data required for the subsequent analyses.")),
                          p(HTML("The fourth tab <strong>Correlation Analysis</strong> allows to run alter the considered variables and assess the correlation among variables in detail.")))
                          )),
        # PLAY AROUND TAB
        tabItem(tabName = "play_around",
                titlePanel("Visualising Optimisation Output"),
                
                sidebarLayout(
                 
                    sidebarPanel( width="200px",align="left",
                                  fluidRow(
                                  id="parfit",
                                  div("Please provide the pareto_fitness.txt file here",style = "text-align: left; font-size:115%"),
                                  div(style = "margin-top: -15px;",fileInput("par_fit", "", accept = ".txt")), 
                                  # Text input for short and long names of objectives
                                  fluidRow(
                                    id="obj_first",
                                    column(6, textInput("short1", "Short Name Column 1")),
                                    column(6, textInput("short2", "Short Name Column 2")),
                                    column(6, textInput("short3", "Short Name Column 3")),
                                    column(6, textInput("short4", "Short Name Column 4")),
                                  )%>% hidden(),
        
                                  actionButton("save_par_fiti", "Save"),
                                  textOutput("uploaded_pareto"))%>%hidden(),
                                  
                                  div("Objective Range", style="display: inline-block; width: 200px;font-size: 150%",
                                      
                                      #slider aesthetics
                                      tags$style(HTML(".irs-grid-text {font-size: 13px;} 
                            .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #ffc61e ;border-top: 1px solid #ffc61e ;border-bottom: 1px solid #ffc61e;}.js-irs-0 .irs-from, .js-irs-0 .irs-to, .js-irs-0 .irs-single { font-size: 13px;background: #ffc61e !important }")),
                                      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #009ade ;border-top: 1px solid #009ade ;border-bottom: 1px solid #009ade;}.js-irs-1 .irs-from, .js-irs-1 .irs-to, .js-irs-1 .irs-single { font-size: 13px;background: #009ade !important }")),
                                      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #af58ba ;border-top: 1px solid #af58ba ;border-bottom: 1px solid #af58ba;}.js-irs-2 .irs-from, .js-irs-2 .irs-to, .js-irs-2 .irs-single { font-size: 13px;background: #af58ba !important }")),
                                      tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #f28522 ;border-top: 1px solid #f28522 ;border-bottom: 1px solid #f28522;}.js-irs-3 .irs-from, .js-irs-3 .irs-to, .js-irs-3 .irs-single { font-size: 13px; background: #f28522 !important }")),
                                      
                                      sliderInput(inputId = "obj1", label= "Objective 1:",min = 0, max = 1, value = c(0,1)),
                                      sliderInput(inputId = "obj2", label = "Objective 2:", min = 0, max = 1,value= c(0,1)),
                                      sliderInput(inputId = "obj3", label = "Objective 3:", min = 0, max = 1, value = c(0,1)),
                                      sliderInput(inputId = "obj4", label = "Objective 4:", min = 0, max = 1, value = c(0,1))
                                      
                                  )
                    ),
                mainPanel(
                div("Parallel Axis plot", style = "text-align: left; font-size:150%"),
                           plotOutput("linePlot",click="clickline"),
                           verbatimTextOutput("lineDetails"),
                           div("Selected Optimum", style = "text-align: left; font-size:150%"),
                           tableOutput("click_info"),
                           div("Selected Objective Ranges (scaled)", style = "text-align: left; font-size:150%"),
                           tableOutput("sliders"), #needed for min and max
                           div("Selected Objective Ranges (absolute)", style = "text-align: left; font-size:150%"),
                           tableOutput("sliders_abs"),
                          
                           div("Difference between selection and the whole Pareto Front", style = "text-align: left; font-size:150%"),
                           plotOutput("sliders_plot")
                          )## PLAY AROUND MAIN PANEL END
                )),
        
        ## DATA PREP PANEL
        
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
                  
                  div("5. shapefile called \"hru\" with four components (.shp .dbf .prj and .shx)",style = "text-align: left; font-size:115%"),
                  div(style = "margin-top: -15px;",fileInput("shapefile", "", multiple = TRUE, 
                                                             accept = c(".shp", ".shx", ".dbf", ".prj"))),
                  
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
    
      # print confirmed selection
      uiOutput(outputId = "confirmed_selection")
     
      )## CORRELATION ANALYSIS MAIN PANEL END
    
    )
  ),
  
  ## PCA PANEL
  tabItem(tabName = "pca",
         
          tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: darkslateblue}")),
          #https://htmlcolorcodes.com/color-names/
          
          titlePanel("Clustering"),
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
                                textOutput("no_row")%>%hidden(),
                                leafletOutput("map")
                                ))
      )))
  