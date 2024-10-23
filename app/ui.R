############################### UI #################################
# comments: 
# Project: Clustering Pareto Solutions/Multi-objective visualisation
# author: cordula.wittekind@ufz.de
####################################################################
ui <- 
  dashboardPage(
    dashboardHeader(title="ParetoPick-R"),
    dashboardSidebar(
      sidebarMenu(id = "tabs",
                  menuItem("Introduction",tabName = "intro", icon = icon("home"),selected=TRUE),
                  menuItem("Visualising the Pareto Front",tabName = "play_around",icon = icon("dashboard")),
                  menuItem("Data Preparation", icon=icon("file",lib = "font-awesome"),tabName = "data_prep"),
                  menuItem("Configure Clustering", tabName = "configure", icon = icon("cog")),
                  
                  conditionalPanel(
                    condition = "input.show_tabs == 'show'",
                    menuItem("Correlation Analysis",icon=icon("random", lib="font-awesome"), tabName = "correlation_analysis"),
                    menuItem("PCA & kmeans/kmedoids",icon=icon("project-diagram", lib="font-awesome"), tabName = "pca")
                  ),
                  
                  menuItem("Cluster Analysis", icon = icon("th"),tabName = "analysis"),
                  menuItem("AHP",icon=icon("sliders-h", lib="font-awesome"),tabName = "ahp")
                  
      )),
    dashboardBody( tags$style(HTML('
                                  /* File status message font size adjustment */
                                  #fileStatusMessage {font-size: 150%;}
                             

                                  /* Logo background color */
                                  .skin-blue .main-header .logo {
                                   background-color: #95C11F;
                                  }

                                  /* Logo background color when hovered */
                                  .skin-blue .main-header .logo:hover {
                                   background-color: #83D0F5;
                                  }

                                  /* Main sidebar background color */
                                  .skin-blue .main-sidebar {
                                    background-color: #03597F;
                                  }

                                  /* AHP tab background color */
                                  .sidebar-menu li a[data-value="ahp"] {
                                    background-color: #4F518C !important;
                                  }
                                  
                                  .sidebar-menu li a[data-value="ahp"]:hover {
                                    background-color: #83D0F5 !important;
                                  }

                                  /* Analysis tab background color */
                                  .sidebar-menu li a[data-value="analysis"] {
                                    background-color: #935D33 !important;
                                  }
                                  
                                  .sidebar-menu li a[data-value="analysis"]:hover {
                                    background-color: #83D0F5 !important;
                                  }

                                  /* Configure, Correlation Analysis and PCA tabs with same background color */
                                  .sidebar-menu li a[data-value="correlation_analysis"] {
                                    background-color: #F7A600 !important;
                                  }
                                  
                                  .sidebar-menu li a[data-value="correlation_analysis"]:hover {
                                    background-color: #83D0F5 !important;
                                  }

                                  .sidebar-menu li a[data-value="pca"] {
                                    background-color: #F7A600 !important;
                                  }
                                  .sidebar-menu li a[data-value="pca"]:hover {
                                    background-color: #83D0F5 !important;
                                  }
                                  
                                    .sidebar-menu li a[data-value="configure"] {
                                    background-color: #F7A600 !important;
                                  }
                                  .sidebar-menu li a[data-value="configure"]:hover {
                                    background-color: #83D0F5 !important;
                                  }
                                   .sidebar-menu li a[data-value="intro"]:hover {
                                    background-color: #83D0F5 !important;
                                  }
                                  
                                  

                                  /* AHP sidebar specific background color */
                                  .sidebar-menu li a[data-value="ahp"] {
                                    background-color: #FFEF2C !important;
                                  }
                                  
                                  
                                  /* Active selected tab in the sidebar menu */
                                  .skin-blue .main-sidebar .sidebar .sidebar-menu .active a {
                                    background-color: #9eb1cf !important;
                                  }

                                  /* Default background and text color for other links in the sidebar menu */
                                  .skin-blue .main-sidebar .sidebar .sidebar-menu a {
                                    background-color: #7a7785;
                                    color: #000000;
                                  }

                                  /* Hover effect for other links in the sidebar menu */
                                  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover {
                                    background-color: #83D0F5;
                                  }

                                  /* Hover effect for the toggle button */
                                  .skin-blue .main-header .navbar .sidebar-toggle:hover {
                                    background-color: #83D0F5;
                                  }

                                  /* General body background color and font family (Montserrat) */
                                  .content-wrapper, .right-side {
                                    background-color: #9eb1cf;
                                    font-family: "Montserrat", sans-serif;
                                  }

                                  /* Styling for .well elements */
                                  .well {
                                    background-color: #b9cae5;
                                    padding: 6px 7px;
                                    font-size: 120%;
                                    border: none;
                                  }

                                  /* Styling for input labels inside .well */
                                  .well label {
                                    color: #2f353e;
                                  }

                                  /* Ensuring content height covers the full view */
                                  .content {min-height: 300vh;}

                                  /* Slider element styling */
                                  .irs-grid-text {
                                    font-size: 13px;
                                  }

                                  .js-irs-0 .irs-single,
                                  .js-irs-0 .irs-bar-edge,
                                  .js-irs-0 .irs-bar {
                                    background: #ffc61e;
                                    border-top: 1px solid #ffc61e;
                                    border-bottom: 1px solid #ffc61e;
                                  }

                                  .js-irs-0 .irs-from,
                                  .js-irs-0 .irs-to,
                                  .js-irs-0 .irs-single {
                                    font-size: 13px;
                                    background: #ffc61e !important;
                                  }

                                  .js-irs-0 .irs-single,
                                  .js-irs-0 .irs-bar-edge,
                                  .js-irs-0 .irs-bar {
                                    background: darkslateblue;
                                  }

                              /* Main panel full-width adjustment */
                                .main-panel-full-width {
                                 margin-left: 0 !important;
                                 width: 100% !important;
                                }

                                /* Main panel size relative to sidebar width */
                                .main-panel {
                                 margin-left: 250px; /* adjust sidebar width */
                                 width: calc(100% - 250px); /* adjust sidebar width */
                                }

                               /* AHP criterion labels made non-bold */
                                #criterion1_ui label,
                                #criterion2_ui label {font-weight: 400;}
   
                               /* Title on maps in analysis tab */
                                 .map-title {
                                  font-weight: bold; font-size:
                                  16px; margin-bottom: 
                                  5px; text-align: center;
                                  }

                               /* R2 and Pearson table in AHP tab */
                                  #relation {
                                  font-size: 18px;
                                  font-weight: bold;
                                  }

                                  #relation th, #relation td {
                                  border: none;
                                  padding: 8px;
                                  }
                                ')),
                   useShinyjs(),
                   
                   tabItems(
                     tabItem(tabName = "intro",
                             titlePanel("Introduction and Background"),
                             mainPanel( div(
                               style = "width: 100%; text-align: justify; font-size:135%;",
                               p("This application analyses OPTAIN optimisation outputs and shall support decision making.  
                                  While all solutions provided by the SWAT+ / COMOLA workflow are pareto-optimal (none of the objectives can be improved without losses 
                                  in other objectives), choosing among a large number of solutions can be daunting."),
                               br(), 
                               p("To reduce complexity while minimising information loss, this application provides two ways to filter/reduce the pareto front:"),
                               tags$ol(tags$li("A clustering algorithm based on a Principal Component Analysis (PCA) and kmeans/kmedoidss.
                                                The user can modify the clustering process, alter the number of tested clusters and the way outliers are handled or how much correlation is accepted across the considered variables.
                                                Finally, those optima  representative for different clusters can be plotted and the measure implementation they recommend can be compared.
                                               "),
                                       br(),
                                       tags$li("An Analytical Hierarchy Process that can be run as standalone method as well as as additional feature on top of the clustered pareto front. ")),
                                       br(),
                                       br(),
                               p(" The application is structured the following way:"),
                               p(HTML("The second tab <strong>Visualising the Pareto Front</strong> provides an overview over the optimisation results. The user can gain insights into the relationships between the objectives and the pareto front by selecting and plotting preferred objective ranges.")),
                               p(HTML("The third tab <b>Data Preparation</b> is needed to produce the data required for the subsequent analyses. Several files need to be provided so the variables considered in the clustering can be calculated.")),
                               p(HTML("The fourth tab <strong>Clustering Part 1 - Correlation Analysis</strong> allows to assess and alter variables considered in the subsequent clustering.")),
                               p(HTML("The fifth tab <strong>Clustering Part 2 - PCA & kmeans/kmedoids</strong> provides the possibility to adapt, modify and finally perform the clustering process.")),
                               p(HTML("The <strong>Analysis</strong> tab lets the user plot the optima remaining after the clustering. Each of these optima is representative for one cluster.")),
                               p(HTML("The tab <strong>AHP - Analytical Hierarchy Process</strong> allows to determine priorities across the pareto front in a different way using weights across the optima. It is possible to combine the clustering results with the AHP."))
                               
                             )
                             )),
                     ### PLAY AROUND TAB ####
            tabItem(tabName = "play_around",
               titlePanel("Visualising the Optimisation Output"),
               
                wellPanel(    p("After providing the pareto_fitness.txt and the objective names (given in the first four columns of pareto_fitness.txt) either here or in the next tab, this tab lets you plot the pareto front in two different ways
                              and explore the effects of reduced objective ranges."),p("You can also select specific points on the pareto front by clicking on the line plot.")),
                             
                             sidebarLayout(
                               
                               
                               sidebarPanel( width = 3,
                                             div(
                                               id="parfit",
                                               "Please provide the pareto_fitness.txt file here and click Save:",style = "text-align: left; font-size:115%",
                                               fileInput("par_fit", "", accept = ".txt"),
                                               actionButton("save_paretofit","Save")),
                                             div(
                                               id="sq",
                                               "If you want you can supply the sq_fitness.txt file here to plot the status quo.",style = "text-align: left; font-size:115%",
                                               fileInput("sq_in","",accept = ".txt"),
                                               actionButton("save_sq_in","Save")),
                                             
                                             div(
                                               id = "obj_first",
                                               "Please provide the objective names as given in the first four columns of the pareto_fitness.txt file:",
                                               style = "text-align: left; font-size:115%", 
                                               textInput("short1", "Objective 1\n (Column 1)"), 
                                               textInput("short2", "Objective 2\n (Column 2)"), 
                                               textInput("short3", "Objective 3\n (Column 3)"), 
                                               textInput("short4", "Objective 4\n (Column 4)"),
                                               actionButton("save_par_fiti", "Save"))
                                             # %>% hidden(), 
                                             ,
                                             div(id="units",
                                                 "If you want you can supply the objectives' units and save them for future use:",
                                                 style= "text-align: left; font-size:115%",
                                                 textInput("unit1","unit Objective 1", value = ""),
                                                 textInput("unit2","unit Objective 2", value = ""),
                                                 textInput("unit3","unit Objective 3", value = ""),
                                                 textInput("unit4","unit Objective 4", value = ""),
                                                 actionButton("save_unit","Save")),
                                             
                                             textOutput("uploaded_pareto"),
                                             br(),
                                             br(),
                                             div(
                                               column(10,
                                                      div("Objective Range", style = "text-align: left; font-size:150%; margin-top: 40px;"),
                                                      
                                                      sliderInput(inputId = "obj1", label=  "Objective 1:", min = 0, max = 1, value = c(0,1), step = 0.01,width = "110%"),
                                                      sliderInput(inputId = "obj2", label = "Objective 2:", min = 0, max = 1, value = c(0,1), step = 0.01,width = "110%"),
                                                      sliderInput(inputId = "obj3", label = "Objective 3:", min = 0, max = 1, value = c(0,1), step = 0.01, width = "110%"),
                                                      sliderInput(inputId = "obj4", label = "Objective 4:", min = 0, max = 1, value = c(0,1), step = 0.01,width = "110%")))
                                             
                               ),
                               mainPanel(
                                 tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #ffc61e ;border-top: 1px solid #ffc61e ;border-bottom: 1px solid #ffc61e;}.js-irs-1 .irs-from, .js-irs-1 .irs-to, .js-irs-1 .irs-single { font-size: 13px;background: #ffc61e !important }")),
                                 tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #009ade ;border-top: 1px solid #009ade ;border-bottom: 1px solid #009ade;}.js-irs-2 .irs-from, .js-irs-2 .irs-to, .js-irs-2 .irs-single { font-size: 13px;background: #009ade !important }")),
                                 tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #aF58ba ;border-top: 1px solid #aF58ba ;border-bottom: 1px solid #aF58ba;}.js-irs-3 .irs-from, .js-irs-3 .irs-to, .js-irs-3 .irs-single { font-size: 13px;background: #aF58ba !important }")),
                                 tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #f28522 ;border-top: 1px solid #f28522 ;border-bottom: 1px solid #f28522;}.js-irs-4 .irs-from, .js-irs-4 .irs-to, .js-irs-4 .irs-single { font-size: 13px;background: #f28522 !important }")),
                                 
                                 div(id = "tab_play1",div("Pareto Plot", style = "text-align: left; font-size:150%"),
                                     plotOutput("first_pareto"), 
                                     checkboxInput("add_sq_f",label = "Show status quo",value = FALSE),
                                     div(id="rev_plot",checkboxInput("rev_box",label="reverse x and y axes",value = FALSE))%>%hidden(),
                                     fluidRow(
                                       column(3,selectInput(inputId = "x_var3",   label = "X-Axis", choices = NULL, multiple = F, selected=NULL)),
                                       column(3,selectInput(inputId = "y_var3",   label = "Y-Axis", choices = NULL, multiple = F, selected=NULL)),
                                       column(3,selectInput(inputId = "col_var3", label = "Colour", choices = NULL, multiple = F, selected=NULL)),
                                       column(3,selectInput(inputId = "size_var3",label = "Size",   choices = NULL, multiple = F, selected=NULL))
                                     ),
                                     div(
                                       style = "display: inline-block; vertical-align: top; margin-right: 0px;",
                                       textInput("fp_plot_savename", label = NULL, value = "pareto")
                                     ),
                                     div(
                                       style = "display: inline-block; vertical-align: top; margin-left: 0px;",
                                       downloadButton("download_fp_plot", "Download Plot")),
                                     
                                     fluidRow(
                                       column(12,
                                              fluidRow(column(6, div("Selected Objective Ranges (scaled)", style = "text-align: left; font-size:150%"),
                                                              tableOutput("sliders")),
                                                       column(6, div("Selected Objective Ranges (absolute)", style = "text-align: left; font-size:150%"),
                                                              tableOutput("sliders_abs"))),
                                              fluidRow(column(6,
                                                              div("Selected Optimum (select in line plot)", style = "text-align: left; font-size:150%"),
                                                              tableOutput("click_info"),
                                                              checkboxInput("save_click_line",label = "Click here to save the selected optimum to the output folder (selected_optima.csv)",value=F)%>%hidden()),
                                                       
                                                       column(6,div("Maximum Objective Ranges (absolute)",style = "text-align: left; font-size:150%"),
                                                              tableOutput("whole_range"))
                                                       
                                              ))),
                                     
                                     
                                     
                                     checkboxInput("plt_sq", label = "Show status quo", value = FALSE)),
                                 
                                 div(id = "tab_play2",div("Parallel Axis plot", style = "text-align: left; font-size:150%"),
                                     plotOutput("linePlot",click="clickline"),
                                     div(
                                       style = "display: inline-block; vertical-align: top; margin-right: 0px;",
                                       textInput("line_plot_savename", label = NULL, value = "parallel line")
                                     ),
                                     div(
                                       style = "display: inline-block; vertical-align: top; margin-left: 0px;",
                                       downloadButton("download_line_plot", "Download Plot")),
                                     verbatimTextOutput("lineDetails"),
                                     
                                     div(id="scatter","Scatter Plot",style = "text-align: left; font-size:150%"),
                                     plotOutput("scatter_plot"),
                                     
                                     div(
                                       style = "display: inline-block; vertical-align: top; margin-right: 0px;",
                                       textInput("scat_plot_savename", label = NULL, value = "pairwise scatter")
                                     ),
                                     div(
                                       style = "display: inline-block; vertical-align: top; margin-left: 0px;",
                                       downloadButton("download_scat_plot", "Download Plot")
                                     ),
                                     
                                     # div("Difference between selection and the whole Pareto Front", style = "text-align: left; font-size:150%"),
                                     # plotOutput("sliders_plot"),
                                     # div(
                                     # style = "display: inline-block; vertical-align: top; margin-right: 0px;",
                                     # textInput("diff_plot_savename", label = NULL, value = "difference barplot")
                                     # ),
                                     # div(
                                     #   style = "display: inline-block; vertical-align: top; margin-left: 0px;",
                                     #   downloadButton("download_diff_plot", "Download Plot")
                                     # )
                                 )
                                 
                               )## PLAY AROUND MAIN PANEL END
                             )),
                     
                     ## DATA PREP PANEL #####
                     
               tabItem(tabName = "data_prep",
                             titlePanel("OPTAIN Data Preparation"),
                             
                             wellPanel(  p("This tab requires you to provide the required optimisation outputs, please retain their names as given here and as produced in the optimisation workflow."),
                                         p(HTML("Please click <strong>Check Files</strong> after doing so.")),
                                         p(HTML("If all files have been found, you can click <strong>Run Prep</strong> to prepare the variables for the subsequent correlation and cluster analysis. This takes can take up to 5 minutes."))),
                             
                             mainPanel(
                               p("To prepare the data for the subsequent correlation and cluster analysis, please provide the following files:",style =  "text-align: left; font-size:150%"),
                               
                               #file numbers are jumbled but just here     
                               div("1. pareto_genomes.txt",style = "text-align: left; font-size:115%"),
                               div(style = "margin-top: -15px;",fileInput("file1", "", accept = ".txt")), 
                               
                               div("2. hru.con",style = "text-align: left; font-size:115%"),
                               div(style = "margin-top: -15px;",fileInput("file2", "", accept = ".con")),
                               
                               
                               div("3. measure_location.csv",style = "text-align: left; font-size:115%"),
                               div(style = "margin-top: -15px;",fileInput("file3", "", accept = ".csv")), 
                               
                               
                               div("4. rout_unit.con", style="text-align: left; font-size:115%"),
                               div(style = "margin-top: -15px;",fileInput("file6", "", accept = ".con")),
                               
                               
                               div("5. shapefile called \"hru\" with four components (.shp .dbf .prj and .shx)",style = "text-align: left; font-size:115%"),
                               div(style = "margin-top: -15px;",fileInput("shapefile", "", multiple = TRUE, 
                                                                          accept = c(".shp", ".shx", ".dbf", ".prj"))),
                               
                               div("6. shapefile called \"basin\" with four components (.shp .dbf .prj and .shx)",style = "text-align: left; font-size:115%"),
                               div(style = "margin-top: -15px;",fileInput("basfile", "", multiple = TRUE, 
                                                                          accept = c(".shp", ".shx", ".dbf", ".prj"))),
                               
                               div(id="sq_avail",
                                   
                                   div("7. sq_fitness.txt (not obligatory but required if you want to plot the status quo)",style = "text-align: left; font-size:115%"),
                                   div(style = "margin-top: -15px;",fileInput("file5", "", accept = ".txt")))%>%hidden(),
                               
                               
                               div(id="fitness_avail",
                                   
                                   div("8. pareto_fitness.txt (if not provided in the previous tab)",style = "text-align: left; font-size:115%"),
                                   div(style = "margin-top: -15px;",fileInput("file4", "", accept = ".txt")))%>%hidden(), 
                               
                               
                               
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
                               
                               div(id="runprep_show",p("Run Preparation Script when ready (this should take no more than five minutes)",style =  "text-align: left; font-size:150%"),
                                   actionButton("runprep", "Run Prep"))%>%hidden,
                               uiOutput("script_output"),
                               br(),br(),
                               div(id="range_title","Range of objective values as given in pareto_fitness.txt",style = "text-align: left; font-size:120%"),
                               tableOutput("obj_conf"),
                               
                               br(),br(),br(),
                               
                               div(id="reset", htmlOutput("reset_prompt"),
                                   actionButton("reset_btn", "Hard Reset",style = "color: white; background-color: red; font-size: 15px; padding: 8px 8px; border-radius: 5px;"),
                                   textOutput("reset_status"))
                               
                             )# DATA PREP MAIN PANEL END
                     ), 
                   ## CONFIGURE CLUSTERING PANEL - USER DECISION FOR HIDING OR SHOWING correlation AND clustering ####
           
                     tabItem(tabName = "configure",
                             
                             titlePanel("Configure Cluster Settings"),
                             
                    mainPanel(
       
                        div(style = "text-align: left; font-size:150%; width: 100%;",
                                    "Would you like to limit the objective ranges prior to clustering?",
                              radioButtons("limra_clust", "", choices = c("Yes", "No"), selected = "No")),
                              
                        conditionalPanel(
                          
                                condition = "input.limra_clust == 'Yes'",
                                                        
                                sliderInput(inputId = "ran1", label= "Objective 1:",min = 0, max = 100, value = c(0,100), width = "110%"),
                                sliderInput(inputId = "ran2", label= "Objective 2:",min = 0, max = 100, value = c(0,100), width = "110%"),
                                sliderInput(inputId = "ran3", label= "Objective 3:",min = 0, max = 100, value = c(0,100), width = "110%"),
                                sliderInput(inputId = "ran4", label= "Objective 4:",min = 0, max = 100, value = c(0,100), width = "110%"),
                                 ),
    
                              tags$div(textOutput("check_range"), style = "color: red;"),
                              br(),
                              br(),
                              
                              div(style = "text-align: left; font-size:150%; width: 100%;",
                                          "Would you like to alter the correlation and cluster settings or run with default settings?",
                                      radioButtons("show_tabs",label="",
                                      choices = list("show cluster tabs" = "show", "run with default settings" = "default"), selected = "default")),
                                      
                                      uiOutput("next_step"),
                                      uiOutput("corr_notthere_config"), 
                                      withSpinner(
                                        uiOutput("spinner_output"),
                                        type = 4  , color = "#F7A600"
                                      )
                        )##################CONFIG MAIN PANEL END
                                    
                             ),
                     
                     ## CORRELATION ANALYSIS PANEL ####
                tabItem(tabName = "correlation_analysis",
                             titlePanel("Clustering Part 1 - Correlation Analysis"),
                             
                             wellPanel( p(HTML("Since correlation among variables can skew cluster results, a correlation analysis and potential removal of variables is required. 
                             For this purpose, please click <strong>Run Correlation Analysis</strong>. 
                             Based on the levels of correlation you can select those variables you would like to exclude from the subsequent clustering. Select them and then click <strong>Confirm Selection</strong>. You can come back to this tab to change the selection of variables later.")),
                                        p(HTML("It is also possible to run the clustering across all variables and select no variables to exclude in this tab, however please always click <strong>Confirm Selection</strong>."))),
                             sidebarLayout(
                               
                               sidebarPanel(
                                 ## display missing files in sidebar
                                 uiOutput("corr_notthere"), 
                                 
                                 div(
                                   id = "corr_sidebar",
                                   div(
                                     "1. Choose variables to be included in the Correlation Analysis:",
                                     style = "text-align: left; font-size:150%"
                                   ),div(
                                     "(those marked with * have been calculated for each measure separately, details under Selected Variables)",
                                     style = "text-align: left; font-size:80%"
                                   ),
                                   checkboxGroupInput("selements", "",  
                                                      choiceNames = c("share_tot (*)", 
                                                                      "share_con (*)",
                                                                      "Moran's I",
                                                                      "channel_frac (*)",
                                                                      "linE"),
                                                      choiceValues=c("share_tot","share_con","moran","channel_frac","linE"),
                                                      selected = c("share_tot","share_con","moran","channel_frac","linE")),
                                   
                                   textOutput("numbercorr"),
                                   div("2. Perform the Correlation Analysis", style = "text-align: left; font-size:150%"),
                                   actionButton("run_corr", "Run Correlation Analysis"),
                                   
                                   div("3. Choose threshold for correlation",style = "text-align: left; font-size:150%"),
                                   div(style = "margin-top: -15px;",shinyWidgets::sliderTextInput(inputId = "thresh", label= "",choices = seq(0.65,0.95,0.05), selected=0.75)),
                                   
                                   div("4. Choose variables that shall be excluded from the PCA",style = "text-align: left; font-size:150%"),
                                   selectInput(inputId = "excl",label = "variables to exclude", choices = NULL, multiple = TRUE),
                                   
                                   div(id="show_conf","5. Please confirm your choice before proceeding to the next tab.",style = "text-align: left; font-size:150%"
                                       ,actionButton("confirm_selection", "Confirm Selection"))%>%hidden,
                                   # print confirmed selection
                                   uiOutput(outputId = "confirmed_selection")
                                   
                                 )),
                               mainPanel(div(id="corr_content", 
                                             
                                             # Display the selected elements from the checkbox group
                                             div("Selected Variables", style = "text-align: left; font-size:150%"),
                                             tableOutput("selements"),
                                             
                                             div("Correlation Analysis", style = "text-align: left; font-size:150%"),
                                             plotOutput("corrplot"),
                                             
                                             div(
                                               style = "display: inline-block; vertical-align: top; margin-right: 0px;",
                                               textInput("corr_plot_savename", label = NULL, value = "correlation")
                                             ),
                                             div(
                                               style = "display: inline-block; vertical-align: top; margin-left: 0px;",
                                               downloadButton("download_corr_plot", "Download Plot")),
                                             
                                             
                                             div("Most correlated variables", style = "text-align: left; font-size:150%"),
                                             DTOutput("corrtable")
                                             
                               ))## CORRELATION ANALYSIS MAIN PANEL END
                               
                             )
                     ),
                     
                     ## Clustering/PCA PANEL ####
                     tabItem(tabName = "pca",
                             
                             #https://htmlcolorcodes.com/color-names/
                             
                             titlePanel("Clustering Part 2 - PCA & kmeans/kmedoids"),
                             
                             wellPanel(  p("This tab requires you to decide on the cluster settings. After selecting which objectives shall be plotted on the x-axis, y-axis and as color and size and after deciding on the axis titles, the clustering can be run with default settings."),
                                         p(HTML("Selecting <strong>Yes</strong> under either 2. or 3. allows to change those default settings and test a variable number of clusters and outlier considerations.")),
                                         p(HTML("The cluster outputs open in separate tabs and can be saved as images."))),
                             
                             sidebarLayout(sidebarPanel(
                               
                               textOutput("no_cluster"),
                               
                               div(
                                 id = "everything_cluster_sidebar",
                                 div("Variables included in the PCA", style = "text-align: left; font-size:150%"),
                                 div(
                                   "to change these variables please return to the previous tab and choose variables to remove",
                                   style = "text-align: left; font-size:100%"
                                 ),
                                 
                                 tableOutput("pca_incl"),
                                 div("PCA Settings (please specify on the right)", style = "text-align: left; font-size:150%"),
                                 htmlOutput("pca_settings_summary"),
                                 div("5. Select a clustering method", style = "text-align: left; font-size:150%"),
                                 
                                 div(
                                   style = "margin-top: -15px;",
                                   radioButtons(
                                     "pcamethod",
                                     "",
                                     choices = c("k-means", "k-medoids"),
                                     selected = "k-means"
                                   )
                                 ),
                                 actionButton("runPCA", "Run PCA and Cluster Analysis"),
                                 textOutput("pca_available")  ,
                                 uiOutput("pca_mess")
                               )), 
                               
                               # PCA Main Panel
                               mainPanel(div(id="everything_cluster_mainpanel",
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
                                               h4("3.3 Please select a limit for the number of extreme solutions allowed in the final clustering:"),
                                               numericInput("outlier_ratio", "Outlier to cluster ratio", value = 0.5, min=0.1, max=0.9),
                                               actionButton("write_outl", "Confirm Outlier Testing")
                                             ),
                                             
                                             conditionalPanel(
                                               condition = "input.outlyn == 'No'",
                                               actionButton("write_outl", "Confirm No Outlier Testing") ),
                                             
                                             div("4. Please specify the number of principal components that shall be tested", style = "text-align: left; font-size:150%"),
                                             numericInput("pca_min", "Minimum number of PCs", value = 7),
                                             numericInput("pca_max", "Maximum number of PCs", value = 7),
                                             actionButton("pcaminmax", "Confirm Number of PCs tested"),
                                             
                                             # PCA printing Background Processes
                                             conditionalPanel(condition = "output.isElementVisible == true",div("Python Background Processes",style = "text-align: left; font-size:150%"),        
                                                              verbatimTextOutput("pca_status")))
                                         
                               ) ## PCA MAIN PANEL END
                               
                               
                             )), 
                     ## Analysis panel ####
                     tabItem(
                       tabName = "analysis",
                       titlePanel("Analysing the remaining optima"), 
                       wellPanel(p("This tab allows you to analyse the cluster outputs and plot and compare the measure implementation across the pareto solutions selected in the clustering. The table shows the subset of pareto_fitness selected as representative for the different clusters."),
                                 p("The plot on the right aligns with the one produced during the clustering. It shows where the representative solutions selected in the table lie."),
                                 p("Please be aware that the plotting of the measure allocation takes about 20 seconds.")), 
                       
                       sidebarLayout(
                         sidebarPanel(id ="analysis_sidebar",
                                      textOutput("analysis_no_clustering")), 
                         
                         mainPanel(
                           id ="main_analysis", 
                           htmlOutput("tabtext"),
                           fluidRow(
                             column(6, tags$div(textOutput("check_default"), style = "color: red;"), 
                                    div(style = "overflow-x: auto;", DTOutput("antab"))),
                             column(6,
                                    tags$div("The default plot shows the pareto front.",style = "text-align: center;font-size: 125%;"),
                                    fluidRow(
                                      column(6,checkboxInput("show_boxplot",label="show the objectives' within-cluster distribution (select from table)",value=FALSE)),
                                      column(6,checkboxInput("show_share_con",label="show the individual measures' share in total considered area (select from table)",value=FALSE))
                                    ),
                                    plotOutput("par_plot_optima"),
                                    checkboxInput("add_whole", label = "Show the whole Pareto front", value = FALSE),
                                    checkboxInput("add_sq",label = "Show status quo",value = FALSE),
                                    div(id="rev_plot2",checkboxInput("rev_box2",label="reverse x and y axes",value = FALSE))%>%hidden(),
                                    div(id="analysis_random",
                                        fluidRow(
                                          column(3,selectInput(inputId = "x_var2",label = "X-Axis", choices = NULL, multiple = F, selected=NULL)),
                                          column(3,selectInput(inputId = "y_var2",label = "Y-Axis", choices = NULL, multiple = F,selected=NULL)),
                                          column(3,selectInput(inputId = "col_var2",label = "Colour", choices = NULL, multiple = F,selected=NULL)),
                                          column(3,selectInput(inputId = "size_var2",label = "Size", choices = NULL, multiple = F,selected=NULL))
                                        ),
                                        
                                        div(
                                          style = "display: inline-block; vertical-align: top; margin-right: 0px;",
                                          textInput("par_plot_savename", label = NULL, value = "cluster results")
                                        ),
                                        div(
                                          style = "display: inline-block; vertical-align: top; margin-left: 0px;",
                                          downloadButton("download_clus_plot", "Download Plot")
                                        )))),
                           
                           actionButton("plt_opti", "Plot Optimum"), textOutput("no_row") %>% hidden(), 
                           div(id="meas_low",textOutput("meas_low")),
                           div(id="plot_spinner",
                               uiOutput("comp_map")%>% withSpinner(color = "#F7A600", hide.ui = TRUE)),
                           br(),br(),br(),
                           br(),br(),br(),
                           br(),br(),br()
                           # ,
                           # div(
                           #   style = "display: inline-block; vertical-align: top; margin-right: 0px;",
                           #   textInput("meas_plot_savename", label = NULL, value = "Measure implementation")
                           # ),
                           # div(
                           #   style = "display: inline-block; vertical-align: top; margin-left: 0px;",
                           #   downloadButton("download_meas_plot", "Download Plot")
                           # )
                           # 
                           
                         )),
                       tags$script(HTML("
                        function toggleSidebar(show) {
                          if (show) {
                            document.getElementById('analysis_sidebar').style.display = 'block';
                            document.getElementById('main_analysis').classList.remove('main-panel-full-width');
                            document.getElementById('main_analysis').classList.add('main-panel');
                          } else {
                            document.getElementById('analysis_sidebar').style.display = 'none';
                            document.getElementById('main_analysis').classList.remove('main-panel');
                            document.getElementById('main_analysis').classList.add('main-panel-full-width');
                          }
                        }
                         "))),
                     
                     ## AHP ####
                     tabItem(
                       tabName = "ahp",
                       titlePanel("Analytical Hierarchy Process"),
                       
                       wellPanel( p("This tab allows you to run a different approach (AHP) to selecting those pareto optima best matching your preferences.
                     AHP is a decision making tool that helps you prioritise different objectives by comparing them in pairs."),
                                  p(HTML("Clicking <strong>analyse objectives</strong> allows you to receive a broad overview of the different objectives' relationships.")),
                                  p("If you want you can limit the objective ranges under 2."),
                                  p("Under 3. you can compare objectives two at a time and and decide which objective is more important and by how much. 
                     ParetoPick-R will assign weights to each objective based on your inputs and check how consistent your choices are. 
                     The respective best choice is plotted on the right and you can decide whether 
                     it should be selected from the whole pareto front or from the subset of cluster results.")),
                       
                       sidebarLayout(
                         sidebarPanel(
                           
                           textOutput("nothing_ran_ahp"),
                           div(id = "ahp_analysis",
                               # "1. Examining the relationship between the objectives.",style = "text-align: left; font-size:150%",
                               
                               # fluidRow(
                                 # column(6, uiOutput("criterion1_ui")),
                                 # column(6, uiOutput("criterion2_ui")),
                                 
                                 # column(6, actionButton("plot_sc", label = "analyse objectives"))
                               # ),
                               
                               fluidRow(
                                 
                                 column(10,
                                        div("1. Limiting the objective space (optional)"),
                                        sliderInput(inputId = "obj1_ahp", label = "Objective 1:", min = 0, max = 100, value = c(0, 100), width = "110%"),
                                        sliderInput(inputId = "obj2_ahp", label = "Objective 2:", min = 0, max = 100, value = c(0, 100), width = "110%"),
                                        sliderInput(inputId = "obj3_ahp", label = "Objective 3:", min = 0, max = 100, value = c(0, 100), width = "110%"),
                                        sliderInput(inputId = "obj4_ahp", label = "Objective 4:", min = 0, max = 100, value = c(0, 100), width = "110%"))
                               )),
                           br(),
                           
                           # div(id = "ahp_weights",
                           #     "3. Deciding on priorities and weighting the objectives.",
                           #     style = "text-align: left; font-size:150%",
                           #     uiOutput("sliders_ui")),
                           
                           div(id="sel_wgt","Selected Weights", style = "text-align: center; font-size: 150%;",
                               div(tableOutput("weights_output"), style = "margin: 0 auto; width: fit-content;")),
                           uiOutput("consistency_check"),tableOutput("which_inconsistency")
                         ),
                         
                         mainPanel(
                           
                           fluidRow(
                             column(
                               width = 12,
                               actionButton("ahp_card1", "Show Card 1"),
                               actionButton("ahp_card2", "Show Card 2"),
                               actionButton("ahp_card3", "Show Card 3"),
                               actionButton("ahp_card4", "Show Card 4"),
                               actionButton("ahp_card5", "Show Card 5"),
                               actionButton("ahp_card6", "Show Card 6")
                             )
                           ),
                           fluidRow(
                             uiOutput("card1_ui")%>%hidden(),
                             uiOutput("card2_ui")%>%hidden(),
                             uiOutput("card3_ui")%>%hidden(),
                             uiOutput("card4_ui")%>%hidden(),
                             uiOutput("card5_ui")%>%hidden(),
                             uiOutput("card6_ui")%>%hidden()
                           ),
                           
                           
                           fluidRow(
                             column(9, plotOutput("scatterPlot")), # 9/12 of the width for the plot
                             column(3, tableOutput("relation")) # 3/12 of the width for the table
                           ),
                           
                           div(id = "pareto_weighted",
                               "Best Option under selected weighting",
                               style = "text-align: center; font-size: 150%;"
                               ),
                           useShinyjs(),
                           
                            div(tableOutput("best_option_output"), style = "margin: 0 auto; width: fit-content;"),
                                                                     
                           checkboxInput("save_ahp",label = "Click here to save the selected optimum to the output folder (selected_optima.csv)",value=F)%>%hidden(),
                         
                               div(id = "random_ahp",
                                  checkboxInput("best_cluster", label = "Best option among cluster solutions", value = FALSE),
                                  style = "margin: 0 auto; width: fit-content;font-size: 100%;"
                               ),
                       
                           plotOutput({"weights_plot"}),
                           div(id="random_ahp2", fluidRow(
                             column(3, selectInput(inputId = "x_var",label = "X-Axis", choices = NULL, multiple = F, selected=NULL)),
                             column(3,selectInput(inputId = "y_var",label = "Y-Axis", choices = NULL, multiple = F,selected=NULL)),
                             column(3,selectInput(inputId = "col_var",label = "Colour", choices = NULL, multiple = F,selected=NULL)),
                             column(3,selectInput(inputId = "size_var",label = "Size", choices = NULL, multiple = F,selected=NULL))),
                             
                             checkboxInput("show_extra_dat", label = "Show cluster solutions", value = FALSE),
                             checkboxInput("show_status_quo", label = "Show Status Quo", value = FALSE),
                             div(id="rev_plot3",checkboxInput("rev_box3",label="reverse x and y axes",value = FALSE))%>%hidden(),
                             
                             div(
                               style = "display: inline-block; vertical-align: top; margin-right: 0px;",
                               textInput("weights_plot_savename", label = NULL, value = "AHP results")
                             ),
                             div(
                               style = "display: inline-block; vertical-align: top; margin-left: 0px;",
                               downloadButton("download_weights_plot", "Download Plot")
                             ),
                             br(),
                             br(),
                             br(),
                             div(style = "display: inline-block; vertical-align: top; margin-left: 0px;",
                                 actionButton("plt_bo", "Plot Measure Implementation under best option"),
                             ),
                             div(id="ahp_spinner", 
                                 uiOutput("plt_bo_measure")
                                 %>% withSpinner(color = "#F7A600", hide.ui = TRUE))
                             
                           )
                           
                         )
                       )
                       
                     )
                   )
    )
  )
