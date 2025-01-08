############################### UI #################################
# comments:
# Project: Clustering Pareto solutions/Multi-objective visualisation
# author: cordula.wittekind@ufz.de
####################################################################
ui <-
  dashboardPage(
    dashboardHeader(title="ParetoPick-R"),
    dashboardSidebar(
      sidebarMenu(id = "tabs",
                  menuItem("Introduction",tabName = "intro", icon = icon("home"),selected=TRUE),
                  menuItem("Data Preparation", icon=icon("file",lib = "font-awesome"),tabName = "data_prep"),
                  menuItem("Visualising the Pareto Front",tabName = "play_around",icon = icon("dashboard")),
                  menuItem("Configure Clustering", tabName = "configure", icon = icon("cog")),

                  conditionalPanel(
                    condition = "input.show_tabs == 'show'",
                    menuItem("Correlation Analysis",icon=icon("random", lib="font-awesome"), tabName = "correlation_analysis"),
                    menuItem("PCA & kmeans/kmedoids",icon=icon("project-diagram", lib="font-awesome"), tabName = "pca")
                  ),

                  menuItem("Cluster Analysis", icon = icon("th"),tabName = "analysis"),
                  menuItem("AHP",icon=icon("sliders-h", lib="font-awesome"),tabName = "ahp"),
      dropdownButton(
        inputId = "glossary_button",
        label = "Glossary",
        icon = icon("book"),
        circle = FALSE,
        status = "primary",

        tags$div(

          tags$ul(
            tags$li(
              tags$b("share_con:"), " Each measure's share in area considered for implementation."
            ),
            tags$li(
              tags$b("moran:"), " The median spatial autocorrelation between HRUs allocated to each implemented measure."
            ),
            tags$li(
              tags$b("channel_frac:"), " The median fraction of water under each implemented measure that is routed directly into the channel."
            ),
            tags$li(
              tags$b("linE:"), " The ratio between structural and management measures."
            ),
            tags$li(
              tags$b("lu_share:"), " The share of land use measures in catchment area considered for implementation."
            )
          )
        )
          )))
    ,
    dashboardBody(  

    tags$style(HTML('
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
                                    background-color: #9eb1cf !important;
                                  }

                                  .sidebar-menu li a[data-value="play_around"] {
                                    background-color: #95C11F !important;
                                  }

                                  .sidebar-menu li a[data-value="play_around"]:hover {
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

                                  /*glossary text color of content */
                                  .dropdown-menu {
                                    color: #333;  /* Dark text color */
                                    background-color: #f8f9fa;  /* Light background */
                                  }

                                   ul {
                                    padding-left: 10px;
                                    list-style-position: inside;
                                    }
                                    ul li {
                                    margin-left: 0;
                                    }
                                    ul li::before { font-size: 8px;  /* glossary reduce bullet point size */
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
                                    background-color: #83D0F5 !important;
                                  }

                                  /* Hover effect for the toggle button */
                                  .skin-blue .main-header .navbar .sidebar-toggle:hover {
                                    background-color: #83D0F5;
                                  }

                                  /* General body background color and font as Montserrat */
                                  .content-wrapper, 
                                  .right-side {
                                   background-color: unset;
                                   font-family: "Montserrat", sans-serif;
                                  }
                                  
                                  .main-sidebar, .left-side {
                                   font-family: "Montserrat", sans-serif;
                                   font-size: 0.95em; 
                                  }

                                  .content-wrapper {
                                    min-height: 500px !important;
                                    height: auto !important;
                                  }

                                  body, .wrapper{
                                    min-height: auto !important;
                                    height: auto !important;
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
                                  .content {
                                    /*min-height: 300vh; */
                                    display: flex;
                                  }

                                  .wrapper {
                                    background-color: unset !important;
                                  }

                                  /* Slider element styling */
                                  .irs-grid-text {
                                    font-size: 13px;
                                  }

                                /* Main panel full-width adjustment */
                                  .main-panel-full-width {
                                   margin-left: 0 !important;
                                   width: 100% !important;
                                  }

                                /* Main panel size relative to sidebar width */
                                  .main-panel {
                                   margin-left: 250px;
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

                                   /* datatable in Analysis tab with vertical lines */
                                  .dataTable td.border-column {
                                   border-right: 1px solid #03597F;
                                  }

                                      ')),

                     useShinyjs(),
                   tags$script(src = "iframeResizer.contentWindow.min.js"),
                  
                   tags$script(HTML("
                                    $(document).on('shiny:value', function(event) {
                                      function removeMinusSigns() {
                                        $('.irs-grid-text, .irs-to, .irs-from').each(function() {
                                          $(this).text($(this).text().replace('-', ''));
                                        });
                                     }

                                      removeMinusSigns();

                                   $(document).on('shiny:inputchanged', function(event) {
                                        if (event.name.startsWith('obj')) { // last tab ahp slider
                                           setTimeout(removeMinusSigns, 5);
                                        }
                                      });

                              $(document).on('shiny:inputchanged', function(event) {
                                        if (event.name.startsWith('ran')) { // configure tab range slider
                                          setTimeout(removeMinusSigns, 5);
                                        }
                                      });


                                   $(document).on('input change', '.irs-with-grid, .irs-to, .irs-from', function() {
                                        setTimeout(removeMinusSigns, 7);
                                      });
                                    });
                                 ")),



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
                               tags$ol(tags$li("A clustering algorithm based on a Principal Component Analysis (PCA) and kmeans/kmedoids.
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
                               p(HTML("The fourth tab <strong>Configure Clustering</strong> allows to perform the clustering with default settings or to jump to the optional tabs for manual clustering.")),
                               
                               p(HTML("
                               <ul>
                                 <li>The tab <strong>Clustering Part 1 - Correlation Analysis</strong> can only be accessed if manual clustering is chosen in the Configure Clustering tab. It allows to assess and alter variables considered in the subsequent clustering.</li>
                                 <li>The tab <strong>Clustering Part 2 - PCA & kmeans/kmedoids</strong> provides the possibility to adapt, modify and finally perform the clustering process.</li>
  
                               </ul>
                                      ")),
                               
                               p(HTML("The <strong>Cluster Analysis</strong> tab lets the user plot the optima remaining after the clustering. Each of these optima is representative for one cluster.")),
                               p(HTML("The tab <strong>AHP - Analytical Hierarchy Process</strong> allows to determine priorities across the pareto front in a different way through assigning weights across the optima. It is possible to combine the clustering results with the AHP.")),
                               br(),br(),
                               p(HTML("To ensure compatibility with algorithms (e.g. CoMOLA) designed for maximisation, some projects used negative numbers. Please note that, unless an objective uses mixed signs, this app omits the minus sign of these values. The interpretation however remains unchanged."))


                             )
                             )),

                     ## DATA PREP PANEL #####

               tabItem(tabName = "data_prep",
                       
                             titlePanel("OPTAIN Data Preparation"),

                             wellPanel(  p(HTML("This tab requires you to provide the optimisation outputs.
                                                 Please refer to the Readme for examples of their structure.
                                                 You can provide a limited set of outputs to only analyse the Pareto front in the next tab.
                                                 Alternatively, you can upload more data and prepare the variables for the subsequent correlation and cluster analysis
                                                 by clicking <strong>Check Files</strong> and
                                                (if all files have been found) <strong>Run Prep</strong>. Please be aware that the preparation might take up to 15 minutes."))),

                             mainPanel(
                               

                               div(p("For being able to plot and analyse the Pareto front, please provide pareto_fitness.txt as well as the objective names.
                                     If you would like to plot the status quo, sq_fitness.txt is also required:"),style = "text-align: left; font-size:140%"),
                               # div(id="fitness_avail",

                               div("pareto_fitness.txt",style = "text-align: left; font-size:115%;",
                                   div(style = "margin-top: -15px;",fileInput("par_fit", "", accept = ".txt"),
                                       div(style= "vertical-align: top; margin-top: -15px;",actionButton("save_paretofit","Save")))),


                               br(),
                               div("sq_fitness.txt (optional)",style = "text-align: left; font-size:115%;",
                                   div(style = "margin-top: -15px;",fileInput("sq_in", "", accept = ".txt"),
                                   div(style= "vertical-align: top; margin-top: -15px;",actionButton("save_sq_in","Save")))),

                               br(),
                               div( "The objective names should align with the first four columns of the pareto_fitness.txt file:",
                                 style = "text-align: left; font-size:115%",
                                 div("*Please note, you can only change these names later if you perform a Hard Reset below.",
                                     style="text-align: left; font-size:90%"),
                                 textInput("short1", "Objective 1\n (Column 1)"),
                                 textInput("short2", "Objective 2\n (Column 2)"),
                                 textInput("short3", "Objective 3\n (Column 3)"),
                                 textInput("short4", "Objective 4\n (Column 4)"),
                                 actionButton("save_par_fiti", "Save")),


                               br(),

                               div(id="units",
                                   "If you want you can supply the objectives' units, you can change them anytime:",
                                   style= "text-align: left; font-size:115%",
                                   textInput("unit1","unit Objective 1", value = ""),
                                   textInput("unit2","unit Objective 2", value = ""),
                                   textInput("unit3","unit Objective 3", value = ""),
                                   textInput("unit4","unit Objective 4", value = ""),
                                   actionButton("save_unit","Save")),

                               br(),

                               div(textOutput("can_visualise"),style = "text-align: left; font-size:115%"),

                               br(),

                               ############################################################# rest not needed for non-OPTAIN projects


                               hr(style = "border-top: 2px solid #03597F;"),  # Horizontal line with custom styling
                               br(),
                               p("If you also want to run the subsequent correlation and cluster analysis, please provide the following files. Their names have to align with what is given here:",style =  "text-align: left; font-size:140%"),

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


                               actionButton("files_avail", "Check Files"),


                               uiOutput("fileStatusMessage"),


                               hr(),

                               div(id="runprep_show",p("Run Preparation Script when ready (depending on the size of the shapefiles this can take up to 10 minutes)",style =  "text-align: left; font-size:150%"),
                                   actionButton("runprep", "Run Prep"))%>%hidden,
                               uiOutput("scriptdp"),
                               br(),br(),
                               div("Please select those measures that are small and require a buffer to enhance their visibility in maps.", style = "text-align: left; font-size:120%; margin-top: 10px;"),

                               div(
                                 style = "display: inline-block; vertical-align: top; margin-right: 0px; margin-top: 5px",
                               selectInput("buffies",label  = "select measures",choices=NULL,selected=NULL,multiple = T)),
                               div(
                                 style = "display: inline-block; vertical-align: top; margin-left: 0px; margin-top: 30px",
                               actionButton("save_buff","Save buffers"))
                               ,
                               br(),br(),


                               div(id="range_title","Range of objective values given in pareto_fitness.txt:",style = "text-align: left; font-size:120%"),
                               tableOutput("obj_conf"),

                               br(),br(),br(),

                               div(id="reset", htmlOutput("reset_prompt"),
                                   actionButton("reset_btn", "Hard Reset",style = "color: white; background-color: red; font-size: 15px; padding: 8px 8px; border-radius: 5px;"),
                                   textOutput("reset_status"))

                             )# DATA PREP MAIN PANEL END
                     ),
               ### PLAY AROUND TAB ####
               tabItem(tabName = "play_around",
                       titlePanel("Visualising the Optimisation Output"),

                       wellPanel(    p("This tab plots the pareto front in a few different ways
                              and lets you explore the effects of reduced objective ranges.
                                       You can select specific points/optima on the pareto front by clicking on them,
                                       then you can plot the map of the respective NSWRM plan."),
                              p("You can also select optima in the line plot and analyse their location in the objective space.")),

                       sidebarLayout(


                         sidebarPanel( width = 3,


                                       textOutput("uploaded_pareto"),

                                       div(id="play_sidebar",
                                         column(12,
                                                div("Objective Range", style = "text-align: left; font-size:150%; margin-top: 10px;"),

                                                sliderInput(inputId = "obj1", label=  "Objective 1:", min = 0, max = 1, value = c(0,1), step = 0.01,width = "120%"),
                                                sliderInput(inputId = "obj2", label = "Objective 2:", min = 0, max = 1, value = c(0,1), step = 0.01,width = "120%"),
                                                sliderInput(inputId = "obj3", label = "Objective 3:", min = 0, max = 1, value = c(0,1), step = 0.01, width = "120%"),
                                                sliderInput(inputId = "obj4", label = "Objective 4:", min = 0, max = 1, value = c(0,1), step = 0.01,width = "120%"),
                                                tags$div(textOutput("ensure_sel"), style = "color: red;"),

                                                tags$p(
                                                  tags$strong("Please Note:"),
                                                  "For some of the visualisations and analyses in this tool, the objectives have been scaled to between 0 and 1 for easier comparison.
                                             1 aligns with the best achievable outcome while 0 aligns with the worst."
                                                ),tags$p("The app does not display negative values, where an objective range covers both negative and positive values, a sign is added.")
                                         ),
                                         div("Frequency of area implemented", style = "text-align: left; font-size:120%; margin-top: 10px;"),
                                         uiOutput("freq_map_play")%>%hidden(),

                                       ), br(),



                         ),
                         mainPanel(width=9,
                           tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #ffc61e ;border-top: 1px solid #ffc61e ;border-bottom: 1px solid #ffc61e;}.js-irs-1 .irs-from, .js-irs-1 .irs-to, .js-irs-1 .irs-single { font-size: 13px;background: #ffc61e !important }")),
                           tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #009ade ;border-top: 1px solid #009ade ;border-bottom: 1px solid #009ade;}.js-irs-2 .irs-from, .js-irs-2 .irs-to, .js-irs-2 .irs-single { font-size: 13px;background: #009ade !important }")),
                           tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #aF58ba ;border-top: 1px solid #aF58ba ;border-bottom: 1px solid #aF58ba;}.js-irs-3 .irs-from, .js-irs-3 .irs-to, .js-irs-3 .irs-single { font-size: 13px;background: #aF58ba !important }")),
                           tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #f28522 ;border-top: 1px solid #f28522 ;border-bottom: 1px solid #f28522;}.js-irs-4 .irs-from, .js-irs-4 .irs-to, .js-irs-4 .irs-single { font-size: 13px;background: #f28522 !important }")),

                           tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #ffc61e ;border-top: 1px solid #ffc61e ;border-bottom: 1px solid #ffc61e;}.js-irs-5 .irs-from, .js-irs-5 .irs-to, .js-irs-5 .irs-single { font-size: 13px;background: #ffc61e !important }")),
                           tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar {background: #009ade ;border-top: 1px solid #009ade ;border-bottom: 1px solid #009ade;}.js-irs-6 .irs-from, .js-irs-6 .irs-to, .js-irs-6 .irs-single { font-size: 13px;background: #009ade !important }")),
                           tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar {background: #aF58ba ;border-top: 1px solid #aF58ba ;border-bottom: 1px solid #aF58ba;}.js-irs-7 .irs-from, .js-irs-7 .irs-to, .js-irs-7 .irs-single { font-size: 13px;background: #aF58ba !important }")),
                           tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar {background: #f28522 ;border-top: 1px solid #f28522 ;border-bottom: 1px solid #f28522;}.js-irs-8 .irs-from, .js-irs-8 .irs-to, .js-irs-8 .irs-single { font-size: 13px;background: #f28522 !important }")),

                           tags$style(HTML(".js-irs-9 .irs-single, .js-irs-9 .irs-bar-edge, .js-irs-9 .irs-bar {background: #ffc61e ;border-top: 1px solid #ffc61e ;border-bottom: 1px solid #ffc61e;}.js-irs-9 .irs-from, .js-irs-9 .irs-to, .js-irs-9 .irs-single { font-size: 13px;background: #ffc61e !important }")),
                           tags$style(HTML(".js-irs-10 .irs-single, .js-irs-10 .irs-bar-edge, .js-irs-10 .irs-bar {background: #009ade ;border-top: 1px solid #009ade ;border-bottom: 1px solid #009ade;}.js-irs-10 .irs-from, .js-irs-10 .irs-to, .js-irs-10 .irs-single { font-size: 13px;background: #009ade !important }")),
                           tags$style(HTML(".js-irs-12 .irs-single, .js-irs-11 .irs-bar-edge, .js-irs-11 .irs-bar {background: #aF58ba ;border-top: 1px solid #aF58ba ;border-bottom: 1px solid #aF58ba;}.js-irs-11 .irs-from, .js-irs-11 .irs-to, .js-irs-11 .irs-single { font-size: 13px;background: #aF58ba !important }")),
                           tags$style(HTML(".js-irs-13 .irs-single, .js-irs-12 .irs-bar-edge, .js-irs-12 .irs-bar {background: #f28522 ;border-top: 1px solid #f28522 ;border-bottom: 1px solid #f28522;}.js-irs-12 .irs-from, .js-irs-12 .irs-to, .js-irs-12 .irs-single { font-size: 13px;background: #f28522 !important }")),
                           tags$style(HTML("#actual_plt_play_measure {width: 450px;height: 550px;margin-bottom: -150px; } ")),
                           tags$style(HTML("#freq_map_play {width: 100%; max-width: 100%; height: 550px; margin-bottom: -150px; } ")),

                           tags$style(HTML(".spinspin { display: inline-block;
                                                        width: 20px;
                                                        height: 20px;
                                                        border: 2px solid rgba(0, 0, 0, 0.1);
                                                        border-radius: 50%;
                                                        border-top-color: #F7A600;
                                                        animation: spin 0.6s linear infinite;
                                                        }
                                                      @keyframes spin {to { transform: rotate(360deg);}}

                                                  ")) ,

                           div(id = "tab_play1",

                               div("Pareto Plot", style = "text-align: left; font-size:150%"),
                               plotOutput("first_pareto",click="clickpoint"),
                               checkboxInput("add_sq_f",label = "Show status quo",value = FALSE),
                               checkboxInput("unit_add1",label = "Show units",value = TRUE),
                               div(id="rev_plot",checkboxInput("rev_box",label="reverse x and y axes",value = FALSE))%>%hidden(),
                               fluidRow(
                                 column(3,selectInput(inputId = "x_var3",   label = "X-Axis", choices = NULL, multiple = F, selected=NULL)),
                                 column(3,selectInput(inputId = "y_var3",   label = "Y-Axis", choices = NULL, multiple = F, selected=NULL)),
                                 column(3,selectInput(inputId = "col_var3", label = "Colour", choices = NULL, multiple = F, selected=NULL)),
                                 column(3,selectInput(inputId = "size_var3",label = "Size",   choices = NULL, multiple = F, selected=NULL))
                               ),

                               br(),
                               div(
                                 style = "display: inline-block; vertical-align: top; margin-right: 0px;",
                                 textInput("fp_plot_savename", label = NULL, value = "pareto")
                               ),
                               div(
                                 style = "display: inline-block; vertical-align: top; margin-left: 0px;",
                                 downloadButton("download_fp_plot", "Download pareto plot")),
                               uiOutput("clickpoint_map") ,#button
                                   uiOutput("plt_play_measure"),#map placeholder
                               withSpinner(
                                 uiOutput("spinner_play"),
                                 type = 4  , color = "#F7A600"
                               ),

                               div(id="download_play_id",
                                   div(
                                     style = "display: inline-block; vertical-align: top; margin-right: 0px; margin-top: 5px;",
                                     textInput("meas_play_savename", label = NULL, value = "measure_implementation")
                                   )
                                   ,
                                   div(
                                 style = "display: inline-block; vertical-align: top; margin-left: 0px; margin-top: 5px;",
                                       downloadButton("download_pm", "Download map"),
                                 div(
                                   id = "spinner_download_play",style = "display: none;",
                                   style = "display: none; vertical-align: top; margin-left: 0px; margin-top: 5px;",
                                   div(class = "spinspin")
                                  ) )
                               )%>%hidden(),

                              br(),
                              br(),
                              div("Number of individual measures implemented in selection compared to full front",
                                  style = "display: flex; justify-content: center; font-size:150%"),

                              div(style="display: flex; flex-direction: column; align-items: center;",
                                  tags$h4("Selected range"),
                                     tableOutput("aep_tab_full"),
                                  conditionalPanel(
                                    condition = "output.selectionmade",
                                    div(
                                      style = "display: flex; flex-direction: column; align-items: center;",
                                    tags$h4("Optiumum (Selected in Pareto plot)"),
                                    tableOutput("aep_tab_one")
                                    ))),

                              br(),
                              br(),

                               hr(style = "border-top: 2px solid #03597F;"),
                               br(),
                               fluidRow(
                                 column(12,
                                        fluidRow(column(6, div("Selected Objective Ranges (scaled)", style = "text-align: left; font-size:150%"),
                                                        tableOutput("sliders")),
                                                 column(6, div("Selected Objective Ranges (absolute)", style = "text-align: left; font-size:150%"),
                                                        tableOutput("sliders_abs"),
                                                        div("*the percentage values signify the difference between the full range and the current max and min selection.", style = "text-align: left; font-size:100%"))),
                                        fluidRow(column(6,
                                                        div("Optimum (selected in line plot)", style = "text-align: left; font-size:150%"),
                                                        tableOutput("click_info"),
                                                        checkboxInput("save_click_line",label = "Click here to save the selected optimum to the output folder (selected_optima.csv)",value=F)%>%hidden()),

                                                 column(6,
                                                        div("Maximum Objective Ranges (absolute)",style = "text-align: left; font-size:150%"),
                                                        tableOutput("whole_range"),
                                                        div("*objectives optimised across negative and positive ranges are shown with signs. All other objectives are given without signs.", style = "text-align: left; font-size:100%"))
                                        ))),




                           div(id = "tab_play2",div("Parallel Axis plot", style = "text-align: left; font-size:150%"),
                               plotOutput("linePlot",click="clickline"),
                               checkboxInput("plt_sq", label = "Show status quo", value = FALSE)),
                           
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

                   ## CONFIGURE CLUSTERING PANEL - USER DECISION FOR HIDING OR SHOWING correlation AND clustering ####

                     tabItem(tabName = "configure",

                             titlePanel("Configure Cluster Settings"),

                     mainPanel(
                       textOutput("config_needs_var"),


              div(id = "config_all",
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
                  )

                    )##################CONFIG MAIN PANEL END
                     ),

                     ## CORRELATION ANALYSIS PANEL ####
                tabItem(tabName = "correlation_analysis",
                             titlePanel("Clustering Part 1 - Correlation Analysis"),

                             wellPanel( p(HTML("A correlation analysis is needed as correlation among variables can skew cluster results. Therefore, please click <strong>Run Correlation Analysis</strong>.
                             Based on the levels of correlation you can select those variables you would like to exclude from the subsequent clustering. Select them and then click <strong>Confirm Selection</strong>. You can come back to this tab to change the selection of variables later.
                                        It is also possible to run the clustering across all variables and select no variables to exclude in this tab, however please always click <strong>Confirm Selection</strong>."))),
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
                                                      choiceNames = c("share_con (*)",
                                                                      "Moran's I",
                                                                      "channel_frac (*)",
                                                                      "linE",
                                                                      "lu_share"),
                                                      choiceValues=c("share_con","moran","channel_frac","linE","lu_share"),
                                                      selected = c("share_con","moran","channel_frac","linE","lu_share")),

                                   textOutput("numbercorr"),
                                   div("2. Perform the Correlation Analysis", style = "text-align: left; font-size:150%"),
                                   actionButton("run_corr", "Run Correlation Analysis"),

                                   div("3. Choose threshold for correlation",style = "text-align: left; font-size:150%"),
                                   div(style = "margin-top: -15px;",shinyWidgets::sliderTextInput(inputId = "thresh", label= "",choices = seq(0.65,0.95,0.05), selected=0.75)),

                                   div("4. Choose variables that shall be excluded from the Cluster Analysis",style = "text-align: left; font-size:150%"),
                                   selectInput(inputId = "excl",label = "variables to exclude", choices = NULL, multiple = TRUE),

                                   div(id="show_conf","5. Please confirm your choice before proceeding to the next tab.",style = "text-align: left; font-size:150%"
                                       ,actionButton("confirm_selection", "Confirm Selection and go to next tab"))%>%hidden,
                                   # print confirmed selection
                                   uiOutput(outputId = "confirmed_selection")

                                 )),
                               mainPanel(div(id="corr_content",

                                             # Display the selected elements from the checkbox group
                                             # div("Selected Variables", style = "text-align: left; font-size:150%"),
                                             # tableOutput("selements"),

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

                             wellPanel(  p(HTML("This tab requires you to decide on the cluster settings. After selecting how the objectives shall be plotted, deciding on the axis titles and confirming the number of PCAs tested, the clustering can be run with default settings.
                                         Selecting <strong>Yes</strong> under either 2. or 3. allows to change those default settings and test a variable number of clusters and outlier considerations.")),
                                         p(HTML("The cluster outputs open in separate tabs and can be saved as images."))),

                             sidebarLayout(sidebarPanel(



                               textOutput("no_cluster"),

                               div(id="everything_else_clustering",
                                 div("5. Select a clustering method", style = "text-align: left; font-size:150%"),

                               div(
                                 style = "margin-top: -15px;",
                                 radioButtons(
                                   "pcamethod",
                                   "",
                                   choices = c("k-means", "k-medoids"),
                                   selected = "k-means"
                                 )
                               )),
                               actionButton("runPCA", "Run PCA and Cluster Analysis"),
                               withSpinner(uiOutput("cluster_spin"),color= "#F7A600"),  # Spinner style (1-8)
                               textOutput("pca_available")  ,
                               uiOutput("pca_mess"),
                               div(
                                 id = "everything_cluster_sidebar",
                                 div("Variables included in the PCA", style = "text-align: left; font-size:150%"),
                                 div(
                                   "to change these variables please return to the previous tab and choose variables to remove",
                                   style = "text-align: left; font-size:100%"
                                 ),

                                 tableOutput("pca_incl"),
                                 div("PCA Settings (please specify on the right)", style = "text-align: left; font-size:150%"),
                                 htmlOutput("pca_settings_summary")
                               )),

                               # PCA Main Panel
                               mainPanel(div(id="everything_cluster_mainpanel",
                                             div("Refine PCA Settings here and click Confirm Choice, Confirm Axis Labels and Confirm Number of PCs tested, then click Run Principal Component Analysis on the left", style = "text-align: left; font-size:150%"),

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
                                             #number of PCAs
                                             div("2. Please specify the number of principal components that shall be tested", style = "text-align: left; font-size:150%"),
                                             numericInput("pca_min", "Minimum number of PCs", value = 7),
                                             numericInput("pca_max", "Maximum number of PCs", value = 7),
                                             actionButton("pcaminmax", "Confirm Number of PCs tested"),



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


                                             #  Cluster number
                                             div("4. Shall several number of clusters be tested?", style = "text-align: left; font-size:150%"),
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

                                             # PCA printing Background Processes
                                             conditionalPanel(condition = "output.isElementVisible == true",div("Python Background Processes",style = "text-align: left; font-size:150%"),
                                                              verbatimTextOutput("pca_status")))

                               ) ## PCA MAIN PANEL END


                             )),
                     ## Analysis panel ####
                     tabItem(
                       tabName = "analysis",
                       titlePanel("Analysing the remaining optima"),
                       wellPanel(p("This tab allows you to analyse the cluster outputs and plot and compare the measure implementation across the pareto solutions selected in the clustering. The table shows those optima selected as representative for the different clusters. The plot on the right aligns with the one produced during the clustering.
                       It shows the location of the optima selected in the table. Please be aware that plotting the measure allocation takes around 20 seconds.")),

                       sidebarLayout(
                         sidebarPanel(id ="analysis_sidebar",
                                      textOutput("analysis_no_clustering")),

                         mainPanel(
                           textOutput("analysis_needs_var"),
                           id ="main_analysis",
                           div(id="analysis_random", #the whole right side of plots and extra stuff under plot can be hidden
                           fluidRow(
                             column(6,
                                    div(id="table_an_title","Optima Representative for Clusters",style="text-align; center;font-size: 150%;"),
                                    htmlOutput("tabtext"),

                                    tags$div(textOutput("check_default"), style = "color: #D10000;"),

                                    div(style = "overflow-x: auto;", DTOutput("antab"))),
                             column(6,
                                    tags$div("Select among plots.",style = "text-align: center;font-size: 125%;"),
                                    fluidRow(
                                      column(3,checkboxInput("show_pareto",label="Plot 1: Pareto solutions representative for the clusters.",value=TRUE)),
                                      column(3,checkboxInput("show_pca_vs_var",label="Plot 2: Objectives (X-axis) versus cluster variables (Y-axis, colour, size) (select from drop down)",value=FALSE)),
                                      column(3,checkboxInput("show_boxplot",label="Plot 3: The objectives' within-cluster distribution (select one from table)",value=FALSE)),
                                      column(3,checkboxInput("show_share_con",label="Plot 4: The individual measures' share in total considered area (select several from table)",value=FALSE))


                                      ),
                                    plotOutput("par_plot_optima"),


                                   conditionalPanel( # pareto plot drop down elements

                                     condition = "input.show_pareto == true",

                                                     checkboxInput("add_whole", label = "Show the whole Pareto front", value = FALSE),
                                    checkboxInput("add_sq",label = "Show status quo",value = FALSE),
                                    checkboxInput("unit_add2",label = "Show units",value = TRUE),
                                    div(id="rev_plot2",checkboxInput("rev_box2",label="reverse x and y axes",value = FALSE))%>%hidden(),

                                        fluidRow(
                                          column(3,selectInput(inputId = "x_var2",label = "X-Axis", choices = NULL, multiple = F, selected=NULL)),
                                          column(3,selectInput(inputId = "y_var2",label = "Y-Axis", choices = NULL, multiple = F,selected=NULL)),
                                          column(3,selectInput(inputId = "col_var2",label = "Colour", choices = NULL, multiple = F,selected=NULL)),
                                          column(3,selectInput(inputId = "size_var2",label = "Size", choices = NULL, multiple = F,selected=NULL))
                                        )),


                                   conditionalPanel( #decision vs. objective space drop down elements
                                     condition = "input.show_pca_vs_var == true",
                                     checkboxInput("flip",label = "flip x and y axes",value = FALSE),

                                     fluidRow(
                                     column(3,selectInput(inputId = "x_var_pcs_vs", label = "X-Axis (Objective)", choices = NULL, multiple =F, selected = NULL)),
                                     column(3,selectInput(inputId = "y_var_pcs_vs", label = "Y-Axis (Cluster variable)", choices = NULL, multiple =F, selected = NULL)),
                                     column(3,selectInput(inputId = "col_var_pcs_vs", label = "Colour (Cluster variable)", choices = NULL, multiple =F, selected = NULL)),
                                     column(3,selectInput(inputId = "size_var_pcs_vs", label = "Size (Cluster variable)", choices = NULL, multiple =F, selected = NULL))

                                   )),

                                        div(
                                          style = "display: inline-block; vertical-align: top; margin-right: 0px;",
                                          textInput("par_plot_savename", label = NULL, value = "cluster results")
                                        ),
                                        div(
                                          style = "display: inline-block; vertical-align: top; margin-left: 0px;",
                                          downloadButton("download_clus_plot", "Download Plot")
                                        )))),

                           actionButton("plt_opti", "Plot map of measure implementation under selected optima"), textOutput("no_row") %>% hidden(),
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
                     AHP is a decision making tool that helps you prioritise different objectives by comparing them in pairs.
                                    If you want you can limit the objective ranges under 1."),
                                  p("Under 2. you can compare objectives two at a time and and decide which objective is more important and by how much.
                     ParetoPick-R will assign weights to each objective based on your inputs and check its consistency.
                     The respective best choice is plotted below and you can decide whether
                     it should be selected from the whole pareto front or from the subset of cluster results.")),

                       sidebarLayout(
                         sidebarPanel(width=3,

                           textOutput("nothing_ran_ahp"),
                           div(id = "ahp_analysis",

                               fluidRow(

                                 column(12,
                                        div("1. Limiting the objective space (optional)",style = "text-align: center; font-size: 120%;"),
                                        sliderInput(inputId = "obj1_ahp", label = "Objective 1:", min = 0, max = 100, value = c(0, 100), width = "120%"),
                                        sliderInput(inputId = "obj2_ahp", label = "Objective 2:", min = 0, max = 100, value = c(0, 100), width = "120%"),
                                        sliderInput(inputId = "obj3_ahp", label = "Objective 3:", min = 0, max = 100, value = c(0, 100), width = "120%"),
                                        sliderInput(inputId = "obj4_ahp", label = "Objective 4:", min = 0, max = 100, value = c(0, 100), width = "120%"))
                               )),

                         ),

                         mainPanel(width=9,
                           tags$style(HTML("#plt_bo_measure {width: 450px;height: 550px;} ")),



                           div(id="all_ahp",
                           fluidRow(
                             div("2. Assign weights",style = "text-align: center; font-size: 150%;"),

                             column(
                               width = 12,
                               actionButton("ahp_card1", "Show Card 1",
                                            style="background-color: #f4f4f4; border-color: #2e6da4"),
                               actionButton("ahp_card2", "Show Card 2",
                                            style="background-color: #f4f4f4; border-color: #2e6da4"),
                               actionButton("ahp_card3", "Show Card 3",
                                            style="background-color: #f4f4f4; border-color: #2e6da4"),
                               actionButton("ahp_card4", "Show Card 4",
                                            style="background-color: #f4f4f4; border-color: #2e6da4"),
                               actionButton("ahp_card5", "Show Card 5",
                                            style="background-color: #f4f4f4; border-color: #2e6da4"),
                               actionButton("ahp_card6", "Show Card 6",
                                            style="background-color: #f4f4f4; border-color: #2e6da4"),
                                 ),
                             br(),br(),br(),
                             br(),br(),br(),
                             checkboxInput("show_all_cards", "Show all comparisons", value = FALSE),
                             br(),
                             div(id="sel_wgt","Selected Weights", style = "text-align: center; font-size: 150%;",
                                 div(tableOutput("weights_output"), style = "margin: 0 auto; width: fit-content;")),
                           ),
                           fluidRow(
                             uiOutput("card1_ui")%>%hidden(),
                             uiOutput("card2_ui")%>%hidden(),
                             uiOutput("card3_ui")%>%hidden(),
                             uiOutput("card4_ui")%>%hidden(),
                             uiOutput("card5_ui")%>%hidden(),
                             uiOutput("card6_ui")%>%hidden()
                           ),
                           br(),

                           div(id = "pareto_weighted", "Best Option under selected weighting", style = "text-align: center; font-size: 150%;"),
                                                   div(tableOutput("best_option_output"), style = "margin: 0 auto; width: fit-content; font-size: 150%;"),

                         checkboxInput("save_ahp",label = "Click here to save the selected optimum to the output folder (selected_optima.csv)",value=F, width = "100%")%>%hidden(),



                          br(),
                          uiOutput("consistency_check"),div(id="cc",textOutput("which_inconsistency"), style = "color: red;"),
                          br(),

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

                             checkboxInput("show_extra_dat", label = "Show cluster solutions", value = T),
                             checkboxInput("show_status_quo", label = "Show Status Quo", value = FALSE),
                             checkboxInput("unit_add3",label = "Show units",value = TRUE),
                             
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
                                 actionButton("plt_bo", "Plot map of measure implementation under best option"),
                             ),

                             uiOutput("plt_bo_measure"),
                             withSpinner(uiOutput("spinner_meas"),
                                         type=4, color ="#F7A600"),

                             div(id="download_ahp_id",
                                 div(
                                  style = "display: inline-block; vertical-align: top; margin-right: 0px; margin-top: 5px;",
                                 textInput("meas_ahp_savename", label = NULL, value = "ahp_measure_implementation")
                                    ),
                                 div(
                                   style = "display: inline-block; vertical-align: top; margin-left: 0px; margin-top: 5px;",
                                 downloadButton(outputId="download_am", label ="Download best option map"),
                                 div(
                                   id = "spinner_download_ahp",style = "display: none;",
                                   style = "display: none; vertical-align: top; margin-left: 0px; margin-top: 5px;",
                                   div(class = "spinspin")
                                 )  )
                                 )%>%hidden()
                           )
                           )

                         )
                       )

                     )
                   )
    )
  )
