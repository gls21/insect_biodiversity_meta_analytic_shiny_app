##### ui.R script

# App opens straight away.
# Runs default model only when user clicks button.
# Uses Goggle Sheets for user to upload data from shiny app. 
# Loads data from Google Sheets.
# Model formula includes Paper ID and Control as random intercepts

ui <- navbarPage(
  
  # Add a theme for the app
  theme = bslib::bs_theme(bootswatch = "flatly", # flatly is the theme name
                          success = "#2c76fe"), # make certain bits the same blue as seen in the app (used colour picker to find exact colour)
  
  # Add a custom error message if the whole app disconnects / fails
  # Using header makes it apply to all tabs
  header = disconnectMessage(text = "An error has occured, please reload the page and try again.",
                             refresh = "", # Don't include a refresh button
                             width = "full", # Message should take up full width of screen
                             size = 30, # Size 30 writing
                             background = "#2c76fe", # Blue background
                             colour = "white", # White writing
                             overlayColour = "grey", # Covers the app and draws attention to message
                             overlayOpacity = 0.8), # Nearly full opaque
  
  
  
  
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  title = icon("bug", lib = "font-awesome", "fa-2x"), # include bug icon as title (gives warning but seems to work)
  
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  # Intro tab
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  tabPanel("Introduction",
           
           # Make all the tab buttons bigger 
           tags$head(
             tags$style(
               HTML(".navbar-nav > li > a {
                font-size: 20px;
              }")
             )
           ),
           
           # ----------------------------------------------------------------------------------------------------------------------
           
           # Pre-amble
           p(h1(tags$b("An interactive platform for insect biodiversity meta-analyses"))),
           
           tags$br(),
           
           p(h4("Insects are a highly diverse taxa and provide vital ecological services, though their underrepresentation in research compared to vertebrates
                results in a knowledge gap of insect biodiversity change and its drivers.")),
           
           p(h4("This app is designed to best utilise the insect biodiversity data available by allowing users to interactively run meta-meta-analytic models,
                which involves analysing multiple meta-analytic studies together by combining effect sizes for each of these.")),
           
           p(h4("Data is taken from a database containing a collection of meta-analytic studies, described as a 'living' database
                due to the ability of users to upload new data from their own meta-analyses.")),
           
           p(h4("In its current state, the app can be used to investigate the effect of agricultural systems on biodiversity in terms of the log response ratio
                (quantifies proportionate change between treatments). You can view definitions of the agricultural systems in the 'Agricultural systems models' tab.
                We aim to expand the functionality to enable the investigation of other variables such as land-use or temperature.")),
           
           p(h4("The app is split into 3 main sections:")),
           
           # Make bullet point list of the descriptions of the 3 main tabs  
           h4(tags$ol(
             tags$li("Use this 'Introduction' to investigate the data sources used within the app."), 
             tags$li("Go to 'Agricultural systems models' to run models to investigate the effect of different agricultural systems on biodiversity."), 
             tags$li("Go to 'Upload data' to upload data from your own meta-analysis.")
           )),
           
           # link to code
           p(h5(icon("github", lib = "font-awesome", "fa-2x"), # add-in github icon
                tags$a(href="https://github.com/gls21/insect_biodiversity_meta_analytic_shiny_app", "View app source code"))),
           
           tags$hr(),
           
           # ----------------------------------------------------------------------------------------------------------------------
           
           ### Make tables on where data comes from
           
           p(h3(tags$b("Introduction"))),
           
           p(h4("Use this tab to investigate the data sources used within this app.")),
           
           tags$hr(),
           
           fluidRow(
             
             column(
               5, # width of this column within the row (each row has to sum to 12 - includes offsets)
               
               # add table legend for overview table
               h5(htmlOutput("table_legend_overview")),
               
               # add sample size overview table
               h5(withSpinner(tableOutput("sample_sizes_overview")))
               
             ),
             
             column(
               7,
               
               # Add map of where data comes from
               withSpinner(plotOutput("map")),
               
               # Add map figure legend
               h5(htmlOutput("map_figure_legend")),
               
               tags$br(),
               
               tags$hr(),
               
               tags$br(),
               
               # add user choice of which paper to show details for
               # This will be reactive based on what papers are included in the data - so can change as more papers are added
               # The options are specified in the server file
               h5(uiOutput("reactive_paper")),
               
               tags$br(),
               
               # add table legend
               h5(htmlOutput("table_legend_paper_details")),
               
               # add paper details table
               h5(withSpinner(tableOutput("paper_details_table"))),
               
               tags$br()
               
             )
           )
           
           # ----------------------------------------------------------------------------------------------------------------------
           
  ),
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  # Agricultural systems tab
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  tabPanel("Agricultural systems models",
           
           # ----------------------------------------------------------------------------------------------------------------------
           
           ### custom CSS for 2 column layout (used below for filter options).
           # For putting the agricultural systems checkboxes in 2 columns
           # Adapted from: https://groups.google.com/g/shiny-discuss/c/DJhII5ZXJ88
           tags$head(
             tags$style(HTML("
              .multicol {
              -webkit-column-count: 2; /* Chrome, Safari, Opera */
              -moz-column-count: 2; /* Firefox */
              column-count: 2;
              }
            "))
           ),
           
           # Make default and custom tab buttons within this tab bigger
           tags$style(HTML("
             .tabbable > .nav > li > a {
             width: 400PX; font-size: 30px
             }
           ")),
           
           # ----------------------------------------------------------------------------------------------------------------------
           
           # Title to show at top of tab
           p(h3(tags$b("Agricultural systems models"))),
           
           p(h4("Use this tab to investigate how different agricultural systems impact biodiversity.
                The models run are robust linear mixed-effects models. Biodiversity of each agricultural system is compared to biodiversity of the conventional 
                agricultural system in terms of LRR (log response ratio). The conventional agricultural system is adjusted to have a log response ratio of 0 
                to enable straightforward comparison. Run the default model to gain an overview, or run custom models if you have specific interests")),
           
           tags$br(),
           
           # -----------------------------------------------------------------------------------------------------------------------
           
           ##### Default and customs tabs within this agricultural systems tab
           
           tabsetPanel(type = "tabs",
                       
                       # ===========================================================================================================
                       
                       ### Default tab
                       
                       # ===========================================================================================================
                       
                       tabPanel("Default model",
                                
                                tags$br(),
                                
                                p(h5("Click 'Run default model' to run a model on all available data. This enables comparison of biodiversity
                                   in different agricultural systems. The model should take around a minute to run, and then you will be able to
                                   view a figure and table of the results.")),
                                
                                tags$hr(),
                                
                                # --------------------------------------------------------------------------------------------------
                                
                                # Button for the user to click to run the default model
                                fluidRow(
                                  
                                  column(
                                    12,
                                    
                                    # action button to run default model
                                    actionButton("run_default_model", "Run default model", style='font-size:125%')
                                  )
                                  
                                ),
                                
                                tags$hr(),
                                
                                # --------------------------------------------------------------------------------------------------
                                
                                # Default figure, figure legend, table legend, and table
                                
                                fluidRow(
                                  
                                  column(
                                    7, # width of this column within the row (each row has to sum to 12 - includes offsets)
                                    
                                    # plot graph
                                    withSpinner(plotOutput("mma_robust_plot")),
                                    
                                    # add figure legend
                                    h5(htmlOutput("figure_legend"))
                                  ),
                                  
                                  column(
                                    5,
                                    
                                    # table legend
                                    h5(htmlOutput("table_legend_default_model_output")),
                                    
                                    # produce table
                                    h5(withSpinner(tableOutput("default_model_table")))
                                  )
                                  
                                ),
                                
                                tags$hr(),
                                
                                # --------------------------------------------------------------------------------------------------
                                
                                # User inputs on figure
                                
                                p(h5("Use this section to choose which agricultural systems to plot. 
                                   You can also choose to plot percentage change instead of the log response ratio,
                                   and to re-scale the x-axis as different agricultural systems are selected.")),
                                
                                
                                fluidRow(
                                  
                                  column(
                                    6,
                                    
                                    # user choice of agricultural system
                                    # Will be reactive based on the agricultural systems present in the data
                                    h5(uiOutput("reactive_treatment"))
                                    
                                  ),
                                  
                                  column(
                                    3,
                                    
                                    h5(radioButtons(inputId = "metric",
                                                    label = "Select metric:",
                                                    choices = c("Adjusted LRR", "Percentage change"),
                                                    selected = "Adjusted LRR")
                                       
                                    )),
                                  
                                  column(
                                    3,
                                    
                                    # user choice of whether to scale the x axis or not
                                    h5(radioButtons(inputId = "x_axis_scale",
                                                    label = "Scaling of the x-axis:",
                                                    choices = c("Fixed scale", "Re-scale"),
                                                    selected = "Fixed scale")
                                    ))
                                ),
                                
                                tags$hr(),
                                
                                # --------------------------------------------------------------------------------------------------
                                
                                # Click to see descriptions of agricultural systems and buttons to download default model results
                                
                                p(h5("Use this section to view definitions of the agricultural systems, and download the R default model summary (.txt file)
                                     or the results table of coefficients (.csv file).")),
                                
                                fluidRow(
                                  
                                  column(
                                    3,
                                    actionButton("show", "Click to see definitions of agricultural systems", style='font-size:125%')
                                  ),
                                  
                                  column(
                                    3,
                                    actionButton("hide", "Hide", style='font-size:125%')
                                  ),
                                  
                                  column(
                                    3,
                                    
                                    # download button for downloading model output
                                    downloadButton(outputId = "download_default_model_output",
                                                   label = "Download R default model summary",
                                                   style='font-size:125%')
                                    
                                  ),
                                  
                                  column(
                                    3,
                                    
                                    # download button for downloading table of coefficients
                                    downloadButton(outputId = "download_default_model_coeffs",
                                                   label = "Download default model table of coefficients",
                                                   style='font-size:125%')
                                  )
                                  
                                ),
                                
                                br(),
                                
                                fluidRow(
                                  
                                  h5(tableOutput("agri_sys_defs_table"))
                                  
                                ),
                                
                                tags$hr(),
                                
                                # --------------------------------------------------------------------------------------------------
                                
                       ),
                       
                       # ===========================================================================================================
                       
                       ### Custom tab
                       
                       # ===========================================================================================================
                       
                       # Running custom models
                       # Going to be based on biodiversity metric used by studies for now, but can easily adapt later on e.g. for location
                       
                       tabPanel("Custom models",
                                
                                tags$br(),
                                
                                h5("Filter the data you are interested in based on the criteria below.
                                   Once you have made your selections, click 'Run custom model'. The model should take less than a minute to run, 
                                   and then you will be able to view a figure and table of the results."),
                                
                                tags$br(),
                                
                                # --------------------------------------------------------------------------------------------------
                                
                                # User inputs on what model to run and a button to run the model
                                
                                fluidRow(
                                  
                                  column(
                                    12,
                                    
                                    # user choice of studies to include in model based on which biodiversity metrics they would like to analyse
                                    # Reactive based on biodiversity metric categories available in the data
                                    
                                    h5(uiOutput("reactive_biodiversity_metric_category"))
                                    
                                  )
                                  
                                ),
                                
                                tags$br(),
                                
                                fluidRow(
                                  
                                  column(
                                    12,
                                    
                                    # include action button to run model once inputs have been selected
                                    actionButton("run_custom_model", "Run custom model", style='font-size:125%')
                                  )
                                  
                                ),
                                
                                tags$hr(),
                                
                                # --------------------------------------------------------------------------------------------------
                                
                                # Graph and table produced based on the custom model run
                                
                                fluidRow(
                                  
                                  column(
                                    7,
                                    
                                    # This will make the stop error messages grey (rather than red) if the model doesn't run
                                    tags$head(tags$style(".shiny-output-error{color: grey;}")),
                                    
                                    # produce custom model graph
                                    withSpinner(plotOutput("custom_model_figure")),
                                    
                                    # add custom model figure legend
                                    h5(htmlOutput("custom_model_figure_legend"))
                                  ),
                                  
                                  column(
                                    5,
                                    
                                    # add table legend
                                    h5(htmlOutput("table_legend_custom_model_output")),
                                    
                                    # add custom model table
                                    h5(withSpinner(tableOutput("custom_model_output_table"))) # shows a loading symbol while model is running.
                                  )
                                  
                                ),
                                
                                
                                tags$hr(),
                                
                                # --------------------------------------------------------------------------------------------------
                                
                                # User input on figure
                                
                                p(h5("Use this section to choose which metric to plot.")),
                                
                                fluidRow(
                                  
                                  column(
                                    12,
                                    
                                    # user choice of metric
                                    h5(radioButtons(inputId = "metric2",
                                                    label = "Select metric:",
                                                    choices = c("Adjusted LRR", "Percentage change"),
                                                    selected = "Adjusted LRR")
                                       
                                    ))
                                  
                                ),
                                
                                tags$hr(),
                                
                                # --------------------------------------------------------------------------------------------------
                                
                                # Click to see descriptions of agricultural systems and put buttons for downloading custom model results
                                
                                p(h5("Use this section to view definitions of the agricultural systems, and download the R custom model summary (.txt file)
                                     or the results table of coefficients (.csv file).")),
                                
                                fluidRow(
                                  
                                  column(
                                    3,
                                    actionButton("show2", "Click to see definitions of agricultural systems", style='font-size:125%')
                                  ),
                                  
                                  column(
                                    3,
                                    actionButton("hide2", "Hide", style='font-size:125%')
                                  ),
                                  
                                  column(
                                    3,
                                    
                                    shinyjs::useShinyjs(), # so can enable and disable the download buttons
                                    
                                    # download button for downloading model output
                                    downloadButton(outputId = "download_custom_model_output",
                                                   label = "Download R custom model summary",
                                                   style='font-size:125%')
                                    
                                  ),
                                  
                                  column(
                                    3,
                                    
                                    # download button for downloading table of coefficients
                                    downloadButton(outputId = "download_custom_model_coeffs",
                                                   label = "Download custom model table of coefficients",
                                                   style='font-size:125%')
                                  )
                                  
                                ),
                                
                                br(),
                                
                                fluidRow(
                                  
                                  h5(tableOutput("agri_sys_defs_table2"))
                                  
                                )
                                
                                
                                # --------------------------------------------------------------------------------------------------
                                
                       ) # close custom tab
                       
                       # -----------------------------------------------------------------------------------------------------------
                       
           ), # close tabs within agricultural systems tab
           
  ), # close agricultural systems tab
  
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  # Upload data tab
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  tabPanel("Upload data",
           
           fluidRow(
             
             column(
               12,
               
               # Text to explain what the tab is for
               p(h3(tags$b("Upload data"))),
               
               h4(p("The app is based on data that is read in from a database stored in Google Sheets. Here you can add a spreadsheet of results from your own meta-analysis,
                    which will then be incorporated into the model outputs of the app. To upload, please follow the steps below:")),
               
               h4(p("1. Enter your first and second name (containing letters, hyphens, and spaces only). This will be used to name the spreadsheet in the database containing your data.")),
               
               h4(p("2. Click 'Browse' to choose and upload your file. The file must be a csv, and contain columns named 'LRR' and 'Treatment'.
                    Upon meeting the checks, the file will be uploaded to the app and a preview of your chosen file will appear.")),
               
               h4(p("3. If you are happy with your chosen file, you can then click 'Upload to database' to store your file remotely.
                    Upon refreshing the app, any outputs will include your uploaded data."))
               
             )
             
           ),
           
           tags$hr(),
           
           fluidRow(
             
             column(
               2,
               
               # User can input their first name
               h5(textInput("first_name", "Please insert your first name:")),
               
               # User can input their second name
               h5(textInput("second_name", "Please insert your second name:")),
               
               tags$br(),
               
               # If meets the checks, the acceptable name will be printed. Otherwise, it will display warnings
               h5(htmlOutput("name_inputted"))
               
             ),
             
             column(
               2,
               
               # Upload button to upload csv file to the shiny app
               h5(fileInput("preview_upload", "Click 'Browse' to upload your meta-analysis data and view a preview", accept = ".csv")),
               
               tags$br(),
               
               h5(p("Once previewed, click here to upload your data to the database")),
               
               # Button to click to upload the data to the googlesheet
               actionButton("upload_to_googlesheets", "Upload to database", style='font-size:125%'),
               
               tags$br(),
               
               tags$br(),
               
               tags$hr(),
               
               # Success message if data sheet gets successfully uploaded to googlesheets
               h5(htmlOutput("upload_complete"))
               
             ),
             
             column(
               8,
               
               # add table legend
               h5(htmlOutput("preview_upload_data_legend")),
               
               # Preview of data to be uploaded
               h5(withSpinner(tableOutput("preview_upload_data")))
               
             )
             
           )
           
  ),
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  # References tab
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
  tabPanel("References",
           
           # Text to explain what the tab is for
           p(h3(tags$b("References"))),
           
           # Include table legend for references table
           h5(htmlOutput("references_table_legend")),
           
           # add paper details table
           h5(withSpinner(tableOutput("references_table")))
           
  )
  
  # ===============================================================================================================================
  # ===============================================================================================================================
  
)



