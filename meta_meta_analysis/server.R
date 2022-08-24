##### server.R script

# App opens straight away.
# Runs default model only when user clicks button.
# Uses Goggle Sheets for user to upload data from shiny app.
# Loads data from Google Sheets.
# Model formula includes Paper ID and Control as random intercepts


server <- function(input, output) {

  # =================================================================================================================
  # =================================================================================================================

  # Load data that will be use throughout server.R file

  # =================================================================================================================
  # =================================================================================================================

  # -------------------------------------------------------------------------------------

  # Load data from Google Sheets

  sheets_names <- reactive({
    # Get a list of the sheets present within the shiny_app_data googlesheet
    sheets_names <- sheet_names(sheet_id)
  })

  list_of_data <- reactive({
    # Read each of the sheets and load their data. Puts them into a list of dataframes
    list_of_data <- lapply(sheets_names(), function (x) read_sheet(ss = sheet_id, sheet = x))
  })

  number_of_studies <- reactive({
    # Count the number of sheets present
    number_of_studies <- length(list_of_data())
  })

  data <- reactive({
    # The initial data is Christina's data
    data <- list_of_data()[[1]]

    # Use a for loop to rbind all the sheets together into 1 dataframe (if more than 1 present)
    if (number_of_studies() > 1) {
      for (i in 2:number_of_studies()) {
        data <- rbind(data, list_of_data()[[i]])
      }
    }

    # Re level data set so conventional is the reference category
    data$Treatment <- as.factor(data$Treatment)
    data$Treatment <- relevel(data$Treatment, ref = "Conventional")

    data <- data
  })

  # -------------------------------------------------------------------------------------

  # Get table of sample sizes - how many instances do we have of each agricultural system? Used later on to add Conventional back into tables.
  sample_sizes <- reactive({
    sample_sizes <- as.data.frame(table(data()$Treatment))
    colnames(sample_sizes) <- c("Treatment", "Frequency")

    sample_sizes <- sample_sizes
  })

  # -------------------------------------------------------------------------------------


  # =================================================================================================================
  # =================================================================================================================

  ##### Intro tab

  # =================================================================================================================
  # =================================================================================================================

  # ---------------------------------------------------------------------------------------------------------------

  ### Table for overview of papers included (in intro tab)

  # Add table legend
  output$table_legend_overview <- renderText({
    paste("<b>Table 1.</b>", "Number of agricultural systems studied and total data points supplied by each paper. For full paper details, see the references tab.")
  })

  # Add sample size table
  output$sample_sizes_overview <- renderTable({

    # Initialise empty data frame
    sample_sizes_table <- data.frame(Paper = character(), Number_of_agricultural_systems = numeric(), Total_data_points = numeric())

    # Calculate statistics for each paper
    for (i in unique(data()$Paper_ID)) { # data is whole spreadsheet

      paper_subset <- data() %>%
        filter(Paper_ID %in% i)

      # make new row which will be added to sample_sizes_table
      new_row <- data.frame(
        Paper = i,
        Number_of_agricultural_systems = length(unique(paper_subset$Treatment)), # number of unique agricultural systems
        Total_data_points = nrow(paper_subset) # number of instances
      )

      sample_sizes_table <- rbind(sample_sizes_table, new_row)

    }

    # Put table in alphabetical order based on paper_ID
    sample_sizes_table <- sample_sizes_table %>%
      arrange(Paper)

    # remove _ from colnames
    colnames(sample_sizes_table) <- c("Paper", "Number of agricultural systems", "Total data points")

    sample_sizes_table <- sample_sizes_table


  },

  striped = TRUE

  )

  # ---------------------------------------------------------------------------------------------------------------

  # Map of where data comes from

  output$map <- renderPlot({

    # Filter the data to include data points which have long and lat available
    coord_data <- data() %>%
      drop_na(Longitude, Latitude) %>%
      filter(Longitude != "." & Latitude != ".")

    # Plot a map of the world
    map(database = "world")

    # Add the points to it based on long and lat
    points(coord_data$Longitude, coord_data$Latitude, col="Blue", cex=2, pch=19)

  })

  # Total number of data points
  total_data_points <- reactive({

    nrow(data())

  })

  # Number of data points with co-ordinates available
  data_with_coords <- reactive({

    data_with_coords <- total_data_points() - (sum(is.na(data()$Longitude) | data()$Longitude == "."))

  })

  # Add map figure legend
  output$map_figure_legend <- renderText({
    paste("<b>Figure 1.</b>", "Map providing geographic representativeness of data included in this app based on latitude and longitude co-ordinates.
          Currently,", data_with_coords(), "out of", total_data_points(), "data points have co-ordinates provided to enable them to be plotted.")
  })

  # ---------------------------------------------------------------------------------------------------------------

  ### Table for details of specific paper chosen by user (in intro tab)

  # Make reactive options
  output$reactive_paper <- renderUI({
    selectInput(inputId = "paper",
                label = "Select paper to show more details for:",
                choices = c("", unique(data()$Paper_ID)),
                selected = NULL)
  })

  # Add table legend
  output$table_legend_paper_details <- renderText({
    paste0("<b>Table 2. </b>", "Details on the agricultural systems studied by ", input$paper, ".", sep="")
  })

  # Add paper details table
  output$paper_details_table <- renderTable({

    selected_paper <- data() %>%
      filter(Paper_ID %in% input$paper) # Shows warnings if you use == rather than %in%

    paper_details_table <- data.frame(Agricultural_system = character(), Number_of_data_points = numeric())

    for (i in unique(selected_paper$Treatment)) {

      subset_paper_by_treatment <- selected_paper %>%
        filter(Treatment == i)

      new_row <- data.frame(
        Agricultural_system = i,
        Number_of_data_points = nrow(subset_paper_by_treatment)
      )

      paper_details_table <- rbind(paper_details_table, new_row)
    }

    paper_details_table <- paper_details_table %>%
      arrange(Agricultural_system)

    # remove _ from colnames
    colnames(paper_details_table) <- c("Agricultural system", "Number of data points")

    paper_details_table <- paper_details_table

  },

  striped = TRUE

  )

  # ---------------------------------------------------------------------------------------------------------------

  # =================================================================================================================
  # =================================================================================================================

  ##### Agricultural systems tab

  # =================================================================================================================
  # =================================================================================================================

  # =================================================================================================================

  #### Default model tab

  # =================================================================================================================

  # -------------------------------------------------------------------------------------

  # Disable the download buttons on page load - so can't click it until a model has successfully run
  shinyjs::disable("download_default_model_output")
  shinyjs::disable("download_default_model_coeffs")

  # -------------------------------------------------------------------------------------

  # Upon the user clicking the run default model, the model is run
  best_robust <- eventReactive(input$run_default_model, {

    rsb <- list(psi2propII(smoothPsi), psi2propII(smoothPsi, k = 2.28))

    best_robust <- rlmer(LRR ~  Treatment + (1|Control) + (1|Paper_ID), data = data(),
                         rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
                         rho.sigma.b = rsb)

  })

  # -------------------------------------------------------------------------------------

  # Extract coefficients

  robust_coefficients_all <- reactive({

    req(best_robust())

    # Enable the download results buttons
    shinyjs::enable("download_default_model_output")
    shinyjs::enable("download_default_model_coeffs")

    # Extract the coefficients from the model
    robust_coefficients_all <- best_robust() %>% summary() %>% .$coefficients %>%
      round(digits = 2) %>% as.data.frame()

    names(robust_coefficients_all) <- c("Original_LRR", "SE", "t")
    row.names(robust_coefficients_all) <- c(sample_sizes()$Treatment)

    # Adjust reference level (intercept = conventional) to 0.
    # Take away the intercept (conventional) value from each level
    robust_coefficients_all$Adjusted_LRR <- ""  # New column where I will store adjusted values

    robust_coefficients_all$Adjusted_LRR[1] <- 0 # Adjust conventional to 0

    for (i in 1:length(robust_coefficients_all$Original_LRR)) { # Adjust other levels
      if (i > 0) {

        # Had to change this because when new data is added, it won't necessarily be 0.02 to take away
        # robust_coefficients_all$Adjusted_LRR[i] <- robust_coefficients_all$Original_LRR[i] - 0.02

        robust_coefficients_all$Adjusted_LRR[i] <- robust_coefficients_all$Original_LRR[i] - robust_coefficients_all$Original_LRR[1]
      }
    }

    # Change Adjusted_LRR class to numeric (came out as character after the for loop)
    robust_coefficients_all$Adjusted_LRR <- as.numeric(robust_coefficients_all$Adjusted_LRR)

    # Obtain other coefficients: CI, percentage_change (plus CI for percentage change)
    robust_coefficients_all <- robust_coefficients_all %>%
      mutate(LCI = Adjusted_LRR - (1.96 * SE),
             UCI = Adjusted_LRR + (1.96 * SE)) %>%
      round(digits = 2) %>%
      mutate(CI = paste(LCI, "to", UCI)) %>%
      mutate(percentage_change = 100 * (exp(Adjusted_LRR) - 1),
             LCI_percent = 100 * (exp(LCI) - 1),
             UCI_percent = 100 * (exp(UCI) - 1)) %>%
      mutate_if(is.numeric, round, 2) %>%
      mutate(CI_percent = paste(LCI_percent, "to", UCI_percent))

    # make rownames into a column, rather than just the rownames - so can use to plot later
    robust_coefficients_all <- tibble::rownames_to_column(robust_coefficients_all, "Treatment")

    # rbind sample sizes table with robust_coefficients table
    robust_coefficients_all <- merge(robust_coefficients_all, sample_sizes(), by = "Treatment", sort = FALSE)

    # Change Treatment class to factor (came out as character after the for loop)
    robust_coefficients_all$Treatment <- as.factor(robust_coefficients_all$Treatment)

    robust_coefficients_all <- robust_coefficients_all
  })

  # Make reactive options for treatments

  # Get variable which will be entered as the choices
  treatment_choices <- reactive({
    treatment_choices <- sort(unique(data()$Treatment))
    treatment_choices <- treatment_choices[treatment_choices != "Conventional"]
  })

  output$reactive_treatment <- renderUI({
    tags$div(class = "multicol",
             checkboxGroupInput(inputId = "treatment",
                                label = "Select agricultural system(s):",
                                choices = treatment_choices(),
                                selected = treatment_choices())
    )
  })

  # filter coefficients based on user choice
  robust_coefficients <- reactive({

    robust_coefficients_all() %>%
      dplyr::filter(Treatment != "Conventional") %>% # remove conventional as this is the reference level
      dplyr::filter(Treatment %in% input$treatment) # user choice
  })

  # ----------------------------------------------------------------------------------------------------

  ### Getting indexes of treatments and corresponding adjusted_LRR values and percentage change values for significant results

  # Make version of dataframe in order on descending Adjusted_LRR value (will be the same for % change)
  ordered_robust_coefficients <- reactive({

    robust_coefficients() %>%
      dplyr::arrange(desc(Adjusted_LRR), Treatment)

  })

  # Indexes of significant treatments
  signif_results_plus_0.25 <- reactive({

    signif_results <- c() # initialize empty vector
    for (i in 1:nrow(ordered_robust_coefficients())) {
      if (ordered_robust_coefficients()$t[i] >= 1.96  || ordered_robust_coefficients()$t[i] <= -1.96) {
        signif_results <- c(signif_results, i)
      }
    }
    # add 0.25 to this so puts the significance star just above the point (not directly on it)
    signif_results_plus_0.25 <- signif_results + 0.25
  })

  # Significant Adjusted_LRR values
  adjusted_LRR <- reactive({

    adjusted_LRR <- c()
    for (i in 1:nrow(ordered_robust_coefficients())) {
      if (ordered_robust_coefficients()$t[i] >= 1.96  || ordered_robust_coefficients()$t[i] <= -1.96) {
        adjusted_LRR <- c(adjusted_LRR, ordered_robust_coefficients()$Adjusted_LRR[i])
      }
    }
    adjusted_LRR <- adjusted_LRR # This is necessary for the app to run without error
  })

  # Significant percentage_change values
  percentage_change <- reactive({

    percentage_change <- c()
    for (i in 1:nrow(ordered_robust_coefficients())) {
      if (ordered_robust_coefficients()$t[i] >= 1.96  || ordered_robust_coefficients()$t[i] <= -1.96) {
        percentage_change <- c(percentage_change, ordered_robust_coefficients()$percentage_change[i])
      }
    }
    percentage_change <- percentage_change # This is necessary for the app to run without error
  })

  # ---------------------------------------------------------------------------------------------------

  ### Get min and max adjusted_LRR and percentage change values for scaling the x axis

  # Adjusted_LRR
  max_LRR <- reactive({

    max(robust_coefficients_all()$UCI)
  })

  min_LRR <- reactive({
    min(robust_coefficients_all()$LCI)
  })

  # Percentage change
  max_percent <- reactive({
    max(robust_coefficients_all()$UCI_percent)
  })

  min_percent <- reactive({
    min(robust_coefficients_all()$LCI_percent)
  })

  # ---------------------------------------------------------------------------------------------------

  ### Plotting default agricultural systems graph

  output$mma_robust_plot <- renderPlot({

    # Stops an error appearing if no treatment inputs are selected
    req(input$treatment)

    ### Plot graphs with scale that is fixed as users select agricultural systems
    if (input$x_axis_scale == "Fixed scale") {

      # Plot adjusted_LRR graph
      if (input$metric == "Adjusted LRR") {
        robust_coefficients() %>%
          ggplot(aes(x = reorder(Treatment, -Adjusted_LRR), Adjusted_LRR)) + # reorder the x variable (treatments) in order of ascending Adjusted_LRR value
          geom_errorbar(aes(x = reorder(Treatment, -Adjusted_LRR), ymin=LCI, ymax=UCI), width=0.2, colour="black", alpha=0.9, size=1.3) +
          geom_point(stat="identity", size = 4, colour = "black") +
          geom_hline(yintercept = 0, color = "blue", linetype = "dashed") + # add conventional reference line
          ylim(min_LRR(), max_LRR()) + # will eventually be on x axis
          labs(x = "Agricultural system", y = "Adjusted log response ratio") +
          coord_flip() + # horizontal so looks more like a forest plot
          annotate("text", x = signif_results_plus_0.25(), y = adjusted_LRR(),   ######### Maybe need better way of adding significance stars?
                   label = "*", size = 8, colour = "blue") +
          theme_bw() +
          theme(axis.text = element_text(size=18),
                axis.title = element_text(size=22),
                panel.grid.major = element_blank(), # no grid lines
                panel.grid.minor = element_blank())

        # Plot percentage_change graph
      } else if (input$metric == "Percentage change") {
        robust_coefficients() %>%
          ggplot(aes(x = reorder(Treatment, -percentage_change), percentage_change)) + # reorder the x variable (treatments) in order of ascending % change value
          geom_errorbar(aes(x = reorder(Treatment, -percentage_change), ymin=LCI_percent, ymax=UCI_percent), width=0.2, colour="black", alpha=0.9, size=1.3) +
          geom_point(stat="identity", size = 4, colour = "black") +
          geom_hline(yintercept = 0, color = "blue", linetype = "dashed") + # add conventional reference line
          ylim(min_percent(), max_percent()) +
          labs(x = "Agricultural system", y = "Percentage change") +
          coord_flip() + # horizontal so looks more like a forest plot
          annotate("text", x = signif_results_plus_0.25(), y = percentage_change(),   ######### Maybe need better way of adding significance stars?
                   label = "*", size = 8, colour = "blue") +
          theme_bw() +
          theme(axis.text = element_text(size=18),
                axis.title = element_text(size=22),
                panel.grid.major = element_blank(), # no grid lines
                panel.grid.minor = element_blank())
      }

      ### Plot graphs with scale that re-scales as users select agricultural systems
    } else if (input$x_axis_scale == "Re-scale") {

      # Plot adjusted_LRR graph
      if (input$metric == "Adjusted LRR") {
        robust_coefficients() %>%
          ggplot(aes(x = reorder(Treatment, -Adjusted_LRR), Adjusted_LRR)) + # reorder the x variable (treatments) in order of ascending Adjusted_LRR value
          geom_errorbar(aes(x = reorder(Treatment, -Adjusted_LRR), ymin=LCI, ymax=UCI), width=0.2, colour="black", alpha=0.9, size=1.3) +
          geom_point(stat="identity", size = 4, colour = "black") +
          geom_hline(yintercept = 0, color = "blue", linetype = "dashed") + # add conventional reference line
          labs(x = "Agricultural system", y = "Adjusted log response ratio") +
          coord_flip() + # horizontal so looks more like a forest plot
          annotate("text", x = signif_results_plus_0.25(), y = adjusted_LRR(),   ######### Maybe need better way of adding significance stars?
                   label = "*", size = 8, colour = "blue") +
          theme_bw() +
          theme(axis.text = element_text(size=18),
                axis.title = element_text(size=22),
                panel.grid.major = element_blank(), # no grid lines
                panel.grid.minor = element_blank())

        # Plot percentage_change graph
      } else if (input$metric == "Percentage change") {
        robust_coefficients() %>%
          ggplot(aes(x = reorder(Treatment, -percentage_change), percentage_change)) + # reorder the x variable (treatments) in order of ascending % change value
          geom_errorbar(aes(x = reorder(Treatment, -percentage_change), ymin=LCI_percent, ymax=UCI_percent), width=0.2, colour="black", alpha=0.9, size=1.3) +
          geom_point(stat="identity", size = 4, colour = "black") +
          geom_hline(yintercept = 0, color = "blue", linetype = "dashed") + # add conventional reference line
          labs(x = "Agricultural system", y = "Percentage change") +
          coord_flip() + # horizontal so looks more like a forest plot
          annotate("text", x = signif_results_plus_0.25(), y = percentage_change(),   ######### Maybe need better way of adding significance stars?
                   label = "*", size = 8, colour = "blue") +
          theme_bw() +
          theme(axis.text = element_text(size=18),
                axis.title = element_text(size=22),
                panel.grid.major = element_blank(), # no grid lines
                panel.grid.minor = element_blank())
      }
    }

  })

  # Add graph figure legend
  output$figure_legend <- renderText({
    paste("<b>Figure 2.</b>", "Compares the biodiversity measured by ", input$metric, " across different agricultural systems compared to the conventional agricultural system
          (blue dashed line). Significance is indicated by blue stars (absolute t-value greater than 1.96).")
  })

  # ----------------------------------------------------------------------------------------------------

  # Default model table legend
  output$table_legend_default_model_output <- renderText({

    paste0("<b>Table 3.</b> ", "Model coefficients extracted or calculated from the model summary. Agricultural systems with absolute t-values greater than 1.96
          have significantly different levels of biodiversity than the conventional agricultural system (", robust_coefficients_all()[1, 13], " data points).", sep = "")
  })

  # Default model table output
  output$default_model_table <- renderTable({

    default_model_table <- robust_coefficients()[c("Treatment", "t", "Adjusted_LRR", "percentage_change", "Frequency")] %>%
      arrange(Adjusted_LRR, desc(Treatment))

    # Remove as not going to include conventional in table anymore as doesn't make sense - just put number of data points in the legend.
    #default_model_table <- rbind(robust_coefficients_all()[1, c(1,4,5,9,13)], default_model_table) # Add in row for conventional at the top of table

    default_model_table <- default_model_table %>%
      relocate(t, .before = Frequency)

    # Remove _ from colnames and adjust column names
    colnames(default_model_table) <- c("Agricultural system", "Adjusted log response ratio", "Percentage change", "t-value", "Data points")

    default_model_table <- default_model_table

  },

  striped = TRUE

  )

  # ---------------------------------------------------------------------------------------------------------------

  ### Downloading default results

  # Downloading the model output

  output$download_default_model_output <- downloadHandler(
    filename = function() {
      paste0("default_model_output", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      write.table(capture.output(summary(best_robust())), file)
    }
  )

  # Downloading the coefficients table

  output$download_default_model_coeffs <- downloadHandler(
    filename = function() {
      paste0("default_model_coefficients", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(robust_coefficients_all(), file)
    }
  )

  # ---------------------------------------------------------------------------------------------------------------

  ### Details of agricultural systems table which shows once button is clicked, and disappears once hide button is clicked

  defs_data <- reactiveValues() # reactiveValues function returns an object for storing reactive values

  observeEvent(input$show, { # When "show" button is selected, the object stores the agri_systems_def data
    defs_data$data <- agri_systems_def
  })

  observeEvent(input$hide, { # When "hide" button is selected, the object stores nothing
    defs_data$data <- NULL
  })

  output$agri_sys_defs_table <- renderTable({
    if (is.null(defs_data$data)) { # If object isn't storing anything, return nothing (i.e. don't show table)
      return()
    } else { # Else, show the table
      agri_systems_def <- agri_systems_def %>%
        arrange(Agricultural_system)
    }

  },

  striped = TRUE

  )

  # ----------------------------------------------------------------------------------------------------------------

  # =================================================================================================================

  ### Custom models tab

  # =================================================================================================================

  # ----------------------------------------------------------------------------------------------------

  # Going to be based on biodiversity metric used for now, but can easily adapt later on e.g. for location

  # Make reactive biodiversity metric choices
  output$reactive_biodiversity_metric_category <- renderUI({
    selectInput(inputId = "biodiversity_metric_category",
                label = "Biodiversity metric(s):",
                choices = unique(data()$biodiversity_metric_category),
                selected = NULL,
                multiple = TRUE)
  })

  ### Filter the data based on user input once the run model button has been pressed
  custom_model_data <- eventReactive(input$run_custom_model, {

    validate(
      need(input$biodiversity_metric_category != "", "Please select at least one biodiveristy metric category.")
    )

    # Filter the data based on the studies the user wants to run the model on
    custom_model_data <- data() %>%
      dplyr::filter(biodiversity_metric_category %in% input$biodiversity_metric_category)

  })

  ### Run the custom model

  custom_model <- reactive({

    req(custom_model_data())

    validate(
      need(sum(custom_model_data()$Treatment == "Conventional") > 1, "Please select additional biodiveristy metric category because your current selection does not currently contain data on the reference conventional level.")
    )

    # Run robust model
    custom_model_rsb <- list(psi2propII(smoothPsi), psi2propII(smoothPsi, k = 2.28))

    # Try and model the model on the currently selected subset of data. If doesn't work, tell user to include more data.
    tryCatch(
      expr = {

        custom_robust_model <- rlmer(LRR ~ Treatment + (1|Control) + (1|Paper_ID), data = custom_model_data(),
                                     rho.sigma.e = psi2propII(smoothPsi, k = 2.28),
                                     rho.sigma.b = custom_model_rsb)

        # If model successfully runs, enable the download results buttons
        shinyjs::enable("download_custom_model_output")
        shinyjs::enable("download_custom_model_coeffs")

        custom_model <- custom_robust_model

      }, error = function(e) {

        # If model does not successfully run, make sure download results buttons are disabled
        shinyjs::disable("download_custom_model_output")
        shinyjs::disable("download_custom_model_coeffs")

        # Then stop the process, and return this error message
        stop(safeError("There is currently insufficient data for this model to run, please select an additional or alternative biodiversity metric category."))
      })

  })

  # Custom model summary

  custom_model_summary <- reactive({

    req(custom_model())

    custom_model_summary <- capture.output(summary(custom_model())) # capture.output allows it to be put into a txt file that the user can download

  })

  # Extract the coefficients
  custom_model_analysis <- reactive({

    req(custom_model())

    # Extract coefficients
    custom_model_coefficients <- custom_model() %>%
      summary() %>%
      .$coefficients %>%
      round(digits = 2) %>%
      as.data.frame()

    # Change column names
    colnames(custom_model_coefficients) <- c("Original_LRR", "SE", "t")

    # Change row names
    # I use sample_sizes$Treatment because this has the correct order of treatments i.e. conventional first, and then the rest alphabetically
    rownames(custom_model_coefficients) <- intersect(sample_sizes()$Treatment, unique(custom_model_data()$Treatment))

    # Adjust reference level (intercept = conventional) to 0.
    # Take away the intercept (conventional) value from each level
    custom_model_coefficients$Adjusted_LRR <- ""  # New column where I will store adjusted values

    custom_model_coefficients$Adjusted_LRR[1] <- 0 # Adjust conventional to 0

    for (i in 1:length(custom_model_coefficients$Original_LRR)) { # Adjust other levels
      if (i > 0) {
        custom_model_coefficients$Adjusted_LRR[i] <- custom_model_coefficients$Original_LRR[i] - custom_model_coefficients$Original_LRR[1]
      }
    }

    # Change Adjusted_LRR class to numeric (came out as character after the for loop)
    custom_model_coefficients$Adjusted_LRR <- as.numeric(custom_model_coefficients$Adjusted_LRR)

    # Obtain other coefficients: CI, percentage_change (plus CI for percentage change)
    custom_model_coefficients <- custom_model_coefficients %>%
      mutate(LCI = Adjusted_LRR - (1.96 * SE),
             UCI = Adjusted_LRR + (1.96 * SE)) %>%
      round(digits = 2) %>%
      mutate(CI = paste(LCI, "to", UCI)) %>%
      mutate(percentage_change = 100 * (exp(Adjusted_LRR) - 1),
             LCI_percent = 100 * (exp(LCI) - 1),
             UCI_percent = 100 * (exp(UCI) - 1)) %>%
      mutate_if(is.numeric, round, 2) %>%
      mutate(CI_percent = paste(LCI_percent, "to", UCI_percent))

    # Make rownames an actual column
    custom_model_coefficients <- tibble::rownames_to_column(custom_model_coefficients, "Treatment")

    # Work out number of papers these results are based on
    # In correct order with conventional first and all others after alphabetically
    custom_sample_sizes <- as.data.frame(table(custom_model_data()$Treatment))
    colnames(custom_sample_sizes) <- c("Treatment", "Frequency")

    # rbind sample sizes table with robust_coefficients table
    custom_model_coefficients <- merge(custom_model_coefficients, custom_sample_sizes, by = "Treatment", sort = FALSE)

    custom_model_analysis <- custom_model_coefficients

  })

  # ----------------------------------------------------------------------------------------------------------------

  ### Getting indexes of treatments and corresponding adjusted_LRR values and percentage change values for significant results

  # Make version of dataframe in order of descending Adjusted_LRR value (will be the same for % change)
  custom_ordered_robust_coefficients <- reactive({
    custom_model_analysis() %>%
      dplyr::arrange(desc(Adjusted_LRR), Treatment) %>%
      dplyr::filter(Treatment != "Conventional")
  })

  # Indexes of significant treatments
  custom_signif_results_plus_0.25 <- reactive({
    signif_results <- c() # initialize empty vector
    for (i in 1:nrow(custom_ordered_robust_coefficients())) {
      if (custom_ordered_robust_coefficients()$t[i] >= 1.96  || custom_ordered_robust_coefficients()$t[i] <= -1.96) {
        signif_results <- c(signif_results, i)
      }
    }
    # add 0.25 to this so puts the significance star just above the point (not directly on it)
    custom_signif_results_plus_0.25 <- signif_results + 0.25
  })

  # Significant Adjusted_LRR values
  custom_adjusted_LRR <- reactive({
    adjusted_LRR <- c()
    for (i in 1:nrow(custom_ordered_robust_coefficients())) {
      if (custom_ordered_robust_coefficients()$t[i] >= 1.96  || custom_ordered_robust_coefficients()$t[i] <= -1.96) {
        adjusted_LRR <- c(adjusted_LRR, custom_ordered_robust_coefficients()$Adjusted_LRR[i])
      }
    }
    custom_adjusted_LRR <- adjusted_LRR # This is necessary for the app to run without error
  })

  # Significant percentage_change values
  custom_percentage_change <- reactive({
    percentage_change <- c()
    for (i in 1:nrow(custom_ordered_robust_coefficients())) {
      if (custom_ordered_robust_coefficients()$t[i] >= 1.96  || custom_ordered_robust_coefficients()$t[i] <= -1.96) {
        percentage_change <- c(percentage_change, custom_ordered_robust_coefficients()$percentage_change[i])
      }
    }
    custom_percentage_change <- percentage_change # This is necessary for the app to run without error
  })

  # ---------------------------------------------------------------------------------------------------

  ### Get min and max adjusted_LRR and percentage change values for fixing the x axis scale

  # Adjusted_LRR
  custom_max_LRR <- reactive({
    max(custom_model_analysis()$UCI)
  })

  custom_min_LRR <- reactive({
    min(custom_model_analysis()$LCI)
  })

  # Percentage change
  custom_max_percent <- reactive({
    max(custom_model_analysis()$UCI_percent)
  })

  custom_min_percent <- reactive({
    min(custom_model_analysis()$LCI_percent)
  })

  # ---------------------------------------------------------------------------------------------------

  ### Plotting custom agricultural systems graph

  output$custom_model_figure <- renderPlot({

    ### Plot adjusted_LRR graph

    if (input$metric2 == "Adjusted LRR"){
      custom_ordered_robust_coefficients() %>%
        ggplot(aes(x = reorder(Treatment, -Adjusted_LRR), Adjusted_LRR)) + # reorder the x variable (treatments) in order of ascending Adjusted_LRR value
        geom_errorbar(aes(x = reorder(Treatment, -Adjusted_LRR), ymin=LCI, ymax=UCI), width=0.2, colour="black", alpha=0.9, size=1.3) +
        geom_point(stat="identity", size = 4, colour = "black") +
        geom_hline(yintercept = 0, color = "blue", linetype = "dashed") + # add conventional reference line
        ylim(custom_min_LRR(), custom_max_LRR()) + # will eventually be on x axis
        labs(x = "Agricultural system", y = "Adjusted log response ratio") +
        coord_flip() + # horizontal so looks more like a forest plot
        annotate("text", x = custom_signif_results_plus_0.25(), y = custom_adjusted_LRR(),
                 label = "*", size = 8, colour = "blue") +
        theme_bw() +
        theme(axis.text = element_text(size=18),
              axis.title = element_text(size=22),
              panel.grid.major = element_blank(), # no grid lines
              panel.grid.minor = element_blank())

      ### plot percentage change graph
    } else if (input$metric2 == "Percentage change") {
      custom_ordered_robust_coefficients() %>%
        ggplot(aes(x = reorder(Treatment, -percentage_change), percentage_change)) + # reorder the x variable (treatments) in order of ascending % change value
        geom_errorbar(aes(x = reorder(Treatment, -percentage_change), ymin=LCI_percent, ymax=UCI_percent), width=0.2, colour="black", alpha=0.9, size=1.3) +
        geom_point(stat="identity", size = 4, colour = "black") +
        geom_hline(yintercept = 0, color = "blue", linetype = "dashed") + # add conventional reference line
        ylim(custom_min_percent(), custom_max_percent()) +
        labs(x = "Agricultural system", y = "Percentage change") +
        coord_flip() + # horizontal so looks more like a forest plot
        annotate("text", x = custom_signif_results_plus_0.25(), y = custom_percentage_change(),
                 label = "*", size = 8, colour = "blue") +
        theme_bw() +
        theme(axis.text = element_text(size=18),
              axis.title = element_text(size=22),
              panel.grid.major = element_blank(), # no grid lines
              panel.grid.minor = element_blank())
    }


  })

  # Produce figure legend
  output$custom_model_figure_legend <- renderText({

    paste("<b>Figure 3. </b>", "Compares the biodiversity measured by ", input$metric, " across different agricultural systems compared to the conventional agricultural system
          (blue dashed line). Significance is indicated by blue stars (absolute t-value greater than 1.96).")
  })

  # ----------------------------------------------------------------------------------------------------------------

  ### Make the table and table legend for custom model

  # Produce table legend
  output$table_legend_custom_model_output <- renderText({

    paste("<b>Table 4. </b>", "Model coefficients extracted or calculated from the model summary. Agricultural systems with absolute t-values greater than 1.96
          have significantly different levels of biodiversity than the conventional agricultural system (", custom_model_analysis()[1, 13], " data points).", sep = "")
  })

  # Produce table
  output$custom_model_output_table <- renderTable({

    custom_model_output_table <- custom_ordered_robust_coefficients()[c("Treatment","t", "Adjusted_LRR", "percentage_change", "Frequency")] %>%
      arrange(Adjusted_LRR, desc(Treatment))

    # Remove as not going to include conventional in table anymore as doesn't make sense - just put number of data points in the legend.
    #custom_model_output_table <- rbind(custom_model_analysis()[1, c(1,4,5,9,13)], custom_model_output_table) # Add in row for conventional at the top of table

    custom_model_output_table <- custom_model_output_table %>%
      relocate(t, .before = Frequency)

    # Remove _ from colnames
    colnames(custom_model_output_table) <- c("Agricultural system", "Adjusted log response ratio", "Percentage change", "t-value", "Data points")

    custom_model_output_table <- custom_model_output_table

  },

  striped = TRUE

  )

  # ----------------------------------------------------------------------------------------------------------------

  ### Downloading the custom R model output and coefficients table

  # Disable the download buttons on page load - so can't click it until a model has successfully run
  shinyjs::disable("download_custom_model_output")
  shinyjs::disable("download_custom_model_coeffs")

  # Disable the download buttons if the biodiversity_metric_category choice changes
  observeEvent(input$biodiversity_metric_category, {
    shinyjs::disable("download_custom_model_output")
    shinyjs::disable("download_custom_model_coeffs")
  })

  # Download custom model output button
  output$download_custom_model_output <- downloadHandler(
    filename = function() {
      paste0("custom_model_output", Sys.Date(), ".txt", sep="")
    },
    content = function(file) {
      write.table(custom_model_summary(), file)
    }
  )

  # Download coefficients table button
  output$download_custom_model_coeffs <- downloadHandler(
    filename = function() {
      paste0("custom_model_coefficients", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(custom_model_analysis(), file)
    }
  )

  # ----------------------------------------------------------------------------------------------------------------

  ### Details of agricultural systems table which shows once button is clicked, and disappears once hide button is clicked

  defs_data2 <- reactiveValues() # reactiveValues function returns an object for storing reactive values

  observeEvent(input$show2, { # When "show" button is selected, the object stores the agri_systems_def data
    defs_data2$data <- agri_systems_def
  })

  observeEvent(input$hide2, { # When "hide" button is selected, the object stores nothing
    defs_data2$data <- NULL
  })

  output$agri_sys_defs_table2 <- renderTable({
    if (is.null(defs_data2$data)) { # If object isn't storing anything, return nothing (i.e. don't show table)
      return()
    } else { # Else, show the table
      agri_systems_def <- agri_systems_def %>%
        arrange(Agricultural_system)
    }

  },

  striped = TRUE

  )

  # ----------------------------------------------------------------------------------------------------------------

  # =================================================================================================================
  # =================================================================================================================

  ##### Upload data tab

  # =================================================================================================================
  # =================================================================================================================

  ### Overview:
  # The user has to input their first and second name (containing only letters to avoid things such as code injection)
  # Then they can upload their file (as long as it meets checks e.g. is a csv, contains correct column, is not a duplicate) and preview it
  # Once previewed, can press submit to googlesheets to upload it to here. Get a completion message if successful.

  # ----------------------------------------------------------------------------------------------------------------

  # Disable the browse/upload button on page load - so can't click it until user has inputted acceptable first and second name
  shinyjs::disable("preview_upload")

  # Also disable the upload to googlesheets button
  shinyjs::disable("upload_to_googlesheets")

  # Enable the browse/upload button if acceptable names have been inputted, otherwise disable it (and disable upload to googlesheets button too)
  observe({
    if (input$first_name != "" &&
        input$second_name != "" &&
        !str_detect(input$first_name, "[^A-Za-z-\\s]") && # name can only contain letters, hyphens, or spaces
        !str_detect(input$second_name, "[^A-Za-z-\\s]")) {

      # enable the browse/upload button
      shinyjs::enable("preview_upload")
    } else {
      shinyjs::disable("preview_upload")
      shinyjs::disable("upload_to_googlesheets")
    }
  })

  # ----------------------------------------------------------------------------------------------------------------

  # The following 3 observeEvents hide the output$upload_complete message
  # (which indicates that a data sheet has been successfully uploaded to googlesheets) if the inputted names or file changes
  # Also disables the upload_to_googlesheets button

  observeEvent(input$first_name, {
    shinyjs::hide("upload_complete")
    shinyjs::disable("upload_to_googlesheets")
  })

  observeEvent(input$second_name, {
    shinyjs::hide("upload_complete")
    shinyjs::disable("upload_to_googlesheets")
  })

  observeEvent(input$preview_upload, {
    shinyjs::hide("upload_complete")
    shinyjs::disable("upload_to_googlesheets")
  })

  # ----------------------------------------------------------------------------------------------------------------

  ### Validation checks on the names inputted which gives feedback to the user
  # Same as checks which enable the browse button, BUT needed in additional to provide user feedback

  output$name_inputted <- renderText({

    validate(

      # First name must not be empty
      need(input$first_name != "", "First name must not be empty."),

      # Second name must not be empty
      need(input$second_name != "", "Second name must not be empty."),

      # First_name must only contain letters
      need(!str_detect(input$first_name, "[^A-Za-z-\\s]"), "First name can only contain letters."), # if contains anything but letters, hyphens, or spaces, it returns FALSE

      # Second_name must only contain letters
      need(!str_detect(input$second_name, "[^A-Za-z-\\s]"), "Second name can only contain letters.") # if contains anything but letters, hyphens, or spaces, it returns FALSE

    )

    paste("") # return blank message if name meets criteria

  })

  # ----------------------------------------------------------------------------------------------------------------

  # Preview the data to be uploaded to googlesheets and enable upload_to_googlesheets button if certain checks are met
  output$preview_upload_data <- renderTable({

    req(input$preview_upload)

    # Validation checks 1
    validate(
      # Make sure file is a csv
      need(tools::file_ext(input$preview_upload$name) == "csv", "File must be a .csv.") %then% # the 'then' function is defined in global.R

        # Make sure file has certain columns - for now LRR and Treatment
        need("LRR" %in% colnames(read.csv(input$preview_upload$datapath)) && "Treatment" %in% colnames(read.csv(input$preview_upload$datapath)), "Data is missing a column(s) with the name(s) 'LRR' and/or 'Treatment.")
    )


    # If passes the checks above, also want to check it is not a duplicate of data already in googlesheets
    # Read in the the sheets present within the shiny_app_data googlesheet
    sheets_names <- sheet_names(sheet_id)

    # Read each of the sheets and load their data. Puts them into a list of dataframes
    list_of_data <- lapply(sheets_names, function (x) read_sheet(ss = sheet_id, sheet = x))

    # Count the number of sheets present
    number_of_studies <- length(list_of_data)

    # Check if any of these are identical to the one the user is trying to upload - output needs to only contain FALSE
    output <- c()
    if (number_of_studies > 1) {
      for (i in 1:number_of_studies) {
        output <- c(output, (isTRUE(all.equal(read.csv(input$preview_upload$datapath), as.data.frame(list_of_data[[i]])))))
      }
    }

    # Validation checks 2
    validate(

      # Make sure file is not a duplicate of one already in the googlesheets
      need(all(output == FALSE), "This is a duplicate of a dataframe already in existence") # Check if they are all FALSE i.e. no identical dataframes

    )

    # Checks needed to enable to upload to googlesheets button - need all checks to be met (inputted name and file)
    if (input$first_name != "" &&
        input$second_name != "" &&
        !str_detect(input$first_name, "[^A-Za-z-\\s]") &&
        !str_detect(input$second_name, "[^A-Za-z-\\s]") &&
        tools::file_ext(input$preview_upload$name) == "csv" &&
        "LRR" %in% colnames(read.csv(input$preview_upload$datapath)) &&
        "Treatment" %in% colnames(read.csv(input$preview_upload$datapath)) &&
        all(output == FALSE)) {
      # enable the browse/upload button
      shinyjs::enable("upload_to_googlesheets")
    }

    # Produce the table
    preview_upload_data <- head(read.csv(input$preview_upload$datapath))

  },

  striped = TRUE

  )


  # Add table legend
  output$preview_upload_data_legend <- renderText({
    paste("<b>Table 5.</b>", "Preview of your data, which you can upload to the database.")
  })


  # ----------------------------------------------------------------------------------------------------------------

  # Upon acceptable name being entered, and selecting an acceptable file, save it to googlesheets, and return a message if successful

  observeEvent(input$upload_to_googlesheets, {

    # Access actual data via datapath column of the dataframe returned by fileInput()
    data <- read.csv(input$preview_upload$datapath)

    # Create a unique file name
    file_name <- paste0(input$first_name, "_", input$second_name, "_", as.integer(Sys.time()), sep = "")

    # Create a filepath using this file_name
    file_path <- file.path(tempdir(), file_name)

    # Write the data to a temporary file locally
    write.csv(data, file_path, row.names = FALSE)

    # Add new tab to gogglesheet
    sheet_add(ss = sheet_id, sheet = file_name)

    # Add new data to this sheet
    write_sheet(data, ss = sheet_id, sheet = file_name)

    shinyjs::show("upload_complete")

    output$upload_complete <- renderText({

      paste0("File with name: ", input$first_name, " ", input$second_name, " ", input$preview_upload$name, " successfully uploaded.")

    })

    shinyjs::disable("upload_to_googlesheets")

  })

  # ----------------------------------------------------------------------------------------------------------------

  # =================================================================================================================
  # =================================================================================================================

  ##### References tab

  # =================================================================================================================
  # =================================================================================================================

  ### References table

  # Add table legend
  output$references_table_legend <- renderText({
    paste("<b>Table 6.</b>", "References for all papers included in the app.")
  })

  # Add references table
  output$references_table <- renderTable({

    # This was giving duplicates of some papers due to tiny differences in paper title (e.g. \n vs \r)
    # references_table <- unique(data()[, c("Paper_ID", "Synthesis_type", "Title", "Author")]) %>%
    #   arrange(Paper_ID)

    # Keep all columns - not just the Paper_ID
    references_table <- data() %>%
      distinct(Paper_ID, .keep_all = TRUE)

    # Now just include the columns we want, and arrange in alphabetical order
    references_table <- references_table[, c("Paper_ID", "Synthesis_type", "Title", "Author")] %>%
      arrange(Paper_ID)

  },

  striped = TRUE

  )

  # =================================================================================================================
  # =================================================================================================================

}


