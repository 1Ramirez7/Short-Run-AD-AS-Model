library(shiny)
library(ggplot2)
library(DT)
library(zip)
library(bslib)
library(rio)

# version 0.1 or # v0.1

source("modules/simulation_calc.R") # module for simulation calculations
source("modules/mathformulas.R") # module for math formulas


# helper function to create each experiment set
initializeExperimentSet <- function(
    expSuffix,         # label like "1", "2", "3", or "4"
    paramNameInput,    # e.g. "param_name", "param_name2"
    newValueInput,     # e.g. "value", "value2"
    startPeriodInput,  # e.g. "start_period", "start_period2"
    lengthInput,       # e.g. "length", "length2"
    addExpButton,      # e.g. "add_experiment", "add_experiment2"
    dtOutput,          # e.g. "experimentsTable", "experimentsTable2"
    deleteRowInput,    # internal ID prefix for deletion, e.g. "delete_1", "delete_2"
    input, output, session
) {
  
  # A reactiveVal to hold the table of experiments for this set
  rv <- reactiveVal(
    data.frame(
      param        = character(0),
      value    = numeric(0),
      start_period = integer(0),
      length       = integer(0),
      stringsAsFactors = FALSE
    )
  )
  
  # Add new experiment row when the user clicks
  observeEvent(input[[addExpButton]], {
    new_experiment <- data.frame(
      param        = input[[paramNameInput]],
      value    = input[[newValueInput]],
      start_period = input[[startPeriodInput]],
      length       = input[[lengthInput]],
      stringsAsFactors = FALSE
    )
    rv( rbind(rv(), new_experiment) )
  })
  
  # Render the experiments table with "Delete" buttons
  output[[dtOutput]] <- renderDT({
    exps <- rv()
    if (nrow(exps) == 0) return(NULL)
    
    deleteButtons <- sapply(seq_len(nrow(exps)), function(i) {
      as.character(
        actionButton(
          inputId = paste0("delete_", expSuffix, "_", i),
          label = "Delete",
          class = "btn btn-danger btn-sm",
          onclick = paste0(
            'Shiny.setInputValue("delete_', expSuffix, '", this.id, {priority: "event"})'
          )
        )
      )
    })
    
    exps$Delete <- deleteButtons
    
    datatable(
      exps,
      escape = FALSE,
      selection = "none",
      options   = list(pageLength = 5)
    )
  }, server = FALSE)
  
  # Handle row deletions
  observeEvent(input[[paste0("delete_", expSuffix)]], {
    row_str <- gsub(paste0("delete_", expSuffix, "_"), "", input[[paste0("delete_", expSuffix)]])
    row_num <- as.numeric(row_str)
    
    exps <- rv()
    if (!is.na(row_num) && row_num >= 1 && row_num <= nrow(exps)) {
      exps <- exps[-row_num, ]
      rv(exps)
    }
  })
  
  return(rv)
}


############
# Helper function for endo table
#
#################


initializeEndoSet <- function(
    endoSuffix,         # label like "1", "2", "3", ...
    edParamNameInput, # e.g. "endo_param_name1"
    newedValueInput,     # e.g. "endo_value1"
    StartperiodedInput,     # e.g. "endo_start_period1"
    LengthInputed,    # e.g. "endo_length1"
    addEndoButton,      # e.g. "add_endo_mod1"
    dtOutputId,         # e.g. "endoTable1"
    deletePrefix,       # e.g. "delete_endo_"
    input, output, session
) {
  # A separate reactiveVal specifically for the 'endo' table
  rvEndo <- reactiveVal(
    data.frame(
      param        = character(0),
      value        = numeric(0),
      start_period = integer(0),
      length       = integer(0),
      stringsAsFactors = FALSE
    )
  )
  
  # Observe: Add row when user clicks 'Add' for this set
  observeEvent(input[[addEndoButton]], {
    new_endo <- data.frame(
      param        = input[[edParamNameInput]],
      value        = input[[newedValueInput]],
      start_period = input[[StartperiodedInput]],
      length       = 1, # v4.2 # defaulting to 1 for now. 
      # this is old code for lenght: input[[LengthInputed]]
      stringsAsFactors = FALSE
    )
    rvEndo( rbind(rvEndo(), new_endo) )
  })
  
  # Render the table with "Delete" buttons
  output[[dtOutputId]] <- renderDT({
    endos <- rvEndo()
    if (nrow(endos) == 0) return(NULL)
    
    # Create a delete button for each row
    deleteButtons <- sapply(seq_len(nrow(endos)), function(i) {
      as.character(
        actionButton(
          inputId = paste0("deleteendo_", endoSuffix, "_", i),
          label   = "Delete",
          class   = "btn btn-danger btn-sm",
          onclick = paste0(
            # The JS will set the input with the row to delete
            'Shiny.setInputValue("deleteendo_', endoSuffix, '", this.id, {priority: "event"})'
          )
        )
      )
    })
    endos$Delete <- deleteButtons
    
    datatable(
      endos,
      escape    = FALSE,
      selection = "none",
      options   = list(pageLength = 5)
    )
  }, server = FALSE)
  
  # Observe: Delete rows
  observeEvent(input[[paste0("deleteendo_", endoSuffix)]], {
    row_str <- gsub(paste0("deleteendo_", endoSuffix, "_"), "", input[[paste0("deleteendo_", endoSuffix)]])
    row_num <- as.numeric(row_str)
    
    endos <- rvEndo()
    if (!is.na(row_num) && row_num >= 1 && row_num <= nrow(endos)) {
      endos <- endos[-row_num, ]
      rvEndo(endos)
    }
  })
  
  return(rvEndo)
} # end of helper function for endo table




#--------------------------------------------------------
#     UI # version 1
#--------------------------------------------------------
# app layout and input controls version1
ui <- page_sidebar(
  title = "The Short Run - AS/AD Framework",
  sidebar = sidebar(
    width = 600,  # adjust the width as needed (not friendly with smaller window size)
    h4("Simulation Parameters"),
    sliderInput("Simulation_Period", "Simulation Period (yrs)",
                min = 5, max = 100, value = 20, step = 1),
    numericInput("plot_start_year", "Plot: Start Year (Period 0)", value = 1776, step = 1),
    sliderInput("a", "Aggregate Demand Shock (a)", min = 0, max = 1, value = 0.0, step = 0.01),
    sliderInput("o", "Shock to Inflation (o)", min = 0, max = 1, value = 0.0, step = 0.01),
    sliderInput("b", "Response to R-r (b)", min = 0, max = 1, value = 0.68, step = 0.01),
    sliderInput("v", "Demand Conditions (v)", min = 0, max = 0.1, value = 0.076, step = 0.001),
    sliderInput("m", "Governs Monetary Policy response to Inflation (m)", min = 0, max = 1, value = 0.5, step = 0.01),
    sliderInput("n", "Parameter n", min = 0, max = 1, value = 0.5, step = 0.01),
    sliderInput("r_bar", "Real Interest Rate (Long Run) [r_bar]", min = 0, max = 10, value = 2, step = 0.1),
    sliderInput("fed_target_initial_inflation", "Initial Target Inflation (%)",
                min = 0, max = 5, value = 2.0, step = 0.1),
    
    h4("Quantitative Experiments"),
    
    do.call(tabsetPanel, c(id = "experiment_tabs",
                           lapply(1:4, function(i) {
                             tabPanel(paste0("Exps. Set ", i),
                                      selectInput(paste0("param_name", i), paste0("Exogenous Variables (a, o, b, v, m, n, fed_target_initial_inflation) Set ", i),
                                                  choices = c("a", "o", "b", "v", "m", "n", "fed_target_initial_inflation"),
                                                  selected = "a"),
                                      numericInput(paste0("value", i), paste0("New Value Set ", i), value = 5, step = 0.01),
                                      numericInput(paste0("start_period", i), paste0("Start Period Set ", i), value = 5, step = 1),
                                      numericInput(paste0("length", i), paste0("Length of Effect Set ", i), value = 5, step = 1),
                                      actionButton(paste0("add_experiment", i), paste0("Add Exp ", i)),
                                      fileInput(paste0("upload_file_", i), paste0("Upload Excel for Exp ", i), accept = c(".xlsx", ".xls")),
                                      div(style = "width: 400px;", DTOutput(paste0("experimentsTable", i)))
                             )
                           })
    )),
    # -- END TABSETPANEL -- 
    
    
    h4("Endogenous modifications"),
    # Endo Table Sets Panel in Sidebar
    do.call(tabsetPanel, c(id = "endo_tabs",
                           lapply(1:4, function(i) {
                             tabPanel(paste0("Endo Set ", i),
                                      selectInput(paste0("endo_param_name", i), paste0(" Variables: update this list) Set ", i), 
                                                  choices = c("inflation_stabilization_inflation", "inflation_stabilization_sroutput", "output_stabilization_inflation", "output_stabilization_sroutput", "taylor_rule_inflation", "taylor_rule_sroutput"), 
                                                  selected = "taylor_rule_sroutput"),
                                      numericInput(paste0("endo_value", i),  paste0("New Value ", i), value = 0, step = 0.1),
                                      numericInput(paste0("endo_start_period", i), paste0("Modification Period ", i), value = 10, step = 1),
                                      #numericInput(paste0("endo_length", i), paste0("Length of Effect ", i), value = 1,  step = 1), # remove since it is not use yet. endo mods are only applied for one period for now. # calculations overwrite endo params > than 1 period
                                      actionButton(paste0("add_endomod", i), paste0("Add Endo ", i)),
                                      fileInput(paste0("endo_upload_file_", i), paste0("Upload Excel for Endo ", i), accept = c(".xlsx", ".xls")),
                                      div(style = "width: 400px;", DTOutput(paste0("endoTable", i ))))}))), 
    
    # Optional checkboxes for plot display options
    checkboxInput("show_counter_visual", "Show Counterfactual", value = TRUE),
    checkboxInput("show_second_exp_visual", "Show Second Experiment", value = FALSE),
    checkboxInput("show_third_exp_visual","Show Third Experiment", value = FALSE),
    checkboxInput("show_fourth_exp_visual", "Show Fourth Experiment", value = FALSE),
    
    actionButton("simulate", "Simulate"),
    h4("Download Results"),
    downloadButton("downloadPlots", "Download All Plots as PNG (ZIP)"),
    downloadButton("downloadData", "Download Data as CSV")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Plots",
               div(style = "width: 1800px; margin: auto;", 
                   fluidRow(
                     column(6, plotOutput("plot_inflation_stabilization_inflation", width = "900px", height = "600px")),
                     column(6, plotOutput("plot_inflation_stabilization_sroutput", width = "900px", height = "600px"))
                   ),
                   fluidRow(
                     column(6, plotOutput("plot_output_stabilization_inflation", width = "900px", height = "600px")),
                     column(6, plotOutput("plot_output_stabilization_sroutput", width = "900px", height = "600px")),
                     column(6, plotOutput("plot_taylor_rule_inflation", width = "900px", height = "600px")),
                     column(6, plotOutput("plot_taylor_rule_sroutput", width = "900px", height = "600px"))
                   ))),
      tabPanel("Ratio scale ? update",
               div(style = "width: 1800px; margin: auto;",
                   fluidRow(
                     column(6, plotOutput("plot_fed_target_initial_inflation", width = "900px", height = "600px")),
                     column(6, plotOutput("plot_R", width = "900px", height = "600px"))),
                   fluidRow(
                     column(6, plotOutput("plot_n", width = "900px", height = "600px")),
                     column(6, plotOutput("plot_v", width = "900px", height = "600px")))
               )
      ),
      tabPanel("Program Results",
               tabsetPanel(
                 tabPanel("First Scenario", tableOutput("results_first_exp_df")),
                 tabPanel("Counterfactual", tableOutput("results_counterfactual_df")),
                 tabPanel("Second Scenario", tableOutput("results_second_exp_df")),
                 tabPanel("Third Scenario", tableOutput("results_third_exp_df")),
                 tabPanel("Fourth Scenario", tableOutput("results_fourth_exp_df"))
               )
      ),
      tabPanel("World", # tab for adding data, needs changing to fit this model
               selectInput("selectedSheet", "Select Region", choices = NULL),
               selectInput("selectedColumn", "Select Country", choices = NULL),
               numericInput("startYear", "Start Year", value = 1960),
               numericInput("endYear",   "End Year",   value = 2023),
               actionButton("calculateAvg", "Get Avg Savings Rate 's'"),
               verbatimTextOutput("avgSavingsRate")  # displays the avg s calculated
      ),
      tabPanel("Formulas Detail", # math formuals
               modFormulaDisplayUI("myFormulas"),  # math formula module
               plotOutput("plot_as_ad_framework") # v2
      )
    )
  )
)

#--------------------------------------------------------
#     SERVER  # version 1
#--------------------------------------------------------
server <- function(input, output, session) {
  
  # adding option for Savings rate average for list of countries. 
  excel_list <- import_list("data/savings_rate_y.xlsx") 
  
  # Populate region choices
  observe({
    updateSelectInput(session, "selectedSheet", choices = names(excel_list), selected = names(excel_list)[1])
  })
  observeEvent(input$selectedSheet, {  # Update column choices
    df <- excel_list[[input$selectedSheet]]
    updateSelectInput(session, "selectedColumn", choices = setdiff(names(df), "year"), selected = setdiff(names(df), "year")[1])
  })
  
  # find the available years
  observeEvent(input$selectedColumn, {
    df <- excel_list[[input$selectedSheet]]
    yearVals <- df$year
    yVals <- as.numeric(df[[input$selectedColumn]])
    valid <- !is.na(yearVals) & !is.na(yVals)
    if (!any(valid)) return()
    validYears <- yearVals[valid]
    minY <- min(validYears)
    maxY <- max(validYears)
    updateNumericInput(session, "startYear", min = minY, max = maxY, value = minY)
    updateNumericInput(session, "endYear",   min = minY, max = maxY, value = maxY)
  })
  
  # Calculate average within the chosen year range
  observeEvent(input$calculateAvg, {
    df <- excel_list[[input$selectedSheet]]
    yearVals <- df$year
    yVals <- as.numeric(df[[input$selectedColumn]])
    valid <- !is.na(yearVals) & !is.na(yVals)
    dfValid <- df[valid, ]
    
    subsetData <- dfValid[dfValid$year >= input$startYear & dfValid$year <= input$endYear, ]
    if (nrow(subsetData) == 0) {  # Check if country has any data
      output$avgSavingsRate <- renderPrint("Country Data Not Available") 
      return()
    }
    
    avgVal <- (mean(subsetData[[input$selectedColumn]]))/100
    updateSliderInput(session, "s", value = avgVal)
    output$avgSavingsRate <- renderPrint(paste("Average Savings Rate:", avgVal)) # Display the average
  }) # 
  
  #----------------------------------
  # EXPERIMENTS TABLE MANAGEMENT # version 1
  #----------------------------------
  # This code along w/ the initializeExperiment function help loop in experiment simulations. 
  experiment_sets <- lapply(1:4, function(i) { # looping exp tables
    initializeExperimentSet(
      expSuffix        = as.character(i),
      paramNameInput   = paste0("param_name", i),
      newValueInput    = paste0("value", i),
      startPeriodInput = paste0("start_period", i),
      lengthInput      = paste0("length", i),
      addExpButton     = paste0("add_experiment", i),
      dtOutput         = paste0("experimentsTable", i),
      deleteRowInput   = paste0("delete_", i),
      input = input, output = output, session = session
    )
  })
  names(experiment_sets) <- paste0("experiments", 1:4)  # looping exp tables
  
  
  # adding excel upload for faster param inputs
  # loop this to match expe loop code
  for (i in 1:4) {
    local({
      set_index <- i
      observeEvent(input[[paste0("upload_file_", set_index)]], {
        req(input[[paste0("upload_file_", set_index)]])
        df_uploaded <- import(input[[paste0("upload_file_", set_index)]]$datapath)
        
        # Validate columns
        required_cols <- c("param", "value", "start_period", "length")
        if (!all(required_cols %in% names(df_uploaded))) {
          showModal(modalDialog(
            title = "Error",
            "The uploaded file must contain columns: param, value, start_period, length.",
            easyClose = TRUE
          ))
          return(NULL)
        }
        
        # Append
        current_data <- experiment_sets[[paste0("experiments", set_index)]]()
        updated_data <- rbind(current_data, df_uploaded)
        experiment_sets[[paste0("experiments", set_index)]](updated_data)
      }) # v3.1 and v3.2 end
    })
  }
  
  #########
  #
  # making endo server function
  ############
  
  endo_sets <- lapply(1:4, function(i) {
    initializeEndoSet(
      endoSuffix         = as.character(i), # e.g. "1", "2", etc.
      edParamNameInput = paste0("endo_param_name", i),
      newedValueInput     = paste0("endo_value", i),
      StartperiodedInput     = paste0("endo_start_period", i),
      LengthInputed    = paste0("endo_length", i),
      addEndoButton      = paste0("add_endomod", i),
      dtOutputId         = paste0("endoTable", i),  # Unique output ID
      deletePrefix       = paste0("delete_endo_", i), # So it won't conflict
      input  = input, output = output, session = session
    )
  })
  
  # Give the list a logical name
  names(endo_sets) <- paste0("endo_", 1:4) # enf of server for endo
  
  for (i in 1:4) { # adding excel upload
    local({
      endo_index <- i
      observeEvent(input[[paste0("endo_upload_file_", endo_index)]], {
        req(input[[paste0("endo_upload_file_", endo_index)]])
        df_endo_uploaded <- import(input[[paste0("endo_upload_file_", endo_index)]]$datapath)
        
        # Validate columns
        endo_required_cols <- c("param", "value", "start_period", "length")
        if (!all(endo_required_cols %in% names(df_endo_uploaded))) {
          showModal(modalDialog(
            title = "Error",
            "The uploaded file must contain columns: param, value, start_period, length.",
            easyClose = TRUE
          ))
          return(NULL)
        }
        
        # Append
        current_endo_data <- endo_sets[[paste0("endo_", endo_index)]]()
        updated_endo_data <- rbind(current_endo_data, df_endo_uploaded)
        endo_sets[[paste0("endo_", endo_index)]](updated_endo_data)
      }) # v4.1 end
    })
  }
  
  
  
  #----------------------------------
  # EVENT-REACTIVE RESULTS # version
  #----------------------------------
  # edit to loop with lapply exp tables
  
  simulate_first_exp_calculations <- eventReactive(input$simulate, {
    # If first set is empty, show a modal (original logic)
    if (nrow(experiment_sets$experiments1()) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Please add at least one experiment in Set 1 before running the simulation.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    num_periods <- input$Simulation_Period + 1
    simulate_solow(num_periods, input$a, input$o, input$b, input$v, input$m, input$n, input$r_bar, input$fed_target_initial_inflation, experiments_df = experiment_sets$experiments1(), endomods_df = endo_sets$endo_1())
  })
  
  # Define a Counterfactual (no experiments)
  simulate_counter_exp_calculations <- eventReactive(input$simulate, {
    num_periods <- input$Simulation_Period + 1
    simulate_solow(num_periods, input$a, input$o, input$b, input$v, input$m, input$n, input$r_bar, input$fed_target_initial_inflation, experiments_df = NULL)  # no overrides
  })
  
  # Define a SECOND scenario (Set 2)
  simulate_second_exp_calculations <- eventReactive(input$simulate, {
    num_periods <- input$Simulation_Period + 1
    experiments_df_to_use <- if (nrow(experiment_sets$experiments2()) == 0) NULL else experiment_sets$experiments2()
    endo_df_to_use <- if (nrow(endo_sets$endo_2()) == 0) NULL else endo_sets$endo_2()  # v4.1
    simulate_solow(num_periods, input$a, input$o, input$b, input$v, input$m, input$n, input$r_bar, input$fed_target_initial_inflation, experiments_df = experiments_df_to_use, endomods_df = endo_df_to_use)
  })
  
  # Define a THIRD scenario (Set 3)
  simulate_third_exp_calculations <- eventReactive(input$simulate, {
    num_periods <- input$Simulation_Period + 1
    experiments_df_to_use <- if (nrow(experiment_sets$experiments3()) == 0) NULL else experiment_sets$experiments3()
    endo_df_to_use <- if (nrow(endo_sets$endo_3()) == 0) NULL else endo_sets$endo_3()
    simulate_solow(num_periods, input$a, input$o, input$b, input$v, input$m, input$n, input$r_bar, input$fed_target_initial_inflation, experiments_df = experiments_df_to_use, endomods_df = endo_df_to_use)
  })
  
  # Define a FOURTH scenario (Set 4)
  simulate_fourth_exp_calculations <- eventReactive(input$simulate, {
    num_periods <- input$Simulation_Period + 1
    experiments_df_to_use <- if (nrow(experiment_sets$experiments4()) == 0) NULL else experiment_sets$experiments4()
    endo_df_to_use <- if (nrow(endo_sets$endo_4()) == 0) NULL else endo_sets$endo_4()
    simulate_solow(num_periods, input$a, input$o, input$b, input$v, input$m, input$n, input$r_bar, input$fed_target_initial_inflation, experiments_df = experiments_df_to_use, endomods_df = endo_df_to_use)
  })
  
  #----------------------------------
  # 4. OUTPUT: TABLES - data frames # version 1
  #----------------------------------
  
  # Render the result table for each scenario
  output$results_first_exp_df <- renderTable({req(simulate_first_exp_calculations()); 
    simulate_first_exp_calculations()}, rownames = FALSE)
  output$results_counterfactual_df <- renderTable({ req(simulate_counter_exp_calculations()) ; 
    simulate_counter_exp_calculations() }, rownames = FALSE)
  output$results_second_exp_df <- renderTable({ req(simulate_second_exp_calculations()); 
    simulate_second_exp_calculations() }, rownames = FALSE)
  output$results_third_exp_df<- renderTable({ req(simulate_third_exp_calculations()); 
    simulate_third_exp_calculations() }, rownames = FALSE)
  output$results_fourth_exp_df <- renderTable({ req(simulate_fourth_exp_calculations()); 
    simulate_fourth_exp_calculations() }, rownames = FALSE) 
  
  
  
  #---------------------------
  # 5. PLOTS # version 1
  #---------------------------
  plot_theme <- theme_bw() + 
    theme(
      plot.title        = element_text(hjust = 0.5),
      axis.title        = element_text(size = 12),
      axis.text         = element_text(size = 10),
      panel.grid.major  = element_line(color = "grey80"),
      panel.grid.minor  = element_blank(),
      strip.background  = element_rect(fill = "grey90", color = "grey90"),
      legend.position = "top",# v4.3
      legend.title = element_blank() # v4.3
    )
  
  # plot placeholder
  make_plot <- function(
    show_plot_placeholder_first,
    show_plot_placeholder_counter,
    show_plot_placeholder_second = NULL,
    show_plot_placeholder_third = NULL,
    show_plot_placeholder_fourth = NULL,
    y_var, title, y_label,
    show_counter_visual = TRUE,
    show_second_exp_visual = FALSE,
    show_third_exp_visual = FALSE,
    show_fourth_exp_visual = FALSE
  ) {
    
    # Base plot with the main experiment data
    p <- ggplot(show_plot_placeholder_first, aes(x = input$plot_start_year + Period, y = .data[[y_var]])) +
      geom_line(aes(color = "First Scenario")) +
      geom_point(data = if(nrow(show_plot_placeholder_first) > 10) { # v3.3.3 adding point limit
        show_plot_placeholder_first[round(seq(1, nrow(show_plot_placeholder_first), length.out = 10)), ]
      } else {
        show_plot_placeholder_first
      }, aes(color = "First Scenario")) + # v3.3.3 end of adding point limit
      scale_color_manual(values = c("First Scenario" = "blue",
                                    "Counterfactual" = "red",
                                    "Second Scenario"  = "green",
                                    "Third Scenario" = "purple",
                                    "Fourth Scenario"  = "orange")) +
      ggtitle(title) + xlab("Period") + ylab(y_label) +
      plot_theme
    
    # Optionally add no-exp line
    if (show_counter_visual && !is.null(show_plot_placeholder_counter)) {
      p <- p +
        geom_line(data = show_plot_placeholder_counter, 
                  aes(x = input$plot_start_year + Period, y = .data[[y_var]], color = "Counterfactual"), 
                  linetype = "dashed") +
        geom_point(data = if(nrow(show_plot_placeholder_counter) > 12) { # v3.3.3 adding point limit
          tmp <- show_plot_placeholder_counter[-1, ] # remove n rows
          tmp[round(seq(1, nrow(tmp), length.out = 12)), ]
        } else {
          show_plot_placeholder_counter[-1, ] # end v3.3.3 limit points
        }, aes(x = input$plot_start_year + Period, y = .data[[y_var]], color = "Counterfactual"), 
        shape = 1, size = 2)
    }
    
    # Optionally add Second Scenario
    if (show_second_exp_visual && !is.null(show_plot_placeholder_second)) {
      p <- p +
        geom_line(data = show_plot_placeholder_second,
                  aes(x = input$plot_start_year + Period, y = .data[[y_var]], color = "Second Scenario")) +
        geom_point(data = if(nrow(show_plot_placeholder_second) > 14) { # v3.3.3 adding point limit
          tmp <- show_plot_placeholder_second[-(1:3), ]
          tmp[round(seq(1, nrow(tmp), length.out = 14)), ]
        } else {
          show_plot_placeholder_second[-(1:3), ] # end v3.3.3 
        }, aes(x = input$plot_start_year + Period, y = .data[[y_var]], color = "Second Scenario"), size = 1)
    }
    
    # Optionally add Third Scenario
    if (show_third_exp_visual && !is.null(show_plot_placeholder_third)) {
      p <- p +
        geom_line(data = show_plot_placeholder_third,
                  aes(x = input$plot_start_year + Period, y = .data[[y_var]], color = "Third Scenario")) +
        geom_point(data = if(nrow(show_plot_placeholder_third) > 16) { # v3.3.3 adding point limit
          tmp <- show_plot_placeholder_third[-(1:5), ]
          tmp[round(seq(1, nrow(tmp), length.out = 16)), ]
        } else {
          show_plot_placeholder_third[-(1:5), ] # end v3.3.3 
        }, aes(x = input$plot_start_year + Period, y = .data[[y_var]], color = "Third Scenario"), size = 1)
    }
    
    # Optionally add Fourth Scenario
    if (show_fourth_exp_visual && !is.null(show_plot_placeholder_fourth)) {
      p <- p +
        geom_line(data = show_plot_placeholder_fourth,
                  aes(x = input$plot_start_year + Period, y = .data[[y_var]], color = "Fourth Scenario")) +
        geom_point(data = if(nrow(show_plot_placeholder_fourth) > 18) { # v3.3.3 adding point limit
          tmp <- show_plot_placeholder_fourth[-(1:7), ]
          tmp[round(seq(1, nrow(tmp), length.out = 18)), ]
        } else {
          show_plot_placeholder_fourth[-(1:7), ] # end v3.3.3 
        }, aes(x = input$plot_start_year + Period, y = .data[[y_var]], color = "Fourth Scenario"), size = 1)
    }
    
    p
  } # end of makeplot code
  
  
  # Store plots for download
  plot_objects <- reactiveValues()
  
  # v4 make plot list to loop render plot code
  plot_specs <- list(
    list(outputId = "plot_inflation_stabilization_inflation", y_var = "inflation_stabilization_inflation", 
         title = "Inflation Stabilization Inflation", y_label = "Change title"),
    
    list(outputId = "plot_inflation_stabilization_sroutput", y_var = "inflation_stabilization_sroutput", 
         title = "Inflation Stabilization Short-Run output", y_label = "change name"),
    
    list(outputId = "plot_output_stabilization_inflation", y_var = "output_stabilization_inflation", 
         title = "The Aggregate Supply Curve", y_label = "(AS) Inflation"), # see ch13 slide 10
    
    list(outputId = "plot_output_stabilization_sroutput", y_var = "output_stabilization_sroutput", 
         title = "output_stabilization_sroutput title", y_label = "chnage name h"),
    
    list(outputId = "plot_taylor_rule_inflation", y_var = "taylor_rule_inflation", 
         title = "taylor_rule_inflation title", y_label = "change name h"),
    
    list(outputId = "plot_taylor_rule_sroutput", y_var = "taylor_rule_sroutput",
         title = "taylor_rule_sroutput title", y_label = "chnage"),
    
    list(outputId = "plot_fed_target_initial_inflation",  y_var = "fed_target_initial_inflation", 
         title = "spacer ", y_label = "spacer "),
    
    list(outputId = "plot_R", y_var = "R",
         title = "Nominal Interest Rate (R)", y_label = "Nominal Interest Rate"),
    
    list(outputId = "plot_n", y_var = "n",
         title = "spacer 2", y_label = "spacer 2"),
    
    list(outputId = "plot_v", y_var = "v",
         title = "spacer 3", y_label = "spacer 3")
  ) # end of list code for looping renderplot code
  
  
  # loop plot outputs, storing them in the plot_objects reactiveValues
  for (spec in plot_specs) {
    
    local({
      plot_id   <- spec$outputId
      var_name  <- spec$y_var
      ttl       <- spec$title
      y_lab     <- spec$y_label
      
      output[[plot_id]] <- renderPlot({
        req(simulate_first_exp_calculations(),
            simulate_counter_exp_calculations(),
            simulate_second_exp_calculations(),
            simulate_third_exp_calculations(),
            simulate_fourth_exp_calculations())
        
        p <- make_plot(
          show_plot_placeholder_first      = simulate_first_exp_calculations(),
          show_plot_placeholder_counter    = simulate_counter_exp_calculations(),
          show_plot_placeholder_second     = simulate_second_exp_calculations(),
          show_plot_placeholder_third      = simulate_third_exp_calculations(),
          show_plot_placeholder_fourth     = simulate_fourth_exp_calculations(),
          
          y_var   = var_name, title   = ttl, y_label = y_lab,
          
          show_counter_visual      = input$show_counter_visual,
          show_second_exp_visual   = input$show_second_exp_visual,
          show_third_exp_visual    = input$show_third_exp_visual,
          show_fourth_exp_visual   = input$show_fourth_exp_visual
        )
        # store in plot_objects for zip download
        plot_objects[[plot_id]] <- p
        p
      }, res = 100)
    })
  } # end of loop plot outoputs
  
  #----------------------------------
  # AS/AD Plot (using only the first scenario) # v2
  #----------------------------------
  # this code does not work because it does two different points but it serves as a placeholder for the actual plot code for now. 
  output$plot_as_ad_framework <- renderPlot({
    req(simulate_first_exp_calculations())
    
    # Choose the specific period to plot:
    period_to_plot <- 6
    
    # Subset the simulation data for that period:
    df <- subset(simulate_first_exp_calculations(), Period == period_to_plot)
    
    # Create two rows: one for the Aggregate Supply (AS) and one for Aggregate Demand (AD)
    # AS: Uses inflation value as the equilibrium y-coordinate
    as_point <- data.frame(
      curve = "AS",
      x     = df$inflation_stabilization_sroutput,
      y     = df$inflation_stabilization_inflation
    )
    
    # AD: Uses the output value as the equilibrium y-coordinate
    ad_point <- data.frame(
      curve = "AD",
      x     = df$inflation_stabilization_sroutput,
      y     = df$inflation_stabilization_sroutput
    )
    
    # Combine into one dataframe:
    plot_df <- rbind(as_point, ad_point)
    
    # Set the fixed slope for both lines:
    slope_val <- 10
    
    # Calculate intercept for each curve: intercept = y - slope * x
    plot_df$slope     <- slope_val
    plot_df$intercept <- plot_df$y - plot_df$slope * plot_df$x
    
    # Create the plot:
    ggplot(plot_df, aes(x = x, y = y, color = curve)) +
      geom_point(size = 4) +
      # Draw each line with the same slope but its own intercept
      geom_abline(aes(slope = slope, intercept = intercept, color = curve), size = 1.2) +
      labs(
        title = "AS/AD Framework (One-Period)",
        x     = "Inflation Stabilization SR Output",
        y     = "Inflation Stabilization Inflation"
      ) +
      plot_theme
  })
  # v2
  

  
  #----------------------------------
  # DOWNLOAD HANDLERS version 1
  #----------------------------------
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("results_", Sys.Date(), ".csv")
    },
    content = function(file) {req(simulate_first_exp_calculations()) # 
      write.csv(simulate_first_exp_calculations(), file, row.names = FALSE)
    }
  )
  
  output$downloadPlots <- downloadHandler(
    filename = function() { paste0("plots_", Sys.Date(), ".zip") },
    content  = function(file) {
      # Grab all the plots out of the reactiveValues # 
      all_plots <- reactiveValuesToList(plot_objects) # for new loop plot code
      
      # save them as .png # v4
      tmp_dir   <- tempdir()
      file_paths <- c()
      for (nm in names(all_plots)) { # adjusting to fit loop plot code
        # nm is e.g. "plot_K", "plot_Y", ...
        plot_file <- file.path(tmp_dir, paste0(nm, ".png"))
        ggsave(plot_file, all_plots[[nm]], device = "png")
        file_paths <- c(file_paths, plot_file)
      }
      zip::zipr(zipfile = file, files = file_paths)
    }
  )
  
  # math formula module code
  simData <- reactive({
    req(input$simulate)  
    simulate_solow(
      num_periods = input$Simulation_Period + 1,
      a = input$a, o = input$o, b = input$b, v = input$v,
      m = input$m, n = input$n,
      fed_target_initial_inflation = input$fed_target_initial_inflation
    )
  })
  modFormulaDisplayServer(
    "myFormulas", 
    results_df_reactive   = simulate_first_exp_calculations) # <-- scenario
  # end of math formula module 
  
}

shinyApp(ui = ui, server = server)


# working on adding real interest rate and nominal interest rate


# v2 adding the AD/AS plot but definetly not correct. the code added is just a placeholder for as the plot code can be completely change. 