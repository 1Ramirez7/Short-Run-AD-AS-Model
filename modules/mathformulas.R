#========================================================
# MODULE: Display the AD-AS MATH Formulas for a selected period
#========================================================
# Using NS to give unique IDs to avoid conflicts
# need to start using NS more often as it will be hard to find a conflicting variable and or object name


# UI function
modFormulaDisplayUI <- function(id) {
  ns <- NS(id)
  tagList(
    # select period for calculations
    numericInput(ns("selected_period"), "Select Period", value = 0, min = 0, step = 1),
    
    # UI outputs for each formula to display
    uiOutput(ns("inflation_stabilization_inflation_ui")),
    uiOutput(ns("inflation_stabilization_sroutput_ui")),
    uiOutput(ns("output_stabilization_inflation_ui")),
    uiOutput(ns("output_stabilization_sroutput_ui")),
    uiOutput(ns("taylor_rule_inflation_ui")),
    uiOutput(ns("taylor_rule_sroutput_ui")),
    uiOutput(ns("nominal_interest_rate_ui"))
  )
}

# Server function
modFormulaDisplayServer <- function(id, results_df_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    getPeriodData <- function() { # Helper: get user-selected period from the simulation results
      df <- results_df_reactive()
      req(df)
      p <- input$selected_period + 1  # period 0 is row index=1
      validate(need(p >= 1 && p <= nrow(df),
          paste0("Selected period is out of range! Please select 0 through ", nrow(df) - 1, ".")))
      df[p, , drop = FALSE]}
    
    #--------------------------------------
    # 1) inflation_stabilization_inflation
    #    π_t = [ π_{t-1} + o + v·b·m·(pi_bar) + a·v ] / [1 + v·b·m]
    #--------------------------------------
    output$inflation_stabilization_inflation_ui <- renderUI({
      df_row <- getPeriodData()
    
      pi_bar  <- df_row$fed_target_initial_inflation
      pi_t    <- df_row$inflation_stabilization_inflation
      o_val   <- df_row$o
      v_val   <- df_row$v
      b_val   <- df_row$b
      m_val   <- df_row$m
      a_val   <- df_row$a
      
      withMathJax(
        tags$div(
          h4("Inflation Stabilization (\\(\\pi_t\\))"),
          # Generic formula
          "$$\\pi_t = \\frac{\\pi_{t-1} + \\bar{o} + \\bar{v}\\,b\\,m\\,\\bar{\\pi} + a\\,\\bar{v}}{1 + \\bar{v}\\,b\\,m}$$",
          
          # Numeric substitution:
          paste0(
            "$$\\pi_t = \\frac{",
            "\\pi_{t-1} + ",
            round(o_val, 3), " + ",
            round(v_val, 3), "\\cdot",
            round(b_val, 3), "\\cdot",
            round(m_val, 3), "\\cdot",
            round(pi_bar, 3), " + ",
            round(a_val, 3), "\\cdot",
            round(v_val, 3),
            "}{1 + ",
            round(v_val, 3), "\\cdot",
            round(b_val, 3), "\\cdot",
            round(m_val, 3),
            "}$$"
          ),
          paste0("Result: \\(\\pi_t = ", round(pi_t, 3), "\\)")
        )
      )
    })
    
    #--------------------------------------
    # 2) inflation_stabilization_sroutput
    #    Y_t = a - [ (b·m)/(1 + v·b·m ) ] · [ π_t - (pi_bar) + o + a·v ]
    #--------------------------------------
    output$inflation_stabilization_sroutput_ui <- renderUI({
      df_row <- getPeriodData()
      pi_bar <- df_row$fed_target_initial_inflation
      
      sr_out <- df_row$inflation_stabilization_sroutput
      pi_t   <- df_row$inflation_stabilization_inflation
      o_val  <- df_row$o
      v_val  <- df_row$v
      b_val  <- df_row$b
      m_val  <- df_row$m
      a_val  <- df_row$a
      
      withMathJax(
        tags$div(
          h4("Inflation Stabilization Output (\\(\\tilde{Y}_t\\))"),
          # Generic formula
          "$$\\tilde{Y}_t = a - \\frac{b\\,m}{1 + \\bar{v}\\,b\\,m}\\,\\bigl(\\pi_t - \\bar{\\pi} + \\bar{o} + a\\,\\bar{v}\\bigr)$$",
          
          # Numeric substitution:
          paste0(
            "$$\\tilde{Y}_t = ",
            round(a_val, 3),
            " - \\frac{",
            round(b_val, 3), "\\cdot",
            round(m_val, 3),
            "}{1 + ",
            round(v_val, 3), "\\cdot",
            round(b_val, 3), "\\cdot",
            round(m_val, 3),
            "} \\Bigl(",
            round(pi_t, 3), " - ",
            round(pi_bar, 3), " + ",
            round(o_val, 3), " + ",
            round(a_val, 3), "\\cdot",
            round(v_val, 3),
            "\\Bigr) $$"
          ),
          paste0("Result: \\(\\tilde{Y}_t = ", round(sr_out, 3), "\\)")
        ))
    })
    
    #--------------------------------------
    # 3) output_stabilization_inflation
    #    π_t = π_{t-1} + v·(a/(1 + bn)) + o
    #--------------------------------------
    output$output_stabilization_inflation_ui <- renderUI({
      df <- results_df_reactive()
      req(df)
      
      p <- input$selected_period + 1
      validate(
        need(p >= 1 && p <= nrow(df),
             paste0("Selected period (", p - 1, ") is out of range!"))
      )
      df_row <- df[p, , drop = FALSE]
      
      pi_val <- df_row$output_stabilization_inflation
      o_val  <- df_row$o
      v_val  <- df_row$v
      a_val  <- df_row$a
      b_val  <- df_row$b
      n_val  <- df_row$n
      
      sr_calc <- a_val / (1 + b_val * n_val)
      
      # previous inflation for numeric substitution
      pi_prev <- if (p > 1) {
        df[p - 1, "output_stabilization_inflation"]
      } else {
        pi_val  # if first period, current = initial
      }
      
      withMathJax(
        tags$div(
          h4("Output Stabilization: Inflation (\\(\\pi_t\\))"),
          
          "$$\\pi_t = \\pi_{t-1} + \\bar{v}\\left( \\frac{a}{1 + b n} \\right) + \\bar{o}$$",
          
          if (p == 1) {
            paste0(
              "First period (Period 0): \\(\\pi_0 = ",
              round(pi_val, 3), "\\).")
          } else {
            paste0(
              "$$\\pi_t = ",
              round(pi_prev, 3),
              " + ",
              round(v_val, 3),
              "\\cdot\\left(\\frac{",
              round(a_val, 3),
              "}{1 + ",
              round(b_val, 3), "\\cdot", round(n_val, 3),
              "}\\right) + ",
              round(o_val, 3),
              " = ",
              round(pi_val, 3),
              "$$"
            )}))
    })
    
    #--------------------------------------
    # 4) output_stabilization_sroutput
    #    Y_t = a / (1 + b·n)
    #--------------------------------------
    output$output_stabilization_sroutput_ui <- renderUI({
      df_row <- getPeriodData()
      sr_out <- df_row$output_stabilization_sroutput
      a_val  <- df_row$a
      b_val  <- df_row$b
      n_val  <- df_row$n
      
      withMathJax(
        tags$div(
          h4("Output Stabilization: Short-Run Output (\\(\\tilde{Y}_t\\))"),
          "$$\\tilde{Y}_t = \\frac{a}{1 + b\\,n}$$",
          paste0(
            "$$\\tilde{Y}_t = \\frac{",
            round(a_val, 3),
            "}{1 + ",
            round(b_val, 3), "\\cdot",
            round(n_val, 3),
            "} = ",
            round(sr_out, 3),
            "$$"
          )))
    })
    
    #--------------------------------------
    # Taylor Rule: Inflation
    #  π_t = π_{t-1} + o + v·Y_t
    #--------------------------------------
    output$taylor_rule_inflation_ui <- renderUI({
      df <- results_df_reactive()
      req(df)
      
      p <- input$selected_period + 1
      validate(
        need(p >= 1 && p <= nrow(df),
             paste0("Selected period (", p - 1, ") is out of range!"))
      )
      df_row <- df[p, , drop = FALSE]
      
      pi_val <- df_row$taylor_rule_inflation
      o_val  <- df_row$o
      v_val  <- df_row$v
      sr_out <- df_row$taylor_rule_sroutput
      
      # For the first period set inflation = fed_target_initial_inflation
      # I need to edit this to just be a generic message when its period 1 or anytime endo modifies it. Endo will overwrite calculations so overwriting the answer will not math out
      # But I still havent implemented endo into the math module
      pi_prev <- if (p > 1) {
        df[p - 1, "taylor_rule_inflation"]
      } else {
        pi_val
      }
      
      withMathJax(
        tags$div(
          h4("Taylor Rule: Inflation (\\(\\pi_{t}^{TR}\\))"),
          "$$\\pi_{t}^{TR} = \\pi_{t-1}^{TR} + \\bar{o} + \\bar{v}\\,\\tilde{Y}_t$$",
          
          if (p == 1) {
            paste0(
              "Period 0: \\(\\pi_{0}^{TR}\\) is set to the initial Fed target. ",
              "Thus \\(\\pi_{0}^{TR} = ", round(pi_val, 3), "\\)."
            )
          } else {
            paste0(
              "$$\\pi_{t}^{TR} = ",
              round(pi_prev, 3),
              " + ",
              round(o_val, 3),
              " + ",
              round(v_val, 3),
              "\\cdot",
              round(sr_out, 3),
              " = ",
              round(pi_val, 3),
              "$$"
            )}))
    })
    
    #----------------------------------------------------
    # Taylor Rule: Short-Run Output (taylor_rule_sroutput_ui)
    #  Code uses: (b*m)/(1 + b*n + v*b*m) * [ (a)/(bm) - pi_{t-1} + pi_bar - o ]
    #----------------------------------------------------
    output$taylor_rule_sroutput_ui <- renderUI({
      df <- results_df_reactive()
      req(df)
      
      p <- input$selected_period + 1
      validate(
        need(p >= 1 && p <= nrow(df),
             paste0("Selected period (", p - 1, ") is out of range!"))
      )
      df_row <- df[p, , drop = FALSE]
      
      sr_out <- df_row$taylor_rule_sroutput
      b_val  <- df_row$b
      m_val  <- df_row$m
      n_val  <- df_row$n
      v_val  <- df_row$v
      a_val  <- df_row$a
      o_val  <- df_row$o
      pi_bar <- df_row$fed_target_initial_inflation
      
      # previous period's inflation (π_{t-1})
      pi_prev <- if (p > 1) {
        df[p - 1, "taylor_rule_inflation"]
      } else {
        NA
      }
      
      withMathJax(
        tags$div(
          h4("Taylor Rule: Short-Run Output (\\(\\tilde{Y}_t\\))"),
          
          # math formula
          "$$\\tilde{Y}_t = \\frac{b\\,m}{1 + b\\,n + \\bar{v}\\,b\\,m}\\Bigl(\\frac{a}{b\\,m} - \\pi_{t-1}^{TR} + \\bar{\\pi} - \\bar{o}\\Bigr)$$",
          
          if (p == 1) {
            paste0(
              "Period 0: short-run output is set to 0. ",
              "Thus \\(\\tilde{Y}_0 = 0.0\\)."
            )
          } else {
            paste0(
              "$$\\tilde{Y}_t = \\frac{",
              round(b_val, 3), "\\cdot", round(m_val, 3),
              "}{1 + ", round(b_val, 3), "\\cdot", round(n_val, 3),
              " + ", round(v_val, 3), "\\cdot", round(b_val, 3),
              "\\cdot", round(m_val, 3), "}\\times\\Bigl(\\frac{",
              round(a_val, 3), "}{", round(b_val, 3), "\\cdot", round(m_val, 3),
              "} - ", round(pi_prev, 3),
              " + ", round(pi_bar, 3),
              " - ", round(o_val, 3), "\\Bigr)",
              " = ",
              round(sr_out, 3),
              "$$"
            )}))})
    #--------------------------------------
    # Nominal Interest Rate (R)
    #    R = r_bar + m*(taylor_rule_inflation - fed_target_initial_inflation) + taylor_rule_inflation
    #--------------------------------------
    output$nominal_interest_rate_ui <- renderUI({
      df_row <- getPeriodData()

      r_bar_val <- df_row$r_bar
      m_val <- df_row$m
      taylor_rule_inflation_val <- df_row$taylor_rule_inflation
      fed_target_initial_inflation_val <- df_row$fed_target_initial_inflation
      R_val <- df_row$R 
      
      withMathJax(
        tags$div(
          h4("Nominal Interest Rate (\\(R\\))"),
          # Display the generic formula using MathJax:
          "$$R = \\bar{r} + m \\cdot \\left(\\pi_{t}^{TR} - \\bar{\\pi}\\right) + \\pi_{t}^{TR}$$",
          
          # Display the formula with numeric substitution:
          paste0(
            "$$R = ",
            round(r_bar_val, 3), " + ",
            round(m_val, 3), " \\cdot \\left(",
            round(taylor_rule_inflation_val, 3), " - ",
            round(fed_target_initial_inflation_val, 3), "\\right) + ",
            round(taylor_rule_inflation_val, 3),
            "$$"
          ),
          paste0("Result: \\(R = ", round(R_val, 3), "\\)")
      ))})
  })
}
