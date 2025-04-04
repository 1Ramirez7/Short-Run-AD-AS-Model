#----------------------------------
# SIMULATION CALCULATIONS
#----------------------------------

simulate_solow <- function(num_periods, a, o, b, v, m, n, r_bar, fed_target_initial_inflation, experiments_df = NULL, endomods_df    = NULL) {
  params <- data.frame(
    Period = 0:(num_periods - 1),
    a = rep(a, num_periods), # aggregate demand shock
    o = rep(o, num_periods), # Shock to inflation
    b = rep(b, num_periods), # param weighting the difference between the real interest rate and MPK
    v = rep(v, num_periods), # Demand Conditions
    m = rep(m, num_periods), # Governs how aggresive monetary policy responds to inflations
    n = rep(n, num_periods), 
    r_bar = rep(r_bar, num_periods), # Real Interest Rate long run?
    fed_target_initial_inflation = rep(fed_target_initial_inflation, num_periods),
    inflation_stabilization_sroutput  = rep(0.0, num_periods),
    inflation_stabilization_inflation = rep(fed_target_initial_inflation, num_periods),
    output_stabilization_sroutput     = rep(0.0, num_periods),
    output_stabilization_inflation    = rep(fed_target_initial_inflation, num_periods),
    taylor_rule_sroutput              = rep(0.0, num_periods),
    taylor_rule_inflation             = rep(fed_target_initial_inflation, num_periods),
    R = rep(2.0, num_periods)
  )
  
  # this does the quantitative experiments. 
  if (!is.null(experiments_df) && nrow(experiments_df) > 0) {
    for (exp in seq_len(nrow(experiments_df))) {
      start <- experiments_df$start_period[exp] + 1
      end   <- min(start + experiments_df$length[exp] - 1, num_periods)
      param_name <- experiments_df$param[exp]
      value  <- experiments_df$value[exp]
      params[start:end, param_name] <- value
    }
  }
  
  # adding endo code to apply endo changes at beg to params
  if (!is.null(endomods_df) && nrow(endomods_df) > 0) {
    for (endo in seq_len(nrow(endomods_df))) {
      start <- endomods_df$start_period[endo] + 1
      end   <- min(start + endomods_df$length[endo] - 1, num_periods)
      param_name <- endomods_df$param[endo]
      value      <- endomods_df$value[endo]
      params[start:end, param_name] <- value
    }
  } 

  
  # For loop over each period
  for (i in seq_len(num_periods)) {
    if (i == 1) { # i=1 corresponds to Period=0 in the df
      params$inflation_stabilization_inflation[i] <- fed_target_initial_inflation
     } else {
      
    params$inflation_stabilization_inflation[i] <-
      (params$inflation_stabilization_inflation[i-1] + params$o[i] + (params$v[i] * 
      params$b[i] * params$m[i]) * params$fed_target_initial_inflation[i] + params$a[i] * 
      params$v[i]) / (1 + params$v[i] * params$b[i] * params$m[i])
    }
    # endo mod for inflation_stabilization_inflation
    if (!is.null(endomods_df) &&
        any(endomods_df$param == "inflation_stabilization_inflation" &
            endomods_df$start_period + 1 == i)) {
      params$inflation_stabilization_inflation[i] <-
        endomods_df$value[endomods_df$param == "inflation_stabilization_inflation" &
                            endomods_df$start_period + 1 == i]
    }
    
    if (i == 1) {
      params$inflation_stabilization_sroutput[i] <- 0.0 
     } else {
      params$inflation_stabilization_sroutput[i] <- params$a[i] - (params$b[i] * 
          params$m[i]) / (1 + params$v[i] * params$b[i] * params$m[i]) *
          (params$inflation_stabilization_inflation[i] - params$fed_target_initial_inflation[i] +
          params$o[i] + params$a[i] * params$v[i])
    }
    if (!is.null(endomods_df) &&
        any(endomods_df$param == "inflation_stabilization_sroutput" &
            endomods_df$start_period + 1 == i)) {
      params$inflation_stabilization_sroutput[i] <- 
        endomods_df$value[endomods_df$param == "inflation_stabilization_sroutput" &
                            endomods_df$start_period + 1 == i]
    }
    
    if (i == 1) {
      params$output_stabilization_sroutput[i] < - 0.0
    } else { 
      params$output_stabilization_sroutput[i] <-  params$a[i] / (1 + params$b[i] * params$n[i])
    }
    if (!is.null(endomods_df) &&
        any(endomods_df$param == "output_stabilization_sroutput" &
            endomods_df$start_period + 1 == i)) {
      params$output_stabilization_sroutput[i] <- 
        endomods_df$value[endomods_df$param == "output_stabilization_sroutput" &
                            endomods_df$start_period + 1 == i]
    }
    
    if (i == 1) {
      params$output_stabilization_inflation[i] < - fed_target_initial_inflation
    } else { 
      params$output_stabilization_inflation[i] <- params$output_stabilization_inflation[i-1] + params$v[i] * (params$a[i]/(1 + params$b[i] * params$n[i])) + params$o[i]
    } # Brother Moncayo has an endo inside equation??
    if (!is.null(endomods_df) &&
        any(endomods_df$param == "output_stabilization_inflation" &
            endomods_df$start_period + 1 == i)) {
      params$output_stabilization_inflation[i] <- 
        endomods_df$value[endomods_df$param == "output_stabilization_inflation" &
                            endomods_df$start_period + 1 == i]
    }
    
    
    if (i == 1) { 
      params$taylor_rule_sroutput[i] <- 0.0
    } else {
      params$taylor_rule_sroutput[i] <- (params$b[i] * params$m[i]) / (1 + params$b[i] * params$n[i] + params$v[i] * params$b[i] * params$m[i]) * (params$a[i] / params$b[i] * params$m[i] - params$taylor_rule_inflation[i-1] + params$fed_target_initial_inflation[i] - params$o[i])
    }
    if (!is.null(endomods_df) &&
        any(endomods_df$param == "taylor_rule_sroutput" &
            endomods_df$start_period + 1 == i)) {
      params$taylor_rule_sroutput[i] <- 
        endomods_df$value[endomods_df$param == "taylor_rule_sroutput" &
                            endomods_df$start_period + 1 == i]
    }
    
    if (i == 1) { 
      params$taylor_rule_inflation[i] <- fed_target_initial_inflation
    } else {
      params$taylor_rule_inflation[i] <- params$taylor_rule_inflation[i-1] + params$o[i] + params$v[i] * params$taylor_rule_sroutput[i]
    }
    if (!is.null(endomods_df) &&
        any(endomods_df$param == "taylor_rule_inflation" &
            endomods_df$start_period + 1 == i)) {
      params$taylor_rule_inflation[i] <- 
        endomods_df$value[endomods_df$param == "taylor_rule_inflation" &
                            endomods_df$start_period + 1 == i]
    }
    
    params$R[i] <- params$r_bar[i] + params$m[i] * (params$taylor_rule_inflation[i]  - params$fed_target_initial_inflation[i]) + params$taylor_rule_inflation[i]
    }

  params
}
