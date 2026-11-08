library(shiny)
library(readxl)
library(lpSolve)

# UI Function for Aggregate Planning Linear Programming (LP) Model
lp_model_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 4,
        
        # 1. File Upload (.xlsx, .xls, .csv)
        h4("1. Data Upload & Columns"),
        fileInput(ns("file"), "Upload an Excel or CSV file (.xlsx, .xls, .csv)",
                  accept = c(".xlsx", ".xls", ".csv")),
        
        selectInput(ns("period_col"), "Period names column", choices = NULL),
        selectInput(ns("demand_col"), "Demand forecast column", choices = NULL),
        selectInput(ns("days_col"), "Working days column", choices = NULL),
        
        textInput(ns("custom_periods"), 
                  "Edit period names (comma-separated)", 
                  value = ""),
        
        hr(),
        
        # 2. LP Model Options (Overtime, Idle Time, Subcontracting)
        h4("2. LP Model Options"),
        checkboxInput(ns("use_overtime"), "Allow Overtime (O_t)", value = TRUE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("use_overtime")),
          numericInput(ns("max_ot_per_worker_day"), 
                       "Max overtime units per worker per day (0 = Unlimited)", 
                       value = 0, min = 0, step = 0.5)
        ),
        
        checkboxInput(ns("use_idle"), "Allow Idle Time / Undertime (P_t <= K * Days * W_t)", value = TRUE),
        
        checkboxInput(ns("use_subcontract"), "Allow Subcontracting (S_t)", value = TRUE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("use_subcontract")),
          numericInput(ns("max_sub_per_period"), 
                       "Max subcontracting units per period (0 = Unlimited)", 
                       value = 0, min = 0, step = 50)
        ),
        
        hr(),
        
        # 3. Productivity (K) Settings
        h4("3. Productivity (K)"),
        radioButtons(ns("k_method"), "How should productivity K be determined?",
                     choices = c("Enter K directly (units / worker / day)" = "direct",
                                 "Compute K from past data" = "past")),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'direct'", ns("k_method")),
          numericInput(ns("k_direct"), "Productivity K (units per worker per day)", 
                       value = 4, min = 0.0001, step = 0.5)
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'past'", ns("k_method")),
          numericInput(ns("past_units"), "Past total units produced", value = 1600, min = 1),
          numericInput(ns("past_workers"), "Past number of workers", value = 20, min = 1),
          numericInput(ns("past_days"), "Past working days", value = 20, min = 1)
        ),
        
        hr(),
        
        # 4. Plant Parameters
        h4("4. Plant Parameters"),
        fluidRow(
          column(4, numericInput(ns("w0"), "Starting Workforce", value = 15, min = 0, step = 1)),
          column(4, numericInput(ns("i0"), "Starting Inventory", value = 100, min = 0, step = 10)),
          column(4, numericInput(ns("it"), "Ending Inventory", value = 50, min = 0, step = 10))
        ),
        
        hr(),
        
        # 5. Cost Parameters
        h4("5. Cost Parameters ($)"),
        fluidRow(
          column(6, numericInput(ns("c_hire"), "Hiring cost (/worker)", value = 300, min = 0)),
          column(6, numericInput(ns("c_fire"), "Firing cost (/worker)", value = 500, min = 0))
        ),
        fluidRow(
          column(6, numericInput(ns("c_hold"), "Holding cost (/unit/period)", value = 2, min = 0)),
          column(6, numericInput(ns("c_reg"), "Regular time cost", value = 80, min = 0))
        ),
        selectInput(ns("reg_cost_basis"), "Regular time cost basis:",
                    choices = c("Per worker per day (W * Days * Cost)" = "per_worker_day",
                                "Per worker per period (W * Cost)" = "per_worker_period",
                                "Per unit produced (P * Cost)" = "per_unit")),
        fluidRow(
          column(6, numericInput(ns("c_ot"), "Overtime cost (/unit)", value = 25, min = 0)),
          column(6, numericInput(ns("c_sub"), "Subcontracting cost (/unit)", value = 35, min = 0))
        ),
        numericInput(ns("c_idle"), "Idle time penalty cost (/unused unit, optional)", value = 0, min = 0)
      ),
      
      mainPanel(
        width = 8,
        h4(textOutput(ns("lp_title_text"))),
        p(strong(textOutput(ns("lp_info_text")))),
        
        h4("Cumulative Production vs. Cumulative Net Demand"),
        plotOutput(ns("cum_plot"), height = "380px"),
        
        hr(),
        
        h4("Cost Comparison: Continuous LP vs. Whole Workers (Rounded-Up W)"),
        fluidRow(
          column(6,
                 h5(strong("Original Continuous LP Cost Breakdown")),
                 tableOutput(ns("cost_table_frac"))),
          column(6,
                 h5(strong("Whole Workers (Rounded-Up) Cost Breakdown")),
                 tableOutput(ns("cost_table_whole")))
        ),
        
        hr(),
        
        h4("Aggregate Plan Tables (Original LP vs. Whole Workers Side-by-Side)"),
        fluidRow(
          column(6,
                 h5(strong("Original Continuous LP Plan")),
                 div(style = "overflow-x: auto;", tableOutput(ns("plan_table_frac")))),
          column(6,
                 h5(strong("Whole Workers (Rounded-Up Workforce) Plan")),
                 div(style = "overflow-x: auto;", tableOutput(ns("plan_table_whole"))))
        )
      )
    )
  )
}

# Server Function for Aggregate Planning LP Model
lp_model_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Read uploaded Excel or CSV file
    uploaded_data <- reactive({
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      
      if (ext %in% c("xlsx", "xls")) {
        as.data.frame(read_excel(input$file$datapath))
      } else if (ext == "csv") {
        read.csv(input$file$datapath, check.names = FALSE)
      } else {
        validate("Please upload a valid .xlsx, .xls, or .csv file.")
      }
    })
    
    # Populate column selectors automatically upon upload
    observeEvent(uploaded_data(), {
      df <- uploaded_data()
      cols <- names(df)
      
      p_default <- cols[grep("period|month|time|date", tolower(cols))[1]]
      d_default <- cols[grep("demand|forecast|req", tolower(cols))[1]]
      w_default <- cols[grep("day|work", tolower(cols))[1]]
      
      if (is.na(p_default) && length(cols) >= 1) p_default <- cols[1]
      if (is.na(d_default) && length(cols) >= 2) d_default <- cols[2]
      if (is.na(w_default) && length(cols) >= 3) w_default <- cols[3]
      
      updateSelectInput(session, "period_col", choices = cols, selected = p_default)
      updateSelectInput(session, "demand_col", choices = cols, selected = d_default)
      updateSelectInput(session, "days_col", choices = cols, selected = w_default)
    })
    
    # Update editable period names text box
    observeEvent(list(uploaded_data(), input$period_col), {
      req(uploaded_data(), input$period_col)
      df <- uploaded_data()
      req(input$period_col %in% names(df))
      raw_p <- as.character(df[[input$period_col]])
      raw_p <- raw_p[!is.na(raw_p) & nzchar(trimws(raw_p))]
      updateTextInput(session, "custom_periods", value = paste(raw_p, collapse = ", "))
    })
    
    # Extract cleaned data frame (Period, Demand, Working Days)
    plan_inputs <- reactive({
      req(uploaded_data(), input$demand_col, input$days_col)
      df <- uploaded_data()
      req(input$demand_col %in% names(df), input$days_col %in% names(df))
      
      d <- as.numeric(df[[input$demand_col]])
      days <- as.numeric(df[[input$days_col]])
      valid_idx <- which(!is.na(d) & !is.na(days) & days > 0)
      validate(need(length(valid_idx) >= 2, "Please select valid numeric Demand and Working Days columns."))
      
      d <- d[valid_idx]
      days <- days[valid_idx]
      n_per <- length(d)
      
      p_names <- character(0)
      if (nzchar(trimws(input$custom_periods))) {
        p_names <- trimws(unlist(strsplit(input$custom_periods, ",")))
      }
      if (length(p_names) < n_per) {
        if (!is.null(input$period_col) && input$period_col %in% names(df)) {
          p_names <- as.character(df[[input$period_col]][valid_idx])
        } else {
          p_names <- paste0("P", seq_len(n_per))
        }
      } else {
        p_names <- p_names[seq_len(n_per)]
      }
      
      list(periods = p_names, demand = d, days = days, n = n_per)
    })
    
    # Calculate Productivity K
    productivity_k <- reactive({
      if (input$k_method == "direct") {
        req(!is.na(input$k_direct))
        validate(need(input$k_direct > 0, "Productivity K must be greater than 0."))
        as.numeric(input$k_direct)
      } else {
        req(!is.na(input$past_units), !is.na(input$past_workers), !is.na(input$past_days))
        validate(need(input$past_units > 0 && input$past_workers > 0 && input$past_days > 0,
                      "Past units, workers, and working days must all be positive."))
        as.numeric(input$past_units) / (as.numeric(input$past_workers) * as.numeric(input$past_days))
      }
    })
    
    # Core LP Solver Function
    # Decision variables per period t (1..T), 8*T variables total:
    # 1..T     : W_t (Workforce in period t)
    # T+1..2T  : H_t (Hired workers in period t)
    # 2T+1..3T : F_t (Fired workers in period t)
    # 3T+1..4T : P_t (Regular time production in period t)
    # 4T+1..5T : U_t (Idle time / Undertime in units in period t)
    # 5T+1..6T : O_t (Overtime production in units in period t)
    # 6T+1..7T : S_t (Subcontracted units in period t)
    # 7T+1..8T : I_t (Ending inventory in period t)
    solve_aggregate_lp <- function(d, days, p_names, K, w0, i0, it_target, fixed_W = NULL) {
      T_per <- length(d)
      n_vars <- 8 * T_per
      
      idx_W <- function(t) t
      idx_H <- function(t) T_per + t
      idx_F <- function(t) 2 * T_per + t
      idx_P <- function(t) 3 * T_per + t
      idx_U <- function(t) 4 * T_per + t
      idx_O <- function(t) 5 * T_per + t
      idx_S <- function(t) 6 * T_per + t
      idx_I <- function(t) 7 * T_per + t
      
      # Objective coefficients
      obj <- numeric(n_vars)
      for (t in seq_len(T_per)) {
        if (input$reg_cost_basis == "per_worker_day") {
          obj[idx_W(t)] <- days[t] * input$c_reg
        } else if (input$reg_cost_basis == "per_worker_period") {
          obj[idx_W(t)] <- input$c_reg
        } else {
          obj[idx_P(t)] <- input$c_reg
        }
        obj[idx_H(t)] <- input$c_hire
        obj[idx_F(t)] <- input$c_fire
        obj[idx_U(t)] <- input$c_idle
        obj[idx_O(t)] <- input$c_ot
        obj[idx_S(t)] <- input$c_sub
        obj[idx_I(t)] <- input$c_hold
      }
      
      const_list <- list()
      dir_vec <- character(0)
      rhs_vec <- numeric(0)
      
      add_constraint <- function(row_vals, dir_str, rhs_val) {
        const_list[[length(const_list) + 1]] <<- row_vals
        dir_vec <<- c(dir_vec, dir_str)
        rhs_vec <<- c(rhs_vec, rhs_val)
      }
      
      # 1. Workforce balance: W_t - W_{t-1} - H_t + F_t = 0
      for (t in seq_len(T_per)) {
        row <- numeric(n_vars)
        row[idx_W(t)] <- 1
        row[idx_H(t)] <- -1
        row[idx_F(t)] <- 1
        if (t == 1) {
          add_constraint(row, "=", w0)
        } else {
          row[idx_W(t - 1)] <- -1
          add_constraint(row, "=", 0)
        }
      }
      
      # 2. Regular Production Capacity & Idle Time: P_t + U_t - K * days_t * W_t = 0
      for (t in seq_len(T_per)) {
        row <- numeric(n_vars)
        row[idx_P(t)] <- 1
        row[idx_U(t)] <- 1
        row[idx_W(t)] <- -K * days[t]
        add_constraint(row, "=", 0)
      }
      
      # 3. Inventory Balance: I_{t-1} + P_t + O_t + S_t - I_t = D_t
      for (t in seq_len(T_per)) {
        row <- numeric(n_vars)
        row[idx_P(t)] <- 1
        row[idx_O(t)] <- 1
        row[idx_S(t)] <- 1
        row[idx_I(t)] <- -1
        if (t == 1) {
          add_constraint(row, "=", d[1] - i0)
        } else {
          row[idx_I(t - 1)] <- 1
          add_constraint(row, "=", d[t])
        }
      }
      
      # 4. Ending Inventory Target at t = T: I_T >= it_target (or = it_target if idle/options allow)
      row_it <- numeric(n_vars)
      row_it[idx_I(T_per)] <- 1
      if (is.null(fixed_W)) {
        add_constraint(row_it, "=", it_target)
      } else {
        add_constraint(row_it, ">=", it_target)
      }
      
      # 5. Option-based constraints (Overtime, Idle Time, Subcontracting)
      for (t in seq_len(T_per)) {
        # Idle Time toggle
        if (!input$use_idle && is.null(fixed_W)) {
          row <- numeric(n_vars)
          row[idx_U(t)] <- 1
          add_constraint(row, "=", 0)
        }
        
        # Overtime toggle & capacity
        if (!input$use_overtime) {
          row <- numeric(n_vars)
          row[idx_O(t)] <- 1
          add_constraint(row, "=", 0)
        } else if (!is.na(input$max_ot_per_worker_day) && input$max_ot_per_worker_day > 0) {
          row <- numeric(n_vars)
          row[idx_O(t)] <- 1
          row[idx_W(t)] <- -input$max_ot_per_worker_day * days[t]
          add_constraint(row, "<=", 0)
        }
        
        # Subcontracting toggle & capacity
        if (!input$use_subcontract) {
          row <- numeric(n_vars)
          row[idx_S(t)] <- 1
          add_constraint(row, "=", 0)
        } else if (!is.na(input$max_sub_per_period) && input$max_sub_per_period > 0) {
          row <- numeric(n_vars)
          row[idx_S(t)] <- 1
          add_constraint(row, "<=", input$max_sub_per_period)
        }
      }
      
      # 6. If fixed_W is supplied (Whole Workers Rounded-Up Plan), fix W_t = fixed_W[t]
      if (!is.null(fixed_W)) {
        for (t in seq_len(T_per)) {
          row <- numeric(n_vars)
          row[idx_W(t)] <- 1
          add_constraint(row, "=", fixed_W[t])
          
          # If user disabled idle time, regular production equals full capacity of rounded-up workers
          if (!input$use_idle) {
            row_u <- numeric(n_vars)
            row_u[idx_U(t)] <- 1
            add_constraint(row_u, "=", 0)
          }
        }
      }
      
      const_mat <- do.call(rbind, const_list)
      sol <- lpSolve::lp("min", obj, const_mat, dir_vec, rhs_vec)
      
      # Fallback: if rounded-up fixed_W with disabled idle/options is infeasible, allow inventory surplus
      if (sol$status != 0 && !is.null(fixed_W)) {
        return(NULL)
      }
      if (sol$status != 0) {
        return(NULL)
      }
      
      x <- sol$solution
      W <- x[idx_W(seq_len(T_per))]
      H <- x[idx_H(seq_len(T_per))]
      Fi <- x[idx_F(seq_len(T_per))]
      P <- x[idx_P(seq_len(T_per))]
      U <- x[idx_U(seq_len(T_per))]
      O <- x[idx_O(seq_len(T_per))]
      S <- x[idx_S(seq_len(T_per))]
      I <- x[idx_I(seq_len(T_per))]
      
      # Itemized Costs
      cost_hire <- sum(H) * input$c_hire
      cost_fire <- sum(Fi) * input$c_fire
      cost_hold <- sum(I) * input$c_hold
      cost_reg  <- switch(input$reg_cost_basis,
                          "per_worker_day"    = sum(W * days) * input$c_reg,
                          "per_worker_period" = sum(W) * input$c_reg,
                          "per_unit"          = sum(P) * input$c_reg)
      cost_ot   <- sum(O) * input$c_ot
      cost_sub  <- sum(S) * input$c_sub
      cost_idle <- sum(U) * input$c_idle
      total_cost <- cost_hire + cost_fire + cost_hold + cost_reg + cost_ot + cost_sub + cost_idle
      
      plan_df <- data.frame(
        Period = p_names,
        Days = days,
        Demand = d,
        Workers = round(W, 2),
        Hired = round(H, 2),
        Fired = round(Fi, 2),
        `Regular Prod` = round(P, 2),
        `Idle (Units)` = round(U, 2),
        Overtime = round(O, 2),
        Subcontract = round(S, 2),
        Inventory = round(I, 2),
        check.names = FALSE
      )
      
      cost_df <- data.frame(
        `Cost Item` = c("Hiring Cost", "Firing Cost", "Inventory Holding Cost",
                        "Regular Time Cost", "Overtime Cost", "Subcontracting Cost",
                        "Idle Time Cost", "TOTAL COST"),
        `Total Units / Workers` = c(round(sum(H), 2),
                                    round(sum(Fi), 2),
                                    round(sum(I), 2),
                                    round(sum(W), 2),
                                    round(sum(O), 2),
                                    round(sum(S), 2),
                                    round(sum(U), 2),
                                    NA),
        `Cost ($)` = round(c(cost_hire, cost_fire, cost_hold, cost_reg, 
                             cost_ot, cost_sub, cost_idle, total_cost), 2),
        check.names = FALSE
      )
      
      list(
        plan_df = plan_df,
        cost_df = cost_df,
        total_cost = total_cost,
        W = W,
        total_supply = P + O + S
      )
    }
    
    # Reactive Computation of Continuous LP and Whole-Worker Rounded Plan
    results <- reactive({
      inp <- plan_inputs()
      K <- productivity_k()
      req(!is.na(input$w0), !is.na(input$i0), !is.na(input$it))
      
      d <- inp$demand
      days <- inp$days
      p_names <- inp$periods
      n <- inp$n
      w0 <- input$w0
      i0 <- input$i0
      it_target <- input$it
      
      # 1. Solve Continuous LP
      sol_frac <- solve_aggregate_lp(d, days, p_names, K, w0, i0, it_target, fixed_W = NULL)
      validate(need(!is.null(sol_frac), 
                    "LP model is infeasible with the current options. Try enabling Overtime, Subcontracting, or Idle Time."))
      
      # 2. Round up fractional workers: W_whole = ceiling(W_frac)
      W_whole <- ceiling(sol_frac$W - 1e-7)
      sol_whole <- solve_aggregate_lp(d, days, p_names, K, w0, i0, it_target, fixed_W = W_whole)
      
      # Fallback deterministic evaluation if LP with fixed_W needs direct inventory accumulation
      if (is.null(sol_whole)) {
        H <- numeric(n); Fi <- numeric(n); P <- W_whole * K * days
        inv <- numeric(n); prev_w <- w0; prev_i <- i0
        for (t in seq_len(n)) {
          diff_w <- W_whole[t] - prev_w
          H[t] <- max(0, diff_w); Fi[t] <- max(0, -diff_w)
          inv[t] <- prev_i + P[t] - d[t]
          prev_w <- W_whole[t]; prev_i <- inv[t]
        }
        cost_hire <- sum(H) * input$c_hire
        cost_fire <- sum(Fi) * input$c_fire
        cost_hold <- sum(pmax(0, inv)) * input$c_hold
        cost_reg  <- switch(input$reg_cost_basis,
                            "per_worker_day"    = sum(W_whole * days) * input$c_reg,
                            "per_worker_period" = sum(W_whole) * input$c_reg,
                            "per_unit"          = sum(P) * input$c_reg)
        total_c <- cost_hire + cost_fire + cost_hold + cost_reg
        
        sol_whole <- list(
          plan_df = data.frame(
            Period = p_names, Days = days, Demand = d, Workers = W_whole,
            Hired = H, Fired = Fi, `Regular Prod` = round(P, 2),
            `Idle (Units)` = 0, Overtime = 0, Subcontract = 0,
            Inventory = round(inv, 2), check.names = FALSE
          ),
          cost_df = data.frame(
            `Cost Item` = c("Hiring Cost", "Firing Cost", "Inventory Holding Cost",
                            "Regular Time Cost", "Overtime Cost", "Subcontracting Cost",
                            "Idle Time Cost", "TOTAL COST"),
            `Total Units / Workers` = c(sum(H), sum(Fi), round(sum(pmax(0, inv)), 2),
                                        sum(W_whole), 0, 0, 0, NA),
            `Cost ($)` = round(c(cost_hire, cost_fire, cost_hold, cost_reg, 0, 0, 0, total_c), 2),
            check.names = FALSE
          ),
          total_cost = total_c,
          W = W_whole,
          total_supply = P
        )
      }
      
      # Cumulative Net Demand
      net_d <- d
      net_d[1] <- d[1] - i0
      net_d[n] <- net_d[n] + it_target
      cum_net_d <- cumsum(net_d)
      
      list(
        periods = p_names,
        n = n,
        K = K,
        cum_net_d = cum_net_d,
        cum_prod_frac = cumsum(sol_frac$total_supply),
        cum_prod_whole = cumsum(sol_whole$total_supply),
        frac = sol_frac,
        whole = sol_whole
      )
    })
    
    # Headline summary
    output$lp_title_text <- renderText({
      req(results())
      res <- results()
      paste0("Linear Programming (LP) Aggregate Plan — Optimal LP Cost: $",
             format(round(res$frac$total_cost, 2), big.mark = ","),
             " | Whole Workers (Rounded-Up) Cost: $",
             format(round(res$whole$total_cost, 2), big.mark = ","))
    })
    
    # Active options & productivity info
    output$lp_info_text <- renderText({
      req(results())
      res <- results()
      opts <- c(
        if (input$use_overtime) "Overtime: ON" else "Overtime: OFF",
        if (input$use_idle) "Idle Time: ON" else "Idle Time: OFF",
        if (input$use_subcontract) "Subcontracting: ON" else "Subcontracting: OFF"
      )
      paste0("Productivity K = ", round(res$K, 4), " units/worker/day | Active LP Options: [",
             paste(opts, collapse = ", "), "]")
    })
    
    # Cumulative Production vs. Cumulative Net Demand Plot
    output$cum_plot <- renderPlot({
      req(results())
      res <- results()
      n <- res$n
      x_idx <- seq_len(n)
      
      y_range <- range(c(res$cum_net_d, res$cum_prod_frac, res$cum_prod_whole), na.rm = TRUE)
      
      plot(x_idx, res$cum_net_d, type = "b", pch = 19, col = "black", lwd = 2,
           xaxt = "n", xlab = "Period", ylab = "Cumulative Units",
           main = "Cumulative Production vs. Cumulative Net Demand (LP Model)",
           ylim = y_range)
      axis(1, at = x_idx, labels = res$periods)
      grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
      
      lines(x_idx, res$cum_prod_frac, type = "b", pch = 17, col = "#1f77b4", lwd = 2, lty = 2)
      lines(x_idx, res$cum_prod_whole, type = "b", pch = 15, col = "#d62728", lwd = 2, lty = 1)
      
      legend("topleft",
             legend = c("Cumulative Net Demand", 
                        "Cumulative Supply (Optimal Continuous LP)", 
                        "Cumulative Supply (Whole Workers LP)"),
             col = c("black", "#1f77b4", "#d62728"),
             pch = c(19, 17, 15),
             lty = c(1, 2, 1),
             lwd = 2,
             bg = "white")
    })
    
    # Cost Tables
    output$cost_table_frac <- renderTable({
      req(results())
      results()$frac$cost_df
    }, na = "-")
    
    output$cost_table_whole <- renderTable({
      req(results())
      results()$whole$cost_df
    }, na = "-")
    
    # Plan Tables
    output$plan_table_frac <- renderTable({
      req(results())
      results()$frac$plan_df
    })
    
    output$plan_table_whole <- renderTable({
      req(results())
      results()$whole$plan_df
    })
  })
}