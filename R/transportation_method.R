library(shiny)
library(readxl)

# UI Function for Aggregate Planning - Transportation Method
transportation_method_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("1. Data Upload & Period Names"),
        fileInput(ns("file"), "Upload Excel or CSV file (.xlsx, .xls, .csv)",
                  accept = c(".xlsx", ".xls", ".csv")),
        
        selectInput(ns("period_col"), "Period names column", choices = NULL),
        selectInput(ns("demand_col"), "Demand forecasts column", choices = NULL),
        selectInput(ns("days_col"), "Working days column", choices = NULL),
        
        textInput(ns("edit_periods"), "Edit period names (comma-separated):", value = ""),
        
        hr(),
        h4("2. Productivity (K)"),
        radioButtons(ns("k_method"), "How should productivity K be determined?",
                     choices = c("Enter K directly (units / worker / day)" = "direct",
                                 "Compute K from past data" = "computed")),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'direct'", ns("k_method")),
          numericInput(ns("k_direct"), "K (units per worker per day)", value = 4, min = 0.001, step = 0.5)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'computed'", ns("k_method")),
          numericInput(ns("past_units"), "Past units produced", value = 4000, min = 1),
          numericInput(ns("past_workers"), "Past number of workers", value = 50, min = 1),
          numericInput(ns("past_days"), "Past working days", value = 20, min = 1),
          verbatimTextOutput(ns("k_computed_display"))
        ),
        
        hr(),
        h4("3. Plant & Workforce Data"),
        fluidRow(
          column(4, numericInput(ns("w0"), "Starting workforce (W0)", value = 15, min = 0)),
          column(4, numericInput(ns("i0"), "Starting inv. (I0)", value = 200, min = 0)),
          column(4, numericInput(ns("it"), "Ending inv. (IT)", value = 100, min = 0))
        ),
        
        radioButtons(ns("workforce_mode"), "Workforce / Regular-Time Capacity Setup:",
                     choices = c("Compute constant workforce from net demand (often fractional)" = "level_net",
                                 "Use fixed target workforce (W)" = "fixed_w",
                                 "Derive workers needed from Regular Time production (Wt = Rt / (K × days))" = "derived_w")),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'fixed_w'", ns("workforce_mode")),
          numericInput(ns("w_target"), "Target workforce (W) [can be fractional]", value = 15, min = 0.1, step = 0.5)
        ),
        
        hr(),
        h4("4. Overtime & Subcontracting Capacities"),
        radioButtons(ns("ot_cap_mode"), "Overtime capacity per period:",
                     choices = c("Percentage of Regular Time capacity (%)" = "pct_reg",
                                 "Fixed maximum units per period" = "fixed_units")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'pct_reg'", ns("ot_cap_mode")),
          numericInput(ns("ot_pct"), "Max Overtime (% of Regular capacity)", value = 25, min = 0, max = 200, step = 5)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'fixed_units'", ns("ot_cap_mode")),
          numericInput(ns("ot_fixed"), "Max Overtime (units / period)", value = 300, min = 0, step = 50)
        ),
        numericInput(ns("sub_cap"), "Max Subcontracting capacity (units / period)", value = 500, min = 0, step = 50),
        
        hr(),
        h4("5. Unit Costs ($)"),
        fluidRow(
          column(6, numericInput(ns("c_hire"), "Hiring ($ / worker)", value = 300, min = 0)),
          column(6, numericInput(ns("c_fire"), "Firing ($ / worker)", value = 500, min = 0))
        ),
        fluidRow(
          column(6, numericInput(ns("c_hold"), "Holding ($ / unit / period)", value = 2, min = 0)),
          column(6, numericInput(ns("c_reg"), "Regular time ($ / unit)", value = 10, min = 0))
        ),
        fluidRow(
          column(6, numericInput(ns("c_ot"), "Overtime ($ / unit)", value = 15, min = 0)),
          column(6, numericInput(ns("c_sub"), "Subcontracting ($ / unit)", value = 20, min = 0))
        )
      ),
      
      mainPanel(
        width = 8,
        h4(textOutput(ns("summary_headline"))),
        p(strong(textOutput(ns("productivity_headline")))),
        
        tabsetPanel(
          # Tab 1: Optimal (Cheapest Cell First) Solution
          tabPanel("Optimal (Cheapest Cell First) Solution",
                   br(),
                   h4("Aggregate Production Plan — Optimal (Cheapest Cell First)"),
                   fluidRow(
                     column(width = 12,
                            uiOutput(ns("opt_plans_ui"))
                     )
                   ),
                   h4("Cost Breakdown & Comparison — Optimal Solution"),
                   tableOutput(ns("opt_cost_table")),
                   h4("Transportation Tableau — Optimal (Cheapest Cell First) Allocations"),
                   p("Note: Each cell shows Allocated Units [Unit Cost $]. Rows are Regular Time (R), Overtime (O), and Subcontracting (S) sources per period."),
                   div(style = "overflow-x: auto;", tableOutput(ns("opt_tableau_table")))
          ),
          
          # Tab 2: North-West Corner (NWC) Starting Solution
          tabPanel("North-West Corner (NWC) Starting Solution",
                   br(),
                   h4("Aggregate Production Plan — North-West Corner Starting Solution"),
                   fluidRow(
                     column(width = 12,
                            uiOutput(ns("nwc_plans_ui"))
                     )
                   ),
                   h4("Cost Breakdown & Comparison — North-West Corner Solution"),
                   tableOutput(ns("nwc_cost_table")),
                   h4("Transportation Tableau — North-West Corner Allocations"),
                   p("Note: Each cell shows Allocated Units [Unit Cost $]. Allocations start from the top-left (North-West) corner."),
                   div(style = "overflow-x: auto;", tableOutput(ns("nwc_tableau_table")))
          ),
          
          # Tab 3: Method Comparison & Cumulative Plot
          tabPanel("Plot & Method Comparison",
                   br(),
                   radioButtons(ns("plot_solution_choice"), "Select solution to display on plot:",
                                choices = c("Optimal (Cheapest Cell First)" = "opt",
                                            "North-West Corner (NWC)" = "nwc"),
                                inline = TRUE),
                   h4("Cumulative Production vs. Cumulative Net Demand"),
                   plotOutput(ns("cum_plot"), height = "380px"),
                   hr(),
                   h4("Summary Comparison: North-West Corner vs. Optimal (Cheapest Cell First)"),
                   tableOutput(ns("method_comparison_table"))
          )
        )
      )
    )
  )
}

# Server Function for Aggregate Planning - Transportation Method
transportation_method_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Read uploaded Excel or CSV file
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
    
    # Update column selectors and auto-detect appropriate columns
    observeEvent(uploaded_data(), {
      df <- uploaded_data()
      cols <- names(df)
      
      p_col <- cols[grep("period|month|time|quarter|week", tolower(cols))[1]]
      if (is.na(p_col)) p_col <- cols[1]
      
      d_col <- cols[grep("demand|forecast|sales", tolower(cols))[1]]
      if (is.na(d_col)) d_col <- cols[min(2, length(cols))]
      
      w_col <- cols[grep("day|work", tolower(cols))[1]]
      if (is.na(w_col)) w_col <- cols[min(3, length(cols))]
      
      updateSelectInput(session, "period_col", choices = cols, selected = p_col)
      updateSelectInput(session, "demand_col", choices = cols, selected = d_col)
      updateSelectInput(session, "days_col", choices = cols, selected = w_col)
    })
    
    # Populate editable period names text box when period column changes
    observeEvent(list(uploaded_data(), input$period_col), {
      req(uploaded_data(), input$period_col)
      df <- uploaded_data()
      req(input$period_col %in% names(df))
      raw_periods <- as.character(df[[input$period_col]])
      raw_periods <- raw_periods[!is.na(raw_periods) & nzchar(raw_periods)]
      updateTextInput(session, "edit_periods", value = paste(raw_periods, collapse = ", "))
    })
    
    # Extract cleaned Period Names, Demand, and Working Days
    core_data <- reactive({
      req(uploaded_data(), input$period_col, input$demand_col, input$days_col)
      df <- uploaded_data()
      req(all(c(input$period_col, input$demand_col, input$days_col) %in% names(df)))
      
      demand <- as.numeric(df[[input$demand_col]])
      days <- as.numeric(df[[input$days_col]])
      valid_idx <- !is.na(demand) & !is.na(days)
      
      demand <- demand[valid_idx]
      days <- days[valid_idx]
      n_periods <- length(demand)
      validate(need(n_periods >= 2, "Uploaded data must have at least 2 valid periods."))
      
      # Period names (use edited names if count matches)
      default_names <- as.character(df[[input$period_col]][valid_idx])
      if (nzchar(trimws(input$edit_periods))) {
        edited <- trimws(unlist(strsplit(input$edit_periods, ",")))
        if (length(edited) == n_periods) {
          period_names <- edited
        } else {
          period_names <- default_names
        }
      } else {
        period_names <- default_names
      }
      
      list(
        periods = period_names,
        demand = demand,
        days = days,
        T_len = n_periods
      )
    })
    
    # 2. Productivity K
    k_val <- reactive({
      if (input$k_method == "direct") {
        req(!is.na(input$k_direct), input$k_direct > 0)
        as.numeric(input$k_direct)
      } else {
        req(!is.na(input$past_units), !is.na(input$past_workers), !is.na(input$past_days))
        validate(need(input$past_workers > 0 && input$past_days > 0,
                      "Past workers and working days must be greater than 0."))
        as.numeric(input$past_units) / (as.numeric(input$past_workers) * as.numeric(input$past_days))
      }
    })
    
    output$k_computed_display <- renderText({
      req(input$k_method == "computed")
      paste0("Computed K = ", round(k_val(), 4), " units / worker / day")
    })
    
    # 3. Core Transportation Solver Function (handles both NWC and Cheapest Cell First)
    solve_transportation_plan <- function(dat, K, W_vec, cap_R, cap_O, cap_S,
                                          w0, i0, it, costs, method = c("opt", "nwc"),
                                          derive_w = FALSE) {
      method <- match.arg(method)
      T_len <- dat$T_len
      demand <- dat$demand
      days <- dat$days
      
      # Compute Net Demand vector by netting out Starting Inventory (I0) and adding Ending Inventory (IT)
      net_demand <- demand
      rem_i0 <- i0
      for (t in seq_len(T_len)) {
        used <- min(rem_i0, net_demand[t])
        net_demand[t] <- net_demand[t] - used
        rem_i0 <- rem_i0 - used
      }
      net_demand[T_len] <- net_demand[T_len] + it
      
      # Build 3*T sources: R_t, O_t, S_t for each period t = 1..T
      n_rows <- 3 * T_len
      supplies <- numeric(n_rows)
      source_labels <- character(n_rows)
      source_period <- integer(n_rows)
      source_type <- character(n_rows)
      base_cost <- numeric(n_rows)
      
      for (t in seq_len(T_len)) {
        idx_r <- 3 * (t - 1) + 1
        idx_o <- 3 * (t - 1) + 2
        idx_s <- 3 * (t - 1) + 3
        
        supplies[c(idx_r, idx_o, idx_s)] <- c(cap_R[t], cap_O[t], cap_S[t])
        source_labels[c(idx_r, idx_o, idx_s)] <- c(
          paste0("Period ", t, " - Regular (R", t, ")"),
          paste0("Period ", t, " - Overtime (O", t, ")"),
          paste0("Period ", t, " - Subcontract (S", t, ")")
        )
        source_period[c(idx_r, idx_o, idx_s)] <- t
        source_type[c(idx_r, idx_o, idx_s)] <- c("R", "O", "S")
        base_cost[c(idx_r, idx_o, idx_s)] <- c(costs$c_reg, costs$c_ot, costs$c_sub)
      }
      
      total_supply <- sum(supplies)
      total_net_demand <- sum(net_demand)
      dummy_demand <- max(0, total_supply - total_net_demand)
      demands <- c(net_demand, dummy_demand)
      n_cols <- T_len + 1
      
      # Unit cost matrix (n_rows x n_cols); backordering (j < t) is forbidden (BIG_M)
      BIG_M <- 1e9
      cost_mat <- matrix(0, nrow = n_rows, ncol = n_cols)
      for (i in seq_len(n_rows)) {
        t_src <- source_period[i]
        for (j in seq_len(T_len)) {
          if (j >= t_src) {
            cost_mat[i, j] <- base_cost[i] + (j - t_src) * costs$c_hold
          } else {
            cost_mat[i, j] <- BIG_M
          }
        }
        cost_mat[i, n_cols] <- 0 # Dummy column has 0 cost
      }
      
      # Allocate supplies to demands
      alloc <- matrix(0, nrow = n_rows, ncol = n_cols)
      sup <- supplies
      dem <- demands
      
      if (method == "nwc") {
        # North-West Corner Rule (feasible cells j >= t_src)
        i <- 1
        j <- 1
        while (i <= n_rows && j <= n_cols) {
          if (cost_mat[i, j] < BIG_M / 2) {
            q <- min(sup[i], dem[j])
            alloc[i, j] <- q
            sup[i] <- sup[i] - q
            dem[j] <- dem[j] - q
            if (sup[i] <= 1e-7 && dem[j] <= 1e-7) {
              i <- i + 1
              j <- j + 1
            } else if (sup[i] <= 1e-7) {
              i <- i + 1
            } else {
              j <- j + 1
            }
          } else {
            j <- j + 1
          }
        }
        # Any unallocated supply due to upper-triangular jump goes to feasible/dummy cells
        for (i_rem in seq_len(n_rows)) {
          if (sup[i_rem] > 1e-7) {
            for (j_rem in seq_len(n_cols)) {
              if (cost_mat[i_rem, j_rem] < BIG_M / 2 && dem[j_rem] > 1e-7) {
                q <- min(sup[i_rem], dem[j_rem])
                alloc[i_rem, j_rem] <- alloc[i_rem, j_rem] + q
                sup[i_rem] <- sup[i_rem] - q
                dem[j_rem] <- dem[j_rem] - q
              }
            }
          }
        }
      } else {
        # Optimal (Cheapest Cell First) Solution
        # Create list of all feasible real demand cells (j = 1..T_len)
        cell_grid <- expand.grid(i = seq_len(n_rows), j = seq_len(T_len))
        cell_grid$cost <- mapply(function(r, c) cost_mat[r, c], cell_grid$i, cell_grid$j)
        cell_grid$src_t <- source_period[cell_grid$i]
        cell_grid <- subset(cell_grid, cost < BIG_M / 2)
        
        # Sort by: 1) lowest unit cost, 2) earliest demand column j, 3) latest production period src_t (minimize holding), 4) row index i
        ord <- order(cell_grid$cost, cell_grid$j, -cell_grid$src_t, cell_grid$i)
        cell_grid <- cell_grid[ord, ]
        
        for (k in seq_len(nrow(cell_grid))) {
          r <- cell_grid$i[k]
          c <- cell_grid$j[k]
          if (sup[r] > 1e-7 && dem[c] > 1e-7) {
            q <- min(sup[r], dem[c])
            alloc[r, c] <- q
            sup[r] <- sup[r] - q
            dem[c] <- dem[c] - q
          }
        }
        # Allocate leftover supply to Dummy column (n_cols)
        for (r in seq_len(n_rows)) {
          if (sup[r] > 1e-7 && dem[n_cols] > 1e-7) {
            q <- min(sup[r], dem[n_cols])
            alloc[r, n_cols] <- q
            sup[r] <- sup[r] - q
            dem[n_cols] <- dem[n_cols] - q
          }
        }
      }
      
      # Extract Period-by-Period Production from Allocations (sum across real destination columns 1..T_len)
      reg_prod <- numeric(T_len)
      ot_prod <- numeric(T_len)
      sub_prod <- numeric(T_len)
      
      for (t in seq_len(T_len)) {
        idx_r <- 3 * (t - 1) + 1
        idx_o <- 3 * (t - 1) + 2
        idx_s <- 3 * (t - 1) + 3
        reg_prod[t] <- sum(alloc[idx_r, seq_len(T_len)])
        ot_prod[t]  <- sum(alloc[idx_o, seq_len(T_len)])
        sub_prod[t] <- sum(alloc[idx_s, seq_len(T_len)])
      }
      
      total_prod <- reg_prod + ot_prod + sub_prod
      
      # Workforce, Hiring, Firing
      if (derive_w) {
        workers <- reg_prod / (K * days)
      } else {
        workers <- W_vec
      }
      
      hired <- numeric(T_len)
      fired <- numeric(T_len)
      prev_w <- w0
      for (t in seq_len(T_len)) {
        diff_w <- workers[t] - prev_w
        hired[t] <- max(0, diff_w)
        fired[t] <- max(0, -diff_w)
        prev_w <- workers[t]
      }
      
      # Ending Inventory per period: I_t = I_{t-1} + Total_Prod_t - Demand_t
      inv <- numeric(T_len)
      prev_i <- i0
      for (t in seq_len(T_len)) {
        inv[t] <- prev_i + total_prod[t] - demand[t]
        prev_i <- inv[t]
      }
      
      # Cost breakdown
      cost_hiring <- sum(hired) * costs$c_hire
      cost_firing <- sum(fired) * costs$c_fire
      cost_reg    <- sum(reg_prod) * costs$c_reg
      cost_ot     <- sum(ot_prod) * costs$c_ot
      cost_sub    <- sum(sub_prod) * costs$c_sub
      cost_hold   <- sum(pmax(0, inv)) * costs$c_hold
      total_cost  <- cost_hiring + cost_firing + cost_reg + cost_ot + cost_sub + cost_hold
      
      # Format Plan Data Frame
      plan_df <- data.frame(
        Period = dat$periods,
        `Working Days` = days,
        Demand = round(demand, 2),
        `Net Demand` = round(net_demand, 2),
        Workers = round(workers, 2),
        Hired = round(hired, 2),
        Fired = round(fired, 2),
        `Regular Prod.` = round(reg_prod, 2),
        `Overtime Prod.` = round(ot_prod, 2),
        Subcontracting = round(sub_prod, 2),
        `Total Prod.` = round(total_prod, 2),
        `Ending Inv.` = round(inv, 2),
        check.names = FALSE
      )
      
      # Format Tableau Data Frame
      tableau_df <- data.frame(Source = source_labels, stringsAsFactors = FALSE, check.names = FALSE)
      for (j in seq_len(T_len)) {
        col_name <- dat$periods[j]
        cell_strs <- character(n_rows)
        for (i in seq_len(n_rows)) {
          if (cost_mat[i, j] >= BIG_M / 2) {
            cell_strs[i] <- "— [M]"
          } else {
            cell_strs[i] <- paste0(round(alloc[i, j], 1), " [$", round(cost_mat[i, j], 1), "]")
          }
        }
        tableau_df[[col_name]] <- cell_strs
      }
      tableau_df[["Dummy (Unused)"]] <- paste0(round(alloc[, n_cols], 1), " [$0]")
      tableau_df[["Supply (Capacity)"]] <- round(supplies, 1)
      
      # Append Net Demand row at the bottom of tableau
      demand_row <- c("Net Demand", as.character(round(net_demand, 1)),
                      as.character(round(dummy_demand, 1)),
                      as.character(round(total_supply, 1)))
      tableau_df <- rbind(tableau_df, demand_row)
      
      list(
        plan_df = plan_df,
        tableau_df = tableau_df,
        workers = workers,
        is_fractional = any(abs(workers - round(workers)) > 1e-4),
        net_demand = net_demand,
        total_prod = total_prod,
        costs = c(
          Hiring = cost_hiring,
          Firing = cost_firing,
          `Regular Time` = cost_reg,
          Overtime = cost_ot,
          Subcontracting = cost_sub,
          `Inventory Holding` = cost_hold,
          `Total Cost` = total_cost
        )
      )
    }
    
    # 4. Compute both Original and Rounded-Up (Whole Workers) plans for NWC and Optimal
    all_solutions <- reactive({
      req(core_data(), k_val())
      dat <- core_data()
      K <- k_val()
      T_len <- dat$T_len
      days <- dat$days
      demand <- dat$demand
      
      w0 <- as.numeric(input$w0)
      i0 <- as.numeric(input$i0)
      it <- as.numeric(input$it)
      
      costs <- list(
        c_hire = as.numeric(input$c_hire),
        c_fire = as.numeric(input$c_fire),
        c_hold = as.numeric(input$c_hold),
        c_reg  = as.numeric(input$c_reg),
        c_ot   = as.numeric(input$c_ot),
        c_sub  = as.numeric(input$c_sub)
      )
      
      total_net_req <- max(0, sum(demand) - i0 + it)
      
      # Determine base Workforce vector and capacities
      if (input$workforce_mode == "level_net") {
        w_const <- total_net_req / (K * sum(days))
        W_orig <- rep(w_const, T_len)
        derive_w <- FALSE
      } else if (input$workforce_mode == "fixed_w") {
        W_orig <- rep(as.numeric(input$w_target), T_len)
        derive_w <- FALSE
      } else {
        # Derived workers from Regular Time production; capacity based on starting workforce W0
        W_orig <- rep(w0, T_len)
        derive_w <- TRUE
      }
      
      cap_R_orig <- W_orig * K * days
      if (input$ot_cap_mode == "pct_reg") {
        cap_O_orig <- (as.numeric(input$ot_pct) / 100) * cap_R_orig
      } else {
        cap_O_orig <- rep(as.numeric(input$ot_fixed), T_len)
      }
      cap_S_orig <- rep(as.numeric(input$sub_cap), T_len)
      
      # Solve Original Plans (Optimal & NWC)
      opt_orig <- solve_transportation_plan(dat, K, W_orig, cap_R_orig, cap_O_orig, cap_S_orig,
                                            w0, i0, it, costs, method = "opt", derive_w = derive_w)
      nwc_orig <- solve_transportation_plan(dat, K, W_orig, cap_R_orig, cap_O_orig, cap_S_orig,
                                            w0, i0, it, costs, method = "nwc", derive_w = derive_w)
      
      # Solve Whole Workers (Rounded-Up) Plans when workforce is fractional
      W_whole_opt <- ceiling(opt_orig$workers - 1e-7)
      cap_R_whole_opt <- W_whole_opt * K * days
      cap_O_whole_opt <- if (input$ot_cap_mode == "pct_reg") (as.numeric(input$ot_pct) / 100) * cap_R_whole_opt else cap_O_orig
      opt_whole <- solve_transportation_plan(dat, K, W_whole_opt, cap_R_whole_opt, cap_O_whole_opt, cap_S_orig,
                                             w0, i0, it, costs, method = "opt", derive_w = FALSE)
      
      W_whole_nwc <- ceiling(nwc_orig$workers - 1e-7)
      cap_R_whole_nwc <- W_whole_nwc * K * days
      cap_O_whole_nwc <- if (input$ot_cap_mode == "pct_reg") (as.numeric(input$ot_pct) / 100) * cap_R_whole_nwc else cap_O_orig
      nwc_whole <- solve_transportation_plan(dat, K, W_whole_nwc, cap_R_whole_nwc, cap_O_whole_nwc, cap_S_orig,
                                             w0, i0, it, costs, method = "nwc", derive_w = FALSE)
      
      list(
        opt_orig = opt_orig,
        opt_whole = opt_whole,
        nwc_orig = nwc_orig,
        nwc_whole = nwc_whole
      )
    })
    
    # Headline outputs
    output$summary_headline <- renderText({
      req(all_solutions())
      sol <- all_solutions()
      paste0("Optimal (Cheapest Cell First) Total Cost: $",
             format(round(sol$opt_orig$costs["Total Cost"], 2), big.mark = ","),
             " | North-West Corner Starting Total Cost: $",
             format(round(sol$nwc_orig$costs["Total Cost"], 2), big.mark = ","))
    })
    
    output$productivity_headline <- renderText({
      req(k_val(), all_solutions())
      sol <- all_solutions()
      w_info <- if (sol$opt_orig$is_fractional) {
        "Fractional workforce detected — Whole Workers (rounded-up) plan is displayed side-by-side."
      } else {
        "Workforce is already in whole numbers."
      }
      paste0("Productivity K = ", round(k_val(), 4), " units/worker/day | ", w_info)
    })
    
    # Dynamic UI for Optimal Plan (shows Whole Workers Plan side-by-side when fractional)
    output$opt_plans_ui <- renderUI({
      req(all_solutions())
      sol <- all_solutions()
      if (sol$opt_orig$is_fractional) {
        fluidRow(
          column(6,
                 h5(strong("Original Plan (Fractional Workers)")),
                 div(style = "overflow-x: auto;", tableOutput(session$ns("opt_plan_orig_table")))
          ),
          column(6,
                 h5(strong("Whole Workers Plan (Rounded Up ⌈W⌉)")),
                 div(style = "overflow-x: auto;", tableOutput(session$ns("opt_plan_whole_table")))
          )
        )
      } else {
        div(
          p(em("Note: Workforce is already an integer. Showing the Aggregate Plan below:")),
          div(style = "overflow-x: auto;", tableOutput(session$ns("opt_plan_orig_table")))
        )
      }
    })
    
    output$opt_plan_orig_table <- renderTable({
      req(all_solutions())
      all_solutions()$opt_orig$plan_df
    })
    
    output$opt_plan_whole_table <- renderTable({
      req(all_solutions())
      all_solutions()$opt_whole$plan_df
    })
    
    output$opt_cost_table <- renderTable({
      req(all_solutions())
      sol <- all_solutions()
      df <- data.frame(
        `Cost Item` = names(sol$opt_orig$costs),
        `Original Plan Cost ($)` = round(as.numeric(sol$opt_orig$costs), 2),
        check.names = FALSE
      )
      if (sol$opt_orig$is_fractional) {
        df[["Whole Workers (Rounded Up) Cost ($)"]] <- round(as.numeric(sol$opt_whole$costs), 2)
        df[["Difference ($)"]] <- round(as.numeric(sol$opt_whole$costs - sol$opt_orig$costs), 2)
      }
      df
    })
    
    output$opt_tableau_table <- renderTable({
      req(all_solutions())
      all_solutions()$opt_orig$tableau_df
    })
    
    # Dynamic UI for NWC Plan (shows Whole Workers Plan side-by-side when fractional)
    output$nwc_plans_ui <- renderUI({
      req(all_solutions())
      sol <- all_solutions()
      if (sol$nwc_orig$is_fractional) {
        fluidRow(
          column(6,
                 h5(strong("Original Plan (Fractional Workers)")),
                 div(style = "overflow-x: auto;", tableOutput(session$ns("nwc_plan_orig_table")))
          ),
          column(6,
                 h5(strong("Whole Workers Plan (Rounded Up ⌈W⌉)")),
                 div(style = "overflow-x: auto;", tableOutput(session$ns("nwc_plan_whole_table")))
          )
        )
      } else {
        div(
          p(em("Note: Workforce is already an integer. Showing the Aggregate Plan below:")),
          div(style = "overflow-x: auto;", tableOutput(session$ns("nwc_plan_orig_table")))
        )
      }
    })
    
    output$nwc_plan_orig_table <- renderTable({
      req(all_solutions())
      all_solutions()$nwc_orig$plan_df
    })
    
    output$nwc_plan_whole_table <- renderTable({
      req(all_solutions())
      all_solutions()$nwc_whole$plan_df
    })
    
    output$nwc_cost_table <- renderTable({
      req(all_solutions())
      sol <- all_solutions()
      df <- data.frame(
        `Cost Item` = names(sol$nwc_orig$costs),
        `Original Plan Cost ($)` = round(as.numeric(sol$nwc_orig$costs), 2),
        check.names = FALSE
      )
      if (sol$nwc_orig$is_fractional) {
        df[["Whole Workers (Rounded Up) Cost ($)"]] <- round(as.numeric(sol$nwc_whole$costs), 2)
        df[["Difference ($)"]] <- round(as.numeric(sol$nwc_whole$costs - sol$nwc_orig$costs), 2)
      }
      df
    })
    
    output$nwc_tableau_table <- renderTable({
      req(all_solutions())
      all_solutions()$nwc_orig$tableau_df
    })
    
    # Cumulative Production vs. Cumulative Net Demand Plot
    output$cum_plot <- renderPlot({
      req(all_solutions(), core_data())
      sol <- all_solutions()
      dat <- core_data()
      
      chosen <- if (input$plot_solution_choice == "opt") sol$opt_orig else sol$nwc_orig
      chosen_whole <- if (input$plot_solution_choice == "opt") sol$opt_whole else sol$nwc_whole
      title_label <- if (input$plot_solution_choice == "opt") {
        "Optimal (Cheapest Cell First) Solution"
      } else {
        "North-West Corner (NWC) Starting Solution"
      }
      
      cum_net_dem <- cumsum(chosen$net_demand)
      cum_prod_orig <- cumsum(chosen$total_prod)
      cum_prod_whole <- cumsum(chosen_whole$total_prod)
      
      T_len <- dat$T_len
      x_idx <- seq_len(T_len)
      y_range <- range(c(cum_net_dem, cum_prod_orig, cum_prod_whole), na.rm = TRUE)
      
      plot(x_idx, cum_prod_orig, type = "b", pch = 19, col = "steelblue", lwd = 2,
           xaxt = "n", xlab = "Period", ylab = "Cumulative Units",
           main = paste0("Cumulative Production vs. Cumulative Net Demand — ", title_label),
           ylim = y_range)
      axis(1, at = x_idx, labels = dat$periods)
      
      lines(x_idx, cum_net_dem, type = "b", pch = 17, col = "firebrick", lwd = 2, lty = 2)
      
      if (chosen$is_fractional) {
        lines(x_idx, cum_prod_whole, type = "b", pch = 15, col = "darkgreen", lwd = 2, lty = 3)
        legend("topleft",
               legend = c("Cumulative Production (Original)",
                          "Cumulative Production (Whole Workers ⌈W⌉)",
                          "Cumulative Net Demand"),
               col = c("steelblue", "darkgreen", "firebrick"),
               pch = c(19, 15, 17), lty = c(1, 3, 2), lwd = 2)
      } else {
        legend("topleft",
               legend = c("Cumulative Production", "Cumulative Net Demand"),
               col = c("steelblue", "firebrick"),
               pch = c(19, 17), lty = c(1, 2), lwd = 2)
      }
    })
    
    # Side-by-side comparison table between NWC and Optimal (Cheapest Cell First)
    output$method_comparison_table <- renderTable({
      req(all_solutions())
      sol <- all_solutions()
      df <- data.frame(
        `Cost Item` = names(sol$opt_orig$costs),
        `North-West Corner (Original $)` = round(as.numeric(sol$nwc_orig$costs), 2),
        `Optimal - Cheapest Cell First (Original $)` = round(as.numeric(sol$opt_orig$costs), 2),
        `Cost Savings by Optimal ($)` = round(as.numeric(sol$nwc_orig$costs - sol$opt_orig$costs), 2),
        check.names = FALSE
      )
      if (sol$opt_orig$is_fractional) {
        df[["North-West Corner (Whole Workers $)"]] <- round(as.numeric(sol$nwc_whole$costs), 2)
        df[["Optimal (Whole Workers $)"]] <- round(as.numeric(sol$opt_whole$costs), 2)
      }
      df
    })
  })
}
