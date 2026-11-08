library(shiny)
library(readxl)

# UI Function for Disaggregation (Square-Root Rule)
disaggregation_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("1. Data Upload & Period Names"),
        fileInput(ns("file"), "Upload Excel or CSV file (.xlsx, .xls, .csv)",
                  accept = c(".xlsx", ".xls", ".csv")),
        
        selectInput(ns("period_col"), "Period names column", choices = NULL),
        selectInput(ns("demand_col"), "Aggregate demand forecasts column", choices = NULL),
        selectInput(ns("days_col"), "Working days column", choices = NULL),
        
        textInput(ns("edit_periods"), "Edit period names (comma-separated):", value = ""),
        
        hr(),
        h4("2. Product Families (Square-Root Rule)"),
        p("Enter comma-separated values for each product family (i = 1, ..., m):"),
        textInput(ns("fam_names"), "Product family names:",
                  value = "Family A, Family B, Family C"),
        textInput(ns("fam_demands"), "Family demand rates / shares (D_i):",
                  value = "500, 300, 200"),
        textInput(ns("fam_setups"), "Family setup costs ($ / setup, K_i):",
                  value = "400, 250, 150"),
        textInput(ns("fam_holding"), "Family holding costs ($ / unit / period, h_i):",
                  value = "2, 2, 2"),
        
        radioButtons(ns("sqrt_formula"), "Square-root allocation weight formula:",
                     choices = c("Standard: w_i ∝ √(K_i × D_i)" = "kd",
                                 "With holding cost: w_i ∝ √(K_i × D_i / h_i)" = "kdh")),
        
        hr(),
        h4("3. Base Aggregate Plan & Productivity (K)"),
        selectInput(ns("agg_strategy"), "Aggregate plan strategy to disaggregate:",
                    choices = c("Constant-Workforce (Level) Plan" = "level",
                                "Zero-Inventory (Chase) Plan" = "chase",
                                "Optimal Transportation Plan (R, O, S)" = "trans")),
        
        radioButtons(ns("k_method"), "Productivity K (units / worker / day):",
                     choices = c("Enter K directly" = "direct",
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
        h4("4. Plant & Cost Data ($)"),
        fluidRow(
          column(4, numericInput(ns("w0"), "Start W0", value = 15, min = 0)),
          column(4, numericInput(ns("i0"), "Start I0", value = 200, min = 0)),
          column(4, numericInput(ns("it"), "End IT", value = 100, min = 0))
        ),
        fluidRow(
          column(6, numericInput(ns("c_hire"), "Hiring ($ / worker)", value = 300, min = 0)),
          column(6, numericInput(ns("c_fire"), "Firing ($ / worker)", value = 500, min = 0))
        ),
        fluidRow(
          column(6, numericInput(ns("c_hold"), "Holding ($ / unit)", value = 2, min = 0)),
          column(6, numericInput(ns("c_reg"), "Regular ($ / unit)", value = 10, min = 0))
        ),
        fluidRow(
          column(6, numericInput(ns("c_ot"), "Overtime ($ / unit)", value = 15, min = 0)),
          column(6, numericInput(ns("c_sub"), "Subcontract ($ / unit)", value = 20, min = 0))
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'trans'", ns("agg_strategy")),
          hr(),
          fluidRow(
            column(6, numericInput(ns("ot_pct"), "Max Overtime (% of Reg)", value = 25, min = 0)),
            column(6, numericInput(ns("sub_cap"), "Max Subcontract / period", value = 500, min = 0))
          )
        )
      ),
      
      mainPanel(
        width = 8,
        h4(textOutput(ns("summary_headline"))),
        p(strong(textOutput(ns("sub_headline")))),
        
        tabsetPanel(
          # Tab 1: Square-Root Rule Disaggregation Results
          tabPanel("Disaggregation by Product Family (Square-Root Rule)",
                   br(),
                   h4("1. Square-Root Rule Allocation Weights & Joint Lot-Sizing Summary"),
                   tableOutput(ns("weights_table")),
                   
                   hr(),
                   radioButtons(ns("disagg_plan_basis"), "Select workforce basis for period-by-period disaggregation:",
                                choices = c("Original Plan (Exact / Fractional Workforce)" = "orig",
                                            "Whole Workers Plan (Rounded-Up Workforce ⌈W⌉)" = "whole"),
                                inline = TRUE),
                   
                   h4("2. Disaggregated Production Quantities by Product Family and Period"),
                   div(style = "overflow-x: auto;", tableOutput(ns("disagg_prod_table"))),
                   
                   h4("3. Disaggregated Setup Frequency, Lot Sizes & Family Costs"),
                   div(style = "overflow-x: auto;", tableOutput(ns("disagg_family_cost_table"))),
                   
                   h4("4. Product Family Production Share Plot"),
                   plotOutput(ns("family_bar_plot"), height = "320px")
          ),
          
          # Tab 2: Aggregate Plan & Whole Workers Comparison
          tabPanel("Aggregate Plan & Costs (Original vs. Whole Workers)",
                   br(),
                   h4("Aggregate Production Plan (Original vs. Whole Workers)"),
                   uiOutput(ns("agg_plans_ui")),
                   
                   h4("Aggregate Plan Cost Breakdown & Comparison"),
                   tableOutput(ns("agg_cost_table"))
          ),
          
          # Tab 3: Cumulative Plot
          tabPanel("Cumulative Production vs. Net Demand Plot",
                   br(),
                   h4("Cumulative Production vs. Cumulative Net Demand"),
                   plotOutput(ns("cum_plot"), height = "380px")
          )
        )
      )
    )
  )
}

# Server Function for Disaggregation (Square-Root Rule)
disaggregation_server <- function(id) {
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
    
    # Auto-detect columns
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
    
    # Populate editable period names
    observeEvent(list(uploaded_data(), input$period_col), {
      req(uploaded_data(), input$period_col)
      df <- uploaded_data()
      req(input$period_col %in% names(df))
      raw_p <- as.character(df[[input$period_col]])
      raw_p <- raw_p[!is.na(raw_p) & nzchar(raw_p)]
      updateTextInput(session, "edit_periods", value = paste(raw_p, collapse = ", "))
    })
    
    # Extract cleaned period data
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
      validate(need(n_periods >= 2, "Uploaded data must contain at least 2 valid periods."))
      
      default_names <- as.character(df[[input$period_col]][valid_idx])
      if (nzchar(trimws(input$edit_periods))) {
        edited <- trimws(unlist(strsplit(input$edit_periods, ",")))
        period_names <- if (length(edited) == n_periods) edited else default_names
      } else {
        period_names <- default_names
      }
      
      list(periods = period_names, demand = demand, days = days, T_len = n_periods)
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
    
    # 3. Parse Product Family Inputs & Compute Square-Root Rule Weights
    family_params <- reactive({
      f_names <- trimws(unlist(strsplit(input$fam_names, ",")))
      f_dem   <- as.numeric(trimws(unlist(strsplit(input$fam_demands, ","))))
      f_setup <- as.numeric(trimws(unlist(strsplit(input$fam_setups, ","))))
      f_hold  <- as.numeric(trimws(unlist(strsplit(input$fam_holding, ","))))
      
      m <- length(f_names)
      validate(
        need(m >= 2, "Please enter at least 2 product families separated by commas."),
        need(length(f_dem) == m && all(!is.na(f_dem)) && all(f_dem > 0),
             "Number of positive family demand values (D_i) must match the number of product families."),
        need(length(f_setup) == m && all(!is.na(f_setup)) && all(f_setup >= 0),
             "Number of non-negative setup costs (K_i) must match the number of product families."),
        need(length(f_hold) == m && all(!is.na(f_hold)) && all(f_hold > 0),
             "Number of positive holding costs (h_i) must match the number of product families.")
      )
      
      demand_share <- f_dem / sum(f_dem)
      
      if (input$sqrt_formula == "kd") {
        raw_sqrt <- sqrt(f_setup * f_dem)
      } else {
        raw_sqrt <- sqrt((f_setup * f_dem) / f_hold)
      }
      
      if (sum(raw_sqrt) <= 0) {
        sqrt_weights <- demand_share
      } else {
        sqrt_weights <- raw_sqrt / sum(raw_sqrt)
      }
      
      # Joint cycle time T* = sqrt(2 * sum(K_i) / sum(h_i * D_i)) and optimal batch sizes Q_i* = D_i * T*
      cycle_time <- sqrt((2 * sum(f_setup)) / sum(f_hold * f_dem))
      opt_batch_q <- f_dem * cycle_time
      
      list(
        m = m,
        names = f_names,
        demands = f_dem,
        demand_share = demand_share,
        setups = f_setup,
        holding = f_hold,
        raw_sqrt = raw_sqrt,
        weights = sqrt_weights,
        cycle_time = cycle_time,
        opt_batch_q = opt_batch_q
      )
    })
    
    # 4. Helper to build Aggregate Plan (Level, Chase, or Transportation) for Original and Whole Workers
    compute_agg_plan <- function(dat, K, strategy, w0, i0, it, costs, ot_pct, sub_cap, round_up = FALSE) {
      T_len <- dat$T_len
      demand <- dat$demand
      days <- dat$days
      
      # Net demand vector
      net_demand <- demand
      rem_i0 <- i0
      for (t in seq_len(T_len)) {
        used <- min(rem_i0, net_demand[t])
        net_demand[t] <- net_demand[t] - used
        rem_i0 <- rem_i0 - used
      }
      net_demand[T_len] <- net_demand[T_len] + it
      
      if (strategy == "level") {
        w_const <- sum(net_demand) / (K * sum(days))
        if (round_up) w_const <- ceiling(w_const - 1e-7)
        workers <- rep(w_const, T_len)
        reg_prod <- workers * K * days
        ot_prod <- rep(0, T_len)
        sub_prod <- rep(0, T_len)
        
      } else if (strategy == "chase") {
        workers <- net_demand / (K * days)
        if (round_up) workers <- ceiling(workers - 1e-7)
        reg_prod <- workers * K * days
        ot_prod <- rep(0, T_len)
        sub_prod <- rep(0, T_len)
        
      } else {
        # Optimal Transportation Plan based on constant workforce from net demand
        w_const <- sum(net_demand) / (K * sum(days))
        if (round_up) w_const <- ceiling(w_const - 1e-7)
        workers <- rep(w_const, T_len)
        
        cap_R <- workers * K * days
        cap_O <- (ot_pct / 100) * cap_R
        cap_S <- rep(sub_cap, T_len)
        
        n_rows <- 3 * T_len
        supplies <- numeric(n_rows)
        src_t <- integer(n_rows)
        base_c <- numeric(n_rows)
        for (t in seq_len(T_len)) {
          idx <- (3 * (t - 1) + 1):(3 * t)
          supplies[idx] <- c(cap_R[t], cap_O[t], cap_S[t])
          src_t[idx] <- t
          base_c[idx] <- c(costs$c_reg, costs$c_ot, costs$c_sub)
        }
        
        BIG_M <- 1e9
        cost_mat <- matrix(BIG_M, nrow = n_rows, ncol = T_len)
        for (r in seq_len(n_rows)) {
          for (c in seq_len(T_len)) {
            if (c >= src_t[r]) cost_mat[r, c] <- base_c[r] + (c - src_t[r]) * costs$c_hold
          }
        }
        
        alloc <- matrix(0, nrow = n_rows, ncol = T_len)
        sup <- supplies
        dem <- net_demand
        grid <- expand.grid(r = seq_len(n_rows), c = seq_len(T_len))
        grid$cost <- mapply(function(r, c) cost_mat[r, c], grid$r, grid$c)
        grid$st <- src_t[grid$r]
        grid <- subset(grid, cost < BIG_M / 2)
        grid <- grid[order(grid$cost, grid$c, -grid$st, grid$r), ]
        
        for (k in seq_len(nrow(grid))) {
          r <- grid$r[k]; c <- grid$c[k]
          if (sup[r] > 1e-7 && dem[c] > 1e-7) {
            q <- min(sup[r], dem[c])
            alloc[r, c] <- q
            sup[r] <- sup[r] - q
            dem[c] <- dem[c] - q
          }
        }
        
        reg_prod <- sapply(seq_len(T_len), function(t) sum(alloc[3 * (t - 1) + 1, ]))
        ot_prod  <- sapply(seq_len(T_len), function(t) sum(alloc[3 * (t - 1) + 2, ]))
        sub_prod <- sapply(seq_len(T_len), function(t) sum(alloc[3 * (t - 1) + 3, ]))
      }
      
      total_prod <- reg_prod + ot_prod + sub_prod
      
      hired <- numeric(T_len)
      fired <- numeric(T_len)
      prev_w <- w0
      for (t in seq_len(T_len)) {
        diff_w <- workers[t] - prev_w
        hired[t] <- max(0, diff_w)
        fired[t] <- max(0, -diff_w)
        prev_w <- workers[t]
      }
      
      inv <- numeric(T_len)
      prev_i <- i0
      for (t in seq_len(T_len)) {
        inv[t] <- prev_i + total_prod[t] - demand[t]
        prev_i <- inv[t]
      }
      
      c_hire_tot <- sum(hired) * costs$c_hire
      c_fire_tot <- sum(fired) * costs$c_fire
      c_reg_tot  <- sum(reg_prod) * costs$c_reg
      c_ot_tot   <- sum(ot_prod) * costs$c_ot
      c_sub_tot  <- sum(sub_prod) * costs$c_sub
      c_hold_tot <- sum(pmax(0, inv)) * costs$c_hold
      tot_cost   <- c_hire_tot + c_fire_tot + c_reg_tot + c_ot_tot + c_sub_tot + c_hold_tot
      
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
      
      list(
        plan_df = plan_df,
        workers = workers,
        is_fractional = any(abs(workers - round(workers)) > 1e-4),
        net_demand = net_demand,
        total_prod = total_prod,
        inv = inv,
        costs = c(
          Hiring = c_hire_tot,
          Firing = c_fire_tot,
          `Regular Time` = c_reg_tot,
          Overtime = c_ot_tot,
          Subcontracting = c_sub_tot,
          `Inventory Holding` = c_hold_tot,
          `Total Aggregate Cost` = tot_cost
        )
      )
    }
    
    # Compute Original and Whole-Workers Aggregate Plans
    agg_results <- reactive({
      req(core_data(), k_val())
      dat <- core_data()
      K <- k_val()
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
      
      orig <- compute_agg_plan(dat, K, input$agg_strategy, w0, i0, it, costs,
                               as.numeric(input$ot_pct), as.numeric(input$sub_cap), round_up = FALSE)
      whole <- compute_agg_plan(dat, K, input$agg_strategy, w0, i0, it, costs,
                                as.numeric(input$ot_pct), as.numeric(input$sub_cap), round_up = TRUE)
      
      list(orig = orig, whole = whole)
    })
    
    # Headline outputs
    output$summary_headline <- renderText({
      req(agg_results(), family_params())
      res <- agg_results()
      fam <- family_params()
      paste0("Disaggregation across ", fam$m, " Product Families (Square-Root Rule) | ",
             "Original Plan Total Cost: $", format(round(res$orig$costs["Total Aggregate Cost"], 2), big.mark = ","),
             " | Whole Workers Plan Total Cost: $", format(round(res$whole$costs["Total Aggregate Cost"], 2), big.mark = ","))
    })
    
    output$sub_headline <- renderText({
      req(k_val(), family_params())
      fam <- family_params()
      w_str <- paste0(fam$names, ": ", round(fam$weights * 100, 1), "%", collapse = " | ")
      paste0("Productivity K = ", round(k_val(), 4), " units/worker/day | Square-Root Shares: ", w_str)
    })
    
    # 1. Square-Root Rule Weights Table
    output$weights_table <- renderTable({
      req(family_params(), agg_results())
      fam <- family_params()
      chosen_plan <- if (input$disagg_plan_basis == "whole") agg_results()$whole else agg_results()$orig
      tot_agg_prod <- sum(chosen_plan$total_prod)
      
      data.frame(
        `Product Family` = fam$names,
        `Demand Rate (D_i)` = round(fam$demands, 2),
        `Setup Cost (K_i $)` = round(fam$setups, 2),
        `Holding Cost (h_i $)` = round(fam$holding, 2),
        `√(K_i × D_i) Factor` = round(fam$raw_sqrt, 3),
        `Square-Root Weight (w_i)` = round(fam$weights, 4),
        `Share (%)` = round(fam$weights * 100, 2),
        `Total Allocated Production` = round(fam$weights * tot_agg_prod, 2),
        check.names = FALSE
      )
    }, digits = 4)
    
    # 2. Period-by-Period Disaggregated Production Table
    output$disagg_prod_table <- renderTable({
      req(family_params(), agg_results(), core_data())
      fam <- family_params()
      dat <- core_data()
      chosen <- if (input$disagg_plan_basis == "whole") agg_results()$whole else agg_results()$orig
      
      df <- data.frame(
        Period = dat$periods,
        `Aggregate Production (P_t)` = round(chosen$total_prod, 2),
        check.names = FALSE
      )
      
      for (i in seq_len(fam$m)) {
        col_label <- paste0(fam$names[i], " (", round(fam$weights[i] * 100, 1), "%)")
        df[[col_label]] <- round(chosen$total_prod * fam$weights[i], 2)
      }
      
      # Add Total row at bottom
      tot_row <- c("Total", round(sum(chosen$total_prod), 2),
                   round(sapply(seq_len(fam$m), function(i) sum(chosen$total_prod * fam$weights[i])), 2))
      df <- rbind(df, tot_row)
      df
    })
    
    # 3. Disaggregated Family Lot-Sizing & Cost Summary Table
    output$disagg_family_cost_table <- renderTable({
      req(family_params(), agg_results())
      fam <- family_params()
      chosen <- if (input$disagg_plan_basis == "whole") agg_results()$whole else agg_results()$orig
      
      alloc_prod <- fam$weights * sum(chosen$total_prod)
      alloc_inv  <- fam$weights * sum(pmax(0, chosen$inv))
      
      # Number of setups N_i = Allocated Production / Optimal Batch Q_i*
      num_setups <- ifelse(fam$opt_batch_q > 0, alloc_prod / fam$opt_batch_q, 0)
      setup_cost_fam <- num_setups * fam$setups
      hold_cost_fam  <- alloc_inv * fam$holding
      prod_cost_fam  <- fam$weights * (chosen$costs["Regular Time"] + chosen$costs["Overtime"] + chosen$costs["Subcontracting"])
      tot_fam_cost   <- setup_cost_fam + hold_cost_fam + prod_cost_fam
      
      data.frame(
        `Product Family` = fam$names,
        `Square-Root Weight (w_i)` = round(fam$weights, 4),
        `Optimal Batch Size (Q_i*)` = round(fam$opt_batch_q, 2),
        `Estimated Setups` = round(num_setups, 2),
        `Setup Cost ($)` = round(setup_cost_fam, 2),
        `Holding Cost ($)` = round(hold_cost_fam, 2),
        `Allocated Prod. Cost ($)` = round(prod_cost_fam, 2),
        `Total Family Cost ($)` = round(tot_fam_cost, 2),
        check.names = FALSE
      )
    })
    
    # 4. Product Family Production Share Bar Plot
    output$family_bar_plot <- renderPlot({
      req(family_params(), agg_results(), core_data())
      fam <- family_params()
      dat <- core_data()
      chosen <- if (input$disagg_plan_basis == "whole") agg_results()$whole else agg_results()$orig
      
      mat <- sapply(seq_len(dat$T_len), function(t) chosen$total_prod[t] * fam$weights)
      if (!is.matrix(mat)) mat <- matrix(mat, nrow = fam$m)
      colnames(mat) <- dat$periods
      rownames(mat) <- fam$names
      
      cols <- hcl.colors(fam$m, palette = "TealGrn")
      barplot(mat, beside = FALSE, col = cols, border = "white",
              xlab = "Period", ylab = "Production Units",
              main = "Disaggregated Production by Product Family (Square-Root Rule)",
              legend.text = fam$names,
              args.legend = list(x = "topright", bty = "n"))
    })
    
    # 5. Aggregate Plans Side-by-Side UI (Original vs Whole Workers)
    output$agg_plans_ui <- renderUI({
      req(agg_results())
      res <- agg_results()
      if (res$orig$is_fractional) {
        fluidRow(
          column(6,
                 h5(strong("Original Plan (Fractional Workers)")),
                 div(style = "overflow-x: auto;", tableOutput(session$ns("agg_orig_table")))
          ),
          column(6,
                 h5(strong("Whole Workers Plan (Rounded Up ⌈W⌉)")),
                 div(style = "overflow-x: auto;", tableOutput(session$ns("agg_whole_table")))
          )
        )
      } else {
        div(
          p(em("Note: Workforce is already in whole numbers. Showing the Aggregate Plan below:")),
          div(style = "overflow-x: auto;", tableOutput(session$ns("agg_orig_table")))
        )
      }
    })
    
    output$agg_orig_table <- renderTable({
      req(agg_results())
      agg_results()$orig$plan_df
    })
    
    output$agg_whole_table <- renderTable({
      req(agg_results())
      agg_results()$whole$plan_df
    })
    
    output$agg_cost_table <- renderTable({
      req(agg_results())
      res <- agg_results()
      df <- data.frame(
        `Cost Item` = names(res$orig$costs),
        `Original Plan Cost ($)` = round(as.numeric(res$orig$costs), 2),
        check.names = FALSE
      )
      if (res$orig$is_fractional) {
        df[["Whole Workers (Rounded Up) Cost ($)"]] <- round(as.numeric(res$whole$costs), 2)
        df[["Difference ($)"]] <- round(as.numeric(res$whole$costs - res$orig$costs), 2)
      }
      df
    })
    
    # 6. Cumulative Production vs. Cumulative Net Demand Plot
    output$cum_plot <- renderPlot({
      req(agg_results(), core_data())
      res <- agg_results()
      dat <- core_data()
      
      cum_net <- cumsum(res$orig$net_demand)
      cum_orig <- cumsum(res$orig$total_prod)
      cum_whole <- cumsum(res$whole$total_prod)
      
      x_idx <- seq_len(dat$T_len)
      y_range <- range(c(cum_net, cum_orig, cum_whole), na.rm = TRUE)
      
      plot(x_idx, cum_orig, type = "b", pch = 19, col = "steelblue", lwd = 2,
           xaxt = "n", xlab = "Period", ylab = "Cumulative Units",
           main = "Cumulative Production vs. Cumulative Net Demand",
           ylim = y_range)
      axis(1, at = x_idx, labels = dat$periods)
      lines(x_idx, cum_net, type = "b", pch = 17, col = "firebrick", lwd = 2, lty = 2)
      
      if (res$orig$is_fractional) {
        lines(x_idx, cum_whole, type = "b", pch = 15, col = "darkgreen", lwd = 2, lty = 3)
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
  })
}
