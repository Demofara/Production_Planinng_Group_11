library(shiny)
library(readxl)

# Reusable UI Function for Aggregate Planning Heuristic Methods
# plan_type: "chase" (Zero-Inventory), "level" (Constant-Workforce), or "mixed" (Mixed Plan)
heuristic_plan_ui <- function(id, plan_type = c("chase", "level", "mixed")) {
  plan_type <- match.arg(plan_type)
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
        
        # Editable period names inside the app
        textInput(ns("custom_periods"), 
                  "Edit period names (comma-separated)", 
                  value = ""),
        
        hr(),
        
        # 2. Productivity (K) Settings
        h4("2. Productivity (K)"),
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
        
        # 3. Plant Data (Initial Workforce & Inventories)
        h4("3. Plant Parameters"),
        fluidRow(
          column(4, numericInput(ns("w0"), "Starting Workforce", value = 15, min = 0, step = 1)),
          column(4, numericInput(ns("i0"), "Starting Inventory", value = 100, min = 0, step = 10)),
          column(4, numericInput(ns("it"), "Ending Inventory", value = 50, min = 0, step = 10))
        ),
        
        hr(),
        
        # 4. Unit Costs
        h4("4. Cost Parameters ($)"),
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
                                "Per unit produced (Production * Cost)" = "per_unit")),
        fluidRow(
          column(6, numericInput(ns("c_ot"), "Overtime cost (/unit)", value = 25, min = 0)),
          column(6, numericInput(ns("c_sub"), "Subcontracting cost (/unit)", value = 35, min = 0))
        ),
        
        hr(),
        
        # 5. Strategy-Specific Settings & Rounding Options
        h4("5. Strategy & Rounding Settings"),
        
        if (plan_type == "level") {
          radioButtons(ns("level_rule"), "Constant workforce calculation rule:",
                       choices = c("No stockouts allowed (Peak cumulative ratio)" = "no_stockout",
                                   "Exact ending inventory (Total horizon average + Subcontracting if short)" = "average"))
        } else if (plan_type == "mixed") {
          tagList(
            checkboxGroupInput(ns("change_periods"), 
                               "Select periods in which workforce is allowed to change:",
                               choices = NULL),
            radioButtons(ns("mixed_rule"), "Workforce rule within each constant interval:",
                         choices = c("No stockouts allowed in interval (Peak ratio per block)" = "no_stockout",
                                     "Match interval net requirement (Average ratio per block)" = "average"))
          )
        } else {
          NULL
        },
        
        radioButtons(ns("round_rule"), "Whole workers (rounding up) method:",
                     choices = c("Round up fractional workers directly: ceiling(W)" = "direct_ceil",
                                 "Sequential ceiling with inventory carry-over" = "seq_ceil"))
      ),
      
      mainPanel(
        width = 8,
        h4(textOutput(ns("plan_title_text"))),
        p(strong(textOutput(ns("productivity_info_text")))),
        
        h4("Cumulative Production vs. Cumulative Net Demand"),
        plotOutput(ns("cum_plot"), height = "380px"),
        
        hr(),
        
        h4("Cost Comparison: Original (Fractional) vs. Whole Workers (Rounded-Up)"),
        fluidRow(
          column(6,
                 h5(strong("Original Plan Cost Breakdown")),
                 tableOutput(ns("cost_table_frac"))),
          column(6,
                 h5(strong("Whole Workers (Rounded-Up) Cost Breakdown")),
                 tableOutput(ns("cost_table_whole")))
        ),
        
        hr(),
        
        h4("Aggregate Plan Tables (Original vs. Whole Workers Side-by-Side)"),
        fluidRow(
          column(6,
                 h5(strong("Original (Fractional Workforce) Plan")),
                 div(style = "overflow-x: auto;", tableOutput(ns("plan_table_frac")))),
          column(6,
                 h5(strong("Whole Workers (Rounded-Up Workforce) Plan")),
                 div(style = "overflow-x: auto;", tableOutput(ns("plan_table_whole"))))
        )
      )
    )
  )
}

# Reusable Server Function for Aggregate Planning Heuristic Methods
heuristic_plan_server <- function(id, plan_type = c("chase", "level", "mixed")) {
  plan_type <- match.arg(plan_type)
  
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
      
      # Smart default column matching
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
    
    # Update editable period names text box when period_col changes
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
      
      # Parse user-edited period names
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
    
    # Update Mixed Plan period checkboxes when periods change
    observeEvent(plan_inputs(), {
      if (plan_type == "mixed") {
        inp <- plan_inputs()
        choices_vec <- as.character(seq_len(inp$n))
        names(choices_vec) <- paste0(seq_len(inp$n), ": ", inp$periods)
        
        # Default: Period 1 and middle period
        mid_p <- max(2, ceiling(inp$n / 2) + 1)
        default_sel <- unique(c("1", as.character(min(inp$n, mid_p))))
        updateCheckboxGroupInput(session, "change_periods",
                                 choices = choices_vec,
                                 selected = default_sel)
      }
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
    
    # Helper function to evaluate a workforce schedule W[1..T] and compute full table + costs
    evaluate_schedule <- function(W, d, days, p_names, K, w0, i0, it_target, allow_sub = FALSE) {
      n <- length(d)
      prod <- W * K * days
      hired <- numeric(n)
      fired <- numeric(n)
      inv <- numeric(n)
      overtime <- rep(0, n)
      subcontract <- rep(0, n)
      
      prev_w <- w0
      prev_i <- i0
      
      for (t in seq_len(n)) {
        diff_w <- W[t] - prev_w
        hired[t] <- if (diff_w > 0) diff_w else 0
        fired[t] <- if (diff_w < 0) -diff_w else 0
        
        avail <- prev_i + prod[t]
        target_floor <- if (t == n) it_target else 0
        
        if (allow_sub && avail < d[t] + target_floor) {
          subcontract[t] <- (d[t] + target_floor) - avail
          inv[t] <- target_floor
        } else {
          inv[t] <- avail - d[t]
        }
        
        prev_w <- W[t]
        prev_i <- inv[t]
      }
      
      # Calculate itemized costs
      cost_hire <- sum(hired) * input$c_hire
      cost_fire <- sum(fired) * input$c_fire
      cost_hold <- sum(pmax(0, inv)) * input$c_hold
      
      cost_reg <- switch(input$reg_cost_basis,
                         "per_worker_day"    = sum(W * days) * input$c_reg,
                         "per_worker_period" = sum(W) * input$c_reg,
                         "per_unit"          = sum(prod) * input$c_reg)
      
      cost_ot  <- sum(overtime) * input$c_ot
      cost_sub <- sum(subcontract) * input$c_sub
      total_cost <- cost_hire + cost_fire + cost_hold + cost_reg + cost_ot + cost_sub
      
      plan_df <- data.frame(
        Period = p_names,
        Days = days,
        Demand = d,
        Workers = round(W, 2),
        Hired = round(hired, 2),
        Fired = round(fired, 2),
        Production = round(prod, 2),
        Inventory = round(inv, 2),
        Overtime = round(overtime, 2),
        Subcontract = round(subcontract, 2),
        check.names = FALSE
      )
      
      cost_df <- data.frame(
        `Cost Item` = c("Hiring Cost", "Firing Cost", "Inventory Holding Cost",
                        "Regular Time Cost", "Overtime Cost", "Subcontracting Cost",
                        "TOTAL COST"),
        `Total Units / Workers` = c(round(sum(hired), 2),
                                    round(sum(fired), 2),
                                    round(sum(pmax(0, inv)), 2),
                                    round(sum(W), 2),
                                    round(sum(overtime), 2),
                                    round(sum(subcontract), 2),
                                    NA),
        `Cost ($)` = round(c(cost_hire, cost_fire, cost_hold, cost_reg, cost_ot, cost_sub, total_cost), 2),
        check.names = FALSE
      )
      
      list(
        plan_df = plan_df,
        cost_df = cost_df,
        total_cost = total_cost,
        W = W,
        prod = prod + subcontract + overtime,
        inv = inv
      )
    }
    
    # Core calculation of Fractional and Whole-Worker plans
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
      
      # Net demand vector and Cumulative Net Demand
      net_d <- d
      net_d[1] <- d[1] - i0
      net_d[n] <- net_d[n] + it_target
      cum_net_d <- cumsum(net_d)
      
      allow_sub <- FALSE
      
      # --- 1. Compute Fractional Workforce W_frac ---
      if (plan_type == "chase") {
        W_frac <- numeric(n)
        prev_i <- i0
        for (t in seq_len(n)) {
          target_i <- if (t == n) it_target else 0
          req_prod <- max(0, d[t] + target_i - prev_i)
          W_frac[t] <- req_prod / (K * days[t])
          prev_i <- prev_i + W_frac[t] * K * days[t] - d[t]
        }
        
      } else if (plan_type == "level") {
        cum_k_days <- cumsum(K * days)
        if (input$level_rule == "no_stockout") {
          w_const <- max(0, max(cum_net_d / cum_k_days))
          allow_sub <- FALSE
        } else {
          w_const <- max(0, sum(net_d) / sum(K * days))
          allow_sub <- TRUE
        }
        W_frac <- rep(w_const, n)
        
      } else if (plan_type == "mixed") {
        sel_ch <- sort(unique(c(1, as.integer(input$change_periods))))
        sel_ch <- sel_ch[sel_ch >= 1 & sel_ch <= n]
        if (length(sel_ch) == 0) sel_ch <- 1
        
        W_frac <- numeric(n)
        curr_i <- i0
        num_blocks <- length(sel_ch)
        
        for (b in seq_len(num_blocks)) {
          start_t <- sel_ch[b]
          end_t <- if (b < num_blocks) sel_ch[b + 1] - 1 else n
          idx <- start_t:end_t
          
          block_d <- d[idx]
          block_d[1] <- block_d[1] - curr_i
          if (end_t == n) {
            block_d[length(block_d)] <- block_d[length(block_d)] + it_target
          }
          
          cum_block_req <- cumsum(block_d)
          cum_block_cap <- cumsum(K * days[idx])
          
          if (input$mixed_rule == "no_stockout") {
            w_block <- max(0, max(cum_block_req / cum_block_cap))
            allow_sub <- FALSE
          } else {
            w_block <- max(0, sum(block_d) / sum(K * days[idx]))
            allow_sub <- TRUE
          }
          
          W_frac[idx] <- w_block
          curr_i <- curr_i + sum(w_block * K * days[idx]) - sum(d[idx])
        }
      }
      
      # --- 2. Compute Whole Workers (Rounded-Up) Workforce W_whole ---
      if (input$round_rule == "direct_ceil" || plan_type == "level") {
        W_whole <- ceiling(W_frac - 1e-9)
      } else if (plan_type == "chase") {
        W_whole <- numeric(n)
        prev_i <- i0
        for (t in seq_len(n)) {
          target_i <- if (t == n) it_target else 0
          req_prod <- max(0, d[t] + target_i - prev_i)
          W_whole[t] <- ceiling((req_prod / (K * days[t])) - 1e-9)
          prev_i <- prev_i + W_whole[t] * K * days[t] - d[t]
        }
      } else if (plan_type == "mixed") {
        sel_ch <- sort(unique(c(1, as.integer(input$change_periods))))
        sel_ch <- sel_ch[sel_ch >= 1 & sel_ch <= n]
        if (length(sel_ch) == 0) sel_ch <- 1
        
        W_whole <- numeric(n)
        curr_i <- i0
        num_blocks <- length(sel_ch)
        
        for (b in seq_len(num_blocks)) {
          start_t <- sel_ch[b]
          end_t <- if (b < num_blocks) sel_ch[b + 1] - 1 else n
          idx <- start_t:end_t
          
          block_d <- d[idx]
          block_d[1] <- block_d[1] - curr_i
          if (end_t == n) {
            block_d[length(block_d)] <- block_d[length(block_d)] + it_target
          }
          
          cum_block_req <- cumsum(block_d)
          cum_block_cap <- cumsum(K * days[idx])
          
          if (input$mixed_rule == "no_stockout") {
            w_block <- ceiling(max(0, max(cum_block_req / cum_block_cap)) - 1e-9)
          } else {
            w_block <- ceiling(max(0, sum(block_d) / sum(K * days[idx])) - 1e-9)
          }
          
          W_whole[idx] <- w_block
          curr_i <- curr_i + sum(w_block * K * days[idx]) - sum(d[idx])
        }
      }
      
      eval_frac  <- evaluate_schedule(W_frac, d, days, p_names, K, w0, i0, it_target, allow_sub)
      eval_whole <- evaluate_schedule(W_whole, d, days, p_names, K, w0, i0, it_target, allow_sub)
      
      list(
        periods = p_names,
        n = n,
        K = K,
        cum_net_d = cum_net_d,
        cum_prod_frac = cumsum(eval_frac$prod),
        cum_prod_whole = cumsum(eval_whole$prod),
        frac = eval_frac,
        whole = eval_whole
      )
    })
    
    # Headline text
    output$plan_title_text <- renderText({
      req(results())
      res <- results()
      title_label <- switch(plan_type,
                            "chase" = "Zero-Inventory (Chase) Plan",
                            "level" = "Constant-Workforce (Level) Plan",
                            "mixed" = "Mixed Aggregate Plan")
      paste0(title_label, " — Original Total Cost: $", 
             format(round(res$frac$total_cost, 2), big.mark = ","),
             " | Whole Workers Total Cost: $", 
             format(round(res$whole$total_cost, 2), big.mark = ","))
    })
    
    # Subtitle with K and workforce summary
    output$productivity_info_text <- renderText({
      req(results())
      res <- results()
      k_source <- if (input$k_method == "direct") "Direct input" else "Computed from past data"
      paste0("Productivity K = ", round(res$K, 4), " units/worker/day (", k_source, ") | ",
             "Starting Workforce W0 = ", input$w0, 
             ", Starting Inventory I0 = ", input$i0, 
             ", Target Ending Inventory IT = ", input$it)
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
           main = "Cumulative Production vs. Cumulative Net Demand",
           ylim = y_range)
      axis(1, at = x_idx, labels = res$periods)
      grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
      
      lines(x_idx, res$cum_prod_frac, type = "b", pch = 17, col = "#1f77b4", lwd = 2, lty = 2)
      lines(x_idx, res$cum_prod_whole, type = "b", pch = 15, col = "#d62728", lwd = 2, lty = 1)
      
      legend("topleft",
             legend = c("Cumulative Net Demand", 
                        "Cumulative Production (Original Fractional)", 
                        "Cumulative Production (Whole Workers)"),
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

# Wrapper functions for clean calls in app.R
chase_plan_ui     <- function(id) heuristic_plan_ui(id, "chase")
chase_plan_server <- function(id) heuristic_plan_server(id, "chase")

level_plan_ui     <- function(id) heuristic_plan_ui(id, "level")
level_plan_server <- function(id) heuristic_plan_server(id, "level")

mixed_plan_ui     <- function(id) heuristic_plan_ui(id, "mixed")
mixed_plan_server <- function(id) heuristic_plan_server(id, "mixed")