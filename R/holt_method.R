library(shiny)
library(readxl)

# UI Function for Holt's Method (Double Exponential Smoothing)
holt_method_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        # Data upload (.xlsx, .xls, or .csv)
        fileInput(ns("file"), "Upload an Excel or CSV file (.xlsx, .xls, .csv)", 
                  accept = c(".xlsx", ".xls", ".csv")),
        
        # Series / column selection
        selectInput(ns("demand_col"), "Demand column", choices = NULL),
        
        hr(),
        
        # Initial values (L0 and T0) selection
        radioButtons(ns("init_method"), "Initial level (L0) and slope (T0):",
                     choices = c("Fit linear regression automatically" = "regression",
                                 "Enter level and slope manually" = "manual")),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("init_method")),
          numericInput(ns("l0_manual"), "Initial level (L0)", value = 180),
          numericInput(ns("t0_manual"), "Initial slope / trend (T0)", value = 3)
        ),
        
        hr(),
        
        # Parameter selection method (alpha and beta)
        radioButtons(ns("param_method"), "How should parameters (α, β) be chosen?",
                     choices = c("I will enter α and β manually" = "manual",
                                 "Find the best α and β (Optimize)" = "auto")),
        
        # Manual alpha and beta inputs
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("param_method")),
          numericInput(ns("alpha_manual"), "Level smoothing constant (α) [0-1]", 
                       value = 0.2, min = 0.01, max = 0.99, step = 0.05),
          numericInput(ns("beta_manual"), "Trend smoothing constant (β) [0-1]", 
                       value = 0.1, min = 0.01, max = 0.99, step = 0.05)
        ),
        
        # Automatic optimization inputs
        conditionalPanel(
          condition = sprintf("input['%s'] == 'auto'", ns("param_method")),
          selectInput(ns("error_metric"), "Minimize which error?",
                      choices = c("MAD", "MSE", "MAPE"))
        ),
        
        hr(),
        
        # Forecast horizon
        numericInput(ns("horizon"), "Forecast horizon (future periods)", 
                     value = 1, min = 1, max = 24, step = 1)
      ),
      mainPanel(
        h4(textOutput(ns("next_period_text"))),
        p(strong(textOutput(ns("param_info_text")))),
        plotOutput(ns("plot")),
        
        h4("Error measures"),
        tableOutput(ns("error_table")),
        
        h4("Future forecasts"),
        tableOutput(ns("future_table")),
        
        h4("Forecasts by period"),
        tableOutput(ns("forecast_table"))
      )
    )
  )
}

# Server Function for Holt's Method
holt_method_server <- function(id) {
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
    
    # Populate column selector when file is uploaded
    observeEvent(uploaded_data(), {
      df <- uploaded_data()
      updateSelectInput(session, "demand_col", choices = names(df))
    })
    
    # Extract selected numeric demand series
    demand_data <- reactive({
      req(uploaded_data(), input$demand_col)
      df <- uploaded_data()
      req(input$demand_col %in% names(df))
      d <- as.numeric(df[[input$demand_col]])
      d[!is.na(d)]
    })
    
    # Helper function to compute Holt's Double Exponential Smoothing
    calc_holt <- function(d, alpha, beta, l0, t0, h = 1) {
      n <- length(d)
      level <- numeric(n)
      trend <- numeric(n)
      forecast <- numeric(n)
      
      prev_l <- l0
      prev_t <- t0
      
      for (t in 1:n) {
        forecast[t] <- prev_l + prev_t
        curr_l <- alpha * d[t] + (1 - alpha) * forecast[t]
        curr_t <- beta * (curr_l - prev_l) + (1 - beta) * prev_t
        
        level[t] <- curr_l
        trend[t] <- curr_t
        
        prev_l <- curr_l
        prev_t <- curr_t
      }
      
      error <- forecast - d
      abs_error <- abs(error)
      
      mad <- mean(abs_error, na.rm = TRUE)
      mse <- mean(error^2, na.rm = TRUE)
      mape <- mean(abs_error / d, na.rm = TRUE) * 100
      
      # Future forecasts for t = n + 1, ..., n + h
      future_steps <- 1:h
      future_forecasts <- level[n] + future_steps * trend[n]
      
      list(
        level = level,
        trend = trend,
        forecast = forecast,
        error = error,
        mad = mad,
        mse = mse,
        mape = mape,
        next_f = future_forecasts[1],
        future_f = future_forecasts,
        alpha = alpha,
        beta = beta,
        l0 = l0,
        t0 = t0
      )
    }
    
    # Compute results based on user selections
    results <- reactive({
      req(demand_data())
      d <- demand_data()
      validate(need(length(d) >= 3, "Demand series must contain at least 3 periods."))
      
      h <- max(1, as.integer(input$horizon))
      
      # Determine initial level (L0) and initial slope (T0)
      if (input$init_method == "regression") {
        t_idx <- seq_along(d)
        fit <- lm(d ~ t_idx)
        l0 <- unname(coef(fit)[1])
        t0 <- unname(coef(fit)[2])
      } else {
        req(!is.na(input$l0_manual), !is.na(input$t0_manual))
        l0 <- input$l0_manual
        t0 <- input$t0_manual
      }
      
      # Determine smoothing constants (alpha and beta)
      if (input$param_method == "manual") {
        req(!is.na(input$alpha_manual), !is.na(input$beta_manual))
        validate(
          need(input$alpha_manual > 0 && input$alpha_manual < 1 &&
                 input$beta_manual > 0 && input$beta_manual < 1,
               "Please enter alpha (α) and beta (β) values strictly between 0 and 1.")
        )
        return(calc_holt(d, input$alpha_manual, input$beta_manual, l0, t0, h))
        
      } else {
        # Objective function for automatic optimization
        obj_fn <- function(par) {
          a <- par[1]
          b <- par[2]
          res <- calc_holt(d, a, b, l0, t0, h = 1)
          switch(input$error_metric,
                 "MAD" = res$mad,
                 "MSE" = res$mse,
                 "MAPE" = res$mape)
        }
        
        # Coarse grid search to find a reliable starting point
        grid_seq <- seq(0.05, 0.95, by = 0.05)
        grid <- expand.grid(a = grid_seq, b = grid_seq)
        grid_vals <- apply(grid, 1, obj_fn)
        best_start <- as.numeric(grid[which.min(grid_vals), ])
        
        # Refine with bounded optimization
        opt <- optim(
          par = best_start,
          fn = obj_fn,
          method = "L-BFGS-B",
          lower = c(0.001, 0.001),
          upper = c(0.999, 0.999)
        )
        
        best_a <- opt$par[1]
        best_b <- opt$par[2]
        
        return(calc_holt(d, best_a, best_b, l0, t0, h))
      }
    })
    
    # Headline text for next period forecast
    output$next_period_text <- renderText({
      req(results())
      res <- results()
      paste0("Holt's Method (α = ", round(res$alpha, 4), 
             ", β = ", round(res$beta, 4), 
             ") — next-period forecast: ", round(res$next_f, 2))
    })
    
    # Subtitle text for initial values and parameter selection
    output$param_info_text <- renderText({
      req(results())
      res <- results()
      init_label <- if (input$init_method == "regression") "Linear regression" else "Manual input"
      param_label <- if (input$param_method == "auto") {
        paste0("Optimized for minimum ", input$error_metric)
      } else {
        "Manual input"
      }
      paste0("Initial Level L0 = ", round(res$l0, 2), 
             ", Initial Slope T0 = ", round(res$t0, 2), " (", init_label, ") | ",
             "Parameters: α = ", round(res$alpha, 4), ", β = ", round(res$beta, 4), 
             " (", param_label, ")")
    })
    
    # Plot actual demand, fitted forecast, and future forecasts
    output$plot <- renderPlot({
      req(results(), demand_data())
      res <- results()
      d <- demand_data()
      n <- length(d)
      h <- length(res$future_f)
      future_periods <- (n + 1):(n + h)
      
      plot(1:n, d, type = "b", pch = 19, col = "black",
           ylab = "Demand", xlab = "Period",
           main = paste0("Actual demand and Holt's Method (α = ", round(res$alpha, 3), 
                         ", β = ", round(res$beta, 3), ") forecast"),
           xlim = c(1, n + h),
           ylim = range(c(d, res$forecast, res$future_f), na.rm = TRUE))
      
      lines(1:n, res$forecast, type = "b", pch = 17, col = "gray50")
      lines(c(n, future_periods), c(res$forecast[n], res$future_f), 
            type = "b", pch = 17, col = "red", lty = 2)
      points(future_periods, res$future_f, col = "red", pch = 17, cex = 1.2)
      
      legend("topleft",
             legend = c("Actual demand", "In-sample forecast", "Future forecast"),
             col = c("black", "gray50", "red"),
             pch = c(19, 17, 17),
             lty = c(1, 1, 2))
    })
    
    # Error measures table
    output$error_table <- renderTable({
      req(results(), demand_data())
      res <- results()
      eval_str <- paste0("1-", length(demand_data()))
      
      data.frame(
        Measure = c("MAD", "MSE", "MAPE (%)"),
        Value = c(res$mad, res$mse, res$mape),
        `Periods evaluated` = c(eval_str, eval_str, eval_str),
        check.names = FALSE
      )
    })
    
    # Future forecasts table
    output$future_table <- renderTable({
      req(results(), demand_data())
      res <- results()
      n <- length(demand_data())
      h <- length(res$future_f)
      
      data.frame(
        `Future Period` = (n + 1):(n + h),
        `Horizon (τ)` = 1:h,
        `Forecast (F = Ln + τ * Tn)` = res$future_f,
        check.names = FALSE
      )
    })
    
    # Period-by-period table (showing Level, Trend, Forecast, Error)
    output$forecast_table <- renderTable({
      req(results(), demand_data())
      res <- results()
      d <- demand_data()
      
      data.frame(
        Period = 1:length(d),
        `Demand (D)` = d,
        `Level (L)` = res$level,
        `Trend (T)` = res$trend,
        `Forecast (F)` = res$forecast,
        `Error (e = F - D)` = res$error,
        check.names = FALSE
      )
    }, na = "")
  })
}