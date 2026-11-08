library(shiny)
library(readxl)

# UI Function for Winters' Method (Triple Exponential Smoothing)
winters_method_ui <- function(id) {
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
        
        # Season length (m) and Seasonality type
        numericInput(ns("season_length"), "Season length / cycle periods (m)",
                     value = 12, min = 2, max = 52, step = 1),
        
        radioButtons(ns("seasonal_type"), "Seasonality type:",
                     choices = c("Multiplicative (F = (L + τT) × S)" = "multiplicative",
                                 "Additive (F = L + τT + S)" = "additive")),
        
        hr(),
        
        # Smoothing parameters selection (alpha, beta, gamma)
        radioButtons(ns("param_method"), "How should parameters (α, β, γ) be chosen?",
                     choices = c("I will enter α, β, and γ manually" = "manual",
                                 "Find the best α, β, and γ (Optimize)" = "auto")),
        
        # Manual parameter inputs
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("param_method")),
          numericInput(ns("alpha_manual"), "Level smoothing constant (α) [0-1]",
                       value = 0.2, min = 0.01, max = 0.99, step = 0.05),
          numericInput(ns("beta_manual"), "Trend smoothing constant (β) [0-1]",
                       value = 0.1, min = 0.01, max = 0.99, step = 0.05),
          numericInput(ns("gamma_manual"), "Seasonal smoothing constant (γ) [0-1]",
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
                     value = 12, min = 1, max = 48, step = 1)
      ),
      
      mainPanel(
        h4(textOutput(ns("next_period_text"))),
        p(strong(textOutput(ns("param_info_text")))),
        
        h4("Actual Demand and Winters' Method Forecast"),
        plotOutput(ns("plot"), height = "360px"),
        
        h4("Initial and Final Seasonal Factors"),
        tableOutput(ns("seasonal_factors_table")),
        
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

# Server Function for Winters' Method
winters_method_server <- function(id) {
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
    
    # Helper function to initialize L0, T0, and initial seasonal indices S[1..m]
    init_winters <- function(d, m, seasonal_type) {
      n <- length(d)
      t_idx <- seq_len(n)
      season_idx <- ((t_idx - 1) %% m) + 1
      
      # Initial trend approximation to extract raw seasonal indices
      fit_raw <- lm(d ~ t_idx)
      trend_raw <- fitted(fit_raw)
      
      if (seasonal_type == "multiplicative") {
        raw_s <- d / trend_raw
        s_init <- sapply(seq_len(m), function(s) mean(raw_s[season_idx == s], na.rm = TRUE))
        s_init <- s_init * (m / sum(s_init))
        deseason <- d / s_init[season_idx]
      } else {
        raw_s <- d - trend_raw
        s_init <- sapply(seq_len(m), function(s) mean(raw_s[season_idx == s], na.rm = TRUE))
        s_init <- s_init - mean(s_init)
        deseason <- d - s_init[season_idx]
      }
      
      # Fit linear regression on deseasonalized demand for L0 and T0
      fit_deseason <- lm(deseason ~ t_idx)
      l0 <- unname(coef(fit_deseason)[1])
      t0 <- unname(coef(fit_deseason)[2])
      
      list(l0 = l0, t0 = t0, s_init = s_init, season_idx = season_idx)
    }
    
    # Helper function to run Winters' Triple Exponential Smoothing recursions
    calc_winters <- function(d, m, alpha, beta, gamma, init_vals, seasonal_type, h = 1) {
      n <- length(d)
      l0 <- init_vals$l0
      t0 <- init_vals$t0
      s_init <- init_vals$s_init
      
      level <- numeric(n)
      trend <- numeric(n)
      S_vec <- numeric(n + m)
      S_vec[1:m] <- s_init
      used_s <- numeric(n)
      updated_s <- numeric(n)
      forecast <- numeric(n)
      
      prev_l <- l0
      prev_t <- t0
      
      for (t in seq_len(n)) {
        s_prev <- S_vec[t]
        used_s[t] <- s_prev
        
        if (seasonal_type == "multiplicative") {
          forecast[t] <- (prev_l + prev_t) * s_prev
          curr_l <- alpha * (d[t] / s_prev) + (1 - alpha) * (prev_l + prev_t)
          curr_t <- beta * (curr_l - prev_l) + (1 - beta) * prev_t
          curr_s <- gamma * (d[t] / curr_l) + (1 - gamma) * s_prev
        } else {
          forecast[t] <- prev_l + prev_t + s_prev
          curr_l <- alpha * (d[t] - s_prev) + (1 - alpha) * (prev_l + prev_t)
          curr_t <- beta * (curr_l - prev_l) + (1 - beta) * prev_t
          curr_s <- gamma * (d[t] - curr_l) + (1 - gamma) * s_prev
        }
        
        level[t] <- curr_l
        trend[t] <- curr_t
        S_vec[t + m] <- curr_s
        updated_s[t] <- curr_s
        
        prev_l <- curr_l
        prev_t <- curr_t
      }
      
      error <- forecast - d
      abs_error <- abs(error)
      mad <- mean(abs_error, na.rm = TRUE)
      mse <- mean(error^2, na.rm = TRUE)
      mape <- mean(abs_error / abs(d), na.rm = TRUE) * 100
      
      # Future forecasts for horizon tau = 1, ..., h
      future_t <- (n + 1):(n + h)
      future_season <- ((future_t - 1) %% m) + 1
      # Latest available seasonal factor for each future step
      latest_cycle_s <- S_vec[(n + 1):(n + m)]
      future_s <- latest_cycle_s[((seq_len(h) - 1) %% m) + 1]
      
      if (seasonal_type == "multiplicative") {
        future_f <- (level[n] + (1:h) * trend[n]) * future_s
      } else {
        future_f <- level[n] + (1:h) * trend[n] + future_s
      }
      
      # Organize final seasonal indices by season 1..m
      final_s_by_season <- numeric(m)
      for (k in seq_len(m)) {
        idx_last <- n - m + k
        season_num <- ((idx_last - 1) %% m) + 1
        final_s_by_season[season_num] <- S_vec[idx_last + m]
      }
      
      list(
        m = m,
        alpha = alpha,
        beta = beta,
        gamma = gamma,
        l0 = l0,
        t0 = t0,
        s_init = s_init,
        final_s = final_s_by_season,
        season_idx = init_vals$season_idx,
        level = level,
        trend = trend,
        used_s = used_s,
        updated_s = updated_s,
        forecast = forecast,
        error = error,
        mad = mad,
        mse = mse,
        mape = mape,
        next_f = future_f[1],
        future_t = future_t,
        future_season = future_season,
        future_s = future_s,
        future_f = future_f
      )
    }
    
    # Compute model results based on user inputs
    results <- reactive({
      req(demand_data(), !is.na(input$season_length))
      d <- demand_data()
      n <- length(d)
      m <- max(2, as.integer(input$season_length))
      h <- max(1, as.integer(input$horizon))
      s_type <- input$seasonal_type
      
      validate(
        need(n >= 2 * m, paste0("Demand series must contain at least 2 full seasonal cycles (", 2 * m, " periods)."))
      )
      
      init_vals <- init_winters(d, m, s_type)
      
      if (input$param_method == "manual") {
        req(!is.na(input$alpha_manual), !is.na(input$beta_manual), !is.na(input$gamma_manual))
        validate(
          need(input$alpha_manual > 0 && input$alpha_manual < 1 &&
                 input$beta_manual > 0 && input$beta_manual < 1 &&
                 input$gamma_manual > 0 && input$gamma_manual < 1,
               "Please enter α, β, and γ values strictly between 0 and 1.")
        )
        return(calc_winters(d, m, input$alpha_manual, input$beta_manual, input$gamma_manual,
                            init_vals, s_type, h))
        
      } else {
        # Automatic optimization of (alpha, beta, gamma)
        obj_fn <- function(par) {
          res <- calc_winters(d, m, par[1], par[2], par[3], init_vals, s_type, h = 1)
          switch(input$error_metric,
                 "MAD"  = res$mad,
                 "MSE"  = res$mse,
                 "MAPE" = res$mape)
        }
        
        # Coarse grid search for a reliable starting point
        grid_pts <- c(0.1, 0.3, 0.5, 0.7)
        grid <- expand.grid(a = grid_pts, b = grid_pts, g = grid_pts)
        grid_vals <- apply(grid, 1, obj_fn)
        best_start <- as.numeric(grid[which.min(grid_vals), ])
        
        opt <- optim(
          par = best_start,
          fn = obj_fn,
          method = "L-BFGS-B",
          lower = c(0.001, 0.001, 0.001),
          upper = c(0.999, 0.999, 0.999)
        )
        
        best_a <- opt$par[1]
        best_b <- opt$par[2]
        best_g <- opt$par[3]
        
        return(calc_winters(d, m, best_a, best_b, best_g, init_vals, s_type, h))
      }
    })
    
    # Next-period forecast headline
    output$next_period_text <- renderText({
      req(results())
      res <- results()
      paste0("Winters' Method (α = ", round(res$alpha, 3),
             ", β = ", round(res$beta, 3),
             ", γ = ", round(res$gamma, 3),
             ") — next-period forecast: ", round(res$next_f, 2))
    })
    
    # Parameter summary subtitle
    output$param_info_text <- renderText({
      req(results())
      res <- results()
      mode_label <- if (input$param_method == "auto") {
        paste0("Optimized for minimum ", input$error_metric)
      } else {
        "Manual input"
      }
      paste0("Initial Level L0 = ", round(res$l0, 2),
             ", Initial Trend T0 = ", round(res$t0, 3), " | ",
             "Parameters: α = ", round(res$alpha, 4),
             ", β = ", round(res$beta, 4),
             ", γ = ", round(res$gamma, 4),
             " (", mode_label, ")")
    })
    
    # Main forecast plot
    output$plot <- renderPlot({
      req(results(), demand_data())
      res <- results()
      d <- demand_data()
      n <- length(d)
      h <- length(res$future_f)
      future_periods <- res$future_t
      
      plot(1:n, d, type = "b", pch = 19, col = "black",
           ylab = "Demand", xlab = "Period",
           main = paste0("Actual demand and Winters' Method (α=", round(res$alpha, 2),
                         ", β=", round(res$beta, 2), ", γ=", round(res$gamma, 2), ") forecast"),
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
    
    # Initial and final seasonal factors table
    output$seasonal_factors_table <- renderTable({
      req(results())
      res <- results()
      data.frame(
        `Season (s)` = seq_len(res$m),
        `Initial Seasonal Factor (S0)` = round(res$s_init, 4),
        `Final Seasonal Factor (Sn)` = round(res$final_s, 4),
        check.names = FALSE
      )
    }, digits = 4)
    
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
      req(results())
      res <- results()
      
      data.frame(
        `Future Period` = res$future_t,
        `Horizon (τ)` = seq_along(res$future_t),
        `Season` = res$future_season,
        `Seasonal Factor (S)` = round(res$future_s, 4),
        `Forecast (F)` = res$future_f,
        check.names = FALSE
      )
    })
    
    # Period-by-period table
    output$forecast_table <- renderTable({
      req(results(), demand_data())
      res <- results()
      d <- demand_data()
      
      data.frame(
        Period = seq_along(d),
        Season = res$season_idx,
        `Demand (D)` = d,
        `Level (L)` = res$level,
        `Trend (T)` = res$trend,
        `Seasonal Used (S_t-m)` = round(res$used_s, 4),
        `Seasonal Updated (S_t)` = round(res$updated_s, 4),
        `Forecast (F)` = res$forecast,
        `Error (e = F - D)` = res$error,
        check.names = FALSE
      )
    }, na = "")
  })
}