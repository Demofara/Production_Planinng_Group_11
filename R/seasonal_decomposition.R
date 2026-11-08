library(shiny)
library(readxl)

# UI Function for Seasonal Decomposition with Moving Averages
seasonal_decomposition_ui <- function(id) {
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
        
        # Season length (m) and Decomposition type
        numericInput(ns("season_length"), "Season length / cycle periods (m)",
                     value = 12, min = 2, max = 52, step = 1),
        
        radioButtons(ns("decomp_type"), "Decomposition model type:",
                     choices = c("Multiplicative (D = T × S × I)" = "multiplicative",
                                 "Additive (D = T + S + I)" = "additive")),
        
        radioButtons(ns("trend_fit_on"), "Fit linear trend regression on:",
                     choices = c("Deseasonalized demand series" = "deseasonalized",
                                 "Centered Moving Average (CMA)" = "cma")),
        
        hr(),
        
        # Forecast horizon
        numericInput(ns("horizon"), "Forecast horizon (future periods)",
                     value = 12, min = 1, max = 48, step = 1)
      ),
      
      mainPanel(
        h4(textOutput(ns("next_period_text"))),
        p(strong(textOutput(ns("model_info_text")))),
        
        h4("Actual Demand, Trend, and Seasonal Forecast"),
        plotOutput(ns("forecast_plot"), height = "350px"),
        
        h4("Normalized Seasonal Indices by Season"),
        plotOutput(ns("seasonal_plot"), height = "260px"),
        
        h4("Seasonal Indices Table"),
        tableOutput(ns("seasonal_table")),
        
        h4("Error measures"),
        tableOutput(ns("error_table")),
        
        h4("Future forecasts"),
        tableOutput(ns("future_table")),
        
        h4("Decomposition and Forecasts by Period"),
        tableOutput(ns("forecast_table"))
      )
    )
  )
}

# Server Function for Seasonal Decomposition with Moving Averages
seasonal_decomposition_server <- function(id) {
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
    
    # Update column choices when file is uploaded
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
    
    # Perform Moving Average Seasonal Decomposition
    results <- reactive({
      req(demand_data(), !is.na(input$season_length))
      d <- demand_data()
      n <- length(d)
      m <- max(2, as.integer(input$season_length))
      h <- max(1, as.integer(input$horizon))
      
      validate(
        need(n >= 2 * m, paste0("Demand series must contain at least 2 full seasonal cycles (", 2 * m, " periods)."))
      )
      
      # 1. Calculate Centered Moving Average (CMA)
      cma <- rep(NA_real_, n)
      if (m %% 2 == 1) {
        # Odd season length: simple symmetric m-MA
        k <- (m - 1) / 2
        for (t in (k + 1):(n - k)) {
          cma[t] <- mean(d[(t - k):(t + k)])
        }
      } else {
        # Even season length: 2 x m centered moving average
        half <- m / 2
        for (t in (half + 1):(n - half)) {
          ma1 <- mean(d[(t - half):(t + half - 1)])
          ma2 <- mean(d[(t - half + 1):(t + half)])
          cma[t] <- 0.5 * (ma1 + ma2)
        }
      }
      
      # 2. Raw seasonal ratios / differences
      season_idx <- ((seq_len(n) - 1) %% m) + 1
      if (input$decomp_type == "multiplicative") {
        raw_s <- d / cma
      } else {
        raw_s <- d - cma
      }
      
      # 3. Average by season and normalize seasonal indices
      s_bar <- numeric(m)
      for (s in seq_len(m)) {
        s_bar[s] <- mean(raw_s[season_idx == s], na.rm = TRUE)
      }
      
      if (input$decomp_type == "multiplicative") {
        s_bar <- s_bar * (m / sum(s_bar))
        s_vec <- s_bar[season_idx]
        deseason <- d / s_vec
      } else {
        s_bar <- s_bar - mean(s_bar)
        s_vec <- s_bar[season_idx]
        deseason <- d - s_vec
      }
      
      # 4. Fit linear trend line (T_t = L0 + T0 * t)
      t_idx <- seq_len(n)
      if (input$trend_fit_on == "deseasonalized") {
        fit <- lm(deseason ~ t_idx)
      } else {
        fit <- lm(cma ~ t_idx)
      }
      l0 <- unname(coef(fit)[1])
      t0 <- unname(coef(fit)[2])
      trend_line <- l0 + t0 * t_idx
      
      # 5. In-sample forecasts and error metrics
      if (input$decomp_type == "multiplicative") {
        forecast <- trend_line * s_vec
      } else {
        forecast <- trend_line + s_vec
      }
      
      error <- forecast - d
      abs_error <- abs(error)
      mad <- mean(abs_error, na.rm = TRUE)
      mse <- mean(error^2, na.rm = TRUE)
      mape <- mean(abs_error / abs(d), na.rm = TRUE) * 100
      
      # 6. Future forecasts for t = n + 1, ..., n + h
      future_t <- (n + 1):(n + h)
      future_season <- ((future_t - 1) %% m) + 1
      future_trend <- l0 + t0 * future_t
      future_s <- s_bar[future_season]
      
      if (input$decomp_type == "multiplicative") {
        future_f <- future_trend * future_s
      } else {
        future_f <- future_trend + future_s
      }
      
      list(
        m = m,
        cma = cma,
        raw_s = raw_s,
        s_bar = s_bar,
        s_vec = s_vec,
        season_idx = season_idx,
        deseason = deseason,
        l0 = l0,
        t0 = t0,
        trend_line = trend_line,
        forecast = forecast,
        error = error,
        mad = mad,
        mse = mse,
        mape = mape,
        next_f = future_f[1],
        future_t = future_t,
        future_season = future_season,
        future_trend = future_trend,
        future_s = future_s,
        future_f = future_f
      )
    })
    
    # Next-period forecast headline
    output$next_period_text <- renderText({
      req(results())
      res <- results()
      paste0("Seasonal Decomposition (m = ", res$m, ") — next-period forecast: ", round(res$next_f, 2))
    })
    
    # Model summary text
    output$model_info_text <- renderText({
      req(results())
      res <- results()
      type_str <- if (input$decomp_type == "multiplicative") "Multiplicative" else "Additive"
      paste0("Model: ", type_str, " Decomposition (m = ", res$m, ") | ",
             "Linear Trend: T(t) = ", round(res$l0, 2), " + ", round(res$t0, 4), " × t")
    })
    
    # Main time series plot
    output$forecast_plot <- renderPlot({
      req(results(), demand_data())
      res <- results()
      d <- demand_data()
      n <- length(d)
      h <- length(res$future_f)
      future_periods <- res$future_t
      
      plot(1:n, d, type = "b", pch = 19, col = "black",
           ylab = "Demand", xlab = "Period",
           main = paste0("Actual Demand, Trend, and Seasonal Decomposition Forecast (m = ", res$m, ")"),
           xlim = c(1, n + h),
           ylim = range(c(d, res$forecast, res$future_f, res$trend_line), na.rm = TRUE))
      
      lines(1:n, res$trend_line, col = "steelblue", lwd = 2, lty = 3)
      lines(1:n, res$forecast, type = "b", pch = 17, col = "gray50")
      lines(c(n, future_periods), c(res$forecast[n], res$future_f),
            type = "b", pch = 17, col = "red", lty = 2)
      
      legend("topleft",
             legend = c("Actual demand", "Trend line", "In-sample forecast", "Future forecast"),
             col = c("black", "steelblue", "gray50", "red"),
             pch = c(19, NA, 17, 17),
             lty = c(1, 3, 1, 2))
    })
    
    # Bar plot of normalized seasonal indices
    output$seasonal_plot <- renderPlot({
      req(results())
      res <- results()
      ref_line <- if (input$decomp_type == "multiplicative") 1 else 0
      
      bp <- barplot(res$s_bar,
                    names.arg = paste0("S", seq_len(res$m)),
                    col = "steelblue", border = "white",
                    ylab = "Seasonal Index", xlab = "Season within Cycle",
                    main = paste0("Normalized Seasonal Indices (Reference = ", ref_line, ")"))
      abline(h = ref_line, col = "red", lty = 2, lwd = 2)
    })
    
    # Seasonal indices table
    output$seasonal_table <- renderTable({
      req(results())
      res <- results()
      data.frame(
        `Season (s)` = seq_len(res$m),
        `Normalized Seasonal Index (S)` = round(res$s_bar, 4),
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
        `Season` = res$future_season,
        `Projected Trend (T)` = res$future_trend,
        `Seasonal Index (S)` = round(res$future_s, 4),
        `Forecast (F)` = res$future_f,
        check.names = FALSE
      )
    })
    
    # Period-by-period decomposition table
    output$forecast_table <- renderTable({
      req(results(), demand_data())
      res <- results()
      d <- demand_data()
      
      data.frame(
        Period = seq_along(d),
        Season = res$season_idx,
        `Demand (D)` = d,
        `Centered MA (CMA)` = res$cma,
        `Seasonal Index (S)` = round(res$s_vec, 4),
        `Deseasonalized` = res$deseason,
        `Trend (T)` = res$trend_line,
        `Forecast (F)` = res$forecast,
        `Error (e = F - D)` = res$error,
        check.names = FALSE
      )
    }, na = "")
  })
}