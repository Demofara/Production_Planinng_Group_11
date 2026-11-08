library(shiny)
library(readxl)

# Reusable UI Function for Box-Jenkins Models ("AR", "MA", "ARMA", "ARIMA")
box_jenkins_ui <- function(id, model_type = c("AR", "MA", "ARMA", "ARIMA")) {
  model_type <- match.arg(model_type)
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
        
        # Parameter selection mode (Manual vs Automatic)
        radioButtons(ns("param_method"),
                     paste0("How should ", model_type, " order parameters be chosen?"),
                     choices = c("I will enter parameters manually" = "manual",
                                 "Find the best parameters (Optimize)" = "auto")),
        
        # --- MANUAL INPUTS ---
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("param_method")),
          if (model_type == "AR") {
            numericInput(ns("p_manual"), "AR order (p)", value = 1, min = 1, max = 12, step = 1)
          } else if (model_type == "MA") {
            numericInput(ns("q_manual"), "MA order (q)", value = 1, min = 1, max = 12, step = 1)
          } else if (model_type == "ARMA") {
            tagList(
              numericInput(ns("p_manual"), "AR order (p)", value = 1, min = 1, max = 10, step = 1),
              numericInput(ns("q_manual"), "MA order (q)", value = 1, min = 1, max = 10, step = 1)
            )
          } else if (model_type == "ARIMA") {
            tagList(
              numericInput(ns("p_manual"), "AR order (p)", value = 1, min = 0, max = 10, step = 1),
              numericInput(ns("d_manual"), "Differencing order (d)", value = 1, min = 0, max = 3, step = 1),
              numericInput(ns("q_manual"), "MA order (q)", value = 1, min = 0, max = 10, step = 1)
            )
          }
        ),
        
        # --- AUTOMATIC OPTIMIZATION INPUTS ---
        conditionalPanel(
          condition = sprintf("input['%s'] == 'auto'", ns("param_method")),
          if (model_type == "AR") {
            numericInput(ns("max_p"), "Maximum AR order (p) to check", value = 5, min = 1, max = 10, step = 1)
          } else if (model_type == "MA") {
            numericInput(ns("max_q"), "Maximum MA order (q) to check", value = 5, min = 1, max = 10, step = 1)
          } else if (model_type == "ARMA") {
            tagList(
              numericInput(ns("max_p"), "Maximum AR order (p) to check", value = 4, min = 1, max = 8, step = 1),
              numericInput(ns("max_q"), "Maximum MA order (q) to check", value = 4, min = 1, max = 8, step = 1)
            )
          } else if (model_type == "ARIMA") {
            tagList(
              numericInput(ns("max_p"), "Maximum AR order (p) to check", value = 3, min = 0, max = 6, step = 1),
              numericInput(ns("max_d"), "Maximum differencing (d) to check", value = 2, min = 1, max = 3, step = 1),
              numericInput(ns("max_q"), "Maximum MA order (q) to check", value = 3, min = 0, max = 6, step = 1)
            )
          },
          selectInput(ns("error_metric"), "Minimize which criterion?",
                      choices = c("MAD", "MSE", "MAPE", "AIC"))
        ),
        
        hr(),
        
        # Forecast horizon and ACF/PACF lag settings
        numericInput(ns("horizon"), "Forecast horizon (future periods)",
                     value = 5, min = 1, max = 24, step = 1),
        numericInput(ns("max_lag"), "Maximum lag for ACF / PACF plots",
                     value = 20, min = 5, max = 40, step = 1)
      ),
      
      mainPanel(
        h4(textOutput(ns("next_period_text"))),
        p(strong(textOutput(ns("model_info_text")))),
        
        h4("ACF and PACF Plots"),
        plotOutput(ns("acf_pacf_plot"), height = "300px"),
        
        h4("Actual Demand and Model Forecast"),
        plotOutput(ns("forecast_plot"), height = "360px"),
        
        h4("Parameter Estimates"),
        tableOutput(ns("param_table")),
        
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

# Reusable Server Function for Box-Jenkins Models
box_jenkins_server <- function(id, model_type = c("AR", "MA", "ARMA", "ARIMA")) {
  model_type <- match.arg(model_type)
  
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
    
    # Populate column choices when file is uploaded
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
    
    # Helper function to safely fit ARIMA(p, d, q) and calculate metrics
    fit_bj_model <- function(d, p, d_order, q, h = 1) {
      n <- length(d)
      
      # Try CSS-ML first, fallback to ML or CSS if numerical issues occur
      fit <- tryCatch(
        stats::arima(d, order = c(p, d_order, q), method = "CSS-ML"),
        error = function(e1) {
          tryCatch(
            stats::arima(d, order = c(p, d_order, q), method = "ML"),
            error = function(e2) {
              tryCatch(
                stats::arima(d, order = c(p, d_order, q), method = "CSS"),
                error = function(e3) NULL
              )
            }
          )
        }
      )
      
      if (is.null(fit)) return(NULL)
      
      # One-step-ahead in-sample fitted values: y_hat_t = y_t - e_t
      resid_vals <- as.numeric(fit$residuals)
      fitted_vals <- as.numeric(d) - resid_vals
      
      # Keep error convention consistent across app: Error (e = F - D)
      error <- fitted_vals - as.numeric(d)
      abs_error <- abs(error)
      
      # Evaluate after differencing initialization periods
      eval_start <- min(n, d_order + 1)
      eval_idx <- eval_start:n
      
      mad <- mean(abs_error[eval_idx], na.rm = TRUE)
      mse <- mean(error[eval_idx]^2, na.rm = TRUE)
      mape <- mean(abs_error[eval_idx] / abs(d[eval_idx]), na.rm = TRUE) * 100
      aic_val <- if (!is.null(fit$aic) && !is.na(fit$aic)) fit$aic else Inf
      
      # Out-of-sample predictions
      pred <- stats::predict(fit, n.ahead = h)
      future_f <- as.numeric(pred$pred)
      future_se <- as.numeric(pred$se)
      
      list(
        fit = fit,
        p = p,
        d_order = d_order,
        q = q,
        forecast = fitted_vals,
        error = error,
        eval_start = eval_start,
        mad = mad,
        mse = mse,
        mape = mape,
        aic = aic_val,
        next_f = future_f[1],
        future_f = future_f,
        lower_95 = future_f - 1.96 * future_se,
        upper_95 = future_f + 1.96 * future_se
      )
    }
    
    # Compute model results based on manual or automatic selection
    results <- reactive({
      req(demand_data())
      d <- demand_data()
      validate(need(length(d) >= 10, "Demand series must contain at least 10 observations for Box-Jenkins modeling."))
      
      h <- max(1, as.integer(input$horizon))
      
      if (input$param_method == "manual") {
        if (model_type == "AR") {
          req(!is.na(input$p_manual))
          p <- max(1, as.integer(input$p_manual))
          d_ord <- 0; q <- 0
        } else if (model_type == "MA") {
          req(!is.na(input$q_manual))
          p <- 0; d_ord <- 0
          q <- max(1, as.integer(input$q_manual))
        } else if (model_type == "ARMA") {
          req(!is.na(input$p_manual), !is.na(input$q_manual))
          p <- max(1, as.integer(input$p_manual))
          d_ord <- 0
          q <- max(1, as.integer(input$q_manual))
        } else {
          req(!is.na(input$p_manual), !is.na(input$d_manual), !is.na(input$q_manual))
          p <- max(0, as.integer(input$p_manual))
          d_ord <- max(0, as.integer(input$d_manual))
          q <- max(0, as.integer(input$q_manual))
          validate(need(p + d_ord + q > 0, "At least one of (p, d, q) must be greater than 0."))
        }
        
        res <- fit_bj_model(d, p, d_ord, q, h)
        validate(need(!is.null(res), "Model failed to converge for the selected parameters. Please try lower orders."))
        return(res)
        
      } else {
        # Automatic grid search over candidate orders
        if (model_type == "AR") {
          max_p <- max(1, as.integer(input$max_p))
          grid <- expand.grid(p = 1:max_p, d = 0, q = 0)
        } else if (model_type == "MA") {
          max_q <- max(1, as.integer(input$max_q))
          grid <- expand.grid(p = 0, d = 0, q = 1:max_q)
        } else if (model_type == "ARMA") {
          max_p <- max(1, as.integer(input$max_p))
          max_q <- max(1, as.integer(input$max_q))
          grid <- expand.grid(p = 1:max_p, d = 0, q = 1:max_q)
        } else {
          max_p <- max(0, as.integer(input$max_p))
          max_d <- max(1, as.integer(input$max_d))
          max_q <- max(0, as.integer(input$max_q))
          grid <- expand.grid(p = 0:max_p, d = 1:max_d, q = 0:max_q)
          grid <- subset(grid, p + q > 0)
        }
        
        best_val <- Inf
        best_res <- NULL
        
        for (i in seq_len(nrow(grid))) {
          cand <- fit_bj_model(d, grid$p[i], grid$d[i], grid$q[i], h)
          if (!is.null(cand)) {
            val <- switch(input$error_metric,
                          "MAD"  = cand$mad,
                          "MSE"  = cand$mse,
                          "MAPE" = cand$mape,
                          "AIC"  = cand$aic)
            if (!is.na(val) && is.finite(val) && val < best_val) {
              best_val <- val
              best_res <- cand
            }
          }
        }
        
        validate(need(!is.null(best_res), "No valid model converged in the search range."))
        return(best_res)
      }
    })
    
    # Helper to format model name nicely
    format_model_label <- function(res) {
      if (model_type == "AR") {
        paste0("AR(", res$p, ")")
      } else if (model_type == "MA") {
        paste0("MA(", res$q, ")")
      } else if (model_type == "ARMA") {
        paste0("ARMA(", res$p, ", ", res$q, ")")
      } else {
        paste0("ARIMA(", res$p, ", ", res$d_order, ", ", res$q, ")")
      }
    }
    
    # Next-period forecast headline
    output$next_period_text <- renderText({
      req(results())
      res <- results()
      paste0(format_model_label(res), " — next-period forecast: ", round(res$next_f, 2))
    })
    
    # Subtitle with selection details and AIC / Sigma^2
    output$model_info_text <- renderText({
      req(results())
      res <- results()
      mode_str <- if (input$param_method == "auto") {
        paste0("Optimized for minimum ", input$error_metric)
      } else {
        "Manual selection"
      }
      aic_str <- if (is.finite(res$aic)) round(res$aic, 2) else "N/A"
      paste0("Selected Model: ", format_model_label(res), " (", mode_str, ") | ",
             "AIC = ", aic_str, " | Residual Variance (σ²) = ", round(res$fit$sigma2, 3))
    })
    
    # Side-by-side ACF and PACF plots
    output$acf_pacf_plot <- renderPlot({
      req(demand_data(), results())
      d <- demand_data()
      res <- results()
      max_lag <- max(5, as.integer(input$max_lag))
      
      # If d > 0 (in ARIMA), show ACF/PACF of the differenced series
      if (res$d_order > 0) {
        series_to_plot <- diff(d, differences = res$d_order)
        series_label <- paste0("Differenced Series (d = ", res$d_order, ")")
      } else {
        series_to_plot <- d
        series_label <- "Original Series"
      }
      
      old_par <- par(mfrow = c(1, 2), mar = c(4.2, 4.2, 3, 1))
      on.exit(par(old_par))
      
      stats::acf(series_to_plot, lag.max = max_lag,
                 main = paste0("ACF — ", series_label),
                 ylab = "Autocorrelation", xlab = "Lag")
      stats::pacf(series_to_plot, lag.max = max_lag,
                  main = paste0("PACF — ", series_label),
                  ylab = "Partial Autocorrelation", xlab = "Lag")
    })
    
    # Main time series forecast plot
    output$forecast_plot <- renderPlot({
      req(results(), demand_data())
      res <- results()
      d <- demand_data()
      n <- length(d)
      h <- length(res$future_f)
      future_periods <- (n + 1):(n + h)
      
      y_range <- range(c(d, res$forecast, res$lower_95, res$upper_95), na.rm = TRUE)
      
      plot(1:n, d, type = "b", pch = 19, col = "black",
           ylab = "Demand", xlab = "Period",
           main = paste0("Actual demand and ", format_model_label(res), " forecast"),
           xlim = c(1, n + h), ylim = y_range)
      
      lines(1:n, res$forecast, type = "b", pch = 17, col = "gray50")
      
      # Shaded 95% prediction interval for future horizon
      polygon(c(future_periods, rev(future_periods)),
              c(res$lower_95, rev(res$upper_95)),
              col = rgb(0.9, 0.2, 0.2, 0.15), border = NA)
      
      lines(c(n, future_periods), c(res$forecast[n], res$future_f),
            type = "b", pch = 17, col = "red", lty = 2)
      points(future_periods, res$future_f, col = "red", pch = 17, cex = 1.2)
      
      legend("topleft",
             legend = c("Actual demand", "In-sample forecast", "Future forecast (with 95% CI)"),
             col = c("black", "gray50", "red"),
             pch = c(19, 17, 17),
             lty = c(1, 1, 2))
    })
    
    # Parameter estimates table (Coefficients, Std. Error, z-stat, p-value)
    output$param_table <- renderTable({
      req(results())
      fit <- results()$fit
      coefs <- coef(fit)
      
      if (length(coefs) == 0) {
        return(data.frame(
          Parameter = "None (Pure random walk / differencing)",
          Estimate = NA,
          `Std. Error` = NA,
          `z value` = NA,
          `p-value` = NA,
          check.names = FALSE
        ))
      }
      
      se <- tryCatch(sqrt(diag(fit$var.coef)), error = function(e) rep(NA, length(coefs)))
      z_val <- coefs / se
      p_val <- 2 * (1 - pnorm(abs(z_val)))
      
      data.frame(
        Parameter = names(coefs),
        Estimate = round(as.numeric(coefs), 4),
        `Std. Error` = round(as.numeric(se), 4),
        `z value` = round(as.numeric(z_val), 3),
        `p-value` = round(as.numeric(p_val), 4),
        check.names = FALSE
      )
    }, digits = 4)
    
    # Error measures table
    output$error_table <- renderTable({
      req(results(), demand_data())
      res <- results()
      eval_str <- paste0(res$eval_start, "-", length(demand_data()))
      
      data.frame(
        Measure = c("MAD", "MSE", "MAPE (%)", "AIC"),
        Value = c(res$mad, res$mse, res$mape, res$aic),
        `Periods evaluated` = c(eval_str, eval_str, eval_str, paste0("1-", length(demand_data()))),
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
        `Forecast` = res$future_f,
        `Lower 95% CI` = res$lower_95,
        `Upper 95% CI` = res$upper_95,
        check.names = FALSE
      )
    })
    
    # Period-by-period in-sample forecast table
    output$forecast_table <- renderTable({
      req(results(), demand_data())
      res <- results()
      d <- demand_data()
      
      data.frame(
        Period = seq_along(d),
        `Demand (D)` = d,
        `Forecast (F)` = res$forecast,
        `Error (e = F - D)` = res$error,
        check.names = FALSE
      )
    }, na = "")
  })
}

# Wrapper functions for clean calls in app.R
ar_model_ui     <- function(id) box_jenkins_ui(id, "AR")
ar_model_server <- function(id) box_jenkins_server(id, "AR")

ma_model_ui     <- function(id) box_jenkins_ui(id, "MA")
ma_model_server <- function(id) box_jenkins_server(id, "MA")

arma_model_ui     <- function(id) box_jenkins_ui(id, "ARMA")
arma_model_server <- function(id) box_jenkins_server(id, "ARMA")

arima_model_ui     <- function(id) box_jenkins_ui(id, "ARIMA")
arima_model_server <- function(id) box_jenkins_server(id, "ARIMA")