library(shiny)
library(readxl)

# UI Function for Seasonal Factors (Quick and Dirty Method)
seasonal_factors_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload an Excel (.xlsx) or CSV (.csv) file", 
                  accept = c(".xlsx", ".csv")),
        
        selectInput(ns("demand_col"), "Select series (Demand column)", choices = NULL),
        
        hr(),
        
        radioButtons(ns("m_method"), "How should season length (L) be chosen?",
                     choices = c("Enter L manually" = "manual",
                                 "Find the best L (Optimize)" = "auto")),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("m_method")),
          numericInput(ns("m_manual"), "Season length L (e.g., 4 for quarterly, 12 for monthly)", 
                       value = 4, min = 2, step = 1)
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'auto'", ns("m_method")),
          numericInput(ns("m_max"), "Maximum season length (L) to check", 
                       value = 12, min = 2, step = 1),
          selectInput(ns("error_metric"), "Minimize which error?", 
                      choices = c("MAD", "MSE", "MAPE"))
        ),
        
        numericInput(ns("horizon"), "Forecast horizon (future periods)", 
                     value = 4, min = 1, step = 1)
      ),
      mainPanel(
        h4(textOutput(ns("summary_text"))),
        p(strong(textOutput(ns("mean_info_text")))),
        plotOutput(ns("plot")),
        
        fluidRow(
          column(6,
                 h4("Error measures"),
                 tableOutput(ns("error_table"))
          ),
          column(6,
                 h4("Seasonal factors (Indices)"),
                 tableOutput(ns("factors_table"))
          )
        ),
        
        h4("Forecasts by period"),
        tableOutput(ns("forecast_table"))
      )
    )
  )
}

# Server Function for Seasonal Factors (Quick and Dirty Method)
seasonal_factors_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Read uploaded Excel or CSV file
    uploaded_data <- reactive({
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      if (ext == "xlsx") {
        read_excel(input$file$datapath)
      } else if (ext == "csv") {
        read.csv(input$file$datapath, check.names = FALSE)
      } else {
        validate("Please upload a valid .xlsx or .csv file.")
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
    
    # Helper function for Quick and Dirty Seasonal Factors calculation
    calc_sf <- function(d, L, h = 1) {
      len <- length(d)
      overall_mean <- mean(d, na.rm = TRUE)
      
      # Assign each observation to its season (1 to L)
      season_idx <- ((seq_len(len) - 1) %% L) + 1
      
      # Ratio to overall mean for each period: c_t = D_t / D_bar
      ratios <- d / overall_mean
      
      # Average seasonal factor for each season j = 1..L
      raw_factors <- sapply(1:L, function(s) mean(ratios[season_idx == s], na.rm = TRUE))
      
      # Normalize factors so that their sum equals L (average = 1)
      seasonal_factors <- raw_factors * (L / sum(raw_factors))
      
      # Fitted values (in-sample forecast): F_t = D_bar * c_season(t)
      forecast <- overall_mean * seasonal_factors[season_idx]
      
      error <- forecast - d
      abs_error <- abs(error)
      
      mad <- mean(abs_error, na.rm = TRUE)
      mse <- mean(error^2, na.rm = TRUE)
      mape <- mean((abs_error / d), na.rm = TRUE) * 100
      
      # Future forecasts for horizon h
      future_periods <- (len + 1):(len + h)
      future_season_idx <- ((future_periods - 1) %% L) + 1
      future_f <- overall_mean * seasonal_factors[future_season_idx]
      
      list(
        forecast = forecast,
        error = error,
        mad = mad,
        mse = mse,
        mape = mape,
        overall_mean = overall_mean,
        seasonal_factors = seasonal_factors,
        season_idx = season_idx,
        future_periods = future_periods,
        future_season_idx = future_season_idx,
        future_f = future_f,
        L = L
      )
    }
    
    # Compute results based on manual or automatic L selection
    results <- reactive({
      req(demand_data())
      d <- demand_data()
      req(length(d) >= 4)
      h <- max(1, as.integer(input$horizon))
      
      if (input$m_method == "manual") {
        req(!is.na(input$m_manual))
        L <- as.integer(input$m_manual)
        validate(
          need(L >= 2 && L <= floor(length(d) / 2),
               paste0("Season length (L) must be between 2 and ", floor(length(d) / 2), "."))
        )
        return(calc_sf(d, L, h))
      } else {
        req(!is.na(input$m_max))
        max_L <- min(as.integer(input$m_max), floor(length(d) / 2))
        validate(
          need(max_L >= 2, "Maximum L must be at least 2.")
        )
        
        best_err <- Inf
        best_res <- NULL
        
        for (L_cand in 2:max_L) {
          res <- calc_sf(d, L_cand, h)
          err_val <- switch(input$error_metric,
                            "MAD" = res$mad,
                            "MSE" = res$mse,
                            "MAPE" = res$mape)
          if (!is.na(err_val) && err_val < best_err) {
            best_err <- err_val
            best_res <- res
          }
        }
        return(best_res)
      }
    })
    
    output$summary_text <- renderText({
      req(results())
      res <- results()
      paste0("Seasonal Factors (L = ", res$L, ") — Next-period (Period ", 
             res$future_periods[1], ") forecast: ", round(res$future_f[1], 2))
    })
    
    output$mean_info_text <- renderText({
      req(results())
      res <- results()
      mode_txt <- if (input$m_method == "auto") {
        paste0("Optimized for minimum ", input$error_metric)
      } else {
        "Manual selection"
      }
      paste0("Overall Stationary Mean (Level) = ", round(res$overall_mean, 2), 
             " | Season Length L = ", res$L, " (", mode_txt, ")")
    })
    
    output$plot <- renderPlot({
      req(results(), demand_data())
      res <- results()
      d <- demand_data()
      len <- length(d)
      
      all_x <- c(1:len, res$future_periods)
      all_y <- c(d, res$forecast, res$future_f)
      
      plot(1:len, d, type = "b", pch = 19, col = "black",
           ylab = "Demand", xlab = "Period",
           main = paste0("Actual Demand and Seasonal Factors (L = ", res$L, ") Forecast"),
           xlim = range(all_x),
           ylim = range(all_y, na.rm = TRUE))
      
      lines(1:len, res$forecast, type = "b", pch = 17, col = "gray40")
      lines(c(len, res$future_periods), c(res$forecast[len], res$future_f), 
            type = "b", pch = 17, col = "red", lty = 2)
      
      legend("bottomleft",
             legend = c("Actual demand", "In-sample forecast", "Future forecast"),
             col = c("black", "gray40", "red"), pch = c(19, 17, 17), lty = c(1, 1, 2))
    })
    
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
    
    output$factors_table <- renderTable({
      req(results())
      res <- results()
      data.frame(
        `Season (j)` = 1:res$L,
        `Seasonal Factor (c_j)` = round(res$seasonal_factors, 4),
        `Expected Season Forecast` = round(res$overall_mean * res$seasonal_factors, 2),
        check.names = FALSE
      )
    })
    
    output$forecast_table <- renderTable({
      req(results(), demand_data())
      res <- results()
      d <- demand_data()
      len <- length(d)
      
      df_hist <- data.frame(
        Period = 1:len,
        Season = res$season_idx,
        `Demand (D)` = d,
        `Seasonal Factor` = round(res$seasonal_factors[res$season_idx], 4),
        `Forecast (F)` = round(res$forecast, 2),
        `Error (e = F - D)` = round(res$error, 2),
        check.names = FALSE
      )
      
      df_fut <- data.frame(
        Period = res$future_periods,
        Season = res$future_season_idx,
        `Demand (D)` = NA,
        `Seasonal Factor` = round(res$seasonal_factors[res$future_season_idx], 4),
        `Forecast (F)` = round(res$future_f, 2),
        `Error (e = F - D)` = NA,
        check.names = FALSE
      )
      
      rbind(df_hist, df_fut)
    }, na = "")
  })
}