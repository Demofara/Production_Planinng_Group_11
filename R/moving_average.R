library(shiny)
library(readxl)

# Arayüz (UI) Fonksiyonu
moving_average_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        # Excel dosya yükleme alanı
        fileInput(ns("file"), "Upload an Excel file (.xlsx)", accept = c(".xlsx")),
        
        # Sütun seçimi
        selectInput(ns("demand_col"), "Demand column", choices = NULL),
        
        # N seçimi için radyo butonları
        radioButtons(ns("n_method"), "How should N be chosen?",
                     choices = c("I will choose N" = "manual", 
                                 "Find the best N" = "auto")),
        
        # Manuel seçim paneli
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("n_method")),
          # Manuel seçimde istenirse N=1 (Naif tahmin) girilebilir, ama minimum 2 de yapılabilir.
          numericInput(ns("n_manual"), "N (number of past periods)", value = 3, min = 1)
        ),
        
        # Otomatik seçim paneli
        conditionalPanel(
          condition = sprintf("input['%s'] == 'auto'", ns("n_method")),
          numericInput(ns("n_max"), "Maximum N to check", value = 5, min = 2),
          selectInput(ns("error_metric"), "Minimize which error?", 
                      choices = c("MAD", "MSE", "MAPE"))
        )
      ),
      mainPanel(
        h4(textOutput(ns("next_period_text"))),
        plotOutput(ns("plot")),
        
        h4("Error measures"),
        tableOutput(ns("error_table")),
        
        h4("Forecasts by period"),
        tableOutput(ns("forecast_table"))
      )
    )
  )
}

# Sunucu (Server) Fonksiyonu
moving_average_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Yüklenen Excel dosyasını oku
    uploaded_data <- reactive({
      req(input$file)
      
      ext <- tools::file_ext(input$file$name)
      if(ext != "xlsx") {
        validate("Lütfen sadece .xlsx uzantılı bir Excel dosyası yükleyin.")
      }
      
      read_excel(input$file$datapath)
    })
    
    # Excel yüklendiğinde, sütun isimlerini dropdown menüye aktar
    observeEvent(uploaded_data(), {
      df <- uploaded_data()
      updateSelectInput(session, "demand_col", choices = names(df))
    })
    
    # Seçilen sütuna ait veriyi al
    demand_data <- reactive({
      req(uploaded_data(), input$demand_col)
      df <- uploaded_data()
      req(input$demand_col %in% names(df))
      
      as.numeric(df[[input$demand_col]])
    })
    
    # Hareketli ortalama ve hata metriklerini hesaplayan yardımcı fonksiyon
    calc_ma <- function(d, n) {
      len <- length(d)
      forecast <- rep(NA, len)
      
      if (n < len) {
        for (i in (n + 1):len) {
          forecast[i] <- mean(d[(i - n):(i - 1)])
        }
      }
      
      error <- forecast - d
      abs_error <- abs(error)
      eval_idx <- (n + 1):len
      
      if(length(eval_idx) > 0) {
        mad <- mean(abs_error[eval_idx], na.rm = TRUE)
        mse <- mean(error[eval_idx]^2, na.rm = TRUE)
        mape <- mean((abs_error[eval_idx] / d[eval_idx]), na.rm = TRUE) * 100
        next_f <- mean(d[(len - n + 1):len])
      } else {
        mad <- NA; mse <- NA; mape <- NA; next_f <- NA
      }
      
      list(forecast = forecast, error = error, 
           mad = mad, mse = mse, mape = mape, 
           next_f = next_f, n = n)
    }
    
    # Seçilen yönteme göre sonuçları hesapla
    results <- reactive({
      req(demand_data())
      d <- demand_data()
      req(length(d) > 0)
      
      if (input$n_method == "manual") {
        n <- input$n_manual
        return(calc_ma(d, n))
      } else {
        # Otomatik optimizasyon
        max_n <- max(2, input$n_max) # Güvenlik için max_n en az 2 olmalı
        best_error <- Inf
        best_res <- NULL
        
        # N=1 OLMAMASI İÇİN DÖNGÜYÜ 2'DEN BAŞLATIYORUZ
        for (i in 2:max_n) {
          res <- calc_ma(d, i)
          
          err_val <- switch(input$error_metric,
                            "MAD" = res$mad,
                            "MSE" = res$mse,
                            "MAPE" = res$mape)
          
          if (!is.na(err_val) && err_val < best_error) {
            best_error <- err_val
            best_res <- res
          }
        }
        return(best_res)
      }
    })
    
    # Gelecek dönem tahmin metni
    output$next_period_text <- renderText({
      req(results())
      res <- results()
      paste0("MA(", res$n, ") - next-period forecast: ", round(res$next_f, 2))
    })
    
    # Grafik çizimi
    output$plot <- renderPlot({
      req(results(), demand_data())
      res <- results()
      d <- demand_data()
      
      plot(d, type = "b", pch = 19, col = "black", 
           ylab = "Demand", xlab = "Period", 
           main = paste0("Actual demand and MA(", res$n, ") forecast"),
           ylim = range(c(d, res$forecast, res$next_f), na.rm = TRUE))
      
      lines(res$forecast, type = "b", pch = 17, col = "gray")
      points(length(d) + 1, res$next_f, col = "red", pch = 17)
      
      legend("bottomleft", legend = c("Actual demand", "Forecast", "Next-period forecast"), 
             col = c("black", "gray", "red"), pch = c(19, 17, 17))
    })
    
    # Hata metrikleri tablosu
    output$error_table <- renderTable({
      req(results(), demand_data())
      res <- results()
      eval_str <- paste0((res$n + 1), "-", length(demand_data()))
      
      data.frame(
        Measure = c("MAD", "MSE", "MAPE (%)"),
        Value = c(res$mad, res$mse, res$mape),
        `Periods evaluated` = c(eval_str, eval_str, eval_str),
        check.names = FALSE
      )
    })
    
    # Dönem bazlı tahmin tablosu
    output$forecast_table <- renderTable({
      req(results(), demand_data())
      res <- results()
      d <- demand_data()
      
      data.frame(
        Period = 1:length(d),
        `Demand (D)` = d,
        `Forecast (F)` = res$forecast,
        `Error (e = F - D)` = res$error,
        check.names = FALSE
      )
    }, na = "")
    
  })
}