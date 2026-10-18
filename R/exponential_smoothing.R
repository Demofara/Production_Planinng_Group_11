library(shiny)
library(readxl)

# Arayüz (UI) Fonksiyonu
exponential_smoothing_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        # Excel dosya yükleme alanı
        fileInput(ns("file"), "Upload an Excel file (.xlsx)", accept = c(".xlsx")),
        
        # Sütun seçimi
        selectInput(ns("demand_col"), "Demand column", choices = NULL),
        
        # Başlangıç tahmini (F1) yöntemi
        radioButtons(ns("init_method"), "Initial forecast (F1) method:",
                     choices = c("Use first period demand (F1 = D1)" = "first_demand",
                                 "Enter F1 manually" = "custom_f1")),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'custom_f1'", ns("init_method")),
          numericInput(ns("f1_val"), "Initial forecast (F1)", value = 240)
        ),
        
        hr(),
        
        # Alpha (α) seçimi için radyo butonları
        radioButtons(ns("alpha_method"), "How should alpha (α) be chosen?",
                     choices = c("I will choose α manually" = "manual", 
                                 "Find the best α (Optimize)" = "auto")),
        
        # Manuel Alpha giriş paneli
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("alpha_method")),
          numericInput(ns("alpha_manual"), "Alpha (α) [between 0 and 1]", 
                       value = 0.3, min = 0.01, max = 0.99, step = 0.05)
        ),
        
        # Otomatik Alpha optimizasyonu paneli
        conditionalPanel(
          condition = sprintf("input['%s'] == 'auto'", ns("alpha_method")),
          selectInput(ns("error_metric"), "Minimize which error?", 
                      choices = c("MAD", "MSE", "MAPE"))
        )
      ),
      mainPanel(
        h4(textOutput(ns("next_period_text"))),
        p(strong(textOutput(ns("alpha_info_text")))),
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
exponential_smoothing_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Yüklenen Excel dosyasını oku
    uploaded_data <- reactive({
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      if (ext != "xlsx") {
        validate("Lütfen sadece .xlsx uzantılı bir Excel dosyası yükleyin.")
      }
      read_excel(input$file$datapath)
    })
    
    # Excel yüklendiğinde sütun isimlerini açılır menüye aktar
    observeEvent(uploaded_data(), {
      df <- uploaded_data()
      updateSelectInput(session, "demand_col", choices = names(df))
    })
    
    # Seçilen sütuna ait talep verisini sayısal vektör olarak al
    demand_data <- reactive({
      req(uploaded_data(), input$demand_col)
      df <- uploaded_data()
      req(input$demand_col %in% names(df))
      as.numeric(df[[input$demand_col]])
    })
    
    # Verilen alpha ve F1 ile Üstel Düzeltme (SES) hesaplayan yardımcı fonksiyon
    calc_ses <- function(d, alpha, f1) {
      len <- length(d)
      forecast <- numeric(len)
      forecast[1] <- f1
      
      if (len >= 2) {
        for (t in 2:len) {
          forecast[t] <- alpha * d[t - 1] + (1 - alpha) * forecast[t - 1]
        }
      }
      
      error <- forecast - d
      abs_error <- abs(error)
      
      # 1. dönem başlangıç (tohum) değeri olduğu için hata metrikleri 2. dönemden itibaren değerlendirilir
      eval_idx <- if (len >= 2) 2:len else 1
      
      mad <- mean(abs_error[eval_idx], na.rm = TRUE)
      mse <- mean(error[eval_idx]^2, na.rm = TRUE)
      mape <- mean((abs_error[eval_idx] / d[eval_idx]), na.rm = TRUE) * 100
      
      # Gelecek dönem (len + 1) tahmini
      next_f <- alpha * d[len] + (1 - alpha) * forecast[len]
      
      list(forecast = forecast, error = error,
           mad = mad, mse = mse, mape = mape,
           next_f = next_f, alpha = alpha, f1 = f1)
    }
    
    # Seçilen yönteme göre sonuçları hesapla
    results <- reactive({
      req(demand_data())
      d <- demand_data()
      req(length(d) >= 2)
      
      # Başlangıç tahmini F1'i belirle
      f1 <- if (input$init_method == "first_demand") {
        d[1]
      } else {
        req(!is.na(input$f1_val))
        input$f1_val
      }
      
      if (input$alpha_method == "manual") {
        req(!is.na(input$alpha_manual))
        validate(
          need(input$alpha_manual > 0 && input$alpha_manual < 1,
               "Lütfen 0 ile 1 arasında bir Alpha (α) değeri girin (Örn: 0.3).")
        )
        return(calc_ses(d, input$alpha_manual, f1))
        
      } else {
        # Otomatik Alpha (α) Optimizasyonu
        obj_fn <- function(a) {
          res <- calc_ses(d, a, f1)
          switch(input$error_metric,
                 "MAD" = res$mad,
                 "MSE" = res$mse,
                 "MAPE" = res$mape)
        }
        
        # Hem kırılma noktalarını kaçırmamak için hassas ızgara taraması hem de sürekli optimizasyon
        grid_alphas <- seq(0.001, 0.999, by = 0.001)
        grid_vals <- sapply(grid_alphas, obj_fn)
        best_grid_a <- grid_alphas[which.min(grid_vals)]
        
        opt <- optimize(f = obj_fn, interval = c(0.001, 0.999))
        best_alpha <- if (opt$objective < min(grid_vals)) opt$minimum else best_grid_a
        
        return(calc_ses(d, best_alpha, f1))
      }
    })
    
    # Gelecek dönem tahmin başlığı
    output$next_period_text <- renderText({
      req(results())
      res <- results()
      paste0("Exponential Smoothing (α = ", round(res$alpha, 4), 
             ") — next-period forecast: ", round(res$next_f, 2))
    })
    
    # Kullanılan parametre bilgisi
    output$alpha_info_text <- renderText({
      req(results())
      res <- results()
      method_label <- if (input$alpha_method == "auto") {
        paste0("Optimized for minimum ", input$error_metric)
      } else {
        "Manual selection"
      }
      paste0("Selected α = ", round(res$alpha, 4), " (", method_label, 
             ") | Initial Forecast F1 = ", round(res$f1, 2))
    })
    
    # Grafik çizimi
    output$plot <- renderPlot({
      req(results(), demand_data())
      res <- results()
      d <- demand_data()
      
      plot(d, type = "b", pch = 19, col = "black",
           ylab = "Demand", xlab = "Period",
           main = paste0("Actual demand and Exponential Smoothing (α = ", round(res$alpha, 3), ") forecast"),
           xlim = c(1, length(d) + 1),
           ylim = range(c(d, res$forecast, res$next_f), na.rm = TRUE))
      
      lines(res$forecast, type = "b", pch = 17, col = "gray")
      points(length(d) + 1, res$next_f, col = "red", pch = 17, cex = 1.3)
      
      legend("bottomleft", 
             legend = c("Actual demand", "Forecast", "Next-period forecast"),
             col = c("black", "gray", "red"), pch = c(19, 17, 17))
    })
    
    # Hata metrikleri tablosu
    output$error_table <- renderTable({
      req(results(), demand_data())
      res <- results()
      eval_str <- paste0("2-", length(demand_data()))
      
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