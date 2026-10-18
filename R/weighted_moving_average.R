library(shiny)
library(readxl)

# Arayüz (UI) Fonksiyonu
weighted_moving_average_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        # Excel dosya yükleme alanı
        fileInput(ns("file"), "Upload an Excel file (.xlsx)", accept = c(".xlsx")),
        
        # Sütun seçimi
        selectInput(ns("demand_col"), "Demand column", choices = NULL),
        
        # Ağırlık seçimi yöntemi
        radioButtons(ns("w_method"), "How should weights be chosen?",
                     choices = c("I will enter weights manually" = "manual", 
                                 "Find the best weights (Optimize)" = "auto")),
        
        # Manuel ağırlık giriş paneli (Kutucuk başlangıçta boş gelir)
        conditionalPanel(
          condition = sprintf("input['%s'] == 'manual'", ns("w_method")),
          textInput(ns("weights_str"), 
                    "Weights (comma-separated, e.g., 0.5, 0.3, 0.2)", 
                    value = ""),
          radioButtons(ns("w_order"), "Weight order:",
                       choices = c("Most recent to oldest (t-1, t-2, ...)" = "recent_first",
                                   "Oldest to most recent (t-N, ..., t-1)" = "oldest_first"))
        ),
        
        # Otomatik ağırlık optimizasyonu paneli
        conditionalPanel(
          condition = sprintf("input['%s'] == 'auto'", ns("w_method")),
          numericInput(ns("n_auto"), "N (number of past periods)", value = 3, min = 2),
          selectInput(ns("error_metric"), "Minimize which error?", 
                      choices = c("MAD", "MSE", "MAPE")),
          checkboxInput(ns("monotonic"), 
                        "Recent periods must have >= weight (w1 >= w2 >= ...)", 
                        value = TRUE)
        )
      ),
      mainPanel(
        h4(textOutput(ns("next_period_text"))),
        p(strong(textOutput(ns("weights_text")))),
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
weighted_moving_average_server <- function(id) {
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
    
    # Sütun isimlerini güncelle
    observeEvent(uploaded_data(), {
      df <- uploaded_data()
      updateSelectInput(session, "demand_col", choices = names(df))
    })
    
    # Seçilen talep sütununu sayısal vektör olarak al
    demand_data <- reactive({
      req(uploaded_data(), input$demand_col)
      df <- uploaded_data()
      req(input$demand_col %in% names(df))
      as.numeric(df[[input$demand_col]])
    })
    
    # Verilen ağırlık vektörü ile WMA hesapla
    calc_wma <- function(d, w) {
      len <- length(d)
      n <- length(w)
      w <- w / sum(w) # Ağırlıkların toplamını 1'e normalize et
      
      forecast <- rep(NA, len)
      if (n < len) {
        for (i in (n + 1):len) {
          forecast[i] <- sum(w * d[(i - 1):(i - n)])
        }
      }
      
      error <- forecast - d
      abs_error <- abs(error)
      eval_idx <- (n + 1):len
      
      if (length(eval_idx) > 0 && n < len) {
        mad <- mean(abs_error[eval_idx], na.rm = TRUE)
        mse <- mean(error[eval_idx]^2, na.rm = TRUE)
        mape <- mean((abs_error[eval_idx] / d[eval_idx]), na.rm = TRUE) * 100
        next_f <- sum(w * d[len:(len - n + 1)])
      } else {
        mad <- NA; mse <- NA; mape <- NA; next_f <- NA
      }
      
      list(forecast = forecast, error = error,
           mad = mad, mse = mse, mape = mape,
           next_f = next_f, n = n, weights = w)
    }
    
    # Seçilen yönteme göre ağırlıkları belirle ve sonuçları hesapla
    results <- reactive({
      req(demand_data())
      d <- demand_data()
      req(length(d) > 2)
      
      if (input$w_method == "manual") {
        # Kutucuk boşken işlem yapmadan bekle
        req(nzchar(trimws(input$weights_str)))
        
        raw_w <- as.numeric(unlist(strsplit(gsub(" ", "", input$weights_str), ",")))
        validate(
          need(length(raw_w) >= 2 && all(!is.na(raw_w)) && all(raw_w >= 0) && sum(raw_w) > 0,
               "Lütfen virgülle ayrılmış en az 2 pozitif ağırlık girin (Örn: 0.5, 0.3, 0.2).")
        )
        
        if (input$w_order == "oldest_first") {
          raw_w <- rev(raw_w)
        }
        
        return(calc_wma(d, raw_w))
        
      } else {
        # Otomatik ağırlık optimizasyonu
        n <- max(2, min(input$n_auto, length(d) - 1))
        
        obj_fn <- function(par) {
          if (input$monotonic) {
            inc <- exp(par)
            w <- rev(cumsum(inc))
          } else {
            w <- exp(par)
          }
          w <- w / sum(w)
          res <- calc_wma(d, w)
          
          switch(input$error_metric,
                 "MAD" = res$mad,
                 "MSE" = res$mse,
                 "MAPE" = res$mape)
        }
        
        opt <- optim(par = rep(0, n), fn = obj_fn, method = "Nelder-Mead")
        
        if (input$monotonic) {
          best_w <- rev(cumsum(exp(opt$par)))
        } else {
          best_w <- exp(opt$par)
        }
        best_w <- best_w / sum(best_w)
        
        return(calc_wma(d, best_w))
      }
    })
    
    # Gelecek dönem tahmin başlığı
    output$next_period_text <- renderText({
      req(results())
      res <- results()
      paste0("WMA(", res$n, ") — next-period forecast: ", round(res$next_f, 2))
    })
    
    # Kullanılan ağırlıkları gösteren metin
    output$weights_text <- renderText({
      req(results())
      res <- results()
      w_formatted <- paste(round(res$weights, 3), collapse = ", ")
      paste0("Normalized Weights (from t-1 to t-", res$n, "): [", w_formatted, "]")
    })
    
    # Grafik çizimi
    output$plot <- renderPlot({
      req(results(), demand_data())
      res <- results()
      d <- demand_data()
      
      plot(d, type = "b", pch = 19, col = "black",
           ylab = "Demand", xlab = "Period",
           main = paste0("Actual demand and WMA(", res$n, ") forecast"),
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