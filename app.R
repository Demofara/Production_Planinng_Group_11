library(shiny)
library(readxl)

# Load all R scripts in the R/ directory
r_files <- list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)
for (f in r_files) {
  source(f, local = FALSE)
}

# Helper to automatically detect the *_ui or *_server function inside a specific R file
get_module_fn <- function(file_name, suffix = "_ui", fallback_candidates = character(0)) {
  for (cand in fallback_candidates) {
    if (exists(cand, mode = "function")) {
      return(get(cand, mode = "function"))
    }
  }
  path <- file.path("R", file_name)
  if (file.exists(path)) {
    parsed <- tryCatch(parse(file = path, keep.source = FALSE), error = function(e) NULL)
    if (!is.null(parsed)) {
      for (expr in parsed) {
        if (is.call(expr) && (identical(expr[[1]], as.name("<-")) || identical(expr[[1]], as.name("=")))) {
          lhs <- as.character(expr[[2]])
          if (length(lhs) == 1 && grepl(paste0(suffix, "$"), lhs) && exists(lhs, mode = "function")) {
            return(get(lhs, mode = "function"))
          }
        }
      }
    }
  }
  NULL
}

call_module_ui <- function(file_name, id, fallback_candidates = character(0)) {
  fn <- get_module_fn(file_name, "_ui", fallback_candidates)
  if (!is.null(fn)) {
    fn(id)
  } else {
    fluidPage(h4(paste("UI function not found in:", file_name)))
  }
}

call_module_server <- function(file_name, id, fallback_candidates = character(0)) {
  fn <- get_module_fn(file_name, "_server", fallback_candidates)
  if (!is.null(fn)) {
    fn(id)
  }
}

ui <- navbarPage("Production Planning",
                 
                 # ==========================================
                 # 1. FORECASTING MODULE
                 # ==========================================
                 tabPanel("Forecasting",
                          tabsetPanel(
                            # 1.1 Stationary series
                            tabPanel("Stationary series",
                                     br(),
                                     tabsetPanel(
                                       tabPanel("Moving Average",
                                                call_module_ui("moving_average.R", "moving_average", "moving_average_ui")),
                                       tabPanel("Weighted Moving Average",
                                                call_module_ui("weighted_moving_average.R", "weighted_moving_average", "weighted_moving_average_ui")),
                                       tabPanel("Simple Exponential Smoothing",
                                                call_module_ui("simple_exponential_smoothing.R", "simple_exponential_smoothing",
                                                               c("simple_exponential_smoothing_ui", "exponential_smoothing_ui"))),
                                       tabPanel("Seasonal Factors",
                                                call_module_ui("seasonal_factors.R", "seasonal_factors", "seasonal_factors_ui"))
                                     )
                            ),
                            
                            # 1.2 Trend series
                            tabPanel("Trend series",
                                     br(),
                                     tabsetPanel(
                                       tabPanel("Holt's Method",
                                                call_module_ui("holt_method.R", "holt_method", "holt_method_ui"))
                                     )
                            ),
                            
                            # 1.3 Seasonal series
                            tabPanel("Seasonal series",
                                     br(),
                                     tabsetPanel(
                                       tabPanel("Seasonal Decomposition (MA)",
                                                call_module_ui("seasonal_decomposition.R", "seasonal_decomposition", "seasonal_decomposition_ui")),
                                       tabPanel("Winters' Method",
                                                call_module_ui("winters_method.R", "winters_method", "winters_method_ui"))
                                     )
                            ),
                            
                            # 1.4 Box-Jenkins
                            tabPanel("Box-Jenkins",
                                     br(),
                                     tabsetPanel(
                                       tabPanel("AR", ar_model_ui("ar_model")),
                                       tabPanel("MA", ma_model_ui("ma_model")),
                                       tabPanel("ARMA", arma_model_ui("arma_model")),
                                       tabPanel("ARIMA", arima_model_ui("arima_model"))
                                     )
                            )
                          )
                 ),
                 
                 # ==========================================
                 # 2. AGGREGATE PLANNING MODULE
                 # ==========================================
                 tabPanel("Aggregate Planning",
                          tabsetPanel(
                            tabPanel("Heuristic Plans",
                                     call_module_ui("heuristic_plans.R", "heuristic_plans",
                                                    c("heuristic_plans_ui", "heuristic_ui", "heuristics_ui"))),
                            
                            tabPanel("Linear Programming",
                                     call_module_ui("lp_model.R", "lp_model",
                                                    c("lp_model_ui", "linear_programming_ui", "lp_ui"))),
                            
                            tabPanel("Transportation Method",
                                     call_module_ui("transportation_method.R", "transportation_method",
                                                    c("transportation_method_ui", "transportation_ui"))),
                            
                            tabPanel("Disaggregation (Square-Root Rule)",
                                     call_module_ui("disaggregation.R", "disaggregation",
                                                    c("disaggregation_ui", "disagg_ui")))
                          )
                 )
)

server <- function(input, output, session) {
  # --- Forecasting: Stationary series ---
  call_module_server("moving_average.R", "moving_average", "moving_average_server")
  call_module_server("weighted_moving_average.R", "weighted_moving_average", "weighted_moving_average_server")
  call_module_server("simple_exponential_smoothing.R", "simple_exponential_smoothing",
                     c("simple_exponential_smoothing_server", "exponential_smoothing_server"))
  call_module_server("seasonal_factors.R", "seasonal_factors", "seasonal_factors_server")
  
  # --- Forecasting: Trend series ---
  call_module_server("holt_method.R", "holt_method", "holt_method_server")
  
  # --- Forecasting: Seasonal series ---
  call_module_server("seasonal_decomposition.R", "seasonal_decomposition", "seasonal_decomposition_server")
  call_module_server("winters_method.R", "winters_method", "winters_method_server")
  
  # --- Forecasting: Box-Jenkins ---
  ar_model_server("ar_model")
  ma_model_server("ma_model")
  arma_model_server("arma_model")
  arima_model_server("arima_model")
  
  # --- Aggregate Planning ---
  call_module_server("heuristic_plans.R", "heuristic_plans",
                     c("heuristic_plans_server", "heuristic_server", "heuristics_server"))
  call_module_server("lp_model.R", "lp_model",
                     c("lp_model_server", "linear_programming_server", "lp_server"))
  call_module_server("transportation_method.R", "transportation_method",
                     c("transportation_method_server", "transportation_server"))
  call_module_server("disaggregation.R", "disaggregation",
                     c("disaggregation_server", "disagg_server"))
}

shinyApp(ui, server)
