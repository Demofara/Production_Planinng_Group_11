library(shiny)

ui <- navbarPage("Production Planning",
                 tabPanel("Forecasting",
                          tabsetPanel(
                            tabPanel("Stationary series",
                                     br(),
                                     tabsetPanel(
                                       tabPanel("Moving Average", moving_average_ui("moving_average")),
                                       tabPanel("Weighted Moving Average", weighted_moving_average_ui("weighted_moving_average")),
                                       tabPanel("Exponential Smoothing", exponential_smoothing_ui("exponential_smoothing"))
                                     )
                            )
                          )
                 )
)

server <- function(input, output, session) {
  moving_average_server("moving_average")
  weighted_moving_average_server("weighted_moving_average")
  exponential_smoothing_server("exponential_smoothing")
}

shinyApp(ui, server)