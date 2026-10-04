# app.R dosyasının içeriği

library(shiny)

# R klasöründeki modülümüzü çağırıyoruz
source("R/moving_average.R") 

ui <- navbarPage("Production Planning",
                 # Eğer Learning Curve modülün yoksa o satırı sildim, sadece yazdığımızı ekledim
                 tabPanel("Moving Average", moving_average_ui("moving_average"))
)

server <- function(input, output, session) {
  # Modülün server kısmını çalıştırıyoruz
  moving_average_server("moving_average")
}

shinyApp(ui, server)