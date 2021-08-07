library(shiny)
library(rsconnect)
library(shinythemes)

rsconnect::setAccountInfo(name='dtcs', token='25A37523AE52220A0DE445A9D8B696DE', secret='OMMf3zDxI4jOhIpxHvsZJOf3MDPfIdMhPmpRSrLV')

# Define UI
ui <- navbarPage(
  theme = shinytheme("superhero"),
  "Investigation Tool into Personnel Disappearance",   
  tabPanel("Background Situation", "To be Updated"),
  tabPanel("Exploratory Data Analysis", "To be Updated"),
  navbarMenu("Inferential Statistics", 
             tabPanel("Network Associations", "To Be Updated"),
             tabPanel("Personnel Movement Plot", "To Be Updated"),
             tabPanel("Transactions Analysis", "To Be Updated")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
