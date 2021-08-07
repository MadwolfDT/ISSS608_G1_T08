library(shiny)
library(rsconnect)
library(shinythemes)
library(clock)

rsconnect::setAccountInfo(name='dtcs', token='25A37523AE52220A0DE445A9D8B696DE', secret='OMMf3zDxI4jOhIpxHvsZJOf3MDPfIdMhPmpRSrLV')

##################import MC 2 data into variables##############################

emply_data <- read_csv("data/car-assignments.csv")
cc_data <- read_csv("data/cc_data.csv")
loyalty_data <- read_csv("data/loyalty_data.csv")

##################import MC 2 data into variables##############################

##########################cleaning MC 2 data###################################

#convert timestamp from character into date/time format
cc_data$timestamp <- date_time_parse(cc_data$timestamp,
                                     zone = "",
                                     format = "%m/%d/%Y %H:%M")

#convert timestamp from character into date/time format
loyalty_data$timestamp <- date_time_parse(loyalty_data$timestamp,
                                          zone = "",
                                          format = "%m/%d/%Y")

#Amend string text for Katrina's Cafe
cc_data <- cc_data %>%
  mutate(location = str_replace_all(location,
                                    pattern = "Katerin.+",
                                    replacement = "Katrina\x27s Caf\xe9"))


loyalty_data <- loyalty_data %>%
  mutate(location = str_replace_all(location,
                                    pattern = "Katerin.+",
                                    replacement = "Katrina\x27s Caf\xe9"))

##########################cleaning MC 2 data###################################

# Define UI
ui <- navbarPage(
  theme = shinytheme("superhero"),
  "Investigation Tool into Personnel Disappearance",   
  tabPanel("Background Situation", "To be Updated"),
  tabPanel("Exploratory Data Analysis", "To be Updated"),
  navbarMenu("Inferential Statistics", 
             tabPanel("Network Associations",
                      "To Be Updated"),
             
################################################################################             
             
             tabPanel("Personnel Movement Plot",
                      titlePanel("Personnel Movement Plot"),
                      
                      fluidRow(
                        column(3,
                               
                               selectInput(
                                 
                                 inputId = "dtemployee_name",
                                 label = "Employee Name",
                                 choices = c(emply_data$FirstName),
                                 
                               ),
                               
                               ),
                        column(9,"Two"),
                      ),
                      
                      fluidRow(
                        column(6,"Three"),
                        column(6,"Four"),
                      ),
                      
                      ),

################################################################################

             tabPanel("Transactions Analysis", "To Be Updated")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
