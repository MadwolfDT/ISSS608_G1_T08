library(shiny)
library(rsconnect)
library(shinythemes)
library(clock)
library(tidyverse)
library(plotly)
library(lubridate)
library(hms)       

rsconnect::setAccountInfo(name='dtcs', token='25A37523AE52220A0DE445A9D8B696DE', secret='OMMf3zDxI4jOhIpxHvsZJOf3MDPfIdMhPmpRSrLV')

##################import MC 2 data into variables##############################

employee <- read_csv("data/EmployeeRecords.csv")
car_data <- read_csv("data/car-assignments.csv")
cc_data <- read_csv("data/cc_data.csv")
loyalty_data <- read_csv("data/loyalty_data.csv")
gps <- read_csv("data/gps.csv")

##################import MC 2 data into variables##############################
browser()
##########################cleaning MC 2 data###################################
gps$Timestamp <- date_time_parse(gps$Timestamp,
                                 zone = "",
                                 format = "%m/%d/%Y %H:%M:%S")

gps <- gps %>%
  mutate(datestamp = as.Date(Timestamp + 60*60*8))

#convert timestamp from character into date/time format
#cc_data$timestamp <- date_time_parse(cc_data$timestamp,
                                     #zone = "",
                                     #format = "%m/%d/%Y %H:%M")

#convert timestamp from character into date/time format
#loyalty_data$timestamp <- date_time_parse(loyalty_data$timestamp,
                                          #zone = "",
                                          #format = "%m/%d/%Y")

#Amend string text for Katrina's Cafe
cc_data <- cc_data %>%
  mutate(location = str_replace_all(location,
                                    pattern = "Katerin.+",
                                    replacement = "Katrina\x27s Caf\xe9"))


loyalty_data <- loyalty_data %>%
  mutate(location = str_replace_all(location,
                                    pattern = "Katerin.+",
                                    replacement = "Katrina\x27s Caf\xe9"))

#Creating Time Bins

#Creating breaks
breaks <- hour(hm("00:00", "7:00","10:00", "12:00", "14:30", "17:00", "19:00", "23:59"))
#Create labels for the breaks
labels <- c("Midnight", "Morning", "Pre-Lunch", "LunchTime", "Afternoon", "Evening", "Night")

#Creating Location Categories

Shops <- c("Albert's Fine Clothing","Shoppers' Delight","Octavio's Office Supplies","Roberts and Sons","Daily Dealz","Frydos Autosupply n' More")
FoodnBev <- c("Brew've Been Served","Hallowed Grounds", "Coffee Cameleon", "Coffee Shack","Bean There Done That","Brewed Awakenings","Jack's Magical Beans","Katrina's Café","Hippokampos","Abila Zacharo","Gelatogalore","Kalami Kafenion","Ouzeri Elian","Guy's Gyros")
Hotel <- c("Chostus Hotel")
Industrial <- c("Kronos Pipe and Irrigation","Nationwide Refinery","Maximum Iron and Steel","Stewart and Sons Fabrication","Carlyle Chemical Inc.","Abila Scrapyard")
Recreational <- c("Ahaggo Museum","Desafio Golf Course")
Supermarket <- c("General Grocer","Kronos Mart")
Refuel <- c("U-Pump","Frank's Fuel")
Others <- c("Abila Airport")

distinctloc <- cc_data %>% distinct(location)
distinctloc <- distinctloc %>% 
  mutate(category = case_when(location %in% Shops ~ 'Shops',
                              location %in% FoodnBev ~ 'F & B',
                              location %in% Hotel ~ 'Hotel',
                              location %in% Industrial ~ 'Industrial',
                              location %in% Recreational ~ 'Recreational',
                              location %in% Supermarket ~ 'Supermarket',
                              location %in% Refuel ~ 'Refuel',
                              location %in% Others ~ 'Others'
  ))

#Converting timestamp from character into date/time format
#Extracting Date and Time
#Adding Time Labels and Location Categories

#cc_data

cc_data <- cc_data %>% 
  #Extract date and time
  mutate(timestamp = mdy_hm(cc_data$timestamp)) %>% 
  mutate(date = as.Date(timestamp),
         time = hms::as_hms(timestamp)) %>% 
  #Add Time Labels
  mutate(TimeCat = cut(x=hour(time), breaks=breaks, labels=labels, include.lowest=TRUE)) %>% 
  #Add Location Categories
  left_join(distinctloc, by = "location")

#loyalty_data

loyalty_data <- loyalty_data %>% 
  #Convert date
  mutate(timestamp = mdy(loyalty_data$timestamp)) %>% 
  #Add Location Categories
  left_join(distinctloc, by = "location")


##########################cleaning MC 2 data###################################

##########################DT Variables###################################

gps_date <- gps %>%
  distinct(datestamp)

##########################DT Variables###################################

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
                                 choices = c(paste(car_data$FirstName, car_data$LastName, sep = " ")),
                                 
                               ),
                               
                               selectInput(
                                 
                                 inputId = "dtdate",
                                 label = "Date",
                                 choices = c(gps_date$datestamp),
                                 
                               ),
                               
                               ),
                        column(9,"Plotted Map to be displayed here"),
                      ),
                      
                      fluidRow(
                        column(6,"Selected Personnel Parking Details"),
                        column(6,"Population Parking Details"),
                      ),
                      
                      ),

################################################################################

            tabPanel("Transaction Amount Analysis", 
                    titlePanel("Transaction Amount Analysis"),
         
                    fluidRow(
                      column(2,
                              selectInput(
                                inputId = "rtlocationcat",
                                label = "Location Category",
                                choices = c(distinctloc$category),
                                selected = "Shops"),
                              submitButton("Apply Selected")
                      ),
                      column(10,
                              plotlyOutput(outputId = "TxnBoxPlotA"),
                      )
                    ),
                    fluidRow(
                      column(2,
                              radioButtons(
                                inputId = "rtradio",
                                label = "View Transactions for Specific Cards",
                                choices = c("Credit Card", "Loyalty Card"),
                                selected = "Credit Card"),
                  
                              conditionalPanel(
                                condition = "input.rtradio == 'Credit Card'",
                                selectInput(
                                  inputId = "rtcreditcard",
                                  label = "Last 4 Digits of Card",
                                  choices = unique(cc_data$last4ccnum)
                                ),
                             ),
                              conditionalPanel(
                                condition = "input.rtradio == 'Loyalty Card'",
                                selectInput(
                                  inputId = "rtloyalcard",
                                  label = "Loyalty Card No.",
                                  choices = unique(loyalty_data$loyaltynum)
                                )
                              ),
                              submitButton("Apply Selected"),
                      ),
                      column(10,
                             conditionalPanel(
                               condition = "input.rtradio == 'Credit Card'",
                               plotlyOutput(outputId = "TxnScatterCredit")
                             ),
                             conditionalPanel(
                               condition = "input.rtradio == 'Loyalty Card'",
                               plotlyOutput(outputId = "TxnScatterLoyalty")
                             )
                        
                      )
                    ),
         
                    "To Be Updated")

################################################################################
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
#########################SOCIAL NETWORK ANALYSIS################################

  
#########################GPS TRACKING ANALYSIS##################################

  
#########################TRANSACTION ANALYSIS###################################
  
  ###For Transaction Boxplot A###
  output$TxnBoxPlotA <- renderPlotly({
    
    #Combining the cc_data and loyalty_data
    ccloyal <- dplyr::bind_rows(list(cc_data = cc_data, loyalty_data = loyalty_data), .id='Source')
    
    p2c <- ccloyal %>% filter(category == input$rtlocationcat) %>% 
      ggplot(aes(x=location, y=price, text=paste("Timestamp:", timestamp,"<br />CC No.:", last4ccnum, "<br />LC No.:", loyaltynum))) +
      geom_boxplot(aes(fill = Source), 
                   position = position_dodge(1)) +
      geom_point(alpha=0) + scale_y_log10() +
      ggtitle("Boxplot of Transaction Amounts by Category") +
      theme(axis.title=element_blank(),
            plot.title=element_text(size=16, face="bold"),
            legend.position = "bottom") +
      scale_x_discrete(limits = rev) + coord_flip()
    
    plotp2c <- p2c %>% ggplotly() %>% 
      layout(boxmode = "group",
             legend = list(orientation = "h", x=0.2, y=-0.1))
    plotp2c$x$data[[1]]$marker$line$color = "red"
    plotp2c$x$data[[1]]$marker$color = "red"
    plotp2c$x$data[[1]]$marker$outliercolor = "red"
    
    plotp2c
    
  })
    
################################################################################  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
