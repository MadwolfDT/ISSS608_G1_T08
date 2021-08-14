library(shiny)
library(rsconnect)
library(shinythemes)
library(tidyverse)
library(plotly)
library(lubridate)
library(hms)
library(raster)
library(sf)
library(tmap)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(parcoords)
library(GGally)
library(scales)
library(shinyscreenshot)
library(DT)
library(visNetwork)
library(igraph)
library(ggraph)
library(tidygraph)
library(widyr)
library(tidytext)
library(ggwordcloud)
library(textdata)
library(corporaexplorer) 
library(topicmodels)
library(widyr)
library(shinyWidgets)
library(shinyjs)
library(chron)
library(udpipe)

rsconnect::setAccountInfo(name='dtcs', token='25A37523AE52220A0DE445A9D8B696DE', secret='OMMf3zDxI4jOhIpxHvsZJOf3MDPfIdMhPmpRSrLV')

####Loading Source Scripts####
source('scripts/read_data.r')
source('scripts/dt_clean_data.r')
source('scripts/dt_variables.r')
source('scripts/mc1_clean_and_import.r')
source('scripts/rt_clean_data.r')


# Define UI ####
ui <- navbarPage(
  
  title =  "Investigation Tool into Personnel Disappearance",
  
  theme = shinytheme("journal"),
  
  tabPanel("Background Situation",
           
           HTML(
             
             "
             <h1>Investigation Tool into Personnel Disappearance</h1>
              <hr>
              <h2>Background Situtation</h2>
              
              <p>In the roughly twenty years that Tethys-based GAStech has been operating a natural gas production site in the island country of Kronos, it has produced remarkable profits and developed strong relationships with the government of Kronos. However, GAStech has not been as successful in demonstrating environmental stewardship.</p>
              
              <p>In<strong> January&nbsp;2014</strong>, the leaders of GAStech are celebrating their new-found fortune as a result of the initial public offering of their very successful company. In the midst of this celebration, several employees of GAStech go missing. An organization known as the Protectors of Kronos (POK) is suspected in the disappearance, but things may not be what they seem.</p>
              
              <p>By the invitation of Kronos and Tethys, an online tool has been developed to aid the law enforcement agencies.</span></p>
              
              <h2>Functions Available</h2>
              
              <ol>
              	<li>
              	<h3>Exploratory Data Analysis</h3>
              
              	<ul>
              		<li>
              		<b>Locations </b>- For Exploration: This function would allow the user to identify the popular locations within Abila, Kronos. In addition, the user would be able to determine the peak periods of the specified locations.
              		</li>
              		<li>
              		<b>Transactions </b>- For Exploration: This function would allow the user to link possible credit card transactions, loyalty card transactions, locations, date and time, to allow inference between the holders of credit cards, loyalty card holders with possible GASTech Employees.
              		</li>
              		<li>
              		<b>Transactions </b>- Individual Cards: This function would allow the user to select two individual cards (credit card or loyalty card) and compare where and when their transactions took place, and their transaction amounts, all within the same space for easy comparison.
              		</li>
              		<li>
              		<b>Employees </b>of GASTech: This function would allow the user to retrieve the bio-data of GASTech employees
              		</li>
              		<li>
              		<b>Email </b>Correspondence: This function would allow the user to determine the email correspondences between the selected GASTech employee and his/her recipients.
              		</li>
              	</ul>
              	</li>
              	<li>
              	<h3>Inferential Statistics</h3>
              
              	<ul>
              		<li>
              		<b>Email Network Analysis</b>: This function would allow the user to search the email conversations by either the GasTech employee, or any keyword or both. The conversation chain is displayed as network graph for easy visualization. 
              		</li>
              		<li>
              		<b>Networks</b>: This function would allow the user to build the entire email network by either links (To, From) or using text. The user is able to view the betweenness of each node. On click, a subgraph appears to help the user see in detail the connections of the employee via email. 
              		</li>
              		<li>
              		<b>Employment Movement Plot</b>: This function would allow the user to map the GPS routes and identify the specific locations which the employee had been to. The GPS route would be plotted on the map, against identified homes of GASTech employees and prominant locations. A data table would be generated to suggest the locations where the employee would had been to, with a box plot suggesting the average time spent at the said location.
              		</li>
              		<li>
              		<b>Transaction Amount Analysis</b>: This function would allow the user to view summary statistics of transaction amounts that took place in each location, for both credit card and loyalty card. Outliers would be highlighted. In addition, users can also select to view transactions of specific cards, and how each transaction amount compares against the full distribution for each location. 
              		</li>
              	</ul>
              	</li>
              </ol>
             
             
             "
             
           )#close bracket for HTML()
           
           ),
  
  navbarMenu(
    "Exploratory Data Analysis",
    
    ####DT Locations Exploration UI####
    tabPanel(title = "Locations - For Exploration!",
             
             fluidRow(
               titlePanel("Locations Heat Map"),
               
               column(4,
                      
                      dateRangeInput(
                        
                        inputId = "dtlocationdaterange",
                        label = "Date (From)",
                        min = gps_date$datestamp[1],
                        max = tail(gps_date$datestamp, n = 1),
                        start = gps_date$datestamp[1],
                        end = tail(gps_date$datestamp, n = 1)
                        
                      ),#close bracket with comma for dateInput
                      
                      checkboxInput(
                        
                        inputId = "dtweekday",
                        label = "Weekdays",
                        value = TRUE
                        
                      ),#close bracket with comma for checkbosInput
                      
                      checkboxInput(
                        
                        inputId = "dtweekend",
                        label = "Weekends",
                        value = TRUE
                        
                      ),#close bracket with comma for checkbosInput
                      
                      actionButton("plottxnheatmap", "Plot"),
                      
                      
               ),#close brackets for column(), need comma
               
               column(8,
                      
                      girafeOutput(outputId = "dtlocationheatmap")
                      
               ),#close brackets for column(), need comma
               
             ),#close brackets for fluidRow(), need comma
             
             fluidRow(
               
               titlePanel("Location Visits by Day"),
               
               column(4,
                      
                      selectInput(
                        
                        inputId = "dtlocationselection",
                        label = "Location",
                        choices = c(cc_locations$location)
                        
                      )
                      
               ),#close brackets for column(), need comma
               
               column(8,
                      
                      plotlyOutput(outputId = "dtsellocationvisits"),
                      
               ),#close brackets for column(), need comma
               
             ),#close brackets for fluidRow(), need comma
             
    ), #close brackets for tabPanel for Locations, need comma
    
    ####RT Transactions Exploration UI####
    tabPanel(title = "Transactions - For Exploration!",
             
             titlePanel("Transactions - An Overview"),
             
             fluidRow(
               column(2, 
                      
                      selectInput(
                        inputId = "rtlevelone",
                        label = "Select Variable One",
                        choices = list(
                          "Location" = "location",
                          "Price" = "price",
                          "CC No." = "ccnum",
                          "LC No." = "loyaltynum",
                          "Date" = "date",
                          "Time" = "hour",
                          "Period of Day" = "TimeCat"
                        ),
                        selected = "location"
                        
                      ), #close bracket and comma for selectInput
                      
                      selectInput(
                        inputId = "rtleveltwo",
                        label = "Select Variable Two",
                        choices = list(
                          "Location" = "location",
                          "Price" = "price",
                          "CC No." = "ccnum",
                          "LC No." = "loyaltynum",
                          "Date" = "date",
                          "Time" = "hour",
                          "Period of Day" = "TimeCat"
                        ),
                        selected = "ccnum"
                        
                      ),#close bracket and comma for selectInput
                      
                      selectInput(
                        inputId = "rtlevelthree",
                        label = "Select Variable Three",
                        choices = list(
                          "Location" = "location",
                          "Price" = "price",
                          "CC No." = "ccnum",
                          "LC No." = "loyaltynum",
                          "Date" = "date",
                          "Time" = "hour",
                          "Period of Day" = "TimeCat"
                        ),
                        selected = "loyaltynum"
                        
                      ),#close bracket and comma for selectInput
                      
                      selectInput(
                        inputId = "rtlevelfour",
                        label = "Select Variable Four",
                        choices = list(
                          "Location" = "location",
                          "Price" = "price",
                          "CC No." = "ccnum",
                          "LC No." = "loyaltynum",
                          "Date" = "date",
                          "Time" = "hour",
                          "Period of Day" = "TimeCat"
                        ),
                        selected = "date"
                        
                      ),#close bracket and comma for selectInput
                      
                      selectInput(
                        inputId = "rtlevelfive",
                        label = "Select Variable Five",
                        choices = list(
                          "Location" = "location",
                          "Price" = "price",
                          "CC No." = "ccnum",
                          "LC No." = "loyaltynum",
                          "Date" = "date",
                          "Time" = "hour",
                          "Period of Day" = "TimeCat"
                        ),
                        selected = "hour"
                        
                      ),#close bracket and comma for selectInput
                      
                      
                      sliderInput("rtslider", label = "Range of CC No.",
                                  min = 0, max = 9736, value = c(1000,2000)
                      ),#close bracket for sliderInput
                      
                      #submitButton("Apply Selected")
                      
                      
               ), #close brackets for column(2), need comma
               
               column(10,
                      
                      parcoordsOutput(
                        outputId = "rtparacoord",
                        width = "100%",
                        height = "600px"
                      ),#close brackets for parcoordsOutput
                      
                      screenshotButton(
                        label = "Capture Chart!",
                        filename = "caseplotscreenshot",
                        id = "rtparacoord",
                        scale = 1,
                        timer = 0
                      ),
                      
                      screenshotButton(label = "Capture Entire Page!"),
                      
               ), #close brackets for column(10), need comma
               
               
             ),#close brackets for fluidRow(), need comma
             
             
    ), #close brackets for tabPanel for EDA, need comma
    
    ####RT Analysis for Specific Cards UI####
    tabPanel("Individual Card Transactions",
             useShinyjs(),
             titlePanel("Compare Transactions for Individual Cards"),
             
             fluidRow(
               column(2,
                      
                      prettyRadioButtons(
                        inputId = "rtjellytop",
                        label = "Select Card Type:",
                        choices = c("Credit Card", "Loyalty Card"
                        ), #close bracket w comma for c
                        selected = "Credit Card",
                        icon = icon("check"),
                        inline = FALSE,
                        bigger = TRUE,
                        status = "info",
                        animation = "jelly"
                      ), #close bracket w comma for prettyradiobutt
                      
                      selectInput(
                        inputId = "rtspeccreditA",
                        label = "Credit Card (Top)",
                        choices = sort(unique(cc_data$last4ccnum))
                      ),#close bracket w comma for selectinput
                      
                      prettyRadioButtons(
                        inputId = "rtjellyA",
                        label = "Fill by:",
                        choices = list(
                          "Transaction Price" = "price",
                          "Time Period" = "TimeCat"
                        ), #close bracket w comma for list
                        selected = "price",
                        icon = icon("check"),
                        inline = TRUE,
                        bigger = TRUE,
                        status = "info",
                        animation = "jelly"
                      ), #close bracket w comma for radiobutt
                      
                      selectInput(
                        inputId = "rtspecloyaltyA",
                        label = "Loyalty Card No.",
                        choices = sort(unique(loyalty_data$loyaltynum))
                      ), #close bracket w comma for selectinput
                      
               ), #close bracket w comma for column
               
               column(10,
                      #price plots credit
                      plotlyOutput(outputId = "rtccpricetop"),
                      #time plots credit
                      plotlyOutput(outputId = "rtcctimetop"),
                      DT::dataTableOutput("rtcctabletop"),
                      #plots loyalty
                      plotlyOutput(outputId = "rtlctop"),
                      DT::dataTableOutput("rtlctabletop")
                      
               ), #close bracket w comma for column
               
             ), #close bracket w comma for fluidRow
             
             fluidRow(
               column(2,
                      
                      prettyRadioButtons(
                        inputId = "rtjellybottom",
                        label = "Select Card Type:",
                        choices = c("Credit Card", "Loyalty Card"
                        ), #close bracket w comma for c
                        selected = "Credit Card",
                        icon = icon("check"),
                        inline = FALSE,
                        bigger = TRUE,
                        status = "info",
                        animation = "jelly"
                      ), #close bracket w comma for prettyradiobutt
                      
                      selectInput(
                        inputId = "rtspeccreditB",
                        label = "Credit Card (Bottom)",
                        choices = sort(unique(cc_data$last4ccnum))
                      ),#close bracket w comma for selectinput
                      
                      prettyRadioButtons(
                        inputId = "rtjellyB",
                        label = "Fill by:",
                        choices = list(
                          "Transaction Price" = "price",
                          "Time Period" = "TimeCat"
                        ), #close bracket w comma for list
                        selected = "price",
                        icon = icon("check"),
                        inline = TRUE,
                        bigger = TRUE,
                        status = "info",
                        animation = "jelly"
                      ), #close bracket w comma for radiobutt
                      
                      selectInput(
                        inputId = "rtspecloyaltyB",
                        label = "Loyalty Card No.(Bottom)",
                        choices = sort(unique(loyalty_data$loyaltynum))
                      ), #close bracket w comma for selectinput
                      
               ), #close bracket w comma for column
               
               column(10,
                      #price plots credit
                      plotlyOutput(outputId = "rtccpricebottom"),
                      #time plots credit
                      plotlyOutput(outputId = "rtcctimebottom"),
                      DT::dataTableOutput("rtcctablebottom"),
                      #plots loyalty
                      plotlyOutput(outputId = "rtlcbottom"),
                      DT::dataTableOutput("rtlctablebottom"),
                      
                      ), #close bracket w comma for column
               
             ), #close bracket w comma for fluidRow  
             
    ), #close bracket with comma for Specific Card Analysis Tab
    
    
    ####NK Employee Bio-data Lookup UI####
    tabPanel(title = 'Employees of GasTech',
             titlePanel("Employee Bio-data Lookup"), 
             column(width=3,
                    h4('Select Look-Up'),
                  
                    radioButtons(inputId = 'view_select', 
                                 label=NULL, 
                                 choices = c('Person','Overview')
                    ),
                    h4('Select Employee'),
                    selectInput(inputId = 'biodata_select', 
                                label = NULL, 
                                choices = list_emp
                    ),
                    h4('Select the Chart to Display'),
                    selectInput(inputId = 'biodata_select2', 
                                label = NULL, 
                                choices = c('Age Comparison',
                                            'Year Joined',
                                            #'Timings of Emails',
                                            'Military Service')
                    )
                    
             ), # close bracket for column 1
             
             column(width=9,
                    div(tableOutput(outputId = 'biodata_output'), 
                        style='color:black;font-weight: bold;font-size: 20px;font-family:"News Cycle", "Arial Narrow Bold", sans-serif;'
                    ),
                    plotlyOutput(width = '100%', 
                                 height=700,
                                 outputId = 'overview_output')
                    
                    
             ) # close bracket for column 2
             
             
    ),  #close brackets for tabPanel for Emp of GasTech
    tabPanel(
      
      ####NK Email Correspondence UI####
      title = "Email Correspondence",
      column(width=3,
             h4("Date Filter"),
             dateRangeInput(inputId = 'date', 
                            label = NULL, 
                            start = min(df.emails$Date.Date), 
                            end = max(df.emails$Date.Date)
             ),
             h4('Time Filter'),
             sliderInput(inputId = 'time',
                         label = NULL, 
                         min = as.POSIXct("1990-01-01 00:00:05", tz = 'GMT'), 
                         max = as.POSIXct("1990-01-01 23:59:59", tz = 'GMT'), 
                         value = c(as.POSIXct("1990-01-01 00:00:05",tz = 'GMT'), 
                                   as.POSIXct("1990-01-01 23:59:59",tz = 'GMT')
                         ),
                         step = 2*60*60,
                         timeFormat = "%H:%M",
                         timezone = "GMT"
             ),
             h4('Select Employee'),
             selectInput(inputId = 'person', 
                         label= NULL, 
                         choices = list_emp)
      ),
      
      column(width = 9, 
             fluidRow(
               
               plotlyOutput(outputId = 'email_convo')
               ),
             fluidRow(
               column(width = 6, 
                      plotlyOutput(outputId = 'timings_email_1')),
               column(width = 6, 
                      plotlyOutput(outputId = 'timings_email_2'))
               )
             
      )
    )
    
    
    
  ), #close brackets for NavbarMenu
  
  navbarMenu("Inferential Statistics", 
             
             ####NK Email Network Analysis UI####
             tabPanel("Email Network Analysis",
                      
                      column(width=3, 
                             
                             h4('Search by'),
                             radioButtons(inputId = 'dt_select', 
                                          label=NULL, 
                                          choices = c('Person','Keywords', 'Both'),
                                          selected = 'Person'
                             ),
                             h4('Select Employee'),
                             selectInput(inputId = 'person2', 
                                         label= NULL, 
                                         choices = list_emp,
                                         selected = list_emp[1]),
                             #textOutput(outputId = 'debug'),
                             h4("Text Input"),
                             h5("Separate the words by a comma as shown in the example below"),
                             textInput(inputId = 'search', 
                                       label = NULL,
                                       value = 'word1,word2'),
                             h4('  '),
                             h4('Commit your Inputs/Filters'),
                             actionButton(inputId = 'go', 
                                          label = "Commit & Display"
                                          )
                      ), #close bracket without comma for column1
                      
                      column(width=9,
                             span(textOutput(outputId = 'info_message'), style='font-size: 20px;font-family:"News Cycle", "Arial Narrow Bold", sans-serif;'),
                             h2(' '),
                             tabsetPanel(
                               tabPanel("Network Display",
                                        visNetworkOutput(outputId = 'vis_email',
                                                         width = "100%", 
                                                         height = 700)
                               ),
                               tabPanel("Data Table",
                                        h2(''),
                                        column(width=1),
                                        column(width=10, DT::dataTableOutput(outputId = 'table'))
                               ) #close bracket for tabpanel
                             )#close bracket without comma for tabsetPanel
                             
                      ) #close bracket without comma for column2
                      
             ), #close bracket with comma for tab Panel Email
             
             ####NK Networks UI ####
             tabPanel(title = "Networks",
                      
                      
                      
                      column(width = 2,
                             h4("Select the Network to Build By:"),
                             h4(' '),
                             radioButtons(inputId='ntwk_select',
                                          label = NULL,
                                          choices = c('Email Correspondence',
                                                      'Email Subjects'),
                                          selected =  'Email Correspondence',
                                          ),
                            
                             h4("Node Sizing"),
                             h4(' '),
                             radioButtons(inputId = 'node_sizings',
                                          label=NULL,
                                          choices = c('None',
                                                      'Betweenness', 
                                                      'Degree', 
                                                      'Out-Degree', 
                                                      'In-Degree', 
                                                      'Closeness'),
                                          selected = 'None'),
                             
                             radioButtons(inputId = 'node_sizings2',
                                          label=NULL,
                                          choices = c('None',
                                                      'Betweenness', 
                                                      'Degree'),
                                          selected = 'None'),
                             h4(' '),
                             div(id='text_div_MA', 
                                 
                             #h4("Modifying Aesthetics"),
                             
                             # actionButton(inputId = 'about', 
                             #              label=NULL, 
                             #              icon = tags$i(class = "fas fa-info", style="font-size: 24px; color: black"),
                             #              style="color: #fff; background-color: #fff; border-color: #fff;"),
                             # 
                             h4('Community Algorithms'),
                             radioButtons(inputId = 'communities',
                                         label=NULL,
                                         choices = c('None',
                                                     'Department',
                                                     'Cluster Lovain',
                                                     'Betweeness',
                                                     'Label Propagation',
                                                     'Fast Greedy',
                                                     'Leading Eigenvector'),
                                         selected = 'Department'
                             ),
                             
                             h4("Direction of Edges"),
                             h4(' '),
                             checkboxInput(inputId = 'arrow',
                                           label = "Yes, Display", 
                                           value = T),
                             h4(' '),
                             h4("Scale the width of Edges"),
                             h4(' '),
                             h4('Input minimum edge weight'),
                             h6('Min Value = 0.1, Max Value = 5'),
                             numericInput(inputId = 'min_width', 
                                          label=NULL, 
                                          min =0.1, max=5, value = 0.5),
                             h4('Input maximum edge weight'),
                             h6('Min Value = 5, Max Value = 15'),
                             numericInput(inputId = 'max_width', 
                                          label=NULL, 
                                          min =5, max=15,value = 7),
                             h5(' '),
                             h4('Select Layout of Graph'),
                             selectInput(inputId = 'layout',
                                         label = NULL,
                                         choices = c(#"add_layout_", 
                                                     #"component_wise", 
                                                     #"layout_as_bipartite", 
                                                     "layout_nicely",
                                                     "layout_as_star", 
                                                     "layout_as_tree", 
                                                     "layout_in_circle", 
                                                     "layout_on_grid", 
                                                     "layout_on_sphere", 
                                                     "layout_randomly", 
                                                     'layout_with_dh', 
                                                     "layout_with_fr", 
                                                     'layout_with_gem', 
                                                     "layout_with_graphopt", 
                                                     "layout_with_kk", 
                                                     "layout_with_lgl", 
                                                     "layout_with_mds", 
                                                     "layout_with_sugiyama"
                                                     #"merge_coords", 
                                                     #"norm_coords", 
                                                     #"normalize"
                                                     ),
                                         selected = 'layout_nicely'
                                         
                                         
                                         ),
                             h4('Highlight Nearest Nodes'),
                             h4(' '),
                             checkboxInput(inputId = 'nearest',
                                          label = 'Yes, highlight',
                                          value = F),
                             h4(' '),
                            h4('Color Sub-Graph Nodes by Department'),
                            h4(' '),
                            checkboxInput(inputId = 'color_sub',
                                          label = 'Yes, color by department',
                                          value = F),
                            h4(' '),
                            h4('Enable Multi-selection of Nodes'),
                            checkboxInput(inputId = 'multi_select',
                                          label = 'Yes, enable multi-select',
                                          value = F)
                                         
                             
                             ) # Close bracket for div
                             
                      ), # close bracket for column
                      
                      
                      column(width=6, 
                             h4("Main Network"),
                             h5("Click a node on the Main Network below, to see their sub-graph on the right"),
                             visNetworkOutput(outputId = 'vis_dept',
                                              width = 800, 
                                              height = 700) #,
                             
                             #visNetworkOutput(outputId = 'text_ntwk',
                              #                width='100%',
                              #                height=700)
                             
                             ),
                      
                      column(width=4, 
                             div(id='sub_graph', 
                                 h4("Sub Network"),
                              
                             visNetworkOutput(outputId = 'vis_dept_sub',
                                              #width = 700, 
                                              height = 700))
                             
                             ),
                      
                      
                      
             ),#close bracket with comma for Networks
             
             ####DT Employee Movement Plot UI####
             tabPanel("Employee Movement Plot",
                      
                      titlePanel("Personnel Movement Plot"),
                      
                      fluidRow(
                        column(4,
                               
                               selectInput(
                                 
                                 inputId = "dtemployee_name",
                                 label = "Employee Name",
                                 choices = c(paste(emply_name$FirstName, emply_name$LastName, sep = " ")),
                                 
                               ),#close bracket with comma for selectInput
                               
                               dateInput(
                                 
                                 inputId = "dtdate",
                                 label = "Date",
                                 min = gps_date$datestamp[1],
                                 max = tail(gps_date$datestamp, n = 1),
                                 value = gps_date$datestamp[1]
                                 
                               ),#close bracket with comma for dateInput
                               
                               sliderInput(
                                 inputId = "dtduration",
                                 label = "Parking Duration (in minutes)",
                                 min = 3,
                                 max = 15,
                                 value = c(5)
                                 
                               ),#close bracket for sliderInput
                               
                               #submitButton("Apply changes")
                               
                        ),#close bracket with comma for column(4)
                        
                        column(8,
                               
                               tmapOutput("mapPlot"),
                               
                        ),#close bracket with comma for column(8)
                        
                      ),#close bracket with comma for fluidRow()
                      
                      fluidRow(
                        column(6,
                               
                               titlePanel("Locations Visited"),
                               
                               DT::dataTableOutput(outputId = 'dtemply_table')
                               
                        ),#close bracket with comma for column(6)
                        
                        column(6, titlePanel("Visitation Data"),
                               
                               girafeOutput(outputId = "dtemply_loc")
                               
                        ),#close bracket with comma for column(6)
                        
                      ),#close bracket with comma for fluidRow()
                      
             ),#close bracket with comma for Plot
             
             
             ####RT Transaction Amount Analysis UI####
             tabPanel("Transaction Amount Analysis",
                      
                      titlePanel("Transaction Amount Analysis"),
                      
                      fluidRow(
                        column(2,
                               selectInput(
                                 inputId = "rtlocationcat",
                                 label = "Location Category",
                                 choices = unique(distinctloc$category),
                                 selected = distinctloc$category[1])
                               #submitButton("Apply Selected")
                        ),#close bracket with comma for column(2)
                        
                        column(10,
                               plotlyOutput(outputId = "TxnBoxPlotA",
                                            width = "100%",
                                            height = "700px")
                        )#close bracket with comma for column(10)
                        
                      ),#close bracket with comma for fluidRow()
                      
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
                                   choices = sort(unique(cc_data$last4ccnum))
                                 )
                               ),#close bracket with comma for conditionalPanel
                               conditionalPanel(
                                 condition = "input.rtradio == 'Loyalty Card'",
                                 selectInput(
                                   inputId = "rtloyalcard",
                                   label = "Loyalty Card No.",
                                   choices = sort(unique(loyalty_data$loyaltynum))
                                 )
                               )#close bracket wo comma for conditionalPanel
                               
                               #submitButton("Apply Selected")
                        ),#close bracket with comma for column(2)
                        
                        column(10,
                               conditionalPanel(
                                 condition = "input.rtradio == 'Credit Card'", br(),
                                 plotlyOutput(outputId = "TxnScatterCredit")
                               ),#close bracket with comma for conditionalPanel
                               conditionalPanel(
                                 condition = "input.rtradio == 'Loyalty Card'", br(),
                                 plotlyOutput(outputId = "TxnScatterLoyalty")
                               ),#close bracket with comma for conditionalPanel
                               
                        ),#close bracket with comma for column(10)
                        
                      ),#close bracket with comma for fluidRow()
                      
             )#close bracket without comma, maybe because last TabPanel, for Txn Analysis
             
  ) #close brackets for navbarMenu, do not need comma
  
) #close brackets for navbarPage, do not need comma

#server codes
server <- function(input, output, session) {
  #output$debug <- renderText({input$person2})
  ####RT Parallel Coord Server Codes####
  #########################
  ###For Parallel Coord ###
  #########################
  output$rtparacoord <- renderParcoords({
    
    #Matching credit card and loyalty card transactions
    
    ccloyalty <- full_join(cc_data, loyalty_data, by = c("date" = "timestamp", "price" = "price", "location" = "location")) %>% 
      left_join(distinctloc, by = "location") %>% 
      dplyr::select(-c("category.x","category.y")) %>%
      mutate(hour = hms::as_hms(round_date(timestamp,"60 mins"))) %>% 
      mutate(lcnum = str_sub(loyaltynum,start = -4))
    
    #Filtering to user selection and plotting
    
    selectedccloyalty <- ccloyalty %>%
      filter(between(last4ccnum,input$rtslider[1],input$rtslider[2])) %>%
      mutate(last4ccnum = as.character(last4ccnum)) %>%
      mutate(ccnum = str_c("C",last4ccnum)) %>% 
      dplyr::select(input$rtlevelone,input$rtleveltwo,input$rtlevelthree,
                    input$rtlevelfour,input$rtlevelfive)
    
    parcoords(
      selectedccloyalty,
      rownames = FALSE,
      reorderable = T,
      brushMode = '1D-axes-multi'
    )#close bracket for parcoords
    
  })#close brackets for output$paracoord
  
  #### NK Email Biodata Server Codes####

  
  observeEvent(input$view_select,{
    
    if (input$view_select=="Person"){
      
      shinyjs::hide(id = "biodata_select2")
      shinyjs::show(id = "biodata_select")
      shinyjs::show(id = 'biodata_output')
      shinyjs::hide(id = 'overview_output')
      
    }else if(input$view_select=="Overview"){
      
      shinyjs::hide(id = "biodata_select")
      shinyjs::show(id = "biodata_select2")
      shinyjs::hide(id = 'biodata_output')
      shinyjs::show(id = 'overview_output')
      
    }
    
    
  })
  
  observeEvent(input$biodata_select2, {
    
    if (input$biodata_select2 =='Timings of Emails'){
      shinyjs::show(id = 'biodata_select')
      
    }else{
      shinyjs::hide(id = "biodata_select")
    }
    
  })
  
  output$overview_output <- renderPlotly({
    
    if (input$biodata_select2=='Age Comparison'){
      
      df.emp <-df.emp %>% 
        mutate(Approx_age = 2014 - year(BirthDate))
      
      age_df <- df.emp %>% 
        select(FullName, Approx_age, CurrentEmploymentType) %>% 
        mutate(FullName2 = fct_reorder(FullName,CurrentEmploymentType)) %>%
        mutate(hjust = ifelse(Approx_age >60, 1.1, -0.1)) %>%
        mutate(vjust =  ifelse(FullName2 == "Varja Lagos",0.5,10))
      
      g<- age_df %>% 
        ggplot(aes(x = Approx_age, y= FullName2)) +
        geom_point(aes(fill = CurrentEmploymentType), size=5, shape=21,stroke=0.1, color='black',alpha=0.7)  +
        scale_fill_manual(values=c("#97C2FC","#FFFF00", "#FB7E81", "#7BE141","#EB7DF4","#7C29F0"))+
        geom_text(aes(label=FullName2), vjust = 3, size=3, fontface = "bold")+
        scale_x_continuous(breaks = c(10, 20, 25,30,35,40,45,50,55,60))+
        expand_limits( y = c(-3, length(levels(age_df$FullName)) + 3)) +
        labs(y="",x="Time",title = "Approximate Ages of GasTech Employees in 2014", color="Department") +
        theme(legend.text = element_text(size = 8),
              panel.background = element_rect(fill="white"),
              panel.grid.major.x = element_line(color="#c9c9c9", linetype = 3, size=0.1),
              panel.border = element_rect(color="grey", fill=NA),
              plot.title = element_text(size=14),
              axis.text.y = element_blank(),
              axis.ticks.x  = element_blank(),
              axis.ticks.y  = element_blank()
        )
      
      gg <- ggplotly(g) %>% layout(height = 700)
      gg
      
      
    }else if(input$biodata_select2=='Year Joined'){
      year_labels <- unique(substr(sort(year(df.emp$CurrentEmploymentStartDate)),3,4))
      year_labels[1] <- "1990"
      year_labels[20] <- "2013"
      
      g<- df.emp %>% 
        select(FullName, CurrentEmploymentStartDate, CurrentEmploymentType) %>% 
        mutate(FullName2 = fct_reorder(FullName,CurrentEmploymentType)) %>%
        mutate(Year  = year(CurrentEmploymentStartDate )) %>% 
        mutate(hjust = ifelse(FullName2 =="Sten Sanjorge Jr.", -0.1, 1.1)) %>%
        ggplot(aes(x = Year, y= FullName2)) +
        geom_point(aes(fill = CurrentEmploymentType), size=5, shape=21, color='black', stroke=0.1, alpha=0.7) +
        geom_text(aes(label=FullName2, hjust =  hjust), size=3) +
        scale_x_continuous(breaks = sort(unique(year(df.emp$CurrentEmploymentStartDate))), labels = year_labels) +
        scale_fill_manual(values=c("#97C2FC","#FFFF00", "#FB7E81", "#7BE141","#EB7DF4","#7C29F0"))+
        labs(y="",x="Year",title = "Year Employee of GasTech Joined", color="Department") +
        theme(legend.text = element_text(size = 5),
              panel.background = element_rect(fill="white"),
              panel.grid.major.x = element_line(color="#c9c9c9", linetype = 3, size=0.1),
              panel.border = element_rect(color="grey", fill=NA),
              plot.title = element_text(size=14),
              axis.text.y = element_blank(),
              axis.ticks.x  = element_blank(),
              axis.ticks.y  = element_blank()
        )
      gg <- ggplotly(g)
      
      gg
    
    }else if(input$biodata_select2=='Military Service'){
      
      df.emp <- df.emp %>% 
        mutate(army_year = year(MilitaryDischargeDate)) %>%
        mutate(disch = paste(MilitaryServiceBranch, army_year)) %>%
        mutate(MilitaryStartYear = year(MilitaryDischargeDate)-2) %>%
        mutate(MilitaryDischargeYear = year(MilitaryDischargeDate))
      
      mil_data<-df.emp %>%
           filter(!is.na(MilitaryDischargeYear)) %>%
           mutate(ArmyLoc =  ifelse(str_detect(MilitaryServiceBranch,"Kronos"),"Kronos","Tethys")) 
      
      
      fig_1 <- ggplotly(mil_data %>%
                       ggplot(aes(y = FullName, x = MilitaryStartYear)) +
                       geom_point(size=2, color="#81b879") +
                       geom_point(aes(MilitaryDischargeYear), color="#769ab5", size=2) +
                       geom_segment(aes(x=MilitaryStartYear, xend=MilitaryDischargeYear, y=FullName,yend=FullName, color=ArmyLoc), size=0.5, alpha=0.7) +
                       scale_x_continuous(breaks = seq(min(mil_data$MilitaryStartYear), max(mil_data$MilitaryDischargeYear), by=2))+
                       #scale_color_manual(values = c("#97C2FC","#7BE141","#AD85E4"))+
                       #facet_wrap(~ArmyLoc, scales = "free") +
                       labs(x = "Year",y ="", title="Time Served in the Military in Kronos", legend='Location')+
                       theme(panel.background = element_rect(fill ="white"),
                             panel.grid.major.y = element_line(colour = "grey",linetype = 3),
                             panel.grid.major.x = element_line(colour = "grey",linetype = 3),
                             panel.border = element_rect(color="grey", fill=NA),
                             axis.ticks.x  = element_blank(),
                             axis.ticks.y  = element_blank())) %>% layout(hoverlabel=list(bgcolor="white"))
      fig_1
      
    }
    
    
  })
  
  
  output$biodata_output <- renderTable(rownames = F,bordered = F,striped = T,align = 'c', {
    
    
    df_tmp <- as.data.frame(t(subset(df.emp, FullName==input$biodata_select)))
    df_tmp <- cbind(newColName = rownames(df_tmp), df_tmp)
    rownames(df_tmp) <- 1:nrow(df_tmp)
    
    colnames(df_tmp) <- c('Bio-data','Details')
    
    df_tmp
    
    
  }) 
  
  
  
  output$info_message<- renderText({if(input$go == 0){paste("Click on the Display Button to render the output")} else{return()}})
  
  # observeEvent(input$about,
  #              {showModal(modalDialog(title = "Help Box",
  #                                     "Adjust the minimum and maximum width of the edges"))
  #              })
  # 
  output$timings_email_1 <- renderPlotly({
    
      
      tmp_person_email_From <- x_full %>% 
        filter(From==input$person) 
      
      tmp_person_email_From_word <- tmp_person_email_From %>% 
                                    unnest_tokens(word, Subject2) %>%
                                    anti_join(stop_words) %>%
                                    group_by(To) %>%
                                    count(word) %>%
                                    summarise(word = paste(word, collapse = ","))
      
      tmp_person_email_From <- tmp_person_email_From %>% 
                               inner_join(tmp_person_email_From_word, by = "To")            
      
      
      a <- list(
        text = paste("From", input$person),
        #font = f,
        xref = "paper",
        yref = "paper",
        yanchor = "bottom",
        xanchor = "center",
        align = "center",
        x = 0.5,
        y = 1,
        showarrow = FALSE
      )
      
      
      fig_1 <- ggplotly(ggplot(tmp_person_email_From, aes(x=Date.Time, y=To)) + 
                          geom_point(shape=21,
                                     aes(color = factor(word), 
                                         text = sprintf("Words : \n %s", str_replace_all(word,',','\n'))))+
                          
                          theme(legend.title = element_blank(),
                                panel.background = element_rect(fill="white"),
                                panel.grid.major.y = element_line(color="#f0f0f0"),
                                plot.title = element_text(size=14),
                                axis.title = element_blank()),tooltip = c("text")) %>% 
                layout(hoverlabel=list(bgcolor="white"), annotations=a)
      
      
      
      
      
      fig_1 <- style(fig_1, showlegend = FALSE)

      
      fig_1
    
  })
  
  
  output$timings_email_2 <- renderPlotly({
    
    
    
    tmp_person_email_To <- x_full %>% 
      filter(To==input$biodata_select) 
    
    tmp_person_email_To_word <- tmp_person_email_To %>% 
      unnest_tokens(word, Subject2) %>%
      anti_join(stop_words) %>%
      group_by(From) %>%
      count(word) %>%
      summarise(word = paste(word, collapse = ","))
    
    tmp_person_email_To <- tmp_person_email_To %>% inner_join(tmp_person_email_To_word, by = "From")
    
  
    
    b <- list(
      text = paste("To", input$person),
      #font = f,
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    )
    
 
    
    fig_2 <- ggplotly(ggplot(tmp_person_email_To, aes(x=Date.Time, y=From)) + 
                        geom_point(shape=21,
                                   aes(color = factor(word),
                                       text = sprintf("Words : \n %s", str_replace_all(word,',','\n'))))+
                        
                        theme(legend.title  = element_blank(),
                              panel.background = element_rect(fill="white"),
                              panel.grid.major.y = element_line(color="#f0f0f0"),
                              plot.title = element_text(size=14),
                              axis.title = element_blank()), tooltip = c("text")) %>% 
      layout(hoverlabel=list(bgcolor="white"), annotations=b)
    
    
    
    #fig_1 <- style(fig_1, showlegend = FALSE)
    fig_2 <- style(fig_2, showlegend = FALSE)
    
    #fig <- subplot(fig_1, fig_2, 
    #               nrows = 2, shareX=F, shareY=F, 
    #               titleX = T, titleY = T) #%>% layout(xaxis = list(domain=list(x=c(0,0.5),y=c(0,0.5))))
    
    fig_2
    
  })
  
  output$email_convo <- renderPlotly({
    
    dat <- x_full %>%
      filter(From == input$person) %>%
      filter(Date.Time >= as_hms(str_extract(input$time[1], 
                                             pattern = '\\d\\d:\\d\\d:\\d\\d')), 
             
             Date.Time <= as_hms(str_extract(input$time[2], 
                                             pattern = '\\d\\d:\\d\\d:\\d\\d')) 
      ) %>%
      filter(Date.Date >= input$date[1], 
             Date.Date <= input$date[2])
    
    details <- x_full %>% 
      filter(From == input$person) %>% 
      select(From_dep, From_title)
    
    tryCatch({
      
      g <- ggplot(dat, aes(y=To, x=Date.Time))+ 
        geom_point(#position=position_dodge(width=1),
          size=2, alpha = 0.6, stroke=0.5, shape=21,
          aes(text = sprintf("Sub: %s<br>Date: %s<br>To: %s<br>To_Title: %s<br>To_Dep: %s<br>Time Sent: %s<br>", 
                             Subject2, 
                             Date.Date, 
                             To,
                             To_title, 
                             To_dep,
                             strftime(Date.Time, format = '%H:%M')
          )
          )
        ) +
        geom_line(aes(group = Subject2, color= Subject2), size=0.2) +
        labs(y="",x="Time",title = paste(input$person,paste(unique(details), collapse = ',')), 
             group="", 
             color="") +
        theme(legend.text = element_text(size = 5),
              panel.background = element_rect(fill="white"),
              panel.grid.major.y = element_line(color="#f0f0f0"),
              plot.title = element_text(size=14))
      
      gg <- ggplotly(g, tooltip = c("text")) 
      gg
      gg }, error = function(e){
        
        text = paste("There was no email correspondence in this time bracket",
                     '<br>',
                     "Please try a different time bracket on the slider")
        ggplot() + 
          annotate("text", x = 0, y = 20, size=8, label = text) + 
          theme(element_blank(), 
                axis.ticks = element_blank(),
                axis.text = element_blank(),
                axis.title = element_blank(),
                panel.background = element_rect(fill='white')
          )
        
      }
    )
    
  })#close brackets for output$email_convo
  
  ####NK temp_g_vf Server Codes####
 
  temp_g_vf <- eventReactive(input$go, {
    
    if (input$dt_select == 'Person'){
      
      m <- x_full %>%
        mutate(s = str_replace(Subject,"RE: ",""))
        
      temp_g <- m %>% 
        filter(To == input$person2 | From == input$person2)
      
      temp_g <- temp_g %>% 
        select(From, From_title, To, To_title, Date.Date, Date.Time, Subject) 
      
    }else if (input$dt_select == 'Keywords'){
      m <- x_full %>%
        mutate(s = str_replace(Subject,"RE: ","")) %>%
        mutate(s = str_to_lower(s))
      
      
      temp_g <- m %>% 
        filter(str_detect(s, str_replace_all(input$search,',','|')))
      
      temp_g <- temp_g %>% 
        select(From, From_title, To, To_title, Date.Date, Date.Time, Subject) 
      
    }else if (input$dt_select == 'Both'){
      m <- x_full %>%
        mutate(s = str_replace(Subject,"RE: ","")) %>%
        mutate(s = str_to_lower(s))
      
      
      temp_g <- m %>% 
        filter(To == input$person2 | From == input$person2) %>%
        filter(str_detect(s, str_replace_all(input$search,',','|')))
      
      temp_g <-temp_g %>% 
        select(From, From_title, To, To_title, Date.Date, Date.Time, Subject) 
    }
    
  })#close brackets for temp_g_vf
  
  ####NK output$table Server Codes####
  
  output$table <- DT::renderDT({
    DT::datatable(data = temp_g_vf(), 
                  fillContainer = F, 
                  options = list(pageLength = 7),
                  rownames = F)
  }) #close brackets for output$table

  ####NK output$vis_dept_TEXT Server Codes####
  
  get_data <- reactive(
    if(input$ntwk_select =='Email Subjects'){
      
      PrepText <- function(textdata, groupvar, textvar, node_type = c("groups","words"), 
                           tokenizer = c("words", "tweets"), pos = c("all", "nouns"),
                           language = "english", remove_stop_words = FALSE, 
                           remove_numbers = NULL, compound_nouns = FALSE,
                           udmodel_lang = NULL,
                           ...) {
        
        # remove non-UTF8 characters
        textdata[[textvar]] <- iconv(textdata[[textvar]],  to="UTF-8", sub='')
        
        # remove emojis, symbols, and meta characters from tweets
        if (tokenizer=="tweets") {
          textdata[[textvar]] <- gsub("&amp;|&lt;|&gt;|RT", "", textdata[[textvar]])
          if (!is.null(remove_numbers) && isTRUE(remove_numbers)) { # && evaluates arg two only if arg one is true
            textdata[[textvar]]<-gsub("\\b\\d+\\b", "",textdata[[textvar]])
          }
        }
        
        if(is.null(udmodel_lang)){
          # udpipe setup
          # download udpipe language model
          lang_mod <- udpipe_download_model(language = language)
          # set up udpipe language model for pos tagging
          udmodel_lang <- udpipe_load_model(file = lang_mod$file_model)
        }
        
        ## DEFAULT: ANNOTATE WORDS NOT COMPOUND NOUNS
        if (isFALSE(compound_nouns)){
          
          textdata_tokens <- as_tibble({{textdata}}) %>%
            select({{groupvar}}, {{textvar}}) %>%
            unnest_tokens(output = "word", input = {{textvar}}, token = {{tokenizer}}, ...)
          
          # get part of speech with udpipe
          # annotate for pos only w/ pre-tokenized data
          # following: https://cran.r-project.org/web/packages/udpipe/vignettes/udpipe-annotation.html#annotate_your_text
          textdata_pos <- as.data.frame(udpipe_annotate(udmodel_lang, x = textdata_tokens$word,
                                                        tagger = "default", parser = "none",
                                                        tokenizer = "vertical"))
          
          # combine part of speech and textdata
          textdata <- bind_cols(textdata_tokens, textdata_pos[, c("upos", "lemma")])
        }
        
        
        ## IF SPECIFIED: ANNOTATE WORDS AND COMPOUND NOUNS
        if (isTRUE(compound_nouns)){
          
          # we use tidytext to flexibly tokenize words or tweets
          textdata_tokens <- as_tibble({{textdata}}) %>%
            select({{groupvar}}, {{textvar}}) %>%
            unnest_tokens(output = "word", input = {{textvar}}, token = {{tokenizer}}, strip_punct = FALSE, ...)
          
          # then we prepare the tokenized documents for dependency parsing
          textdata_tokens <- textdata_tokens %>% 
            group_by_({{groupvar}}) %>% 
            summarise(documents = paste(word, collapse = "\n"))
          
          # parse dependencies with udpipe
          textdata_dep <- as.data.frame(udpipe_annotate(udmodel_lang, x = textdata_tokens$documents,
                                                        doc_id = textdata_tokens[[groupvar]],
                                                        tagger = "default", parser = "default"))
          
          # NOUN COMPOUNDS
          # retrieve noun compounds
          # row numbers of all compound elements
          noun_compound <- which(textdata_dep$dep_rel=="compound")
          # list of consecutive compound elements
          compound_elements <- split(noun_compound, cumsum(c(1, diff(noun_compound) != 1)))
          # vector of compound bases
          compound_bases <- mapply(`[[`, compound_elements, lengths(compound_elements))+1
          # add compound bases to compound list
          all_compound_elements <- mapply(c, compound_elements, compound_bases, SIMPLIFY = FALSE)
          # retrieve all text elements and collapse them to get compound nouns
          compound_nouns <- sapply(all_compound_elements, function(x) paste0(textdata_dep$lemma[x], collapse = " "))
          
          # assign compound nouns to compound bases 
          textdata_dep$lemma[compound_bases] <- compound_nouns
          
          # remove compound elements and punctuation from dataframe
          textdata_dep <- textdata_dep %>% 
            filter(dep_rel!="compound" & upos!="PUNCT")
          
          # rename df and groupvar to avoid redudant coding
          textdata <- textdata_dep
          names(textdata)[1] <- groupvar
        }
        
        # remove stopwords
        if (remove_stop_words) {
          textdata <- {{textdata}} %>% 
            anti_join(get_stopwords(language = language), by = c("lemma" = "word"))
        }
        
        # subset to nouns and proper nouns (if desired)
        if (length(pos)>1){
          warning(paste0("You did not specify `pos`. Function defaults to all parts of speech."))
          pos <- "all"
        }
        if (pos=="nouns"){
          textdata <- {{textdata}} %>% 
            filter(upos%in%c("NOUN", "PROPN"))
        }
        
        # count word occurences within grouping variable
        if (length(node_type)>1){
          warning(paste0("You did not specify a `node_type`. Returned nodes are ", groupvar, "."))
          node_type <- "groups"
        }
        
        if (node_type=="groups"){
          # count terms by group
          textdata <- {{textdata}} %>%
            group_by_({{groupvar}}) %>%
            count(lemma) %>%
            rename(count = n)
        }
        
        if (node_type=="words"){
          # count groups by term
          textdata <- {{textdata}} %>%
            group_by(lemma) %>%
            count_({{groupvar}}) %>%
            rename(count = n)
        }
        
        return({{textdata}})
      }
      
      CreateTextnet <- function(tidytextobject){
        
        # determine network type
        node_type <- ifelse(names(tidytextobject)[1]=="lemma",
                            "words",
                            "groups")
        
        # determine if regular or signed nets
        nets_type <- ifelse(ncol(tidytextobject)>3,
                            "signed",
                            "regular")
        
        # remove grouping allow arranging after adding tfidf
        tidytextobject <- ungroup(tidytextobject)
        
        # create adjacency matrix for node_type groups
        if(node_type=="groups"){
          
          # rename grouping variable for easier referencing
          names(tidytextobject)[1] <- "group"
          
          # add tfidf for adjacency matrix
          for_adjacency <- tidytextobject %>%
            # calculate tfidf
            bind_tf_idf(lemma, group, count) %>%
            # sort on lemma
            arrange(lemma)
          
          # remove lemmas used by only one author
          for_adjacency <- for_adjacency %>% 
            group_by(lemma) %>%
            filter(n() > 1) %>% 
            ungroup()
          
          # create sparse matrix of tfidfs
          # this appears to work and provides a convenient wrapper around sparseMatrix
          # the error appears to be related to some deprecated functions
          suppressWarnings(for_crossprod <- cast_sparse(for_adjacency, row = group, col = lemma, value = tf_idf))
          for_crossprod <- for_crossprod[sort(rownames(for_crossprod)),]
          
          # create sparse matrix of sentiment scores and multiply with tfidf matrix
          if(nets_type=="signed"){
            suppressWarnings(sent_matrix <- cast_sparse(for_adjacency, row = group, col = lemma, value = sentiment))
            sent_matrix <- sent_matrix[sort(rownames(sent_matrix)),]
            for_crossprod <- sent_matrix * for_crossprod
          }
          
          # the line above is not working with the noun phrase function
          # create weighted adjacency matrix
          weighted_adjacency <- Matrix::tcrossprod(for_crossprod)
          # create igraph object
          text_network <- graph.adjacency(weighted_adjacency, mode="undirected", weighted = TRUE, diag = FALSE)
        }
        
        # create adjacency matrix for node_type words
        if(node_type=="words"){
          
          # rename grouping variable for easier referencing
          names(tidytextobject)[2] <- "group"
          
          # add tfidf for adjacency matrix
          for_adjacency <- tidytextobject %>%
            # calculate tfidf
            bind_tf_idf(group, lemma, count) %>%
            # sort on lemma
            arrange(group)
          
          # create sparse matrix
          # this appears to work and provides a convenient wrapper around sparseMatrix
          # the error appears to be related to some deprecated functions
          suppressWarnings(for_crossprod <- cast_sparse(for_adjacency, row = lemma, col = group, value = tf_idf))
          for_crossprod <- for_crossprod[sort(rownames(for_crossprod)),]
          
          # create sparse matrix of sentiment scores and multiply with tfidf matrix
          if(nets_type=="signed"){
            suppressWarnings(sent_matrix <- cast_sparse(for_adjacency, row = group, col = lemma, value = sentiment))
            sent_matrix <- sent_matrix[sort(rownames(sent_matrix)),]
            for_crossprod <- sent_matrix * for_crossprod
          }
          
          # create weighted adjacency matrix
          weighted_adjacency <- Matrix::tcrossprod(for_crossprod)
          # create igraph object
          text_network <- graph.adjacency(weighted_adjacency, mode="undirected", weighted = TRUE, diag = FALSE)
        }
        
        return(text_network)
        
      }
      
      emails_grouped<- df.emails %>% 
        mutate(To = str_split(To,pattern=',')) %>% 
        unnest_longer(To) %>% 
        mutate(To = str_trim(To),
               From = str_trim(From)) %>%
        filter(!(From==To)) %>%
        unnest_tokens(word, Subject) %>%
        anti_join(stop_words) %>% 
        group_by(From,To) %>%
        count(word) %>%
        summarise(word = paste(word, collapse = ",")) %>%
        ungroup() %>%
        group_by(From) %>%
        left_join(df.emp, by=c("From"="FullName")) %>%
        select(c(From, To, word, CurrentEmploymentType)) %>%
        rename(group = CurrentEmploymentType) %>%
        replace_na(replace=list(From_dep = "Executive"))
      
      
      email_text <- PrepText(emails_grouped, 
                                      groupvar = "From", 
                                      textvar = "word", 
                                      node_type = "groups", 
                                      tokenizer = "words", 
                                      compound_nouns = TRUE)
      
      email_text_nt <- CreateTextnet(email_text)
      return(email_text_nt)
    })

  
  
  observeEvent(input$ntwk_select,{
    
    if (input$ntwk_select =='Email Subjects'){
      
      shinyjs::hide(id = "node_sizings")
      shinyjs::show(id = "node_sizings2")
      #shinyjs::hide(id = "Modifying Aesthetics")
      shinyjs::hide(id = "arrow")
      shinyjs::hide(id = "about")
      shinyjs::hide("text_div_MA")
      shinyjs::hide(id = "min_width")
      shinyjs::hide(id = "max_width")
      shinyjs::hide(id='sub_graph')
      
    }else if(input$ntwk_select =='Email Correspondence'){
      
      shinyjs::show(id = "node_sizings")
      shinyjs::hide(id = "node_sizings2")
      #shinyjs::show(id = "text_div_MA")
      shinyjs::show("text_div_MA")
      shinyjs::show(id = "arrow")
      shinyjs::show(id = "about")
      shinyjs::show(id = "min_width")
      shinyjs::show(id = "max_width")
      shinyjs::show(id='sub_graph')
      
    }
    
  })
  
  observeEvent(input$ntwk_select, {
  if (input$ntwk_select =='Email Subjects') {
    
  output$vis_dept <- renderVisNetwork({

    showModal(modalDialog("Network under Construction!", footer="Please wait.."))
    
    VisTextNet_e_2 <- function(text_network, alpha = .25, label_degree_cut=0, df){
      
      if (igraph::has.multiple(text_network))
        stop("textnets does not yet support multiple edges")
      if (is.null(V(text_network)$name)){
        text_network <- set_vertex_attr(text_network, "name", value = as.character(1:vcount(text_network)))
      }
      
      #create network backbone 
      
      e <- cbind(igraph::as_data_frame(text_network)[, 1:2 ], 
                 weight =   E(text_network)$weight)
      
      # in
      w_in <- graph.strength(text_network, mode = "in")
      w_in <- data.frame(to = names(w_in), w_in, stringsAsFactors = FALSE)
      k_in <- degree(text_network, mode = "in")
      k_in <- data.frame(to = names(k_in), k_in, stringsAsFactors = FALSE)
      
      e_in <- e %>%
        left_join(w_in, by = "to") %>%
        left_join(k_in, by = "to") %>%
        mutate(alpha_in = (1-(weight/w_in))^(k_in-1))
      
      # out
      
      w_out <- graph.strength(text_network, mode = "out")
      w_out <- data.frame(from = names(w_out), w_out, stringsAsFactors = FALSE)
      k_out <- degree(text_network, mode = "out")
      k_out <- data.frame(from = names(k_out), k_out, stringsAsFactors = FALSE)
      
      e_out <- e %>%
        left_join(w_out, by = "from") %>%
        left_join(k_out, by = "from") %>%
        mutate(alpha_out = (1-(weight/w_out))^(k_out-1))
      
      e_full <- left_join(e_in, e_out, by = c("from", "to", "weight"))
      
      e_full <- e_full %>%
        mutate(alpha = ifelse(alpha_in < alpha_out, alpha_in, alpha_out)) %>%
        select(from, to, alpha)
      
      E(text_network)$alpha <- e_full$alpha
      
      pruned <- delete.edges(text_network, which(E(text_network)$alpha >= alpha))
      pruned <- delete.vertices(pruned, which(degree(pruned) == 0))
      
      
      isolates <- V(pruned)[degree(pruned)==0]
      pruned <- delete.vertices(pruned, isolates)
      
      
      size <- 25
      
      if (input$node_sizings2=='Betweenness'){
        
        nodes <- data.frame(id = V(pruned)$name, 
                            #title = paste0("Degree of Node: <br>",
                            #               V(pruned)$degree),
                            size =  ifelse(round(betweenness(pruned))==0,10,round(betweenness(pruned)))
                            )
        
      }else if (input$node_sizings2=='Degree'){
        nodes <- data.frame(id = V(pruned)$name, 
                            #title = paste0("Degree of Node: <br>",
                            #               V(pruned)$degree),
                            size =  round(degree(pruned))
        )
        
      }else{
        nodes <- data.frame(id = V(pruned)$name, 
                            #title = paste0("Degree of Node: <br>",
                            #               V(pruned)$degree),
                            size =  size
                            )
      }
      
      nodes<-nodes %>% 
        left_join(select(df, FullName,CurrentEmploymentType), by= c("id"="FullName")) %>% 
        rename(group= CurrentEmploymentType) %>%
        replace_na(list(group="Executive"))
      
      nodes <- nodes[order(nodes$id, decreasing = F),]
      nodes$shadow <- TRUE
      nodes <- nodes %>% mutate(font.size = 20, font.weight= 900)
      nodes$color.highlight.background <- "brown"
      
      edges <- get.data.frame(pruned, what="edges")[1:2]
      edges$value <- E(pruned)$weight
      nodes$color.highlight.background <- "brown"
      
     
      
      visNetwork(nodes, edges) %>%
        visLayout(randomSeed = 123) %>% 
        visOptions(highlightNearest = TRUE, 
                   nodesIdSelection = TRUE, 
                   selectedBy = "group") %>% 
        visNodes(labelHighlightBold = T, size=20) %>%
        visEdges(width = 0.01,length = 10, scaling = list(min=0.1,max=3)) %>%
        visIgraphLayout(layout = 'layout.davidson.harel') %>%
        visInteraction(multiselect = TRUE) %>%
        visLegend(zoom = F)

      
    }
    
    set.seed(390)
    
    p<-VisTextNet_e_2(get_data(), df = df.emp)
    
    removeModal()
    
    p
    
  })
  
  } else{
    output$vis_dept <- renderVisNetwork({
      
      links <- df.emails %>% 
        mutate(To = str_split(To,pattern=',')) %>% 
        unnest_longer(To) %>% 
        mutate(To = str_trim(To),
               From = str_trim(From)) %>%
        filter(!(From==To)) %>%
        group_by(From, To) %>%
        summarise(count=n()) %>%
        rename(weight = count)
      
      
      nodes_df <- data.frame(id = unique(c(links$From, links$To))) %>%
        left_join(df.emp, by = c("id"="FullName")) %>% 
        select(c(id, 
                 CurrentEmploymentTitle, 
                 CurrentEmploymentType)) %>%
        rename(title = CurrentEmploymentTitle, 
               department = CurrentEmploymentType) %>%
        replace_na(list(department = "Executive", 
                        title = "CEO"))
      
      email_network <- graph_from_data_frame(d = links, 
                                             vertices = nodes_df, 
                                             directed = T)
      
      email_network <- simplify(email_network)
      
      
      
      if (input$node_sizings=='None'){
        
        nodes <- data.frame(id = V(email_network)$name, 
                            title = V(email_network)$name, 
                            group = V(email_network)$department)
        
      }else if (input$node_sizings=='Betweenness'){
        sizing <- data.frame(size = round(betweenness(email_network)/5))
        
        nodes <- data.frame(id = V(email_network)$name, 
                            title = V(email_network)$name, 
                            group = V(email_network)$department,
                            size = sizing)
        
      }else if (input$node_sizings=='Degree'){
        sizing <- data.frame(size = round(degree(email_network)*1.5))
        
        nodes <- data.frame(id = V(email_network)$name, 
                            title = V(email_network)$name, 
                            group = V(email_network)$department,
                            size = sizing)
        
      }else if (input$node_sizings=='Out-Degree'){
        sizing <- data.frame(size = round(degree(email_network, mode = 'out')*2))
        
        nodes <- data.frame(id = V(email_network)$name, 
                            title = V(email_network)$name, 
                            group = V(email_network)$department,
                            size = sizing)
      }else if (input$node_sizings=='In-Degree'){
        sizing <- data.frame(size = round(degree(email_network, mode='in')*2))
        
        nodes <- data.frame(id = V(email_network)$name, 
                            title = V(email_network)$name, 
                            group = V(email_network)$department,
                            size = sizing)
      }else if (input$node_sizings=='Closeness'){
        sizing <- data.frame(size = round(closeness(email_network)*10000))
        
        nodes <- data.frame(id = V(email_network)$name, 
                            title = V(email_network)$name, 
                            group = V(email_network)$department,
                            size = sizing)
      }
      
      
      
      if (input$communities=='None'){
        
        nodes$group <- 1
        leg <- F
        
      }else if (input$communities=='Department'){
        
        nodes
        leg <- T
        
      
      }else if (input$communities=="Cluster Lovain"){
        leg <- F
        
        email_network2 <- as.undirected(email_network)
        
        communities<-cluster_louvain(email_network2)
        
        results<-data.frame(cbind(communities$names, 
                                  communities$membership),
                            stringsAsFactors = FALSE)
        
        names(results)<-c("id","group")
        
        nodes <- nodes %>% select(-c(group))
        nodes <- nodes %>% left_join(results)
        

      }else if (input$communities=="Betweenness"){
        leg <- F
        
        email_network2 <- as.undirected(email_network)
        
        communities<-cluster_edge_betweenness(email_network2)
        
        results<-data.frame(cbind(communities$names, 
                                  communities$membership),
                            stringsAsFactors = FALSE)
        
        names(results)<-c("id","group")
        
        nodes <- nodes %>% select(-c(group))
        nodes <- nodes %>% left_join(results)
        
        
      }else if (input$communities=="Label Propagation"){
        leg <- F
        
        email_network2 <- as.undirected(email_network)
        
        communities<-cluster_label_prop(email_network2)
        
        results<-data.frame(cbind(communities$names, 
                                  communities$membership),
                            stringsAsFactors = FALSE)
        
        names(results)<-c("id","group")
        
        nodes <- nodes %>% select(-c(group))
        nodes <- nodes %>% left_join(results)
        
        
      }else if (input$communities=="Fast Greedy"){
        leg <- F
        
        email_network2 <- as.undirected(email_network)
        
        communities<-cluster_fast_greedy(email_network2)
        
        results<-data.frame(cbind(communities$names, 
                                  communities$membership),
                            stringsAsFactors = FALSE)
        
        names(results)<-c("id","group")
        
        nodes <- nodes %>% select(-c(group))
        nodes <- nodes %>% left_join(results)
        
        
      }else if (input$communities=="Leading Eigenvector"){
        
        leg <- F
        email_network2 <- as.undirected(email_network)
        
        communities<-cluster_leading_eigen(email_network2)
        
        results<-data.frame(cbind(communities$names, 
                                  communities$membership),
                            stringsAsFactors = FALSE)
        
        names(results)<-c("id","group")
        
        nodes <- nodes %>% select(-c(group))
        nodes <- nodes %>% left_join(results)
        
        
      }
      
      
      
      edges <- get.data.frame(email_network, what="edges")[1:2]
      edges$value <- links$weight
      #edges$color.opacity <- 0.8
      #edges$color.highlight <- "red"
      
      nodes$color.highlight.background <- "brown"
      
      nodes <- nodes %>% 
        mutate(font.size = 25,
               font.weight= 1000)
    
      
      if (input$arrow){
        show <- 'middle'
        
      }else{
        show<- NULL
      }
      
      if (input$nearest){
        hk <- T
      }else{
        hk <- F
      }
      if (input$multi_select){
        ms <-T
      } else{
        ms<-F
      }
      
      set.seed(399)
      
      p <- visNetwork(nodes = nodes, 
                      edges = edges,  width = "100%", height = 700) %>%
        visOptions(highlightNearest = list(enabled = T, degree = 1)#, 
                   #nodesIdSelection = T,
                   #selectedBy = "group"
                   
        ) %>% #arrows = show, 
        visEdges(arrows = show, width = 0.01,length = 10, scaling = list(min=input$min_width, max=input$max_width)) %>% 
        visLayout(randomSeed = 123) %>%
        visNodes(labelHighlightBold = T) %>%
        visIgraphLayout(layout = input$layout) %>% #"layout_nicely") %>%
        visInteraction(multiselect = ms) %>%
        visOptions(selectedBy = "group", highlightNearest = hk) %>%
        visLegend(zoom = T, width = 0.15, enabled = leg) %>%
        visEvents(selectNode = "function(nodes) {
            Shiny.onInputChange('current_node_id', nodes)
            
            ;}")
      p
      
      
    })
  }
    
  })

  
  observeEvent(input$go, {
    

  
  #### NK vis_email code ####
    
  output$vis_email <- renderVisNetwork({
    
    tryCatch({
      temp_g_links <- temp_g_vf() %>% 
        select(From, To, Subject) %>% 
        rename(group = Subject)
      
      temp_g_nodes <- data.frame(id = unique(c(temp_g_vf()$From, temp_g_vf()$To)))
      #temp_g_nodes$title <- temp_g_nodes$id
      
      temp_g_nodes <- temp_g_nodes %>% 
        left_join(select(df.emp, FullName, CurrentEmploymentType), by = c('id' = 'FullName')) %>%
        rename(group = CurrentEmploymentType) %>%
        replace_na(list(group = "Executive"))
      
      temp_g_nodes$color.highlight.background <- "brown"
      
      
      temp_graph <- graph_from_data_frame(d = temp_g_links,
                                          vertices = temp_g_nodes,
                                          directed = T)
      
      
      data <- toVisNetworkData(temp_graph)
      
      
      visNetwork(data$nodes, data$edges) %>%
        visEdges(arrows = "middle",width = 0.01,length = 5, scaling = list(min=0.1,max=3)) %>%
        visIgraphLayout() %>%
        visNodes(size=20) %>%
        visOptions(nodesIdSelection = T, highlightNearest = T) %>%
        visLegend(zoom = F) %>% 
        visInteraction(multiselect = TRUE)
        
    },
    error = function(e){visNetwork(nodes = as.data.frame(unique(x_full$From)), edges = x_full[1:2])})
    
  }) # close brackets for output$vis_email
  })
  ####
  
  
  #### NK observeEvent Server Codes####

  
  
  observeEvent(
    input$current_node_id, {
      
      output$vis_dept_sub <- renderVisNetwork({
        
       
        
        links <- df.emails %>% 
          mutate(To = str_split(To,pattern=',')) %>% 
          unnest_longer(To) %>% 
          mutate(To = str_trim(To),
                 From = str_trim(From)) %>%
          filter(!(From==To)) %>%
          group_by(From, To) %>%
          summarise(count=n()) %>%
          rename(weight = count) %>%
          filter(From == input$current_node_id$nodes | To == input$current_node_id$nodes)
        
        nodes_df <- data.frame(id = unique(c(links$From, links$To))) %>%
          left_join(df.emp, by = c("id"="FullName")) %>% 
          select(c(id, 
                   CurrentEmploymentTitle, 
                   CurrentEmploymentType)) %>%
          rename(title = CurrentEmploymentTitle, 
                 department = CurrentEmploymentType) %>%
          replace_na(list(department = "Executive", 
                          title = "CEO"))
        
        email_network <- graph_from_data_frame(d = links, 
                                               vertices = nodes_df
        )
        
        email_network <- simplify(email_network)
        
        if (input$color_sub){
          nodes <- data.frame(id = V(email_network)$name, 
                              title = V(email_network)$name, 
                              group = V(email_network)$department)
          leg <- T
        }else{
          nodes <- data.frame(id = V(email_network)$name, 
                              title = V(email_network)$name,
                              group=1)
          leg <- F
        }
        
        nodes$label <- nodes$id
        
        
        edges <- get.data.frame(email_network, what="edges")[1:2]
        edges$value <- links$weight
        #edges$color.opacity <- 0.8
        #edges$color.highlight <- "red"
        
        nodes$color.highlight.background <- "brown"
        
        nodes <- nodes %>% mutate(font.size = 25, 
                                  font.weight= 1000)
        
        
        if (input$arrow){
          show <- 'middle'
          
        }else{
          show<- NULL
        }
        
        set.seed(399)
        
        p <- visNetwork(nodes = nodes, 
                        edges = edges,  width = "100%", height = 700 #%>%
          #visOptions(highlightNearest = list(enabled = T, degree = 1)#, 
                     #nodesIdSelection = T,
                     #selectedBy = "group"
                     
                        ) %>% 
          visEdges(arrows = show, width = 0.01,length = 10, scaling = list(min=0.1,max=3)) %>% 
          visLayout(randomSeed = 123) %>%
          visNodes(labelHighlightBold = T) %>%
          #visPhysics(stabilization = 5,
          #           barnesHut = list(springLength =230, avoidOverlap=0.2), 
          #           forceAtlas2Based = list(gravitaionalConstant = -100,
          #                                   centralGravity = 0.5)) %>%
          visIgraphLayout(layout = "layout_nicely") %>%
          visInteraction(multiselect = TRUE) %>%
          visLegend(enabled = leg, zoom = F, position = 'left') 
         
        
      })#close brackets for output$vis_dept_sub, no comma
      
    }#close curly bracket for observeEvent, no comma
  )#close bracket for observeEvent, no comma
  
  ####DT Map Plotting Server Codes####
  #########################
  ##### Plot observe ######
  #########################
  
  observe({
    
    inputFirstName <- strsplit(input$dtemployee_name, " ")[[1]][1]
    inputLastName <- strsplit(input$dtemployee_name, " ")[[1]][2]
    
    selectedID <- car_data %>%
      filter(FirstName == inputFirstName & LastName == inputLastName) %>%
      distinct(CarID)
    
    selected_gps <- gps %>%
      filter(id == selectedID$CarID & datestamp == input$dtdate)
    
    dtemply_id <- selectedID$CarID
    dtemply_date <- input$dtdate
    dtemply_stop_time <- duration(input$dtduration, units = "minutes")
    
    dtemply_gps <- gps %>%
      filter(id == dtemply_id, datestamp == dtemply_date) %>%
      mutate(duration = seconds_to_period(Timestamp - lag(Timestamp)))
    
    dtemply_gps[order(dtemply_gps$Timestamp),]
    
    #check if the row is 1 or less
    n <- nrow(dtemply_gps)
    
    no_data <- ifelse(n <= 1, TRUE, FALSE)
    
    if(!no_data){
      
      #trying to check if there are any duration more than dtemply_stop_time
      j <- 0
      for(i in 2:n){
        
        if(as.numeric(dtemply_gps$duration[i]) >= dtemply_stop_time) {
          j <- j + 1
        }
        
        no_duration <- ifelse(j < 1, TRUE, FALSE)
        
      }
      
    }
    
    if(!no_data & !no_duration){
      
      dtemply_locations <- dtemply_gps %>%
        filter(duration >= dtemply_stop_time) %>%
        mutate(lat11 = round(lat, digits = 4)) %>%
        mutate(long11 = round(long, digits = 4)) %>%
        dplyr::select(-c(id, datestamp)) %>%
        mutate(arrival = Timestamp - duration) %>%
        mutate(sequence = 1:n())
      
      x <- m_detailed_home_list %>%
        dplyr::select("lat11", "long11", "category")
      
      z <- detailedPOI_gps %>%
        dplyr::select("lat11", "long11", "category") %>%
        filter(category != "")
      
      x <- rbind(x,z)
      
      y <- left_join(dtemply_locations, x, by = c("lat11" = "lat11", "long11" = "long11"))
      
      dtemply_locations <-  y
      
      col_order <- c("sequence", "arrival", "Timestamp", "duration", "lat", "long", "lat11", "long11", "category")
      dtemply_locations <- dtemply_locations[, col_order]
      
      dtemply_home <- m_simplified_home_list %>%
        filter(id == dtemply_id) %>%
        distinct(lat11, long11)
      
      dtemply_path <- gps %>%
        filter(id == dtemply_id) %>%
        filter(datestamp == dtemply_date)
      
    }
    
    ##################################################
    
    if(!no_data & !no_duration){
      
      dtemply_locations_sf <- st_as_sf(dtemply_locations, 
                                       coords = c("long11", "lat11"), 
                                       crs = 4326) %>%
        st_cast("POINT")
      
      dtemply_home_sf <- st_as_sf(dtemply_home, 
                                  coords = c("long11", "lat11"), 
                                  crs = 4326) %>%
        st_cast("POINT")
      
      #convert to coordinates
      dtemply_path_sf <- st_as_sf(dtemply_path,
                                  coords = c("long", "lat"),
                                  crs = 4326)
      
      #string to gps path
      dtemply_gps_path_sf <- dtemply_path_sf %>%
        summarize(m = mean(Timestamp),
                  do_union = FALSE) %>%
        st_cast("LINESTRING")
      
    }
    
    ##################################################
    
    if(!no_data & !no_duration){
      
      tmap_dtemply_home <- tm_shape(dtemply_home_sf) + 
        tm_dots(size = 0.1,
                alpha = 1,
                col = "yellow")
      
      tmap_dtemply_locations <- tm_shape(dtemply_locations_sf) + 
        tm_dots(size = 0.05,
                alpha = 1,
                col = "red")
      
      tmap_gps_path <- tm_shape(dtemply_gps_path_sf) + 
        tm_lines()
      
      output$mapPlot <- renderTmap({
        tmap_Base + 
          tmap_gps_path + 
          tmap_home + 
          tmap_refinedPOI +
          tmap_dtemply_home + 
          tmap_dtemply_locations
        
      })#close curly and brackers for output$mapPlot, without comma
      
    }
    
    #########################
    #For output$dtemply_table
    ######################### 
    
    if(!no_data & !no_duration){
      
      output$dtemply_table <- DT::renderDT({
        
        data_dtemply_locations <- dtemply_locations %>%
          rename("Order" = "sequence",
                 "Depart" = "Timestamp",
                 "Arrive" = "arrival",
                 "Duration" = "duration",
                 "Location" = "category",
                 "Lat" = "lat11",
                 "Long" = "long11") %>%
          dplyr::select(-c("lat", "long"))
        
        data_dtemply_locations$Duration <- as.character(data_dtemply_locations$Duration)
        
        DT::datatable(data = data_dtemply_locations, fillContainer = F, 
                      options = list(pageLength = 10),
                      rownames = F)
        
      })#close brackets for output$dtemply_table
      
      output$dtemply_loc <- renderGirafe({
        
        distinct_dtemply_locations <- dtemply_locations %>%
          distinct(lat11, long11, category, sequence) %>%
          mutate("location" = ifelse(is.na(category), sequence, category)) %>%
          filter(as.character(location) != "Home") %>%
          dplyr::select(-c(sequence, category))
        
        dtemply_loc_plot_data <- semi_join(location_gps, distinct_dtemply_locations, by = c("lat11" = "lat11", "long11" = "long11")) %>%
          filter(id <= 35) %>%
          dplyr::select(lat11, long11, stop, id) %>%
          left_join(distinct_dtemply_locations, by = c("lat11" = "lat11", "long11" = "long11")) %>%
          left_join(car_data, by = c("id" = "CarID")) %>%
          mutate(name = paste(FirstName, LastName, sep = " ")) %>%
          select("lat11", "long11", "stop", "name", "location") %>%
          rename("Duration" = "stop",
                 "Name" = "name") %>%
          dplyr::select(-c(lat11, long11))
        
        dtemploy_boxplot <- ggplot(data = dtemply_loc_plot_data,
                                   aes(group = location,
                                       tooltip = as.character(seconds_to_period(Duration)))) +
          labs(y = "Location (Order/ Name)", x = "Duration in Seconds", title = "Time Spent at Location") +
          geom_boxplot_interactive(aes(as_hms(Duration), location)) +
          #scale_x_time(name = "Duration") +
          theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1.1, hjust = 1.1),
                axis.text.y = element_text(size = 7),
                plot.title = element_text(hjust = 0.5))
        
        girafe(
          ggobj = dtemploy_boxplot,
          width_svg = 6,
          height_svg = 6*0.618)
        
      })#close brackets for output$dtemply_loc
      
    }#close bracket for the if() statement
    else {
      
      #need to figure out how to printf "No Data Available"
      
    }
    
  })#close curly and brackers for observe, without comma
  
  ####DT Locations Heat Map Server Codes####
  
  observeEvent(input$plottxnheatmap,{
    
    # runif({
    
    fromdate <- input$dtlocationdaterange[1]
    todate <- input$dtlocationdaterange[2]
    isWeekday <- input$dtweekday
    isWeekend <- input$dtweekend
    
    # fromdate <- cc_data$date[1]
    # todate <- cc_data$date[840]
    
    if(isWeekday == TRUE & isWeekend == TRUE){
      heatmap_cc <- cc_data %>%
        filter(date >= fromdate & date <= todate) %>%
        mutate(daydate = weekdays(timestamp)) %>%
        group_by(location, daydate, time) %>%
        add_count(location, daydate, time, name = "count") %>%
        dplyr::select(-c(timestamp, TimeCat, category))
    }
    
    if(isWeekday == TRUE & isWeekend == FALSE){
      heatmap_cc <- cc_data %>%
        filter(date >= fromdate & date <= todate) %>%
        mutate(daydate = weekdays(timestamp)) %>%
        group_by(location, daydate, time) %>%
        add_count(location, daydate, time, name = "count") %>%
        dplyr::select(-c(timestamp, TimeCat, category)) %>%
        mutate(isweekend = is.weekend(date)) %>%
        filter(isweekend == FALSE)
    }
    
    if(isWeekday == FALSE & isWeekend == TRUE){
      heatmap_cc <- cc_data %>%
        filter(date >= fromdate & date <= todate) %>%
        mutate(daydate = weekdays(timestamp)) %>%
        group_by(location, daydate, time) %>%
        add_count(location, daydate, time, name = "count") %>%
        dplyr::select(-c(timestamp, TimeCat, category)) %>%
        mutate(isweekend = is.weekend(date)) %>%
        filter(isweekend == TRUE)
    }
    
    if(isWeekday == FALSE & isWeekend == FALSE){
      
      # p1 <- NULL
      # 
      # girafe(
      #   ggobj = p1,
      #   width_svg = 6,
      #   height_svg = 6*0.618)
      
    } else {
      
      x1 <- length(unique(heatmap_cc$count))
      
      cc_colours1 <- colorRampPalette(c('green', 'yellow', 'orange', 'red'))(x1)
      
      p1 <- ggplot(heatmap_cc,
                   aes(time, location)) + 
        geom_tile_interactive(aes(fill = factor(count))) + 
        scale_fill_manual(values = cc_colours1,
                          name = "Frequency") +
        #breaks = levels(count)[seq(1, x, by = 5)]) +
        labs(y = "Locations", x = "Time (Static)", title = "Transactions/ Locations Heat Map") +
        theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1.1, hjust = 1.1),
              axis.text.y = element_text(size = 7),
              plot.title = element_text(hjust = 0.5))
      
      output$dtlocationheatmap <- renderGirafe({
        
        girafe(
          ggobj = p1,
          width_svg = 6,
          height_svg = 6*0.618)
        
      })#close curly and brackers for dtlocationheatmap, without comma
      
    }#close curly for else()
    
  }) #close bracket without comma for observeReactive
  
  ####DT Location Visits Plot####
  
  output$dtsellocationvisits <- renderPlotly({
    
    selectedlocation <- input$dtlocationselection
    
    sellocationvisits <- cc_data %>%
      filter(location == selectedlocation) %>%
      mutate(day = weekdays(date)) %>%
      select(-c(timestamp, price, last4ccnum, time, TimeCat, category))
    
    p2 <- ggplot(sellocationvisits) + 
      geom_bar(aes(factor(day, weekdays(min(date) + 0:6))),
               width = 0.5,
               fill = '#6FDDF8',
               color = "black") + 
      labs(x = "Day", y = "Number of Transactions", title = paste(selectedlocation, "Visits")) +
      ylim(0, ylim_locations) + 
      scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) + 
      theme(axis.text.x = element_text(size = 8, angle = 45, vjust = 1.1, hjust = 1.1),
            axis.text.y = element_text(size = 7),
            plot.title = element_text(hjust = 0.5),
            panel.background = element_rect(fill = "#FDFCD8"),
            panel.grid.major = element_line(size = 0.1, linetype = "solid", color = "#6D6D6D"),
            panel.grid.minor = element_line(size = 0.05, linetype = "solid", color = "#A4A4A4"))
    
  })#close bracket without comma for output$dtsellocationvisits
  
  ####RT Specific Card Tile Plots Server Codes####
  ################################
  ####SPECIFIC CARD TILE PLOTS####
  ################################
  
  
  ## TOP PLOTS AND DATATABLES
  output$rtccpricetop <- renderPlotly({
    
    #user selection
    indivcc <- cc_data %>% filter(last4ccnum == input$rtspeccreditA) %>% 
      mutate(date = as.POSIXct.Date(date))
    
    #cc txn tile (fill by price)
    indivccplotA <- ggplot(indivcc, aes(date, location)) +
      geom_tile(aes(fill = price,
                    text = sprintf("Location: %s<br>Date: %s<br>Period: %s<br>Amount: $%s<br>CC No.: %s",
                                   location,
                                   timestamp,
                                   TimeCat,
                                   price,
                                   last4ccnum
                                   ))) +
      scale_fill_gradient(low="#56B1F7", high = "#132B43") +
      labs(title = "All Transactions (for Selected Credit Card)",
           fill = "Price Range") +
      scale_x_datetime(breaks = breaks_pretty(14), labels = label_date_short()) +
      scale_y_discrete(limits = rev) +
      theme(axis.text.x = element_text(angle = 0),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title=element_text(face="bold")) +
      theme(legend.key.height = unit(1, "cm"))
    
    ggplotly(indivccplotA, tooltip = c("text"))
    
  }) #close curly and brackers for pricetiletop, without comma
  
  output$rtcctimetop <- renderPlotly({
    
    #user selection
    indivcc <- cc_data %>% filter(last4ccnum == input$rtspeccreditA) %>% 
      mutate(date = as.POSIXct.Date(date))
    
    indivccplotB <- ggplot(indivcc, aes(date, location)) +
      geom_tile(aes(fill = TimeCat,
                    text = sprintf("Location: %s<br>Date: %s<br>Period: %s<br>Amount: $%s<br>CC No.: %s",
                                   location,
                                   timestamp,
                                   TimeCat,
                                   price,
                                   last4ccnum
                                   ))) +
      scale_color_brewer(palette = "Set2",
                         labels = c("Morning", "Pre-Lunch", "LunchTime", "Afternoon", "Evening", "Night","Midnight"))+
      labs(title = "All Transactions (for Selected Credit Card)",
           fill = "Time of Day") +
      scale_x_datetime(breaks = breaks_pretty(14), labels = label_date_short()) +
      scale_y_discrete(limits = rev) +
      theme(axis.text.x = element_text(angle = 0),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title=element_text(face="bold")) #+
      #theme(legend.key.height = unit(1, "cm"))
    
    ggplotly(indivccplotB, tooltip = c("text"))
    
  }) #close bracket and curly wo comma for credit time tile
  
  #For specific loyaltycard tile plot
  
  output$rtlctop <- renderPlotly({
    
    #User Selection
    indivlc <- loyalty_data %>% filter(loyaltynum == input$rtspecloyaltyA) %>% 
      mutate(date = as.POSIXct.Date(timestamp))
    
    #lc txn tile (fill by price)
    indivlcplot <- ggplot(indivlc, aes(date, location)) +
      geom_tile(aes(fill = price,
                    text = sprintf("Location: %s<br>Date: %s<br>Amount: $%s<br>LC No.: %s",
                                   location,
                                   timestamp,
                                   price,
                                   loyaltynum))) +
      scale_fill_gradient(low="#56B1F7", high = "#132B43") +
      labs(title = "All Transactions (for Selected Loyalty Card)",
           fill = "Price Range") +
      scale_x_datetime(breaks = breaks_pretty(14), labels = label_date_short()) +
      scale_y_discrete(limits = rev) +
      theme(axis.text.x = element_text(angle = 0),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title=element_text(face="bold")) +
      theme(legend.key.height = unit(1, "cm"))
    
    ggplotly(indivlcplot, tooltip = c("text")) 
    
    
  }) #close bracket and curly wo comma for loyalty tile
  
  #For DataTables (Specific Card)
  
  output$rtcctabletop <- renderDT({
    
    DT::datatable(cc_data %>% filter(last4ccnum == input$rtspeccreditA) %>%
                    dplyr::select(date, time, location, price, last4ccnum), options = list(
                      initComplete = JS(
                        "function(settings, json) {",
                        "$('body').css({'font-family': 'Helvetica'});",
                        "}"
                      )
                    ))
  }) #close bracket and curly wo comma for renderDT
  
  output$rtlctabletop <- renderDT({
    
    DT::datatable(loyalty_data %>% filter(loyaltynum == input$rtspecloyaltyA) %>%
                    dplyr::select(timestamp, location, price, loyaltynum), options = list(
                      initComplete = JS(
                        "function(settings, json) {",
                        "$('body').css({'font-family': 'Helvetica'});",
                        "}"
                      )
                    ))
    
  }) #close bracket and curly wo comma for renderDT
  
  
  ## BOTTOM PLOTS AND DATATABLES
  output$rtccpricebottom <- renderPlotly({
    
    #user selection
    indivcc <- cc_data %>% filter(last4ccnum == input$rtspeccreditB) %>% 
      mutate(date = as.POSIXct.Date(date))
    
    #cc txn tile (fill by price)
    indivccplotA <- ggplot(indivcc, aes(date, location)) +
      geom_tile(aes(fill = price,
                    text = sprintf("Location: %s<br>Date: %s<br>Period: %s<br>Amount: $%s<br>CC No.: %s",
                                   location,
                                   timestamp,
                                   TimeCat,
                                   price,
                                   last4ccnum
                                   ))) +
      scale_fill_gradient(low="#56B1F7", high = "#132B43") +
      labs(title = "All Transactions (for Selected Credit Card)",
           fill = "Price Range") +
      scale_x_datetime(breaks = breaks_pretty(14), labels = label_date_short()) +
      scale_y_discrete(limits = rev) +
      theme(axis.text.x = element_text(angle = 0),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title=element_text(face="bold")) +
      theme(legend.key.height = unit(1, "cm"))
    
    ggplotly(indivccplotA, tooltip = c("text"))
    
  }) #close curly and brackers for pricetiletop, without comma
  
  output$rtcctimebottom <- renderPlotly({
    
    #user selection
    indivcc <- cc_data %>% filter(last4ccnum == input$rtspeccreditB) %>% 
      mutate(date = as.POSIXct.Date(date))
    
    indivccplotB <- ggplot(indivcc, aes(date, location)) +
      geom_tile(aes(fill = TimeCat,
                    text = sprintf("Location: %s<br>Date: %s<br>Period: %s<br>Amount: $%s<br>CC No.: %s",
                                   location,
                                   timestamp,
                                   TimeCat,
                                   price,
                                   last4ccnum
                                   ))) +
      scale_color_brewer(palette = "Set2",
                         labels = c("Morning", "Pre-Lunch", "LunchTime", "Afternoon", "Evening", "Night","Midnight")) +
            labs(title = "All Transactions (for Selected Credit Card)",
            fill = "Time of Day") +
      scale_x_datetime(breaks = breaks_pretty(14), labels = label_date_short()) +
      scale_y_discrete(limits = rev) +
      theme(axis.text.x = element_text(angle = 0),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title=element_text(face="bold")) +
      theme(legend.key.height = unit(1, "cm"))
    
    ggplotly(indivccplotB, tooltip = c("text"))
    
  }) #close bracket and curly wo comma for credit time tile
  
  #For specific loyaltycard tile plot
  
  output$rtlcbottom <- renderPlotly({
    
    #User Selection
    indivlc <- loyalty_data %>% filter(loyaltynum == input$rtspecloyaltyB) %>% 
      mutate(date = as.POSIXct.Date(timestamp))
    
    #lc txn tile (fill by price)
    indivlcplot <- ggplot(indivlc, aes(date, location)) +
      geom_tile(aes(fill = price,
                    text = sprintf("Location: %s<br>Date: %s<br>Amount: $%s<br>LC No.: %s",
                                   location,
                                   timestamp,
                                   price,
                                   loyaltynum
                      
                    ))) +
      scale_fill_gradient(low="#56B1F7", high = "#132B43") +
      labs(title = "All Transactions (for Selected Loyalty Card)",
           fill = "Price Range") +
      scale_x_datetime(breaks = breaks_pretty(14), labels = label_date_short()) +
      scale_y_discrete(limits = rev) +
      theme(axis.text.x = element_text(angle = 0),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title=element_text(face="bold")) +
      theme(legend.key.height = unit(1, "cm"))
    
    ggplotly(indivlcplot, tooltip = c("text")) 
    
    
  }) #close bracket and curly wo comma for loyalty tile
  
  #For DataTables (Specific Card)
  
  output$rtcctablebottom <- renderDT({
    
    DT::datatable(cc_data %>% filter(last4ccnum == input$rtspeccreditB) %>%
                    dplyr::select(date, time, location, price, last4ccnum), options = list(
                      initComplete = JS(
                        "function(settings, json) {",
                        "$('body').css({'font-family': 'Helvetica'});",
                        "}"
                      )
                    ))
  }) #close bracket and curly wo comma for renderDT
  
  output$rtlctablebottom <- renderDT({
    
    DT::datatable(loyalty_data %>% filter(loyaltynum == input$rtspecloyaltyB) %>%
                    dplyr::select(timestamp, location, price, loyaltynum), options = list(
                      initComplete = JS(
                        "function(settings, json) {",
                        "$('body').css({'font-family': 'Helvetica'});",
                        "}"
                      )
                    ))
    
  }) #close bracket and curly wo comma for renderDT
  
  
  
  
  ####RT Txn Box Plots Server Codes####  
  #########################
  ##### Txn Box Plot ######
  #########################
  
  output$TxnBoxPlotA <- renderPlotly({
    
    #Combining the cc_data and loyalty_data
    ccloyal <- dplyr::bind_rows(list(cc_data = cc_data, loyalty_data = loyalty_data), .id='Source')
    
    p2c <- ccloyal %>% filter(category == input$rtlocationcat) %>% 
      ggplot(aes(x=location, y=price)) +
      geom_boxplot(aes(fill = Source), 
                   position = position_dodge(1)) +
      geom_point(alpha=0, position = position_dodge(1), 
                 aes(text = sprintf("Location: %s<br>Date: %s<br>Amount: $%s<br>CC No.: %s<br>LC No.: %s",
                                    location,
                                    timestamp,
                                    price,
                                    last4ccnum,
                                    loyaltynum))
                 ) + scale_y_log10() +
      ggtitle("Boxplot of Transaction Amounts") +
      theme(axis.title=element_blank(),
            plot.title=element_text(size=16, face="bold"),
            legend.position = "bottom") +
      scale_x_discrete(limits = rev) +
      scale_fill_manual(values=c("#56B1F7","#132B43")) +
      coord_flip()
    
    plotp2c <- p2c %>% ggplotly(tooltip=c("text")) %>% 
      layout(boxmode = "group",
             legend = list(orientation = "h", x=0.4, y=-0.1))
    plotp2c$x$data[[1]]$marker$line$color = "red"
    plotp2c$x$data[[1]]$marker$color = "red"
    plotp2c$x$data[[1]]$marker$outliercolor = "red"
    
    plotp2c
    
  })#close curly and brackers for Txn Box plot, without comma
  
  ####RT CC Box Plots Server Codes####
  #########################
  #####  CC Box Plot ######
  #########################
  
  #For Credit Card Plot
  output$TxnScatterCredit <- renderPlotly({
    
    #From User selection
    ccselection <- cc_data %>% filter(last4ccnum == input$rtcreditcard)
    
    p2e <- ccselection %>%  
      ggplot(aes(x=location, y=price, text=paste("CC No.:", last4ccnum))) +
      geom_point(alpha = 0.8,
                 size = 6,
                 shape = 18,
                 aes(color = price,
                     text = sprintf("Location: %s<br>Date: %s<br>Amount: $%s<br>CC No.: %s",
                                    location,
                                    timestamp,
                                    price,
                                    last4ccnum
                                    ))) + scale_y_log10() + coord_flip() +
      
      ggtitle("Card-Specific Transaction Amounts (Credit Card)") +
      theme(axis.title=element_blank(),
            plot.title=element_text(size=16, face="bold")) +
      xlab("Transaction Amount") +
      labs(color = "Price Range") +
      scale_x_discrete(limits = rev) +
      scale_color_gradient(low = "#56B1F7", high = "#132B43")
    
    plotp2e <- ggplotly(p2e, width_svg = 7, height_svg = 7, tooltip = c("text"))
    
    plotp2e
    
  })#close curly and brackers for output$TxnScatterCredit, without comma
  
  ####RT LC Box Plots Server Codes####
  #########################
  #####  LC Box Plot ######
  #########################
  
  #For Loyalty Card Plot
  output$TxnScatterLoyalty <- renderPlotly({
    
    #From User selection
    lcselection <- loyalty_data %>% filter(loyaltynum == input$rtloyalcard)
    
    p2f <- lcselection %>%  
      ggplot(aes(x=location, y=price)) +
      geom_point(alpha=0.8,
                 size = 6,
                 shape = 18,
                 aes(color = price,
                     text = sprintf("Location: %s<br>Date: %s<br>Amount: $%s<br>LC No.: %s",
                                    location,
                                    timestamp,
                                    price,
                                    loyaltynum))) + 
      scale_y_log10() + coord_flip() +
      
      ggtitle("Card-Specific Transaction Amounts (Loyalty)") +
      theme(axis.title=element_blank(),
            plot.title=element_text(size=16, face="bold")) +
      xlab("Transaction Amount") +
      labs(color = "Price Range") +
      scale_x_discrete(limits = rev) +
      scale_color_gradient(low = "#56B1F7", high = "#132B43")
    
    plotp2f <- ggplotly(p2f, width_svg = 7, height_svg = 7, tooltip = c("text"))
    
    plotp2f
    
  })#close curly and brackers for output$TxnScatterLoyalty, without comma
  
  ####RT ShinyJS Server Codes####
  #########ALL SHINYJS OBSERVEEVENTS#########
  
  #For Spec Card Page Top Plot
  
  observeEvent(input$rtjellytop, {
    
    if(input$rtjellytop == "Credit Card"){
      shinyjs::hide(id = "rtspecloyaltyA")
      shinyjs::hide(id = "rtlctop")
      shinyjs::hide(id = "rtlctabletop")
      shinyjs::show(id = "rtspeccreditA")
      shinyjs::show(id = "rtjellyA")
      shinyjs::show(id = "rtcctabletop")
      
    } else if(input$rtjellytop == "Loyalty Card"){
      shinyjs::hide(id = "rtspeccreditA")
      shinyjs::hide(id = "rtjellyA")
      shinyjs::hide(id = "rtccpricetop")
      shinyjs::hide(id = "rtcctimetop")
      shinyjs::hide(id = "rtcctabletop")
      shinyjs::show(id = "rtspecloyaltyA")
      shinyjs::show(id = "rtlctop")
      shinyjs::show(id = "rtlctabletop")
    }
    
  })
  
  observeEvent(input$rtjellyA, {
    
    if(input$rtjellyA == "price"){
      
      shinyjs::show(id = "rtccpricetop")
      shinyjs::hide(id = "rtcctimetop")
      
    } else if (input$rtjellyA == "TimeCat"){
      
      shinyjs::show(id = "rtcctimetop")
      shinyjs::hide(id = "rtccpricetop")
    }
  })
  
  #For spec card bottom plots
  
  observeEvent(input$rtjellybottom, {
    
    if(input$rtjellybottom == "Credit Card"){
      shinyjs::hide(id = "rtspecloyaltyB")
      shinyjs::hide(id = "rtlcbottom")
      shinyjs::hide(id = "rtlctablebottom")
      shinyjs::show(id = "rtspeccreditB")
      shinyjs::show(id = "rtjellyB")
      shinyjs::show(id = "rtcctablebottom")
      
    } else if(input$rtjellybottom == "Loyalty Card"){
      shinyjs::hide(id = "rtspeccreditB")
      shinyjs::hide(id = "rtjellyB")
      shinyjs::hide(id = "rtccpricebottom")
      shinyjs::hide(id = "rtcctimebottom")
      shinyjs::hide(id = "rtcctablebottom")
      shinyjs::show(id = "rtspecloyaltyB")
      shinyjs::show(id = "rtlcbottom")
      shinyjs::show(id = "rtlctablebottom")
    }
    
  })
  
  observeEvent(input$rtjellyB, {
    
    if(input$rtjellyB == "price"){
      
      shinyjs::show(id = "rtccpricebottom")
      shinyjs::hide(id = "rtcctimebottom")
      
    } else if (input$rtjellyB == "TimeCat"){
      
      shinyjs::show(id = "rtcctimebottom")
      shinyjs::hide(id = "rtccpricebottom")
    }
  })
  
  
  #For Txn Boxplots
  
  observeEvent(input$rtradio, {
    
    if(input$rtradio == "Credit Card"){
      shinyjs::hide(id = "rtloyalcard")
      shinyjs::hide(id = "TxnScatterLoyalty")
      shinyjs::show(id = "rtcreditcard")
      shinyjs::show(id = "TxnScatterCredit")
    } else if(input$rtradio == "Loyalty Card"){
      shinyjs::hide(id = "rtcreditcard")
      shinyjs::hide(id = "TxnScatterCredit")
      shinyjs::show(id = "rtloyalcard")
      shinyjs::show(id = "TxnScatterLoyalty")
    }
    
  })
  
  
  
  ####################################
  
}#close curly bracket for server

# Run the application 
shinyApp(ui = ui, server = server)
