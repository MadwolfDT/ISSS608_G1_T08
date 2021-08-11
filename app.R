library(shiny)
library(rsconnect)
library(shinythemes)
#library(clock)
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
#library(textnets)
library(textdata)
library(corporaexplorer) 
library(topicmodels)
library(widyr)

rsconnect::setAccountInfo(name='dtcs', token='25A37523AE52220A0DE445A9D8B696DE', secret='OMMf3zDxI4jOhIpxHvsZJOf3MDPfIdMhPmpRSrLV')

source('scripts/read_data.r')
source('scripts/dt_clean_data.r')
source('scripts/dt_variables.r')
source('scripts/mc1_clean_and_import.r')
source('scripts/rt_clean_data.r')

# Define UI
ui <- navbarPage(
  
  title =  "Investigation Tool into Personnel Disappearance",
  
  theme = shinytheme("journal"),

  tabPanel("Background Situation", "To be Updated"),
  
  navbarMenu(
    "Exploratory Data Analysis",
    tabPanel(title = "Transactions - For Exploration!",
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
                    
                    screenshotButton("Take a Screenshot!")
                    
                    ), #close brackets for column(10), need comma
             
             
           ),#close brackets for fluidRow(), need comma
           
           
           ), #close brackets for tabPanel for EDA, need comma
    tabPanel(title = 'Employees of GasTech',
             
             column(width=3,
                    radioButtons(inputId = 'view_select', label='Overview or Person Look-up', choices = c('Person','Overview'))
        
                    )
             
             
             )  #close brackets for tabPanel for Emp of GasTech
    
    
    
    ), #close brackets for NavbarMenu

  navbarMenu("Inferential Statistics", 
             tabPanel("Email Network Analysis",
                      
                      sidebarLayout(
                        sidebarPanel('Inputs',
                                     dateRangeInput(inputId = 'date', 
                                                    label = 'Date Filter', 
                                                    start = min(df.emails$Date.Date), 
                                                    end=max(df.emails$Date.Date)
                                     ),
                                     sliderInput(inputId = 'time',
                                                 label = 'Time Filter', 
                                                 min = as.POSIXct("1990-01-01 00:00:10", tz = 'GMT'), 
                                                 max = as.POSIXct("1990-01-01 23:59:59", tz = 'GMT'), 
                                                 value = c(as.POSIXct("1990-01-01 00:00:10",tz = 'GMT'), 
                                                           as.POSIXct("1990-01-01 23:59:59",tz = 'GMT')),
                                                 step = 1,
                                                 timeFormat = "%H:%M",
                                                 timezone = "GMT"
                                     ),
                                     selectInput(inputId = 'person', 
                                                 label= 'Employee', 
                                                 choices = unique(df.emp$FullName)
                                     ),
                                     radioButtons(inputId = 'dt_select', 
                                                  label='DataTable Displayed by:', 
                                                  choices = c('Person','Keywords', 'Both')
                                     ),
                                     textInput(inputId = 'search', label = 'Email Text Search'),
                                     
                                     actionButton(inputId = 'go', label = "Display")
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel('Email Overview',
                                     plotlyOutput(outputId = 'email_convo')),
                            tabPanel('Tabular Overview',
                                     DT::dataTableOutput(outputId = 'table')),
                            tabPanel('Network Overview',
                                     visNetworkOutput(outputId = 'vis_email')
                                    )#close bracket for tabpanel
                            
                                  )#close bracket without comma for tabsetPanel
                          
                               )#close bracket without comma for mainPanel
                        
                          )#close bracket without comma for sidebarLayout
                      
                      ),#close bracket with comma for Email
             
             tabPanel("Networks",
                      
                      sidebarLayout(
                        sidebarPanel('Inputs',
                                     radioButtons(inputId = 'node_sizings',
                                                  label="Size by",
                                                  choices = c('None','Betweenness', 'Degree', 'Closeness'),
                                                  selected = 'None')),
                        mainPanel(column(width=5, 
                                         visNetworkOutput(outputId = 'vis_dept',width = "100%", height = 700)),
                                  column(width = 5, 
                                         visNetworkOutput(outputId = 'vis_dept_sub',width = "100%", height = 700))
                                  )#close without comma bracket for mainPanel
                        
                              )#close bracket without comma for sidebarLayout
                      
                      ),#close bracket with comma for Networks
             
             tabPanel("Employee Movement Plot",
                      
                      titlePanel("Personnel Movement Plot"),
                      
                      fluidRow(
                        column(4,
                               
                               selectInput(
                                 
                                 inputId = "dtemployee_name",
                                 label = "Employee Name",
                                 choices = c(paste(emply_name$FirstName, emply_name$LastName, sep = " ")),
                                 
                               ),#close bracket with comma for selectInput
                               
                               selectInput(
                                 
                                 inputId = "dtdate",
                                 label = "Date",
                                 choices = c(gps_date$datestamp),
                                 
                               ),#close bracket with comma for selectInput
                               
                               #submitButton("Apply changes")
                               
                              ),#close bracket with comma for column(4)
                        
                        column(8,tmapOutput("mapPlot")
                               ),#close bracket with comma for column(8)
                              
                              ),#close bracket with comma for fluidRow()
                      
                      fluidRow(
                        column(6,"Selected Personnel Details"),#close bracket with comma for column(6)
                        column(6,"Population Parking Details"),#close bracket with comma for column(6)
                              ),#close bracket with comma for fluidRow()
                      
                      ),#close bracket with comma for Plot
             
             tabPanel("Analysis for Specific Cards",
                      titlePanel("Transactions for Specific Cards"),
                      
                      fluidRow(
                        column(2,
                               radioButtons(
                                 inputId = "rtradiospeccardA",
                                 label = "Select Card Type",
                                 choices = c("Credit Card", "Loyalty Card"),
                                 selected = "Credit Card"
                               ),#close bracket w comma for radiobutton
                               
                               conditionalPanel(
                                 condition = "input.rtradiospeccardA == 'Credit Card'",
                                 selectInput(
                                   inputId = "rtspeccreditA",
                                   label = "Last 4 Digits of Card",
                                   choices = unique(cc_data$last4ccnum)
                                 )
                               ), #close bracket w comma for conPanel
                               
                               conditionalPanel(
                                 condition = "input.rtradiospeccardA == 'Credit Card'",
                                 radioButtons(
                                   inputId = "rtradiospeccardfillA",
                                   label = "Select Fill Type",
                                   choices = list(
                                     "Transaction Price" = "price",
                                     "Time Period" = "TimeCat"
                                   ),
                                   selected = "price"
                                 ) #close bracket wo comma for radioButton
                               ), #close bracket w comma for conPanel
                               
                               conditionalPanel(
                                 condition = "input.rtradiospeccardA == 'Loyalty Card'",
                                 selectInput(
                                   inputId = "rtspecloyaltyA",
                                   label = "Loyalty Card No.",
                                   choices = unique(loyalty_data$loyaltynum)
                                 )
                               ), #close bracket w comma for conPanel
                                
                        ), #close bracket with comma for column
                        
                        column(10,
                               
                               conditionalPanel(
                                 condition = "input.rtradiospeccardA == 'Loyalty Card'",
                                 plotlyOutput(outputId = "rtspeclcA"),
                                 DT::dataTableOutput("rtspeclctableA"),
                                 "To be UpdatedC"
                                 
                               )
                        ), #close bracket with comma for column
                        
                      ), #close bracket with comma for fluidRow
                      
               
             ), #close bracket with comma for Specific Card Analysis Tab
             
             
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
                               plotlyOutput(outputId = "TxnBoxPlotA")
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
                                   choices = unique(cc_data$last4ccnum)
                                 )
                               ),#close bracket with comma for conditionalPanel
                               conditionalPanel(
                                 condition = "input.rtradio == 'Loyalty Card'",
                                 selectInput(
                                   inputId = "rtloyalcard",
                                   label = "Loyalty Card No.",
                                   choices = unique(loyalty_data$loyaltynum)
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
  
  #########################
  ###For Email Convo    ###
  #########################  
  
  output$email_convo <- renderPlotly({
    
    dat <- x_full %>%
      filter(From == input$person) %>%
      filter(Date.Time >= as_hms(str_extract(input$time[1], pattern = '\\d\\d:\\d\\d:\\d\\d')), 
             Date.Time <= as_hms(str_extract(input$time[2], pattern = '\\d\\d:\\d\\d:\\d\\d')) 
      ) %>%
      filter(Date.Date >= input$date[1], 
             Date.Date <= input$date[2])
    
    details <- x_full %>% 
      filter(From == input$person) %>% 
      select(From_dep, From_title)
    
    g <- ggplot(dat, aes(y=To, x=Date.Time))+ 
      geom_point(#position=position_dodge(width=1),
        size=2, alpha = 0.6, stroke=0.5, shape=21,
        aes(text = sprintf("Sub: %s<br>Date: %s<br>To: %s<br>To_Title: %s<br>To_Dep: %s<br>Time Sent: %s<br>", 
                           Subject2, 
                           Date.Date, 
                           To,
                           To_title, 
                           To_dep,
                           Date.Time))) +
      geom_line(aes(group = Subject2, color= Subject2), size=0.2) +
      labs(y="",x="Time",title = paste(input$person,paste(unique(details), collapse = ',')), 
           group="", 
           color="") +
      theme(legend.text = element_text(size = 8),
            panel.background = element_rect(fill="white"),
            panel.grid.major.y = element_line(color="#f0f0f0"),
            plot.title = element_text(size=14))
    
    gg <- ggplotly(g, tooltip = c("text")) 
    gg
    
  })#close brackets for output$email_convo
  
  #########################
  ###For temp_g_vf      ###
  #########################  
  
  temp_g_vf <- eventReactive(input$go, {
    
    if (input$dt_select == 'Person'){
      m <- x_full %>%
        mutate(s = str_replace(Subject,"RE: ","")) 
      
      
      temp_g <- m %>% 
        filter(To == input$person | From == input$person)
      
      temp_g <- temp_g %>% 
        select(From, From_title, To, To_title, Date.Date, Date.Time, Subject) 
      
    }else if (input$dt_select == 'Keywords'){
      m <- x_full %>%
        mutate(s = str_replace(Subject,"RE: ","")) 
      
      
      temp_g <- m %>% 
        filter(str_detect(s, str_replace_all(input$search,',','|')))
      
      temp_g <- temp_g %>% 
        select(From, From_title, To, To_title, Date.Date, Date.Time, Subject) 
      
    }else{
      m <- x_full %>%
        mutate(s = str_replace(Subject,"RE: ","")) 
      
      
      temp_g <- m %>% 
        filter(To == input$person | From == input$person) %>%
        filter(str_detect(s, str_replace_all(input$search,',','|')))
      
      temp_g <-temp_g %>% 
        select(From, From_title, To, To_title, Date.Date, Date.Time, Subject) 
    }
  })#close brackets for temp_g_vf
  
  #########################
  ###For output$table   ###
  ######################### 
  
  output$table <- DT::renderDT({
    DT::datatable(data = temp_g_vf(), fillContainer = F, 
                  options = list(pageLength = 10),
                  rownames = F)
  })#close brackets for output$table
  
  #########################
  ###For output$vis_email##
  ######################### 
  
  output$vis_email <- renderVisNetwork({
    
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
    
    lnodes <- data.frame(shape = rep('square',6),
                         label = c("Executive",
                                   "Facilities",
                                   "Engineering",
                                   "Administration",
                                   "Security",
                                   "Information Technology"),
                         color.background = c("#FB7E81",
                                              "#7BE141",
                                              "#FFFF00",
                                              "#97C2FC",
                                              "#AD85E4",
                                              "#EB7DF4"),
                         color.border = rep('black',6),
                         font.align =  rep("center",6),
                         font.size = rep(20,6)
    )
    
    data <- toVisNetworkData(temp_graph)
    
    visNetwork(data$nodes, data$edges) %>%
      visEdges(arrows = "middle",width = 0.01,length = 5, scaling = list(min=0.1,max=3)) %>%
      visIgraphLayout() %>%
      visNodes(size=20) %>%
      visOptions(nodesIdSelection = T, highlightNearest = T) %>%
      visLegend(zoom = T, addNodes = lnodes, useGroups = F)  %>%
      visGroups(groupname = "Executive", 
                # Red
                color = list(border = "#FA0A10", 
                             background = "#FB7E81", 
                             highlight = list(border = "#FA0A10", background = "#FB7E81"),
                             hover = list(background = "#FA0A10", border = "#FB7E81")
                )) %>%
      visGroups(groupname = "Facilities", 
                # Green
                color = list(border = "#41A906", 
                             background = "#7BE141", 
                             highlight = list(border = "#41A906", background = "#7BE141"),
                             hover = list(background = "#41A906", border = "#7BE141")
                )) %>%
      visGroups(groupname = "Engineering", 
                # Yellow
                color = list(border = "#FFA500", 
                             background = "#FFFF00", 
                             highlight = list(border = "#FFA500", background = "#FFFF00"),
                             hover = list(background = "#FFA500", border = "#FFFF00")
                )) %>%
      visGroups(groupname = "Administration", 
                # Blue
                color = list(border = "#2B7CE9", 
                             background = "#97C2FC", 
                             highlight = list(border = "#2B7CE9", background = "#97C2FC"),
                             hover = list(background = "#2B7CE9", border = "#97C2FC")
                )) %>%
      visGroups(groupname = "Security", 
                # Purple
                color = list(border = "#7C29F0", 
                             background = "#AD85E4", 
                             highlight = list(border = "#7C29F0", background = "#AD85E4"),
                             hover = list(background = "#7C29F0", border = "#AD85E4")
                )) %>%
      visGroups(groupname = "Information Technology", 
                # Magenta
                color = list(border = "#E129F0", 
                             background = "#EB7DF4", 
                             highlight = list(border = "#E129F0", background = "#EB7DF4"),
                             hover = list(background = "#E129F0", border = "#EB7DF4")
                )) %>%
      visLegend(zoom = T, addNodes = lnodes, useGroups = F) %>%
      visInteraction(multiselect = TRUE)
    
    
  })#close brackets for output$vis_email
  
  
  #########################
  ###For output$vis_dept ##
  #########################
  
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
                                           vertices = nodes_df)
    
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
      
    }else if (input$node_sizings=='Closeness'){
      sizing <- data.frame(size = round(closeness(email_network)*5000))
      
      nodes <- data.frame(id = V(email_network)$name, 
                          title = V(email_network)$name, 
                          group = V(email_network)$department,
                          size = sizing)
    }
    
    
    
    edges <- get.data.frame(email_network, what="edges")[1:2]
    edges$value <- links$weight
    #edges$color.opacity <- 0.8
    #edges$color.highlight <- "red"
    
    nodes$color.highlight.background <- "brown"
    
    nodes <- nodes %>% mutate(font.size = 25, 
                              font.weight= 1000)
    
    
    lnodes <- data.frame(shape = rep('square',6),
                         label = c("Executive",
                                   "Facilities",
                                   "Engineering",
                                   "Administration",
                                   "Security",
                                   "Information Technology"),
                         color.background = c("#FB7E81",
                                              "#7BE141",
                                              "#FFFF00",
                                              "#97C2FC",
                                              "#AD85E4",
                                              "#EB7DF4"),
                         color.border = rep('black',6),
                         font.align =  rep("center",6),
                         font.size = rep(20,6)
    )
    
    
    
    
    p <- visNetwork(nodes = nodes, 
                    edges = edges,  width = "100%", height = 700) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1), 
                 nodesIdSelection = T,
                 selectedBy = "group"
                 
      ) %>% 
      visEdges(width = 0.01,length = 10, scaling = list(min=0.1,max=3)) %>% 
      visLayout(randomSeed = 123) %>%
      visNodes(labelHighlightBold = T) %>%
      #visPhysics(stabilization = 5,
      #           barnesHut = list(springLength =230, avoidOverlap=0.2), 
      #           forceAtlas2Based = list(gravitaionalConstant = -100,
      #                                   centralGravity = 0.5)) %>%
      visIgraphLayout(layout = "layout_nicely") %>%
      visInteraction(multiselect = TRUE) %>%
      visLegend(enabled = T) %>%
      visGroups(groupname = "Executive", 
                # Red
                color = list(border = "#FA0A10", 
                             background = "#FB7E81", 
                             highlight = list(border = "#FA0A10", background = "#FB7E81"),
                             hover = list(background = "#FA0A10", border = "#FB7E81")
                )) %>%
      visGroups(groupname = "Facilities", 
                # Green
                color = list(border = "#41A906", 
                             background = "#7BE141", 
                             highlight = list(border = "#41A906", background = "#7BE141"),
                             hover = list(background = "#41A906", border = "#7BE141")
                )) %>%
      visGroups(groupname = "Engineering", 
                # Yellow
                color = list(border = "#FFA500", 
                             background = "#FFFF00", 
                             highlight = list(border = "#FFA500", background = "#FFFF00"),
                             hover = list(background = "#FFA500", border = "#FFFF00")
                )) %>%
      visGroups(groupname = "Administration", 
                # Blue
                color = list(border = "#2B7CE9", 
                             background = "#97C2FC", 
                             highlight = list(border = "#2B7CE9", background = "#97C2FC"),
                             hover = list(background = "#2B7CE9", border = "#97C2FC")
                )) %>%
      visGroups(groupname = "Security", 
                # Purple
                color = list(border = "#7C29F0", 
                             background = "#AD85E4", 
                             highlight = list(border = "#7C29F0", background = "#AD85E4"),
                             hover = list(background = "#7C29F0", border = "#AD85E4")
                )) %>%
      visGroups(groupname = "Information Technology", 
                # Magenta
                color = list(border = "#E129F0", 
                             background = "#EB7DF4", 
                             highlight = list(border = "#E129F0", background = "#EB7DF4"),
                             hover = list(background = "#E129F0", border = "#EB7DF4")
                )) %>%
      visLegend(zoom = T, addNodes = lnodes, useGroups = F) %>%
      visEvents(selectNode = "function(nodes) {
            Shiny.onInputChange('current_node_id', nodes);
            ;}")
    p
    
    
  })#close brackets for output$vis_dept
  
  
  #########################
  ##### observeEvent ######
  #########################
  
  
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
        
        nodes <- data.frame(id = V(email_network)$name, 
                            title = V(email_network)$name, 
                            group = V(email_network)$department)
        
        
        edges <- get.data.frame(email_network, what="edges")[1:2]
        edges$value <- links$weight
        #edges$color.opacity <- 0.8
        #edges$color.highlight <- "red"
        
        nodes$color.highlight.background <- "brown"
        
        nodes <- nodes %>% mutate(font.size = 25, 
                                  font.weight= 1000)
        
        
        lnodes <- data.frame(shape = rep('square',6),
                             label = c("Executive",
                                       "Facilities",
                                       "Engineering",
                                       "Administration",
                                       "Security",
                                       "Information Technology"),
                             color.background = c("#FB7E81",
                                                  "#7BE141",
                                                  "#FFFF00",
                                                  "#97C2FC",
                                                  "#AD85E4",
                                                  "#EB7DF4"),
                             color.border = rep('black',6),
                             font.align =  rep("center",6),
                             font.size = rep(20,6)
        )
        
        
        p <- visNetwork(nodes = nodes, 
                        edges = edges,  width = "100%", height = 700) %>%
          visOptions(highlightNearest = list(enabled = T, degree = 1), 
                     nodesIdSelection = T,
                     selectedBy = "group"
                     
          ) %>% 
          visEdges(width = 0.01,length = 10, scaling = list(min=0.1,max=3)) %>% 
          visLayout(randomSeed = 123) %>%
          visNodes(labelHighlightBold = T) %>%
          #visPhysics(stabilization = 5,
          #           barnesHut = list(springLength =230, avoidOverlap=0.2), 
          #           forceAtlas2Based = list(gravitaionalConstant = -100,
          #                                   centralGravity = 0.5)) %>%
          visIgraphLayout(layout = "layout_nicely") %>%
          visInteraction(multiselect = TRUE) %>%
          visLegend(enabled = T) %>%
          visGroups(groupname = "Executive", 
                    # Red
                    color = list(border = "#FA0A10", 
                                 background = "#FB7E81", 
                                 highlight = list(border = "#FA0A10", background = "#FB7E81"),
                                 hover = list(background = "#FA0A10", border = "#FB7E81")
                    )) %>%
          visGroups(groupname = "Facilities", 
                    # Green
                    color = list(border = "#41A906", 
                                 background = "#7BE141", 
                                 highlight = list(border = "#41A906", background = "#7BE141"),
                                 hover = list(background = "#41A906", border = "#7BE141")
                    )) %>%
          visGroups(groupname = "Engineering", 
                    # Yellow
                    color = list(border = "#FFA500", 
                                 background = "#FFFF00", 
                                 highlight = list(border = "#FFA500", background = "#FFFF00"),
                                 hover = list(background = "#FFA500", border = "#FFFF00")
                    )) %>%
          visGroups(groupname = "Administration", 
                    # Blue
                    color = list(border = "#2B7CE9", 
                                 background = "#97C2FC", 
                                 highlight = list(border = "#2B7CE9", background = "#97C2FC"),
                                 hover = list(background = "#2B7CE9", border = "#97C2FC")
                    )) %>%
          visGroups(groupname = "Security", 
                    # Purple
                    color = list(border = "#7C29F0", 
                                 background = "#AD85E4", 
                                 highlight = list(border = "#7C29F0", background = "#AD85E4"),
                                 hover = list(background = "#7C29F0", border = "#AD85E4")
                    )) %>%
          visGroups(groupname = "Information Technology", 
                    # Magenta
                    color = list(border = "#E129F0", 
                                 background = "#EB7DF4", 
                                 highlight = list(border = "#E129F0", background = "#EB7DF4"),
                                 hover = list(background = "#E129F0", border = "#EB7DF4")
                    )) %>%
          visLegend(zoom = T, addNodes = lnodes, useGroups = F) 
        
        
        
      })#close brackets for output$vis_dept_sub, no comma
      
    }#close curly bracket for observeEvent, no comma
    
  )#close bracket for observeEvent, no comma
  
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
    
    gps_sf <- st_as_sf(selected_gps,
                       coords = c("long", "lat"),
                       crs = 4326)
    
    #create GPS path
    gps_path <- gps_sf %>%
      summarize(m = mean(Timestamp),
                do_union = FALSE) %>%
      st_cast("LINESTRING")
    
    output$mapPlot <- renderTmap({
      tmap_Base + tmap_home + tmap_refinedPOI +
        tm_shape(gps_path) + 
        tm_lines()
      
      #print(selectedID)
      
    })#close curly and brackers for output$mapPlot, without comma
    
  })#close curly and brackers for observe, without comma
  
  #########################
  ##### Txn Box Plot ######
  #########################
  
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
    
  })#close curly and brackers for Txn Box plot, without comma
  
  #########################
  #####  CC Box Plot ######
  #########################
  
  #For Credit Card Plot
  output$TxnScatterCredit <- renderPlotly({
    
    #From User selection
    ccselection <- cc_data %>% filter(last4ccnum == input$rtcreditcard)
    
    p2e <- ccselection %>%  
      ggplot(aes(x=location, y=price, text=paste("CC No.:", last4ccnum))) +
      geom_point(alpha=1) + scale_y_log10() + coord_flip() +
      
      ggtitle("Card-Specific Transaction Amounts (Credit Card)") +
      theme(axis.title=element_blank(),
            plot.title=element_text(size=16, face="bold")) +
      xlab("Transaction Amount") +
      scale_x_discrete(limits = rev)
    
    plotp2e <- ggplotly(p2e, width_svg = 7, height_svg = 7)
    plotp2e$x$data[[1]]$marker$line$color = "red"
    plotp2e$x$data[[1]]$marker$color = "red"
    plotp2e$x$data[[1]]$marker$outliercolor = "red"
    
    plotp2e
    
  })#close curly and brackers for output$TxnScatterCredit, without comma
  
  
  #########################
  #####  LC Box Plot ######
  #########################
  
  #For Loyalty Card Plot
  output$TxnScatterLoyalty <- renderPlotly({
    
    #From User selection
    lcselection <- loyalty_data %>% filter(loyaltynum == input$rtloyalcard)
    
    p2f <- lcselection %>%  
      ggplot(aes(x=location, y=price, text=paste("LC No.:", loyaltynum))) +
      geom_point(alpha=1) + scale_y_log10() + coord_flip() +
      
      ggtitle("Card-Specific Transaction Amounts (Loyalty)") +
      theme(axis.title=element_blank(),
            plot.title=element_text(size=16, face="bold")) +
      xlab("Transaction Amount") +
      scale_x_discrete(limits = rev)
    
    plotp2f <- ggplotly(p2f, width_svg = 7, height_svg = 7)
    plotp2f$x$data[[1]]$marker$line$color = "red"
    plotp2f$x$data[[1]]$marker$color = "red"
    plotp2f$x$data[[1]]$marker$outliercolor = "red"
    
    plotp2f
    
  })#close curly and brackers for output$TxnScatterLoyalty, without comma
  
  
}#close curly bracket for server

# Run the application 
shinyApp(ui = ui, server = server)
