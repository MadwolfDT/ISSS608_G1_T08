#Creating Time Bins

#Creating breaks
breaks <- hour(hm("00:00", "6:00","9:00", "11:00", "13:30", "16:00", "18:00", "23:59"))
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
  #mutate(timestamp = mdy_hm(cc_data$timestamp)) %>% 
  mutate(date = as.Date(timestamp),
         time = hms::as_hms(timestamp)) %>% 
  #Add Time Labels
  mutate(TimeCat = cut(x=hour(time), breaks=breaks, labels=labels, include.lowest=TRUE)) %>% 
  #Add Location Categories
  left_join(distinctloc, by = "location")

#loyalty_data

loyalty_data <- loyalty_data %>% 
  #Convert date
  #mutate(timestamp = mdy(loyalty_data$timestamp)) %>% 
  #Add Location Categories
  left_join(distinctloc, by = "location")