#To avoid getting scientific notation for numbers. To disable, set it to 0
options(scipen = 999)

gps$Timestamp <- mdy_hms(gps$Timestamp)

gps <- gps %>%
  mutate(datestamp = as.Date(Timestamp))

gps_date <- gps %>%
  distinct(datestamp)

#convert timestamp from character into date/time format
cc_data$timestamp <- mdy_hm(cc_data$timestamp)

#convert timestamp from character into date/time format
loyalty_data$timestamp <- mdy(loyalty_data$timestamp)

#Amend string text for Katrina's Cafe
cc_data <- cc_data %>%
  mutate(location = str_replace_all(location,
                                    pattern = "Katerin.+",
                                    replacement = "Katrina\x27s Caf\xe9"))


loyalty_data <- loyalty_data %>%
  mutate(location = str_replace_all(location,
                                    pattern = "Katerin.+",
                                    replacement = "Katrina\x27s Caf\xe9"))

cc_locations <- cc_data %>%
  distinct(location)

ylim_locations <- cc_data %>%
  mutate(day = weekdays(timestamp)) %>%
  group_by(location, day) %>%
  count(location, day, name = "count") %>%
  ungroup()

ylim_locations <- max(ylim_locations$count)

location_gps <- gps %>%
  group_by(id) %>%
  mutate(lat11 = round(lat, digits = 4)) %>%
  mutate(long11 = round(long, digits = 4)) %>%
  mutate(stop = Timestamp - lag(Timestamp)) %>%
  mutate(parked = ifelse(stop >60*3, TRUE,FALSE)) %>%
  ungroup() %>%
  filter(parked == TRUE)

emply_name <- car_data %>%
  filter(!is.na(CarID))