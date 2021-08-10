#To avoid getting scientific notation for numbers. To disable, set it to 0
options(scipen = 999)

gps$Timestamp <- date_time_parse(gps$Timestamp,
                                 zone = "",
                                 format = "%m/%d/%Y %H:%M:%S")

gps <- gps %>%
  mutate(datestamp = as.Date(Timestamp + 60*60*8))

gps_date <- gps %>%
  distinct(datestamp)

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