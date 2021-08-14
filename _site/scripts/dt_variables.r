revised_home_POI <- location_gps %>%
  filter(stop >= 60*60*8) %>%
  distinct(id,lat11,long11) %>%
  filter(id <= 35) %>%
  filter(lat11 != 36.0480 & long11 != 24.8796) %>%
  group_by(lat11, long11) %>%
  group_by(id) %>%
  mutate(lat_interval = lat11 - lag(lat11), long_interval = long11 - lag(long11)) %>%
  ungroup() %>%
  filter(id != 9 & id != 21 & id != 25 & id != 28)

#First set of detailed home list derived (for csv printing)
detailed_home_list <- revised_home_POI %>%
  dplyr::select(-c(lat_interval,long_interval)) %>%
  left_join(car_data, by = c("id" = "CarID")) %>%
  mutate(category = "Home")

#First set of home list derived
simplified_home_list <- revised_home_POI %>%
  filter(is.na(lat_interval)) %>%
  dplyr::select(-c(lat_interval, long_interval)) %>%
  left_join(car_data, by = c("id" = "CarID"))

####################################################################
#Finding the home for 9
emplyr_in_q <- location_gps %>%
  filter(id == 9) %>%
  filter(lat11 != 36.0480 & long11 != 24.8796) %>%
  filter(stop >= 60*60*8) %>%
  mutate(lat = lat11, long = long11) %>%
  group_by(id, lat11, long11) %>%
  count(lat11, long11, name = "count") %>%
  ungroup()

x <- emplyr_in_q %>%
  distinct(id, lat11, long11)

x <- x[order(x$lat11),]

detailed_home_list_9 <- x %>%
  mutate(lat_interval = lat11 - lag(lat11), long_interval = long11 - lag(long11)) %>%
  filter(lead(between(lat_interval, -0.0001, 0.0001)) | 
           lead(between(long_interval, -0.0001, 0.0001)) |
           between(lat_interval, -0.0001, 0.0001) | 
           between(long_interval, -0.0001, 0.0001)) %>%
  dplyr::select(-c(lat_interval, long_interval)) %>%
  left_join(car_data, by = c("id" = "CarID")) %>%
  mutate(category = "Home")

simplified_home_list_9 <- left_join(x, emplyr_in_q, by = c("lat11" = "lat11", "long11" = "long11", "id" = "id")) %>%
  filter(count > 1) %>%
  dplyr::select(-c(count)) %>%
  left_join(car_data, by = c("id" = "CarID"))

#9 has 2 'houses'

#Finding the home for 21
emplyr_in_q <- location_gps %>%
  filter(id == 21) %>%
  filter(lat11 != 36.0480 & long11 != 24.8796) %>%
  filter(stop >= 60*60*8) %>%
  mutate(lat = lat11, long = long11) %>%
  group_by(id, lat11, long11) %>%
  count(lat11, long11, name = "count") %>%
  ungroup()

x <- emplyr_in_q %>%
  distinct(id, lat11, long11)

x <- x[order(x$lat11),]

detailed_home_list_21 <- x %>%
  mutate(lat_interval = lat11 - lag(lat11), long_interval = long11 - lag(long11)) %>%
  filter(lead(between(lat_interval, -0.0001, 0.0001)) | 
           lead(between(long_interval, -0.0001, 0.0001)) |
           between(lat_interval, -0.0001, 0.0001) | 
           between(long_interval, -0.0001, 0.0001)) %>%
  dplyr::select(-c(lat_interval, long_interval)) %>%
  filter(!between(lat11, 36.0633, 36.0635) & !between(long11, 24.8961, 24.8963)) %>%
  left_join(car_data, by = c("id" = "CarID")) %>%
  mutate(category = "Home")

simplified_home_list_21 <- left_join(x, emplyr_in_q, by = c("lat11" = "lat11", "long11" = "long11", "id" = "id")) %>%
  filter(count > 1) %>%
  dplyr::select(-c(count)) %>%
  mutate(lat_interval = lat11 - lag(lat11), long_interval = long11 - lag(long11)) %>%
  filter(between(lat_interval, -0.0001, 0.0001) | 
           between(long_interval, -0.0001, 0.0001)) %>%
  filter(lat11 != 36.0634 & long11 != 24.8962) %>%
  dplyr::select(-c(lat_interval, long_interval)) %>%
  left_join(car_data, by = c("id" = "CarID"))

#21 has 2 'houses'. 36.0634/24.8962 is established to be 14's residence

#Finding the home for 25
emplyr_in_q <- location_gps %>%
  filter(id == 25) %>%
  filter(lat11 != 36.0480 & long11 != 24.8796) %>%
  filter(stop >= 60*60*8) %>%
  mutate(lat = lat11, long = long11) %>%
  group_by(id, lat11, long11) %>%
  count(lat11, long11, name = "count") %>%
  filter(count != 1) %>% #delete the lat/long for this, might be a visit
  ungroup()

x <- emplyr_in_q %>%
  distinct(id, lat11, long11)

x <- x[order(x$lat11),]

detailed_home_list_25 <- x %>%
  mutate(lat_interval = lat11 - lag(lat11), long_interval = long11 - lag(long11)) %>%
  filter(lead(between(lat_interval, -0.0001, 0.0001)) | 
           lead(between(long_interval, -0.0001, 0.0001)) |
           between(lat_interval, -0.0001, 0.0001) | 
           between(long_interval, -0.0001, 0.0001)) %>%
  dplyr::select(-c(lat_interval, long_interval)) %>%
  left_join(car_data, by = c("id" = "CarID")) %>%
  mutate(category = "Home")

simplified_home_list_25 <- left_join(x, emplyr_in_q, by = c("lat11" = "lat11", "long11" = "long11", "id" = "id")) %>%
  filter(count > 1) %>%
  dplyr::select(-c(count)) %>%
  mutate(lat_interval = lat11 - lag(lat11), long_interval = long11 - lag(long11)) %>%
  filter(is.na(lat_interval)) %>%
  dplyr::select(-c(lat_interval, long_interval)) %>%
  left_join(car_data, by = c("id" = "CarID"))

#Finding the home for 28
emplyr_in_q <- location_gps %>%
  filter(id == 28) %>%
  filter(lat11 != 36.0480 & long11 != 24.8796) %>%
  filter(stop >= 60*60*8) %>%
  mutate(lat = lat11, long = long11) %>%
  group_by(id, lat11, long11) %>%
  count(lat11, long11, name = "count") %>%
  ungroup()

x <- emplyr_in_q %>%
  distinct(id, lat11, long11)

x <- x[order(x$lat11),]

detailed_home_list_28 <- x %>%
  mutate(lat_interval = lat11 - lag(lat11), long_interval = long11 - lag(long11)) %>%
  filter(lead(between(lat_interval, -0.0001, 0.0001)) | 
           lead(between(long_interval, -0.0001, 0.0001)) |
           between(lat_interval, -0.0001, 0.0001) | 
           between(long_interval, -0.0001, 0.0001)) %>%
  dplyr::select(-c(lat_interval, long_interval)) %>%
  left_join(car_data, by = c("id" = "CarID")) %>%
  mutate(category = "Home")

simplified_home_list_28 <- left_join(x, emplyr_in_q, by = c("lat11" = "lat11", "long11" = "long11", "id" = "id")) %>%
  filter(count > 1) %>%
  dplyr::select(-c(count)) %>%
  mutate(lat_interval = lat11 - lag(lat11), long_interval = long11 - lag(long11)) %>%
  filter(is.na(lat_interval)) %>%
  dplyr::select(-c(lat_interval, long_interval)) %>%
  left_join(car_data, by = c("id" = "CarID"))


#############################################################################
#reorder

detailed_home_list <- detailed_home_list[c(4,5,6,7,1,2,3,8)]
simplified_home_list <- simplified_home_list[c(4,5,6,7,1,2,3)]

detailed_home_list_9 <- detailed_home_list_9[c(4,5,6,7,1,2,3,8)]
simplified_home_list_9 <- simplified_home_list_9[c(4,5,6,7,1,2,3)]

detailed_home_list_21 <- detailed_home_list_21[c(4,5,6,7,1,2,3,8)]
simplified_home_list_21 <- simplified_home_list_21[c(4,5,6,7,1,2,3)]

detailed_home_list_25 <- detailed_home_list_25[c(4,5,6,7,1,2,3,8)]
simplified_home_list_25 <- simplified_home_list_25[c(4,5,6,7,1,2,3)]

detailed_home_list_28 <- detailed_home_list_28[c(4,5,6,7,1,2,3,8)]
simplified_home_list_28 <- simplified_home_list_28[c(4,5,6,7,1,2,3)]
##############################################################################

##############################################################################

m_detailed_home_list <- rbind(detailed_home_list, detailed_home_list_9, detailed_home_list_21, detailed_home_list_25, detailed_home_list_28)
m_simplified_home_list <- rbind(simplified_home_list, simplified_home_list_9, simplified_home_list_21, simplified_home_list_25, simplified_home_list_28)

##############################################################################


POI_gps <- location_gps %>%
  group_by(lat11, long11) %>%
  count(lat11, long11, name = "numberoflocations") %>%
  ungroup() %>%
  mutate(lat_interval = lat11 - lag(lat11)) %>%
  mutate(long_interval = long11 - lag(long11)) %>%
  mutate(lat = lat11, long = long11)

x.refinedPOI_gps <- anti_join(POI_gps, m_detailed_home_list, by = c("lat11" = "lat11", "long11" = "long11"))

#distinct POIs
refinedPOI_gps <- x.refinedPOI_gps %>%
  filter(!between(lat_interval, -0.0001, 0.0001) & !between(long_interval, -0.0001,0.0001) | is.na(lat_interval)) %>%
  dplyr::select(-c(lat_interval, long_interval, numberoflocations)) %>%
  mutate(lat = lat11, long = long11)

#detailed distinct POIs
detailedPOI_gps <- x.refinedPOI_gps %>%
  dplyr::select(-c(lat_interval, long_interval, numberoflocations)) %>%
  mutate(lat = as.numeric(lat11), long = as.numeric(long11))

dtlat <- c(36.0480,
           36.0481,
           36.0743,
           36.0658,
           36.0671,
           36.0640,
           36.0529,
           36.0572,
           36.0509,
           36.0546,
           36.0544,
           36.0557,
           36.0696,
           36.0675,
           36.0817,
           36.0895,
           36.0604,
           36.0603,
           36.0597,
           36.0634,
           36.0632,
           36.0598)

dtlong <- c(24.8796,
            24.8796,
            24.8460,
            24.8498,
            24.8467,
            24.8414,
            24.8494,
            24.8449,
            24.8259,
            24.8898,
            24.8999,
            24.9025,
            24.8691,
            24.8733,
            24.8509,
            24.8607,
            24.8565,
            24.8565,
            24.8580,
            24.8510,
            24.8523,
            24.8580)

category <- c("GASTech",
              "GASTech",
              "Abila Scrap",
              "Kronos Mart",
              "Kronos Mart",
              "Maximum Iron and Steel",
              "Kronos Capital",
              "Pilau Park",
              "Abila Airport",
              "Coffee Chameleon",
              "Katrina's Cafe",
              "Brew've Been Served",
              "U-Pump",
              "Jack's Magic Beans",
              "Bean There, Done That",
              "Desafio Golf Course",
              "General Grocer",
              "General Grocer",
              "General Grocer",
              "Robert and Sons",
              "Roberts and Sons",
              "General Grocer")

detailednewPOIs <- data.frame(dtlat, dtlong, category)

x.detailedPOI_gps <- left_join(detailedPOI_gps, detailednewPOIs, by = c("lat" = "dtlat", "long" = "dtlong"))

detailedPOI_gps <- x.detailedPOI_gps


###############################map plotting codes##############################

m_simplified_home_list_sf <- st_as_sf(m_simplified_home_list, 
                                      coords = c("long11", "lat11"), 
                                      crs = 4326) %>%
  st_cast("POINT")

refinedPOI_gps_sf <- st_as_sf(refinedPOI_gps, 
                              coords = c("long11", "lat11"), 
                              crs = 4326) %>%
  st_cast("POINT")

bgmap <- raster("data/Geospatial/MC2-tourist.tif")


Abila_st <- st_read(dsn = "data/Geospatial", 
                    layer = 'Abila')
####################################

tmap_Base <- tm_shape(bgmap) +
  tm_rgb(bgmap, r = 1, g = 2, b = 3,
         alpha = 0.5,
         saturation = 1,
         interpolate = TRUE,
         max.value = 255)

tmap_home <- tm_shape(m_simplified_home_list_sf) + 
  tm_dots(size = 0.05,
          alpha = 1,
          col = "blue")

tmap_refinedPOI <- tm_shape(refinedPOI_gps_sf) + 
  tm_dots(size = 0.02,
          alpha = 1,
          col = "cyan")