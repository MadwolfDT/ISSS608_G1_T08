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

x <- anti_join(POI_gps, m_detailed_home_list, by = c("lat11" = "lat11", "long11" = "long11"))

refinedPOI_gps <- x %>%
  filter(!between(lat_interval, -0.0001, 0.0001) & !between(long_interval, -0.0001,0.0001) | is.na(lat_interval)) %>%
  dplyr::select(-c(lat_interval, long_interval, numberoflocations)) %>%
  mutate(lat = lat11, long = long11)

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