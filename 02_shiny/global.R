#Install and Launch R Packages
library(shiny)
library(shinythemes)
library(plotly)
library(ggiraph)
library(rgdal)
library(tmap)
library(sf)
library(raster)
library(igraph)
library(ggraph)
library(visNetwork)
library(collapsibleTree)
library(clock)
library(lubridate)
library(profvis)
library(DT)
library(shinyWidgets)
library(ggalluvial)
library(ggforce)
library(tidyverse)

#import csv data ----
#df_cc <- read_csv("data/aspatial/cc_data.csv")
# df_loyalty <- read_csv("data/aspatial/loyalty_data.csv")
# df_cars <- read_csv("data/aspatial/car-assignments.csv")
# df_gps <- read_csv("data/aspatial/gps.csv")


#import georeference tif file
bgmap <- raster("data/Geospatial/abila_map.tif")


#data preparation ----
#clean credit card data
# df_cc <- df_cc %>%
#   mutate(location = ifelse(str_detect(location, "Katerina"), "Katerina's Cafe", location))
# 
# df_cc$date <- date_time_parse(df_cc$timestamp,
#                               zone = "",
#                               format = "%m/%d/%Y")
# 
# df_cc$day <- wday(df_cc$date,
#                   label = TRUE,
#                   abbr = TRUE)
# 
# df_cc$timestamp <- date_time_parse(df_cc$timestamp,
#                                    zone = "",
#                                    format = "%m/%d/%Y %H:%M")
# 
# df_cc$hour <- get_hour(df_cc$timestamp)
# 
# df_cc$last4ccnum <- as_factor(df_cc$last4ccnum)
# 
# write_rds(df_cc, "data/aspatial/df_cc.rds")

df_cc <- read_rds("data/aspatial/df_cc.rds")


#clean loyalty data
# df_loyalty <- df_loyalty %>%
#   mutate(location = ifelse(str_detect(location, "Katerina"), "Katerina's Cafe", location))
# 
# df_loyalty$date <- date_time_parse(df_loyalty$timestamp,
#                                    zone = "",
#                                    format = "%m/%d/%Y")
# 
# df_loyalty$timestamp <- date_time_parse(df_loyalty$timestamp,
#                                         zone = "",
#                                         format = "%m/%d/%Y")
# 
# df_loyalty$day <- wday(df_loyalty$timestamp,
#                        label = TRUE,
#                        abbr = TRUE)
# 
# write_rds(df_loyalty, "data/aspatial/df_loyalty.rds")

df_loyalty <-  read_rds("data/aspatial/df_loyalty.rds")


#clean car assignment
# df_cars <- df_cars %>%
#   #concatenate first and last name
#   mutate(FullName = paste(FirstName, LastName, sep = " ")) %>%
#   rename(Department = CurrentEmploymentType) %>%
#   rename(Title = CurrentEmploymentTitle)
# 
# df_cars$CarID <- as_factor(df_cars$CarID)
# 
# write_rds(df_cars, "data/aspatial/df_cars.rds")

df_cars <- read_rds("data/aspatial/df_cars.rds")


#clean gps data
# df_gps <- df_gps %>%
#   rename(timestamp = Timestamp) %>%
#   rename(CarID = id)
# 
# df_gps$date <- date_time_parse(df_gps$timestamp,
#                                zone = "",
#                                format = "%m/%d/%Y")
# 
# df_gps$day <- as.factor(wday(df_gps$date,
#                              label = TRUE,
#                              abbr = TRUE))
# 
# df_gps$timestamp <- date_time_parse(df_gps$timestamp,
#                                     zone = "",
#                                     format = "%m/%d/%Y %H:%M:%S")
# 
# df_gps$hour <- get_hour(df_gps$timestamp)
# 
# df_gps$CarID <- as_factor(df_gps$CarID)
# 
# write_rds(df_gps, "data/aspatial/df_gps.rds")

df_gps <- read_rds("data/aspatial/df_gps.rds")


#data joining ----
#financial data
# df_cc_loyalty <- full_join(df_cc %>% select(-c("day")),
#                            df_loyalty %>% select(-c("day","timestamp")),
#                            by = c("date" = "date",
#                                   "location" = "location",
#                                   "price" = "price"))
# 
# df_cc_loyalty$day <- wday(df_cc_loyalty$date,
#                           label = TRUE,
#                           abbr = TRUE)
# 
# #rearrange columns
# df_cc_loyalty <- df_cc_loyalty %>%
#   select("timestamp", "date", "day", "hour", "location", "price", "last4ccnum", "loyaltynum")
# 
# 
# write_rds(df_cc_loyalty, "data/aspatial/df_cc_loyalty.rds")

df_cc_loyalty <- read_rds("data/aspatial/df_cc_loyalty.rds")

#geospatial data
#exceed github file size limit if stored as rds
df_car_gps <- left_join(df_gps,
                        df_cars %>% select(-c("FirstName", "LastName")),
                        by = "CarID")

sf_car_gps <- st_as_sf(df_car_gps,
                       coords = c("long", "lat"),
                       crs = 4326,
                       remove = FALSE)

# df_paths <- sf_car_gps %>%
#   group_by(CarID, date, hour) %>%
#   summarize(m = mean(timestamp),
#             do_union = FALSE) %>%
#   st_cast("LINESTRING")
# p = npts(df_paths, by_feature = TRUE)
# df_paths <- cbind(df_paths, p)
# df_paths <- df_paths %>% filter(p > 1)
# 
# write_rds(df_paths, "data/Geospatial/df_paths.rds")

df_paths <- read_rds("data/Geospatial/df_paths.rds")

sf_poi <- sf_car_gps %>%
  group_by(CarID) %>%
  mutate(DepartureTimestamp = lead(timestamp, order_by = CarID)) %>%
  mutate(Timestamp_diff_seconds = DepartureTimestamp - timestamp) %>%
  mutate(is_poi = ifelse(Timestamp_diff_seconds >= 60 * 5, TRUE, FALSE)) %>%
  filter(is_poi == TRUE)

sf_poi <- rename(sf_poi, ArrivalTimestamp = timestamp)

df_pois <- sf_poi %>%
  mutate(ArrivalDate = as.Date.POSIXct(ArrivalTimestamp,
                                       format = "%m/%d/%Y"),
         DepartureDate = as.Date.POSIXct(DepartureTimestamp,
                                         format = "%m/%d/%Y"),
         MinutesDuration = round(Timestamp_diff_seconds / 60, 2),
         HoursDuration = round(MinutesDuration /60, 2)) %>%
  select(long,
         lat,
         CarID,
         date,
         day,
         hour,
         ArrivalTimestamp,
         DepartureTimestamp,
         ArrivalDate,
         DepartureDate,
         MinutesDuration,
         HoursDuration,
         FullName,
         Department,
         Title)


# df_cclc_match <- read_csv("data/aspatial/cc_loyalty_match.csv")
# df_cclc_match$last4ccnum <- as_factor(df_cclc_match$last4ccnum)
# write_rds(df_cclc_match, "data/aspatial/df_cclc_match.rds")
df_cclc_match <- read_rds("data/aspatial/df_cclc_match.rds")


#import credit card and loyalty card owners ----
# df_cclc_owners <- read_csv("data/aspatial/cclc_owners_map.csv")
# df_cclc_owners$last4ccnum <- as_factor(df_cclc_owners$last4ccnum)
# write_rds(df_cclc_owners, "data/aspatial/df_cclc_owners.rds")

df_cclc_owners <- read_rds("data/aspatial/df_cclc_owners.rds")

# df_cc_map <- left_join(df_cc, df_cclc_owners, by = "last4ccnum") %>% 
#   mutate(Department = replace_na(Department,"Unknown")) %>% 
#   mutate(FullName = replace_na(FullName,"Unknown")) %>% 
#   mutate(Title = replace_na(Title,"Unknown"))
# write_rds(df_cc_map, "data/aspatial/df_cc_map.rds")

df_cc_map <- read_rds("data/aspatial/df_cc_map.rds")

#viz color gradient ----
low_color <- "lightyellow"
high_color <- "darkgoldenrod"
bg_color <- "snow"
