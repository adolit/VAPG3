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
library(clock)
library(lubridate)
library(tidyverse)
library(DT)
library(shinyWidgets)

#import csv data ----
df_cc <- read_csv("data/aspatial/cc_data.csv")
df_loyalty <- read_csv("data/aspatial/loyalty_data.csv")
df_cars <- read_csv("data/aspatial/car-assignments.csv")
df_gps <- read_csv("data/aspatial/gps.csv")


#import georeference tif file
bgmap <- raster("data/Geospatial/abila_map.tif")


#data preparation ----
#clean credit card data
df_cc <- df_cc %>%
  mutate(location = ifelse(str_detect(location, "Katerina"), "Katerina's Cafe", location))

df_cc$date <- date_time_parse(df_cc$timestamp,
                              zone = "",
                              format = "%m/%d/%Y")

df_cc$day <- wday(df_cc$date,
                  label = TRUE,
                  abbr = TRUE)

df_cc$timestamp <- date_time_parse(df_cc$timestamp,
                                   zone = "",
                                   format = "%m/%d/%Y %H:%M")

df_cc$hour <- get_hour(df_cc$timestamp)

#clean loyalty data
df_loyalty <- df_loyalty %>%
  mutate(location = ifelse(str_detect(location, "Katerina"), "Katerina's Cafe", location))

df_loyalty$date <- date_time_parse(df_loyalty$timestamp,
                                   zone = "",
                                   format = "%m/%d/%Y")

df_loyalty$timestamp <- date_time_parse(df_loyalty$timestamp,
                                        zone = "",
                                        format = "%m/%d/%Y")

df_loyalty$day <- wday(df_loyalty$timestamp,
                       label = TRUE,
                       abbr = TRUE)


#clean car assignment
df_cars <- df_cars %>%
  #concatenate first and last name
  mutate(FullName = paste(FirstName, LastName, sep = " ")) %>%
  rename(Department = CurrentEmploymentType) %>%
  rename(Title = CurrentEmploymentTitle)

df_cars$CarID <- as_factor(df_cars$CarID)

#clean gps data
df_gps <- df_gps %>%
  rename(timestamp = Timestamp) %>%
  rename(CarID = id)

df_gps$date <- date_time_parse(df_gps$timestamp,
                               zone = "",
                               format = "%m/%d/%Y")

df_gps$day <- as.factor(wday(df_gps$date,
                             label = TRUE,
                             abbr = TRUE))

df_gps$timestamp <- date_time_parse(df_gps$timestamp,
                                    zone = "",
                                    format = "%m/%d/%Y %H:%M:%S")

df_gps$hour <- get_hour(df_gps$timestamp)

df_gps$CarID <- as_factor(df_gps$CarID)


#data joining ----
#financial data
df_cc_loyalty <- full_join(df_cc %>% select(-c("day")),
                           df_loyalty %>% select(-c("day","timestamp")), 
                           by = c("date" = "date", 
                                  "location" = "location", 
                                  "price" = "price"))

df_cc_loyalty$day <- wday(df_cc_loyalty$date,
                          label = TRUE,
                          abbr = TRUE)

#rearrange columns
df_cc_loyalty <- df_cc_loyalty %>%
  select("timestamp", "date", "day", "hour", "location", "price", "last4ccnum", "loyaltynum")


#geospatial data
df_car_gps <- left_join(df_gps, 
                        df_cars %>% select(-c("FirstName", "LastName")),
                        by = "CarID")

sf_gps <- st_as_sf(df_gps,
                   coords = c("long", "lat"),
                   crs = 4326,
                   remove = FALSE)

sf_car_gps <- left_join(sf_gps, 
                        df_cars %>% select(-c("FirstName", "LastName")),
                        by = "CarID")

sf_poi <- sf_car_gps %>%
  group_by(CarID) %>%
  mutate(DepartureTimestamp = lead(timestamp, order_by = CarID)) %>%
  mutate(Timestamp_diff_seconds = DepartureTimestamp - timestamp) %>%
  mutate(is_poi = ifelse(Timestamp_diff_seconds >= 60 * 5, TRUE, FALSE)) %>%
  filter(is_poi == TRUE)

sf_poi <- rename(sf_poi, ArrivalTimestamp = timestamp)

gps_dots_selected <- sf_poi %>%
  mutate(MinutesDuration = round(Timestamp_diff_seconds / 60, 2)) %>%
  select(long,
         lat,
         CarID,
         date,
         day,
         hour,
         ArrivalTimestamp,
         DepartureTimestamp,
         MinutesDuration,
         FullName,
         Department,
         Title)


#colors ----
low_color <- "lightyellow"
high_color <- "lightyellow4"