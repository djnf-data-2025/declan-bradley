#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
library(tidyverse)

# A generic library for database connections
# https://dbi.r-dbi.org/
library(DBI)

# A specific package for the flavor of database you are connecting.  
# For MYSql, the package is library(RMySQL)
# For Postgres, the package is library(RPostgreSQL)
# I'm using a SQLite database for this example
# https://rsqlite.r-dbi.org/
library(RSQLite)

# For R to SQL translation
library(dbplyr)
library(janitor)
library(googlesheets4)
library(readxl)
library(here)
library(leaflet)
library(googledrive)
source('declan_railstate/shared_scripts/helper_functions.R')

placards_crosswalk <- read_csv(here("declan_railstate/other_data/placards_crosswalk_cfr_census_railstate.csv")) %>%
  filter(in_railstate==TRUE) %>%
  select(placard_type, hazmat_class_railstate, hazmat_name_cfr) 

conn <- create_railstate_db_connection(db_type="merged", merged_geography="all_sensors", sensor_number=NULL)

railstate_tables_and_cols <- get_railstate_tables_and_cols(conn=conn, get_count_of_rows_very_slow = FALSE)

railstate_table_connections <- create_table_connections(conn)

#
#
#

cars_w_hazmat_classes <- railstate_table_connections$tHazmat |>
  filter(car_hazmat_info_str != "[]") %>%
  filter(car_hazmat_info_str != "null") %>%
  filter(car_hazmat_info_str != '[{"placardType": null, "hazmatClass": null}]') %>%
  filter(car_hazmat_info_str != '[{"placardType": "EMPTY", "hazmatClass": null}]') %>%
  filter(placardType != "OTHER") %>%
  filter(placardType != "EMPTY")

cars_w_hazmat_classes <- cars_w_hazmat_classes |>
  left_join(railstate_table_connections$tTrainSightings |>
            select(sightingId, trainId))

cars_w_hazmat_classes <- cars_w_hazmat_classes |>
  left_join(railstate_table_connections$tCars |>
            select(sightingId, carPosition, type, isLoaded),
            by=c('sightingId', 'carPosition'))

# presuming 1 means true
cars_w_hazmat_classes <- cars_w_hazmat_classes |>
  mutate(key_car = case_when(
    (placardType %in% c("UN1005", "UN3318")) && (type == "Tank Car") && (is.na(isLoaded) || (isLoaded == 1)) ~ TRUE,
    TRUE ~ FALSE
  )) |>
  mutate(hazard_shipment = case_when(
    ((placardType %in% c("UN1005", "UN3318")) || (hazmatClass %in% c(2.1, 1.1, 1.2))) && (is.na(isLoaded) || (isLoaded == 1)) ~ TRUE,
    TRUE ~ FALSE
  ))

key_cars_per_train <- cars_w_hazmat_classes |>
  select(trainId, key_car, hazard_shipment) |>
  group_by(trainId) |>
  summarize(num_key_cars = sum(key_car))

hazard_shipments_per_train <- cars_w_hazmat_classes |>
  select(trainId, key_car, hazard_shipment) |>
  group_by(trainId) |>
  summarize(num_hazard_shipments = sum(hazard_shipment))

danger_cars_per_train <- left_join(key_cars_per_train, hazard_shipments_per_train, by='trainId')

key_trains <- danger_cars_per_train |>
  filter(num_key_cars >= 5 || num_hazard_shipments >= 20)

# disclaimer: as of midnight I have not been able to attain a result from anything below this step. I believe the code is correct, and that it will eventually produce an answer, but when I run it my console freezes and blocks further input. I have seen this happen before today, the command evnetually produced a correct result after nearly 20 minutes of processing. I have no way to estimate how long this process will take, but hope it will be done by morning.
key_train_ids <- key_trains |>
  pull(trainId) |>
  unique()

violators <- railstate_table_connections$tTrainSightings |>
  filter((trainId %in% key_train_ids))

violators |>
  collect() |>
  write.csv('violating-trains.csv', row.names=FALSE)

#
#
#
