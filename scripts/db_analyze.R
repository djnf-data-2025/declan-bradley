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


# train sightings broken down by cars
railstate_table_connections$tCars %>%
  head()

railstate_table_connections$tCars %>%
  colnames()

railstate_table_connections$tCars %>%
  select(sightingId, carPosition, equipmentParameters) |>
  head() |>
  pull(equipmentParameters)


# exploring tHazmat
railstate_table_connections$tHazmat |>
  colnames()

railstate_table_connections$tHazmat |>
  head()

#exploring tSensorInfo
# stores geographic info
railstate_table_connections$tSensorInfo |>
  colnames()

railstate_table_connections$tSensorInfo |>
  head()

# exploring tTrainSightings
railstate_table_connections$tTrainSightings |>
  colnames()

railstate_table_connections$tTrainSightings |>
  head()

# do hazmat trains travel slower?
# in what regions are hazmat trains most common?

hazmat_sightings <- railstate_table_connections$tHazmat |>
  filter(car_hazmat_info_str != "[]") %>%
  filter(car_hazmat_info_str != "null") %>%
  filter(car_hazmat_info_str != '[{"placardType": null, "hazmatClass": null}]') %>%
  filter(car_hazmat_info_str != '[{"placardType": "EMPTY", "hazmatClass": null}]') %>%
  filter(placardType != "OTHER") %>%
  filter(placardType != "EMPTY") |>
  collect() |>
  colnames()
  left_join(placards_crosswalk |>
            rename(hazmatClass = hazmat_class_railstate) |>
            rename(placardType = placard_type), by=c('placardType', 'hazmatClass'))
  

hazmat_sightings_uni <- hazmat_sightings |> 
  select(sightingId, hazmat_name_cfr) |> 
  unique()

#verify sightings are unique
railstate_table_connections$tTrainSightings |>
  pull(sightingId) |>
  length()

railstate_table_connections$tTrainSightings |>
  pull(sightingId) |>
  unique() |>
  length()

detections <- railstate_table_connections$tTrainSightings |>
  select(sightingId, sensorId, detectionTimeSensorLocal, speedMph) |>
  collect()

materials_table_unique <- hazmat_sightings_uni |>
  collect() |>
  left_join(detections)

materials_by_speed <- materials_table_unique |>
  group_by(hazmat_name_cfr) |>
  summarize(meanSpeed = mean(speedMph)) |>
  arrange(desc(meanSpeed))

trains_matched_to_sightings <- railstate_table_connections$tTrainSightings |>
  select(sightingId, trainId) |>
  collect()

materials_on_trains <- hazmat_sightings |>
  left_join(trains_matched_to_sightings)

# petroleum and diesel most common
materials_on_trains |>
  select(hazmat_name_cfr, trainId) |>
  unique() |>
  group_by(hazmat_name_cfr) |>
  summarize(num_trains = length(hazmat_name_cfr)) |>
  arrange(desc(num_trains))

railstate_table_connections$tCars |>
  filter(hazmats != "null") |>
  colnames()


# start hazard analysis here
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

key_train_ids <- key_trains |>
  pull(trainId) |>
  unique()

violators <- railstate_table_connections$tTrainSightings |>
  filter((trainId %in% key_train_ids))

violators |>
  collect() |>
  write.csv('violating-trains.csv', row.names=FALSE)
