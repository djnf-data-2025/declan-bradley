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
