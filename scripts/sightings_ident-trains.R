source('scripts/load_db.R')
library(mosaic)

df <- railstate_table_connections$tHazmat |>
  left_join(railstate_table_connections$tTrainSightings |> 
    select(sightingId, sensorId), by='sightingId')

df <- df |>
  left_join(railstate_table_connections$tCars |>
    select(sightingId, carPosition, type, isLoaded),
    by=c('sightingId', 'carPosition'))

df <- df |>
  mutate(key_car = case_when(
    ((placardType %in% c("UN1005", "UN3318")) | (hazmatClass %in% c(6.1))) & (type == "Tank Car") & (is.na(isLoaded) || (isLoaded == 1)) ~ TRUE,
    TRUE ~ FALSE
  )) |>
  mutate(hazard_shipment = case_when(
    ((placardType %in% c("UN1005", "UN3318")) | (hazmatClass %in% c(2.1, 1.1, 1.2, 6.1))) & (is.na(isLoaded) | (isLoaded == 1)) ~ TRUE,
    TRUE ~ FALSE
  ))

df <- df |>
  group_by(sightingId) |>
  summarize(num_key_cars = sum(key_car), num_hazard_shipments = sum(hazard_shipment))

df <- df |>
  mutate(is_key_train = ((num_key_cars >= 5) | (num_hazard_shipments >= 20)))

write.csv(df, "data/sighting-id_train-car-counts.csv.csv", row.names=FALSE)

df <- read_csv("data/sighting-id_train-car-counts.csv.csv")

df <- df |>
  left_join(railstate_table_connections$tTrainSightings |>
            select(sightingId, speedMph) |>
            collect(), by='sightingId')

write.csv(df, 'data/key-sightings.csv', row.names=FALSE)