source('scripts/load_db.R')

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

danger_cars_per_train |>
  write.csv('declan_railstate/danger_cars.csv')

message('succesfully saved danger cars')

key_trains <- danger_cars_per_train |>
  filter(num_key_cars >= 5 || num_hazard_shipments >= 20)

message('identified key trains')

key_train_ids <- key_trains |>
  pull(trainId) |>
  unique()

message('saved key trains to vector')

violators <- railstate_table_connections$tTrainSightings |>
  filter((trainId %in% key_train_ids)) |>
  filter(speedMph >= 50)

message('identified violators')

violators |>
  collect() |>
  write.csv('violating-trains.csv', row.names=FALSE)

message('saved violators')