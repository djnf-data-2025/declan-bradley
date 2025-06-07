source('scripts/load_db.R')
library(mosaic)
library(tidygeocoder)

sighting_counts <- read_csv('data/key-sightings.csv') |>
  mutate(is_key_train = case_when(
    is_key_train == 0 ~ FALSE,
    is_key_train == 1 ~ TRUE
  ))

sighting_counts <- sighting_counts |>
  left_join(read_csv('data/train-ids.csv'))

key_sightings <- sighting_counts |>
  filter(is_key_train)

violators <- key_sightings |> 
  filter(is_key_train) |>
  filter(speedMph > 50)

violators <- violators |>
  left_join(railstate_table_connections$tTrainSightings |>
            select(sightingId, sensorId) |>
            collect(), by='sightingId')

sensor_info <- railstate_table_connections$tSensorInfo |> 
  select(sensorId, name, lat, lng, timezone, country, region) |>
  rename(sensorName = name) |>
  collect()

violators <- left_join(violators, sensor_info, by='sensorId')

# a bunch of these are in canada and therefore not actually illegal
violators |>
  group_by(country) |>
  summarize(num_trains = length(sightingId)) |>
  arrange(desc(num_trains))

violators <- violators |>
  filter(country == "United States")

violators <- violators |>
  mutate(excessSpeed = speedMph - 50)

# most violators have low excesses, median 2.27 mph in excess
favstats(violators$excessSpeed)

#66 traveling more than 5 mph over the limit
violators |> 
  filter(excessSpeed >= 5) |> 
  dim()

# 12 more than 10pmh over
violators |> 
  filter(excessSpeed >= 10) |> 
  dim()

# there were 102102 sightings of key trains operating in the us in this data
key_sightings |>
  pull(sightingId) |>
  unique() |>
  length()

# 294 sightings of speeding key trains
violators |>
  pull(sightingId) |>
  unique() |>
  length()

train_counts_by_excess <- violators |>
  mutate(excessSpeedBinned = floor(excessSpeed / 5)) |>
  group_by(excessSpeedBinned) |>
  summarize(numSightings = length(sightingId)) |>
  mutate(speedLabel = paste((excessSpeedBinned * 5) + 50, "-", (excessSpeedBinned * 5) + 5 + 50, " mph", sep="")) |>
  select(speedLabel, numSightings) |>
  mutate(percentOfSightings = numSightings / key_sightings |>
    pull(sightingId) |>
    unique() |>
    length())

train_counts_by_excess |>
  mutate(percentOfSightings = round(percentOfSightings * 100, digits=2)) |>
  mutate(percentOfSightings = paste(percentOfSightings, "%", sep="")) |>
  write.csv('viz/train-counts-by-excess.csv', row.names=FALSE)



# major violations
major_violators <- violators |>
  filter(excessSpeed >= 10)

# 50 violations by Northfolk Southern
major_violators |> 
  group_by(trainOperator) |>
  summarize(num_trains = length(unique(trainId))) |>
  arrange(desc(num_trains))

major_violators <- major_violators |>
  reverse_geocode(lat, lng, full_results=FALSE)

major_violators <- major_violators |>
  mutate(state = str_squish(str_split_i(address, ",", -3)))

# only one wrong
major_violators |>
  filter(state %in% state.name) |>
  dim()
dim(major_violators)

# 50 in ohio
# that's where the east palestine accident happened!
# starting to get somewhere here
major_violators |>
  group_by(state) |>
  summarize(num_trains = length(unique(trainId)))

# approximately same number of accidents, couple of repeat violators in Texas
major_violators |>
  group_by(state) |>
  summarize(num_incidents = length(trainId))

major_violators <- major_violators |>
  mutate(detectionTimeSensorLocal = parse_date_time(detectionTimeSensorLocal, orders="%Y-%m-%d %H:%M:%S")) |>
  mutate(date = date(detectionTimeSensorLocal))

major_violators |>
  count(year(date))

# speeding is more common in winter
major_violators |>
  count(month(date))

# all 50 major violators in ohio were operated by norfolk southern
major_violators |>
  filter(state == "Ohio") |>
  count(trainOperator)

# the same operator of the east palestine derailment!!!!
# https://www.norfolksouthern.com/
# https://en.wikipedia.org/wiki/East_Palestine,_Ohio,_train_derailment
# real independent finding

# all the violators found on the day of the east palestine accident were in canada
violators |> 
  filter(year(detectionTimeUTC) == 2023) |> 
  filter(month(detectionTimeUTC) == 2) |> 
  filter(day(detectionTimeUTC) == 3) |> 
  reverse_geocode(lat, lng) |> 
  select(detectionTimeUTC, speedMph, address) |> 
  pull(address)


major_violators <- major_violators |> 
  filter(state == "Ohio") |> 
  mutate(town = str_split_i(address, ",", 3))

# all of the major violations in ohio are detected by the new waterford sensor, right outside east palestine
# 4.6 miles, 8 minute drive
major_violators |>
  pull(town) |>
  unique()

ohio_violations_tbl <- major_violators |> 
  filter(state == "Ohio") |> 
  select(detectionTimeSensorLocal, trainOperator, trainId, speedMph, excessSpeed)

ohio_violations_tbl <- ohio_violations_tbl |>
  mutate(date = date(detectionTimeSensorLocal))

ohio_violations_tbl$time <- ohio_violations_tbl |> 
  pull(detectionTimeSensorLocal) |> 
  str_split_i("T", 2) |> 
  str_split_i("\\.", 1) |> parse_date_time(orders="%H:%M:%S")

ohio_violations_tbl <- ohio_violations_tbl |>
  mutate(date_clean = strftime(date, "%b %d, %Y")) |>
  mutate(time_clean = strftime(time, "%H:%M %p"))

ohio_violations_tbl <- ohio_violations_tbl |> 
  arrange(desc(speedMph)) |>
  select(date_clean, time_clean, speedMph, trainOperator) |>
  mutate(trainOperator = gsub("NS", "Norfolk Southern", trainOperator)) |>
  rename(Date = date_clean) |>
  rename(`Local Time` = time_clean) |>
  rename(`Speed mph` = speedMph) |>
  rename(`Train Operator` = trainOperator)

ohio_violations_tbl |>
  write.csv('viz/east_palestine-viols.csv', row.names=FALSE)

# let's look at the east palestine sensor in summary
# the sensor has id 255
major_violators |> 
  pull(sensorId) |>
  unique()

sensor_255_info <- railstate_table_connections$tSensorInfo |> 
  filter(sensorId == 255)

#this sensor is only 7 miles from the accident site
# wikipedia, NEEDS to be checked but fine for preliminary since data confirms approximate location https://en.wikipedia.org/wiki/East_Palestine,_Ohio,_train_derailment
sensor_255_info


east_palestine_sightings <- railstate_table_connections$tTrainSightings |>
  filter(sensorId == 255) |>
  select(sightingId, trainId, detectionTimeSensorLocal, direction, trainType, speedMph, trainOperator, estGallonageCapacity, estLengthFeet, warnings, issues)

east_palestine_sightings |>
  write.csv("data/east-palestine-sightings.csv", row.names=FALSE)

east_pal <- read_csv('data/east-palestine-sightings.csv')

east_pal <- east_pal |>
  filter(trainId %in% key_trains$trainId)

# there have been 457 key trains passing by east pal
dim(east_pal)

east_pal <- east_pal |>
  mutate(excessSpeed = speedMph - 50)

# 10% of all key train sightings in east palestine are speeding by 10 mph or more
east_pal |> 
  filter(excessSpeed >= 10) |>
  pull(trainId) |>
  length() / east_pal |> 
    pull(trainId) |>
    length()

# ns is the only company operating hazard trains near east pal
east_pal |>
  group_by(trainOperator) |>
  summarize(numSightings = length(trainId))

# all 50 of the speeding hazard trains are ns operated
east_pal |>
  filter(excessSpeed >= 10) |>
  group_by(trainOperator) |>
  summarize(numSightings = length(trainId))

east_pal <- east_pal |>
  mutate(date = str_split_i(detectionTimeSensorLocal, "T", 1)) |>
  mutate(date = parse_date_time(date, orders="%Y-%m-%d")) |>
  mutate(time = detectionTimeSensorLocal |>
    str_split_i("T", 2) |> 
    str_split_i("\\.", 1) |> 
    parse_date_time(orders="%H:%M:%S"))

# almost all speeders are eastbound
east_pal |> 
  filter(excessSpeed >= 10) |> 
  group_by(direction) |> 
  summarize(speeders = length(trainId))

# but key trains in general are more likely to westbound, making this disproprortion particularly striking
east_pal |> 
  count(direction)

# all speeding key trains are manifest trains
east_pal |> 
  filter(excessSpeed >= 10) |> 
  count(trainType)

east_pal_viz <- east_pal |>
  arrange(detectionTimeSensorLocal) |>
  select(detectionTimeSensorLocal, date, time, speedMph, excessSpeed, trainId) |>
  mutate(`Speed Limit` = case_when(
    excessSpeed >= 10 ~ "Severe Violations of Safety Codes",
    excessSpeed < 10 & excessSpeed > 0 ~ "Minor Violations",
    TRUE ~ "Operating Within Limits"
  )) |>
  mutate(`Local Time` = strftime(time, "%H:%M %p")) |>
  select(!time)

east_pal_viz |>
  rename(`Date & Time` = detectionTimeSensorLocal,
        Date = date,
        `Speed mph` = speedMph) |>
  mutate(Location = "East Palestine, OH") |>
  write.csv("viz/east-pal_lollipops.csv", row.names=FALSE)

east_pal |>
  filter(excessSpeed > 0) |>
  filter(excessSpeed < 10) |>
  pull(trainId) |>
  length()


# ranking of materials

major_violating_trains <- major_violators |>
  pull(trainId) |> 
  unique()

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

major_violating_cars <- cars_w_hazmat_classes |>
  filter(trainId %in% major_violating_trains) |>
  collect()

write.csv(major_violating_cars, "data/major-violating-cars.csv", row.names=FALSE)

material_counts <- major_violating_cars |>
  group_by(placardType, hazmatClass) |>
  summarize(num_trains = length(unique(trainId)),
            num_cars = length(carPosition))

placards_crosswalk <- read_csv(here("declan_railstate/other_data/placards_crosswalk_cfr_census_railstate.csv")) %>%
  filter(in_railstate==TRUE) %>%
  select(placard_type, hazmat_class_railstate, hazmat_name_cfr) 
  
material_counts <- material_counts |>
  left_join(placards_crosswalk |>
    rename(placardType = placard_type, hazmatClass=hazmat_class_railstate), by=c('placardType', 'hazmatClass'))

materials_table <- material_counts |> 
  select(hazmat_name_cfr, num_trains, num_cars) |> 
  arrange(desc(num_trains)) |>
  mutate(percentTrains = num_trains / length(major_violating_trains))

east_pal_sightings <- railstate_table_connections$tTrainSightings |>
  filter(sensorId == 255) |>
  pull(sightingId) |>
  unique()

most_common_speeding_materials <- major_violating_cars |>
  filter(sightingId %in% east_pal_sightings) |>
  group_by(placardType, hazmatClass) |>
  summarize(num_trains = length(unique(trainId)),
            num_cars = length(carPosition)) |>
  left_join(placards_crosswalk |>
    rename(placardType = placard_type, hazmatClass=hazmat_class_railstate), by=c('placardType', 'hazmatClass')) |>
  select(hazmat_name_cfr, num_trains, num_cars) |> 
  arrange(desc(num_trains))

materials_table |>
  select(hazmat_name_cfr, num_trains, num_cars) |> 
  arrange(desc(num_trains)) |>
  head(n=10) |>
  select(hazmat_name_cfr, num_trains, num_cars) |>
  rename(`Hazardous Material` = hazmat_name_cfr,
        `Train Sightings` = num_trains,
        `Total Cars` = num_cars) |>
  write.csv('viz/speeding-materials.csv')

# verify that these are definitely harzard trains at the time of interception
east_pal_sightings <- railstate_table_connections$tHazmat |>
  left_join(railstate_table_connections$tTrainSightings |> 
    select(sightingId, sensorId), by='sightingId') |>
  filter(sensorId == 255)

east_pal_sightings <- east_pal_sightings |>
  left_join(railstate_table_connections$tCars |>
    select(sightingId, carPosition, type, isLoaded),
    by=c('sightingId', 'carPosition'))

east_pal_sightings <- east_pal_sightings |>
  mutate(key_car = case_when(
    ((placardType %in% c("UN1005", "UN3318")) | (hazmatClass %in% c(6.1))) & (type == "Tank Car") & (is.na(isLoaded) || (isLoaded == 1)) ~ TRUE,
    TRUE ~ FALSE
  )) |>
  mutate(hazard_shipment = case_when(
    ((placardType %in% c("UN1005", "UN3318")) | (hazmatClass %in% c(2.1, 1.1, 1.2, 6.1))) & (is.na(isLoaded) | (isLoaded == 1)) ~ TRUE,
    TRUE ~ FALSE
  ))

east_pal_sightings <- east_pal_sightings |>
  group_by(sightingId) |>
  summarize(num_key_cars = sum(key_car), num_hazard_shipments = sum(hazard_shipment))

east_pal_sightings <- east_pal_sightings |>
  mutate(is_key_train = ((num_key_cars >= 5) | (num_hazard_shipments >= 20)))

write.csv(east_pal_sightings, "data/east-pal_by_sightingId.csv", row.names=FALSE)

east_pal_sightings <- read_csv('data/east-pal_by_sightingId.csv')

east_pal_sightings <- east_pal_sightings |>
  left_join(railstate_table_connections$tTrainSightings |>
            filter(sensorId) |>
            select(sightingId, speedMph) |>
            collect(), by='sightingId')
