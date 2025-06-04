source('scripts/load_db.R')
library(mosaic)

violators <- read_csv('data/violating-trains.csv')

sensor_info <- railstate_table_connections$tSensorInfo |> 
  select(sensorId, name, lat, lng, timezone, country, region) |>
  rename(sensorName = name) |>
  collect()

violators <- left_join(violators, sensor_info, by='sensorId')

# a bunch of these are in canada and therefore not actually illegal
violators |>
  group_by(country) |>
  summarize(num_trains = length(unique(trainId))) |>
  arrange(desc(num_trains))

violators <- violators |>
  filter(country == "United States")

violators <- violators |>
  mutate(excessSpeed = speedMph - 50)

# most violators have low excesses, median 3.5 mph in excess
favstats(violators$excessSpeed)

#579 traveling more than 5 mph over the limit
violators |> 
  filter(excessSpeed >= 5) |> 
  dim()

# 79 more than 10pmh over
violators |> 
  filter(excessSpeed >= 10) |> 
  dim()

# most (77) are unique trains, very few are repeat sightings
# hypothesis: speeding trains don't speed typically, these are single incidents per journey
violators |> 
  filter(excessSpeed >= 10) |> 
  pull(trainId) |> 
  unique() |> 
  length()

violators <- violators |>
  mutate(trainAppearances = violators$trainId |>
    lapply(function(x) {length(which(violators$trainId == x))}) |>
    unlist())

# 300 trains appear 3 times
violators |>
  group_by(trainAppearances) |>
  summarize(numTrains = length(unique(trainId)))

violators |>
  group_by(trainAppearances) |>
  summarize(typ_excess = median(excessSpeed))

# 9 trains traveling at least 10pmh over the limit at least twice
violators |>
  filter(excessSpeed >= 10) |>
  group_by(trainAppearances) |>
  summarize(numTrains = length(unique(trainId))) |>
  filter(trainAppearances >= 2) |>
  pull(numTrains) |>
  sum()

key_trains <- read_csv("data/key_trains.csv")

trains_by_sensor <- railstate_table_connections$tTrainSightings |>
  select(trainId, sensorId)

sensor_countries <- railstate_table_connections$tSensorInfo |>
  select(sensorId, country)

trains_by_country <- left_join(trains_by_sensor, sensor_countries, by='sensorId')
us_trains <- trains_by_country |> 
  filter(country == "United States")

us_train_ids <- us_trains |>
  pull(trainId)

us_key_trains <- key_trains |>
  filter(trainId %in% us_train_ids)

# there were 21811 unique key trains operating in the us in this data
us_key_trains |>
  pull(trainId) |>
  unique() |>
  length()

train_counts_by_excess <- violators |>
  mutate(excessSpeedBinned = floor(excessSpeed / 5)) |>
  group_by(excessSpeedBinned) |>
  summarize(numTrains = length(unique(trainId))) |>
  mutate(speedLabel = paste((excessSpeedBinned * 5) + 50, "-", (excessSpeedBinned * 5) + 5 + 50, " mph", sep="")) |>
  select(speedLabel, numTrains) |>
  mutate(percentOfUsOperating = numTrains / us_key_trains |>
    pull(trainId) |>
    unique() |>
    length())

train_counts_by_excess |>
  mutate(percentOfUsOperating = round(percentOfUsOperating * 100, digits=2)) |>
  mutate(percentOfUsOperating = paste(percentOfUsOperating, "%", sep="")) |>
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

# fixed the only bad one
major_violators[4,]$state <- str_squish(str_split_i(major_violators[4,]$address, ",", -2))

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
  mutate(detectionTimeUTC = parse_date_time(detectionTimeUTC, orders="%Y-%m-%d %H:%M:%S")) |>
  mutate(date = date(detectionTimeUTC))

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
