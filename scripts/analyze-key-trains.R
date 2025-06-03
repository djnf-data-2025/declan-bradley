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

# more than 500 trains traveling at least 10pmh over the limit at least twice
violators |>
  filter(excessSpeed >= 10) |>
  group_by(trainAppearances) |>
  summarize(numTrains = length(unique(trainId))) |>
  filter(trainAppearances >= 2) |>
  pull(numTrains) |>
  sum()

train_counts_by_excess <- violators |>
  mutate(excessSpeedBinned = floor(excessSpeed / 5)) |>
  group_by(excessSpeedBinned) |>
  summarize(numTrains = length(unique(trainId))) |>
  mutate(speedLabel = paste((excessSpeedBinned * 5) + 50, "-", (excessSpeedBinned * 5) + 5 + 50, " mph", sep="")) |>
  select(speedLabel, numTrains)

train_counts_by_excess |>
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

major_violators |>
  filter(state == "Ohio") |>
  pull(excessSpeed) |>
  favstats()

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

major_violators |>
  arrange(date)
