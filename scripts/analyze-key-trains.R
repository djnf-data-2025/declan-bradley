source('scripts/load_db.R')
library(mosaic)

violators <- read_csv('data/violating-trains.csv')

violators <- violators |>
  mutate(excessSpeed = speedMph - 50)

# most violators have low excesses, median 3.5 mph in excess
favstats(violators$excessSpeed)

#3.7k traveling more than 5 mph over the limit
violators |> 
  filter(excessSpeed >= 5) |> 
  dim()

# 1.1k more than 10pmh over
violators |> 
  filter(excessSpeed >= 10) |> 
  dim()

# most (1139) are unique trains, very few are repeat sightings
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

violators |>
  filter(excessSpeed >= 10) |>
  pull(trainId) |>
  unique() |>
  length()

sensor_info <- railstate_table_connections$tSensorInfo |> 
  select(sensorId, name, lat, lng, timezone, country, region) |>
  rename(sensorName = name) |>
  collect()

violators <- left_join(violators, sensor_info, by='sensorId')

violators |>
  filter(speedMph >= 90) |>
  select(detectionTimeSensorLocal, trainOperator, sensorName, lat, lng, region, country) |>
  pull(trainOperator)

violators |> 
  group_by(trainOperator) |>
  summarize(num_trains = length(unique(trainId))) |>
  arrange(desc(num_trains))

