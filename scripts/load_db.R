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