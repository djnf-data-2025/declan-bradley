



# Create database connection
create_railstate_db_connection <- function(db_type="merged", merged_geography="all_sensors", sensor_number=NULL)  {
  
  # Set path options based on whether we want a database with lots of sensors or one sensor
  merged_db_path <- here::here("railstate_databases/merged_databases")
  individual_db_path <- here::here("railstate_databases/individual_sensor_databases") 
  
  if (db_type == "merged") {
    
    # Set base path
    base_path <- merged_db_path
    
    # Set db path based on geography
    if (merged_geography == "us") {
      db_path <- paste0(base_path, "/us_sensors/railstate_sensors_us.db")
    } else if (merged_geography == "canada") {
      db_path <- paste0(base_path, "/canada_sensors/railstate_sensors_canada.db")
    } else if (merged_geography == "all_sensors") {
      db_path <- paste0(base_path, "/all_sensors/merged_database_2025_03_11.db")
    } else {
      stop("Invalid merged_geography argument. Must be one of 'us', 'canada', or 'all_sensors'")
    }
  } else if (db_type == "individual_sensor") {
    if (is.null(sensor_number)) {
      stop("Must provide a sensor number when using db_type='individual_sensor'")
    } else {
      base_path <- individual_db_path
      db_path <- paste0(individual_db_path, "/railstate_sensor_", as.character(sensor_number), ".db")
    }
    
  }
  
  railstate_con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
  
  return(railstate_con)
}

get_railstate_tables_and_cols <- function(conn=conn, get_count_of_rows_very_slow = FALSE) {
  
  # Establish connection using the create_rs_db_connection function
  conn <- conn
  
  # List all tables in the database
  tables <- dbListTables(conn)
  
  # Initialize an empty list to store summary information
  summary_list <- list()
  
  # Loop through each table and collect summary info
  for (table_name in tables) {
    
    # Get the column information using PRAGMA
    query_col_info <- paste("PRAGMA table_info(", table_name, ")", sep = "")
    col_info <- dbGetQuery(conn, query_col_info)
    
    # Get the number of columns
    num_cols <- nrow(col_info)
    
    # Get the column names
    column_names <- col_info$name
    
    if (get_count_of_rows_very_slow == TRUE) {
      # Get the number of rows in the table
      query_count_rows <- paste("SELECT COUNT(*) FROM", table_name)
      count_rows <- dbGetQuery(conn, query_count_rows)
      num_rows <- count_rows[[1]]
    } else {
      num_rows <- "Not calculated"
    }
    
    # Store the information in a list
    summary_list[[table_name]] <- list(
      table_name = table_name,
      num_cols = num_cols,
      num_rows = num_rows,
      column_names = paste(column_names, collapse = ", ")
    )
  }
  
  # Convert the summary list to a dataframe
  rs_db_summary_df <- do.call(rbind, lapply(summary_list, as.data.frame, stringsAsFactors = FALSE))
  
  # Close the connection
  #dbDisconnect(conn)
  
  # Return the summary dataframe
  return(rs_db_summary_df)
}


get_railstate_db_table <- function(table_name, conn=conn, limit = 100000) {
  
  # Establish connection using the create_rs_db_connection function
  conn <- conn
  
  if (limit == FALSE) {
    
    query <- paste("SELECT * FROM", table_name)
    
  } else {
    query <- paste("SELECT * FROM", table_name, "LIMIT", limit)     
  }
  
  
  # Execute the query and fetch all rows
  result <- dbGetQuery(conn, query)
  
  
  
  # print returned only the first 100000 rows or whatever limit value is, say set limit value as argument to return more
  print(paste0("Returned only up to the first ", limit, " rows. Change limit argument to return more."))
  # Return the result as a dataframe
  return(result)
}

# Write a function to create connections to each table in conn loop
# Each table name connection object should be returned to the global environment
create_table_connections <- function(conn) {
  
  # Establish connection using the create_rs_db_connection function
  conn <- conn
  
  # List all tables in the database
  tables <- dbListTables(conn)
  
  # Initialize an empty list to store table connections
  table_connections <- list()
  
  # Loop through each table and collect summary info
  for (table_name in tables) {
    
    # Create table level connections
    table_connections[[table_name]] <- tbl(conn, table_name)
    print(table_name)
  }
  
  return(table_connections)
}