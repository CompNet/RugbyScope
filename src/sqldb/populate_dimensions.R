########################################################################
# Add the dimension tables to an existing empty database.
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/sqldb/populate_dimensions.R")
########################################################################
library("DBI")
library("RSQLite")
library("readr")
library("dplyr")
library("stringr")
library("tidyr")

source("src/common/logging.R")




########################################################################
# database file
db_file <- file.path("data", "rugbyscope3.sqlite")

# connect and create database if it does not exist
con <- dbConnect(SQLite(), db_file)

# enable foreign key support
dbExecute(con, "PRAGMA foreign_keys = ON;")




########################################################################
# load CSV tables
data_folder <- file.path("data", "fusion")

tlog("Loading CSV tables")

teams <- read.csv(file.path(data_folder, "teams_09.csv"))
tlog(2, "Number of teams: ", nrow(teams))

players <- read.csv(file.path(data_folder, "players_13.csv"))
tlog(2, "Number of players: ", nrow(players))

stints <- read.csv(file.path(data_folder, "stints_20_firststint.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# helper functions

split_values <- function(x) {
    if(is.na(x) || x == "")
        return(character(0))

    values <- str_split(x, "; ")[[1]]
    values <- str_trim(values)
    values[values != ""]
}

get_dimension_id <- function(table, column, value) {
    result <- dbGetQuery(
        con,
        paste0(
            "SELECT rugbyscope_id
             FROM ",
            table,
            "
             WHERE ",
            column,
            " = ?"
        ),
        params=list(value)
    )

    if(nrow(result) == 0)
        return(NA_integer_)

    result$rugbyscope_id
}

insert_dimension <- function(table, column, values) {
    values <- unique(values)

    values <- values[!is.na(values) & values!=""]

    for(v in values) {
        dbExecute(con, paste0(
                "
                INSERT OR IGNORE INTO ",
                table,
                "(",
                column,
                ")
                VALUES(?)
                "
            ), params = list(v)
        )        
    }
}




########################################################################
# country dimension

country_values <- c(
    players$citizenships |>
        lapply(split_values) |>
        unlist(),
    players$sportCountries |>
        lapply(split_values) |>
        unlist(),
    teams$countries |>
        lapply(split_values) |>
        unlist()
)

insert_dimension(
    "country",
    "country_name",
    country_values
)




########################################################################
# location dimension

location_values <- c(
    players$birthPlaces |>
        lapply(split_values) |>
        unlist(),
    players$deathPlaces |>
        lapply(split_values) |>
        unlist(),
    teams$locations |>
        lapply(split_values) |>
        unlist()
)

insert_dimension(
    "location",
    "location_name",
    location_values
)




########################################################################
# venue dimension

venues <- teams |>
    select(
        homeVenueNames,
        homeVenueCapacities
    ) |>
    rowwise() |>
    mutate(
        names = list(split_values(homeVenueNames)),
        caps = list(split_values(homeVenueCapacities))
    ) |>
    unnest(names)

for(i in seq_len(nrow(venues))) {
    name <- venues$names[i]
    capacity <- NA
    dbExecute(
        con,
        "
        INSERT OR IGNORE INTO venue
        (
            name,
            capacity
        )
        VALUES (?,?)
        ",
        params = list(name, capacity)
    )
}




########################################################################
# competition dimension

competition_values <-
    teams$competitions |>
    lapply(split_values) |>
    unlist()

insert_dimension(
    "competition",
    "name",
    competition_values
)




########################################################################
# governing body dimension

governing_values <-
    teams$affiliations |>
    lapply(split_values) |>
    unlist()

insert_dimension(
    "governing_body",
    "name",
    governing_values
)




########################################################################
# data source dimension

source_values <-
    stints$dataSources |>
    lapply(split_values) |>
    unlist()

insert_dimension(
    "data_source",
    "name",
    source_values
)




########################################################################
# disconnect

dbDisconnect(con)
tlog("Dimension loading completed")
