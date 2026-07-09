########################################################################
# Create the structure of the SQL DB.
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/sqldb/create_schema.R")
########################################################################
library("DBI")
library("RSQLite")

source("src/common/logging.R")




########################################################################
# database file
db_file <- file.path("data", "rugbyscope.sqlite")

# connect and create database if it does not exist
con <- dbConnect(SQLite(), db_file)

# enable foreign key support
dbExecute(con, "PRAGMA foreign_keys = ON;")




########################################################################
# core tables

dbExecute(con, "
CREATE TABLE IF NOT EXISTS governing_body (
    rugbyscope_id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS competition (
    rugbyscope_id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS venue (
    rugbyscope_id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT,
    capacity INTEGER
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS location (
    rugbyscope_id INTEGER PRIMARY KEY AUTOINCREMENT,
    location_name TEXT
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS country (
    rugbyscope_id INTEGER PRIMARY KEY AUTOINCREMENT,
    country_name TEXT
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS data_source (
    rugbyscope_id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT
);
")




########################################################################
# team table

dbExecute(con, "
CREATE TABLE IF NOT EXISTS team (
    rugbyscope_id INTEGER PRIMARY KEY,
    wikidata_id TEXT,
    full_name TEXT,
    type TEXT,
    inception_date DATE,
    termination_date DATE,
    tier INTEGER,
    all_rugby_id TEXT,
    google_knowl_id TEXT,
    wikipedia_en TEXT,
    wikipedia_fr TEXT,
    wikipedia_it TEXT,
    wikipedia_es TEXT,
    wikipedia_ja TEXT,
    dbpedia_id TEXT,
    comments TEXT
);
")




########################################################################
# player table

dbExecute(con, "
CREATE TABLE IF NOT EXISTS player (
    wikidata_id TEXT PRIMARY KEY,
    full_name TEXT,
    birth_date DATE,
    birth_place_id INTEGER,
    death_date DATE,
    death_place_id INTEGER,
    career_start_year INTEGER,
    career_end_year INTEGER,
    weight INTEGER,
    height INTEGER,
    espn_scrum_id TEXT,
    all_rugby_id TEXT,
    google_knowl_id TEXT,
    its_rugby_id TEXT,
    rugby_database_id TEXT,
    wikipedia_en TEXT,
    wikipedia_fr TEXT,
    wikipedia_it TEXT,
    wikipedia_es TEXT,
    wikipedia_ja TEXT,
    dbpedia_id TEXT,
    FOREIGN KEY (birth_place_id) REFERENCES location(rugbyscope_id),
    FOREIGN KEY (death_place_id) REFERENCES location(rugbyscope_id)
);
")




########################################################################
# stint table

dbExecute(con, "
CREATE TABLE IF NOT EXISTS stint (
    rugbyscope_id INTEGER PRIMARY KEY,
    player_id TEXT,
    team_id INTEGER,
    type TEXT,
    start_year INTEGER,
    end_year INTEGER,
    matches_played INTEGER,
    points_scored INTEGER,
    FOREIGN KEY (player_id) REFERENCES player(wikidata_id),
    FOREIGN KEY (team_id) REFERENCES team(rugbyscope_id)
);
")




########################################################################
# junction tables

dbExecute(con, "
CREATE TABLE IF NOT EXISTS team_governing_body (
    team_id INTEGER,
    governing_body_id INTEGER,
    PRIMARY KEY (team_id, governing_body_id),
    FOREIGN KEY (team_id) REFERENCES team(rugbyscope_id),
    FOREIGN KEY (governing_body_id) REFERENCES governing_body(rugbyscope_id)
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS team_competition (
    team_id INTEGER,
    competition_id INTEGER,
    PRIMARY KEY (team_id, competition_id),
    FOREIGN KEY (team_id) REFERENCES team(rugbyscope_id),
    FOREIGN KEY (competition_id) REFERENCES competition(rugbyscope_id)
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS team_venue (
    team_id INTEGER,
    venue_id INTEGER,
    PRIMARY KEY (team_id, venue_id),
    FOREIGN KEY (team_id) REFERENCES team(rugbyscope_id),
    FOREIGN KEY (venue_id) REFERENCES venue(rugbyscope_id)
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS team_location (
    team_id INTEGER,
    location_id INTEGER,
    PRIMARY KEY (team_id, location_id),
    FOREIGN KEY (team_id) REFERENCES team(rugbyscope_id),
    FOREIGN KEY (location_id) REFERENCES location(rugbyscope_id)
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS team_country (
    team_id INTEGER,
    country_id INTEGER,
    PRIMARY KEY (team_id, country_id),
    FOREIGN KEY (team_id) REFERENCES team(rugbyscope_id),
    FOREIGN KEY (country_id) REFERENCES country(rugbyscope_id)
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS player_citizenship (
    player_id TEXT,
    country_id INTEGER,
    PRIMARY KEY (player_id, country_id),
    FOREIGN KEY (player_id) REFERENCES player(wikidata_id),
    FOREIGN KEY (country_id) REFERENCES country(rugbyscope_id)
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS player_sport_country (
    player_id TEXT,
    country_id INTEGER,
    PRIMARY KEY (player_id, country_id),
    FOREIGN KEY (player_id) REFERENCES player(wikidata_id),
    FOREIGN KEY (country_id) REFERENCES country(rugbyscope_id)
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS stint_data_source (
    stint_id INTEGER,
    data_source_id INTEGER,
    PRIMARY KEY (stint_id, data_source_id),
    FOREIGN KEY (stint_id) REFERENCES stint(rugbyscope_id),
    FOREIGN KEY (data_source_id) REFERENCES data_source(rugbyscope_id)
);
")




########################################################################
# multi-valued attributes

dbExecute(con, "
CREATE TABLE IF NOT EXISTS player_firstname (
    player_id TEXT,
    firstname TEXT,
    `order` INTEGER,
    PRIMARY KEY (player_id, `order`),
    FOREIGN KEY (player_id) REFERENCES player(wikidata_id)
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS player_lastname (
    player_id TEXT,
    lastname TEXT,
    `order` INTEGER,
    PRIMARY KEY (player_id, `order`),
    FOREIGN KEY (player_id) REFERENCES player(wikidata_id)
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS player_altname (
    player_id TEXT,
    altname TEXT,
    PRIMARY KEY (player_id, altname),
    FOREIGN KEY (player_id) REFERENCES player(wikidata_id)
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS team_altname (
    team_id INTEGER,
    altname TEXT,
    PRIMARY KEY (team_id, altname),
    FOREIGN KEY (team_id) REFERENCES team(rugbyscope_id)
);
")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS player_position (
    player_id TEXT,
    position TEXT,
    PRIMARY KEY (player_id, position),
    FOREIGN KEY (player_id) REFERENCES player(wikidata_id)
);
")




########################################################################
# helpful indexes

dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_player_name ON player(full_name);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_team_name ON team(full_name);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_stint_player ON stint(player_id);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_stint_team ON stint(team_id);")
dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_location_name ON location(location_name);")




########################################################################
# disconnect

tlog("Database successfully initialized at:", db_file)
dbDisconnect(con)
