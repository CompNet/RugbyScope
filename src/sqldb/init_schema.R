########################################################################
# Initializes the structure of the DB.
#
# 07/2025 Vincent Labatut
#
# setwd("C:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/sqldb/init_schema.R")
########################################################################
library("DBI")
library("RSQLite")

source("src/common/logging.R")




########################################################################
# reset DB parameter
reset <- TRUE

# set database file
db_path <- file.path("data", "rugbyscope.sqlite")




########################################################################
# SQL commands

SCHEMA_SQL <- c(
"CREATE TABLE team (
  rugbyscope_id     INTEGER PRIMARY KEY,
  wikidata_id       TEXT,
  full_name         TEXT,
  type              TEXT,
  inception_date    TEXT,
  termination_date  TEXT,
  venue_id          INTEGER,
  tier              INTEGER,
  all_rugby_id      TEXT,
  google_knowl_id   TEXT,
  wikipedia_en      TEXT,
  wikipedia_fr      TEXT,
  wikipedia_it      TEXT,
  wikipedia_es      TEXT,
  wikipedia_ja      TEXT,
  dbpedia_id        TEXT,
  comments          TEXT
);",

"CREATE TABLE governing_body (
  rugbyscope_id INTEGER PRIMARY KEY,
  name          TEXT
);",

"CREATE TABLE competition (
  rugbyscope_id INTEGER PRIMARY KEY,
  name          TEXT
);",

"CREATE TABLE team_governing_body (
  team_id            INTEGER REFERENCES team(rugbyscope_id),
  governing_body_id  INTEGER REFERENCES governing_body(rugbyscope_id)
);",

"CREATE TABLE team_competition (
  team_id        INTEGER REFERENCES team(rugbyscope_id),
  competition_id INTEGER REFERENCES competition(rugbyscope_id)
);",

"CREATE TABLE venue (
  rugbyscope_id INTEGER PRIMARY KEY,
  name          TEXT,
  capacity      INTEGER
);",

"CREATE TABLE team_venue (
  team_id  INTEGER REFERENCES team(rugbyscope_id),
  venue_id INTEGER REFERENCES venue(rugbyscope_id)
);",

"CREATE TABLE location (
  rugbyscope_id INTEGER PRIMARY KEY,
  location_name TEXT
);",

"CREATE TABLE team_location (
  team_id     INTEGER REFERENCES team(rugbyscope_id),
  location_id INTEGER REFERENCES location(rugbyscope_id)
);",

"CREATE TABLE player (
  rugbyscope_id      INTEGER PRIMARY KEY,
  full_name          TEXT,
  birth_date         TEXT,
  birth_place_id     INTEGER REFERENCES location(rugbyscope_id),
  death_date         TEXT,
  death_place_id     INTEGER REFERENCES location(rugbyscope_id),
  career_start_year  INTEGER,
  career_end_year    INTEGER,
  weight             INTEGER,
  height             INTEGER,
  espn_scrum_id      TEXT,
  all_rugby_id       TEXT,
  google_knowl_id    TEXT,
  its_rugby_id       TEXT,
  rugby_database_id  TEXT,
  wikidata_id        TEXT,
  wikipedia_en       TEXT,
  wikipedia_fr       TEXT,
  wikipedia_it       TEXT,
  wikipedia_es       TEXT,
  wikipedia_ja       TEXT,
  dbpedia_id         TEXT
);",

"CREATE TABLE player_lastname (
  player_id INTEGER REFERENCES player(rugbyscope_id),
  lastname  TEXT,
  rank      INTEGER
);",

"CREATE TABLE player_firstname (
  player_id INTEGER REFERENCES player(rugbyscope_id),
  firstname TEXT,
  rank      INTEGER
);",

"CREATE TABLE player_altname (
  player_id INTEGER REFERENCES player(rugbyscope_id),
  altname   TEXT
);",

"CREATE TABLE team_altname (
  team_id INTEGER REFERENCES team(rugbyscope_id),
  altname TEXT
);",

"CREATE TABLE country (
  rugbyscope_id INTEGER PRIMARY KEY,
  country_name  TEXT
);",

"CREATE TABLE player_citizenship (
  player_id  INTEGER REFERENCES player(rugbyscope_id),
  country_id INTEGER REFERENCES country(rugbyscope_id)
);",

"CREATE TABLE player_sport_country (
  player_id  INTEGER REFERENCES player(rugbyscope_id),
  country_id INTEGER REFERENCES country(rugbyscope_id)
);",

"CREATE TABLE team_country (
  team_id    INTEGER REFERENCES team(rugbyscope_id),
  country_id INTEGER REFERENCES country(rugbyscope_id)
);",

"CREATE TABLE player_position (
  player_id INTEGER REFERENCES player(rugbyscope_id),
  position  TEXT
);",

"CREATE TABLE stint (
  rugbyscope_id  INTEGER PRIMARY KEY,
  player_id      INTEGER REFERENCES player(rugbyscope_id),
  team_id        INTEGER REFERENCES team(rugbyscope_id),
  type           TEXT,
  start_year     INTEGER,
  end_year       INTEGER,
  matches_played INTEGER,
  points_scored  INTEGER
);",

"CREATE TABLE stint_data_source (
  stint_id    INTEGER REFERENCES stint(rugbyscope_id),
  data_source TEXT
);"
)




########################################################################
# run SQL commands

tlog("Initializing schema")

# connect and create database if it does not exist
con <- dbConnect(SQLite(), db_path)

# possibly remove existing tables
if (reset) {
  tlog("Remove existing tables")
  existing <- dbListTables(con)
  for (tbl in existing) 
    dbExecute(con, sprintf('DROP TABLE IF EXISTS "%s";', tbl))
}

# create tables
dbExecute(con, "PRAGMA foreign_keys = OFF;") # off during bulk load, verified at the end
for (stmt in SCHEMA_SQL)
  dbExecute(con, stmt)
tlog("Schema created: ", length(SCHEMA_SQL), " tables")

# disconnect
dbDisconnect(con)
