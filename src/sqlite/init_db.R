## =============================================================================
## sqlite_init_db.R
##
## ETL script that initializes a SQLite database matching the rugby DBML
## schema, and populates it from three source CSV files:
##   - players.csv
##   - teams.csv
##   - stints.csv
##
## Approach: EXTRACT (read CSVs) -> TRANSFORM (clean, split multi-valued
## fields, dedupe lookup dimensions, resolve enum domains, resolve
## surrogate/foreign keys) -> LOAD (create schema, bulk-write tables).
##
## Notes on source data quirks handled here:
##   * Multi-valued fields are ";"-separated in a single CSV cell (e.g.
##     "altNames", "citizenships", "positions", "dataSources", ...). These
##     are exploded into their respective junction tables.
##   * "homeVenueNames" / "homeVenueCapacities" in teams.csv are PARALLEL
##     ";"-separated lists (same order == same venue), so they are zipped
##     together rather than split independently.
##   * "firstNames" / "lastNames" in players.csv are ordered lists -> the
##     order is preserved via the `rank` column in player_firstname /
##     player_lastname.
##   * Enum domains (team.type, stint.type, player_position.position,
##     stint_data_source.data_source) are NOT hard-coded: they are
##     discovered at runtime from the CSVs (pre-scan pass) and turned into
##     SQL CHECK constraints when the schema is created.
##   * players.csv and stints.csv do not carry a rugbyscope_id, so
##     surrogate integer keys are generated for the `player` and `stint`
##     tables during the transform step. teams.csv DOES provide
##     rugbyscopeId, which is used as-is for `team.rugbyscope_id`.
## =============================================================================
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/sqlite/init_db.R")

suppressPackageStartupMessages({
  library("DBI")
  library("RSQLite")
  library("readr")
  library("dplyr")
  library("purrr")
  library("tidyr")
  library("stringr")
})

## -----------------------------------------------------------------------
## 0. CONFIGURATION
## -----------------------------------------------------------------------

INPUT_FOLDER <- file.path("data")
PLAYERS_CSV <- file.path(INPUT_FOLDER, "players.csv")
TEAMS_CSV   <- file.path(INPUT_FOLDER, "teams.csv")
STINTS_CSV  <- file.path(INPUT_FOLDER, "stints.csv")
DB_PATH     <- file.path(INPUT_FOLDER, "rugbyscope.sqlite")

## Start from a clean database every run
if (file.exists(DB_PATH)) file.remove(DB_PATH)

## -----------------------------------------------------------------------
## 1. HELPER FUNCTIONS
## -----------------------------------------------------------------------

# Split a ";"-separated cell into a trimmed character vector, dropping
# blanks/NA. Returns character(0) for empty/NA input.
split_multi <- function(x, sep = ";") {
  if (is.na(x) || !nzchar(trimws(x))) return(character(0))
  parts <- str_split(x, sep)[[1]]
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

# Explode a multi-valued column into a long (id, value) tibble.
expand_multi <- function(ids, values) {
  map2_dfr(ids, values, function(id, val) {
    v <- split_multi(val)
    if (length(v) == 0) return(tibble(id = integer(0), value = character(0)))
    tibble(id = id, value = v)
  })
}

# Same as expand_multi but also keeps the 1-based position of each value
# (used for firstName / lastName ordering).
expand_multi_ranked <- function(ids, values) {
  map2_dfr(ids, values, function(id, val) {
    v <- split_multi(val)
    if (length(v) == 0) {
      return(tibble(id = integer(0), value = character(0), rank = integer(0)))
    }
    tibble(id = id, value = v, rank = seq_along(v))
  })
}

# Zip two PARALLEL multi-valued columns (same ";" order) into a long tibble.
# If the two lists differ in length for a row, pad the shorter with NA.
expand_paired <- function(ids, values_a, values_b) {
  pmap_dfr(list(ids, values_a, values_b), function(id, a, b) {
    va <- split_multi(a)
    vb <- split_multi(b)
    n <- max(length(va), length(vb))
    if (n == 0) return(tibble(id = integer(0), a = character(0), b = character(0)))
    length(va) <- n
    length(vb) <- n
    tibble(id = id, a = va, b = vb)
  })
}

# Build a deduplicated dimension/lookup table with a surrogate integer key
# from a character vector of raw values.
build_lookup <- function(values, id_col, name_col) {
  vals <- sort(unique(values[!is.na(values) & nzchar(trimws(values))]))
  out <- tibble(id = seq_along(vals), name = vals)
  names(out) <- c(id_col, name_col)
  out
}

# Parse a date column that may contain "NA" or malformed values; returns
# character in ISO format (or NA) for SQLite TEXT storage.
clean_date <- function(x) {
  d <- suppressWarnings(as.Date(x))
  as.character(d)
}

# Parse an integer column defensively (handles "NA", blanks, stray text).
clean_int <- function(x) {
  suppressWarnings(as.integer(x))
}

# Discover the runtime enum domain (sorted unique non-NA values) for a
# possibly multi-valued column.
discover_enum <- function(raw_values, multi = FALSE) {
  if (multi) {
    vals <- unlist(map(raw_values, split_multi))
  } else {
    vals <- raw_values[!is.na(raw_values) & nzchar(trimws(raw_values))]
  }
  sort(unique(trimws(vals)))
}

# Turn an enum domain into a SQL CHECK(...) clause fragment, or "" if the
# domain is empty (column left unconstrained in that case).
sql_check_clause <- function(col, domain) {
  if (length(domain) == 0) return("")
  escaped <- gsub("'", "''", domain)
  quoted <- paste0("'", escaped, "'")
  paste0(" CHECK (", col, " IN (", paste(quoted, collapse = ", "), "))")
}

message("Helper functions loaded.")

## -----------------------------------------------------------------------
## 2. EXTRACT: read raw CSVs
## -----------------------------------------------------------------------

message("Reading source CSVs...")

players_raw <- read_csv(PLAYERS_CSV, na = c("NA", ""), col_types = cols(.default = "c"))
teams_raw   <- read_csv(TEAMS_CSV,   na = c("NA", ""), col_types = cols(.default = "c"))
stints_raw  <- read_csv(STINTS_CSV,  na = c("NA", ""), col_types = cols(.default = "c"))

message(sprintf("  players.csv: %d rows", nrow(players_raw)))
message(sprintf("  teams.csv:   %d rows", nrow(teams_raw)))
message(sprintf("  stints.csv:  %d rows", nrow(stints_raw)))

## -----------------------------------------------------------------------
## 3. TRANSFORM (part A): assign surrogate keys, pre-scan enum domains
## -----------------------------------------------------------------------

# Surrogate keys: teams already have rugbyscopeId in the source; players
# and stints do not, so we mint sequential integer ids here.
players_raw <- players_raw %>% mutate(.player_id = row_number())
teams_raw   <- teams_raw   %>% mutate(rugbyscope_id = clean_int(rugbyscopeId))
stints_raw  <- stints_raw  %>% mutate(.stint_id = row_number())

# Enum domains discovered at runtime from the data itself.
team_type_domain        <- discover_enum(teams_raw$type,               multi = FALSE)
stint_type_domain       <- discover_enum(stints_raw$type,              multi = FALSE)
position_domain         <- discover_enum(players_raw$positions,        multi = TRUE)
data_source_domain      <- discover_enum(stints_raw$dataSources,       multi = TRUE)

message("Discovered enum domains:")
message("  team.type: ",              paste(team_type_domain, collapse = ", "))
message("  stint.type: ",             paste(stint_type_domain, collapse = ", "))
message("  player_position.position: ", paste(position_domain, collapse = ", "))
message("  stint_data_source.data_source: ", paste(data_source_domain, collapse = ", "))

## -----------------------------------------------------------------------
## 4. TRANSFORM (part B): build shared dimension/lookup tables
## -----------------------------------------------------------------------

## --- country --------------------------------------------------------------
country_values <- c(
  unlist(map(players_raw$citizenships, split_multi)),
  unlist(map(players_raw$sportCountries, split_multi)),
  unlist(map(teams_raw$countries, split_multi))
)
country_dim <- build_lookup(country_values, "rugbyscope_id", "country_name")

## --- location ---------------------------------------------------------------
location_values <- c(
  players_raw$birthPlace,
  players_raw$deathPlace,
  unlist(map(teams_raw$locations, split_multi))
)
location_dim <- build_lookup(location_values, "rugbyscope_id", "location_name")

## --- governing_body (from teams$affiliations) ------------------------------
gb_values <- unlist(map(teams_raw$affiliations, split_multi))
governing_body_dim <- build_lookup(gb_values, "rugbyscope_id", "name")

## --- competition (from teams$competitions) --------------------------------
competition_values <- unlist(map(teams_raw$competitions, split_multi))
competition_dim <- build_lookup(competition_values, "rugbyscope_id", "name")

## --- venue (from paired teams$homeVenueNames / homeVenueCapacities) -------
venue_pairs <- expand_paired(teams_raw$rugbyscope_id, teams_raw$homeVenueNames, teams_raw$homeVenueCapacities)
venue_pairs <- venue_pairs %>%
  filter(!is.na(a) & nzchar(trimws(a))) %>%
  transmute(team_id = id, name = trimws(a), capacity = clean_int(b))

venue_dim <- venue_pairs %>%
  group_by(name) %>%
  summarise(
    capacity = if (all(is.na(capacity))) NA_integer_ else first(na.omit(capacity)),
    .groups = "drop"
  ) %>%
  arrange(name) %>%
  mutate(rugbyscope_id = row_number()) %>%
  select(rugbyscope_id, name, capacity)

message("Dimension tables built:")
message(sprintf("  country: %d, location: %d, governing_body: %d, competition: %d, venue: %d",
                 nrow(country_dim), nrow(location_dim), nrow(governing_body_dim),
                 nrow(competition_dim), nrow(venue_dim)))

## -----------------------------------------------------------------------
## 5. TRANSFORM (part C): build fact tables + junction tables
## -----------------------------------------------------------------------

## === TEAM ===================================================================
team_tbl <- teams_raw %>%
  transmute(
    rugbyscope_id     = rugbyscope_id,
    wikidata_id       = wikidataId,
    full_name         = fullName,
    type              = type,
    inception_date    = clean_date(inceptionDate),
    termination_date  = clean_date(terminationDate),
    tier              = clean_int(tier),
    all_rugby_id      = allRugbyId,
    google_knowl_id   = googleKnowlId,
    wikipedia_en      = wikipediaEn,
    wikipedia_fr      = wikipediaFr,
    wikipedia_it      = wikipediaIt,
    wikipedia_es      = wikipediaEs,
    wikipedia_ja      = wikipediaJa,
    dbpedia_id        = dbpediaId,
    comments          = comment
  )

# team_venue junction (from venue_pairs) + resolve venue_id
team_venue_tbl <- venue_pairs %>%
  left_join(venue_dim %>% select(venue_id = rugbyscope_id, name), by = "name") %>%
  transmute(team_id, venue_id)

# team.venue_id = first venue listed for that team (denormalised convenience field)
team_first_venue <- team_venue_tbl %>% group_by(team_id) %>% slice(1) %>% ungroup() %>%
  select(rugbyscope_id = team_id, venue_id)
team_tbl <- team_tbl %>% left_join(team_first_venue, by = "rugbyscope_id")

# team_altname
team_altname_tbl <- expand_multi(teams_raw$rugbyscope_id, teams_raw$altNames) %>%
  transmute(team_id = id, altname = value)

# team_country
team_country_tbl <- expand_multi(teams_raw$rugbyscope_id, teams_raw$countries) %>%
  left_join(country_dim, by = c("value" = "country_name")) %>%
  transmute(team_id = id, country_id = rugbyscope_id)

# team_affiliation
team_affiliation_tbl <- expand_multi(teams_raw$rugbyscope_id, teams_raw$affiliations) %>%
  left_join(governing_body_dim, by = c("value" = "name")) %>%
  transmute(team_id = id, governing_body_id = rugbyscope_id)

# team_competition
team_competition_tbl <- expand_multi(teams_raw$rugbyscope_id, teams_raw$competitions) %>%
  left_join(competition_dim, by = c("value" = "name")) %>%
  transmute(team_id = id, competition_id = rugbyscope_id)

# team_location
team_location_tbl <- expand_multi(teams_raw$rugbyscope_id, teams_raw$locations) %>%
  left_join(location_dim, by = c("value" = "location_name")) %>%
  transmute(team_id = id, location_id = rugbyscope_id)

## === PLAYER =================================================================
player_tbl <- players_raw %>%
  left_join(location_dim, by = c("birthPlace" = "location_name")) %>%
  rename(birth_place_id = rugbyscope_id) %>%
  left_join(location_dim, by = c("deathPlace" = "location_name")) %>%
  rename(death_place_id = rugbyscope_id) %>%
  transmute(
    rugbyscope_id      = .player_id,
    full_name          = fullName,
    birth_date         = clean_date(birthDate),
    birth_place_id     = birth_place_id,
    death_date         = clean_date(deathDate),
    death_place_id     = death_place_id,
    career_start_year  = clean_int(careerStartYear),
    career_end_year    = clean_int(careerEndYear),
    weight             = clean_int(weight),
    height             = clean_int(height),
    espn_scrum_id      = espnScrumId,
    all_rugby_id       = allRugbyId,
    google_knowl_id    = googleKnowlId,
    its_rugby_id       = itsRugbyId,
    rugby_database_id  = rugbyDatabaseId,
    wikidata_id        = wikidataId,
    wikipedia_en       = wikipediaEn,
    wikipedia_fr       = wikipediaFr,
    wikipedia_it       = wikipediaIt,
    wikipedia_es       = wikipediaEs,
    wikipedia_ja       = wikipediaJa,
    dbpedia_id         = dbpediaId
  )

# lookup used later to resolve stint.player_id from stints$playerId (wikidata id)
player_wd_lookup <- player_tbl %>% filter(!is.na(wikidata_id)) %>%
  select(wikidata_id, player_id = rugbyscope_id)

# player_firstname / player_lastname (ordered)
player_firstname_tbl <- expand_multi_ranked(players_raw$.player_id, players_raw$firstNames) %>%
  transmute(player_id = id, firstname = value, rank = rank)
player_lastname_tbl <- expand_multi_ranked(players_raw$.player_id, players_raw$lastNames) %>%
  transmute(player_id = id, lastname = value, rank = rank)

# player_altname
player_altname_tbl <- expand_multi(players_raw$.player_id, players_raw$altNames) %>%
  transmute(player_id = id, altname = value)

# player_citizenship / player_sport_country
player_citizenship_tbl <- expand_multi(players_raw$.player_id, players_raw$citizenships) %>%
  left_join(country_dim, by = c("value" = "country_name")) %>%
  transmute(player_id = id, country_id = rugbyscope_id)

player_sport_country_tbl <- expand_multi(players_raw$.player_id, players_raw$sportCountries) %>%
  left_join(country_dim, by = c("value" = "country_name")) %>%
  transmute(player_id = id, country_id = rugbyscope_id)

# player_position
player_position_tbl <- expand_multi(players_raw$.player_id, players_raw$positions) %>%
  transmute(player_id = id, position = value)

## === STINT ===================================================================
# resolve team_id: prefer teamRsId (already a rugbyscope_id); fall back to
# teamWdId via team.wikidata_id
team_wd_lookup <- team_tbl %>% filter(!is.na(wikidata_id)) %>%
  select(wikidata_id, wd_team_id = rugbyscope_id)

stints_resolved <- stints_raw %>%
  mutate(teamRsId = clean_int(teamRsId)) %>%
  left_join(player_wd_lookup, by = c("playerId" = "wikidata_id")) %>%
  left_join(team_wd_lookup,   by = c("teamWdId" = "wikidata_id")) %>%
  mutate(resolved_team_id = coalesce(teamRsId, wd_team_id))

unmatched_players <- stints_resolved %>% filter(is.na(player_id)) %>% nrow()
unmatched_teams    <- stints_resolved %>% filter(is.na(resolved_team_id)) %>% nrow()
if (unmatched_players > 0) warning(sprintf("%d stint rows could not be matched to a player.", unmatched_players))
if (unmatched_teams > 0)    warning(sprintf("%d stint rows could not be matched to a team.", unmatched_teams))

stint_tbl <- stints_resolved %>%
  transmute(
    rugbyscope_id  = .stint_id,
    player_id      = player_id,
    team_id        = resolved_team_id,
    type           = type,
    start_year     = clean_int(startYear),
    end_year       = clean_int(endYear),
    matches_played = clean_int(matchesPlayed),
    points_scored  = clean_int(pointsScored)
  )

stint_data_source_tbl <- expand_multi(stints_raw$.stint_id, stints_raw$dataSources) %>%
  transmute(stint_id = id, data_source = value)

message("Fact / junction tables built.")

## -----------------------------------------------------------------------
## 6. LOAD: create schema (with runtime-discovered enum CHECK constraints)
## -----------------------------------------------------------------------

con <- dbConnect(RSQLite::SQLite(), DB_PATH)
dbExecute(con, "PRAGMA foreign_keys = ON;")

message("Creating schema...")

dbExecute(con, "
CREATE TABLE governing_body (
  rugbyscope_id INTEGER PRIMARY KEY,
  name VARCHAR
);")

dbExecute(con, "
CREATE TABLE competition (
  rugbyscope_id INTEGER PRIMARY KEY,
  name VARCHAR
);")

dbExecute(con, "
CREATE TABLE venue (
  rugbyscope_id INTEGER PRIMARY KEY,
  name VARCHAR,
  capacity INTEGER
);")

dbExecute(con, "
CREATE TABLE location (
  rugbyscope_id INTEGER PRIMARY KEY,
  location_name VARCHAR
);")

dbExecute(con, "
CREATE TABLE country (
  rugbyscope_id INTEGER PRIMARY KEY,
  country_name VARCHAR
);")

dbExecute(con, sprintf("
CREATE TABLE team (
  rugbyscope_id INTEGER PRIMARY KEY,
  wikidata_id VARCHAR,
  full_name VARCHAR,
  type VARCHAR%s,
  inception_date DATE,
  termination_date DATE,
  venue_id INTEGER REFERENCES venue(rugbyscope_id),
  tier INTEGER,
  all_rugby_id VARCHAR,
  google_knowl_id VARCHAR,
  wikipedia_en VARCHAR,
  wikipedia_fr VARCHAR,
  wikipedia_it VARCHAR,
  wikipedia_es VARCHAR,
  wikipedia_ja VARCHAR,
  dbpedia_id VARCHAR,
  comments VARCHAR
);", sql_check_clause("type", team_type_domain)))

dbExecute(con, "
CREATE TABLE team_affiliation (
  team_id INTEGER REFERENCES team(rugbyscope_id),
  governing_body_id INTEGER REFERENCES governing_body(rugbyscope_id)
);")

dbExecute(con, "
CREATE TABLE team_competition (
  team_id INTEGER REFERENCES team(rugbyscope_id),
  competition_id INTEGER REFERENCES competition(rugbyscope_id)
);")

dbExecute(con, "
CREATE TABLE team_venue (
  team_id INTEGER REFERENCES team(rugbyscope_id),
  venue_id INTEGER REFERENCES venue(rugbyscope_id)
);")

dbExecute(con, "
CREATE TABLE team_location (
  team_id INTEGER REFERENCES team(rugbyscope_id),
  location_id INTEGER REFERENCES location(rugbyscope_id)
);")

dbExecute(con, "
CREATE TABLE team_altname (
  team_id INTEGER REFERENCES team(rugbyscope_id),
  altname VARCHAR
);")

dbExecute(con, "
CREATE TABLE team_country (
  team_id INTEGER REFERENCES team(rugbyscope_id),
  country_id INTEGER REFERENCES country(rugbyscope_id)
);")

dbExecute(con, "
CREATE TABLE player (
  rugbyscope_id INTEGER PRIMARY KEY,
  full_name VARCHAR,
  birth_date DATE,
  birth_place_id INTEGER REFERENCES location(rugbyscope_id),
  death_date DATE,
  death_place_id INTEGER REFERENCES location(rugbyscope_id),
  career_start_year INTEGER,
  career_end_year INTEGER,
  weight INTEGER,
  height INTEGER,
  espn_scrum_id VARCHAR,
  all_rugby_id VARCHAR,
  google_knowl_id VARCHAR,
  its_rugby_id VARCHAR,
  rugby_database_id VARCHAR,
  wikidata_id VARCHAR,
  wikipedia_en VARCHAR,
  wikipedia_fr VARCHAR,
  wikipedia_it VARCHAR,
  wikipedia_es VARCHAR,
  wikipedia_ja VARCHAR,
  dbpedia_id VARCHAR
);")

dbExecute(con, "
CREATE TABLE player_lastname (
  player_id INTEGER REFERENCES player(rugbyscope_id),
  lastname VARCHAR,
  rank INTEGER
);")

dbExecute(con, "
CREATE TABLE player_firstname (
  player_id INTEGER REFERENCES player(rugbyscope_id),
  firstname VARCHAR,
  rank INTEGER
);")

dbExecute(con, "
CREATE TABLE player_altname (
  player_id INTEGER REFERENCES player(rugbyscope_id),
  altname VARCHAR
);")

dbExecute(con, "
CREATE TABLE player_citizenship (
  player_id INTEGER REFERENCES player(rugbyscope_id),
  country_id INTEGER REFERENCES country(rugbyscope_id)
);")

dbExecute(con, "
CREATE TABLE player_sport_country (
  player_id INTEGER REFERENCES player(rugbyscope_id),
  country_id INTEGER REFERENCES country(rugbyscope_id)
);")

dbExecute(con, sprintf("
CREATE TABLE player_position (
  player_id INTEGER REFERENCES player(rugbyscope_id),
  position VARCHAR%s
);", sql_check_clause("position", position_domain)))

dbExecute(con, sprintf("
CREATE TABLE stint (
  rugbyscope_id INTEGER PRIMARY KEY,
  player_id INTEGER REFERENCES player(rugbyscope_id),
  team_id INTEGER REFERENCES team(rugbyscope_id),
  type VARCHAR%s,
  start_year INTEGER,
  end_year INTEGER,
  matches_played INTEGER,
  points_scored INTEGER
);", sql_check_clause("type", stint_type_domain)))

dbExecute(con, sprintf("
CREATE TABLE stint_data_source (
  stint_id INTEGER REFERENCES stint(rugbyscope_id),
  data_source VARCHAR%s
);", sql_check_clause("data_source", data_source_domain)))

## Helpful indexes on FK columns
fk_indexes <- list(
  c("team_affiliation", "team_id"), c("team_affiliation", "governing_body_id"),
  c("team_competition", "team_id"),    c("team_competition", "competition_id"),
  c("team_venue", "team_id"),          c("team_venue", "venue_id"),
  c("team_location", "team_id"),       c("team_location", "location_id"),
  c("team_altname", "team_id"),
  c("team_country", "team_id"),        c("team_country", "country_id"),
  c("player_lastname", "player_id"),   c("player_firstname", "player_id"),
  c("player_altname", "player_id"),
  c("player_citizenship", "player_id"),   c("player_citizenship", "country_id"),
  c("player_sport_country", "player_id"), c("player_sport_country", "country_id"),
  c("player_position", "player_id"),
  c("stint", "player_id"), c("stint", "team_id"),
  c("stint_data_source", "stint_id")
)
walk(fk_indexes, function(pair) {
  idx_name <- paste0("idx_", pair[1], "_", pair[2])
  dbExecute(con, sprintf("CREATE INDEX %s ON %s(%s);", idx_name, pair[1], pair[2]))
})

message("Schema created.")

## -----------------------------------------------------------------------
## 7. LOAD: write data (dimensions first, then facts, then junctions)
## -----------------------------------------------------------------------

message("Loading data...")

dbWriteTable(con, "governing_body", governing_body_dim, append = TRUE, row.names = FALSE)
dbWriteTable(con, "competition",    competition_dim,    append = TRUE, row.names = FALSE)
dbWriteTable(con, "venue",          venue_dim,           append = TRUE, row.names = FALSE)
dbWriteTable(con, "location",       location_dim,        append = TRUE, row.names = FALSE)
dbWriteTable(con, "country",        country_dim,         append = TRUE, row.names = FALSE)

dbWriteTable(con, "team", team_tbl, append = TRUE, row.names = FALSE)
dbWriteTable(con, "team_affiliation", team_affiliation_tbl, append = TRUE, row.names = FALSE)
dbWriteTable(con, "team_competition", team_competition_tbl, append = TRUE, row.names = FALSE)
dbWriteTable(con, "team_venue", team_venue_tbl, append = TRUE, row.names = FALSE)
dbWriteTable(con, "team_location", team_location_tbl, append = TRUE, row.names = FALSE)
dbWriteTable(con, "team_altname", team_altname_tbl, append = TRUE, row.names = FALSE)
dbWriteTable(con, "team_country", team_country_tbl, append = TRUE, row.names = FALSE)

dbWriteTable(con, "player", player_tbl, append = TRUE, row.names = FALSE)
dbWriteTable(con, "player_firstname", player_firstname_tbl, append = TRUE, row.names = FALSE)
dbWriteTable(con, "player_lastname", player_lastname_tbl, append = TRUE, row.names = FALSE)
dbWriteTable(con, "player_altname", player_altname_tbl, append = TRUE, row.names = FALSE)
dbWriteTable(con, "player_citizenship", player_citizenship_tbl, append = TRUE, row.names = FALSE)
dbWriteTable(con, "player_sport_country", player_sport_country_tbl, append = TRUE, row.names = FALSE)
dbWriteTable(con, "player_position", player_position_tbl, append = TRUE, row.names = FALSE)

dbWriteTable(con, "stint", stint_tbl, append = TRUE, row.names = FALSE)
dbWriteTable(con, "stint_data_source", stint_data_source_tbl, append = TRUE, row.names = FALSE)

message("Data loaded.")

## -----------------------------------------------------------------------
## 8. SUMMARY
## -----------------------------------------------------------------------

all_tables <- dbListTables(con)
counts <- map_dfr(all_tables, function(t) {
  n <- dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s;", t))$n
  tibble(table = t, rows = n)
}) %>% arrange(table)

print(counts)

dbDisconnect(con)
message(sprintf("Done. Database written to: %s", normalizePath(DB_PATH)))
