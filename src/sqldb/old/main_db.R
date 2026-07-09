########################################################################
# Create the structure of the SQL DB.
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/sqldb/xxxxx.R")
########################################################################
library("DBI")
library("RSQLite")
library("dplyr")
#library("readr")
library("purrr")
library("tidyr")

source("src/common/logging.R")
source("src/sqldb/common_functions.R")




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
# create shared dimension tables: location, country, governing_body, competition, venue
tlog("Building shared dimension tables")

# table location
team_locations  <- unlist(split_multi_vec(teams$locations))
birth_places    <- unlist(split_multi_vec(players$birthPlaces))
death_places    <- unlist(split_multi_vec(players$deathPlaces))
location_dim <- build_dimension(team_locations, birth_places, death_places, value_name = "location_name")
tlog("Dimension built: ", nrow(location_dim), " locations")

# table country
team_countries  <- unlist(split_multi_vec(teams$countries))
citizenships    <- unlist(split_multi_vec(players$citizenships))
sport_countries <- unlist(split_multi_vec(players$sportCountries))
country_dim <- build_dimension(team_countries, citizenships, sport_countries, value_name = "country_name")
tlog("Dimension built: ", nrow(country_dim), " countries")

# table governing_body
affiliations <- unlist(split_multi_vec(teams$affiliations))
governing_body_dim <- build_dimension(affiliations, value_name = "name")
tlog("Dimension built: ", nrow(governing_body_dim), " governing bodies")

# table competition
competitions <- unlist(split_multi_vec(teams$competitions))
competition_dim <- build_dimension(competitions, value_name = "name")
tlog("Dimension built: ", nrow(competition_dim), " competition")

# table venue
venue_pairs <- map2(
  split_multi_vec(teams$homeVenueNames),
  split_multi_vec(teams$homeVenueCapacities),
  function(names, caps) {
    if (length(names) == 0) return(tibble(name = character(0), capacity = integer(0)))
    # pad capacities if fewer than names were supplied
    length(caps) <- length(names)
    tibble(name = names, capacity = to_int(caps))
  }
) %>% bind_rows()

venue_dim <- venue_pairs %>%
  filter(!is.na(name), nzchar(str_trim(name))) %>%
  mutate(name = str_trim(name)) %>%
  group_by(name) %>%
  summarise(capacity = first(na.omit(capacity)), .groups = "drop") %>%
  arrange(name) %>%
  mutate(id = row_number()) %>%
  select(id, name, capacity)
tlog("Dimension built: ", nrow(venue_dim), " venues")











# -----------------------------------------------------------------------------
# 5. TRANSFORM — team + team bridge tables
# -----------------------------------------------------------------------------

transform_teams <- function(teams, dims) {
  log_step("Transforming team table...")

  team_tbl <- teams %>%
    transmute(
      rugbyscope_id    = to_int(rugbyscopeId),
      wikidata_id      = wikidataId,
      full_name        = fullName,
      type             = type,
      inception_date   = map_chr(inceptionDate, parse_date_safe),
      termination_date = map_chr(terminationDate, parse_date_safe),
      venue_id         = NA_integer_,  # authoritative link lives in team_venue bridge
      tier             = to_int(tier),
      all_rugby_id     = allRugbyId,
      google_knowl_id  = googleKnowlId,
      wikipedia_en     = wikipediaEn,
      wikipedia_fr     = wikipediaFr,
      wikipedia_it     = wikipediaIt,
      wikipedia_es     = wikipediaEs,
      wikipedia_ja     = wikipediaJa,
      dbpedia_id       = dbpediaId,
      comments         = comments
    )

  teams_keyed <- teams %>% mutate(.team_id = to_int(rugbyscopeId))

  team_altname <- explode_column(teams_keyed, ".team_id", "altNames") %>%
    transmute(team_id = .team_id, altname = value)

  team_governing_body <- explode_column(teams_keyed, ".team_id", "affiliations") %>%
    left_join(dims$governing_body, by = c("value" = "name")) %>%
    transmute(team_id = .team_id, governing_body_id = id)

  team_competition <- explode_column(teams_keyed, ".team_id", "competitions") %>%
    left_join(dims$competition, by = c("value" = "name")) %>%
    transmute(team_id = .team_id, competition_id = id)

  team_country <- explode_column(teams_keyed, ".team_id", "countries") %>%
    left_join(dims$country, by = c("value" = "country_name")) %>%
    transmute(team_id = .team_id, country_id = id)

  team_location <- explode_column(teams_keyed, ".team_id", "locations") %>%
    left_join(dims$location, by = c("value" = "location_name")) %>%
    transmute(team_id = .team_id, location_id = id)

  team_venue <- explode_column(teams_keyed, ".team_id", "homeVenueNames") %>%
    mutate(value = str_trim(value)) %>%
    left_join(dims$venue, by = c("value" = "name")) %>%
    transmute(team_id = .team_id, venue_id = id)

  log_step(sprintf("Transformed %d teams.", nrow(team_tbl)))

  list(
    team                = team_tbl,
    team_altname        = team_altname,
    team_governing_body = team_governing_body,
    team_competition    = team_competition,
    team_country        = team_country,
    team_location       = team_location,
    team_venue          = team_venue
  )
}

# -----------------------------------------------------------------------------
# 6. TRANSFORM — player + player bridge tables
# -----------------------------------------------------------------------------

transform_players <- function(players, dims) {
  log_step("Transforming player table...")

  # Surrogate keys, since Players CSV has no rugbyscope_id of its own.
  players <- players %>% mutate(.player_id = row_number())

  # birth/death place: take the first listed value as *the* place.
  first_or_na <- function(x) {
    parts <- split_multi(x)
    if (length(parts) == 0) return(NA_character_)
    parts[1]
  }

  player_tbl <- players %>%
    rowwise() %>%
    mutate(
      .birth_place_name = first_or_na(birthPlaces),
      .death_place_name = first_or_na(deathPlaces)
    ) %>%
    ungroup() %>%
    left_join(dims$location, by = c(".birth_place_name" = "location_name")) %>%
    rename(birth_place_id = id) %>%
    left_join(dims$location, by = c(".death_place_name" = "location_name")) %>%
    rename(death_place_id = id) %>%
    transmute(
      rugbyscope_id     = .player_id,
      full_name         = fullName,
      birth_date        = map_chr(birthDate, parse_date_safe),
      birth_place_id    = birth_place_id,
      death_date        = map_chr(deathDate, parse_date_safe),
      death_place_id    = death_place_id,
      career_start_year = to_int(careerStartYear),
      career_end_year   = to_int(careerEndYear),
      weight            = to_int(weight),
      height            = to_int(height),
      espn_scrum_id     = espnScrumId,
      all_rugby_id      = allRugbyId,
      google_knowl_id   = googleKnowlId,
      its_rugby_id      = itsRugbyId,
      rugby_database_id = rugbyDatabaseId,
      wikidata_id       = wikidataId,
      wikipedia_en      = wikipediaEn,
      wikipedia_fr      = wikipediaFr,
      wikipedia_it      = wikipediaIt,
      wikipedia_es      = wikipediaEs,
      wikipedia_ja      = wikipediaJa,
      dbpedia_id        = dbpediaId
    )

  player_firstname <- explode_column(players, ".player_id", "firstNames") %>%
    transmute(player_id = .player_id, firstname = value, rank = rank)

  player_lastname <- explode_column(players, ".player_id", "lastNames") %>%
    transmute(player_id = .player_id, lastname = value, rank = rank)

  player_altname <- explode_column(players, ".player_id", "altNames") %>%
    transmute(player_id = .player_id, altname = value)

  player_citizenship <- explode_column(players, ".player_id", "citizenships") %>%
    left_join(dims$country, by = c("value" = "country_name")) %>%
    transmute(player_id = .player_id, country_id = id)

  player_sport_country <- explode_column(players, ".player_id", "sportCountries") %>%
    left_join(dims$country, by = c("value" = "country_name")) %>%
    transmute(player_id = .player_id, country_id = id)

  player_position <- explode_column(players, ".player_id", "positions") %>%
    transmute(player_id = .player_id, position = value)

  # Keep the wikidataId -> rugbyscope_id map for resolving stint.player_id later.
  player_id_lookup <- players %>% transmute(wikidata_id = wikidataId, player_id = .player_id)

  log_step(sprintf("Transformed %d players.", nrow(player_tbl)))

  list(
    player                = player_tbl,
    player_firstname      = player_firstname,
    player_lastname       = player_lastname,
    player_altname        = player_altname,
    player_citizenship    = player_citizenship,
    player_sport_country  = player_sport_country,
    player_position       = player_position,
    player_id_lookup      = player_id_lookup
  )
}

# -----------------------------------------------------------------------------
# 7. TRANSFORM — stint + stint_data_source
# -----------------------------------------------------------------------------

transform_stints <- function(stints, player_id_lookup, team_ids) {
  log_step("Transforming stint table...")

  stints <- stints %>%
    mutate(.stint_id = row_number()) %>%
    left_join(player_id_lookup, by = c("playerId" = "wikidata_id"))

  n_unmatched_players <- sum(is.na(stints$player_id))
  if (n_unmatched_players > 0) {
    warning(sprintf(
      "%d stint(s) reference a playerId (wikidataId) not found in the players CSV.",
      n_unmatched_players
    ))
  }

  stint_tbl <- stints %>%
    mutate(.team_id_int = to_int(teamRsId)) %>%
    transmute(
      rugbyscope_id  = .stint_id,
      player_id      = player_id,
      team_id        = ifelse(.team_id_int %in% team_ids, .team_id_int, NA_integer_),
      type           = type,
      start_year     = to_int(startYear),
      end_year       = to_int(endYear),
      matches_played = to_int(matchesPlayed),
      points_scored  = to_int(pointsScored)
    )

  n_unmatched_teams <- sum(is.na(stint_tbl$team_id) & !is.na(to_int(stints$teamRsId)))
  if (n_unmatched_teams > 0) {
    warning(sprintf(
      "%d stint(s) reference a teamRsId not found in the teams CSV.",
      n_unmatched_teams
    ))
  }

  stint_data_source <- explode_column(stints, ".stint_id", "dataSources") %>%
    transmute(stint_id = .stint_id, data_source = value)

  log_step(sprintf("Transformed %d stints.", nrow(stint_tbl)))

  list(stint = stint_tbl, stint_data_source = stint_data_source)
}

# -----------------------------------------------------------------------------
# 8. LOAD
# -----------------------------------------------------------------------------

load_table <- function(con, name, df) {
  # dbWriteTable(append = TRUE) inserts rows into the already-created table
  # without altering the schema defined in SCHEMA_SQL.
  dbWriteTable(con, name, as.data.frame(df), append = TRUE, row.names = FALSE)
  log_step(sprintf("Loaded %-22s %6d rows", name, nrow(df)))
}

load_all <- function(con, dims, teams_out, players_out, stints_out) {
  log_step("Loading tables into the database...")
  dbBegin(con)
  tryCatch({
    load_table(con, "location",       dims$location %>% rename(rugbyscope_id = id))
    load_table(con, "country",        dims$country %>% rename(rugbyscope_id = id))
    load_table(con, "governing_body", dims$governing_body %>% rename(rugbyscope_id = id))
    load_table(con, "competition",    dims$competition %>% rename(rugbyscope_id = id))
    load_table(con, "venue",          dims$venue %>% rename(rugbyscope_id = id))

    load_table(con, "team",                teams_out$team)
    load_table(con, "team_altname",        teams_out$team_altname)
    load_table(con, "team_governing_body", teams_out$team_governing_body)
    load_table(con, "team_competition",    teams_out$team_competition)
    load_table(con, "team_country",        teams_out$team_country)
    load_table(con, "team_location",       teams_out$team_location)
    load_table(con, "team_venue",          teams_out$team_venue)

    load_table(con, "player",               players_out$player)
    load_table(con, "player_firstname",     players_out$player_firstname)
    load_table(con, "player_lastname",      players_out$player_lastname)
    load_table(con, "player_altname",       players_out$player_altname)
    load_table(con, "player_citizenship",   players_out$player_citizenship)
    load_table(con, "player_sport_country", players_out$player_sport_country)
    load_table(con, "player_position",      players_out$player_position)

    load_table(con, "stint",             stints_out$stint)
    load_table(con, "stint_data_source", stints_out$stint_data_source)

    dbCommit(con)
    log_step("All tables loaded and committed.")
  }, error = function(e) {
    dbRollback(con)
    stop("Load failed, transaction rolled back: ", conditionMessage(e))
  })
}

# -----------------------------------------------------------------------------
# 9. VALIDATION
# -----------------------------------------------------------------------------

validate_foreign_keys <- function(con) {
  log_step("Validating foreign key integrity...")
  dbExecute(con, "PRAGMA foreign_keys = ON;")
  problems <- dbGetQuery(con, "PRAGMA foreign_key_check;")
  if (nrow(problems) == 0) {
    log_step("No foreign key violations found.")
  } else {
    warning(sprintf("%d foreign key violation(s) found — see returned data frame.", nrow(problems)))
    print(problems)
  }
  invisible(problems)
}

# -----------------------------------------------------------------------------
# 10. MAIN
# -----------------------------------------------------------------------------

#  raw <- extract_csvs()

  dims <- build_shared_dimensions(raw$players, raw$teams)

  teams_out   <- transform_teams(raw$teams, dims)
  players_out <- transform_players(raw$players, dims)
  stints_out  <- transform_stints(
    raw$stints,
    players_out$player_id_lookup,
    team_ids = teams_out$team$rugbyscope_id
  )

#  init_schema(con)
  load_all(con, dims, teams_out, players_out, stints_out)
  validate_foreign_keys(con)

  log_step(sprintf("Done. Database written to: %s", normalizePath(CONFIG$db_path)))
