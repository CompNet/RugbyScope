## =============================================================================
## sqlite_recover_csv.R
##
## Reverse-ETL: reconstructs players.csv, teams.csv and stints.csv from the
## SQLite database created by init_rugby_db.R.
##
## Approach: EXTRACT (read normalised tables) -> TRANSFORM (re-collapse
## junction tables into ";"-joined multi-valued cells, in their original
## order; resolve surrogate keys back to natural/business keys) -> LOAD
## (write CSVs whose quoting style matches the original source files).
##
## Ordering guarantees:
##   * player_firstname / player_lastname store an explicit `rank` column,
##     which is used to rebuild "firstNames" / "lastNames" in the exact
##     original order.
##   * All other multi-valued junction tables (altnames, citizenships,
##     positions, affiliations, nations, competitions, locations,
##     data sources, venues) have no explicit rank, so insertion order is
##     recovered via SQLite's implicit `rowid`, which init_rugby_db.R filled
##     in the same order as the original CSV rows.
##   * Rows in the three output files are ordered by each table's
##     rugbyscope_id, which was itself assigned in original-row order by
##     init_rugby_db.R (row_number() for player/stint; the CSV-provided
##     rugbyscopeId for team).
##
## Known limitations (information that init_rugby_db.R did not retain, and
## therefore cannot be perfectly restored):
##   * stints.csv originally had teamWdId and/or teamRsId populated
##     depending on what the source row happened to specify; the database
##     only stores a single resolved team_id. On export BOTH teamWdId and
##     teamRsId are filled in whenever the matched team has that identifier
##     available, even if only one of the two was present in the original
##     row.
##   * Empty-string values (e.g. "" in teams.csv$comment) were loaded as NA
##     by init_rugby_db.R (na = c("NA", "")), so that distinction from a
##     genuine literal "NA" cannot be recovered; both are exported as NA.
##   * Row order for teams.csv is NOT guaranteed to match the original file:
##     team.rugbyscope_id came directly from the source "rugbyscopeId"
##     column and is an INTEGER PRIMARY KEY, which SQLite treats as an
##     alias for rowid -- so no separate "original file position" is
##     stored for teams. Exporting `ORDER BY rugbyscope_id` therefore
##     reproduces teams in ascending id order, which only matches the
##     original row order if the source file happened to be id-sorted.
##     (players.csv and stints.csv don't have this issue: their surrogate
##     rugbyscope_id was assigned via row_number() in original file order,
##     so ordering by rugbyscope_id does reproduce the original order.)
##     If exact team row order matters, add a `load_order INTEGER` column
##     to the `team` table in init_rugby_db.R (populated from row_number()
##     alongside rugbyscope_id) and ORDER BY that instead below.
## =============================================================================
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/sqlite/recover_csv.R")

suppressPackageStartupMessages({
  library("DBI")
  library("RSQLite")
  library("dplyr")
  library("tidyr")
})

## -----------------------------------------------------------------------
## 0. CONFIGURATION
## -----------------------------------------------------------------------

OUTPUT_FOLDER <- file.path("data")
PLAYERS_CSV <- file.path(OUTPUT_FOLDER, "reconstructed_players.csv")
TEAMS_CSV   <- file.path(OUTPUT_FOLDER, "reconstructed_teams.csv")
STINTS_CSV  <- file.path(OUTPUT_FOLDER, "reconstructed_stints.csv")
DB_PATH     <- file.path(OUTPUT_FOLDER, "rugbyscope.sqlite")

con <- dbConnect(RSQLite::SQLite(), DB_PATH)

## -----------------------------------------------------------------------
## 1. HELPER FUNCTIONS
## -----------------------------------------------------------------------

# Read a junction table together with SQLite's implicit rowid, which
# reflects insertion (== original CSV) order.
read_ordered <- function(table) {
  dbGetQuery(con, sprintf("SELECT rowid AS rowid_, * FROM %s;", table)) %>% as_tibble()
}

# Collapse a long (id, value) table into one ";"-joined string per id,
# in rowid_ order. Returns a two-column tibble: id, value.
collapse_multi <- function(df, id_col, value_col) {
  df %>%
    arrange(.data[[id_col]], rowid_) %>%
    group_by(id = .data[[id_col]]) %>%
    summarise(value = paste(.data[[value_col]], collapse = "; "), .groups = "drop")
}

# Same idea but ordered by an explicit rank column instead of rowid_
# (used for firstNames / lastNames).
collapse_ranked <- function(df, id_col, value_col, rank_col) {
  df %>%
    arrange(.data[[id_col]], .data[[rank_col]]) %>%
    group_by(id = .data[[id_col]]) %>%
    summarise(value = paste(.data[[value_col]], collapse = "; "), .groups = "drop")
}

# Write a CSV whose quoting style mirrors the original source files: only
# character/factor columns get quoted (Date and numeric columns don't),
# and NA is written as the literal unquoted token NA. This is exactly
# base R's write.csv() default behaviour, so we rely on it directly and
# just make sure every column has the right R class beforehand.
write_source_style_csv <- function(df, path) {
  utils::write.csv(df, file = path, row.names = FALSE, na = "NA")
}

message("Connected to ", DB_PATH, ". Reading normalised tables...")

## =========================================================================
## 2. TEAMS
## =========================================================================

team           <- dbReadTable(con, "team") %>% as_tibble()
governing_body <- dbReadTable(con, "governing_body") %>% as_tibble()
competition    <- dbReadTable(con, "competition") %>% as_tibble()
venue          <- dbReadTable(con, "venue") %>% as_tibble()
location       <- dbReadTable(con, "location") %>% as_tibble()
nation         <- dbReadTable(con, "nation") %>% as_tibble()

team_altname_c <- collapse_multi(read_ordered("team_altname"), "team_id", "altname")

team_affiliation_c <- read_ordered("team_affiliation") %>%
  left_join(governing_body, by = c("governing_body_id" = "rugbyscope_id")) %>%
  collapse_multi("team_id", "name")

team_nation_c <- read_ordered("team_nation") %>%
  left_join(nation, by = c("nation_id" = "rugbyscope_id")) %>%
  collapse_multi("team_id", "nation_name")

team_competition_c <- read_ordered("team_competition") %>%
  left_join(competition, by = c("competition_id" = "rugbyscope_id")) %>%
  collapse_multi("team_id", "name")

team_location_c <- read_ordered("team_location") %>%
  left_join(location, by = c("location_id" = "rugbyscope_id")) %>%
  collapse_multi("team_id", "location_name")

# homeVenueNames / homeVenueCapacities are PARALLEL lists -> rebuild both
# together, in the same (team_id, rowid_) order, so positions still match.
team_venue_c <- read_ordered("team_venue") %>%
  left_join(venue, by = c("venue_id" = "rugbyscope_id")) %>%
  arrange(team_id, rowid_) %>%
  group_by(id = team_id) %>%
  summarise(
    homeVenueNames = paste(name, collapse = "; "),
    homeVenueCapacities = if (all(is.na(capacity))) {
      NA_character_
    } else {
      paste(ifelse(is.na(capacity), "NA", as.character(as.integer(capacity))), collapse = "; ")
    },
    .groups = "drop"
  )

teams_out <- team %>%
  left_join(team_altname_c      %>% rename(altNames = value),      by = c("rugbyscope_id" = "id")) %>%
  left_join(team_affiliation_c  %>% rename(affiliations = value),  by = c("rugbyscope_id" = "id")) %>%
  left_join(team_nation_c       %>% rename(nations = value),       by = c("rugbyscope_id" = "id")) %>%
  left_join(team_competition_c  %>% rename(competitions = value),  by = c("rugbyscope_id" = "id")) %>%
  left_join(team_location_c     %>% rename(locations = value),     by = c("rugbyscope_id" = "id")) %>%
  left_join(team_venue_c,                                          by = c("rugbyscope_id" = "id")) %>%
  arrange(rugbyscope_id) %>%
  transmute(
    rugbyscopeId        = as.integer(rugbyscope_id),
    wikidataId           = wikidata_id,
    fullName             = full_name,
    type                 = type,
    inceptionDate        = as.Date(inception_date),
    terminationDate      = as.Date(termination_date),
    altNames, affiliations, nations, competitions,
    tier                 = as.integer(tier),
    homeVenueNames, homeVenueCapacities, locations,
    allRugbyId           = all_rugby_id,
    googleKnowlId        = google_knowl_id,
    wikipediaEn          = wikipedia_en,
    wikipediaFr          = wikipedia_fr,
    wikipediaIt          = wikipedia_it,
    wikipediaEs          = wikipedia_es,
    wikipediaJa          = wikipedia_ja,
    dbpediaId            = dbpedia_id,
    comment              = comments
  )

message(sprintf("Reconstructed teams: %d rows.", nrow(teams_out)))

## =========================================================================
## 3. PLAYERS
## =========================================================================

player <- dbReadTable(con, "player") %>% as_tibble()

player_firstname_c <- collapse_ranked(dbReadTable(con, "player_firstname") %>% as_tibble(),
                                       "player_id", "firstname", "rank")
player_lastname_c  <- collapse_ranked(dbReadTable(con, "player_lastname") %>% as_tibble(),
                                       "player_id", "lastname", "rank")
player_altname_c   <- collapse_multi(read_ordered("player_altname"), "player_id", "altname")

player_citizenship_c <- read_ordered("player_citizenship") %>%
  left_join(nation, by = c("nation_id" = "rugbyscope_id")) %>%
  collapse_multi("player_id", "nation_name")

player_sport_nation_c <- read_ordered("player_sport_nation") %>%
  left_join(nation, by = c("nation_id" = "rugbyscope_id")) %>%
  collapse_multi("player_id", "nation_name")

player_position_c <- collapse_multi(read_ordered("player_position"), "player_id", "position")

players_out <- player %>%
  left_join(player_firstname_c      %>% rename(firstNames = value),                                         by = c("rugbyscope_id" = "id")) %>%
  left_join(player_lastname_c       %>% rename(lastNames = value),                                          by = c("rugbyscope_id" = "id")) %>%
  left_join(player_altname_c        %>% rename(altNames = value),                                           by = c("rugbyscope_id" = "id")) %>%
  left_join(location                %>% select(birth_place_id = rugbyscope_id, birthPlace = location_name), by = "birth_place_id") %>%
  left_join(location                %>% select(death_place_id = rugbyscope_id, deathPlace = location_name), by = "death_place_id") %>%
  left_join(player_citizenship_c    %>% rename(citizenships = value),                                       by = c("rugbyscope_id" = "id")) %>%
  left_join(player_sport_nation_c   %>% rename(sportNations = value),                                       by = c("rugbyscope_id" = "id")) %>%
  left_join(player_position_c       %>% rename(positions = value),                                          by = c("rugbyscope_id" = "id")) %>%
  arrange(rugbyscope_id) %>%
  transmute(
    wikidataId          = wikidata_id,
    fullName            = full_name,
    firstNames, lastNames, altNames,
    birthDate           = as.Date(birth_date),
    birthPlace,
    deathDate           = as.Date(death_date),
    deathPlace,
    citizenships, sportNations, positions,
    careerStartYear     = as.integer(career_start_year),
    careerEndYear       = as.integer(career_end_year),
    weight              = as.integer(weight),
    height              = as.integer(height),
    espnScrumId         = espn_scrum_id,
    allRugbyId          = all_rugby_id,
    googleKnowlId       = google_knowl_id,
    itsRugbyId          = its_rugby_id,
    rugbyDatabaseId     = rugby_database_id,
    wikipediaEn         = wikipedia_en,
    wikipediaFr         = wikipedia_fr,
    wikipediaIt         = wikipedia_it,
    wikipediaEs         = wikipedia_es,
    wikipediaJa         = wikipedia_ja,
    dbpediaId           = dbpedia_id
  )

message(sprintf("Reconstructed players: %d rows.", nrow(players_out)))

## =========================================================================
## 4. STINTS
## =========================================================================

stint <- dbReadTable(con, "stint") %>% as_tibble()

player_lookup <- player %>% transmute(player_id = rugbyscope_id, playerId = wikidata_id, playerName = full_name)
team_lookup   <- team   %>% transmute(team_id   = rugbyscope_id, teamWdId = wikidata_id, teamRsId = rugbyscope_id, teamName = full_name)

stint_data_source_c <- collapse_multi(read_ordered("stint_data_source"), "stint_id", "data_source")

stints_out <- stint %>%
  left_join(player_lookup, by = "player_id") %>%
  left_join(team_lookup,   by = "team_id") %>%
  left_join(stint_data_source_c %>% rename(dataSources = value), by = c("rugbyscope_id" = "id")) %>%
  arrange(rugbyscope_id) %>%
  transmute(
    playerId, playerName, type,
    teamWdId,
    teamRsId       = as.integer(teamRsId),
    teamName,
    startYear      = as.integer(start_year),
    endYear        = as.integer(end_year),
    matchesPlayed  = as.integer(matches_played),
    pointsScored   = as.integer(points_scored),
    dataSources
  )

message(sprintf("Reconstructed stints: %d rows.", nrow(stints_out)))

## -----------------------------------------------------------------------
## 5. LOAD: write CSVs (source-style quoting)
## -----------------------------------------------------------------------

write_source_style_csv(teams_out,   TEAMS_CSV)
write_source_style_csv(players_out, PLAYERS_CSV)
write_source_style_csv(stints_out,  STINTS_CSV)

dbDisconnect(con)

message("Done. Files written:")
message("  ", normalizePath(TEAMS_CSV))
message("  ", normalizePath(PLAYERS_CSV))
message("  ", normalizePath(STINTS_CSV))
