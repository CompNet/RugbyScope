########################################################################
# Main script for the raw data retrieval process. It invokes the
# Wikidata, DBpedia and Wikipedia scripts to form the tables used to
# extract various types of networks. The corresponding CSV files are
# stored in folder`data`. See the individual scripts for details.
#
# Important: these scripts extract data from *live* repositories such as
# DBpedia, Wikidata, and Wikipedia. They all function correctly at the
# time of writing this comment, but it is likely that the evolution of
# these datasource will break compatibility, and that they do not work
# anymore at the time of reading these lines.
#
# Vincent Labatut
# 01/2025
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/build_database.R")
########################################################################





########################################################################
# retrieve the Wikidata tables: this takes at least several hours
source("src/wikidata/retrieve_data.R")
# this produces the following CSV files in folder `data/wikidata/`:
# - `players.csv`: list of players
# - `teams.csv`: list of teams
# - `stints.csv`: list of stints

# compute a few stats for these tables
source("src/wikidata/compute_stats.R")
# produced files located in `data/wikidata/stats`




########################################################################
# retrieve the DBpedia tables
source("src/dbpedia/retrieve_data.R")
# this produces the following CSV files in folder `data/dbpedia/`:
# - `players.csv`: list of players
# - `teams.csv`: list of teams
# no stint table, as these data are not available on DBpedia

# merge DBpedia data (player and teams) into previously retrieved Wikidata tables
source("src/dbpedia/integrate_data.R")
# this produces two files in folder `data/fusion/`:
# - `players_01_wd-dbp.csv`: merged list of players
# - `teams_01_wd-dbp.csv`: merged list of players




########################################################################
# merge the manually curated reference teams into the previously merged team table
source("src/wikidata/integrate_reference.R")
# this produces one file in folder `data/fusion/`:
# - `teams_02_ref.csv`: merged list of teams




########################################################################
# retrieve the raw data from Wikipedia by looping over the players listed in the above table

# japanese Wikipedia
system("python src/wikipedia/japanese/retrieve_stints.py")
# this produces the raw CSV files in folder `data/wikipedia/japanese/raw/`
source("src/wikipedia/japanese/clean_tables.R")
# this produces the clean CSV files in folder `data/wikipedia/japanese/`

# french Wikipedia
system("python src/wikipedia/french/retrieve_stints.py")
# this produces the raw CSV files in folder `data/wikipedia/french/raw/`
source("src/wikipedia/french/clean_tables.R")
# this produces the clean CSV files in folder `data/wikipedia/french/`

# italian Wikipedia
system("python src/wikipedia/italian/retrieve_stints.py")
# this produces the raw CSV files in folder `data/wikipedia/italian/raw/`
source("src/wikipedia/italian/clean_tables.R")
# this produces the clean CSV files in folder `data/wikipedia/italian/`

# spanish Wikipedia
system("python src/wikipedia/spanish/retrieve_stints.py")
# this produces the raw CSV files in folder `data/wikipedia/spanish/raw/`
source("src/wikipedia/spanish/clean_tables.R")
# this produces the clean CSV files in folder `data/wikipedia/spanish/`

# english Wikipedia
# this produces the raw CSV files in folder `data/wikipedia/english/raw/`
system("python src/wikipedia/english/retrieve_stints.py")
# this complements the data retrieved by the previous script, in the same folder
source("src/wikipedia/english/clean_tables.R")
# this produces the clean CSV files in folder `data/wikipedia/english/`




########################################################################
# merge the Wikipedia data with the previous tables

# japanese Wikipedia
#source("src/wikipedia/japanese/compare_merged.R")
# optional: generates a few stats
source("src/wikipedia/japanese/integrate_data.R")
# this produces the following files in folder `data/fusion/`:
# - `players_02_jawp.csv`: merged list of players
# - `teams_03_jawp.csv`: merged list of teams
# - `stints_01_wd-jawp.csv`: merged list of stints

# french Wikipedia
source("src/wikipedia/french/integrate_data.R")
# this produces the following files in folder `data/fusion/`:
# - `players_03_frwp.csv`: merged list of players
# - `teams_04_frwp.csv`: merged list of teams
# - `stints_02_frwp.csv`: merged list of stints

# italian Wikipedia
source("src/wikipedia/italian/integrate_data.R")
# this produces the following files in folder `data/fusion/`:
# - `players_04_itwp.csv`: merged list of players
# - `teams_05_itwp.csv`: merged list of teams
# - `stints_03_itwp.csv`: merged list of stints

# spanish Wikipedia
source("src/wikipedia/spanish/integrate_data.R")
# this produces the following files in folder `data/fusion/`:
# - `players_05_eswp.csv`: merged list of players
# - `teams_06_eswp.csv`: merged list of teams
# - `stints_04_eswp.csv`: merged list of stints

# english Wikipedia
source("src/wikipedia/english/integrate_data.R")
# this produces the following files in folder `data/fusion/`:
# - `players_06_enwp.csv`: merged list of players
# - `teams_07_enwp.csv`: merged list of teams
# - `stints_05_enwp.csv`: merged list of stints




########################################################################
# finalize the tables by merging similar stints, and performing other cleaning operations

# first round of merging stints
source("src/fusion/merge_stints.R")
# other stint merging scripts, not used in the end
source("src/fusion/merge_stints_identical.R")
source("src/fusion/merge_stints_nested.R")

# fix various issues
source("src/fusion/clean_data.R")
# add missing stint types
source("src/fusion/add_stint_types.R")
# detect missing birthdates
source("src/fusion/complement_birthdates.R")
# add missing stint years
source("src/fusion/complement_firststints.R")

# identify remaining errors for manual fix
source("src/fusion/check_players.R")
source("src/fusion/check_teams.R")
source("src/fusion/check_stints.R")

# this produces all the remaning files in folder `data/fusion`:
# - `players_XX.csv`: various versions of the players table
# - `teams_XX.csv`: various versions of the teams table
# - `stints_XX.csv`: various versions of the stint tables

# the very final version of these tables is located in folder `data`:
# - `players.csv`: final list of players
# - `teams.csv`: final list of teams
# - `stints.csv`: final list of stints




########################################################################
# produce the SQL database
source("src/sqlite/init_db.R")
# check that we reconstruct the original files correctly
source("src/sqlite/recover_csv.R")
# produce SQL dump
source("src/sqlite/dump_to_sql.R")

# this produces the following files in folder `data`:
# - `data/rugbyscope.sqlite`: SQLite DB file
# - `data/rugbyscope.sql`: SQL script to re-create DB




########################################################################
# generate stats and plots for the final version of the database

# statistics for the data taken globally
source("src/stats/global_players.R")
source("src/stats/global_teams.R")
source("src/stats/global_stints.R")

# statistics considering the temporal evolution of the data
source("src/stats/evol_all-nbr.R")
source("src/stats/evol_player-nbr.R")
source("src/stats/evol_player-size.R")
source("src/stats/evol_player-completeness.R")
source("src/stats/evol_team-nbr.R")
source("src/stats/evol_team-completeness.R")
source("src/stats/evol_stint-nbr.R")
source("src/stats/evol_stint-completeness.R")

# the produced files are located in folder `data/stats`
