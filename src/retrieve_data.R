########################################################################
# Main script for the raw data retrieval process. It invokes the
# Wikidata, DBpedia and Wikipedia scripts to form the tables used to
# extract various types of networks. The corresponding CSV files are
# stored in folder`data/tables`. See the individual scripts for details.
#
# Vincent Labatut
# 01/2025
#
# setwd("C:/Users/Vincent/eclipse/workspaces/Test/RugbyScope/RugbyScope")
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

# japanese wikipedia
system("python src/wikipedia/japanese/retrieve_stints.pys")
# this produces the raw files in folder `data/wikipedia/japanese/raw/`
source("src/wikipedia/japanese/clean_tables.R")
# this produces the clean files in folder `data/wikipedia/japanese/`

# french wikipedia
# TODO
source("src/wikipedia/french/clean_tables.R")

# english wikipedia
# TODO
source("src/wikipedia/english/clean_tables.R")

# spanish wikipedia
# TODO
source("src/wikipedia/spanish/clean_tables.R")

# italian wikipedia
# TODO
source("src/wikipedia/italian/clean_tables.R")




########################################################################
# merge the Wikipedia data with the previous tables

# japanese wikipedia
#source("src/wikipedia/japanese/compare_merged.R")
# optional: generates a few stats
source("src/wikipedia/japanese/integrate_data.R")
# this produces the following files in folder `data/fusion/`:
# - `players_02_jawp.csv`: merged list of players
# - `teams_03_jawp.csv`: merged list of teams
# - `stints_01_wd-jawp.csv`: merged list of stints

# french wikipedia
source("src/wikipedia/french/integrate_data.R")

# english wikipedia
source("src/wikipedia/english/integrate_data.R")

# spanish wikipedia
source("src/wikipedia/spanish/integrate_data.R")

# italian wikipedia
source("src/wikipedia/italian/integrate_data.R")
