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
# - `careers.csv`: career steps

# compute a few stats for these tables
source("src/wikidata/compute_stats.R")
# produced files located in `data/wikidata/stats`




########################################################################
# retrieve the DBpedia tables
source("src/dbpedia/retrieve_data.R")
# this produces the following CSV files in folder `data/dbpedia/`:
# - `players.csv`: list of players
# - `teams.csv`: list of teams
# no career table, as these data are not available on DBpedia

# merge DBpedia data (player and teams) into previously retrieved Wikidata tables
source("src/dbpedia/merge_wikidata.R")
# this produces two files in filder `data/dbpedia/`:
# - `fusion_players_wd-dbp.csv`: merged list of players
# - `fusion_teams_wd-dbp.csv`: merged list of players




########################################################################
# merge the manually curated reference teams into the previously merged team table
source("src/wikidata/merge_reference.R")
# this produces one file in filder `data/wikidata/`:
# - `fusion_players_wd-dbp-ref.csv`: merged list of players




########################################################################
# retrieve the raw data from Wikipedia by looping over the players listed in the above table
# TODO
