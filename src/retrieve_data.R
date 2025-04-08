########################################################################
# Main script for the raw data retrieval process. It invokes the
# Wikidata, DBpedia and Wikipedia scripts to form the tables used to
# extract various types of networks. The corresponding CSV files are
# stored in folder`data/tables`. See the individual scripts for details.
#
# Important: these scripts extract data from *live* repositories such as
# DBpedia, Wikidata, and Wikipedia. They all function correctly at the
# time of writing this comment, but it is likely that the evolution of
# these datasource will break them, and that they do not work anymore
# at the time of reading these lines.
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
# this produces the raw CSV files in folder `data/wikipedia/japanese/raw/`
source("src/wikipedia/japanese/clean_tables.R")
# this produces the clean CSV files in folder `data/wikipedia/japanese/`

# french wikipedia
system("python src/wikipedia/french/retrieve_stints.pys")
# this produces the raw CSV files in folder `data/wikipedia/french/raw/`
source("src/wikipedia/french/clean_tables.R")
# this produces the clean CSV files in folder `data/wikipedia/french/`

# italian wikipedia
system("python src/wikipedia/italian/retrieve_stints.pys")
# this produces the raw CSV files in folder `data/wikipedia/italian/raw/`
source("src/wikipedia/italian/clean_tables.R")
# this produces the clean CSV files in folder `data/wikipedia/italian/`

# english wikipedia
# TODO
source("src/wikipedia/english/clean_tables.R")

# spanish wikipedia
# TODO
source("src/wikipedia/spanish/clean_tables.R")




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
# this produces the following files in folder `data/fusion/`:
# - `players_03_frwp.csv`: merged list of players
# - `teams_04_frwp.csv`: merged list of teams
# - `stints_02_frwp.csv`: merged list of stints

# italian wikipedia
source("src/wikipedia/italian/integrate_data.R")
# this produces the following files in folder `data/fusion/`:
# - `players_04_itwp.csv`: merged list of players
# - `teams_05_itwp.csv`: merged list of teams
# - `stints_03_itwp.csv`: merged list of stints

# english wikipedia
source("src/wikipedia/english/integrate_data.R")

# spanish wikipedia
source("src/wikipedia/spanish/integrate_data.R")
