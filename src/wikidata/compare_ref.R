# Compare the teams retrieved from Wikidata to those in the manually
# curated lists.
#
# Vincent Labatut
# 01/2025
########################################################################
library("stringi")
library("dplyr")

source("src/common/logging.R")




########################################################################
# paths
table_folder <- file.path("data", "wikidata", "tables")
ref_folder <- file.path("data", "references")




########################################################################
# Normalizes the specified club names by removing diacritics and case.
#
# names: original names.
# mode: FALSE for a light typographic normalization, TRUE for a more
#       advanced normalization using a map of acronyms. 
#
# returns: list of normalized names.
########################################################################
normalize_names <- function(names, mode = TRUE) {
  result <- names

  # remove all diacritics
  result <- stri_trans_general(str = result, id = "Latin-ASCII")

  # switch to uppercase
  result <- toupper(result)

  # remove points
  result <- gsub(".", "", result, fixed = TRUE)

  # replace hyphens by spaces
  result <- gsub("-", " ", result, fixed = TRUE)

  if (mode) {
    # turn standard expressions into acronyms
    map <- c()
    map["Amicale Laïque"] <- "AL"
    map["Amicale Sportive"] <- "AS"
    map["Association Sportive"] <- "AS"
    map["Association Sportive et Culturelle"] <- "ASC"
    map["Association Amicale et Sportive"] <- "AAS"
    map["Athletic Club"] <- "AC"
    map["Cercle Amical"] <- "CA"
    map["Cercle Municipal"] <- "CM"
    map["Club Amical"] <- "CA"
    map["Athletic Club Rugby"] <- "ACR"
    map["Club Athlétique"] <- "CA"
    map["Club Athlétique et Sportif"] <- "CAS"
    map["Club Atlético"] <- "CA"
    map["Club de Rugby"] <- "CR"
    map["Club Deportivo"] <- "CD"
    map["Club Gimnasia y Esgrima"] <- "CGE"
    map["Club Municipal"] <- "CM"
    map["Club Olympique"] <- "CO"
    map["Club Omnisport"] <- "CO"
    map["Club Social"] <- "CS"
    map["Club Social y Deportivo"] <- "CSD"
    map["Club Sportif"] <- "CS"
    map["Club Universitario"] <- "CU"
    map["Cricket & Rugby Club"] <- "CRC"
    map["Étoile Sportive"] <- "ES"
    map["Football Club"] <- "FC"
    map["Groupe Sportif"] <- "GS"
    map["Jeunesse Olympique"] <- "JO"
    map["Jeunesse Sportive"] <- "JS"
    map["Jockey Club"] <- "JC"
    map["Olympic Rugby Club"] <- "ORC"
    map["Racing Club"] <- "RC"
    map["Racing Rugby Club"] <- "RCC"
    map["Rassemblement"] <- "Ras"
    map["Rst"] <- "Ras"
    map["Rugby & Hockey Club"] <- "RHC"
    map["Rugby Athletic Club"] <- "RAC"
    map["Rugby Club"] <- "RC"
    map["Rugby Club Sportif"] <- "RCS"
    map["Rugby Football Club"] <- "RFC"
    map["Rugby Olympic Club"] <- "ROC"
    map["Rugby Olympique"] <- "RO"
    map["Rugby Union Football Club"] <- "RUFC"
    map["Rugby Union Sportive"] <- "RUS"
    map["Sport Athlétique"] <- "SA"
    map["Sport Rugby"] <- "SR"
    map["Sporting Club"] <- "SC"
    map["Sporting Union"] <- "SU"
    map["Stade Athlétique"] <- "SA"
    map["Stade Olympique"] <- "SO"
    map["Union Athlétique"] <- "UA"
    map["Union Club"] <- "UC"
    map["Union Sportive Athlétique"] <- "USA"
    map["Union Sportive Olympique"] <- "USO"
    map["Union Sportive"] <- "US"
    map["Universitario Rugby Club"] <- "URC"
    map["Université Club"] <- "UC"
    names <- names(map)
    map <- stri_trans_general(str = map, id = "Latin-ASCII")
    map <- toupper(map)
    names <- stri_trans_general(str = names, id = "Latin-ASCII")
    names <- toupper(names)
    map <- rev(map[order(nchar(names))])
    names <- rev(names[order(nchar(names))])
    names(map) <- names
    for (m in 1:length(map)) {
      mm <- names(map)[m]
      result <- gsub(mm, map[mm], result, fixed = TRUE)
    }

    # normalize "saint"
    result <- gsub("\\bSAINT(E)?\\b", "ST", result, fixed = FALSE)
  }

  return(result)
}




########################################################################
# Matches string from the source vector into the target vector. The
# function looks for the best matches, considering the specifics of
# rugby union club names. The specified strings can be lists of names,
# separated by "; ".
#
# If there are duplicates in the target list, it is possible for one
# source name to be matched to several target names, in # which case the
# function returns a list. Otherwise, it returns a vector.
#
# src_names: vector of names that we want to match to the reference names.
# tgt_names: vector of reference names.
#
# returns: best matches for each source name, as positions in the target
#          vectors, or NA if there is no match.
########################################################################
match_names <- function(src_names, tgt_names) {
  # init result list
  result <- list()

  ### first attempt: for all original names

  # apply light normalization
  norm_src_names <- normalize_names(names = src_names, mode = FALSE)
  norm_tgt_names <- normalize_names(names = tgt_names, mode = FALSE)

  # split names into lists
  norm_src_names <- strsplit(norm_src_names, "; ")
  norm_tgt_names <- strsplit(norm_tgt_names, "; ")

  # match each src names to the tgt names
  for (i in 1:length(norm_src_names)) {
    tlog(2, "Matching team #", i, "/", length(norm_src_names))
    tmp <- which(sapply(1:length(norm_tgt_names), function(j) {
      length(intersect(norm_src_names[[i]], norm_tgt_names[[j]])) > 0
    }))
    if(length(tmp) == 0)
      tmp <- NA
    result[[i]] <- tmp
  }


  ### second attempt: for non-matched names

  # perform deeper normalization
  norm_src_names <- normalize_names(names = src_names, mode = TRUE)
  norm_tgt_names <- normalize_names(names = tgt_names, mode = TRUE)

  # split names into lists
  norm_src_names <- strsplit(norm_src_names, "; ")
  norm_tgt_names <- strsplit(norm_tgt_names, "; ")

  # match each remaining src name to the tgt names
  idx <- which(sapply(result, function(x) all(is.na(x))))
  for (i in idx) {
    tlog(2, "Matching team #", i, "/", length(norm_src_names))
    tmp <- which(sapply(1:length(norm_tgt_names), function(j) {
      length(intersect(norm_src_names[[i]], norm_tgt_names[[j]])) > 0
    }))
    if(length(tmp) == 0)
      tmp <- NA
    result[[i]] <- tmp
  }


  ### build final result
  #idx <- which(sapply(result, length) > 1)
  i <- 1
  src_names[idx[i]]
  tgt_names[result[[idx[i]]]]
  if (all(sapply(result, length) == 1))
    result <- sapply(result, function(x) x[1])


  return(result)
}




########################################################################
# load data table
teams <- read.csv(file.path(table_folder, "all_teams_descr.csv"))
cat("Raw number of teams:", nrow(teams), "\n")




########################################################################
# load reference tables
all_teams <- NA
countries <- c("AR", "AU", "EN", "FJ", "FR", "IE", "IT", "JP", "NZ", "SC", "WA", "ZA")
country_refs <- list()
for (country in countries) {
  tab_file <- file.path(ref_folder, paste0(country, "_teams.csv"))

  # # debug
  # country_refs[[country]] <- read.csv(tab_file, sep = "\t")
  # write.csv(x = country_refs[[country]], file = tab_file, row.names = FALSE, fileEncoding = "UTF-8")

  country_refs[[country]] <- read.csv(tab_file)
  country_refs[[country]] <- country_refs[[country]] %>% mutate(across(where(is.character), ~ na_if(., "")))
  cat("Reading ", country, ": ", nrow(country_refs[[country]]), " teams\n", sep = "")

  if (all(is.na(all_teams)))
    all_teams <- country_refs[[country]]
  else
    all_teams <- rbind(all_teams, country_refs[[country]])
}

# check that the same WD id is not used several times in the reference table
tlog(0, "Checking duplicate WD ids in reference table:")
print(which(table(all_teams[, "wikidataId"]) > 1))




########################################################################
for (country in countries) {
  cat("Processing ", country, "\n", sep = "")

  # get info
  ref_names <- normalize_names(country_refs[[country]][, "name"])
  tiers <- country_refs[[country]][, "tier"]
  theor_hits <- length(which(!is.na(country_refs[[country]][, "wikidataId"])))
  wd_names <- normalize_names(teams[, "clubLabel"])

  # match names
  idx <- match(ref_names, wd_names)
  identified <- !is.na(idx)
  hits <- length(which(identified))


# TODO match alt names too
# use approximate matching to increase hits
# use already matched teams to restrict comparison to open cases
# use two steps: if not found after the first, remove generic rugby parts and re-check

# use ref table info to complete our tables: division, affiliation, country...

# check that the excel data do not contain several times the same WikidataID
# check whether clubs retrieved from WD are all present in the ref

# retrieve alt names from wikidata
# https://stackoverflow.com/questions/46850562/how-to-query-wikidata-for-also-known-as

  # display stats
  cat("Actual hits: ", hits, "/", length(ref_names), "\n", sep = "")
  cat("Theoretical hits: ", theor_hits, "/", length(ref_names), "\n", sep = "")
  print(table(tiers, identified))
  # print(ref_names[!identified])

  # record detailed list
  tab <- cbind(country_refs[[country]], teams[idx, c("clubId", "clubLabel")])
  tab_file <- file.path(table_folder, paste0(country, "_teams_matches.csv"))
  write.csv(tab, tab_file, row.names = FALSE)

  # display misses
  missed <- is.na(idx)
  print(cbind(country_refs[[country]][missed, "name"], country_refs[[country]][missed, "wikidataId"]))
}




#########################################################
# Number of clubs by country according to WP
# https://en.wikipedia.org/wiki/List_of_rugby_union_playing_countries
#########################################################
# Argentina:     420
# Australia:     767
# England:      1809
# Fiji:          490
# France:       1798
# Ireland:       221
# Italy:         784
# Japan:        1522
# New Zealand:   600
# Scotland:      251
# South Africa: 1526
# Wales:         250
# Note: this probably includes a bunch of clubs out of the standard pyramid (e.g. corporate clubs in France)
#########################################################

dpb_table_folder <- file.path("data", "dbpedia", "tables")
tab_file <- file.path(dpb_table_folder, "fusion_teams.csv")
teams <- read.csv(tab_file)
wd_names1 <- teams[, "fullName"]
wd_names2 <- teams[, "altNames"]
# ref_names <- country_refs[[country]][, "name"]
ref_names <- all_teams[, "name"]

# merge wd names in a single list
wd_names <- sapply(1:length(wd_names1), function(i) {
  if (is.na(wd_names2[i]))
    wd_names1[i]
  else
    paste0(wd_names1[i], "; ", wd_names2[i])
})

# match WD into reference
result <- match_names(src_names = wd_names, tgt_names = ref_names)
