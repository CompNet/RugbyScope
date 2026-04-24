########################################################################
# Loads the raw English Wikipedia tables and performs some basic cleaning.
#
# 05/2025 Vincent Labatut
########################################################################
library("stringi")
library("stringr")
library("dplyr")
library("httr")
library("jsonlite")

source("src/common/logging.R")
source("src/common/norm_names.R")
source("src/common/norm_teams.R")




########################################################################
# start logging
start.rec.log("CleaningEnWP")




########################################################################
# load EN WP tables
tlog("Loading Wikipedia EN tables")
folder <- file.path("data", "wikipedia", "english")

players <- read.csv(file.path(folder, "raw", "player_info.csv"))
tlog(2, "Raw number of players: ", nrow(players))
players <- players %>% mutate(across(where(is.character), ~ na_if(., "")))

stints <- read.csv(file.path(folder, "raw", "stints_info.csv"))
tlog(2, "Raw number of stints: ", nrow(stints))
stints <- stints %>% mutate(across(where(is.character), ~ na_if(., "")))

stints_comp <- read.csv(file.path(folder, "raw", "stint_info_comp.csv"))
tlog(2, "Raw number of stints: ", nrow(stints_comp))
stints_comp <- stints_comp %>% mutate(across(where(is.character), ~ na_if(., "")))




########################################################################
# clean the player table
tlog(0, "Cleaning the player table")
tlog(2, "Number of players found on EN Wikipedia: ", nrow(players))


#### did that just once and recorded the correction in player_info6.csv
# # fix WD ids, which are incorrect for full homonyms
# tlog(2, "Fixing incorrect WD ids in complete homonyms")
# fusion_folder <- file.path("data", "fusion")
# fus_players <- read.csv(file.path(fusion_folder, "players_05_eswp.csv"))
# players[, "wpPage"] <- gsub("https://en.wikipedia.org/wiki/", "", players[, "wpPage"], fixed = TRUE)
# idx <- match(players[, "wpPage"], fus_players[, "wikipediaEn"])
# players[, "origWdId"] <- fus_players[idx, "wikidataId"]
# write.csv(players, file.path(folder, "raw/player_info6.csv"), row.names = FALSE)
####

#### did that just once, and recorded the correction in player_info7.csv
#### edit: not needed anymore after new WP scraping
# # remove duplicate rows (some rows are almost identical, for some reason)
# tt <- table(players[, "origWdId"])
# ids <- names(which(tt > 1))
# tlog(2, "Found ", length(ids), " duplicate rows: removing them")
# # checking that the duplicate rows are identical: fine
# #for (id in ids) {
# #  #print(id)
# #  idx <- which(players[, "origWdId"] == id)
# #  if ((is.na(players[idx[1], "birthDate"]) && !is.na(players[idx[2], "birthDate"]))
# #      || (!is.na(players[idx[1], "birthDate"]) && is.na(players[idx[2], "birthDate"]))
# #      || (!is.na(players[idx[1], "birthDate"]) && !is.na(players[idx[2], "birthDate"])
# #          && players[idx[1], "birthDate"] != players[idx[2], "birthDate"]))
# #    print(players[idx, 2:ncol(players)])
# #}
# # removing the second occurrence of the duplicate rows
# del <- c()
# for (id in ids) {
#  idx <- which(players[, "origWdId"] == id)
#  del <- c(del, idx[2])
# }
# players <- players[-del, ]
# write.csv(players, file.path(folder, "raw/player_info7.csv"), row.names = FALSE)
####

# normalize positions
tlog(2, "Normalize rugby positions")
all_positions <- players[, "positions"]
# all_positions <- gsub("\\[\\d+\\]", "", all_positions, fixed = FALSE)
all_positions <- gsub("2nd 5/8 (rugby union)", "Inside Centre", all_positions, fixed = TRUE)
all_positions <- gsub("/", "; ", all_positions, fixed = TRUE)
# all_positions <- gsub(Encoding("\xa0"), "_", all_positions, fixed = FALSE)
all_positions <- strsplit(all_positions, "; ")
unique_positions <- sort(unique(trimws(unlist(all_positions))))
#table(unique_positions)
#write.csv(unique_positions, file.path(folder, "check_positions.csv"), row.names = FALSE)
# position conversion map
temp <- read.csv(file.path(folder, "maps", "text2position.csv"))
map <- temp[, "position"]
names(map) <- temp[, "text"]
# clean positions
for (p in 1:length(all_positions)) {
  positions <- trimws(all_positions[[p]])

  # if (length(positions) == 0) {
  #   positions <- " "
  # } else {
    # normalize positions names
    for (position in names(map))
      positions[positions == position] <- map[position]
  # }

  # update list
  positions <- unique(positions[!is.na(positions)])
  all_positions[[p]] <- positions
}
# collapse to get strings again
all_positions <- sapply(all_positions, function(positions) paste0(positions, collapse = "; "))
all_positions[all_positions == "NA" | all_positions == ""] <- NA
names(all_positions) <- NULL
players[, "positions"] <- all_positions

# converting meter heights to centimeters
#sort(unique(players[, "height"]))
heights <- as.numeric(players[, "height"])
heights[heights == 0.0196] <- 1.96
heights[heights == 1.12] <- 1.69
heights[heights == 1.3] <- 1.78
heights[heights == 1.45] <- 1.78
heights[heights == 1.57] <- 1.78
idx <- which(heights < 3)
heights[idx] <- heights[idx] * 100
# remove zero heights
idx <- which(heights == 0)
heights[idx] <- NA
# update in table
players[, "height"] <- heights

# checking weights
#sort(unique(players[, "weight"]))
weights <- as.numeric(players[, "weight"])
weights[weights == 220] <- 107
# remove small weights
idx <- which(weights <= 10)
weights[idx] <- NA
# update in table
players[, "weight"] <- weights

# clean birth dates
birth_dates <- players[, "birthDate"]
# deal with certain incomplete dates
birth_dates <- gsub("^(\\d{4})$", "\\1-01-01", birth_dates, fixed = FALSE)      # form YYYY
birth_dates <- gsub("^(\\d{4}-\\d{2})$", "\\1-01", birth_dates, fixed = FALSE)  # form YYYY-MM
birth_dates <- gsub("^(\\d{4}-\\d{2})-00$", "\\1-01", birth_dates, fixed = FALSE)  # form YYYY-MM-00
birth_dates[!is.na(birth_dates) & birth_dates == ""] <- NA
#head(sort(unique(birth_dates)), 20)
#tail(sort(unique(birth_dates)), 20)
birth_dates[birth_dates == "2012-03-10"] <- "1925-06-11"
#idx <- which(!is.na(birth_dates) & is.na(as.Date(birth_dates)))
#print(birth_dates[idx])
birth_dates <- as.Date(birth_dates)
players[, "birthDate"] <- birth_dates

# clean death dates
death_dates <- players[, "deathDate"]
# deal with certain incomplete dates
death_dates <- gsub("^(\\d{4})$", "\\1-01-01", death_dates, fixed = FALSE)      # form YYYY
death_dates <- gsub("^(\\d{4}-\\d{2})$", "\\1-01", death_dates, fixed = FALSE)  # form YYYY-MM
death_dates <- gsub("^(\\d{4}-\\d{2})-00$", "\\1-01", death_dates, fixed = FALSE)  # form YYYY-MM-00
death_dates[!is.na(death_dates) & death_dates == ""] <- NA
#head(sort(unique(death_dates)), 20)
#tail(sort(unique(death_dates)), 20)
#idx <- which(!is.na(death_dates) & is.na(as.Date(death_dates)))
#print(death_dates[idx])
death_dates <- as.Date(death_dates)
players[, "deathDate"] <- death_dates

# clean birth places
tlog(2, "Normalize place names")
all_locs <- c(players[, "birthPlaceWP"], players[, "deathPlaceWP"])
#which(grepl(";", all_locs, fixed = TRUE))
all_locs <- gsub('cowra, new south wales[1]"warrangong", koorawatha (near cowra)', "cowra, new south wales", all_locs, fixed = TRUE)
all_locs <- gsub("\\[\\d+\\]", "", all_locs, fixed = FALSE)
all_locs <- gsub("[a]", "", all_locs, fixed = TRUE)
all_locs <- gsub("[citation needed]", "", all_locs, fixed = TRUE)
all_locs <- gsub("[", "", all_locs, fixed = TRUE)
all_locs <- gsub("]", "", all_locs, fixed = TRUE)
all_locs <- gsub(' ref name="scrum" />', "", all_locs, fixed = TRUE)
all_locs <- gsub("calcutta,bengal presidency,british india(present-day kolkata,west bengal, india)", "kolkata, west bengal, india", all_locs, fixed = TRUE)
all_locs <- gsub("sarn,mid glamorgan, wales(now sarn,bridgend county borough)", "sarn, wales", all_locs, fixed = TRUE)
all_locs <- gsub('"', "", all_locs, fixed = TRUE)
all_locs <- gsub("[’ʻ]", "'", all_locs, fixed = FALSE)
all_locs <- gsub("ǁ", "", all_locs, fixed = TRUE)
all_locs <- gsub("\\((.+)\\)", ", \\1", all_locs, fixed = FALSE)
all_locs <- gsub("africasiblings =suleiman hartzenbergmunier hartzenberg", "africa", all_locs, fixed = TRUE)
all_locs <- gsub("australia.", "australia", all_locs, fixed = TRUE)
all_locs <- gsub("hmshawke,atlantic ocean", "HLS hawke", all_locs, fixed = TRUE)
all_locs <- gsub("in what would becomesouth rhodesiaand later zimbabwe", "zimbabwe", all_locs, fixed = TRUE)
all_locs <- gsub("nearsouthampton", "southampton", all_locs, fixed = TRUE)
all_locs <- gsub("provinceunion", "province, union", all_locs, fixed = TRUE)
all_locs <- gsub("zagreb,sr croatia, sfr", "zagreb, croatia", all_locs, fixed = TRUE)
all_locs <- gsub("tailevufiji", "tailevu, fiji", all_locs, fixed = TRUE)
all_locs <- gsub("caerphillywales", "caerphilly, wales", all_locs, fixed = TRUE)
all_locs <- gsub("bury.england", "bury, england", all_locs, fixed = TRUE)
all_locs <- gsub("cornwallengland", "cornwall, england", all_locs, fixed = TRUE)
all_locs <- gsub(". england", "england", all_locs, fixed = TRUE)
all_locs <- gsub("antrimnorthern", "antrim, northern", all_locs, fixed = TRUE)
all_locs <- gsub("essexengland", "essex, england", all_locs, fixed = TRUE)
all_locs <- gsub("hertfordshireengland", "hertfordshire, england", all_locs, fixed = TRUE)
all_locs <- gsub("kadavu fiji", "kadavu, fiji", all_locs, fixed = TRUE)
all_locs <- gsub("kadavu. fiji", "kadavu, fiji", all_locs, fixed = TRUE)
all_locs <- gsub("london england", "london, england", all_locs, fixed = TRUE)
all_locs <- gsub("missouriunited", "missouri, united", all_locs, fixed = TRUE)
all_locs <- gsub("otagonew", "otago, new", all_locs, fixed = TRUE)
all_locs <- gsub("raised inlimerick", "limerick", all_locs, fixed = TRUE)
all_locs <- gsub("bloemfonteinsouth", "bloemfontein, south", all_locs, fixed = TRUE)
all_locs <- gsub("balghupar0india", "balghupar, india", all_locs, fixed = TRUE)
all_locs <- gsub("auckland new zealand", "auckland, new zealand", all_locs, fixed = TRUE)
all_locs <- gsub("ashton-under-lynedistrict", "ashton-under-lyne", all_locs, fixed = TRUE)
all_locs <- gsub("adourfrance", "adour, france", all_locs, fixed = TRUE)
all_locs <- gsub("waleswarrangong", "wales, warrangong", all_locs, fixed = TRUE)
all_locs <- gsub("parramattacolony of new south wales", "parramatta, new south wales", all_locs, fixed = TRUE)
all_locs <- gsub("ofsouth", "of south", all_locs, fixed = TRUE)
all_locs <- gsub("sr bosnia and herzegovina", "bosnia and herzegovina", all_locs, fixed = TRUE)
all_locs <- gsub("sr croatia", "croatia", all_locs, fixed = TRUE)
all_locs <- gsub("sr romania", "romania", all_locs, fixed = TRUE)
all_locs <- gsub("tbilisigeorgia", "tbilisi, georgia", all_locs, fixed = TRUE)
all_locs <- gsub("countyromania", "county, romania", all_locs, fixed = TRUE)
all_locs <- gsub("tongatapu. tonga", "tongatapu, tonga", all_locs, fixed = TRUE)
all_locs <- gsub("tyne & wear", "tyne and wear", all_locs, fixed = TRUE)
all_locs <- gsub("west riding of yorkshire", "west riding, yorkshire", all_locs, fixed = TRUE)
all_locs <- gsub("williston northern cape", "williston, northern cape", all_locs, fixed = TRUE)
all_locs <- gsub("present-day kolkata", "kolkata", all_locs, fixed = TRUE)
all_locs <- gsub("now bela-bela", "bela-bela", all_locs, fixed = TRUE)
all_locs <- gsub("now inmoldova", "inmoldova", all_locs, fixed = TRUE)
all_locs <- gsub("now lephalale", "lephalale", all_locs, fixed = TRUE)
all_locs <- gsub("now mokopane", "mokopane", all_locs, fixed = TRUE)
all_locs <- gsub("now sarn", "sarn", all_locs, fixed = TRUE)
all_locs <- gsub("nowdún laoghaire", "dún laoghaire", all_locs, fixed = TRUE)
all_locs <- gsub("noweastern cape", "eastern cape", all_locs, fixed = TRUE)
all_locs <- gsub("nowfree state", "free state", all_locs, fixed = TRUE)
all_locs <- gsub("nownamibia", "namibia", all_locs, fixed = TRUE)
all_locs <- gsub("nowpapua new guinea", "papua new guinea", all_locs, fixed = TRUE)
all_locs <- gsub("nowpolokwane", "polokwane", all_locs, fixed = TRUE)
all_locs <- gsub("nowwestern cape", "western cape", all_locs, fixed = TRUE)
all_locs <- gsub("nowzimbabwe", "zimbabwe", all_locs, fixed = TRUE)
all_locs <- gsub("scottish borders, scotland", "scotland", all_locs, fixed = TRUE)
all_locs <- gsub("bau,colony of fiji, british empire", "bau, fiji", all_locs, fixed = TRUE)
all_locs <- gsub("cape province south africa", "cape province, south africa", all_locs, fixed = TRUE)
all_locs <- gsub("cape province.south africa","cape province, south africa", all_locs, fixed = TRUE)
all_locs <- gsub("County Corkireland", "county cork, ireland", all_locs, fixed = TRUE)
all_locs <- gsub("dublin ireland", "dublin, ireland", all_locs, fixed = TRUE)
all_locs <- gsub("near thecanary islands", "canary islands, spain", all_locs, fixed = TRUE)
all_locs <- gsub("nearjutland", "jutland, denmark", all_locs, fixed = TRUE)
all_locs <- gsub("hmstiger,dogger bank,north sea", "england", all_locs, fixed = TRUE)
all_locs <- gsub("northern ireland.", "northern ireland", all_locs, fixed = TRUE)
all_locs <- gsub("nsw australia", "new south wales, australia", all_locs, fixed = TRUE)
all_locs <- gsub("nswaustralia", "new south wales, australia", all_locs, fixed = TRUE)
all_locs <- gsub("offcoromandel peninsula", "coromandel peninsula, new zealand", all_locs, fixed = TRUE)
all_locs <- gsub("at sea, officeland", "iceland", all_locs, fixed = TRUE)
all_locs <- gsub(".bucharest, romania", "bucharest, romania", all_locs, fixed = TRUE)
all_locs <- gsub(".tonedale, wellington, somerset", "tonedale, wellington, somerset", all_locs, fixed = TRUE)
all_locs <- gsub("†flers, france", "flers, france", all_locs, fixed = TRUE)
all_locs <- gsub("10th arrondissement of paris", "paris", all_locs, fixed = TRUE)
all_locs <- gsub("nearzillebeke", "zillebeke", all_locs, fixed = TRUE)
all_locs <- gsub("nearverdun", "verdun", all_locs, fixed = TRUE)
all_locs <- gsub("neartobruk", "tobruk", all_locs, fixed = TRUE)
all_locs <- gsub("nearte pohue", "te pohue", all_locs, fixed = TRUE)
all_locs <- gsub("nearosches", "osches", all_locs, fixed = TRUE)
all_locs <- gsub("nearnorthwich", "northwich", all_locs, fixed = TRUE)
all_locs <- gsub("nearnormanton", "normanton", all_locs, fixed = TRUE)
all_locs <- gsub("nearlüderitz", "lüderitz", all_locs, fixed = TRUE)
all_locs <- gsub("nearlille", "lille", all_locs, fixed = TRUE)
all_locs <- gsub("nearkimberley", "kimberley", all_locs, fixed = TRUE)
all_locs <- gsub("neargatton", "gatton", all_locs, fixed = TRUE)
all_locs <- gsub("neardornitz", "dornitz", all_locs, fixed = TRUE)
all_locs <- gsub("neardeir ibzi", "deir ibzi", all_locs, fixed = TRUE)
all_locs <- gsub("nearboshof", "boshof", all_locs, fixed = TRUE)
all_locs <- gsub("nearbir hakeim", "bir hakeim", all_locs, fixed = TRUE)
all_locs <- gsub("nearbathurst", "bathurst", all_locs, fixed = TRUE)
all_locs <- gsub("nearappleby", "appleby", all_locs, fixed = TRUE)
all_locs <- gsub("near shatterbury", "shatterbury", all_locs, fixed = TRUE)
all_locs <- gsub("washington d.c.", "washington dc", all_locs, fixed = TRUE)
all_locs <- gsub(",limpopo, south africa", "limpopo, south africa", all_locs, fixed = TRUE)
all_locs <- str_to_title(all_locs)
all_locs <- strsplit(all_locs, ",")
unique_locs <- sort(unique(trimws(unlist(all_locs))))
#write.csv(unique_locs, file.path(folder, "check_locations.csv"), row.names = FALSE)

# distinguish between towns/regions and countries
#### check number of distinct names by row
#tt <- table(sapply(all_locs, length))
#all_locs[which(sapply(all_locs, length) == 4)]
####
idx_unq <- which(sapply(all_locs, length) == 1)
idx_sev <- which(sapply(all_locs, length) > 1)
countries <- sapply(all_locs[idx_sev], function(ll) ll[length(ll)])
#unique_countries <- sort(unique(trimws(unlist(countries))))
#write.csv(unique_countries, file.path(folder, "check_countries.csv"), row.names = FALSE)
country_map <- read.csv(file.path(folder, "maps", "text2country.csv"))
countries <- rep(NA, length(all_locs))
# loop over entries with more than one location name
for (i in idx_sev) {
  locs <- trimws(all_locs[[i]])
  if (length(locs) > 1) {
    # look for the potential country in the map
    ii <- which(country_map[, "original"] == locs[length(locs)])
    # if it is up there
    if (length(ii) == 1) {
      # add the normalized string to the country list
      countries[i] <- country_map[ii, "normalized"]
      # and possibly delete the original string from locs
      if (!as.logical(country_map[ii, "keep"])) {
        locs <- locs[-length(locs)]
      }
    }
  }
  all_locs[[i]] <- locs
}
# try to identify countries in entries with a single name
for (i in idx_unq) {
  locs <- trimws(all_locs[[i]])
  if (length(locs) == 1) {
    # look for the potential country in the map (normalized names or not)
    ii <- which(country_map[, "normalized"] == locs[1])
    if (length(ii) == 0)
      ii <- which(country_map[, "original"] == locs[1])
    # if it is up there
    if (length(ii) >= 1) {
      # add the normalized string to the country list
      countries[i] <- country_map[ii[1], "normalized"]
    }
  }
  all_locs[[i]] <- locs
}
#length(which(!is.na(countries)))
# collapse to get strings again
all_locs <- sapply(all_locs, function(locs) paste0(locs, collapse = "; "))
all_locs[all_locs == "NA" | all_locs == ""] <- NA
names(all_locs) <- NULL
players[, "birthPlace"] <- all_locs[1:nrow(players)]
players[, "deathPlace"] <- all_locs[(nrow(players)+1):(2*nrow(players))]
# add countries as a column
colnames(players)[colnames(players) == "birthPlaceWP"] <- "birthCountry"
players[, "birthCountry"] <- countries[1:nrow(players)]
colnames(players)[colnames(players) == "deathPlaceWP"] <- "deathCountry"
players[, "deathCountry"] <- countries[(nrow(players)+1):(2*nrow(players))]
#write.csv(players, file.path(folder, "raw/tmp.csv"), row.names = FALSE)

# remove superfluous columns
sup_cols <- c("X", "debugComment", "wpPage", "currentTeam")
tlog(2, "Remove superfluous columns: ", paste0(sup_cols, collapse = ", "))
cols <- which(colnames(players) %in% sup_cols)
players <- players[, -cols]

# rename certain columns
colnames(players)[which(colnames(players) == "origWdId")] <- "wikidataId"
colnames(players)[which(colnames(players) == "origName")] <- "fullName"
colnames(players)[which(colnames(players) == "wiki_Name")] <- "enName"




########################################################################
# clean the stint table
tlog(0, "Cleaning the stint table")

#### did that just once and recorded the correction in stint_info5.csv
## fix WD ids, which are incorrect for full homonyms
#tlog(2, "Fixing incorrect WD ids in complete homonyms")
#fusion_folder <- file.path("data", "fusion")
#fus_players <- read.csv(file.path(fusion_folder, "players_05_eswp.csv"))
#stints[, "wpPage"] <- gsub("https://en.wikipedia.org/wiki/", "", stints[, "wpPage"], fixed = TRUE)
#idx <- match(stints[, "wpPage"], fus_players[, "wikipediaEn"])
#stints[, "origWdId"] <- fus_players[idx, "wikidataId"]
#write.csv(stints, file.path(folder, "raw/stint_info5.csv"), row.names = FALSE)
####

#### did that just once, and recorded the correction in stint_info6.csv
## remove duplicate rows (some rows are identical, for some reason)
#str <- apply(stints[, 2:ncol(stints)], 1, function(row) paste0(row, collapse = ","))
#tt <- table(str)
#ids <- names(which(tt > 1))
#tlog(2, "Found ", length(ids), " duplicate rows: removing them")
## removing the second occurrence of the duplicate rows
#del <- c()
#for (id in ids) {
# idx <- which(str == id)
# del <- c(del, idx[2:length(idx)])
#}
#stints <- stints[-del, ]
#write.csv(stints, file.path(folder, "raw/stint_info6.csv"), row.names = FALSE)
####

#### debug: checking the unique period values
#head(sort(unique(stints[, "timePeriod"])))
#tail(sort(unique(stints[, "timePeriod"])))
####

# remove certain useless rows
idx <- which(stints[, "teamName"] == "total" | stints[, "timePeriod"] == "total") # rows displaying total numbers of points scored and match played
tlog("Removing ", length(idx), " spurious stints representing total statistics")
stints <- stints[-idx, ]
# # remove stints with footnote issues (temporary)
# idx <- which(stints[, "timePeriod"] %in% c("[", "1", "]"))
# tlog("Removing ", length(idx), " stints with a footnote instead of a date")
# stints <- stints[-idx, ]

# fix some specific cases
all_periods <- stints[, "timePeriod"]
all_periods <- gsub("\u00A0", " ", all_periods, fixed = FALSE)
all_periods <- gsub("\u200E", "", all_periods, fixed = FALSE)
all_periods <- gsub("\u2010", "-", all_periods, fixed = FALSE)
all_periods <- gsub("\u2013", "-", all_periods, fixed = FALSE)
all_periods <- gsub("\u2014", "-", all_periods, fixed = FALSE)
all_periods <- gsub("\u2015", "-", all_periods, fixed = FALSE)
all_periods <- gsub("\u2212", "-", all_periods, fixed = FALSE)
all_periods <- gsub("\u2500", "-", all_periods, fixed = FALSE)
all_periods <- gsub("\u30FC", "-", all_periods, fixed = FALSE)
all_periods <- gsub("^1964-19671970-1974$", "1964-1967, 1970-1974", all_periods, fixed = FALSE)
all_periods <- gsub("^1966-1970s1966-1969$", "1966-1970", all_periods, fixed = FALSE)
all_periods <- gsub("^1978-present1966-1978$", "1978-", all_periods, fixed = FALSE)
all_periods <- gsub("^-1995-2006$", "1995-1997, 2001-2006", all_periods, fixed = FALSE)
all_periods <- gsub("^1908-11, 13-20$", "1908-1911, 1913-1920", all_periods, fixed = FALSE)
all_periods <- gsub("^1899-1906, 1908-09, 11$", "1899-1906, 1908-1909, 1911-1911", all_periods, fixed = FALSE)
all_periods <- gsub("^1899-1906, 08-09, 11$", "1899-1906, 1908-1909, 1911-1919", all_periods, fixed = FALSE)
all_periods <- gsub("^1904-05, 08-09$", "1904-1905, 1908-1909", all_periods, fixed = FALSE)
all_periods <- gsub("^1907-08, 11-13$", "1907-1908, 1911-1913", all_periods, fixed = FALSE)
all_periods <- gsub("^1914-15, 19-22$", "1914-1915, 1919-1922", all_periods, fixed = FALSE)
all_periods <- gsub("^1904 & 1908$", "1904-1904, 1908-1908", all_periods, fixed = FALSE)
all_periods <- gsub("^1908-13 & 1919$", "1908-1913, 1919-1919", all_periods, fixed = FALSE)
all_periods <- gsub("^1938 & 1949$", "1938-1938, 1949-1949", all_periods, fixed = FALSE)
all_periods <- gsub("^1974&1977$", "1974-1974, 1977-1977", all_periods, fixed = FALSE)
all_periods <- gsub("^1988/89 - 1997/98$", "1988-1989, 1997-1998", all_periods, fixed = FALSE)
all_periods <- gsub("^1989 & 1993$", "1989-1989, 1993-1993", all_periods, fixed = FALSE)
all_periods <- gsub("^1994-1996/1997-2005$", "1994-1996, 1997-2005", all_periods, fixed = FALSE)
all_periods <- gsub("^1995-1996/1997-2005$", "1995-1996, 1997-2005", all_periods, fixed = FALSE)
all_periods <- gsub("^1995&2001$", "1995-1995, 2001-2001", all_periods, fixed = FALSE)
all_periods <- gsub("^1997/2005/2009$", "1997-1997, 2005-2005, 2009-2009", all_periods, fixed = FALSE)
all_periods <- gsub("^1998-03 & 2013-15$", "1998-2003, 2013-2015", all_periods, fixed = FALSE)
all_periods <- gsub("^1998/99$", "1998-1999", all_periods, fixed = FALSE)
all_periods <- gsub("^1998/99 -2000/01$", "1998-1999, 2000-2001", all_periods, fixed = FALSE)
all_periods <- gsub("^1999-2001 2006-08$", "1999-2001, 2006-2008", all_periods, fixed = FALSE)
all_periods <- gsub("^1999 & 2001$", "1999-1999, 2001-2001", all_periods, fixed = FALSE)
all_periods <- gsub("^1908-14, 20-21$", "1908-2014, 2020-2021", all_periods, fixed = FALSE)
all_periods <- gsub("^20000-2006$", "2000-2006", all_periods, fixed = FALSE)
all_periods <- gsub("^2006/07 -$", "2006-2007", all_periods, fixed = FALSE)
all_periods <- gsub("^2001/02 -$", "2001-2002", all_periods, fixed = FALSE)
all_periods <- gsub("^2002-2003 & 2011-2016$", "2002-2003, 2011-2016", all_periods, fixed = FALSE)
all_periods <- gsub("^2004-07 & 2011-2012$", "2004-2007, 2011-2012", all_periods, fixed = FALSE)
all_periods <- gsub("^2004 & 2006$", "2004-2004, 2006-2006", all_periods, fixed = FALSE)
all_periods <- gsub("^2006 & 2009$", "2006-2006, 2009-2009", all_periods, fixed = FALSE)
all_periods <- gsub("^2007 & 2007$", "2007-2007", all_periods, fixed = FALSE)
all_periods <- gsub("^2005, 2006, 2007 & 2008$", "2005-2008", all_periods, fixed = FALSE)
all_periods <- gsub("^2007 & 2009$", "2007-2007, 2009-2009", all_periods, fixed = FALSE)
all_periods <- gsub("^2008 & 2013$", "2008-2008, 2013-2013", all_periods, fixed = FALSE)
all_periods <- gsub("^2010 & 2012$", "2010-2010, 2012-2012", all_periods, fixed = FALSE)
all_periods <- gsub("^2011-2019 & 2023-2025$", "2011-2019, 2023-2025", all_periods, fixed = FALSE)
all_periods <- gsub("^2012-2019 2022-2023$", "2012-2019, 2022-2023", all_periods, fixed = FALSE)
all_periods <- gsub("^2013-2016 2020-$", "2013-2016, 2020-", all_periods, fixed = FALSE)
all_periods <- gsub("^2014-16 2024-$", "2014-16, 2024-", all_periods, fixed = FALSE)
all_periods <- gsub("^2014-2017 2019-2021$", "2014-2017, 2019-2021", all_periods, fixed = FALSE)
all_periods <- gsub("^2015-2020 2024-$", "2015-2020, 2024-", all_periods, fixed = FALSE)
all_periods <- gsub("^2018-2021 2023-2024$", "2018-2021, 2023-2024", all_periods, fixed = FALSE)
all_periods <- gsub("^2019-2022 2023-$", "2019-2022, 2023-", all_periods, fixed = FALSE)
all_periods <- gsub("^2020-2021 2023-$", "2020-2021, 2023-", all_periods, fixed = FALSE)
all_periods <- gsub("^2020-2022 2023-2024$", "2020-2022, 2023-2024", all_periods, fixed = FALSE)
all_periods <- gsub("^2008, 11, 15$", "2008-2008, 2011-2011, 2015-2015", all_periods, fixed = FALSE)
all_periods <- gsub("^2003-09, 11-12$", "2003-2009, 2011-2012", all_periods, fixed = FALSE)
all_periods <- gsub("^2009, 12-16$", "2009-2009, 2012-2016", all_periods, fixed = FALSE)
all_periods <- gsub("^1898, 1900-05, 12$", "1898-1898, 1900-1905, 1912-1912", all_periods, fixed = FALSE)
all_periods <- gsub("^2002-10, 13$", "2002-2010, 2013-2013", all_periods, fixed = FALSE)
all_periods <- gsub("^2005, 08, 13$", "2005-2005, 2008-2008, 2013-2013", all_periods, fixed = FALSE)
all_periods <- gsub("^1910, 14$", "1910-1910, 1914-1914", all_periods, fixed = FALSE)
all_periods <- gsub("^2012, 14-$", "2012-2012, 2014-", all_periods, fixed = FALSE)
all_periods <- gsub("^2011-13, 15$", "2011-2013, 2015-2015", all_periods, fixed = FALSE)
all_periods <- gsub("^2009-13, 15-$", "2009-2013, 2015-", all_periods, fixed = FALSE)
all_periods <- gsub("^2008,13, 15-$", "2008-2008, 2013-2013, 2015-", all_periods, fixed = FALSE)
all_periods <- gsub("^2008-14, 16$", "2008-2014, 2016-2016", all_periods, fixed = FALSE)
all_periods <- gsub("^2007-13, 16$", "2007-2013, 2016-2016", all_periods, fixed = FALSE)
all_periods <- gsub("^2015, 16-17, 18$", "2015-2015, 2016-2017, 2018-2018", all_periods, fixed = FALSE)
all_periods <- gsub("^2010-12, 17-$", "2010-2012, 2017-", all_periods, fixed = FALSE)
all_periods <- gsub("^2014, 17$", "2014-2014, 2017-2017", all_periods, fixed = FALSE)
all_periods <- gsub("^2013-14, 17$", "2013-2014, 2017-2017", all_periods, fixed = FALSE)
all_periods <- gsub("^2006-15, 17-19;$", "2006-2015, 2017-2019", all_periods, fixed = FALSE)
all_periods <- gsub("^2010-16, 18-19$", "2010-2016, 2018-2019", all_periods, fixed = FALSE)
all_periods <- gsub("^2006-12, 18-19$", "2006-2012, 2018-2019", all_periods, fixed = FALSE)
all_periods <- gsub("^1913-15, 19$", "1913-1915, 1919-1919", all_periods, fixed = FALSE)
all_periods <- gsub("^2014-2015, 19$", "2014-2015, 2019-2019", all_periods, fixed = FALSE)
all_periods <- gsub("^2016-18, 19-$", "2016-2018, 2019-", all_periods, fixed = FALSE)
all_periods <- gsub("^1988/89-1997/98$", "1988-1989, 1997-1998", all_periods, fixed = FALSE)
all_periods <- gsub("^1998/99-2000/01$", "1998-1999, 2000-2001", all_periods, fixed = FALSE)
all_periods <- gsub("^2013-14, 17-$", "2013-2014, 2017-", all_periods, fixed = FALSE)
all_periods <- gsub("^2001&2005$", "2001-2001, 2005-2005", all_periods, fixed = FALSE)
all_periods <- gsub("^1918, 20, 23, 25-26$", "1918-1918, 1920-1920, 1923-1923, 1925-1926", all_periods, fixed = FALSE)
all_periods <- gsub("^1918, 20, 23, 25-26, 30-31$", "1918-1918, 1920-1920, 1923-1923, 1925-1926, 1930-1931", all_periods, fixed = FALSE)
all_periods <- gsub("^2006, 2007 & 2008$", "2006-2008", all_periods, fixed = FALSE)
all_periods <- gsub("^1920, 23-27, 29$", "1920-1920, 1923-1927, 1929-1929", all_periods, fixed = FALSE)
all_periods <- gsub("^1921-22, 24-30$", "1921-1922, 1924-1930", all_periods, fixed = FALSE)
all_periods <- gsub("^1918-24, 26-27, 29-30$", "1918-1924, 1926-1927, 1929-1930", all_periods, fixed = FALSE)
all_periods <- gsub("^1923-25, 27-32$", "1923-1925, 1927-1932", all_periods, fixed = FALSE)
all_periods <- gsub("^1921-25, 27-29, 32$", "1921-1925, 1927-1929, 1932-1932", all_periods, fixed = FALSE)
all_periods <- gsub("^1922-23, 28-29$", "1922-1923, 1928-1929", all_periods, fixed = FALSE)
all_periods <- gsub("^1928, '29, '31$", "1928-1928, 1929-1929, 1931-1931", all_periods, fixed = FALSE)
all_periods <- gsub("^1928-30, 32-37$", "1928-1930, 1932-1937", all_periods, fixed = FALSE)
all_periods <- gsub("^1926-31, 36-38$", "1926-1931, 1936-1938", all_periods, fixed = FALSE)
all_periods <- gsub("^1931-34, 39$", "1931-1934, 1939-1939", all_periods, fixed = FALSE)
all_periods <- gsub("^1939, 41, 43, 46$", "1939-1939, 1941-1941, 1943-1943, 1946-1946", all_periods, fixed = FALSE)
all_periods <- gsub("^1941-42, 46-49$", "1941-1942, 1946-1949", all_periods, fixed = FALSE)
all_periods <- gsub("^1951-53, 55-64$", "1951-1953, 1955-1964", all_periods, fixed = FALSE)
all_periods <- gsub("^1951-55, 60-61$", "1951-1955, 1960-1961", all_periods, fixed = FALSE)
all_periods <- gsub("^1954-59, 61-66$", "1954-1959, 1961-1966", all_periods, fixed = FALSE)
all_periods <- gsub("^1954-60, 62-68$", "1954-1960, 1962-1968", all_periods, fixed = FALSE)
all_periods <- gsub("^1958-63, 65-67$", "1958-1963, 1965-1967", all_periods, fixed = FALSE)
all_periods <- gsub("^1957-65, 67-71$", "1957-1965, 1967-1971", all_periods, fixed = FALSE)
all_periods <- gsub("^1961-65, 68$", "1961-1965, 1968-1968", all_periods, fixed = FALSE)
all_periods <- gsub("^1963-64, 68-69$", "1963-1964, 1968-1969", all_periods, fixed = FALSE)
all_periods <- gsub("^1966-68, 70$", "1966-1968, 1970", all_periods, fixed = FALSE)
all_periods <- gsub("^1962-69,72$", "1962-1969, 1972-1972", all_periods, fixed = FALSE)
all_periods <- gsub("^1968-70, 74$", "1968-1970, 1974-1974", all_periods, fixed = FALSE)
all_periods <- gsub("^1970,75-76,78-80$", "1970-1970, 1975-1976, 1978-1980", all_periods, fixed = FALSE)
all_periods <- gsub("^1968-69, 76$", "1968-1969, 1976-1976", all_periods, fixed = FALSE)
all_periods <- gsub("^1966-74, 77-78$", "1966-1974, 1977-1978", all_periods, fixed = FALSE)
all_periods <- gsub("^1974, '76$", "1974-1974, 1976-1976", all_periods, fixed = FALSE)
all_periods <- gsub("^1975, '76$", "1975-1975, 1976-1976", all_periods, fixed = FALSE)
all_periods <- gsub("^1973-76, 79, 82$", "1973-1976, 1979-1979, 1982-1982", all_periods, fixed = FALSE)
all_periods <- gsub("^1972-77, 80$", "1972-1977, 1980-1980", all_periods, fixed = FALSE)
all_periods <- gsub("^1978, 80, 83$", "1978-1978, 1980-1980, 1983-1983", all_periods, fixed = FALSE)
all_periods <- gsub("^1875-77,80,82,83$", "1875-1877, 1880-1880, 1882-1882, 1883-1883", all_periods, fixed = FALSE)
all_periods <- gsub("^1972-77, 80-81$", "1972-1977, 1980-1981", all_periods, fixed = FALSE)
all_periods <- gsub("^1977-78, 80-83$", "1977-1978, 1980-1983", all_periods, fixed = FALSE)
all_periods <- gsub("^1971-78, 80-84$", "1971-1978, 1980-1984", all_periods, fixed = FALSE)
all_periods <- gsub("^1970-79, 81-87$", "1970-1979, 1981-1987", all_periods, fixed = FALSE)
all_periods <- gsub("^1977-78, 84-87$", "1977-1978, 1984-1987", all_periods, fixed = FALSE)
all_periods <- gsub("^1883, 86-89$", "1883-1883, 1886-1889", all_periods, fixed = FALSE)
all_periods <- gsub("^1881-83, 86-89, 92$", "1881-1883, 1886-1889, 1892-1892", all_periods, fixed = FALSE)
all_periods <- gsub("^1879-80, 88$", "1879-1880, 1888-1888", all_periods, fixed = FALSE)
all_periods <- gsub("^1981-86, 88-91$", "1981-1986, 1988-1991", all_periods, fixed = FALSE)
all_periods <- gsub("^1985-86, 90-94$", "1985-1986, 1990-1994", all_periods, fixed = FALSE)
all_periods <- gsub("^1985-88, 91-92, 95$", "1985-1988, 1991-1992, 1995-1995", all_periods, fixed = FALSE)
all_periods <- gsub("^1989, 92$", "1989-1989, 1992-1992", all_periods, fixed = FALSE)
all_periods <- gsub("^1986-87, 92-93$", "1986-1987, 1992-1993", all_periods, fixed = FALSE)
all_periods <- gsub("^1987, 92-94, 96$", "1987-1987, 1992-1994, 1996-1996", all_periods, fixed = FALSE)
all_periods <- gsub("^1983-86, 92-97$", "1983-1986, 1992-1997", all_periods, fixed = FALSE)
all_periods <- gsub("^1984-87, 93-94$", "1984-1987, 1993-1994", all_periods, fixed = FALSE)
all_periods <- gsub("^1989-90, 93-94$", "1989-1990, 1993-1994", all_periods, fixed = FALSE)
all_periods <- gsub("^1990-91, 93-96$", "1990-1991, 1993-1996", all_periods, fixed = FALSE)
all_periods <- gsub("^1989-91, 93-98$", "1989-1991, 1993-1998", all_periods, fixed = FALSE)
all_periods <- gsub("^1882, 94$", "1882-1882, 1894-1894", all_periods, fixed = FALSE)
all_periods <- gsub("^1891-92, 94$", "1891-1892, 1894-1894", all_periods, fixed = FALSE)
all_periods <- gsub("^1984-91, 94-95$", "1984-1991, 1994-1995", all_periods, fixed = FALSE)
all_periods <- gsub("^1890-92, 94-96$", "1890-1892, 1894-1896", all_periods, fixed = FALSE)
all_periods <- gsub("^1893, 95$", "1893-1893, 1895-1895", all_periods, fixed = FALSE)
all_periods <- gsub("^1993, 95-98$", "1993-1993, 1995-1998", all_periods, fixed = FALSE)
all_periods <- gsub("^1894, 96, 98, 1900$", "1894-1894, 1896-1896, 1898-1898, 1900-1900", all_periods, fixed = FALSE)
all_periods <- gsub("^1892-94, 96-97, 99-1903$", "1892-1894, 1896-1897, 1899-1903", all_periods, fixed = FALSE)
all_periods <- gsub("^1990-94, 96-99$", "1990-1994, 1996-1999", all_periods, fixed = FALSE)
all_periods <- gsub("^1991-93, 96-99$", "1991-1993, 1996-1999", all_periods, fixed = FALSE)
all_periods <- gsub("^1984-93, 97$", "1984-1993, 1997-1997", all_periods, fixed = FALSE)
all_periods <- gsub("^1991-94, 97-98$", "1991-1994, 1997-1998", all_periods, fixed = FALSE)
all_periods <- gsub("^1990-92, 98$", "1990-1992, 1998-1998", all_periods, fixed = FALSE)
all_periods <- gsub("^1994-95, 98-01$", "1994-1995, 1998-2001", all_periods, fixed = FALSE)
all_periods <- gsub("^1992-1994, 98-99$", "1992-1994, 1998-1999", all_periods, fixed = FALSE)
all_periods <- gsub("^1890-94, 98-99$", "1890-1894, 1898-1899", all_periods, fixed = FALSE)
all_periods <- gsub("^'98-'04, '06$", "1998-2004, 2006-2006", all_periods, fixed = FALSE)
# all_periods <- gsub("^xxxx$", "xxxx", all_periods, fixed = FALSE)
all_periods <- gsub("^(\\d{4})/(\\d+)$", "\\1-\\2", all_periods, fixed = FALSE)
all_periods <- gsub(";", ",", all_periods, fixed = FALSE)
all_periods <- strsplit(all_periods, ",")
unique_periods <- sort(unique(trimws(unlist(all_periods))))
#table(unique_periods)
norm_periods <- unique_periods
norm_periods <- gsub("['_?→<>≥]", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("^-$", "", norm_periods, fixed = FALSE)
norm_periods <- gsub(" ?- ?", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("\\+", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("-+", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("-unknown$", "-", norm_periods, fixed = FALSE)
#
norm_periods <- gsub("^c\\.", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("-c\\.", "-", norm_periods, fixed = FALSE)
#
norm_periods <- gsub("^ to present$", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^whitland 2022-present$", "2022-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^-05$", "-2005", norm_periods, fixed = FALSE)
norm_periods <- gsub("^-17$", "-2017", norm_periods, fixed = FALSE)
norm_periods <- gsub("^-92$", "-1992", norm_periods, fixed = FALSE)
norm_periods <- gsub("^xxxx-06$", "-2006", norm_periods, fixed = FALSE)
norm_periods <- gsub("^until 2011$", "-2011", norm_periods, fixed = FALSE)
norm_periods <- gsub("^until 1988$", "-1988", norm_periods, fixed = FALSE)
norm_periods <- gsub("^to c\\.1906representative$", "-1906", norm_periods, fixed = FALSE)
norm_periods <- gsub("^to 1968$", "-1968", norm_periods, fixed = FALSE)
norm_periods <- gsub("^to 1874$", "-1874", norm_periods, fixed = FALSE)
norm_periods <- gsub("^to 1871$", "-1871", norm_periods, fixed = FALSE)
norm_periods <- gsub("^summer 2006$", "2006", norm_periods, fixed = FALSE)
norm_periods <- gsub("^summer 2007$", "2007", norm_periods, fixed = FALSE)
norm_periods <- gsub("^pre-1883$", "-1883", norm_periods, fixed = FALSE)
norm_periods <- gsub("^oct\\. 2014$", "2014", norm_periods, fixed = FALSE)
norm_periods <- gsub("^aug\\. 2014$", "2014", norm_periods, fixed = FALSE)
norm_periods <- gsub("^dec 2019-$", "2019-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^dec 2017-2019$", "2017-2019", norm_periods, fixed = FALSE)
norm_periods <- gsub("^93-1900$", "1893-1900", norm_periods, fixed = FALSE)
norm_periods <- gsub("^98-1900$", "1898-1900", norm_periods, fixed = FALSE)
norm_periods <- gsub("^99-1900$", "1899-1900", norm_periods, fixed = FALSE)
norm_periods <- gsub("^99-1903$", "1899-1903", norm_periods, fixed = FALSE)
norm_periods <- gsub("-present day$", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("-present$", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("-pres(\\.)?$", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub(" onwards$", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("-current$", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub(" ?\\(loan\\)", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("^\\(0\\)$", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("^\\.$", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("^\\.{3}", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("^\\[", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("^\\]", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("^_+", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1878-0$", "1878-1880", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1876-0$", "1876-1880", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1874-0$", "1874-1880", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1871-0$", "1871-1880", norm_periods, fixed = FALSE)
norm_periods <- gsub("^-06$", "-2006", norm_periods, fixed = FALSE)
norm_periods <- gsub("^-00$", "-2000", norm_periods, fixed = FALSE)
norm_periods <- gsub("^-03$", "-2003", norm_periods, fixed = FALSE)
norm_periods <- gsub("^-97$", "-1997", norm_periods, fixed = FALSE)
norm_periods <- gsub("^0-1912$", "-1912", norm_periods, fixed = FALSE)
norm_periods <- gsub("^0-0$", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("^0-02$", "2000-2002", norm_periods, fixed = FALSE)
norm_periods <- gsub("0000", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("^00-02$", "2000-2002", norm_periods, fixed = FALSE)
norm_periods <- gsub("^00-1906$", "1900-1906", norm_periods, fixed = FALSE)
norm_periods <- gsub("^00-2012$", "2000-2012", norm_periods, fixed = FALSE)
norm_periods <- gsub("^0(\\d)$", "200\\1", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1882 to 1889\\.$", "1882-1889", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1884 to 1888$", "1884-1888", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1885 to 1888$", "1885-1888", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1888 to 1893$", "1888-1893", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1889 to 1892\\.$", "1889-1892", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1889 to 1893$", "1889-1893", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1892 to 1896$", "1892-1896", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1892-895$", "1892-1895", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1895-02$", "1895-1902", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1895-04$", "1895-1904", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1897-04$", "1897-1904", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1897-08$", "1897-1908", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1898-01$", "1898-1901", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1899-00$", "1899-1900", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1899-03$", "1899-1903", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1899-05$", "1899-1905", norm_periods, fixed = FALSE)
norm_periods <- gsub("^19-1991$", "-1991", norm_periods, fixed = FALSE)
norm_periods <- gsub("^19-1995$", "-1995", norm_periods, fixed = FALSE)
norm_periods <- gsub("^19-2004$", "-2004", norm_periods, fixed = FALSE)
norm_periods <- gsub("^19-1984$", "-1984", norm_periods, fixed = FALSE)
norm_periods <- gsub("^19-1987$", "-1987", norm_periods, fixed = FALSE)
norm_periods <- gsub("^190-1972$", "-1972", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1900-88$", "-1988", norm_periods, fixed = FALSE)
norm_periods <- gsub("^19-19$", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1910-1914\\(0\\)$", "1910-1914", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1936 \\(coach\\)$", "1936-1936", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1951-00$", "1951-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^197-1979$", "-1979", norm_periods, fixed = FALSE)
norm_periods <- gsub("^197-1986$", "-1986", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1970-00$", "1970-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1951-00$", "1951-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1907-00$", "1907-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2013-00$", "2013-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1979 to present$", "1979-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^198-19$", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("^198-1988$", "-1988", norm_periods, fixed = FALSE)
norm_periods <- gsub("^198-1990$", "-1990", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1981989$", "-1989", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1985-198$", "1985-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1986-19$", "1986-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1989-01$", "1989-2001", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1989-03$", "1989-2003", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1988-00$", "1988-2000", norm_periods, fixed = FALSE)
norm_periods <- gsub("^199-2002$", "-2002", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1990-199$", "1990-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1994-199$", "1994-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1998-200$", "1998-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1983-198$", "1983-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1988-1900$", "1988-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1900-1995$", "-1995", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1990-1900$", "1990-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1900-1998$", "-1998", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1980-1900$", "1980-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^(199\\d)-([01]\\d)$", "\\1-20\\2", norm_periods, fixed = FALSE)
norm_periods <- gsub("^\\d+[xs]+(-.*)$", "\\1", norm_periods, fixed = FALSE)
norm_periods <- gsub("^(.*-)\\d+[xs]+$", "\\1", norm_periods, fixed = FALSE)
norm_periods <- gsub("^\\d+[xs]+$", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("^20-2003$", "-2003", norm_periods, fixed = FALSE)
norm_periods <- gsub("^20-2007$", "-2007", norm_periods, fixed = FALSE)
norm_periods <- gsub("^20-2011$", "-2011", norm_periods, fixed = FALSE)
norm_periods <- gsub("^20-2014$", "-2014", norm_periods, fixed = FALSE)
norm_periods <- gsub("^20-2015$", "-2015", norm_periods, fixed = FALSE)
norm_periods <- gsub("^20-2017$", "-2017", norm_periods, fixed = FALSE)
norm_periods <- gsub("^20-2018$", "-2018", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2006-15 22-$", "2006-2015", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2010-nov 2017$", "2010-2017", norm_periods, fixed = FALSE)
norm_periods <- gsub("^20113$", "2011-2013", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2015-170$", "2015-2017", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2019-000$", "2019-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2019-2020-2021-2022-2023$", "2019-2023", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2019-2024\\.$", "2019-2024", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2019-nov 2019$", "2019-2019", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2021-\\.2022$", "2021-2022", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2022-2024\\.$", "2022-2024", norm_periods, fixed = FALSE)
norm_periods <- gsub("^20240-2022$", "2020-2022", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2018-\\.$", "2018-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^19600-62$", "1960-1962", norm_periods, fixed = FALSE)
norm_periods <- gsub("^199-1995$", "-1995", norm_periods, fixed = FALSE)
norm_periods <- gsub("^19121912$", "1912-1912", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2004 2007$", "2004-2007", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1936-19370$", "1936-1937", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2020-2027$", "2020-2023", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1987-01$", "1987-2001", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1988-04$", "1988-2004", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1984-1980$", "1984-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^1997-1990$", "1997-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2013-2000$", "2013-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2018-2000$", "2018-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^2001-2000$", "2001-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^0$", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("^200-$", "", norm_periods, fixed = FALSE)
# norm_periods <- gsub("^xxxx$", "xxxx", norm_periods, fixed = FALSE)
#
norm_periods <- gsub("^(\\d{4})-0$", "\\1-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^0(-\\d{4})$", "\\1", norm_periods, fixed = FALSE)
norm_periods <- gsub("^(\\d{4})$", "\\1-\\1", norm_periods, fixed = FALSE)
norm_periods <- gsub("^([01]\\d)-(\\d{2})$", "20\\1-20\\2", norm_periods, fixed = FALSE)
norm_periods <- gsub("^(\\d{3})(\\d)-(\\d)$", "\\1\\2-\\1\\3", norm_periods, fixed = FALSE)
norm_periods <- gsub("^(\\d{2})(\\d{2})-(\\d{2})$", "\\1\\2-\\1\\3", norm_periods, fixed = FALSE)
norm_periods <- gsub("^-$", "", norm_periods, fixed = FALSE)
#all_periods[which(sapply(all_periods, function(x) any(trimws(x)==unique_periods[which(norm_periods=="14")])))]

#### debug
#norm_periods[grepl("total", norm_periods, fixed = FALSE)]
#stints[grepl("total", stints[, "timePeriod"], fixed = FALSE), ]
####
#sort(unique(trimws(unlist(norm_periods))))
#print(unique_periods[which(norm_periods == "98-04")])
#print(stints[which(sapply(all_periods, function(lst) any(trimws(lst) == "2"))), ])
####
#write.csv(sort(unique(norm_periods)), file.path(folder, "raw/periods.csv"), row.names = FALSE)
####

# apply normalization to table
tlog("Applying date normalization to periods")
map_dates <- norm_periods
names(map_dates) <- unique_periods
for (p in 1:length(all_periods)) {
  ps <- trimws(all_periods[[p]])
  ps <- map_dates[ps]
  all_periods[[p]] <- ps
}
stints[, "timePeriod"] <- sapply(all_periods, function(row) paste0(row, collapse = "; "))

# add columns for start/end years
stints <- cbind(stints, matrix(NA, nrow = nrow(stints), ncol = 2))
colnames(stints)[(ncol(stints) - 1):ncol(stints)] <- c("startYear", "endYear")

# split rows containing multiple stints
new_stints <- stints[-(1:nrow(stints)), ]
for (r in 1:nrow(stints)) {
  # if (r %% 1000 == 0)
    tlog(4, "Processing row ", r, "/", nrow(stints))

  periods <- stints[r, "timePeriod"]
  team_names <- stints[r, "teamName"]
  team_urls <- stints[r, "teamWP"]
  matches_played <- as.character(stints[r, "matchesPlayed"])
  points_scored <- as.character(stints[r, "pointsScored"])

  # split period by semicolon
  ll <- 0
  if (is.na(periods) || periods == "")
    periods <- NA
  else
    periods <- trimws(strsplit(periods, ";")[[1]])
  if (is.na(team_names) || team_names == "")
    team_names <- NA
  else
    team_names <- trimws(team_names)
  if (is.na(team_urls) || team_urls == "")
    team_urls <- NA
  else
    team_urls <- trimws(team_urls)
  if (is.na(matches_played) || matches_played == "")
    matches_played <- NA
  else
    matches_played <- trimws(matches_played)
  if (is.na(points_scored) || points_scored == "")
    points_scored <- NA
  else
    points_scored <- trimws(points_scored)

  # processing each period
  years <- c()
  start_years <- c()
  end_years <- c()
  for (p in 1:length(periods)) {
    per <- periods[p]
    # there is a hyphen in the period
    if (grepl("-", per, fixed = TRUE)) {
      pers <- as.integer(strsplit(per, "-")[[1]])
      # end year missing
      if (length(pers) < 2 || is.na(pers[2])) {
        years <- c(years, 1)
        start_years <- c(start_years, pers[1])
        end_years <- c(end_years, NA)
      # start year missing
      } else if (is.na(pers[1])) {
        years <- c(years, 1)
        start_years <- c(start_years, NA)
        end_years <- c(end_years, pers[2])
      # both start and end years present
      } else {
        if (pers[2] < pers[1])
          pers[2] <- pers[2] + floor(pers[1] / 100) * 100
        years <- c(years, pers[2] - pers[1] + 1)
        start_years <- c(start_years, pers[1])
        end_years <- c(end_years, pers[2])
      }
    # no hyphen in the period
    } else {
      start_years <- c(start_years, per)
      end_years <- c(end_years, per)
      per <- paste0(per, "-", per)
      years <- c(years, 1)
    }
    periods[p] <- per
  }

  start_years <- trimws(start_years)
  start_years[start_years == "?"] <- NA
  end_years <- trimws(end_years)
  end_years[end_years == "?"] <- NA

  # init rows
  new_rows <- stints[rep(r, length(periods)), ]
  new_rows[, "timePeriod"] <- periods
  new_rows[, "teamName"] <- rep(team_names, length(periods))
  new_rows[, "teamWP"] <- rep(team_urls, length(periods))
  new_rows[, "startYear"] <- start_years
  new_rows[, "endYear"] <- end_years

  # adjust stats based on number of years
  if (!is.na(matches_played))
    new_rows[, "matchesPlayed"] <- round(as.integer(matches_played) * years / sum(years))
  if (!is.na(points_scored))
    new_rows[, "pointsScored"] <- round(as.integer(points_scored) * years / sum(years))

  # add rows to table
  new_stints <- rbind(new_stints, new_rows)
}
#### debug: check newly created year fields
#options(warn = 2)
#sort(unique(new_stints[, "startYear"]))
#sort(unique(new_stints[, "endYear"]))
#new_stints[which(new_stints[, "endYear"] == "4006"),]
####
#which(new_stints[, "startYear"] > new_stints[, "endYear"])
####

# clean team urls
idx <- which(grepl("action=edit&redlink=1", new_stints[, "teamWP"], fixed = FALSE))
if (length(idx) > 0)
  new_stints[idx, "teamWP"] <- NA
#tail(sort(unique(new_stints[, "teamWP"])))
idx <- which(new_stints[, "teamWP"] == "")
if (length(idx) > 0)
  new_stints[idx, "teamWP"] <- NA
#print(length(which(!is.na(new_stints[, "teamWP"]))))  # 60,339/63,792 non-NAs

# solve wikipedia redirections
old_urls <- sort(unique(new_stints[, "teamWP"]))
new_urls <- rep(NA, length(old_urls))
for (r in 1:length(old_urls)) {
  url <- old_urls[r]
  # if (r %% 100 == 0)
    tlog(4, "Solving redirections for entry ", r, "/", length(old_urls), " (", url, ")")

  if (!is.na(url)) {
    if (url == "")
      new_urls[r] <- NA
    else
      # solve redirection
      new_urls[r] <- solve_redirections(name = url, lang = "en")
      Sys.sleep(0.25)
  }

  if (!is.na(url) && is.na(new_urls[r]))
    tlog(6, "Lost URL: ", r)
}
# put unique URL into the sting table
idx1 <- match(new_stints[, "teamWP"], old_urls)
idx2 <- which(!is.na(idx1))
new_stints[idx2, "teamWP"] <- new_urls[idx1[idx2]]
#### debug: check if we lost some URL after the above processing
#print(length(which(!is.na(old_urls) & is.na(new_urls))))
#print(length(which(!is.na(new_stints[, "teamWP"]))))  # 60,338/63,792 non-NAs
####

# remove superfluous columns
#sup_cols <- c("X")
#tlog(2, "Remove superfluous columns: ", paste0(sup_cols, collapse = ", "))
#cols <- which(colnames(new_stints) %in% sup_cols)
#new_stints <- new_stints[, -cols]

# rename certain columns
colnames(new_stints)[which(colnames(new_stints) == "wiki_Name")] <- "enName"




########################################################################
# clean the complementary stint table
tlog(0, "Cleaning the complementary stint table")

#### debug: checking the unique period values
#head(sort(unique(stints_comp[, "timePeriod"])))
#tail(sort(unique(stints_comp[, "timePeriod"])))
####

# fix cases where only the club is provided
idx <- which(!is.na(stints_comp[, "timePeriod"]) & stints_comp[,"timePeriod"]!="\u2013" & stints_comp[,"timePeriod"]!="-" & !grepl("\\d", stints_comp[, "timePeriod"], fixed=FALSE) & is.na(stints_comp[, "teamName"]))
#tail(stints_comp[idx, c("timePeriod", "teamName")])
stints_comp[idx, "teamName"] <- stints_comp[idx, "timePeriod"]
stints_comp[idx, "timePeriod"] <- NA

# fix some specific cases
all_periods <- stints_comp[, "timePeriod"]
all_periods <- gsub("\u00A0", " ", all_periods, fixed = FALSE)
all_periods <- gsub("\u200E", "", all_periods, fixed = FALSE)
all_periods <- gsub("\u2014", "-", all_periods, fixed = FALSE)
all_periods <- gsub("\u2013", "-", all_periods, fixed = FALSE)
all_periods <- gsub("\u2015", "-", all_periods, fixed = FALSE)
all_periods <- gsub("\u2212", "-", all_periods, fixed = FALSE)
#
all_periods <- gsub(";", ",", all_periods, fixed = FALSE)
all_periods <- strsplit(all_periods, ",")
unique_periods <- sort(unique(trimws(unlist(all_periods))))
#
norm_periods <- unique_periods
norm_periods <- gsub("['_→<>≥]", "", norm_periods, fixed = FALSE)
norm_periods <- gsub(" ?- ?", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("\\+", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("-+", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("-unknown$", "-", norm_periods, fixed = FALSE)
#
norm_periods <- gsub("^c\\.", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("[-–]c\\.", "-", norm_periods, fixed = FALSE)
#
norm_periods <- gsub("^\\?-", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^\\?{2}-", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("^\\?{4}-", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("-\\?$", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("-\\?{2}$", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("-\\?{4}$", "-", norm_periods, fixed = FALSE)
norm_periods <- gsub("\\d{2}\\?{2}", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("\\d{3}\\?", "", norm_periods, fixed = FALSE)
norm_periods <- gsub("^\\?+$", "", norm_periods, fixed = FALSE)
#idx <- which(grepl("?", norm_periods, fixed=TRUE))
#norm_periods[idx]
#
norm_periods <- gsub("^-$", "", norm_periods, fixed = FALSE)
##### debug
# print(head(sort(norm_periods)))
# print(sort(norm_periods)[1:500])
# str1 <- "20-2017" 
# idx1 <- which(norm_periods == str1)
# str2 <- unique_periods[idx1]
# idx2 <- which(str2 == all_periods)
# print(all_periods[idx2]); print(idx2)
#####

# apply normalization to table
tlog("Applying date normalization to periods")
map_dates <- norm_periods
names(map_dates) <- unique_periods
for (p in 1:length(all_periods)) {
  ps <- trimws(all_periods[[p]])
  ps <- map_dates[ps]
  all_periods[[p]] <- ps
}
stints_comp[, "timePeriod"] <- sapply(all_periods, function(row) paste0(row, collapse = "; "))

# add columns for start/end years
stints_comp <- cbind(stints_comp, matrix(NA, nrow = nrow(stints_comp), ncol = 2))
colnames(stints_comp)[(ncol(stints_comp) - 1):ncol(stints_comp)] <- c("startYear", "endYear")

# split rows containing multiple stints
new_stints_comp <- stints_comp[-(1:nrow(stints_comp)), ]
for (r in 1:nrow(stints_comp)) {
  # if (r %% 1000 == 0)
    tlog(4, "Processing row ", r, "/", nrow(stints_comp))

  periods <- stints_comp[r, "timePeriod"]
  team_names <- stints_comp[r, "teamName"]
  team_urls <- stints_comp[r, "teamWP"]
  matches_played <- as.character(stints_comp[r, "matchesPlayed"])
  points_scored <- as.character(stints_comp[r, "pointsScored"])

  # split period by semicolon
  ll <- 0
  if (is.na(periods) || periods == "")
    periods <- NA
  else
    periods <- trimws(strsplit(periods, ";")[[1]])
  if (is.na(team_names) || team_names == "")
    team_names <- NA
  else
    team_names <- trimws(team_names)
  if (is.na(team_urls) || team_urls == "")
    team_urls <- NA
  else
    team_urls <- trimws(team_urls)
  if (is.na(matches_played) || matches_played == "")
    matches_played <- NA
  else
    matches_played <- trimws(matches_played)
  if (is.na(points_scored) || points_scored == "")
    points_scored <- NA
  else
    points_scored <- gsub("[(),]", "", trimws(points_scored))

  # processing each period
  years <- c()
  start_years <- c()
  end_years <- c()
  for (p in 1:length(periods)) {
    per <- periods[p]
    # there is a hyphen in the period
    if (grepl("-", per, fixed = TRUE)) {
      pers <- as.integer(strsplit(per, "-")[[1]])
      # end year missing
      if (length(pers) < 2 || is.na(pers[2])) {
        years <- c(years, 1)
        start_years <- c(start_years, pers[1])
        end_years <- c(end_years, NA)
      # start year missing
      } else if (is.na(pers[1])) {
        years <- c(years, 1)
        start_years <- c(start_years, NA)
        end_years <- c(end_years, pers[2])
      # both start and end years present
      } else {
        if (pers[2] < pers[1])
          pers[2] <- pers[2] + floor(pers[1] / 100) * 100
        years <- c(years, pers[2] - pers[1] + 1)
        start_years <- c(start_years, pers[1])
        end_years <- c(end_years, pers[2])
      }
    # no hyphen in the period
    } else {
      start_years <- c(start_years, per)
      end_years <- c(end_years, per)
      per <- paste0(per, "-", per)
      years <- c(years, 1)
    }
    periods[p] <- per
  }

  start_years <- trimws(start_years)
  start_years[start_years == "?"] <- NA
  end_years <- trimws(end_years)
  end_years[end_years == "?"] <- NA

  # init rows
  new_rows <- stints_comp[rep(r, length(periods)), ]
  new_rows[, "timePeriod"] <- periods
  new_rows[, "teamName"] <- rep(team_names, length(periods))
  new_rows[, "teamWP"] <- rep(team_urls, length(periods))
  new_rows[, "startYear"] <- start_years
  new_rows[, "endYear"] <- end_years

  # adjust stats based on number of years
  if (!is.na(matches_played))
    new_rows[, "matchesPlayed"] <- round(as.integer(matches_played) * years / sum(years))
  if (!is.na(points_scored))
    new_rows[, "pointsScored"] <- round(as.integer(points_scored) * years / sum(years))

  # add rows to table
  new_stints_comp <- rbind(new_stints_comp, new_rows)
}
#### debug: check newly created year fields
#options(warn = 2)
#sort(unique(new_stints_comp[, "startYear"]))
#sort(unique(new_stints_comp[, "endYear"]))
#new_stints_comp[which(new_stints_comp[, "endYear"] == "4006"),]
####
#idx <- which(new_stints_comp[, "startYear"] > new_stints_comp[, "endYear"])
#print(new_stints_comp[idx, ])
####

# clean team urls
idx <- which(grepl("action=edit&redlink=1", new_stints_comp[, "teamWP"], fixed = FALSE))
if (length(idx) > 0)
  new_stints_comp[idx, "teamWP"] <- NA
#tail(sort(unique(new_stints_comp[, "teamWP"])))
idx <- which(new_stints_comp[, "teamWP"] == "")
if (length(idx) > 0)
  new_stints_comp[idx, "teamWP"] <- NA
#print(length(which(!is.na(new_stints_comp[, "teamWP"]))))  # 8,991/11,515 non-NAs

# solve wikipedia redirections
old_urls <- sort(unique(new_stints_comp[, "teamWP"]))
new_urls <- rep(NA, length(old_urls))
for (r in 1:length(old_urls)) {
  url <- old_urls[r]
  # if (r %% 100 == 0)
    tlog(4, "Solving redirections for entry ", r, "/", length(old_urls), " (", url, ")")

  if (!is.na(url)) {
    if (url == "")
      new_urls[r] <- NA
    else
      # solve redirection
      new_urls[r] <- solve_redirections(name = url, lang = "en")
      Sys.sleep(0.25)
  }

  if (!is.na(url) && is.na(new_urls[r]))
    tlog(6, "Lost URL: ", r)
}
# put unique URL into the sting table
idx1 <- match(new_stints_comp[, "teamWP"], old_urls)
idx2 <- which(!is.na(idx1))
new_stints_comp[idx2, "teamWP"] <- new_urls[idx1[idx2]]
#### debug: check if we lost some URL after the above processing
#print(length(which(!is.na(old_urls) & is.na(new_urls))))
#print(length(which(!is.na(new_stints_comp[, "teamWP"]))))  # 8,954/11,515 non-NAs
####




########################################################################
# merge stints and stints_comp

# init new table
new_new_stints <- new_stints
matches <- 0

# loop over complement table
tn <- str_to_upper(new_stints[, "teamName"])
tn_comp <- str_to_upper(new_stints_comp[, "teamName"])
for(r in 1:nrow(new_stints_comp)) {
  tlog(2, "Processing row ", r, "/", nrow(new_stints_comp))

  idx <- which(new_stints[, "origWdId"] == new_stints_comp[r, "origWdId"] &
                 tn == tn_comp[r] &
                 (is.na(new_stints[, "startYear"]) | is.na(new_stints_comp[r, "startYear"]) | new_stints[, "startYear"] == new_stints_comp[r, "startYear"]) &
                 (is.na(new_stints[, "endYear"]) | is.na(new_stints_comp[r, "endYear"]) | new_stints[, "endYear"] == new_stints_comp[r, "endYear"]))
  # nothing found: add row to table
  if (length(idx) == 0) {
    tlog(4, "No match found for row ", r, ": add to table")

    new_new_stints <- rbind(new_new_stints, data.frame(new_stints_comp[r, c("origWdId","origName")], "wiki_name"="", new_stints_comp[r, c("wpPage","stintType","timePeriod","teamName","teamWP","matchesPlayed","pointsScored","startYear","endYear")]))

  # several matches found: log warning
  } else if (length(idx) > 1) {
    tlog(6, "WARNING: Multiple matches found for row ", r, ": ", paste0(idx, collapse = ", "))

  # just one single match: merge into table
  } else {
    tlog(4, "Match found for row ", r, ": merge into table")
    matches <- matches + 1

    if (is.na(new_stints[idx, "stintType"]))
      new_new_stints[idx, "stintType"] <- new_stints_comp[r, "stintType"]
    if (is.na(new_stints[idx, "teamName"]))
      new_new_stints[idx, "teamName"] <- new_stints_comp[r, "teamName"]
    if (is.na(new_stints[idx, "teamWP"]))
      new_new_stints[idx, "teamWP"] <- new_stints_comp[r, "teamWP"]
    if (is.na(new_stints[idx, "teamWP"]))
      new_new_stints[idx, "teamWP"] <- new_stints_comp[r, "teamWP"]
    if (is.na(new_stints[idx, "timePeriod"]))
      new_new_stints[idx, "timePeriod"] <- new_stints_comp[r, "timePeriod"]
    if (is.na(new_stints[idx, "endYear"]))
      new_new_stints[idx, "endYear"] <- new_stints_comp[r, "endYear"]
    if (is.na(new_stints[idx, "matchesPlayed"]))
      new_new_stints[idx, "matchesPlayed"] <- new_stints_comp[r, "matchesPlayed"]
    if (is.na(new_stints[idx, "pointsScored"]))
      new_new_stints[idx, "pointsScored"] <- new_stints_comp[r, "pointsScored"]
  }
}
tlog(2, "Number of matches found: ", matches, "/", nrow(new_stints_comp)) # 11,406

new_stints_comp[, "pointsScored"] <- as.integer(new_stints_comp[, "pointsScored"])




########################################################################
# record both cleaned tables as new files

# record player table
tab_file <- file.path(folder, "players.csv")
tlog(2, "Record player table as: ", tab_file)
write.csv(players, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# record stint table
tab_file <- file.path(folder, "stints.csv")
tlog(2, "Record stint table as: ", tab_file)
write.csv(new_new_stints, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# stop logging
end.rec.log()
