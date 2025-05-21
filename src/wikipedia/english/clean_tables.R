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

players <- read.csv(file.path(folder, "raw", "PP_player_info4.csv"))
tlog(2, "Raw number of players: ", nrow(players))
players <- players %>% mutate(across(where(is.character), ~ na_if(., "")))

stints <- read.csv(file.path(folder, "raw", "PP_stint_info.csv"))
tlog(2, "Raw number of stints: ", nrow(stints))
stints <- stints %>% mutate(across(where(is.character), ~ na_if(., "")))




########################################################################
# clean the player table
tlog(0, "Cleaning the player table")
tlog(2, "Number of players found on EN Wikipedia: ", nrow(players))


#### did that just once and recorded the correction in PP_player_info3.csv
## fix WD ids, which are incorrect for full homonyms
#tlog(2, "Fixing incorrect WD ids in complete homonyms")
#fusion_folder <- file.path("data", "fusion")
#fus_players <- read.csv(file.path(fusion_folder, "players_05_eswp.csv"))
#players[, "wpPage"] <- gsub("https://en.wikipedia.org/wiki/", "", players[, "wpPage"], fixed = TRUE)
#idx <- match(players[, "wpPage"], fus_players[, "wikipediaEn"])
#players[, "origWdId"] <- fus_players[idx, "wikidataId"]
#write.csv(players, file.path(folder, "raw/PP_player_info3.csv"), row.names = FALSE)
####

#### did that just once, and recorded the correctionin PP_player_info4.csv
# remove duplicate rows (some rows are almost identical, for some reason)
#tt <- table(players[, "origWdId"])
#ids <- names(which(tt > 1))
#tlog(2, "Found ", length(ids), " duplicate rows: removing them")
## checking that the duplicate rows are identical: fine
##for (id in ids) {
##  #print(id)
##  idx <- which(players[, "origWdId"] == id)
##  if ((is.na(players[idx[1], "birthDate"]) && !is.na(players[idx[2], "birthDate"]))
##      || (!is.na(players[idx[1], "birthDate"]) && is.na(players[idx[2], "birthDate"]))
##      || (!is.na(players[idx[1], "birthDate"]) && !is.na(players[idx[2], "birthDate"])
##          && players[idx[1], "birthDate"] != players[idx[2], "birthDate"]))
##    print(players[idx, 2:ncol(players)])
##}
## removing the second occurrence of the duplicate rows
#del <- c()
#for (id in ids) {
#  idx <- which(players[, "origWdId"] == id)
#  del <- c(del, idx[2])
#}
#players <- players[-del, ]
#write.csv(players, file.path(folder, "raw/PP_player_info4.csv"), row.names = FALSE)
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
#idx <- which(!is.na(birth_dates) & is.na(as.Date(birth_dates)))
#print(birth_dates[idx])
birth_dates <- as.Date(birth_dates)
players[, "birthDate"] <- birth_dates

# no death dates to clean

# clean birth places
tlog(2, "Normalize birth places")
all_locs <- players[, "birthPlaceWP"]
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
      # and possibly delete the string from locs
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
    if (length(ii) > 1) {
      # add the normalized string to the country list
      countries[i] <- country_map[ii, "normalized"]
    }
  }
  all_locs[[i]] <- locs
}
#length(which(!is.na(countries)))
# collapse to get strings again
all_locs <- sapply(all_locs, function(locs) paste0(locs, collapse = "; "))
all_locs[all_locs == "NA" | all_locs == ""] <- NA
names(all_locs) <- NULL
players[, "birthPlace"] <- all_locs
# add countries
players[, "birthPlaceWP"] <- countries
colnames(players)[colnames(players) == "birthPlaceWP"] <- "birthCountry"
#write.csv(players, file.path(folder, "raw/tmp.csv"), row.names = FALSE)

# remove superfluous columns
sup_cols <- c("X", "debugComment", "wpPage", "deathDate", "deathPlace", "deathPlaceWP", "currentTeam")
tlog(2, "Remove superfluous columns: ", paste0(sup_cols, collapse = ", "))
cols <- which(colnames(players) %in% sup_cols)
players <- players[, -cols]




########################################################################
# clean the stint table
tlog(0, "Cleaning the stint table")
# TODO

#### debug: checking the unique period values
#head(sort(unique(stints[, "timePeriod"])))
####

# fix some specific cases
stints[, "timePeriod"] <- gsub("[−–‐]", "-", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^02/17-2017$", "2017-2017", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^03/2025-$", "2025-", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^10/17-2020$", "2017-2020", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^10/2019-2020$", "2019-2020", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^11/22-01/23$", "2022-2023", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^2005-11/2005$", "2005-2005", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^2010-122011$", "2010-2011", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^2010-122011$", "2010-2011", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^2014-02/17$", "2014-2017", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^2016-10/2019$", "2016-2019", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^2017-10/17$", "2017-2017", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^avant 2005$", "-2005", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^Années 1960$", "-1960", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^été 2006$", "2006", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^Jusqu'en 1999$", "-1999", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^jusqu'en 2004$", "-2004", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^Jusqu'en 2006$", "-2006", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^Sept. 2013$", "2013-2013", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^2017-present$", "2017-", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("(19|20)[?x]{2}", "", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("(19\\d|20\\d)[?x]", "", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("[*?.… ]+", "", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("/", "-", stints[, "timePeriod"], fixed = TRUE)
stints[, "timePeriod"] <- gsub("^-$", "", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("--", "-", stints[, "timePeriod"], fixed = TRUE)
stints[, "timePeriod"] <- gsub("^(\\d{4})$", "\\1-\\1", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^-(\\d{4}-\\d{4})$", "\\1", stints[, "timePeriod"], fixed = FALSE)
stints[, "timePeriod"] <- gsub("^(\\d{4}-\\d{4})-$", "\\1", stints[, "timePeriod"], fixed = FALSE)
# stints <- data.frame(lapply(stints, function(col) gsub("\\[\\d+\\]", "", col, fixed = FALSE)))

# add columns for start/end years
stints <- cbind(stints, matrix(NA, nrow = nrow(stints), ncol = 2))
colnames(stints)[(ncol(stints) - 1):ncol(stints)] <- c("startYear", "endYear")

# split time periods
for (r in 1:nrow(stints)) {
  if (r %% 1000 == 0)
    tlog(4, "Processing row ", r, "/", nrow(stints))
  period <- stints[r, "timePeriod"]

  # there is a hyphen in the period
  if (grepl("-", period, fixed = TRUE)) {
    pers <- as.integer(strsplit(period, "-")[[1]])
    # end year missing
    if (length(pers) < 2 || is.na(pers[2])) {
      start_year <- pers[1]
      end_year <- NA
    # start year missing
    } else if (is.na(pers[1])) {
      start_years <- NA
      end_years <- pers[2]
    # both start and end years present
    } else {
      if (pers[2] < pers[1])
        pers[2] <- pers[2] + floor(pers[1] / 100) * 100
      start_year <- pers[1]
      end_year <- pers[2]
    }
  # no hyphen in the period
  } else {
    start_year <- as.integer(period)
    end_year <- as.integer(period)
  }

  # update table
  stints[r, "startYear"] <- trimws(start_year)
  stints[r, "endYear"] <- trimws(end_year)
  stints[r, "timePeriod"] <- paste0(start_year, "-", end_year)
}
#### debug: check newly created year fields
#sort(unique(stints[, "startYear"]))
#sort(unique(stints[, "endYear"]))
####

# clean team urls
idx <- which(grepl("action=edit&redlink=1", stints[, "teamWP"], fixed = FALSE))
if (length(idx) > 0)
  stints[idx, "teamWP"] <- NA
#tail(sort(unique(stints[, "teamWP"])))
idx <- which(stints[, "teamWP"] == "")
if (length(idx) > 0)
  stints[idx, "teamWP"] <- NA
#print(length(which(!is.na(stints[, "teamWP"]))))  # 41,334/46,999 non-NAs

# solve wikipedia redirections
old_urls <- stints[, "teamWP"]
for (r in 1:nrow(stints)) {
  url <- stints[r, "teamWP"]
  # if (r %% 100 == 0)
    tlog(4, "Solving redirections for entry ", r, "/", nrow(stints), " (", url, ")")

  if (!is.na(url)) {
    if (url == "")
      stints[r, "teamWP"] <- NA
    else
      # solve redirection
      stints[r, "teamWP"] <- solve_redirections(name = url, lang = "en")
  }

  if (!is.na(url) && is.na(stints[r, "teamWP"]))
    tlog(6, "Difference: ", r)
}
#### debug: check if we lost some URL after the above processing
#print(length(which(!is.na(old_urls) & is.na(stints[, "teamWP"]))))
#print(length(which(!is.na(stints[, "teamWP"]))))  # 41,334/46,999 non-NAs
####




########################################################################
# record both cleaned tables as new files

# record player table
tab_file <- file.path(folder, "players.csv")
tlog(2, "Record player table as: ", tab_file)
write.csv(players, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# record stint table
tab_file <- file.path(folder, "stints.csv")
tlog(2, "Record stint table as: ", tab_file)
write.csv(stints, tab_file, row.names = FALSE, fileEncoding = "UTF-8")




########################################################################
# stop logging
end.rec.log()
