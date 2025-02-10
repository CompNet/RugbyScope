########################################################################
# Loads the raw Japanese Wikipedia tables and performs some basic cleaning.
#
# 02/2025 Vincent Labatut
########################################################################
library("stringi")
library("stringr")
library("dplyr")

source("src/common/logging.R")
source("src/common/norm_teams.R")




########################################################################
# load WP JA tables
tlog("Loading Wikipedia JA tables")
folder <- file.path("data", "wikipedia", "japanese")

players <- read.csv(file.path(folder, "raw", "player_info.csv"))
tlog(2, "Raw number of players: ", nrow(players))
players <- players %>% mutate(across(where(is.character), ~ na_if(., "")))

careers <- read.csv(file.path(folder, "raw", "player_careers.csv"))
tlog(2, "Raw number of career steps: ", nrow(careers))
careers <- careers %>% mutate(across(where(is.character), ~ na_if(., "")))




########################################################################
# clean the player table
tlog(0, "Cleaning the player table")

# remove players with no career steps
# table(players[, "positions"])
#  [1] "origWdId"     "origName"     "debugComment" "jaName"       "wpPage"      
#  [6] "birthDate"    "birthPlace"   "birthPlaceWP" "deathDate"    "deathPlace"  
# [11] "deathPlaceWP" "height"       "weight"       "currentTeam"

# normalize positions
all_positions <- players[, "positions"]
all_positions <- gsub("\\[\\d+\\]", "", all_positions, fixed = FALSE)
all_positions <- gsub("、", "; ", all_positions, fixed = TRUE)
all_positions <- strsplit(all_positions, "; ")
unique_positions <- sort(unique(trimws(unlist(all_positions))))
# position conversion map
map <- c()
map["BK"] <- "Back"
map["CTB"] <- "Centre"
map["FB"] <- "Fullback"
map["FL"] <- "Flanker"
map["Flanker"] <- "Flanker"
map["LO"] <- "Lock"
map["Lock/Flanker"] <- "Lock; Flanker"
map["No. 8"] <- "Number 8"
map["No.8"] <- "Number 8"
map["NO8"] <- "Number 8"
map["Number 8"] <- "Number 8"
map["PR"] <- "Prop"
map["Second Row Forward"] <- "2nd Row"
map["SH"] <- "Scrum-Half"
map["SO"] <- "Fly-Half"
map["WTB"] <- "Winger"
map["インサイドセンター"] <- "Inside Centre"
map["ウィング"] <- "Winger"
map["ウイング"] <- "Winger"
map["ウィング(WTB)"] <- "Winger"
map["ウィング（WTB）"] <- "Winger"
map["ウィング・スクラムハーフ"] <- "Winger; Scrum-Half"
map["ウィング・フルバック"] <- "Winger; Fullback"
map["オープン"] <- "Openside Flanker"
map["オープンウィングアウトサイドセンター"] <- "Outside Centre"
map["スクラムハーフ"] <- "Scrum-Half"
map["スクラムハーフ　ウイング"] <- "Scrum-Half; Winger"
map["スクラムハーフ/ウィング"] <- "Scrum-Half; Winger"
map["スタンドオフ"] <- "Fly-Half"
map["スタンドオフ (フライハーフ)"] <- "Fly-Half"
map["スタンドオフ センター ウィング フルバック"] <- "Fly-Half; Centre; Winger; Fullback"
map["スタンドオフ　センター　フルバック"] <- "Fly-Half; Centre; Fullback"
map["スタンドオフ(SO)"] <- "Fly-Half"
map["スタンドオフ/センター"] <- "Fly-Half; Centre"
map["スタンドオフ/フライハーフ（SO/Flyhalf）"] <- "Fly-Half"
map["スリークォーターバック"] <- "Three-Quarter"
map["センター"] <- "Centre"
map["センター (CTB)"] <- "Centre"
map["センター(CTB)"] <- "Centre"
map["センター（CTB）"] <- "Centre"
map["センター, ウイング"] <- "Centre; Winger"
map["センター・フルバック"] <- "Centre; Fullback"
map["タイトヘッドプロップ"] <- "Tighthead Prop"
map["ナンバー8"] <- "Number 8"
map["ナンバーエイト"] <- "Number 8"
map["ナンバーエイトフランカー"] <- "Number 8; Flanker"
map["ハーフバック/スクラムハーフ"] <- "Scrum-Half"
map["フォワード"] <- "Forward"
map["フッカー"] <- "Hooker"
map["フライハーフ"] <- "Fly-Half"
map["フライハーフ（スタンドオフ）"] <- "Fly-Half"
map["フランカー"] <- "Flanker"
map["フランカー(FL)"] <- "Flanker"
map["フルバック"] <- "Fullback"
map["フルバック(FB)"] <- "Fullback"
map["プロップ"] <- "Prop"
map["フロントロー"] <- "1st Row"
map["ユーティリティBK"] <- "Utility Back"
map["ユーティリティバックス"] <- "Utility Back"
map["ラグビーユニオンのポジション#フルバック"] <- "Fullback"
map["ルースヘッド・プロップ"] <- "Loosehead Prop"
map["ロック"] <- "Lock"
map["ロック(LO)"] <- "Lock"
map["ロック／フランカー"] <- "Lock; Flanker"
map["不明"] <- ""
map["右ウィング"] <- "Right Winger"
map["右プロップ"] <- "Loosehead Prop"
# clean positions
for (p in 1:length(all_positions)) {
  positions <- all_positions[[p]]

  if (length(positions) == 0) {
    positions <- " "
  } else {
    # normalize positions names
    for (position in names(map))
      positions[positions == position] <- map[position]
  }

  # update list
  all_positions[[p]] <- positions
}
# collapse to get strings again
all_positions <- sapply(all_positions, function(positions) paste0(positions, collapse = "; "))
all_positions[all_positions == "NA"] <- NA
names(all_positions) <- NULL










########################################################################
# clean the career table
tlog(0, "Cleaning the career table")

# remove erroneous career steps
idx <- which(careers[, "period"] == "ERROR 404")
if (length(idx) > 0) {
  tlog(2, "Found ", length(idx), "/", nrow(careers), " incorrect career steps")
  careers <- careers[-idx, ]
}

# remove players involved only in removed teams
idx <- which(careers[, "teamId"] %in% removed_teams)
player_ids <- sort(unique(careers[idx, "playerId"]))
except_teams <- c()   # non-removed teams employing players that played for removed teams (debug)
remove_list <- c()    # players to remove (only played in removed teams)
for (player_id in player_ids) {
  ii <- which(careers[, "playerId"] == player_id)
  player_teams <- setdiff(unique(careers[ii, "teamId"]), removed_teams)
  except_teams <- union(except_teams, player_teams)
  if (length(player_teams) == 0)
    remove_list <- c(remove_list, player_id)
  # else
  #   print(player_teams)
}
if (length(remove_list) > 0) {
  idx <- match(remove_list, players[, "customId"])
  tlog(2, "Found ", length(idx), "/", nrow(players), " players involved exclusively in removed teams")
  players <- players[-idx, ]
}

# remove career steps involving removed teams
idx <- which(careers[, "teamId"] %in% removed_teams)
if (length(idx) > 0) {
  tlog(2, "Found ", length(idx), "/", nrow(careers), " career steps involving a removed team")
  careers <- careers[-idx, ]
}

# update team names based on team table (some teams were renamed)
idx <- match(careers[, "teamId"], teams[, "customId"])
careers[, "teamName"] <- teams[idx, "shortName"]

# normalize date format, split start/end years
tlog(2, "Normalizing dates")
careers[, "startYear"] <- NA
careers[, "endYear"] <- NA
str <- strsplit(careers[, "period"], "/")
for (s in 1:length(str)) {
  if (!all(is.na(str[[s]]))) {
    if (length(str[[s]]) == 1)
      period <- c(str[[s]], str[[s]])
    else
      period <- str[[s]]
    for(p in 1:2)
    if (as.integer(period[p]) < 27)
      period[p] <- paste0("20", period[p])
    else if (as.integer(period[p]) < 100)
      period[p] <- paste0("19", period[p])
    careers[s, c("startYear", "endYear")] <- period
  }
}

# remove now superflous period column
col <- which(colnames(careers) == "period")
careers <- careers[, -col]




########################################################################
# record all three cleaned tables as new files

# record player table
tab_file <- file.path(folder, "players_descr.csv")
tlog(2, "Record player table as: ", tab_file)
write.csv(players, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# record career table
tab_file <- file.path(folder, "players_careers.csv")
tlog(2, "Record career table as: ", tab_file)
write.csv(careers, tab_file, row.names = FALSE, fileEncoding = "UTF-8")
