########################################################################
# Loads the raw Japanese Wikipedia tables and performs some basic cleaning.
#
# 02/2025 Vincent Labatut
########################################################################
library("stringi")
library("stringr")
library("dplyr")
library("httr")
library("jsonlite")

source("src/common/logging.R")
source("src/common/norm_teams.R")




########################################################################
# Retrieves the English title of a Wikipedia page based on the name of
# the Japanese version of the page.
#
# ja_name: name of the Japanese WP page (the end of its URL, not its title).
#
# returns: the English title of the corresponding page.
########################################################################
get_english_title <- function(ja_name) {
  # set up HTTP query
  if (startsWith(ja_name, "http")) {
    lang <- gsub("https://([a-z]{2}).wikipedia.org/wiki/.*", "\\1", ja_name)
    ja_name <- gsub("https://[a-z]{2}.wikipedia.org/wiki/(.*)", "\\1", ja_name)
  } else {
    lang <- "ja"
    if (startsWith(ja_name, "%"))
      ja_name <- URLdecode(ja_name)
  }
  url <- paste0("https://", lang, ".wikipedia.org/w/api.php")

  params <- list(
    action = "query",
    prop = "langlinks",
    titles = ja_name,
    lllang = "en",
    format = "json"
  )

  # send to server
  go_on <- TRUE
  while (go_on) {
    response <- tryCatch({GET(url, query = params)}, error = function(e) {tlog("Server error: ", e$message); NA})
    go_on <- all(is.na(response))
  }

  # retrieve english title
  data <- content(response, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(data)
  page <- json_data$query$pages[[1]]
  result <- page$langlinks["*"][1, 1]

  return(result)
}




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

# remove players with no career steps
# table(players[, "positions"])
#  [1] "origWdId"     "origName"     "jaName"             
#  [6] "birthPlace"   "birthPlaceWP" "deathPlace"   "deathPlaceWP" 

# weights and heights are ok
#sort(unique(players[, "height"]))
#sort(unique(players[, "weight"]))

# birth and death dates are ok
#sort(unique(players[, "birthDate"]))
#sort(unique(players[, "deathDate"]))

# birth and death places
all_places <- c(players[, "birthPlaceWP"], players[, "deathPlaceWP"])
# all_places <- gsub("\\[.+\\]", "", all_places, fixed = FALSE)
all_places <- strsplit(all_places, "; ")
unique_places <- sort(unique(trimws(unlist(all_places))))
unique_places <- unique_places[!grepl("redlink=1", unique_places, fixed = TRUE)]
unique_places <- unique_places[!startsWith(unique_places, "#")]
map <- c()
for (i in 1:length(unique_places)) {
  unique_place <- unique_places[i]
  tlog(2, "Retrieving translation for \"", unique_place, "\" (", i, "/", length(unique_places), ")")
  if (startsWith(unique_place, "/wiki/"))
    unique_place <- substr(unique_place, start = nchar("/wiki/") + 1, stop = nchar(unique_place))
  title <- get_english_title(unique_place)
  tlog(4, "Result: ", title)
  if (is.null(title))
    title <- unique_place[i]
  map[unique_places[i]] <- title
}
map["https://en.wikipedia.org/wiki/%C5%8Ct%C4%81huhu"] <- "Ōtāhuhu"
map["https://en.wikipedia.org/wiki/B%C3%A2rlad"] <- "Bârlad"
map["https://en.wikipedia.org/wiki/Baulkham_Hills"] <- "Baulkham Hills"
map["https://en.wikipedia.org/wiki/Bivolari"] <- "Bivolari"
map["https://en.wikipedia.org/wiki/Busia,_Kenya"] <- "Busia, Kenya"
map["https://en.wikipedia.org/wiki/Craigavon,_County_Armagh"] <- "Craigavon; County Armagh"
map["https://en.wikipedia.org/wiki/Dewsbury"] <- "Dewsbury"
map["https://en.wikipedia.org/wiki/Gwaun-Cae-Gurwen"] <- "Gwaun-Cae-Gurwen"
map["https://en.wikipedia.org/wiki/Houma_(Tongatapu)"] <- "Houma; Tongatapu"
map["https://en.wikipedia.org/wiki/Las_Condes"] <- "Las Condes"
map["https://en.wikipedia.org/wiki/Llandough,_Penarth"] <- "Llandough; Penarth"
map["https://en.wikipedia.org/wiki/Maesycoed"] <- "Maesycoed"
map["https://en.wikipedia.org/wiki/Penarth"] <- "Penarth"
map["https://en.wikipedia.org/wiki/Petersham,_New_South_Wales"] <- "Petersham; New South Wales"
map["https://en.wikipedia.org/wiki/Pris%C4%83cani"] <- "Prisăcani"
map["https://en.wikipedia.org/wiki/Sidcup"] <- "Sidcup"
map["https://en.wikipedia.org/wiki/Tokoroa"] <- "Tokoroa"
map["https://en.wikipedia.org/wiki/Whakat%C4%81ne"] <- ""
map["https://es.wikipedia.org/wiki/Puke_(Tonga)"] <- "Puke; Tonga"
map["https://to.wikipedia.org/wiki/Lapaha"] <- "Lapaha"
map["https://to.wikipedia.org/wiki/Matahau"] <- "Matahau"
map["https://to.wikipedia.org/wiki/Tofoa"] <- "Tofoa"
map["/wiki/Auckland"] <- "Auckland"
map["/wiki/%E3%82%A4%E3%83%BC%E3%82%B9%E3%83%88%E3%83%AD%E3%83%B3%E3%83%89%E3%83%B3"] <- "East London; South Africa"
map["/wiki/%E3%82%A6%E3%82%A3%E3%83%AB%E3%83%88%E3%82%B7%E3%83%A3%E3%83%BC%E5%B7%9E"] <- "Wiltshire"
map["/wiki/%E3%82%A6%E3%82%A7%E3%82%B9%E3%83%88%E3%83%A8%E3%83%BC%E3%82%AF%E3%82%B7%E3%83%A3%E3%83%BC"] <- "West Yorkshire"
map["/wiki/%E3%82%A6%E3%82%A9%E3%83%89%E3%83%B3%E3%82%AC"] <- "Albury; New South Wales"
map["/wiki/%E3%82%A8%E3%82%A2_(%E3%82%B5%E3%82%A6%E3%82%B9%E3%83%BB%E3%82%A8%E3%82%A2%E3%83%BC%E3%82%B7%E3%83%A3%E3%83%BC)"] <- "Ayr; South Ayrshire"
map["/wiki/%E3%82%AA%E3%83%81%E3%83%AF%E3%83%AD%E3%83%B3%E3%82%B4"] <- "Ottiwarongo"
map["/wiki/%E3%82%AB%E3%83%8A%E3%83%AA%E3%82%A2%E8%AB%B8%E5%B3%B6%E5%B7%9E"] <- "Canary Islands"
map["/wiki/%E3%82%AB%E3%83%AB%E3%83%95%E3%82%A9%E3%83%AB%E3%83%8B%E3%82%A2%E5%B7%9E"] <- "California"
map["/wiki/%E3%82%AB%E3%83%B3%E3%83%96%E3%83%AA%E3%82%A2%E5%B7%9E"] <- "Cumbria; England"
map["/wiki/%E3%82%AD%E3%82%A8%E3%83%95"] <- "Kyiv"
map["/wiki/%E3%82%AD%E3%83%AB%E3%83%87%E3%82%A2%E5%B7%9E"] <- "County Kildare"
map["/wiki/%E3%82%B0%E3%83%A9%E3%83%8F%E3%83%A0%E3%82%BA%E3%82%BF%E3%82%A6%E3%83%B3"] <- "Makanda"
map["/wiki/%E3%82%B0%E3%83%AD%E3%82%B9%E3%82%BF%E3%83%BC%E3%82%B7%E3%83%A3%E3%83%BC"] <- "Gloucestershire"
map["/wiki/%E3%82%B3%E3%83%BC%E3%83%B3%E3%82%A6%E3%82%A9%E3%83%BC%E3%83%AB%E5%B7%9E"] <- "Cornwall"
map["/wiki/%E3%82%B5%E3%83%95%E3%82%A9%E3%83%BC%E3%82%AF%E5%B7%9E"] <- "Suffolk"
map["/wiki/%E3%82%B5%E3%83%AA%E3%83%BC%E5%B7%9E"] <- "Surrey; England"
map["/wiki/%E3%82%B9%E3%82%BF%E3%83%83%E3%83%95%E3%82%A9%E3%83%BC%E3%83%89%E3%82%B7%E3%83%A3%E3%83%BC%E5%B7%9E"] <- "Staffordshire"
map["/wiki/%E3%82%BF%E3%83%A9%E3%83%8A%E3%82%AD"] <- "Taranaki Region"
map["/wiki/%E3%83%87%E3%83%B4%E3%82%A9%E3%83%B3%E5%B7%9E"] <- "Devon"
map["/wiki/%E3%83%88%E3%83%B3%E3%82%AC%E7%8E%8B%E5%9B%BD"] <- "Tonga"
map["/wiki/%E3%83%8B%E3%83%A5%E3%83%BC%E3%83%A8%E3%83%BC%E3%82%AF%E5%B8%82"] <- "New York City"
map["/wiki/%E3%83%8E%E3%83%BC%E3%83%95%E3%82%A9%E3%83%BC%E3%82%AF%E5%B7%9E"] <- "Norfolk"
map["/wiki/%E3%83%8F%E3%83%B3%E3%83%97%E3%82%B7%E3%83%A3%E3%83%BC%E5%B7%9E"] <- "Hampshire"
map["/wiki/%E3%83%93%E3%82%B8%E3%83%A3%E3%83%9B%E3%83%A8%E3%83%BC%E3%82%B5"] <- "Villa Joyosa"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Australia.svg"] <- "Australia"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Catalonia.svg"] <- "Catalonia"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_England.svg"] <- "England"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Fiji.svg"] <- "Fiji"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_France.svg"] <- "France"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Georgia.svg"] <- "Georgia"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Japan.svg"] <- "Japan"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_New_South_Wales.svg"] <- "New South Wales"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_New_Zealand.svg"] <- "New Zealand"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Occitania.svg"] <- "Occitania"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Russia.svg"] <- "Russia"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Samoa.svg"] <- "Samoa"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_South_Africa.svg"] <- "South Africa"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_the_region_Auvergne-Rh%C3%B4ne-Alpes.svg"] <- "Auvergnes"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Tonga.svg"] <- "Tonga"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Wallis_and_Futuna.svg"] <- "Wallis and Futuna"
map["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Zimbabwe.svg"] <- "Zimbabwe"
map["/wiki/%E3%83%95%E3%83%A9%E3%82%A4%E3%83%96%E3%83%AB%E3%82%AF"] <- "Freiburg im Breisgau"
map["/wiki/%E3%83%96%E3%83%AA%E3%82%BA%E3%83%99%E3%83%B3"] <- "Brisbane"
map["/wiki/%E3%83%99%E3%82%B9%E3%83%AC%E3%83%98%E3%83%A0"] <- "Bethlehem"
map["/wiki/%E3%83%99%E3%83%AB%E3%83%B4%E3%82%A3%E3%83%AB"] <- "Belleville"
map["/wiki/%E3%83%A8%E3%83%BC%E3%82%AF%E3%82%B7%E3%83%A3%E3%83%BC%E5%B7%9E"] <- "Yorkshire"
map["/wiki/%E3%83%AD%E3%83%84%E3%83%9E%E5%B3%B6"] <- "Rotuma"
map["/wiki/%E4%BA%AC%E6%A9%8B%E5%8C%BA"] <- "Kyobashi Ward"
map["/wiki/%E4%BA%AC%E9%83%BD"] <- "Kyoto"
map["/wiki/%E5%8D%97%E3%82%A2%E3%83%95%E3%83%AA%E3%82%AB"] <- "South Africa"
map["/wiki/%E5%B0%8F%E7%9F%B3%E5%B7%9D%E5%8C%BA"] <- "Koishikawa Ward"
map["/wiki/%E5%B1%B1%E6%9D%B1%E7%9C%81_(%E6%B1%AA%E5%85%86%E9%8A%98%E6%94%BF%E6%A8%A9)"] <- "Shandong Province"






# remove superfluous columns
sup_cols <- c("debugComment", "wpPage", "currentTeam")





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
