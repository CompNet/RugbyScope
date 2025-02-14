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
# the page in the WP URL, possibly in a different language.
#
# name: name of the WP page (the end of its URL, not its title).
# lang: language of the WP page bearing this name.
#
# returns: the English title of the corresponding page.
########################################################################
get_english_title <- function(name, lang = "ja") {
  # normalize parameters
  if (startsWith(name, "http")) {
    lang <- gsub("https://([a-z]{2}).wikipedia.org/wiki/.*", "\\1", name)
    name <- gsub("https://[a-z]{2}.wikipedia.org/wiki/(.*)", "\\1", name)
  } else {
    if (startsWith(name, "/wiki/"))
      name <- substr(name, start = nchar("/wiki/") + 1, stop = nchar(name))
  }
  if (startsWith(name, "%"))
    name <- URLdecode(name)

  # set up HTTP query
  url <- paste0("https://", lang, ".wikipedia.org/w/api.php")
  params <- list(
    action = "query",
    prop = "langlinks",
    titles = name,
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
  if (lang == "en")
    result <- page$title
  else
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

# filter out players with no japanese page
idx <- which(players[, "debugComment"] == "No WP JA page")
tlog(2, "Removing players without a japanese WP page: ", length(idx), "/", nrow(players))
players <- players[-idx, ]
tlog(4, "Remaing players: ", nrow(players))

# normalize positions
tlog(2, "Normalize rugby positions")
all_positions <- players[, "positions"]
all_positions <- gsub("\\[\\d+\\]", "", all_positions, fixed = FALSE)
all_positions <- gsub("、", "; ", all_positions, fixed = TRUE)
all_positions <- strsplit(all_positions, "; ")
unique_positions <- sort(unique(trimws(unlist(all_positions))))
#table(unique_positions)
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
players[, "positions"] <- all_positions

# weights and heights are ok
#sort(unique(players[, "height"]))
#sort(unique(players[, "weight"]))

# birth and death dates are ok
#sort(unique(players[, "birthDate"]))
#sort(unique(players[, "deathDate"]))

# birth and death places
tlog(2, "Normalize birth and death places")
all_urls <- c(players[, "birthPlaceWP"], players[, "deathPlaceWP"])
all_urls <- strsplit(all_urls, "; ")
unique_urls <- sort(unique(trimws(unlist(all_urls))))
unique_urls <- unique_urls[!grepl("redlink=1", unique_urls, fixed = TRUE)]
unique_urls <- unique_urls[!startsWith(unique_urls, "#")]
# define conversion map for locations
tlog(4, "Building the conversion maps")
map_url <- c()
for (i in 1:length(unique_urls)) {
  unique_url <- unique_urls[i]
  tlog(6, "Retrieving translation for \"", unique_url, "\" (", i, "/", length(unique_urls), ")")
  title <- get_english_title(unique_url)
  tlog(8, "Result: ", title)
  if (is.null(title))
    title <- unique_url[i]
  map_url[unique_urls[i]] <- title
}
#which(is.na(map_url))
map_url["https://es.wikipedia.org/wiki/Puke_(Tonga)"] <- "Puke; Tonga"
map_url["https://to.wikipedia.org/wiki/Lapaha"] <- "Lapaha"
map_url["https://to.wikipedia.org/wiki/Matahau"] <- "Matahau"
map_url["https://to.wikipedia.org/wiki/Tofoa"] <- "Tofoa"
map_url["/wiki/Auckland"] <- "Auckland"
map_url["/wiki/%E3%82%A4%E3%83%BC%E3%82%B9%E3%83%88%E3%83%AD%E3%83%B3%E3%83%89%E3%83%B3"] <- "East London; South Africa"
map_url["/wiki/%E3%82%A6%E3%82%A3%E3%83%AB%E3%83%88%E3%82%B7%E3%83%A3%E3%83%BC%E5%B7%9E"] <- "Wiltshire"
map_url["/wiki/%E3%82%A6%E3%82%A7%E3%82%B9%E3%83%88%E3%83%A8%E3%83%BC%E3%82%AF%E3%82%B7%E3%83%A3%E3%83%BC"] <- "West Yorkshire"
map_url["/wiki/%E3%82%A6%E3%82%A9%E3%83%89%E3%83%B3%E3%82%AC"] <- "Albury; New South Wales"
map_url["/wiki/%E3%82%A8%E3%82%A2_(%E3%82%B5%E3%82%A6%E3%82%B9%E3%83%BB%E3%82%A8%E3%82%A2%E3%83%BC%E3%82%B7%E3%83%A3%E3%83%BC)"] <- "Ayr; South Ayrshire"
map_url["/wiki/%E3%82%AA%E3%83%81%E3%83%AF%E3%83%AD%E3%83%B3%E3%82%B4"] <- "Ottiwarongo"
map_url["/wiki/%E3%82%AB%E3%83%8A%E3%83%AA%E3%82%A2%E8%AB%B8%E5%B3%B6%E5%B7%9E"] <- "Canary Islands"
map_url["/wiki/%E3%82%AB%E3%83%AB%E3%83%95%E3%82%A9%E3%83%AB%E3%83%8B%E3%82%A2%E5%B7%9E"] <- "California"
map_url["/wiki/%E3%82%AB%E3%83%B3%E3%83%96%E3%83%AA%E3%82%A2%E5%B7%9E"] <- "Cumbria; England"
map_url["/wiki/%E3%82%AD%E3%82%A8%E3%83%95"] <- "Kyiv"
map_url["/wiki/%E3%82%AD%E3%83%AB%E3%83%87%E3%82%A2%E5%B7%9E"] <- "County Kildare"
map_url["/wiki/%E3%82%B0%E3%83%A9%E3%83%8F%E3%83%A0%E3%82%BA%E3%82%BF%E3%82%A6%E3%83%B3"] <- "Makanda"
map_url["/wiki/%E3%82%B0%E3%83%AD%E3%82%B9%E3%82%BF%E3%83%BC%E3%82%B7%E3%83%A3%E3%83%BC"] <- "Gloucestershire"
map_url["/wiki/%E3%82%B3%E3%83%BC%E3%83%B3%E3%82%A6%E3%82%A9%E3%83%BC%E3%83%AB%E5%B7%9E"] <- "Cornwall"
map_url["/wiki/%E3%82%B5%E3%83%95%E3%82%A9%E3%83%BC%E3%82%AF%E5%B7%9E"] <- "Suffolk"
map_url["/wiki/%E3%82%B5%E3%83%AA%E3%83%BC%E5%B7%9E"] <- "Surrey; England"
map_url["/wiki/%E3%82%B9%E3%82%BF%E3%83%83%E3%83%95%E3%82%A9%E3%83%BC%E3%83%89%E3%82%B7%E3%83%A3%E3%83%BC%E5%B7%9E"] <- "Staffordshire"
map_url["/wiki/%E3%82%BF%E3%83%A9%E3%83%8A%E3%82%AD"] <- "Taranaki Region"
map_url["/wiki/%E3%83%87%E3%83%B4%E3%82%A9%E3%83%B3%E5%B7%9E"] <- "Devon"
map_url["/wiki/%E3%83%88%E3%83%B3%E3%82%AC%E7%8E%8B%E5%9B%BD"] <- "Tonga"
map_url["/wiki/%E3%83%8B%E3%83%A5%E3%83%BC%E3%83%A8%E3%83%BC%E3%82%AF%E5%B8%82"] <- "New York City"
map_url["/wiki/%E3%83%8E%E3%83%BC%E3%83%95%E3%82%A9%E3%83%BC%E3%82%AF%E5%B7%9E"] <- "Norfolk"
map_url["/wiki/%E3%83%8F%E3%83%B3%E3%83%97%E3%82%B7%E3%83%A3%E3%83%BC%E5%B7%9E"] <- "Hampshire"
map_url["/wiki/%E3%83%93%E3%82%B8%E3%83%A3%E3%83%9B%E3%83%A8%E3%83%BC%E3%82%B5"] <- "Villa Joyosa"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Australia.svg"] <- "Australia"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Catalonia.svg"] <- "Catalonia"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_England.svg"] <- "England"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Fiji.svg"] <- "Fiji"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_France.svg"] <- "France"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Georgia.svg"] <- "Georgia"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Japan.svg"] <- "Japan"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_New_South_Wales.svg"] <- "New South Wales"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_New_Zealand.svg"] <- "New Zealand"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Occitania.svg"] <- "Occitania"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Russia.svg"] <- "Russia"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Samoa.svg"] <- "Samoa"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_South_Africa.svg"] <- "South Africa"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_the_region_Auvergne-Rh%C3%B4ne-Alpes.svg"] <- "Auvergne"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Tonga.svg"] <- "Tonga"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Wallis_and_Futuna.svg"] <- "Wallis and Futuna"
map_url["/wiki/%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB:Flag_of_Zimbabwe.svg"] <- "Zimbabwe"
map_url["/wiki/%E3%83%95%E3%83%A9%E3%82%A4%E3%83%96%E3%83%AB%E3%82%AF"] <- "Freiburg im Breisgau"
map_url["/wiki/%E3%83%96%E3%83%AA%E3%82%BA%E3%83%99%E3%83%B3"] <- "Brisbane"
map_url["/wiki/%E3%83%99%E3%82%B9%E3%83%AC%E3%83%98%E3%83%A0"] <- "Bethlehem"
map_url["/wiki/%E3%83%99%E3%83%AB%E3%83%B4%E3%82%A3%E3%83%AB"] <- "Belleville"
map_url["/wiki/%E3%83%A8%E3%83%BC%E3%82%AF%E3%82%B7%E3%83%A3%E3%83%BC%E5%B7%9E"] <- "Yorkshire"
map_url["/wiki/%E3%83%AD%E3%83%84%E3%83%9E%E5%B3%B6"] <- "Rotuma"
map_url["/wiki/%E4%BA%AC%E6%A9%8B%E5%8C%BA"] <- "Kyobashi Ward"
map_url["/wiki/%E4%BA%AC%E9%83%BD"] <- "Kyoto"
map_url["/wiki/%E5%8D%97%E3%82%A2%E3%83%95%E3%83%AA%E3%82%AB"] <- "South Africa"
map_url["/wiki/%E5%B0%8F%E7%9F%B3%E5%B7%9D%E5%8C%BA"] <- "Koishikawa Ward"
map_url["/wiki/%E5%B1%B1%E6%9D%B1%E7%9C%81_(%E6%B1%AA%E5%85%86%E9%8A%98%E6%94%BF%E6%A8%A9)"] <- "Shandong Province"
# translation map
map_ja <- c()
map_ja["アイルシャム"] <- "Aylsham"
map_ja["アシュフォード"] <- "Ashford"
map_ja["アスコット"] <- "Ascot"
map_ja["アッパーハット"] <- "Upper Hutt"
map_ja["アディ"] <- "Adi"
map_ja["アデレード"] <- "Adelaide"
map_ja["アリワルノース"] <- "Aliwal North"
map_ja["アロア"] <- "Alois"
map_ja["イーズタウン"] <- "Easttown"
map_ja["イースト・デレハム"] <- "East Dereham"
map_ja["イニシュモア"] <- "Inishmore"
map_ja["ヴァイオラ"] <- "Viola"
map_ja["ウェリントン"] <- "Wellington"
map_ja["ヴライプルク"] <- "Vrijpurg"
map_ja["ヴリヘイド"] <- "Vryheid"
map_ja["エシャンゼニ郡"] <- "Echanzenie County"
map_ja["エルビセーニ"] <- "Elviseny"
map_ja["エンコボ"] <- "Encobo"
map_ja["エンパンゲニ"] <- "Empangeni"
map_ja["オノ・イ・ラウ"] <- "Ono-i-Lau"
map_ja["オポティキ"] <- "Opotiki"
map_ja["オマルル"] <- "Omaruru"
map_ja["カールトンヴィル"] <- "Carltonville"
map_ja["カールトンビル"] <- "Carlton Building"
map_ja["ガウ島"] <- "Gau Island"
map_ja["カメンカ"] <- "Kamenka"
map_ja["ガラシエル"] <- "Galaciel"
map_ja["カワカワ"] <- "Kawakawa"
map_ja["カンボーン"] <- "Camborne"
map_ja["キラロー"] <- "Killaloe"
map_ja["クウィーンビアン"] <- "Queenbian"
map_ja["グウォイン＝カー＝ゲーワン"] <- "Gwoyn Kerr Gewan"
map_ja["グラ・フモルルイ"] <- "Gura Fumorrui"
map_ja["クライガボン"] <- "Craigavon"
map_ja["クリーブランド"] <- "Cleveland"
map_ja["クルーガーズドープ"] <- "Krugersdorp"
map_ja["グレイヴズエンド"] <- "Gravesend"
map_ja["クレイガヴォン"] <- "Craigavon"
map_ja["グロデニ"] <- "Glodeni"
map_ja["クワリンダイ"] <- "Kuwarindai"
map_ja["ケンブリッジ"] <- "Cambridge"
map_ja["ゴア"] <- "Goa"
map_ja["ゴーセイノン"] <- "Gorseinon"
map_ja["ゴーリー"] <- "Gorey"
map_ja["コスティネシュティ"] <- "Costinésti"
map_ja["ゴッドマンチェスター"] <- "Godmanchester"
map_ja["コロボウ"] <- "Corobou"
map_ja["コンセプションベイサウス"] <- "Conception Bay South"
map_ja["サウスオークランド"] <- "South Auckland"
map_ja["サウスポート"] <- "Southport"
map_ja["サマセット・ウェスト"] <- "Somerset West"
map_ja["サマセットウェスト"] <- "Somerset West"
map_ja["サン＝ペ＝シュル＝ニヴェル"] <- "Saint-Pé-sur-Nivelle"
map_ja["シドカップ"] <- "Sidcup"
map_ja["ジモン"] <- "Jimon"
map_ja["シャビオット"] <- "Chaviot"
map_ja["スコーン"] <- "Scone"
map_ja["スタンダートン"] <- "Standerton"
map_ja["スティームボートスプリングス"] <- "Steamboat Springs"
map_ja["スティッツビル"] <- "Stittsville"
map_ja["スプリングウッド"] <- "Springwood"
map_ja["セール"] <- "Sale"
map_ja["セロ・カムビレエフスコエ"] <- "Selo-Kambileyevskoye"
map_ja["セントアルバート"] <- "St. Albert"
map_ja["ソロカ"] <- "Soroka"
map_ja["ダーリンハースト"] <- "Darlinghurst"
map_ja["ダイサート"] <- "Dysart"
map_ja["タイハペ"] <- "Taihape"
map_ja["タカプナ"] <- "Takapuna"
map_ja["ダコラム"] <- "Dacolum"
map_ja["タワフラット"] <- "Tower Flat"
map_ja["ダンカン"] <- "Duncan"
map_ja["チャーチ・ヴィレッジ"] <- "Church Village"
map_ja["チョーリー"] <- "Chorley"
map_ja["ティペラリー"] <- "Tipperary"
map_ja["デュースベリー"] <- "Dewsbury"
map_ja["トコロア"] <- "Tokoroa"
map_ja["トフォア"] <- "Tofoa"
map_ja["トレオルヒ"] <- "Treorch"
map_ja["トレバノス"] <- "Trebanos"
map_ja["ナイジェル"] <- "Nigel"
map_ja["ナイタシリ"] <- "Naitasiri"
map_ja["ナヴニソール"] <- "Navnisor"
map_ja["ナチュヴァ"] <- "Natuvava"
map_ja["ナディ"] <- "Nadi"
map_ja["ナドロガ＝ナヴォサ"] <- "Nadroga-Navsa"
map_ja["ナドロガ＝ナヴサ"] <- "Nadroga-Navsa"
map_ja["ナブア"] <- "Navua"
map_ja["ニュー・スムナー・ビーチ"] <- "New Summer Beach"
map_ja["ヌクイラウ"] <- "Nukuilau"
map_ja["ネグリレシュティ"] <- "Negrireshti"
map_ja["ノースハーバー"] <- "North Harbor"
map_ja["ノッティングリー"] <- "Nottingley"
map_ja["パークス"] <- "Parks"
map_ja["ハイデルベルク"] <- "Heidelberg"
map_ja["バデリム"] <- "Buderim"
map_ja["パパクラ"] <- "Papakura"
map_ja["パラパラウム"] <- "Paraparaumu"
map_ja["バリーナヒンチ"] <- "Ballynahinch"
map_ja["バリナ"] <- "Barina"
map_ja["ハンターズビル"] <- "Huntersville"
map_ja["ピーブルス"] <- "Peebles"
map_ja["ビヴォラリ"] <- "Vivolari"
map_ja["ヒューマンズドロップ"] <- "Human's Drop"
map_ja["ファレアシウ"] <- "Faleaciu"
map_ja["フィックスブルグ"] <- "Ficksburg"
map_ja["フェアフィールド"] <- "Fairfield"
map_ja["フォーブス"] <- "Forbes"
map_ja["プケ"] <- "Puke"
map_ja["ブシア"] <- "Busia"
map_ja["ブライアンストン"] <- "Bryanston"
map_ja["ブラクパン"] <- "Brakpan"
map_ja["ブラックロック"] <- "Black Rock"
map_ja["プリサカーニ"] <- "Purisakani"
map_ja["ブリッジ・オブ・アーラン"] <- "Bridge of Allan"
map_ja["ブリッツ"] <- "Blitz"
map_ja["ブルラド"] <- "Burlad"
map_ja["プレストン"] <- "Preston"
map_ja["プロサーパイン"] <- "Proserpine"
map_ja["ベチャル"] <- "Bechar"
map_ja["ベツレヘム"] <- "Bethlehem"
map_ja["ペナース"] <- "Penarth"
map_ja["ベルビル"] <- "Belleville"
map_ja["ペンリス"] <- "Penrith"
map_ja["ホウマ"] <- "Houma"
map_ja["ポリルア"] <- "Porirua"
map_ja["ポンタプリズ"] <- "Pontapriz"
map_ja["マームズブリー"] <- "Malmesbury"
map_ja["マエシーコード"] <- "Maeshikodo"
map_ja["マガリスブルク"] <- "Magalisburg"
map_ja["マクレーン"] <- "McClain"
map_ja["マシロン"] <- "Massillon"
map_ja["マタハウ"] <- "Matahau"
map_ja["マリナ"] <- "Marina"
map_ja["マンリー"] <- "Manly"
map_ja["ミント"] <- "Minto"
map_ja["ムーレーズバーグ"] <- "Mooresburg"
map_ja["ムダンタン"] <- "Mudantan"
map_ja["モディアディスクルーフ"] <- "Modiadisukurufu"
map_ja["モトゥーチュア"] <- "Motuture"
map_ja["モトゥエカ"] <- "Motueka"
map_ja["モトゥツア"] <- "Mototua"
map_ja["モトオチュア"] <- "Motoochua"
map_ja["モトトゥア"] <- "Mototua"
map_ja["モリストン"] <- "Morriston"
map_ja["モンダベザン"] <- "Mondabesan"
map_ja["ヤドゥア"] <- "Yadua"
map_ja["ユニオンデール"] <- "Uniondale"
map_ja["ヨール"] <- "Youghal"
map_ja["ラーガン"] <- "Lurgan"
map_ja["ラウトア"] <- "Lautoa"
map_ja["ラウ諸島"] <- "Lau Island"
map_ja["ラザーグレン"] <- "Rutherglen"
map_ja["ラス・コンデス"] <- "Las Condes"
map_ja["ラヌメザン"] <- "Lanemezan"
map_ja["ラパハ"] <- "Rapaha"
map_ja["ラファエラ"] <- "Raffaella"
map_ja["ランドヴァリー"] <- "Randvalley"
map_ja["ランドウィック"] <- "Randwick"
map_ja["ランドー"] <- "Landau"
map_ja["ラントリサント"] <- "Llantrisant"
map_ja["ランビット・ファドレ"] <- "Lambit"
map_ja["リスル＝アダン"] <- "L'Isle-Adan"
map_ja["リンデン"] <- "Linden"
map_ja["ルフィノ"] <- "Rufino"
map_ja["ルラントリセント"] <- "Lelan Tricent"
map_ja["レッドヒル"] <- "Red Hill"
map_ja["レファラレ"] <- "Referrale"
map_ja["レホボス"] <- "Rehoboth"
map_ja["ロイヤル・タンブリッジ・ウェルズ"] <- "Royal Tunbridge Wells"
map_ja["ローデポールト"] <- "Rohdepoort"
map_ja["ロードポート"] <- "Rohdepoort"
map_ja["ロンゴテメ"] <- "Longoteme"
map_ja["奈良県葛城市"] <- "Katsuragi"
map_ja["福岡県太宰府市"] <- "Dazaifu"
map_ja["ǁKaras Region"] <- "Karas Region"
# clean locations
tlog(4, "Substituting in the table")
cols <- c("birthPlace", "deathPlace")
for (col in cols) {
  tlog(6, "Normalizing \"", col, "\"")
  all_places <- players[, col]
  all_places <- gsub("\\[.+\\]", "", all_places, fixed = FALSE)
  all_places <- strsplit(all_places, "; ")

  for (p in 1:length(all_places)) {
    places <- all_places[[p]]
    urls <- all_urls[[p]]

    if (length(places) == 0) {
      places <- " "
    } else {
      # normalize place names
      for (url in names(map_url))
        places[urls == url] <- map_url[url]

      # translate remaining names
      for (ja_name in names(map_ja))
        places[places == ja_name] <- map_ja[ja_name]

      # remove duplicates
      places <- gsub(", ?", "; ", places, fixed = FALSE)
      places <- unique(unlist(strsplit(places, "; ")))
    }

    # update list
    all_places[[p]] <- places
  }
  # collapse to get strings again
  all_places <- sapply(all_places, function(places) paste0(places, collapse = "; "))
  all_places[all_places == "NA"] <- NA
  names(all_places) <- NULL
  players[, col] <- all_places
}
all_places <- c(players[, "birthPlace"], players[, "deathPlace"])
all_places <- gsub("\\[.+\\]", "", all_places, fixed = FALSE)
all_places <- strsplit(all_places, "; ")
all_place <- sort(unique(unlist(all_places)))

# rename certain columns
tlog(2, "Rename certain columns")
col <- which(colnames(players) == "origWdId")
colnames(players)[col] <- "wikidataId"
col <- which(colnames(players) == "origName")
colnames(players)[col] <- "fullName"

# remove superfluous columns
sup_cols <- c("jaName", "debugComment", "wpPage", "currentTeam", "birthPlaceWP", "deathPlaceWP")
tlog(2, "Remove superfluous columns: ", paste0(sup_cols, collapse = ", "))
cols <- which(colnames(players) %in% sup_cols)
players <- players[, -cols]




########################################################################
# clean the career table
tlog(0, "Cleaning the career table")

# fix some specific cases
careers <- data.frame(lapply(careers, function(col) gsub(";([^ ])", "; \\1", col, fixed = FALSE)))
careers <- data.frame(lapply(careers, function(col) gsub(";$", "; ", col, fixed = FALSE)))
careers <- data.frame(lapply(careers, function(col) gsub("\\[\\d+\\]", "", col, fixed = FALSE)))
careers <- data.frame(lapply(careers, function(col) gsub(",;", ", ;", col, fixed = TRUE)))
careers <- data.frame(lapply(careers, function(col) gsub("; （No.370）", "", col, fixed = TRUE)))
careers <- data.frame(lapply(careers, function(col) gsub("–", "-", col, fixed = TRUE)))

# add columns for start/end years
careers <- cbind(careers, matrix(NA, nrow = nrow(careers), ncol = 2))
colnames(careers)[(ncol(careers) - 1):ncol(careers)] <- c("startYear", "endYear")

# split rows containing multiple steps
new_careers <- careers[-(1:nrow(careers)), ]
err <- c()
for (r in 1:nrow(careers)) {
  tlog(4, "Processing row ", r, "/", nrow(careers))

  periods <- careers[r, "timePeriod"]
  team_names <- careers[r, "teamName"]
  team_urls <- careers[r, "teamWP"]
  matches_played <- careers[r, "matchesPlayed"]
  points_scored <- careers[r, "pointsScored"]

  # try to split row by semicolon, same number of parts for each field (unless totally empty)
  ll <- 0
  if (is.na(periods) || periods == "") {
    periods <- NA
  } else {
    periods <- trimws(strsplit(periods, ";")[[1]])
    ll <- length(periods)
  }
  if (is.na(team_names) || team_names == "") {
    team_names <- NA
  } else {
    team_names <- trimws(strsplit(team_names, ";")[[1]])
    if (ll > 0 && length(team_names) != ll) {
      err <- union(err,  careers[r, "wpPage"])
      tlog(6, "Error (team_names): ", paste0(careers[r, ], collapse = ", "))
    } else
      ll <- length(team_names)
  }
  if (is.na(team_urls) || team_urls == "") {
    team_urls <- NA
  } else {
    team_urls <- trimws(strsplit(team_urls, ";")[[1]])
    if (ll > 0 && length(team_urls) != ll) {
      err <- union(err,  careers[r, "wpPage"])
      tlog(6, "Error (team_urls): ", paste0(careers[r, ], collapse = ", "))
    } else
      ll <- length(team_urls)
  }
  if (is.na(matches_played) || matches_played == "") {
    matches_played <- NA
  } else {
    matches_played <- trimws(strsplit(matches_played, ";")[[1]])
    if (ll > 0 && length(matches_played) != ll) {
      err <- union(err,  careers[r, "wpPage"])
      tlog(6, "Error (matches_played): ", paste0(careers[r, ], collapse = ", "))
    } else
      ll <- length(matches_played)
  }
  if (is.na(points_scored) || points_scored == "") {
    points_scored <- NA
  } else {
    points_scored <- trimws(strsplit(points_scored, ";")[[1]])
    if (ll > 0 && length(points_scored) != ll) {
      err <- union(err,  careers[r, "wpPage"])
      tlog(6, "Error (pointsScored): ", paste0(careers[r, ], collapse = ", "))
    }
  }

  # processing each part of the original row (possibly a single one)
  for (i in 1:ll) {
    # if no period information, nothing special to do
    if (all(is.na(periods)) || is.na(periods[i]) || periods[i] == "") {
      periods_sep <- NA
      start_years <- NA
      end_years <- NA
      years <- NA

    # otherwise, try to split row by comma
    } else {
      periods_sep <- strsplit(periods[i], ",")[[1]]
      # compute the number of years
      years <- c()
      start_years <- c()
      end_years <- c()
      for (p in 1:length(periods_sep)) {
        per <- periods_sep[p]
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
        periods_sep[p] <- per
      }
    }

    # init rows
    new_rows <- careers[rep(r, length(periods_sep)), ]
    new_rows[, "timePeriod"] <- periods_sep
    new_rows[, "teamName"] <- rep(team_names[i], length(periods_sep))
    new_rows[, "teamWP"] <- rep(team_urls[i], length(periods_sep))
    new_rows[, "startYear"] <- start_years
    new_rows[, "endYear"] <- end_years

    # adjust stats based on number of years
    if (!is.na(matches_played[i]))
      new_rows[, "matchesPlayed"] <- round(as.integer(matches_played[i]) * years / sum(years))
    if (!is.na(points_scored[i]))
      new_rows[, "pointsScored"] <- round(as.integer(points_scored[i]) * years / sum(years))

    # add rows to table
    new_careers <- rbind(new_careers, new_rows)
  }
}
#options(warn = 0)

# clean team urls
idx <- which(grepl("action=edit&redlink=1", new_careers[, "teamWP"], fixed = FALSE))
new_careers[idx, "teamWP"] <- NA
#tail(sort(unique(new_careers[, "teamWP"])))




########################################################################
# record all three cleaned tables as new files

# record player table
tab_file <- file.path(folder, "players.csv")
tlog(2, "Record player table as: ", tab_file)
write.csv(players, tab_file, row.names = FALSE, fileEncoding = "UTF-8")

# record career table
tab_file <- file.path(folder, "careers.csv")
tlog(2, "Record career table as: ", tab_file)
write.csv(new_careers, tab_file, row.names = FALSE, fileEncoding = "UTF-8")
