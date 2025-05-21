########################################################################
# Functions used to clean certain data fields from the DBpedia tables.
#
# Vincent Labatut
# 01/2025
########################################################################




########################################################################
# Takes the player table and returns a normalized column representing
# their rugby positions (possibly mutliple positions by player).
#
# players: player table.
# 
# returns: vector of strings, one for each player in the table.
########################################################################
get_clean_positions <- function(players) {
  all_positions <- players[, "positions"]

  # possibly split multiple values
  all_positions <- strsplit(all_positions, "; ")

  # see all existing values
  # table(unlist(all_positions))

  # RUGBY POSITIONS
  #####
  # Forward
  # ├─ 1st Row = Front Row
  # │  ├─ Prop
  # │  │  ├─ Loosehead Prop
  # │  │  └─ Tighthead Prop
  # │  └─ Hooker
  # ├─ 2nd Row = Lock
  # │  ├─ Loosehead Lock
  # │  └─ Tighthead Lock
  # └─ 3rd Row = Back Row = Loose Forward
  #    ├─ Flanker = Wing Forward = Third-line Wing
  #    │  ├─ Openside Flanker
  #    │  └─ Blindside Flanker
  #    └─ Number 8 = Eightman
  # Back
  # ├─ Half-Back
  # │  ├─ Scrum-Half = Inside-Half = Inside Left
  # │  └─ Fly-Half = First Five-Eighth = Outside Half = Out-Half = Stand-Off
  # ├─ Three-Quarter
  # │  ├─ Centre
  # │  │  ├─ Inside Centre = Inside Back = Second Five-Eighth
  # │  │  └─ Outside Centre = Outside Back = Centre Three-Quarter = Outside Half
  # │  └─ Winger = Wing Three Quarter
  # │     ├─ Left Winger = Left Back
  # │     └─ Right Winger = Right Back
  # └─ Fullback

  # define conversion map
  map <- c()
  map[" Centre"] <- "Centre"
  map[" Lock"] <- "Lock"
  map["2nd row"] <- "Lock"
  map["8"] <- "Number 8"
  map["Assistant Coach-Defence"] <- ""
  map["Attack Coach"] <- ""
  map["back"] <- "Back"
  map["Back"] <- "Back"
  map["back-row"] <- "3rd Row"
  map["Back-row"] <- "3rd Row"
  map["Back-row forward"] <- "3rd Row"
  map["back row"] <- "3rd Row"
  map["Back row"] <- "3rd Row"
  map["Back Row"] <- "3rd Row"
  map["Back row forward"] <- "3rd Row"
  map["Back Row Forward"] <- "3rd Row"
  map["Backrow"] <- "3rd Row"
  map["Blindside"] <- "Blindside Flanker"
  map["Blindside flanker"] <- "Blindside Flanker"
  map["Blindside Flanker"] <- "Blindside Flanker"
  map["Center"] <- "Centre"
  map["centre"] <- "Centre"
  map["Centre"] <- "Centre"
  map["Centre ;"] <- "Centre"
  map["Centre Fly-Half"] <- "Centre; Fly-Half"
  map["centre three-quarter"] <- "Outside Centre"
  map["centro"] <- "Centre"
  map["coach"] <- ""
  map["Coach"] <- ""
  map["Director of Rugby"] <- ""
  map["eighthman"] <- "Number 8"
  map["Eighthman"] <- "Number 8"
  map["First-Five"] <- "Fly-Half"
  map["First five-eight"] <- "Fly-Half"
  map["First five-eighth"] <- "Fly-Half"
  map["Five-eighth"] <- "Fly-Half; Inside Centre"
  map["Five-Eighth"] <- "Fly-Half; Inside Centre"
  map["Flank"] <- "Flanker"
  map["flanker"] <- "Flanker"
  map["Flanker"] <- "Flanker"
  map["Flanker #8"] <- "Flanker; Number 8"
  map["Flanker (rugby)"] <- "Flanker"
  map["Flanker,hooker"] <- "Flanker; Hooker"
  map["fly-half"] <- "Fly-Half"
  map["Fly-half"] <- "Fly-Half"
  map["Fly-Half"] <- "Fly-Half"
  map["fly-half Five-eighth & halfback"] <- "Fly-Half; Scrum-Half"
  map["Fly-half;"] <- "Fly-Half"
  map["fly half"] <- "Fly-Half"
  map["Fly half"] <- "Fly-Half"
  map["Fly Half"] <- "Fly-Half"
  map["flyhalf"] <- "Fly-Half"
  map["Flyhalf"] <- "Fly-Half"
  map["forward"] <- "Forward"
  map["Forward"] <- "Forward"
  map["Forward (association football)"] <- "Forward"
  map["Forwards"] <- "Forward"
  map["Front-row"] <- "1st Row"
  map["Front-rower"] <- "1st Row"
  map["Front line"] <- "1st Row"
  map["front row"] <- "1st Row"
  map["Front row"] <- "1st Row"
  map["Front Row"] <- "1st Row"
  map["full-back"] <- "Fullback"
  map["Full-back"] <- "Fullback"
  map["Full-Back"] <- "Fullback"
  map["full back"] <- "Fullback"
  map["Full back"] <- "Fullback"
  map["Full Back"] <- "Fullback"
  map["fullback"] <- "Fullback"
  map["Fullback"] <- "Fullback"
  map["Fullback (Rugby union)"] <- "Fullback"
  map["Fullback centre"] <- "Fullback; Centre"
  map["Fullback rugby union"] <- "Fullback"
  map["Half"] <- "Half-Back"
  map["half-back"] <- "Half-Back"
  map["Half-back"] <- "Half-Back"
  map["Half-Back"] <- "Half-Back"
  map["Half–back"] <- "Half-Back"
  map["halfback"] <- "Half-Back"
  map["Halfback"] <- "Half-Back"
  map["Head Coach"] <- ""
  map["hooker"] <- "Hooker"
  map["Hooker"] <- "Hooker"
  map["Hooker & Prop"] <- "Hooker; Prop"
  map["Hooker for NSW U19"] <- "Hooker"
  map["HookerBack row"] <- "Hooker; 3rd Row"
  map["Inside Back"] <- "Inside Centre"
  map["Inside center"] <- "Inside Centre"
  map["Inside Center"] <- "Inside Centre"
  map["Inside centre"] <- "Inside Centre"
  map["Inside Centre"] <- "Inside Centre"
  map["Left wing"] <- "Left Winger"
  map["LH"] <- "Loosehead Prop"
  map["lock"] <- "Lock"
  map["Lock"] <- "Lock"
  map["Lock (Rugby Union)"] <- "Lock"
  map["Lock forward"] <- "Lock"
  map["Lock Forward"] <- "Lock"
  map["loose forward"] <- "3rd Row"
  map["Loose forward"] <- "3rd Row"
  map["Loose Forward"] <- "3rd Row"
  map["Loose Forward,Centres"] <- "3rd Row; Centre"
  map["Loose head"] <- "Loosehead Prop"
  map["Loose Head Prop"] <- "Loosehead Prop"
  map["Loosehead"] <- "Loosehead Prop"
  map["Loosehead prop"] <- "Loosehead Prop"
  map["Loosehead Prop"] <- "Loosehead Prop"
  map["Midfield"] <- "Fly-Half; Centre"
  map["No 8"] <- "Number 8"
  map["No 8."] <- "Number 8"
  map["No. 8"] <- "Number 8"
  map["No.8"] <- "Number 8"
  map["No8"] <- "Number 8"
  map["Number 8"] <- "Number 8"
  map["number eight"] <- "Number 8"
  map["Number eight"] <- "Number 8"
  map["Number Eight"] <- "Number 8"
  map["number8"] <- "Number 8"
  map["Open Side Flanker"] <- "Openside Flanker"
  map["Openside flanker"] <- "Openside Flanker"
  map["Openside Flanker"] <- "Openside Flanker"
  map["Out-half"] <- "Fly-Half"
  map["Out-Half"] <- "Fly-Half"
  map["Out half"] <- "Fly-Half"
  map["Outside back"] <- "Outside Centre"
  map["Outside Back"] <- "Outside Centre"
  map["Outside backs"] <- "Outside Centre"
  map["Outside center"] <- "Outside Centre"
  map["Outside Center"] <- "Outside Centre"
  map["Outside centre"] <- "Outside Centre"
  map["Outside Centre"] <- "Outside Centre"
  map["Outside Half"] <- "Outside Centre"
  map["prop"] <- "Prop"
  map["Prop"] <- "Prop"
  map["prop forward"] <- "Prop"
  map["Prop forward"] <- "Prop"
  map["Prop Forward"] <- "Prop"
  map["Referee"] <- ""
  map["Right wing"] <- "Right Winger"
  map["Right Wing positions"] <- "Right Winger"
  map["Rugby league positions"] <- ""
  map["Rugby union positions"] <- ""
  map["Rugby union positions First five-eight"] <- "Fly-Half"
  map["scrum-half"] <- "Scrum-Half"
  map["Scrum-half"] <- "Scrum-Half"
  map["Scrum-Half"] <- "Scrum-Half"
  map["scrum-half Fly-half"] <- "Scrum-Half; Fly-Half"
  map["scrum half"] <- "Scrum-Half"
  map["Scrum half"] <- "Scrum-Half"
  map["Scrum Half"] <- "Scrum-Half"
  map["scrumhalf"] <- "Scrum-Half"
  map["Scrumhalf"] <- "Scrum-Half"
  map["Second-row"] <- "Lock"
  map["Second-row forward"] <- "Lock"
  map["Second-rower"] <- "Lock"
  map["Second five"] <- "Inside Centre"
  map["Second five-eighth"] <- "Inside Centre"
  map["Second Five Eighth"] <- "Inside Centre"
  map["Second row"] <- "Lock"
  map["Second Row"] <- "Lock"
  map["Second Row Forward"] <- "Lock"
  map["Shute Shield"] <- ""
  map["Stand-off"] <- "Fly-Half"
  map["Stand-off half"] <- "Fly-Half"
  map["Stand off"] <- "Fly-Half"
  map["Standoff"] <- "Fly-Half"
  map["TH Prop"] <- "Tighthead Prop"
  map["Third line wing"] <- "3rd Row; Winger"
  map["Third row"] <- "3rd Row"
  map["Thomas Ellison"] <- ""
  map["three-quarter"] <- "Three–Quarter"
  map["Three-quarter"] <- "Three–Quarter"
  map["Three-quarter back"] <- "Three–Quarter"
  map["Three-quarter back – centre"] <- "Three–Quarter; Centre"
  map["three-quarters"] <- "Three–Quarter"
  map["Three-quarters"] <- "Three–Quarter"
  map["Three–quarter"] <- "Three–Quarter"
  map["Three Quarter"] <- "Three–Quarter"
  map["Threequarters"] <- "Three–Quarter"
  map["Tight head prop"] <- "Tighthead Prop"
  map["Tight head Prop"] <- "Tighthead Prop"
  map["Tight Head Prop"] <- "Tighthead Prop"
  map["tighthead prop"] <- "Tighthead Prop"
  map["Tighthead prop"] <- "Tighthead Prop"
  map["Tighthead Prop"] <- "Tighthead Prop"
  map["U21,"] <- ""
  map["Utility"] <- "Utility"
  map["utility back"] <- "Utility Back"
  map["Utility back"] <- "Utility Back"
  map["Utility Back"] <- "Utility Back"
  map["Utility forward"] <- "Utility Forward"
  map["Utility player"] <- "Utility"
  map["Versatile back"] <- "Utility Back"
  map["wing"] <- "Winger"
  map["Wing"] <- "Winger"
  map["Wing-forward"] <- "Flanker"
  map["Wing forward"] <- "Flanker"
  map["Wing Forward"] <- "Flanker"
  map["Wing three-quarter"] <- "Winger"
  map["Wing Three Quarter"] <- "Winger"
  map["wing threequarter"] <- "Winger"
  map["Wing,"] <- "Winger"
  map["Wing,centre"] <- "Winger; Centre"
  map["wing1906."] <- "Winger"
  map["WingCentre"] <- "Winger; Centre"
  map["winger"] <- "Winger"
  map["Winger"] <- "Winger"

  # convert positions
  for (p in 1:length(all_positions)) {
    positions <- all_positions[[p]]

    # normalize rugby positions
    for (position in names(map))
      positions[positions == position] <- map[position]

    # remove positions from other sports
    idx <- which(!(positions %in% setdiff(map, "")))
    if (length(idx) > 0)
      positions <- positions[-idx]
    if (length(positions) == 0)
      positions <- NA

    all_positions[[p]] <- positions
  }

  # collapse to get strings again
  result <- sapply(all_positions, function(positions) if(all(is.na(positions))) NA else paste0(positions, collapse = "; "))
  result[result == "NA"] <- NA
  return(result)
}
