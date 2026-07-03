########################################################################
# Rugby positions
########################################################################
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
# │  │  ├─ Inside Centre = Inside Back = Second Five-Eighth = Left Centre
# │  │  └─ Outside Centre = Outside Back = Centre Three-Quarter = Outside Half = Right Centre # nolint
# │  └─ Winger = Wing Three Quarter
# │     ├─ Left Winger = Left Back
# │     └─ Right Winger = Right Back
# └─ Fullback
########################################################################




########################################################################
# Aggregates positions depending on the specified granularity.
#
# positions: vector of original positions.
# granularity: level of granularity.
#
# returns: modified vector of positions.
########################################################################
aggregate_positions <- function (positions, granularity) {
  positions2 <- positions

  if (granularity >= 1) {
    top_positions <- c(
      "Loosehead Prop", "Tighthead Prop", "Hooker",
      "Loosehead Lock", "Tighthead Lock",
      "Openside Flanker", "Blindside Flanker",
      "Number 8",
      "Scrum-Half", "Fly-Half",
      "Inside Centre", "Outside Centre",
      "Left Winger", "Right Winger",
      "Fullback"
    )
  }

  if (granularity >= 2) {
    top_positions <- c(
      "Prop", "Hooker",
      "Loosehead Lock", "Tighthead Lock",
      "Flanker", "Number 8",
      "Scrum-Half", "Fly-Half",
      "Centre", "Winger", "Fullback"
    )
    map <- c(
      "Loosehead Prop" = "Prop",
      "Tighthead Prop" = "Prop",
      "Openside Flanker" = "Flanker",
      "Blindside Flanker" = "Flanker",
      "Inside Centre" = "Centre",
      "Outside Centre" = "Centre",
      "Left Winger" = "Winger",
      "Right Winger" = "Winger"
    )
    idx <- which(positions2 %in% names(map))
    positions2[idx] <- map[positions2[idx]]
  }

  if (granularity >= 3) {
    top_positions <- c(
      "1st Row", "2nd Row", "3rd Row",
      "Half-Back", "Three-Quarter", "Fullback"
    )
    map <- c(
      "Prop" = "1st Row",
      "Hooker" = "1st Row",
      "Loosehead Lock" = "2nd Row",
      "Tighthead Lock" = "2nd Row",
      "Flanker" = "3rd Row",
      "Number 8" = "3rd Row",
      "Scrum-Half" = "Half-Back",
      "Fly-Half" = "Half-Back",
      "Centre" = "Three-Quarter",
      "Winger" = "Three-Quarter"
    )
    idx <- which(positions2 %in% names(map))
    positions2[idx] <- map[positions2[idx]]
  }

  if (granularity >= 4) {
    top_positions <- c("Forward", "Back")
    map <- c(
      "1st Row" = "Forward",
      "2nd Row" = "Forward",
      "3rd Row" = "Forward",
      "Half-Back" = "Back",
      "Half-Back" = "Back",
      "Three-Quarter" = "Back"
    )
    idx <- which(positions2 %in% names(map))
    positions2[idx] <- map[positions2[idx]]
  }

  result <- list(positions = positions2, top_positions = top_positions)
  return(result)
}
