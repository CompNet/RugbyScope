########################################################################
# Generates various plots regarding the stints table, without
# considering time.
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/stats/global_stints.R")
########################################################################
library("dplyr")
library("UpSetR")
library("circlize")
library("corrplot")
#library("pheatmap")
library("viridis")




source("src/common/logging.R")
source("src/common/colors.R")




########################################################################
# start logging
start.rec.log("GlobalStints")




########################################################################
# create output folder
stats_folder <- file.path("data", "stats", "global_stints")
dir.create(stats_folder, showWarnings = FALSE, recursive = TRUE)

# load tables
source("src/stats/load_all_tables.R")




########################################################################
# distribution of start years

start_dates <- stints[, "startYear"]
start_dates <- start_dates[!is.na(start_dates)]

plot_file <- file.path(stats_folder, paste0("start-years", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  hist(start_dates,
    main = NA,
    xlab = "Stint start year",
    col = "red",
    breaks = 30,
    las = 2
  )
dev.off()




########################################################################
# distribution of end years

end_dates <- stints[, "endYear"]
end_dates <- end_dates[!is.na(end_dates)]

plot_file <- file.path(stats_folder, paste0("end-years", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  hist(end_dates,
    main = NA,
    xlab = "Stint end year",
    col = "red",
    breaks = 30,
    las = 2
  )
dev.off()




########################################################################
# distribution of types

# retrieve types
types <- stints[, "type"]

# count values
types_tt <- table(types, useNA = "always")
print(types_tt)

# focus on most frequent values
types_tt0 <- sort(table(types, useNA = "no"), decreasing = TRUE)
top_types <- names(types_tt0)

# remove NAs
types_tt2 <- types_tt[!is.na(names(types_tt))]

# set colors
color_palette <- get.palette(values = length(top_types))
names(color_palette) <- top_types

# generate barplot
plot_file <- file.path(stats_folder, paste0("types", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  barplot(
    height = types_tt2[top_types],
    names.arg = top_types,
    #xlab = "Stint types",
    legend = FALSE,
    #las = 2, log = "y",
    col = color_palette[top_types]
  )
dev.off()




########################################################################
# distribution of data sources
source_names <- c("enWP", "esWP", "frWP", "itWP", "jaWP", "WD")

# get data source info
data_sources <- c()
data_sources_df <- matrix(0, nrow = nrow(stints), ncol = length(source_names), dimnames = list(c(), source_names))
for (s in 1:nrow(stints)) {
  if (s %% 1000 == 0)
    tlog(2, "Processing stint ", s, "/", nrow(stints))

  # get data source list
  lst <- strsplit(stints[s, "dataSources"], split = ";")
  for (source in lst[[1]])
    data_sources_df[t, trimws(source)] <- 1
  stint_data_sources <- trimws(lst[[1]])

  # add to stat list
  data_sources <- c(data_sources, stint_data_sources)
}

# count values
sources_tt <- table(data_sources, useNA = "always")
print(sources_tt)

# focus on most frequent values
sources_tt0 <- sort(table(data_sources, useNA = "no"), decreasing = TRUE)
top_data_sources <- names(sources_tt0)

# remove NAs
sources_tt2 <- sources_tt[!is.na(names(sources_tt))]

# set colors
color_palette <- DATASOURCE_COLORS

# generate barplot
plot_file <- file.path(stats_folder, paste0("data-sources_barplot", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  barplot(
    height = sources_tt2[top_data_sources],
    names.arg = top_data_sources,
    #xlab = "Stint data sources",
    legend = FALSE,
    las = 2,
    col = color_palette[top_data_sources]
  )
dev.off()

# generate up-set diagram
plot_file <- file.path(stats_folder, paste0("data-sources_up-set", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  upset(as.data.frame(data_sources_df), sets = source_names)
dev.off()

# generate chordal diagram
overlap <- t(data_sources_df) %*% data_sources_df
diag(overlap) <- 0
#
plot_file <- file.path(stats_folder, paste0("data-sources_chord-diag", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  chordDiagram(overlap, grid.col = color_palette[source_names])
dev.off()

# generate Jaccard similarity matrix
jacc_sim <- matrix(0, nrow = length(source_names), ncol = length(source_names), dimnames = list(source_names, source_names))
for (i in 1:length(source_names)) {
  for (j in 1:length(source_names)) {
    jacc_sim[i, j] <- sum(data_sources_df[, i] & data_sources_df[, j]) / sum(data_sources_df[, i] | data_sources_df[, j])
  }
}
#
plot_file <- file.path(stats_folder, paste0("data-sources_jaccard-matrix", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  corrplot(jacc_sim,
    is.corr = FALSE, diag = FALSE,
    method = "color",
    addCoef.col = "white",
    col = viridis(100), col.lim = c(0, 1), tl.col = "black"
  )
dev.off()




########################################################################
# distribution of team countries
plot_top <- 12

# get team country info
team_countries <- c()
for (s in 1:nrow(stints)) {
  if (s %% 1000 == 0)
    tlog(2, "Processing stint ", s, "/", nrow(stints))

  team_id <- stints[s, "teamRsId"]
  t <- which(teams[, "rugbyscopeId"] == team_id)

  # get country list
  stint_countries <- trimws(strsplit(teams[t, "countries"], split = ";")[[1]])

  # add to stat list
  team_countries <- c(team_countries, stint_countries)
}

# count values
tm_countr_tt <- table(team_countries, useNA = "always")
print(tm_countr_tt)

# focus on most frequent values
tm_countr_tt0 <- sort(table(team_countries, useNA = "no"), decreasing = TRUE)
top_countries <- names(tm_countr_tt0)[1:plot_top]

# remove NAs
tm_countr_tt2 <- tm_countr_tt[!is.na(names(tm_countr_tt))]

# add a new value for category others
tm_countr_tt2 <- c(tm_countr_tt2, "Others" = sum(tm_countr_tt2[!(names(tm_countr_tt2) %in% top_countries)], na.rm = TRUE))
top_countries <- c(top_countries, "Others")

# set colors
color_palette <- c(COUNTRY_COLORS, "Others" = "#919191")

# generate barplot
plot_file <- file.path(stats_folder, paste0("team-countries", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  barplot(
    height = tm_countr_tt2[top_countries],
    names.arg = top_countries,
    #xlab = "Stint team countries",
    legend = FALSE,
    las = 2,
    col = color_palette[top_countries]
  )
dev.off()




########################################################################
# distribution of player countries
plot_top <- 12

# get player country info
player_countries <- c()
for (s in 1:nrow(stints)) {
  if (s %% 1000 == 0)
    tlog(2, "Processing stint ", s, "/", nrow(stints))

  player_id <- stints[s, "playerId"]
  p <- which(players[, "wikidataId"] == player_id)

  # get country list
  sport_countries <- players[p, "sportCountries"]
  if (!is.na(sport_countries))
    stint_countries <- trimws(strsplit(sport_countries, split = ";")[[1]])
  else {
    citizenships <- players[p, "citizenships"]
    stint_countries <- trimws(strsplit(citizenships, split = ";")[[1]])
  }

  # add to stat list
  player_countries <- c(player_countries, stint_countries)
}

# count values
pl_countr_tt <- table(player_countries, useNA = "always")
print(pl_countr_tt)

# focus on most frequent values
pl_countr_tt0 <- sort(table(player_countries, useNA = "no"), decreasing = TRUE)
top_countries <- names(pl_countr_tt0)[1:plot_top]

# remove NAs
pl_countr_tt2 <- pl_countr_tt[!is.na(names(pl_countr_tt))]

# add a new value for category others
pl_countr_tt2 <- c(pl_countr_tt2, "Others" = sum(pl_countr_tt2[!(names(pl_countr_tt2) %in% top_countries)], na.rm = TRUE))
top_countries <- c(top_countries, "Others")

# set colors
color_palette <- c(COUNTRY_COLORS, "Others" = "#919191")

# generate barplot
plot_file <- file.path(stats_folder, paste0("player-countries", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  barplot(
    height = pl_countr_tt2[top_countries],
    names.arg = top_countries,
    #xlab = "Stint player countries",
    legend = FALSE,
    las = 2,
    col = color_palette[top_countries]
  )
dev.off()




########################################################################
# distribution of sources vs. player countries

# retrieve sources
cr_data_sources <- c()
cr_countries <- c()
for (s in 1:nrow(stints)) {
  if (s %% 1000 == 0)
    tlog(2, "Processing stint ", s, "/", nrow(stints))

  player_id <- stints[s, "playerId"]
  p <- which(players[, "wikidataId"] == player_id)

  # get countries
  sport_countries <- players[p, "sportCountries"]
  if (!is.na(sport_countries))
    player_countries <- trimws(strsplit(sport_countries, split = ";")[[1]])
  else {
    citizenships <- players[p, "citizenships"]
    player_countries <- trimws(strsplit(citizenships, split = ";")[[1]])
  }

  # get data source list
  stint_data_sources <- trimws(strsplit(stints[s, "dataSources"], split = ";")[[1]])

  # add to stat lists
  cr_countries <- c(cr_countries, rep(player_countries, each = length(stint_data_sources)))
  cr_data_sources <- c(cr_data_sources, rep(stint_data_sources, length(player_countries)))
}

# count values
cr_tt <- table(cr_data_sources, cr_countries, useNA = "always")
print(cr_tt)

# replace minority countries
idx <- which(is.na(colnames(cr_tt)) | !(colnames(cr_tt) %in% top_countries))
cr_tt2 <- cbind(cr_tt, "Others" = rowSums(cr_tt[, idx], na.rm = TRUE))

# remove NAs
cr_tt2 <- cr_tt2[!is.na(rownames(cr_tt2)), ]

# generate contingency table
for (i in 1:2) {
  if (i ==  2) {
    for (top_country in top_countries)
      cr_tt2[, top_country] <- 100 * cr_tt2[, top_country] / pl_countr_tt2[top_country]
  }
  plot_file <- file.path(stats_folder, paste0("data-sources_vs_player-countries_contingency", if (i == 1) "-nbr" else "-prop", ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    corrplot(cr_tt2[, top_countries],
      is.corr = FALSE, #diag = FALSE,
      method = "color",
      number.digits = 0,
      addCoef.col = "white", number.cex = if(i == 1) 0.75 else 1,
      col = viridis(100), #col.lim = c(0, 1), 
      tl.col = "black"
    )
  dev.off()
}




########################################################################
# distribution of sources vs. team countries

# retrieve sources
cr_data_sources <- c()
cr_countries <- c()
for (s in 1:nrow(stints)) {
  if (s %% 1000 == 0)
    tlog(2, "Processing stint ", s, "/", nrow(stints))

  team_id <- stints[s, "teamRsId"]
  t <- which(teams[, "rugbyscopeId"] == team_id)

  # get countries
  team_countries <- trimws(strsplit(teams[t, "countries"], split = ";")[[1]])

  # get data source list
  stint_data_sources <- trimws(strsplit(stints[s, "dataSources"], split = ";")[[1]])

  # add to stat lists
  cr_countries <- c(cr_countries, rep(team_countries, each = length(stint_data_sources)))
  cr_data_sources <- c(cr_data_sources, rep(stint_data_sources, length(team_countries)))
}

# count values
cr_tt <- table(cr_data_sources, cr_countries, useNA = "always")
print(cr_tt)

# replace minority countries
idx <- which(is.na(colnames(cr_tt)) | !(colnames(cr_tt) %in% top_countries))
cr_tt2 <- cbind(cr_tt, "Others" = rowSums(cr_tt[, idx], na.rm = TRUE))

# remove NAs
cr_tt2 <- cr_tt2[!is.na(rownames(cr_tt2)), ]

# generate contingency table
for (i in 1:2) {
  if (i ==  2) {
    for (top_country in top_countries)
      cr_tt2[, top_country] <- 100 * cr_tt2[, top_country] / tm_countr_tt2[top_country]
  }
  plot_file <- file.path(stats_folder, paste0("data-sources_vs_team-countries_contingency", if (i == 1) "-nbr" else "-prop", ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    corrplot(cr_tt2[, top_countries],
      is.corr = FALSE, #diag = FALSE,
      method = "color",
      number.digits = 0,
      addCoef.col = "white", number.cex = if(i == 1) 0.75 else 1,
      col = viridis(100), #col.lim = c(0, 1), 
      tl.col = "black"
    )
  dev.off()
}




########################################################################
# completeness stats
fields <- c("type", "startYear", "endYear", "matchesPlayed", "pointsScored", "dataSources")

pal <- viridis(100)
vals <- sapply(fields, function(field) length(which(!is.na(stints[, field])))) * 100 / nrow(stints)

colors <- pal[pmax(1, pmin(100, round(vals)))]

# generate barplot
plot_file <- file.path(stats_folder, paste0("completeness_all-fields.pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  bp <- barplot(
    height = vals,
    names.arg = fields,
    #xlab = "Team fields",
    legend = FALSE,
    las = 2,
    col = colors,
  )
  text(
    x = bp,
    y = vals - 0.1 * max(vals),
    labels = paste0(round(vals), "%", sep = ""),
    pos = 3, col = sapply(vals, function(val) if (val < 75) "white" else "black"),
    cex = 1.5, font = 2
  )
dev.off()




########################################################################
# stop logging
end.rec.log()
