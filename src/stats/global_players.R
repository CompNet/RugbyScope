########################################################################
# Generates various plots regarding the players table, without
# considering time.
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/stats/global_players.R")
########################################################################
library("dplyr")
library("UpSetR")
library("circlize")
library("corrplot")
#library("pheatmap")
library("viridis")



source("src/common/logging.R")
source("src/common/colors.R")
source("src/common/norm_positions.R")




########################################################################
# start logging
start.rec.log("GlobalPlayers")




########################################################################
# create output folder
stats_folder <- file.path("data", "stats", "global_players")
dir.create(stats_folder, showWarnings = FALSE, recursive = TRUE)

# load tables
source("src/stats/load_all_tables.R")




########################################################################
# distribution of birthdates

birth_year <- players[, "birthDate"] %>% as.Date() %>% format("%Y") %>% as.integer()
birth_year <- birth_year[!is.na(birth_year)]

plot_file <- file.path(stats_folder, paste0("birthyears", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  hist(birth_year,
    main = NA,
    xlab = "Player year of birth",
    col = "red"
  )
dev.off()




########################################################################
# distribution of deathdates

death_year <- players[, "deathDate"] %>% as.Date() %>% format("%Y") %>% as.integer()
death_year <- death_year[!is.na(death_year)]

plot_file <- file.path(stats_folder, paste0("deathyears", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  hist(death_year,
    main = NA,
    xlab = "Player year of death",
    col = "red"
  )
dev.off()




########################################################################
# distribution of countries
plot_top <- 12

# get country info
countries <- c()
for (p in 1:nrow(players)) {
  if (p %% 1000 == 0)
    tlog(2, "Processing player ", p, "/", nrow(players))
  player_id <- players[p, "wikidataId"]

  # get country list
  sport_countries <- players[p, "sportCountries"]
  if (!is.na(sport_countries))
    player_countries <- trimws(strsplit(sport_countries, split = ";")[[1]])
  else {
    citizenships <- players[p, "citizenships"]
    player_countries <- trimws(strsplit(citizenships, split = ";")[[1]])
  }

  # add to stat list
  countries <- c(countries, player_countries)
}

# count values
countr_tt <- table(countries, useNA = "always")
print(countr_tt)

# focus on most frequent values
countr_tt0 <- sort(table(countries, useNA = "no"), decreasing = TRUE)
top_countries <- names(countr_tt0)[1:plot_top]

# remove NAs
countr_tt2 <- countr_tt[!is.na(names(countr_tt))]

# add a new value for category others
countr_tt2 <- c(countr_tt2, "Others" = sum(countr_tt2[!(names(countr_tt2) %in% top_countries)], na.rm = TRUE))
top_countries <- c(top_countries, "Others")

# set colors
color_palette <- c(COUNTRY_COLORS, "Others" = "#919191")

# generate barplot
plot_file <- file.path(stats_folder, paste0("countries", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  barplot(
    height = countr_tt2[top_countries],
    names.arg = top_countries,
    #xlab = "Player Countries",
    legend = FALSE,
    las = 2,
    col = color_palette[top_countries]
  )
dev.off()




########################################################################
# distribution of positions
plot_top <- 12

# retrieve positions
positions <- c()
pos_heights <- c()
pos_weights <- c()
for (p in 1:nrow(players)) {
  if (p %% 1000 == 0)
    tlog(2, "Processing entry ", p, "/", nrow(players))
  player_id <- players[p, "wikidataId"]

  # get position lists
  pos <- players[p, "positions"]
  if (!is.na(pos)) {
    player_positions <- trimws(strsplit(pos, split = ";")[[1]])

    # add to stat list
    positions <- c(positions, player_positions)
    pos_heights <- c(pos_heights, rep(players[p, "height"], length(player_positions)))
    pos_weights <- c(pos_weights, rep(players[p, "height"], length(player_positions)))
  }
}

# loop over aggregation levels
for (plot_agg in 1:4) {
  # aggregate positions
  tmp <- aggregate_positions(positions = positions, granularity = plot_agg)
  positions2 <- tmp$positions
  top_positions <- tmp$top_positions

  # count values
  pos_tt <- table(positions2, useNA = "always")
  print(pos_tt)

  # # focus on most frequent values
  # pos_tt0 <- sort(table(positions, useNA = "no"), decreasing = TRUE)
  # top_positions <- names(pos_tt0)[1:min(plot_top, length(pos_tt0))]

  # remove NAs
  pos_tt2 <- pos_tt[!is.na(names(pos_tt))]

  # add a new value for category others
  pos_tt2 <- c(pos_tt2, "Others" = sum(pos_tt2[!(names(pos_tt2) %in% top_positions)], na.rm = TRUE))
  top_positions <- c(top_positions, "Others")

  # set colors
  color_palette <- c(POSITION_COLORS, "Others" = "#919191")

  # generate barplot
  plot_file <- file.path(stats_folder, paste0("positions_agg=", plot_agg, ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    barplot(
      height = pos_tt2[top_positions],
      names.arg = top_positions,
      #xlab = "Player positions",
      legend = FALSE,
      las = 2,
      col = color_palette[top_positions]
    )
  dev.off()
}




########################################################################
# distribution of heights

heights <- players[, "height"]
heights <- heights[!is.na(heights)]

plot_file <- file.path(stats_folder, paste0("heights", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  hist(heights,
    main = NA,
    xlab = "Player height (cm)",
    col = "red",
    breaks = 30
  )
dev.off()

# heights vs. position
for (plot_agg in 1:4) {
  # aggregate positions
  tmp <- aggregate_positions(positions = positions, granularity = plot_agg)
  positions2 <- tmp$positions
  top_positions <- tmp$top_positions

  # add category others
  positions2[!(positions2 %in% top_positions)] <- "Others"
  top_positions <- c(top_positions, "Others")

  # compute density by position
  densities <- list()
  xlim <- range(pos_heights, na.rm = TRUE)
  ylim <- c(1, -1)
  for (position in top_positions) {
    idx <- which(!is.na(pos_heights) & !is.na(positions2) & positions2 == position)
    if (length(idx) > 2) {
      densities[[position]] <- density(pos_heights[idx], na.rm = TRUE)
      ylim[1] <- min(c(ylim[1], densities[[position]]$y))
      ylim[2] <- max(c(ylim[2], densities[[position]]$y))
    }
  }

  # set colors
  color_palette <- c(POSITION_COLORS, "Others" = "#919191")

  # generate density plot
  plot_file <- file.path(stats_folder, paste0("height_vs_position_agg=", plot_agg, ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    # init empty plot
    plot(NULL,
      xlim = xlim, ylim = ylim,
      xlab = "Player height (cm)",
      ylab = "Density"
    )
    # add series
    for (position in top_positions) {
      if (!all(is.na(densities[[position]]))) {
        lines(densities[[position]],
          col = color_palette[position],
          lwd = 2
        )
      }
    }
    # add legend
    legend(
      "topleft",
      legend = top_positions,
      # cex = 0.8,
      fill = color_palette[top_positions]
    )
  dev.off()
}




########################################################################
# distribution of weights

weights <- players[, "weight"]
weights <- weights[!is.na(weights)]

plot_file <- file.path(stats_folder, paste0("weights", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  hist(weights,
    main = NA,
    xlab = "Player weight (kg)",
    col = "red",
    breaks = 30
  )
dev.off()

# weights vs. position
for (plot_agg in 1:4) {
  # aggregate positions
  tmp <- aggregate_positions(positions = positions, granularity = plot_agg)
  positions2 <- tmp$positions
  top_positions <- tmp$top_positions

  # add category others
  positions2[!(positions2 %in% top_positions)] <- "Others"
  top_positions <- c(top_positions, "Others")

  # compute density by position
  densities <- list()
  xlim <- range(pos_weights, na.rm = TRUE)
  ylim <- c(1, -1)
  for (position in top_positions) {
    idx <- which(!is.na(pos_weights) & !is.na(positions2) & positions2 == position)
    if (length(idx) > 2) {
      densities[[position]] <- density(pos_weights[idx], na.rm = TRUE)
      ylim[1] <- min(c(ylim[1], densities[[position]]$y))
      ylim[2] <- max(c(ylim[2], densities[[position]]$y))
    }
  }

  # set colors
  color_palette <- c(POSITION_COLORS, "Others" = "#919191")

  # generate density plot
  plot_file <- file.path(stats_folder, paste0("weight_vs_position_agg=", plot_agg, ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    # init empty plot
    plot(NULL,
      xlim = xlim, ylim = ylim,
      xlab = "Player weight (kg)",
      ylab = "Density"
    )
    # add series
    for (position in top_positions) {
      if (!all(is.na(densities[[position]]))) {
        lines(densities[[position]],
          col = color_palette[position],
          lwd = 2
        )
      }
    }
    # add legend
    legend(
      "topleft",
      legend = top_positions,
      # cex = 0.8,
      fill = color_palette[top_positions]
    )
  dev.off()
}




########################################################################
# distribution of sources
source_names <- c("DBPD", "enWP", "esWP", "frWP", "itWP", "jaWP", "WD")
map <- c("dbpediaId" = "DBPD", "wikipediaEn" = "enWP", "wikipediaEs" = "esWP", "wikipediaFr" = "frWP", "wikipediaIt" = "itWP", "wikipediaJa" = "jaWP", "wikidataId" = "WD")

# retrieve sources
data_sources <- c()
data_sources_df <- matrix(0, nrow = nrow(players), ncol = length(source_names), dimnames = list(c(), source_names))
for (p in 1:nrow(players)) {
  if (p %% 1000 == 0)
    tlog(2, "Processing entry ", p, "/", nrow(players))
  player_id <- players[p, "wikidataId"]

  player_data_sources <- c()
  for (col in names(map)) {
    if (!is.na(players[p, col])) {
      player_data_sources <- c(player_data_sources, map[col])
      data_sources_df[p, map[col]] <- 1
    }
  }

  # add to stat list
  data_sources <- c(data_sources, player_data_sources)
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
    #xlab = "Player data sources",
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
# distribution of sources vs. countries

# retrieve sources
cr_data_sources <- c()
cr_countries <- c()
for (p in 1:nrow(players)) {
  if (p %% 1000 == 0)
    tlog(2, "Processing entry ", p, "/", nrow(players))
  player_id <- players[p, "wikidataId"]

  # get countries
  sport_countries <- players[p, "sportCountries"]
  if (!is.na(sport_countries))
    player_countries <- trimws(strsplit(sport_countries, split = ";")[[1]])
  else {
    citizenships <- players[p, "citizenships"]
    player_countries <- trimws(strsplit(citizenships, split = ";")[[1]])
  }

  # get data sources
  player_data_sources <- c()
  for (col in names(map)) {
    if (!is.na(players[p, col])) {
      player_data_sources <- c(player_data_sources, map[col])
    }
  }

  # add to stat lists
  cr_countries <- c(cr_countries, rep(player_countries, each = length(player_data_sources)))
  cr_data_sources <- c(cr_data_sources, rep(player_data_sources, length(player_countries)))
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
      cr_tt2[, top_country] <- 100 * cr_tt2[, top_country] / countr_tt2[top_country]
  }
  plot_file <- file.path(stats_folder, paste0("data-sources_vs_countries_contingency", if (i == 1) "-nbr" else "-prop", ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    corrplot(cr_tt2[, top_countries],
      is.corr = FALSE, #diag = FALSE,
      method = "color",
      number.digits = 0,
      addCoef.col = "white",
      col = viridis(100), #col.lim = c(0, 1), 
      tl.col = "black"
    )
  dev.off()
}




########################################################################
# overall completeness stats
countries <- sapply(1:nrow(players), function(p) if (is.na(players[p, "sportCountries"])) players[p, "citizenships"] else players[p, "sportCountries"])
players <- cbind(players, "countries" = countries)

field_groups <- list(
  "perso-fields" = c("birthDate", "birthPlace", "deathDate", "deathPlace", "countries"),
  "rugby-fields" = c("positions", "careerStartYear", "careerEndYear", "weight", "height"),
  "id-fields" = c("espnScrumId", "allRugbyId", "googleKnowlId", "itsRugbyId", "rugbyDatabaseId", "dbpediaId"),
  "wp-fields" = c("wikipediaEn", "wikipediaFr", "wikipediaIt", "wikipediaEs", "wikipediaJa")
)

pal <- viridis(100)
for (g in 1:(length(field_groups) + 1)) {
  if (g > length(field_groups)) {
    fields <- unlist(field_groups)
    group_name <- "all-fields"
  } else {
    fields <- field_groups[[g]]
    group_name <- names(field_groups)[g]
  }
  vals <- sapply(fields, function(field) length(which(!is.na(players[, field])))) * 100 / nrow(players)

  colors <- pal[pmax(1, pmin(100, round(vals)))]

  # generate barplot
  plot_file <- file.path(stats_folder, paste0("completeness_", group_name, ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    bp <- barplot(
      height = vals,
      names.arg = fields,
      #xlab = "Player fields",
      legend = FALSE,
      las = 2,
      col = colors,
    )
    text(
      x = if (g > length(field_groups)) bp + 0.20 else bp,
      y = vals - 0.1 * max(vals),
      labels = paste0(round(vals), "%", sep = ""),
      pos = 3, col = sapply(vals, function(val) if (val < 75) "white" else "black"),
      cex = if (g > length(field_groups)) 0.75 else 1.5, font = 2,
      srt = if (g > length(field_groups)) 90 else 0
    )
  dev.off()
}




########################################################################
# stop logging
end.rec.log()
