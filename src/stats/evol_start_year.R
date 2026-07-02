########################################################################
# Generates various plots regarding the evolution of the number if players
# that started their career in a given year, using both stints years
# a career start year :
# - overall
# - by country
# - by position
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/stats/evol_start_year.R")
########################################################################
source("src/common/logging.R")
source("src/common/colors.R")




########################################################################
# start logging
start.rec.log("EvolCareerStat")




########################################################################
# create output folder
stats_folder <- file.path("data", "stats", "evol_nbr-players_vs_start-year")
dir.create(stats_folder, showWarnings = FALSE, recursive = TRUE)

# load tables
tlog("Loading data tables")

data_folder <- file.path("data", "fusion")

players <- read.csv(file.path(data_folder, "players_12.csv"))
tlog(2, "Number of players: ", nrow(players))

teams <- read.csv(file.path(data_folder, "teams_09.csv"))
tlog(2, "Number of teams: ", nrow(teams))

stints <- read.csv(file.path(data_folder, "stints_20_firststint.csv"))
tlog(2, "Number of stints: ", nrow(stints))




########################################################################
# total number of player by start year

# compute stats
start_years <- c()
for (p in 1:nrow(players)) {
  player_id <- players[p, "wikidataId"]
  if (p %% 1000 == 0)
    tlog(2, "Processing player ", player_id, "(", p, "/", nrow(players), ")")

  career_start <- players[p, "careerStartYear"]

  player_stints <- stints[stints[, "playerId"] == player_id, ]
  first_stint <- suppressWarnings(min(c(player_stints[, "startYear"], player_stints[, "endYear"]), na.rm = TRUE))

  if (is.na(first_stint) || is.infinite(first_stint))
    start_years <- c(start_years, career_start)
  else
    start_years <- c(start_years, first_stint)
}
all_tt <- table(start_years, useNA = "always")
print(all_tt)



# produce plot file
for (plot_log in c(FALSE, TRUE)) {
  for (plot_smoothed in c(FALSE, TRUE)) {
    plot_file <- file.path(stats_folder, paste0("all_smoothed=", plot_smoothed, "_ylog=", plot_log, ".pdf"))
    tlog("Producing plot file: ", plot_file)

    all_tt2 <- all_tt[!is.na(names(all_tt))]

    pdf(plot_file, width = 14, height = 7)
    x <- as.integer(names(all_tt2))
    y <- as.integer(all_tt2)
    if (plot_smoothed)
      fit <- loess(y ~ x, na.action = na.exclude, span = 0.05)
    plot(
      x = x,
      y = if (plot_smoothed) predict(fit) else y,
      xlab = "Year", ylab = "Number of players",
      main = "Number of players by career year start",
      log = if (plot_log) "y" else "",
      type = "l", col = "red", lwd = 2
    )
    dev.off()
  }
}




########################################################################
# number of player by start year by country

# note: multiple citizenship are possible, so the same player can be counted several times

# get country info
countries <- c()
country_years <- c()
for (p in 1:nrow(players)) {
  player_id <- players[p, "wikidataId"]
  if (p %% 1000 == 0)
    tlog(2, "Processing player ", player_id, "(", p, "/", nrow(players), ")")

  # get country list
  sport_countries <- players[p, "sportCountries"]
  if (!is.na(sport_countries))
    player_countries <- trimws(strsplit(sport_countries, split = ";")[[1]])
  else {
    citizenships <- players[p, "citizenships"]
    player_countries <- trimws(strsplit(citizenships, split = ";")[[1]])
  }

  # add to stat list
  for (player_country in player_countries) {
    countries <- c(countries, player_country)
    country_years <- c(country_years, start_years[p])
  }
}
countr_tt <- table(countries, country_years, useNA = "always")
print(countr_tt)

# produce plot files
for (plot_log in c(FALSE, TRUE)) {
  for (plot_smoothed in c(FALSE, TRUE)) {
    for (plot_top in c(5, 8)) {
      plot_file <- file.path(stats_folder, paste0("countries_top=", plot_top, "_smoothed=", plot_smoothed, "_ylog=", plot_log, ".pdf"))
      tlog("Producing plot file: ", plot_file)

      # overal values
      countr_tt0 <- sort(table(countries, useNA = "no"), decreasing = TRUE)
      top_countries <- names(countr_tt0)[1:plot_top]

      # set colors
      color_palette <- c(COUNTRY_COLORS, "Others" = "#919191")

      countr_tt2 <- countr_tt[, !is.na(colnames(countr_tt))]
      # add a new line for the rest of the countries
      countr_tt2 <- rbind(countr_tt2, "Others" = colSums(countr_tt2[!(rownames(countr_tt2) %in% top_countries), , drop = FALSE], na.rm = TRUE))
      top_countries <- c(top_countries, "Others")
      # replace zeros by NA to avoid log(0) in the plot
      if (plot_log)
        countr_tt2[countr_tt2 == 0] <- NA

      pdf(plot_file, width = 14, height = 7)
        plot(NULL,
          xlab = "Year", ylab = "Number of players",
          log = if (plot_log) "y" else "",
          main = "Number of players by career year start",
          xlim = c(min(as.integer(colnames(countr_tt2))), max(as.integer(colnames(countr_tt2)))),
          ylim = c(min(countr_tt2, na.rm = TRUE), max(countr_tt2, na.rm = TRUE))
        )
        for (country in top_countries) {
          x <- as.integer(colnames(countr_tt2))
          y <- as.integer(countr_tt2[country, ])
          if (plot_smoothed)
              fit <- loess(y ~ x, na.action = na.exclude, span = 0.1)
          lines(
            x = x,
            y = if (plot_smoothed) predict(fit) else y,
            col = color_palette[country],
            lwd = 2
          )
        }
        legend(
          "topleft",
          legend = top_countries,
          # cex = 0.8,
          fill = color_palette[top_countries]
        )
      dev.off()
    }
  }
}




########################################################################
# number of players by start year by position

# note: multiple positions are possible, so the same player can be counted several times

# get country info
positions <- c()
position_years <- c()
for (p in 1:nrow(players)) {
  player_id <- players[p, "wikidataId"]
  if (p %% 1000 == 0)
    tlog(2, "Processing player ", player_id, "(", p, "/", nrow(players), ")")

  # get position lists
  pos <- players[p, "positions"]
  if (!is.na(pos)) {
    player_positions <- trimws(strsplit(pos, split = ";")[[1]])

    # add to stat list
    for (player_position in player_positions) {
      positions <- c(positions, player_position)
      position_years <- c(position_years, start_years[p])
    }
  } else {
    positions <- c(positions, NA)
    position_years <- c(position_years, start_years[p])
  }
}
pos_tt <- table(positions, position_years, useNA = "always")
print(pos_tt)

# produce plot files
for (plot_log in c(FALSE, TRUE)) {
  for (plot_smoothed in c(FALSE, TRUE)) {
    for (plot_top in c(5, 8)) {
      plot_file <- file.path(stats_folder, paste0("positions_top=", plot_top, "_smoothed=", plot_smoothed, "_ylog=", plot_log, ".pdf"))
      tlog("Producing plot file: ", plot_file)

      # overal values
      pos_tt0 <- sort(table(positions, useNA = "no"), decreasing = TRUE)
      top_positions <- names(pos_tt0)[1:plot_top]

      # set colors
      color_palette <- c(get.palette(values = plot_top), "#919191")
      names(color_palette) <- c(top_positions, "Others")

      pos_tt2 <- pos_tt[, !is.na(colnames(pos_tt))]
      # add a new line for the rest of the positions
      pos_tt2 <- rbind(pos_tt2, "Others" = colSums(pos_tt2[!(rownames(pos_tt2) %in% top_positions), , drop = FALSE], na.rm = TRUE))
      top_positions <- c(top_positions, "Others")
      # replace zeros by NA to avoid log(0) in the plot
      if (plot_log)
        pos_tt2[pos_tt2 == 0] <- NA

      pdf(plot_file, width = 14, height = 7)
        plot(NULL,
          xlab = "Year", ylab = "Number of players",
          log = if (plot_log) "y" else "",
          main = "Number of players by career year start",
          xlim = c(min(as.integer(colnames(pos_tt2))), max(as.integer(colnames(pos_tt2)))),
          ylim = c(min(pos_tt2, na.rm = TRUE), max(pos_tt2, na.rm = TRUE))
        )
        for (position in top_positions) {
          x <- as.integer(colnames(pos_tt2))
          y <- as.integer(pos_tt2[position, ])
          if (plot_smoothed)
              fit <- loess(y ~ x, na.action = na.exclude, span = 0.1)
          lines(
            x = x,
            y = if (plot_smoothed) predict(fit) else y,
            col = color_palette[position],
            lwd = 2
          )
        }
        legend(
          "topleft",
          legend = top_positions,
          # cex = 0.8,
          fill = color_palette[top_positions]
        )
      dev.off()
    }
  }
}




########################################################################
# stop logging
end.rec.log()
