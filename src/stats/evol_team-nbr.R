########################################################################
# Generates various plots regarding the evolution of the number of teams
# that were founded in a given year, or that were active at a given year:
# - overall
# - by country
# - by type
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/stats/evol_team-nbr.R")
########################################################################
source("src/common/logging.R")
source("src/common/colors.R")




########################################################################
# start logging
start.rec.log("EvolTeamNbr")




########################################################################
# parameters

mode <- "active-years"  # "start-year" or "active-years"
mode_labels <- c("start-year" = "career start year", "active-years" = "active year")
mode_xlabels <- c("start-year" = "Career start year", "active-years" = "Active year")




########################################################################
# create output folder
stats_folder <- file.path("data", "stats", paste0("evol_team-nbr_vs_", mode))
dir.create(stats_folder, showWarnings = FALSE, recursive = TRUE)

# load tables
source("src/stats/load_all_tables.R")




########################################################################
# total number of teams by start year

# identify start years
ref_years <- c()
ref_teams <- c()
if (mode == "start-year") {
  for (t in 1:nrow(teams)) {
    team_id <- teams[t, "rugbyscopeId"]
    if (t %% 1000 == 0)
      tlog(2, "Processing team ", team_id, " (", t, "/", nrow(teams), ")")

    inception_year <- teams[p, "inceptionDate"] %>% as.Date() %>% format("%Y") %>% as.integer()

    team_stints <- stints[stints[, "teamRsId"] == team_id, ]
    first_stint <- suppressWarnings(min(c(team_stints[, "startYear"], team_stints[, "endYear"]), na.rm = TRUE))

    if (is.na(first_stint) || is.infinite(first_stint))
      ref_years <- c(ref_years, inception_year)
    else
      ref_years <- c(ref_years, first_stint)
    ref_teams <- c(ref_teams, team_id)
  }
# identify active years
} else if (mode == "active-years") {
  for (t in 1:nrow(teams)) {
    team_id <- teams[t, "rugbyscopeId"]
    if (t %% 1000 == 0)
      tlog(2, "Processing team ", team_id, " (", t, "/", nrow(teams), ")")

    team_stints <- stints[stints[, "teamRsId"] == team_id, ]
    active_years <- c()
    if (nrow(team_stints) > 0) {
      for (s in 1:nrow(team_stints)) {
        stint_start <- team_stints[s, "startYear"]
        stint_end <- team_stints[s, "endYear"]
        if (is.na(stint_start)) {
          if (!is.na(stint_end))
            active_years <- c(active_years, stint_end)
        } else {
          if (is.na(stint_end))
            active_years <- c(active_years, stint_start)
          else
            active_years <- c(active_years, stint_start:stint_end)
        }
      }
      active_years <- sort(unique(active_years))
    }

    if (length(active_years) > 0) {
      ref_years <- c(ref_years, active_years)
      ref_teams <- c(ref_teams, rep(team_id, length(active_years)))
    }
  }
} else {
  stop("Unknown mode: ", mode)
}
all_tt <- table(ref_years, useNA = "always")
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
      xlab = mode_xlabels[mode], ylab = "Number of teams",
      main = paste0("Number of teams as a function of ", mode_labels[mode]),
      log = if (plot_log) "y" else "",
      type = "l", col = "red", lwd = 2
    )
    dev.off()
  }
}




########################################################################
# number of teams by year by country

# note: some teams are related to multiple countries, so the same team can be counted several times

# get country info
countries <- c()
country_years <- c()
for (i in 1:length(ref_teams)) {
  if (i %% 1000 == 0)
    tlog(2, "Processing entry ", i, "/", length(ref_teams))
  team_id <- ref_teams[i]
  t <- which(teams[, "rugbyscopeId"] == team_id)

  # get country list
  sport_countries <- teams[t, "countries"]
  if (!is.na(sport_countries)) {
    team_countries <- trimws(strsplit(sport_countries, split = ";")[[1]])
    countries <- c(countries, team_countries)
    country_years <- c(country_years, rep(ref_years[t], length(team_countries)))
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
        countr_tt2[countr_tt2 == 0] <- 1

      pdf(plot_file, width = 14, height = 7)
        plot(NULL,
          xlab = mode_xlabels[mode], ylab = "Number of teams",
          log = if (plot_log) "y" else "",
          main = paste0("Number of teams as a function of ", mode_labels[mode]),
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
# number of teams by year by type

# get type info
types <- c()
for (i in 1:length(ref_teams)) {
  if (i %% 1000 == 0)
    tlog(2, "Processing entry ", i, "/", length(ref_teams))
  team_id <- ref_teams[i]
  t <- which(teams[, "rugbyscopeId"] == team_id)

  # add type to stat list
  types <- c(types, teams[t, "type"])
}
pos_tt <- table(types, ref_years, useNA = "always")
print(pos_tt)



# produce plot files
types2 <- types
for (plot_log in c(FALSE, TRUE)) {
  for (plot_smoothed in c(FALSE, TRUE)) {
    for (plot_agg in 1:2) {
      plot_file <- file.path(stats_folder, paste0("types_agg=", plot_agg, "_smoothed=", plot_smoothed, "_ylog=", plot_log, ".pdf"))
      tlog("Producing plot file: ", plot_file)

      # apply aggregation
      if (plot_agg == 2) {
        for (type2 in c("National U18 team", "National U19 team", "National U20 team", "National U21 team", "National U23 team")) {
          types2[types2 == type] <- "National youth team"
        }
      }

      # overal values
      pos_tt0 <- sort(table(types2, useNA = "no"), decreasing = TRUE)
      top_types <- names(pos_tt0)

      # set colors
      color_palette <- TEAMTYPE_COLORS

      # compute freq table
      pos_tt <- table(types2, ref_years, useNA = "always")
      pos_tt <- pos_tt[, !is.na(colnames(pos_tt))]

      # replace zeros by NA to avoid log(0) in the plot
      if (plot_log)
        pos_tt[pos_tt == 0] <- NA

      pdf(plot_file, width = 14, height = 7)
        plot(NULL,
          xlab = mode_xlabels[mode], ylab = "Number of teams",
          log = if (plot_log) "y" else "",
          main = paste0("Number of teams as a function of ", mode_labels[mode]),
          xlim = c(min(as.integer(colnames(pos_tt))), max(as.integer(colnames(pos_tt)))),
          ylim = c(min(pos_tt, na.rm = TRUE), max(pos_tt, na.rm = TRUE))
        )
        for (type in top_types) {
          x <- as.integer(colnames(pos_tt))
          y <- as.integer(pos_tt[type, ])
          if (plot_smoothed)
              fit <- loess(y ~ x, na.action = na.exclude, span = 0.1)
          lines(
            x = x,
            y = if (plot_smoothed) predict(fit) else y,
            col = color_palette[type],
            lwd = 2
          )
        }
        legend(
          "topleft",
          legend = top_types,
          # cex = 0.8,
          fill = color_palette[top_types]
        )
      dev.off()
    }
  }
}




########################################################################
# stop logging
end.rec.log()
