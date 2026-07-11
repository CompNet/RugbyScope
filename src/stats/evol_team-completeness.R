########################################################################
# Generates various plots regarding the evolution of the completeness of
# team fields.
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/stats/evol_team-completeness.R")
########################################################################
library("dplyr")




source("src/common/logging.R")
source("src/common/colors.R")




########################################################################
# parameters

mode <- "active-years"  # "start-year" or "active-years"
mode_labels <- c("start-year" = "career start year", "active-years" = "active year")
mode_xlabels <- c("start-year" = "Career start year", "active-years" = "Active year")




########################################################################
# start logging
start.rec.log(paste0("EvolTeamCompleteness-", mode))
tlog("mode: ", mode)




########################################################################
# create output folder
stats_folder <- file.path("data", "stats", paste0("evol_team-completeness_vs_", mode))
dir.create(stats_folder, showWarnings = FALSE, recursive = TRUE)

# load tables
source("src/stats/load_all_tables.R")




########################################################################
# total number of teams by start year
field_groups <- list(
  "perso-fields" = c("type", "inceptionDate", "terminationDate", "countries"),
  "rugby-fields" = c("affiliations", "competitions", "tier", "homeVenueNames", "homeVenueCapacities", "locations"),
  "id-fields" = c("wikidataId", "allRugbyId", "googleKnowlId", "dbpediaId"),
  "wp-fields" = c("wikipediaEn", "wikipediaFr", "wikipediaIt", "wikipediaEs", "wikipediaJa")
)

# identify start years
ref_years <- c()
ref_teams <- c()
comp_rates_lst <- list()
for (group in names(field_groups)) {
  tmp <- list()
  for (field in field_groups[[group]])
    tmp[[field]] <- c()
  comp_rates_lst[[group]] <- tmp
}
if (mode == "start-year") {
  for (t in 1:nrow(teams)) {
    team_id <- teams[t, "rugbyscopeId"]
    if (t %% 1000 == 0)
      tlog(2, "Processing team ", team_id, " (", t, "/", nrow(teams), ")")

    inception_year <- teams[p, "inceptionDate"] %>% as.Date() %>% format("%Y") %>% as.integer()

    team_stints <- stints[stints[, "teamRsId"] == team_id, ]
    first_stint <- suppressWarnings(min(c(team_stints[, "startYear"], team_stints[, "endYear"]), na.rm = TRUE))

    if (is.na(first_stint) || is.infinite(first_stint))
      team_years <- inception_year
    else
      team_years <- first_stint
    ref_years <- c(ref_years, team_years)
    ref_teams <- c(ref_teams, team_id)

    # update completeness lists
    for (group in names(field_groups)) {
      tmp <- comp_rates_lst[[group]]
      for (field in field_groups[[group]])
        tmp[[field]] <- c(tmp[[field]], rep(is.na(teams[t, field]), length(team_years)))
      comp_rates_lst[[group]] <- tmp
    }
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

      # update completeness lists
      for (group in names(field_groups)) {
        tmp <- comp_rates_lst[[group]]
        for (field in field_groups[[group]])
          tmp[[field]] <- c(tmp[[field]], rep(is.na(teams[t, field]), length(active_years)))
        comp_rates_lst[[group]] <- tmp
      }
    }
  }
} else {
  stop("Unknown mode: ", mode)
}



# produce plot file
for (g in 1:length(field_groups)) {
  fields <- field_groups[[g]]
  group_name <- names(field_groups)[g]

  idx <- which(!is.na(ref_years))

  # compute completeness rates
  vals <- list()
  for (field in fields) {
    vals[[field]] <- table(ref_years[idx], comp_rates_lst[[g]][[field]][idx])[, "FALSE"] *100 / table(ref_years, useNA = "no")
  }

  # set colors
  colors <- get.palette(length(fields))
  names(colors) <- fields

  for (plot_log in c(FALSE, TRUE)) {
    for (plot_smoothed in c(FALSE, TRUE)) {
      plot_file <- file.path(stats_folder, paste0("completeness_", group_name, "_smoothed=", plot_smoothed, "_ylog=", plot_log, ".pdf"))
      tlog("Producing plot file: ", plot_file)

      pdf(plot_file, width = 14, height = 7)
        # init plot
        plot(NULL,
          xlab = mode_xlabels[mode], ylab = "Completeness rate (%)",
          main = paste0("Team field completeness as a function of ", mode_labels[mode]),
          log = if (plot_log) "y" else "",
          xlim = range(ref_years[idx]), ylim = if (plot_log) c(1, 100) else c(0, 100)
        )
        # add series
        for (field in fields) {
          x <- as.integer(names(vals[[field]]))
          y <- c(vals[[field]])
          if (plot_smoothed)
            fit <- loess(y ~ x, na.action = na.exclude, span = 0.05)
          lines(
            x = x,
            y = if (plot_smoothed) predict(fit) else y,
            type = "l", col = colors[field], lwd = 2
          )
        }
        # add legend
        legend(
          "bottomleft",
          legend = fields,
          # cex = 0.8,
          fill = colors[fields]
        )
      dev.off()
    }
  }
}




########################################################################
# stop logging
end.rec.log()
