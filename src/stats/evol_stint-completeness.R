########################################################################
# Generates various plots regarding the evolution of the completeness of
# the stint fields.
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/stats/evol_stint-completeness.R")
########################################################################
source("src/common/logging.R")
source("src/common/colors.R")




########################################################################
# start logging
start.rec.log("EvolStintCompleteness")




########################################################################
# parameters

mode <- "start-year"  # "start-year" or "active-years"
mode_labels <- c("start-year" = "career start year", "active-years" = "active year")
mode_xlabels <- c("start-year" = "Career start year", "active-years" = "Active year")




########################################################################
# create output folder
stats_folder <- file.path("data", "stats", paste0("evol_stint-completeness_vs_", mode))
dir.create(stats_folder, showWarnings = FALSE, recursive = TRUE)

# load tables
source("src/stats/load_all_tables.R")




########################################################################
# total number of stints by start year
fields <- c("type", "startYear", "endYear", "matchesPlayed", "pointsScored", "dataSources")

# identify start years
ref_years <- c()
ref_stints <- c()
comp_rates_lst <- list()
for (field in fields)
  comp_rates_lst[[field]] <- c()
if (mode == "start-year") {
  for (s in 1:nrow(stints)) {
    if (s %% 1000 == 0)
      tlog(2, "Processing stint ", s, "/", nrow(stints))

    # get stint year
    start_year <- stints[s, "startYear"]
    end_year <- stints[s, "endYear"]
    if (is.na(start_year))
      year <- end_year
    else if (is.na(end_year))
      year <- start_year
    else
      year <- min(start_year, end_year)

    # add to list
    ref_years <- c(ref_years, year)
    ref_stints <- c(ref_stints, s)

    # update completeness lists
    for (field in fields)
      comp_rates_lst[[field]] <- c(comp_rates_lst[[field]], rep(is.na(stints[s, field]), length(year)))
  }
# identify active years
} else if (mode == "active-years") {
  for (s in 1:nrow(stints)) {
    if (s %% 1000 == 0)
      tlog(2, "Processing stint ", s, "/", nrow(stints))

    active_years <- c()
    start_year <- stints[s, "startYear"]
    end_year <- stints[s, "endYear"]
    if (is.na(start_year)) {
      if (!is.na(end_year))
        active_years <- c(active_years, end_year)
    } else {
      if (is.na(end_year))
        active_years <- c(active_years, start_year)
      else
        active_years <- c(active_years, start_year:end_year)
    }
    active_years <- sort(unique(active_years))

    if (length(active_years) > 0) {
      ref_years <- c(ref_years, active_years)
      ref_stints <- c(ref_stints, rep(s, length(active_years)))

      # update completeness lists
      for (field in fields)
        comp_rates_lst[[field]] <- c(comp_rates_lst[[field]], rep(is.na(stints[s, field]), length(active_years)))
    }
  }
} else {
  stop("Unknown mode: ", mode)
}



# produce plot file
idx <- which(!is.na(ref_years))
# compute completeness rates
vals <- list()
for (field in fields)
  vals[[field]] <- table(ref_years[idx], comp_rates_lst[[field]][idx])[, "FALSE"] *100 / table(ref_years, useNA = "no")

# set colors
colors <- get.palette(length(fields))
names(colors) <- fields

for (plot_log in c(FALSE, TRUE)) {
  for (plot_smoothed in c(FALSE, TRUE)) {
    plot_file <- file.path(stats_folder, paste0("completeness_all-fields", "_smoothed=", plot_smoothed, "_ylog=", plot_log, ".pdf"))
    tlog("Producing plot file: ", plot_file)

    pdf(plot_file, width = 14, height = 7)
      par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
      par(mar = c(3.00, 2.75, 1.75, 0.10))  # control margins: B L T R
      # init plot
      plot(NULL,
        xlab = mode_xlabels[mode], ylab = "Completeness rate (%)",
        #main = paste0("Stint field completeness as a function of ", mode_labels[mode]),
        log = if (plot_log) "y" else "",
        xlim = range(ref_years[idx]), ylim = if (plot_log) c(1, 100) else c(0, 100)
      )
      # add vertical lines
      abline(v = "1871", col = "black")
      abline(v = "1895", col = "black")
      u3 <- if (plot_log) 10^par("usr")[3] else par("usr")[3]
      u4 <- if (plot_log) 10^par("usr")[4] else par("usr")[4]
      rect(1914, u3, 1918, u4, border = NA, col = "#EEEEEE")
      # abline(v = "1914", col = "black")
      # abline(v = "1918", col = "black")
      rect(1939, u3, 1945, u4, border = NA, col = "#EEEEEE")
      # abline(v = "1939", col = "black")
      # abline(v = "1945", col = "black")
      abline(v = "1995", col = "black")
      for (y in seq(1987, 2023, by = 4))
        abline(v = y, col = "black", lty = 3)
      axis(3, at = c(1871, 1895, 1916, 1942, 1995), labels = c("RFU", "Schism", "WW1", "WW2", "Professionalism"))
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
        fill = colors[fields],
        bg = "#FFFFFFBB"
      )
    dev.off()
  }
}




########################################################################
# stop logging
end.rec.log()
