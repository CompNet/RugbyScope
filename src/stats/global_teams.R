########################################################################
# Generates various plots regarding the teams table, without
# considering time.
#
# 07/2025 Vincent Labatut
#
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/stats/global_teams.R")
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
start.rec.log("GlobalTeams")




########################################################################
# create output folder
stats_folder <- file.path("data", "stats", "global_teams")
dir.create(stats_folder, showWarnings = FALSE, recursive = TRUE)

# load tables
source("src/stats/load_all_tables.R")




########################################################################
# distribution of inception years

inception_dates <- teams[, "inceptionDate"] %>% as.Date() %>% format("%Y") %>% as.integer()
inception_dates <- inception_dates[!is.na(inception_dates)]

plot_file <- file.path(stats_folder, paste0("inception-years", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hist(inception_dates,
    main = NA,
    xlab = "Team inception year",
    col = "red",
    # las = 2,
    breaks = 30
  )
dev.off()




########################################################################
# distribution of termination years

termination_dates <- teams[, "terminationDate"] %>% as.Date() %>% format("%Y") %>% as.integer()
termination_dates <- termination_dates[!is.na(termination_dates)]

plot_file <- file.path(stats_folder, paste0("termination-years", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hist(termination_dates,
    main = NA,
    xlab = "Team termination year",
    col = "red",
    # las = 2,
    breaks = 30
  )
dev.off()




########################################################################
# distribution of countries
plot_top <- 12

# get country info
countries <- c()
for (t in 1:nrow(teams)) {
  if (t %% 1000 == 0)
    tlog(2, "Processing team ", t, "/", nrow(teams))
  team_id <- teams[t, "rugbyscopeId"]

  # get country list
  team_countries <- trimws(strsplit(teams[t, "countries"], split = ";")[[1]])

  # add to stat list
  countries <- c(countries, team_countries)
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
  par(mgp = c(3.0, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(1.50, 4.00, 0.25, 0.00))  # control margins: B L T R
  heights <- countr_tt2[top_countries]

  # init plot
  bp <- barplot(
    height = heights,
    #xlab = "Team Countries",
    ylab = "Frequency",
    names.arg = FALSE,
    legend = FALSE,
    las = 2,
    col = color_palette[top_countries]
  )
  mtext("Team country", side = 1, line = 0.25)

  # decide bar text pos
  outside_text <- which(heights < 0.5 * max(heights, na.rm = TRUE))
  inside_text <- which(heights >= 0.5 * max(heights, na.rm = TRUE))
  # bar names on top
  if (length(outside_text) > 0) {
    text(bp[outside_text],
      heights[outside_text] + 0.025 * max(heights, na.rm = TRUE),
      labels = top_countries[outside_text],
      col = "black",
      srt = 90,
      adj = c(0, 0.5),
      xpd = TRUE
    )
  }

  # bar names inside
  if (length(inside_text) > 0) {
    text(bp[inside_text],
      heights[inside_text] - 0.025 * max(heights, na.rm = TRUE),
      labels = top_countries[inside_text],
      col = text_color(color_palette[top_countries[inside_text]]),
      srt = 90,
      adj = c(1, 0.5),
      xpd = TRUE
    )
  }
dev.off()




########################################################################
# distribution of types
plot_top <- 9

# retrieve types
types <- teams[, "type"]

# count values
types_tt <- table(types, useNA = "always")
print(types_tt)

# focus on most frequent values
types_tt0 <- sort(table(types, useNA = "no"), decreasing = TRUE)
top_types <- names(types_tt0)[1:plot_top]

# remove NAs
types_tt2 <- types_tt[!is.na(names(types_tt))]

# set colors
color_palette <- TEAMTYPE_COLORS

# add a new value for category others
if (length(unique(types)) > plot_top) {
  types_tt2 <- c(types_tt2, "Others" = sum(types_tt2[!(names(types_tt2) %in% top_types)], na.rm = TRUE))
  top_types <- c(top_types, "Others")

  # set colors
  color_palette <- c(color_palette, "Others" = "#919191")
}

# generate barplot
plot_file <- file.path(stats_folder, paste0("types", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(1.50, 2.50, 0.00, 0.00))  # control margins: B L T R
  heights <- types_tt2[top_types]

  # init plot
  bp <- barplot(
    height = heights,
    # xlab = "Team type",
    ylab = "Frequency",
    names.arg = FALSE,
    legend = FALSE,
    las = 2, log = "y",
    col = color_palette[top_types]
  )
  mtext("Team type", side = 1, line = 0.25)

  # decide bar text pos
  outside_text <- which(heights < 0.5 * max(heights, na.rm = TRUE))
  inside_text <- which(heights >= 0.5 * max(heights, na.rm = TRUE))

  # bar names on top
  if (length(outside_text) > 0) {
    text(bp[outside_text],
      10^(log(heights[outside_text],10) + 0.025 * max(log(heights,10), na.rm = TRUE)),
      labels = top_types[outside_text],
      col = "black",
      srt = 90,
      adj = c(0, 0.5),
      xpd = TRUE
    )
  }

  # bar names inside
  if (length(inside_text) > 0) {
    text(bp[inside_text],
      10^(log(heights[inside_text], 10) - 0.025 * max(log(heights, 10), na.rm = TRUE)),
      labels = top_types[inside_text],
      col = text_color(color_palette[top_types[inside_text]]),
      srt = 90,
      adj = c(1, 0.5),
      xpd = TRUE
    )
  }
dev.off()




########################################################################
# distribution of stint number and duration

# retrieve stint stats
stint_nbr <- c()
stint_dur <- c()
for (t in 1:nrow(teams)) {
  if (t %% 1000 == 0)
    tlog(2, "Processing entry ", t, "/", nrow(teams))
  team_id <- teams[t, "rugbyscopeId"]

  team_stints <- stints[stints[, "teamRsId"] == team_id, ]
  stint_nbr <- c(stint_nbr, nrow(team_stints))
  stint_dur <- c(stint_dur, mean(team_stints[, "endYear"] - team_stints[, "startYear"], na.rm = TRUE))
}
tlog("Average stint number by team: ", mean(stint_nbr, na.rm = TRUE), "(sd: ", sd(stint_nbr, na.rm = TRUE),")")
tlog("Average stint duration by team: ", mean(stint_dur, na.rm = TRUE), "(sd: ", sd(stint_dur, na.rm = TRUE),")")


plot_file <- file.path(stats_folder, paste0("stint-numbers", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  hist(stint_nbr,
    main = NA,
    xlab = "Number of stints by team",
    # breaks = 30,
    # log = "y",
    col = "red"
  )

  # #### log version -- start

  # hh <- hist(stint_nbr, plot = FALSE)
  # # choose a baseline for the bars (can't be 0 on log scale)
  # ybottom <- min(hh$counts[hh$counts > 0])  # or just use 1
  # # set up an empty plot with correct axes/limits
  # plot(
  #   x = hh$breaks,
  #   y = c(hh$counts, NA),
  #   type = "n",
  #   xlab = "Number of stints by team",
  #   ylab = "Frequency",
  #   ylim = c(ybottom, max(hh$counts)),
  #   log = "y"
  # )

  # # draw the bars manually
  # rect(
  #   xleft = hh$breaks[-length(hh$breaks)],
  #   xright = hh$breaks[-1],
  #   ybottom = ybottom,
  #   ytop = ifelse(hh$counts == 0, ybottom, hh$counts),
  #   col = "red"
  # )

  # #### log version -- end

dev.off()


plot_file <- file.path(stats_folder, paste0("stint-durations", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(3.00, 2.75, 0.50, 0.00))  # control margins: B L T R

  # excluding a statistic anomaly: a 50 year long stint (far longer than the rest)
  idx <- which(players[, "wikidataId"] == "Q131675151")

  hist(stint_dur[-idx],
    main = NA,
    xlab = "Average stint duration by team",
    # log = "y",
    col = "red"
  )
dev.off()




########################################################################
# distribution of sources
source_names <- c("DBPD", "enWP", "esWP", "frWP", "itWP", "jaWP", "WD")
map <- c("dbpediaId" = "DBPD", "wikipediaEn" = "enWP", "wikipediaEs" = "esWP", "wikipediaFr" = "frWP", "wikipediaIt" = "itWP", "wikipediaJa" = "jaWP", "wikidataId" = "WD")

# retrieve sources
data_sources <- c()
data_sources_df <- matrix(0, nrow = nrow(teams), ncol = length(source_names), dimnames = list(c(), source_names))
for (t in 1:nrow(teams)) {
  if (t %% 1000 == 0)
    tlog(2, "Processing entry ", t, "/", nrow(teams))
  team_id <- teams[t, "rugbyscopeId"]

  team_data_sources <- c()
  for (col in names(map)) {
    if (!is.na(teams[t, col])) {
      team_data_sources <- c(team_data_sources, map[col])
      data_sources_df[t, map[col]] <- 1
    }
  }

  # add to stat list
  data_sources <- c(data_sources, team_data_sources)
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
  par(mgp = c(1.5, 0.5, 0))             # reduce space between axis title / axis values and axis line
  par(mar = c(1.50, 2.50, 0.00, 0.00))  # control margins: B L T R
  heights <- sources_tt2[top_data_sources]

  # init plot
  bp <- barplot(
    height = heights,
    # xlab = "Team data source",
    ylab = "Frequency",
    names.arg = FALSE,
    legend = FALSE,
    #las = 2,
    col = color_palette[top_data_sources]
  )
  mtext("Team data source", side = 1, line = 0.25)

  # decide bar text pos
  outside_text <- which(heights < 0.5 * max(heights, na.rm = TRUE))
  inside_text <- which(heights >= 0.5 * max(heights, na.rm = TRUE))

  # bar names on top
  if (length(outside_text) > 0) {
    text(bp[outside_text],
      heights[outside_text] + 0.025 * max(heights, na.rm = TRUE),
      labels = top_data_sources[outside_text], cex = 2,
      col = "black",
      srt = 90,
      adj = c(0, 0.5),
      xpd = TRUE
    )
  }

  # bar names inside
  if (length(inside_text) > 0) {
    text(bp[inside_text],
      heights[inside_text] - 0.025 * max(heights, na.rm = TRUE),
      labels = top_data_sources[inside_text], cex = 2,
      col = text_color(color_palette[top_data_sources[inside_text]]),
      srt = 90,
      adj = c(1, 0.5),
      xpd = TRUE
    )
  }
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
for (t in 1:nrow(teams)) {
  if (t %% 1000 == 0)
    tlog(2, "Processing entry ", t, "/", nrow(teams))
  team_id <- teams[t, "rugbyscopeId"]

  # get countries
  team_countries <- trimws(strsplit(teams[t, "countries"], split = ";")[[1]])

  # get data sources
  team_data_sources <- c()
  for (col in names(map)) {
    if (!is.na(teams[t, col])) {
      team_data_sources <- c(team_data_sources, map[col])
    }
  }

  # add to stat lists
  cr_countries <- c(cr_countries, rep(team_countries, each = length(team_data_sources)))
  cr_data_sources <- c(cr_data_sources, rep(team_data_sources, length(team_countries)))
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
field_groups <- list(
  "perso-fields" = c("type", "inceptionDate", "terminationDate", "countries"),
  "rugby-fields" = c("affiliations", "competitions", "tier", "homeVenueNames", "homeVenueCapacities", "locations"),
  "id-fields" = c("wikidataId", "allRugbyId", "googleKnowlId", "dbpediaId"),
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
  vals <- sapply(fields, function(field) length(which(!is.na(teams[, field])))) * 100 / nrow(teams)

  colors <- pal[pmax(1, pmin(100, round(vals)))]

  # generate barplot
  plot_file <- file.path(stats_folder, paste0("completeness_", group_name, ".pdf"))
  tlog("Producing plot file: ", plot_file)
  pdf(plot_file, width = 7, height = 7)
    par(mgp = c(2.0, 0.5, 0))             # reduce space between axis title / axis values and axis line
    par(mar = c(9.00, 3.00, 0.50, 0.00))  # control margins: B L T R

    # init barplot
    bp <- barplot(
      height = vals,
      names.arg = fields,
      #xlab = "Team fields",
      legend = FALSE,
      las = 2,
      col = colors,
    )

    # add bar values
    text(
      x = if (g > length(field_groups)) bp + 0.20 else bp,
      y = if (g > length(field_groups)) vals - 0.06 * max(vals) else vals - 0.1 * max(vals),
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
