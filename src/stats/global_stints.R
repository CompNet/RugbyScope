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
for (t in 1:nrow(stints)) {
  if (t %% 1000 == 0)
    tlog(2, "Processing stint ", t, "/", nrow(stints))
  stint_id <- stints[t, "rugbyscopeId"]

  # get data source list
  lst <- strsplit(stints[t, "dataSources"], split = ";")
  for (source in lst[[1]])
    data_sources_df[t, trimws(source)] <- 1
  stint_data_sources <- trimws(lst[[1]])

  # add to stat list
  data_sources <- c(data_sources, stint_data_sources)
}

# count values
countr_tt <- table(data_sources, useNA = "always")
print(countr_tt)

# focus on most frequent values
countr_tt0 <- sort(table(data_sources, useNA = "no"), decreasing = TRUE)
top_data_sources <- names(countr_tt0)

# remove NAs
countr_tt2 <- countr_tt[!is.na(names(countr_tt))]

# set colors
color_palette <- DATASOURCE_COLORS

# generate barplot
plot_file <- file.path(stats_folder, paste0("data-sources", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  barplot(
    height = countr_tt2[top_data_sources],
    names.arg = top_data_sources,
    #xlab = "Stint data sources",
    legend = FALSE,
    las = 2,
    col = color_palette[top_data_sources]
  )
dev.off()

upset(as.data.frame(data_sources_df), sets = source_names)

overlap <- t(data_sources_df) %*% data_sources_df
chordDiagram(overlap, grid.col = color_palette[source_names])

# team country
# player country





########################################################################
# distribution of countries
plot_top <- 12

# get country info
countries <- c()
for (t in 1:nrow(stints)) {
  if (t %% 1000 == 0)
    tlog(2, "Processing stint ", t, "/", nrow(stints))
  stint_id <- stints[t, "rugbyscopeId"]

  # get country list
  stint_countries <- trimws(strsplit(stints[t, "countries"], split = ";")[[1]])

  # add to stat list
  countries <- c(countries, stint_countries)
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
    #xlab = "Stint countries",
    legend = FALSE,
    las = 2,
    col = color_palette[top_countries]
  )
dev.off()




########################################################################
# distribution of types
plot_top <- 9

# retrieve types
types <- stints[, "type"]

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
  barplot(
    height = types_tt2[top_types],
    names.arg = top_types,
    #xlab = "Stint types",
    legend = FALSE,
    las = 2, log = "y",
    col = color_palette[top_types]
  )
dev.off()




########################################################################
# stop logging
end.rec.log()
