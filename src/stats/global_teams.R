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
  hist(inception_dates,
    main = NA,
    xlab = "Team inception year",
    col = "red",
    breaks = 30,
    las = 2
  )
dev.off()




########################################################################
# distribution of termination years

termination_dates <- teams[, "terminationDate"] %>% as.Date() %>% format("%Y") %>% as.integer()
termination_dates <- termination_dates[!is.na(termination_dates)]

plot_file <- file.path(stats_folder, paste0("termination-years", ".pdf"))
tlog("Producing plot file: ", plot_file)
pdf(plot_file, width = 7, height = 7)
  hist(termination_dates,
    main = NA,
    xlab = "Team termination year",
    col = "red",
    breaks = 30,
    las = 2
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
  barplot(
    height = countr_tt2[top_countries],
    names.arg = top_countries,
    #xlab = "Team Countries",
    legend = FALSE,
    las = 2,
    col = color_palette[top_countries]
  )
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
  barplot(
    height = types_tt2[top_types],
    names.arg = top_types,
    #xlab = "Team types",
    legend = FALSE,
    las = 2, log = "y",
    col = color_palette[top_types]
  )
dev.off()




########################################################################
# stop logging
end.rec.log()
