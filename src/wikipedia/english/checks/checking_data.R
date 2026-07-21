###########################################################################
## Project: RugbyScope
## Script purpose: Hunting for problems in data
## Date: 06/02/2026
## Author: David JP O'Sullivan
########################################################################### 

library(tidyverse)

df <- read_csv("./data/wikipedia/english/profile_links/profile_links_Wikidata.csv")
sum(!is.na(df$wikipediaEn))


getwd()
bio_capped <- read_csv("./data/wikipedia/english/raw0/PP_player_info_capped_players_2.csv")
bio_wikidata <- read_csv("./data/wikipedia/english/raw0/PP_player_info_wikidata_players_4.csv")

stint_capped <- read_csv("./data/wikipedia/english/raw0/PP_player_info_wikidata_players_4.csv")
stint_wikidata <- read_csv("./data/wikipedia/english/raw0/PP_stint_info_wikidata_players_4.csv")


sum(bio_capped$wpPage %in% bio_wikidata$wpPage)/nrow(bio_capped)
sum(bio_wikidata$wpPage %in% bio_capped$wpPage)/nrow(bio_wikidata)

bio_capped |> count(weight)

ggplot(bio_capped, aes(x = weight)) + geom_histogram()

# low weight players
bio_wikidata |> filter(weight != 0, weight < 50) |> select(-c(1,2,3,7)) |> arrange(weight) |> print(n = Inf)
bio_wikidata |> filter(weight == 25 ) |> arrange(weight) |> select(wiki_Name, wpPage, weight)

# dealing with point seems to be a problem
# https://en.wikipedia.org/wiki/Rob_Herring_(rugby_union)
# https://en.wikipedia.org/wiki/Jean_Gachassin

bio_capped$height

bio_capped |> ggplot(aes(x = height)) + geom_histogram()
bio_capped |> filter(height <30) |>  ggplot(aes(x = height)) + geom_histogram()


### they all seem fine.
# hight weight players
bio_capped |> filter(weight > 130) |> select(-c(1,2,3,7)) |> arrange(desc(weight))
bio_capped |> filter(weight > 130) |> select(-c(1,2,3,7)) |> arrange(desc(weight)) |> pull(wpPage)

# dealing with point seems to be a problem
# https://en.wikipedia.org/wiki/Rob_Herring_(rugby_union)
# https://en.wikipedia.org/wiki/Jean_Gachassin



# work on height ----------------------------------------------------------

ggplot(bio_capped, aes(x = height)) + geom_histogram()
bio_capped |> ggplot(aes(x = height)) + geom_histogram()
bio_capped |> filter(height <50, height != 0) |> ggplot(aes(x = height)) + geom_histogram()

# large height players
bio_capped |> filter(height >50) |> select(-c(1,2,3,7)) |> arrange(weight) |> print(n = Inf)
bio_capped |> filter(height > 50) |> arrange(height) |> select(wiki_Name, wpPage, height)


# small height players
bio_capped |> filter(height <1.5, height != 0) |> select(-c(1,2,3,7)) |> arrange(weight) |> print(n = Inf)
bio_capped |> filter(height <1.5, height != 0) |> select(-c(1,2,3,7)) |> arrange(weight) |> pull(wpPage)

bio_capped |> filter(height <1.5, height != 0) |> arrange(height) |> select(wiki_Name, wpPage, height)



# weight ------------------------------------------------------------------


ggplot(bio_capped, aes(x = weight)) + geom_histogram()
bio_capped |> ggplot(aes(x = weight)) + geom_histogram()
bio_capped |> filter(weight != 0) |> ggplot(aes(x = weight)) + geom_histogram()

# large height players
bio_capped |> filter(height >50) |> select(-c(1,2,3,7)) |> arrange(weight) |> print(n = Inf)
bio_capped |> filter(height > 50) |> arrange(height) |> select(wiki_Name, wpPage, height)


# small height players
bio_capped |> filter(height <1.5, height != 0) |> select(-c(1,2,3,7)) |> arrange(weight) |> print(n = Inf)
bio_capped |> filter(height <1.5, height != 0) |> select(-c(1,2,3,7)) |> arrange(weight) |> pull(wpPage)

bio_capped |> filter(height <1.5, height != 0) |> arrange(height) |> select(wiki_Name, wpPage, height)



# plot of date of birth ---------------------------------------------------


library(dplyr)
library(ggplot2)
library(lubridate)

# Create frequency table of birth years
df_years <- 
  bind_rows(
    bio_wikidata |> mutate(type = "Wiki"),
    # bio_capped |> mutate(type = "Capped")
    ) |> 
  mutate(birth_year = year(birthDate)) %>%
  group_by(type) |> 
  filter(!is.na(birth_year)) %>%
  count(birth_year)

# Plot frequency
ggplot(df_years, aes(x = birth_year, y = n, color = type)) +
  geom_line(size = 1) +
  geom_point(color = "black", size = 2) +
  geom_point() +
  labs(
    title = "Frequency of Birth Years",
    x = "Birth Year",
    y = "Count"
  ) +
  cowplot::theme_cowplot()



# -------------------------------------------------------------------------

bio_capped |> count(currentTeam)
bio_capped |> count(positions) |> arrange(desc(n)) |> print(n = Inf)

bio_capped |> filter(is.na(positions)) |> select(wiki_Name, wpPage) |> print(n = Inf)
bio_capped |> filter(positions == "asst coachnsw waratahs") |> select(wiki_Name, wpPage) |> print(n = Inf)
bio_capped |> filter(positions == "2019 apia") |> select(wiki_Name, wpPage) |> print(n = Inf)
bio_capped |> filter(positions == "striker") |> select(wiki_Name, wpPage) |> print(n = Inf)



# check for duplicated rows -----------------------------------------------

sum(duplicated(bio_capped))
sum(duplicated(bio_wikidata))

sum(duplicated(stint_capped))
sum(duplicated(stint_wikidata))

duplicated(bio_capped)

bio_capped |> 
  filter(duplicated(wiki_Name) | duplicated(wiki_Name, fromLast = TRUE)) |> 
  arrange(wiki_Name) |> 
  select(4:10) |> 
  print(n = Inf) 



stint_wikidata |> 
  select(3:4) |> 
  # slice(50000:70000) |>
  rowwise() |> 
  mutate(
    row_check = glue::glue("{paste(across(everything()), collapse = '_')}")
  ) |> 
  filter(duplicated(row_check) | duplicated(row_check, fromLast = TRUE)) |> 
  arrange(row_check) |> 
  print(n = Inf)
  
##########################################################################
# wikidata bio ------------------------------------------------------------------
#########################################################################


bio_wikidata |> count(weight)

ggplot(bio_wikidata, aes(x = weight)) + geom_histogram()

# low weight players
# fixed
bio_wikidata |> filter(weight != 0, weight < 50) |> select(-c(1,2,3,7)) |> arrange(weight) |> print(n = Inf)
bio_wikidata |> filter(weight == 25 ) |> arrange(weight) |> select(wiki_Name, wpPage, weight)




bio_wikidata |> ggplot(aes(x = height)) + geom_histogram()
bio_wikidata |> filter(height <3) |>  ggplot(aes(x = height)) + geom_histogram()


### they all seem fine.
# hight weight players
bio_wikidata |> filter(height > 130) |> select(-c(1,2,3,7)) |> arrange(desc(weight))
bio_wikidata |> filter(height > 130) |> select(-c(1,2,3,7)) |> arrange(desc(weight)) |> pull(wpPage)



##########################################################################
# stints ------------------------------------------------------------------
#########################################################################

stint_wikidata |> count(stintType)
stint_wikidata |> count(timePeriod) |> arrange(desc(n)) |> print(n = Inf)
stint_wikidata |> count(teamName) |> arrange(desc(n)) |> print(n = Inf)


stint_wikidata |> filter(teamName == "[") |> select(-c(1:2))
stint_wikidata |> filter(wpPage == "https://en.wikipedia.org/wiki/David_Penalva") |> 
  select(-c(1:4))
stint_wikidata |> filter(teamName == "[") |> select(-c(1:2)) |> pull(wpPage)



stint_wikidata |> filter(teamName == "1") |> select(-c(1:2))
stint_wikidata |> filter(teamName == "1") |> select(-c(1:2)) |> pull(wpPage)


stint_wikidata |> filter(is.na(teamName)) |> select(-c(1:2))
stint_wikidata |> filter(is.na(teamName)) |> select(-c(1:2)) |> pull(wpPage)

stint_wikidata |> filter(wiki_name == "jens schmidt")
stint_wikidata |> filter(wiki_name == "jens schmidt") |> pull(teamWP)

stint_wikidata |> filter(wiki_name == "carlos huntley-robertson")
stint_wikidata |> filter(wiki_name == "carlos huntley-robertson") |> pull(teamWP)

stint_wikidata |> filter(wpPage == "https://en.wikipedia.org/wiki/Mario_Sagario")
stint_wikidata |> filter(wpPage == "https://en.wikipedia.org/wiki/Jens_Schmidt")
stint_wikidata |> filter(wpPage == "https://en.wikipedia.org/wiki/Tony_Gray_(rugby_union)")


# =============================================================================
# create a table of what needs to be fixed.
# =============================================================================

# =============================================================================
# start looking at the player info 
# =============================================================================

# bio_wikidata <- read_csv("./data/wikipedia/english/PP_player_info_wikidata_players_4.csv")
# stint_wikidata <- read_csv("./data/wikipedia/english/PP_stint_info_wikidata_players_4.csv")

# weight is fine
bio_wikidata |> ggplot(aes(x = weight)) + geom_histogram()
bio_wikidata |> filter(weight != 0) |> ggplot(aes(x = weight)) + geom_histogram()

# fix some of the large and one very small heights
bio_wikidata |> ggplot(aes(x = height)) + geom_histogram()
bio_wikidata |> filter(height != 0, height <= 3) |> ggplot(aes(x = height)) + geom_histogram()
bio_wikidata |> filter(height != 0, height <= 1.5) |> ggplot(aes(x = height)) + geom_histogram()
bio_wikidata |> filter(height > 3) |> ggplot(aes(x = height)) + geom_histogram()

bio_wikidata |> filter(height > 3) |> select(-c(1:3)) |> select(wiki_Name, wpPage, height, weight)

bio_wikidata |> ggplot(aes(x = height)) + geom_histogram()
bio_wikidata |> filter(height < 1) |> ggplot(aes(x = height)) + geom_histogram()


# look at positions
bio_wikidata |> count(positions) |> arrange(desc(n)) |> print(n = Inf)

# =============================================================================
# look at stints
# =============================================================================

stint_wikidata |> ggplot(aes(x = matchesPlayed)) + geom_histogram()
stint_wikidata |> ggplot(aes(x = matchesPlayed)) + geom_histogram() + scale_x_log10()
stint_wikidata |> filter(matchesPlayed < 5*10^3) |> ggplot(aes(x = matchesPlayed)) + geom_histogram()

stint_wikidata |> select(c(3:4, matchesPlayed)) |>arrange(desc(matchesPlayed)) |> filter(matchesPlayed > 10^3/2) |> print(n = Inf)
vec <- stint_wikidata |> select(c(3:4, matchesPlayed)) |>arrange(desc(matchesPlayed)) |> filter(matchesPlayed > 10^3/2) |> pull(wiki_name)
paste(vec, collapse = "' , '")
# 
# very large points need to be fixed
stint_wikidata |> ggplot(aes(x = pointsScored)) + geom_histogram()
stint_wikidata |> ggplot(aes(x = pointsScored)) + geom_histogram() + scale_x_log10()
stint_wikidata |> filter(pointsScored < 10^4) |> ggplot(aes(x = pointsScored)) + geom_histogram()
stint_wikidata |> filter(pointsScored !=0, pointsScored < 10^4) |> ggplot(aes(x = pointsScored)) + geom_histogram() + scale_x_log10()

stint_wikidata |> select(c(3:4, pointsScored)) |> filter(pointsScored > 10^4) |> arrange(desc(pointsScored)) |> print(n = Inf)
stint_wikidata |> select(c(3:4, pointsScored)) |> filter(pointsScored > 10^3, pointsScored < 10^4) |> arrange(desc(pointsScored)) |> print(n = Inf)


stint_wikidata |> count(stintType) |> arrange(desc(n))
stint_wikidata |> count(timePeriod) |> arrange(desc(n)) |> print(n = Inf)
stint_wikidata |> count(timePeriod) |> arrange(desc(n)) |> slice(2000:3000) |> print(n = Inf)

stint_wikidata |> filter(timePeriod == "0000-1976") |> select(3:6)
stint_wikidata |> filter(stringr::str_detect(timePeriod, "0000")) |> select(3:6) |> print(n = Inf)

# =============================================================================
# fixes
# =============================================================================


biowikdata_fixed <- bio_wikidata %>%
  mutate(
    height = case_when(
      height > 150 ~ height / 100,
      height == 1 ~ 0,
      TRUE ~ height
    )
  )