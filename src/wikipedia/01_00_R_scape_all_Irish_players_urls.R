library(rvest)


# install.packages("rvest")
# install.packages("stringr")
# install.packages("stringi")


# To clean data
library(tidyverse)
library(lubridate)
library(janitor)
# To scrape data
library(rvest)
library(httr)
library(polite)

url <- "https://en.wikipedia.org/wiki/List_of_Ireland_national_rugby_union_players"
url_bow <- polite::bow(url)
url_bow

ind_html <-
  polite::scrape(url_bow) %>%  # scrape web page
  rvest::html_nodes("table.wikitable") %>% # pull out specific table
  rvest::html_table(fill = TRUE) 

# Scrape again specifically for URLs in the names
names_urls <- polite::scrape(url_bow) %>%
  rvest::html_nodes("table.wikitable") %>%
  rvest::html_nodes("tr td:first-child a") %>% # Assuming the name is in the first column and has a hyperlink
  rvest::html_attr("href")

ind_tab <- 
  ind_html[[1]] %>% 
  clean_names()

irish_players <- ind_tab %>% mutate(url = glue::glue("https://en.wikipedia.org/{names_urls}"))
write_csv(x = irish_players, file = "./data/IP.csv")


library(reticulate)
use_python("C:\\Users\\David.OSullivan\\AppData\\Local\\anaconda3")
