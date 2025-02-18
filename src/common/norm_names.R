########################################################################
# Functions used to normalize names using Wikipedia.
#
# 02/2025 Vincent Labatut
########################################################################
library("httr")
library("jsonlite")

source("src/common/logging.R")




########################################################################
# Retrieves the English title of a Wikipedia page based on the name of
# the page in the WP URL, possibly in a different language.
#
# name: name of the WP page (the end of its URL, not its title).
# lang: language of the WP page bearing this name.
#
# returns: the English title of the corresponding page.
########################################################################
get_english_title <- function(name, lang = "ja") {
  # normalize parameters
  if (startsWith(name, "http")) {
    lang <- gsub("https://([a-z]{2}).wikipedia.org/wiki/.*", "\\1", name)
    name <- gsub("https://[a-z]{2}.wikipedia.org/wiki/(.*)", "\\1", name)
  } else {
    if (startsWith(name, "/wiki/"))
      name <- substr(name, start = nchar("/wiki/") + 1, stop = nchar(name))
  }
  if (grepl("%", name, fixed = TRUE))
    name <- URLdecode(name)

  # get the proper original title by solving redirections
  url <- paste0("https://", lang, ".wikipedia.org/w/api.php")
  params <- list(
    action = "query",
    titles = name,
    redirects = "true",
    format = "json"
  )
  # send to server
  go_on <- TRUE
  while (go_on) {
    response <- tryCatch({GET(url, query = params)}, error = function(e) {tlog("Server error: ", e$message); NA})
    if (all(is.na(response))) {
      Sys.sleep(2)
      tlog("Server error: retrying")
    }
    else
      go_on <- FALSE
  }
  # retrieve final title
  data <- content(response, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(data)
  page <- json_data$query$pages[[1]]
  name <- page$title

  # set up HTTP query
  url <- paste0("https://", lang, ".wikipedia.org/w/api.php")
  params <- list(
    action = "query",
    prop = "langlinks",
    titles = name,
    lllang = "en",
    format = "json"
  )
  # send to server
  go_on <- TRUE
  while (go_on) {
    response <- tryCatch({GET(url, query = params)}, error = function(e) {tlog("Server error: ", e$message); NA})
    if (all(is.na(response))) {
      Sys.sleep(2)
      tlog("Server error: retrying")
    }
    else
      go_on <- FALSE
  }
  # retrieve english title
  data <- content(response, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(data)
  page <- json_data$query$pages[[1]]
  if (lang == "en")
    result <- page$title
  else
    result <- page$langlinks["*"][1, 1]

  return(result)
}
