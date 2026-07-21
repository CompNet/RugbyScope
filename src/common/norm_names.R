########################################################################
# Functions used to normalize names using Wikipedia.
#
# 02/2025 Vincent Labatut
########################################################################
library("httr")
library("jsonlite")

source("src/common/logging.R")




########################################################################
# Receives the url or name of a Wikipedia page, and solves potential
# redirections. Returns the final name after redirection.
#
# name: name of the WP page (the end of its URL, not its title).
# lang: language of the WP page bearing this name.
#
# returns: the name of the redirected page (may contain underscores).
########################################################################
solve_redirections <- function(name, lang = "ja") {
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

  # solve redirections
  url <- paste0("https://", lang, ".wikipedia.org/w/api.php")
  params <- list(
    action = "query",
    titles = name,
    redirects = "true",
    format = "json",
    prop = "info",
    inprop = "url"
  )

  # send to server
  go_on <- TRUE
  while (go_on) {
    response <- tryCatch({GET(url, query = params)}, error = function(e) {tlog("Server error: ", e$message); NA})
    if (all(is.na(response))) {
      Sys.sleep(2)
      tlog("Server error: retrying ", url)
    }
    else
      go_on <- FALSE
  }

  # retrieve final name
  data <- content(response, as = "text", encoding = "UTF-8")
  json_data <- fromJSON(data)
  page <- json_data$query$pages[[1]]
  url <- page$canonicalurl
  result <- gsub("https://[a-z]{2}.wikipedia.org/wiki/(.*)", "\\1", url)

  return(result)
}




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

  # solve WP redirections
  name <- solve_redirections(name, lang)
  if (grepl("%", name, fixed = TRUE))
    name <- URLdecode(name)

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
      tlog("Server error: retrying ", url)
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
