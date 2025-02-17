########################################################################
# Functions used to clean WP data and to integrate them to our tables.
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
  if (startsWith(name, "%"))
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
    go_on <- all(is.na(response))
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
