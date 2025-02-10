library(httr)
library(jsonlite)

########################################################################
# Retrieves the English title of a Wikipedia page based on the name of
# the Japanese version of the page.
#
# ja_name: name of the Japanese WP page (the end of its URL, not its title).
#
# returns: the English title of the corresponding page.
########################################################################
get_english_title <- function(japanese_title) {
  # set up HTTP query
  url <- "https://ja.wikipedia.org/w/api.php"
  params <- list(
    action = "query",
    prop = "langlinks",
    titles = japanese_title,
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
  result <- page$langlinks["*"][1,1]

  return(result)
}
