########################################################################
# Common functions used when creating the proper DB
#
# 07/2025 Vincent Labatut
########################################################################
library("stringr")
library("dplyr")




########################################################################
# Splits a plural cell into a trimmed character vector, dropping blanks.
#
# x: cell content.
# sep: separator of multiple values.
#
# returns: the split string.
########################################################################
split_multi <- function(x, sep = ";") {
  if (is.na(x) || !nzchar(str_trim(x)))
    result <- character(0)
  else {
    parts <- str_split(x, fixed(sep))[[1]]
    parts <- str_trim(parts)
    result <- parts[nzchar(parts)]
  }

  return(result)
}

########################################################################
# Vectorized version of split_multi, returning a list-column-friendly list.
#
# x: vector of cell content.
# sep: separator of multiple values.
#
# returns: the split strings.
########################################################################
split_multi_vec <- function(x, sep = ";") {
  map(x, split_multi, sep = sep)
}

########################################################################
# Explodes a table on a plural column.
#
# df: data table to consider.
# id_col: column to consider.
# value_col: raw multi-value string
#
# Returns: a long tibble with columns {id_col, value, rank} — rank = 1-based position within the cell,
# useful for ordered lists like first/last names.
########################################################################
explode_column <- function(df, id_col, value_col, sep = ";") {
  df %>%
    select(all_of(c(id_col, value_col))) %>%
    mutate(.values = split_multi_vec(.data[[value_col]], sep = sep)) %>%
    select(all_of(id_col), .values) %>%
    unnest_longer(.values, values_to = "value", indices_to = "rank") %>%
    rename(!!id_col := all_of(id_col))
}

########################################################################
# Builds a dimension table {id, value} of unique, non-empty values from one
# or more character vectors (already split/exploded), preserving first-seen
# order. `value_name` sets the name of the value column in the output.
########################################################################
build_dimension <- function(..., value_name = "value") {
  vals <- c(...)
  vals <- vals[!is.na(vals) & nzchar(str_trim(vals))]
  vals <- str_trim(vals)
  uniq <- unique(vals)
  tibble(id = seq_along(uniq), value = uniq) %>%
    rename(!!value_name := value)
}

########################################################################
# Parses a date string into ISO 8601 (YYYY-MM-DD) text, tolerating blanks and
# a few common alternate formats. Returns NA_character_ on failure.
#
# x: string date to parse.
#
# returns: parsed date.
########################################################################
parse_date_safe <- function(x) {
  if (is.na(x) || !nzchar(str_trim(x))) return(NA_character_)
  x <- str_trim(x)
  fmts <- c("%Y-%m-%d", "%Y/%m/%d", "%d-%m-%Y", "%d/%m/%Y")
  for (f in fmts) {
    d <- as.Date(x, format = f)
    if (!is.na(d)) return(format(d, "%Y-%m-%d"))
  }
  # last resort: year-only ("1987")
  if (str_detect(x, "^\\d{4}$")) return(paste0(x, "-01-01"))
  NA_character_
}
