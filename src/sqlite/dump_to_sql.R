## =============================================================================
## dump_rugby_db.R
##
## Exports a SQLite database as a single runnable .sql script (schema +
## data), using only DBI/RSQLite -- no external `sqlite3` command-line tool
## required. The resulting file can be replayed against a fresh SQLite
## database (e.g. via RSQLite, DB Browser for SQLite, or any other SQLite
## client) to reconstruct the database exactly.
## =============================================================================
# setwd("D:/Users/Vincent/eclipse/workspaces/Test/RugbyScope")
# source("src/sqlite/dump_to_sql.R")

suppressPackageStartupMessages({
  library("DBI")
  library("RSQLite")
})

INPUT_FOLDER <- file.path("data")
DB_PATH     <- file.path(INPUT_FOLDER, "rugbyscope.sqlite")
SQL_PATH <- file.path(INPUT_FOLDER, "rugbyscope.sql")

## -----------------------------------------------------------------------
## Helper: turn an R vector into a vector of SQL literals (vectorised, so
## this stays fast even for large tables -- no per-row R loop needed).
## -----------------------------------------------------------------------
sql_literal_vec <- function(col) {
  out <- character(length(col))
  na_idx <- is.na(col)
  out[na_idx] <- "NULL"

  if (is.numeric(col)) {
    # trim = TRUE drops padding; scientific = FALSE avoids "1e+05"-style
    # output, which SQLite would otherwise happily accept but which is
    # harder for a human to read/diff.
    out[!na_idx] <- format(col[!na_idx], scientific = FALSE, trim = TRUE)
  } else if (is.logical(col)) {
    out[!na_idx] <- ifelse(col[!na_idx], "1", "0")
  } else {
    escaped <- gsub("'", "''", as.character(col[!na_idx]), fixed = TRUE)
    out[!na_idx] <- paste0("'", escaped, "'")
  }
  out
}

## -----------------------------------------------------------------------
## Helper: generate one INSERT statement per row of a table (vectorised
## across columns and rows; returns a character vector, one string/row).
## -----------------------------------------------------------------------
build_insert_statements <- function(con, table) {
  df <- dbReadTable(con, table)
  if (nrow(df) == 0) return(character(0))

  literal_cols <- lapply(df, sql_literal_vec)
  # paste() is vectorised element-wise across multiple vector arguments,
  # so this combines column-1-value, column-2-value, ... row by row
  # without an explicit loop.
  row_values <- do.call(paste, c(literal_cols, list(sep = ", ")))

  col_list <- paste(names(df), collapse = ", ")
  sprintf("INSERT INTO %s (%s) VALUES (%s);", table, col_list, row_values)
}

## -----------------------------------------------------------------------
## Main export routine
## -----------------------------------------------------------------------
dump_sqlite_to_sql <- function(db_path, sql_path) {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  # sqlite_master holds the exact CREATE TABLE / CREATE INDEX text that
  # was originally executed -- reusing it means the rebuilt schema
  # (including CHECK constraints, PRIMARY KEY, etc.) is byte-for-byte
  # faithful, with no need to reconstruct DDL by hand.
  schema <- dbGetQuery(con, "
    SELECT type, name, sql
    FROM sqlite_master
    WHERE sql IS NOT NULL AND name NOT LIKE 'sqlite_%'
    ORDER BY CASE type WHEN 'table' THEN 1 WHEN 'index' THEN 2 ELSE 3 END;
  ")

  tables  <- schema$name[schema$type == "table"]
  indexes <- schema$sql[schema$type == "index"]

  lines <- c("PRAGMA foreign_keys=OFF;", "BEGIN TRANSACTION;")

  for (tbl in tables) {
    create_sql <- schema$sql[schema$type == "table" & schema$name == tbl]
    message(sprintf("Dumping table '%s'...", tbl))
    lines <- c(lines, paste0(create_sql, ";"), build_insert_statements(con, tbl))
  }

  if (length(indexes) > 0) {
    lines <- c(lines, paste0(indexes, ";"))
  }

  lines <- c(lines, "COMMIT;")

  writeLines(lines, sql_path)
  message(sprintf("SQL dump written to: %s (%d tables, %d lines)",
                   normalizePath(sql_path), length(tables), length(lines)))
}

dump_sqlite_to_sql(DB_PATH, SQL_PATH)

## -----------------------------------------------------------------------
## To rebuild the database from this script later, in R:
##
##   con <- DBI::dbConnect(RSQLite::SQLite(), "rugby_rebuilt.sqlite")
##   stmts <- strsplit(paste(readLines("rugby_dump.sql"), collapse = "\n"), ";\n")[[1]]
##   for (s in stmts) if (nzchar(trimws(s))) DBI::dbExecute(con, paste0(s, ";"))
##   DBI::dbDisconnect(con)
##
## (Splitting on ";\n" is safe here because every string literal in the
## dump has already been through sql_literal_vec(), so no literal value
## can itself contain a raw ";\n" sequence -- newlines inside text fields,
## if any, are not altered by escaping, so extremely unusual data
## containing an embedded ";" immediately followed by a newline could in
## principle confuse this simple splitter. For untrusted or unusual data,
## use a proper SQLite client to run the .sql file instead of this naive
## split.)
## =============================================================================
