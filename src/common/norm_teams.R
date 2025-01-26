########################################################################
# Functions used to normalize certain fields that describe rugby union
# teams. This processing is generic, i.e. not tied to a specific data 
# source.
#
# Vincent Labatut
# 01/2025
########################################################################
library("stringi")

source("src/common/logging.R")




########################################################################
# Normalizes the specified team names. The function implements three
# levels of normalization:
#   1. Removing diacritics, case, and points, replace hyphens by spaces.
#   2. Turning standard expression into acronyms (e.g. Rugby Club => RC).
#   3. Outright remove these standard expressions.
#
# names: original names.
# level: normalization level (1, 2 or 3), see above.
#
# returns: list of normalized names.
########################################################################
normalize_names <- function(names, level = 1) {
  result <- names

  #### first level of normalization
  # remove all diacritics
  result <- stri_trans_general(str = result, id = "Latin-ASCII")

  # switch to uppercase
  result <- toupper(result)

  # remove points
  result <- gsub(".", "", result, fixed = TRUE)

  # replace hyphens by spaces
  result <- gsub("-", " ", result, fixed = TRUE)

  # trim
  result <- trimws(result)

  #### more advanced normalization
  if (level > 1) {
    # normalize "saint"
    result <- gsub("\\bSAINT(E)?\\b", "ST", result, fixed = FALSE)

    # define conversion map
    map <- c()
    map["Amatori Rugby"] <- "AR"
    map["Amicale Laïque"] <- "AL"
    map["Amicale Sportive"] <- "AS"
    map["Association Sportive"] <- "AS"
    map["Association Sportive et Culturelle"] <- "ASC"
    map["Association Amicale et Sportive"] <- "AAS"
    map["Associazione Sportiva"] <- "AS"
    map["Associazione Sportiva Dilettantistica"] <- "ASD"
    map["Athletic Club"] <- "AC"
    map["Cercle Amical"] <- "CA"
    map["Cercle Municipal"] <- "CM"
    map["Club Amical"] <- "CA"
    map["Athletic Club Rugby"] <- "ACR"
    map["Club Athlétique"] <- "CA"
    map["Club Athlétique et Sportif"] <- "CAS"
    map["Club Atlético"] <- "CA"
    map["Club de Rugby"] <- "CR"
    map["Club Deportivo"] <- "CD"
    map["Club Gimnasia y Esgrima"] <- "CGE"
    map["Club Municipal"] <- "CM"
    map["Club Olympique"] <- "CO"
    map["Club Omnisport"] <- "CO"
    map["Club Social"] <- "CS"
    map["Club Social y Deportivo"] <- "CSD"
    map["Club Sportif"] <- "CS"
    map["Club Universitario"] <- "CU"
    map["Cricket & Rugby Club"] <- "CRC"
    map["Centro Universitario Sportivo"] <- "CUS"
    map["Étoile Sportive"] <- "ES"
    map["Football Club"] <- "FC"
    map["Groupe Sportif"] <- "GS"
    map["Jeunesse Olympique"] <- "JO"
    map["Jeunesse Sportive"] <- "JS"
    map["Jockey Club"] <- "JC"
    map["Olympic Rugby Club"] <- "ORC"
    map["Racing Club"] <- "RC"
    map["Racing Rugby Club"] <- "RCC"
    map["Rassemblement"] <- "Ras"
    map["Rst"] <- "Ras"
    map["Rugby & Hockey Club"] <- "RHC"
    map["Rugby Athletic Club"] <- "RAC"
    map["Rugby Club"] <- "RC"
    map["Rugby Club Sportif"] <- "RCS"
    map["Rugby Football Club"] <- "RFC"
    map["Rugby Olympic Club"] <- "ROC"
    map["Rugby Olympique"] <- "RO"
    map["Rugby Union Football Club"] <- "RUFC"
    map["Rugby Union Sportive"] <- "RUS"
    map["Sport Athlétique"] <- "SA"
    map["Sport Rugby"] <- "SR"
    map["Sporting Club"] <- "SC"
    map["Sporting Union"] <- "SU"
    map["Società a Responsabilità Limitata"] <- "SRL"
    map["Società Sportiva Dilettantistica"] <- "SSD"
    map["Stade Athlétique"] <- "SA"
    map["Stade Olympique"] <- "SO"
    map["Union Athlétique"] <- "UA"
    map["Union Club"] <- "UC"
    map["Union Sportive Athlétique"] <- "USA"
    map["Union Sportive Olympique"] <- "USO"
    map["Union Sportive"] <- "US"
    map["Unione Rugby"] <- "UR"
    map["Unione Sportiva"] <- "US"
    map["Universitario Rugby Club"] <- "URC"
    map["Université Club"] <- "UC"

    # remove diacritics and switch to upper case
    names <- names(map)
    map <- stri_trans_general(str = map, id = "Latin-ASCII")
    map <- toupper(map)
    names <- stri_trans_general(str = names, id = "Latin-ASCII")
    names(map) <- toupper(names)
    # add regex word boundaries
    names(map) <- paste0("\\b", names(map), "\\b")

    # third level map
    if (level == 3) {
      # add abbreviations as keys in the map, associated to empty strings
      lg <- length(unique(map))
      map2 <- rep("", lg)
      names(map2) <- paste0("\\b", unique(map), "\\b")
      map <- c(map, map2)

      # replace every other value by an empty string
      map[names(map)] <- rep("", length(map))
    }

    # order map by decreasing string length
    names <- names(map)
    map <- rev(map[order(nchar(names))])
    names(map) <- rev(names[order(nchar(names))])

    # loop over map to replace original expressions
    for (m in 1:length(map)) {
      mm <- names(map)[m]
      result <- gsub(mm, map[mm], result, fixed = FALSE)
    }

    # trim heading/trailing white spaces
    result <- trimws(result)
  }

  return(result)
}




########################################################################
# Matches string from the source vector into the target vector. The
# function looks for the best matches, considering the specifics of
# rugby union team names. The specified strings can be lists of names,
# separated by "; ".
#
# If there are duplicates in the target list, it is possible for one
# source name to be matched to several target names, in which case the
# function returns a list. Otherwise, it returns a vector.
#
# src_names: vector of (possibly multiple) names that we want to match
#            to the reference names.
# tgt_names: vector of (possibly multiple) reference names.
#
# returns: best matches for each source name, expressed as positions in
#          the target vector, or NA if there is no match at all.
########################################################################
match_names <- function(src_names, tgt_names) {
  # init result list
  result <- list()

  ### first attempt: for all original names

  # apply light normalization
  norm_src_names <- normalize_names(names = src_names, level = 1)
  norm_tgt_names <- normalize_names(names = tgt_names, level = 1)

  # split names into lists
  norm_src_names <- strsplit(norm_src_names, "; ")
  norm_tgt_names <- strsplit(norm_tgt_names, "; ")

  # match each src names to the tgt names
  tlog(0, "Matching: level 1")
  for (i in 1:length(norm_src_names)) {
    tlog(2, "Matching team #", i, "/", length(norm_src_names))
    tmp <- which(sapply(1:length(norm_tgt_names), function(j) {
      length(intersect(norm_src_names[[i]], norm_tgt_names[[j]])) > 0
    }))
    if(length(tmp) == 0)
      tmp <- NA
    result[[i]] <- tmp
  }


  ### second and third attempts: for non-matched names
  for (level in 2:3) {
    tlog(0, "Matching: level ", level)

    # perform deeper normalization
    norm_src_names <- normalize_names(names = src_names, level = level)
    norm_tgt_names <- normalize_names(names = tgt_names, level = level)

    # split names into lists
    norm_src_names <- strsplit(norm_src_names, "; ")
    norm_tgt_names <- strsplit(norm_tgt_names, "; ")

    # match each remaining src name to the tgt names
    idx <- which(sapply(result, function(x) all(is.na(x))))
    if (length(idx) > 0) {
      for (i in idx) {
        tlog(2, "Matching team #", i, "/", length(norm_src_names))
        tmp <- which(sapply(1:length(norm_tgt_names), function(j) {
          length(intersect(norm_src_names[[i]], norm_tgt_names[[j]])) > 0
        }))
        if(length(tmp) == 0)
          tmp <- NA
        result[[i]] <- tmp
      }
    }
  }


  ### build final result
  #idx <- which(sapply(result, length) > 1)
  i <- 1
  src_names[idx[i]]
  tgt_names[result[[idx[i]]]]
  if (all(sapply(result, length) == 1))
    result <- sapply(result, function(x) x[1])


  return(result)
}
