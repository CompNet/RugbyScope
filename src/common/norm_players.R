########################################################################
# Functions used to normalize certain fields that describe rugby union
# players. This processing is generic, i.e. not tied to a specific data
# source.
#
# Vincent Labatut
# 01/2025
########################################################################




########################################################################
# Receives a vector of possible heights for a *single* rugby player, possibly
# expressed in a mix of metric and imperial units. The function tries
# first to identify a realistc metric value: if it cannot, it tries
# to convert imperial to metric, while obtaining a realistic value.
# If none of that is possible, it returns NA. If the height is expressed
# in meters, it is converted to centimeters.
#
# heights: original heights, in a mix of metric and imperial units.
#
# returns: most likely measure in metric unit, or NA if no realistic
#          value could be found or estimated.
########################################################################
get_clean_height <- function(heights) {
  # possibly convert to numeric values
  heights <- as.numeric(heights)
  heights <- heights[!is.na(heights)]

  # no numerical value available
  if (length(heights) == 0)
    height <- NA
  # comparing the numerical values
  else {
    # look for values within a realistic interval (meters & centimeters)
    lh <- which(heights > 1.5 & heights < 2.3 | heights > 150 & heights < 230)
    if (length(lh) > 0) {
      # found some: return a metric value
      height <- heights[lh[1]]
      # possibly convert meters to centimeters
      if (height < 2.3)
        height <- height * 100
    } else {
      # no realistic metric value found: try converting from imperial (inches & feet)
      lh <- which(heights > 60 & heights < 90 | heights > 4.92 & heights < 7.55)
      if (length(lh) > 0) {
        # found some: return a metric value
        height <- heights[lh[1]]
        # possibly convert inches to centimeters
        if (height > 60)
          height <- height * 2.54
        # possibly convert feet to centimeters
        else
          height <- height * 30.48
      } else
        # could not find any realistic value
        height <- NA
    }
  }

  return(height)
}




########################################################################
# Same as `get_clean_height` but for a set of rugby union players.
#
# heights: vector of strings, each one corresponding to a specific player,
#          and possibly containing several values representing his height,
#          separated by semicolons.
#
# returns: a vector of normalized heights in cm, or NA if no conversion was
#          possible.
########################################################################
get_clean_heights <- function(heights) {
  # splitting the strings
  all_heights <- strsplit(heights, "; ")

  # converting each vector to a single value
  result <- sapply(all_heights, get_clean_height)

  return(result)
}




########################################################################
# Receives a vector of possible weights for a *single* rugby player, possibly
# expressed in a mix of metric and imperial units. The function tries
# first to identify a realistc metric value: if it cannot, it tries
# to convert imperial to metric, while obtaining a realistic value.
# If none of that is possible, it returns NA.
#
# weights: original weights, in a mix of metric and imperial units.
#
# returns: most likely measure in metric unit, or NA if no realistic
#          value could be found or estimated.
########################################################################
get_clean_weight <- function(weights) {
  # possibly convert to numeric values
  weights <- as.numeric(weights)
  weights <- weights[!is.na(weights)]

  # no numerical value available for weights
  if (length(weights) == 0)
    weight <- NA

  # comparing the numerical values
  else {
    # look for values within a realistic interval (kilograms)
    lh <- which(weights > 60 & weights < 150)
    if (length(lh) > 0) {
      # found some: return a metric value
      weight <- weights[lh[1]]
    } else {
      # no realistic metric value found: try converting from imperial (pounds & stones)
      lh <- which(weights > 132.277 & weights < 330.693 | weights > 9.44838 & weights < 23.621)
      if (length(lh) > 0) {
        # found some: return a metric value
        weight <- weights[lh[1]]
        # possibly convert pounds to kilograms
        if (weight > 132.277)
          weight <- weight / 2.205
        # possibly convert feet to centimeters
        else
          weight <- weight * 6.350
      } else
        # could not find any realistic value
        weight <- NA
    }
  }

  return(weight)
}




########################################################################
# Same as `get_clean_weight` but for a set of rugby union players.
#
# weights: vector of strings, each one corresponding to a specific player,
#          and possibly containing several values representing his weight,
#          separated by semicolons.
#
# returns: a vector of normalized weights in kg, or NA if no conversion was
#          possible.
########################################################################
get_clean_weights <- function(weights) {
  # splitting the strings
  all_weights <- strsplit(weights, "; ")

  # converting each vector to a single value
  result <- sapply(all_weights, get_clean_weight)

  return(result)
}
