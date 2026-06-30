###############################
# - DATABASE
#   - notes:
#     - we should have kept stint order and loan indications (esp. to extract networks)
#     - write an algo to split appropriately loans and such, and list the cases that could not be split properly?
#     - produce a raw version, and one where we try to complement missing data with heuristics?
#   - complement data:
#     - remove stints and parts of stints before 18 yo (without changing stats), using birthdate
#       > this should be a separate DB
#     - complement missing U18 (and other youth teams) dates using player's age
#     - and do the opposite: complement missing birthdates using Uxx stints
#       U18~17yo - U19~18yo - U20~18/19yo - U21~19/20yo
#     - use other sources to complement missing dates (esp. NA-NA in career start)? https://www.allrugby.com/
#     - handle cases where there are stints without dates and the first dated stints starts >18yo (could remove the updated stints)
#     - add missing values : dates, others ?
#       > use LLM to retrive them from WP?
###############################
# - STATS
#   - evolution of number of players based on birthdate and country
#   - evolution of the number of stints based on start date and country
#   - distribution of players/teams by country (static)
#   - distribution of stints by source (static)
#     - find a way to show how redundant they are
#     - compare evolution of number of player/team/stint *by data source*
###############################
# - DATA AUGMENTATION FOR NET EXTRACTION
#   - complement missing dates / split depending on loans, etc.
#     note : overlapping stints at clubs = loans
#            ...but there are also just simultaneous membership
#   - merge junior/senior consecutive stints with the same team (only bc not useful for network extraction)
# - missing end years:
#   - leverage present start year in following stint
#     with a limit of 30 years for the very last missing end year? (or average career duration)
# - missing LAST end years:
#   - use average stint duration for this team? or for the considered player?
#   - possibly put 2025 or 2026 for current players?
###############################
