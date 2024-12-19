# Tests of the WikidataR package.
# 
# !!!! DON'T RUN THAT !!!!
#
# Vincent Labatut
# 12/2024
########################################################################
library("WikidataR")
library("igraph")

# irish internationals
results <- query_wikidata(
'SELECT DISTINCT ?player ?playerLabel
WHERE
{
  ?player wdt:P106 wd:Q14089670;        # P106=occupation, Q14089670=Rugby Union player
          wdt:P413/wdt:P31 wd:Q6583019; # P54=position played on team / speciality, 
          wdt:P54 wd:Q599903.	        # P54=member of sports team, Q599903=Ireland national rugby union team
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],mul,en". } # just to get the label right
}')




res1 <- query_wikidata(
'SELECT DISTINCT ?player ?playerLabel
WHERE
{
  ?player wdt:P641 wd:Q5849;     # P641=sport, Q5849=Rugby Union
          wdt:P54 ?team.	     # P54=member of sports team, 
  ?team wdt:P31 wd:Q43009164.	 # P31=instance of, Q43009164=Rugby Union club
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],mul,en". } # just to get the label right
}')

res2 <- query_wikidata(
'SELECT DISTINCT ?player ?playerLabel
WHERE
{
  ?player wdt:P106 wd:Q14089670; # P106=occupation, Q14089670=Rugby Union player
          wdt:P54 ?team.	     # P54=member of sports team, 
  ?team wdt:P31 wd:Q43009164.	 # P31=instance of, Q43009164=Rugby Union club
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],mul,en". } # just to get the label right
}')
diff <- setdiff(res2$player, res1$player)


res3 <- query_wikidata(
'SELECT DISTINCT ?player ?playerLabel
WHERE
{
  ?player wdt:P106 wd:Q14089670; # P106=occupation, Q14089670=Rugby Union player
          wdt:P413 ?position;    # P413=position played on team / speciality, 
          wdt:P54 ?team.	     # P54=member of sports team, 
  ?position wdt:P31 wd:Q6583019. # P31=instance of, Q6583019=rugby union position
  ?team wdt:P31 wd:Q43009164.	 # P31=instance of, Q43009164=Rugby Union club
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],mul,en". } # just to get the label right
}')
diff <- setdiff(res2$player, res3$player)






irish_inter1 <- query_wikidata(
'SELECT DISTINCT ?player ?playerLabel
WHERE
{
  ?player wdt:P106 wd:Q14089670;        # P106=occupation, Q14089670=Rugby Union player
          wdt:P54 wd:Q599903.	        # P54=member of sports team, Q599903=Ireland national rugby union team
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],mul,en". } # just to get the label right
}')

irish_inter2 <- query_wikidata(
'SELECT DISTINCT ?player ?playerLabel
WHERE
{
  ?player wdt:P106 wd:Q14089670;        # P106=occupation, Q14089670=Rugby Union player
          wdt:P413/wdt:P31 wd:Q6583019; # P54=position played on team / speciality, 
          wdt:P54 wd:Q599903.	        # P54=member of sports team, Q599903=Ireland national rugby union team
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],mul,en". } # just to get the label right
}')
diff <- setdiff(irish_inter1$player, irish_inter2$player)





# complete extraction of irish players
complt <- query_wikidata(#format="smart",
'SELECT DISTINCT ?player ?playerLabel
  (GROUP_CONCAT(DISTINCT ?firstnameLabel; SEPARATOR=", ") as ?firstnameLabels)
  (GROUP_CONCAT(DISTINCT ?lastnameLabel; SEPARATOR=", ") as ?lastnameLabels)
  ?sexLabel 
  (MAX(?dob) AS ?dobMax) (CONCAT(STR(YEAR(?dobMax)),"-",STR(MONTH(?dobMax)),"-",STR(DAY(?dobMax))) AS ?dobFormat)
  (GROUP_CONCAT(DISTINCT ?pobLabel; SEPARATOR=", ") as ?pobLabels)
  (MAX(?dod) AS ?dodMax) (CONCAT(STR(YEAR(?dodMax)),"-",STR(MONTH(?dodMax)),"-",STR(DAY(?dodMax))) AS ?dodFormat)
  (GROUP_CONCAT(DISTINCT ?podLabel; SEPARATOR=", ") as ?podLabels)
  (GROUP_CONCAT(DISTINCT ?citizenshipLabel; SEPARATOR=", ") as ?citizenshipLabels)
  (GROUP_CONCAT(DISTINCT ?sportCountryLabel; SEPARATOR=", ") as ?sportCountryLabels)
  (GROUP_CONCAT(DISTINCT ?positionLabel; SEPARATOR=", ") as ?positionLabels)
  ?careerStart ?careerEnd
  ?mass ?height
  ?ESPNscrumID ?AllRugbyID ?GoogleKnowlID ?ItsRugbyID ?RugbyDatabaseID
WHERE
{
  ?player wdt:P106 wd:Q14089670; # P106=occupation, Q14089670=Rugby Union player
          wdt:P54 wd:Q599903.	 # P54=member of sports team, Q599903=Ireland national rugby union team
  OPTIONAL{?player wdt:P735 ?firstname.}
  OPTIONAL{?player wdt:P734 ?lastname.}
  OPTIONAL{?player wdt:P21 ?sex.}
  OPTIONAL{?player wdt:P569 ?dob.}
  OPTIONAL{?player wdt:P19 ?pob.}
  OPTIONAL{?player wdt:P570 ?dod.}
  OPTIONAL{?player wdt:P20 ?pod.}
  OPTIONAL{?player wdt:P27 ?citizenship.}
  OPTIONAL{?player wdt:P1532 ?sportCountry.}
  OPTIONAL{?player wdt:P413 ?position.}
  OPTIONAL{?player wdt:P2031 ?careerStart.}
  OPTIONAL{?player wdt:P2032 ?careerEnd.}
  OPTIONAL{?player wdt:P2067 ?mass.}
  OPTIONAL{?player wdt:P2048 ?height.}
  OPTIONAL{?player wdt:P858 ?ESPNscrumID.}
  OPTIONAL{?player wdt:P9903 ?AllRugbyID.}
  OPTIONAL{?player wdt:P2671 ?GoogleKnowlID.}
  OPTIONAL{?player wdt:P3769 ?ItsRugbyID.}
  OPTIONAL{?player wdt:P12335 ?RugbyDatabaseID.}
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],mul,en". 
                           ?player rdfs:label ?playerLabel.
                           ?firstname rdfs:label ?firstnameLabel.
                           ?lastname rdfs:label ?lastnameLabel.
                           ?sex rdfs:label ?sexLabel.
                           ?pob rdfs:label ?pobLabel.
                           ?pod rdfs:label ?podLabel.
                           ?citizenship rdfs:label ?citizenshipLabel.
                           ?sportCountry rdfs:label ?sportCountryLabel.
                           ?position rdfs:label ?positionLabel.
                         } # just to get the label right
}
GROUP BY ?player ?playerLabel ?sexLabel ?dobFormat ?dodFormat ?careerStart ?careerEnd ?mass ?height ?ESPNscrumID ?AllRugbyID ?GoogleKnowlID ?ItsRugbyID ?RugbyDatabaseID
')
complt <- complt[,-which(colnames(complt)=="dobMax")]
complt <- complt[,-which(colnames(complt)=="dodMax")]
write.csv(complt, file.path("out","irish_intnl_players_descr.csv"))
print(apply(complt,2,class))
print.data.frame(complt[1:10,])

doubles <- names(which(table(complt$player)>1))
for(double in doubles)
{   idx <- which(complt$player==double)
    print(idx)
    print.data.frame(complt[idx,])
    print(colnames(complt)[which(complt[idx[1],]!=complt[idx[2],])])
}



# # get the careers of these players
# players <- complt$player
# careers <- NA
# for(player in players){ 
#   career <- query_wikidata(
#   paste0('SELECT DISTINCT ?clubLabel 
#   (STR(YEAR(?startDate)) AS ?startYear)
#   (STR(YEAR(?endDate)) AS ?endYear)
#   ?played
#   ?points
# WHERE
# { ',player,' p:P54 ?statement.                 # P54=member of sports team, statement=relation between player/club
#   ?statement ps:P54 ?club.                  # ps:=object of the relation, pq:=qualifier of the relation
# #  ?club wdt:P31 wd:Q43009164.	            # P31=instance of, Q43009164=Rugby union club >> disabling this line means retrieving junior clubs and international teams
#   OPTIONAL{?statement pq:P580 ?startDate;}  # P580=start time
#   OPTIONAL{?statement pq:P582 ?endDate.}    # P582=end time
#   OPTIONAL{?statement pq:P1350 ?played.}    # P1350=number of matches played
#   OPTIONAL{?statement pq:P1351 ?points.}    # P1351=number of points/goals/set scored
 
#   SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],mul,en". } # just to get the label right
# }
# '))
#   if(all(is.na(careers)))
#     careers <- career
#   else
#     careers <- rbind(careers, career)
# }

# better in a single query
careers <- query_wikidata(
'SELECT DISTINCT 
  ?player ?playerLabel
  ?club ?clubLabel 
  (STR(YEAR(?startDate)) AS ?startYear)
  (STR(YEAR(?endDate)) AS ?endYear)
  ?played
  ?points
WHERE
{ #BIND(wd:Q3547810 AS ?player)
  
  # list of players
  ?player wdt:P106 wd:Q14089670;            # P106=occupation, Q14089670=Rugby Union player
          wdt:P54 wd:Q599903.	            # P54=member of sports team, Q599903=Ireland national rugby union team
  
  # their club contracts
  ?player p:P54 ?statement.                 # P54=member of sports team, statement=relation between player/club
  ?statement ps:P54 ?club.                  # ps:=object of the relation, pq:=qualifier of the relation
  ?club wdt:P31 wd:Q43009164.	            # P31=instance of, Q43009164=Rugby union club >> disabling this line means retrieving junior clubs and international teams
  OPTIONAL{?statement pq:P580 ?startDate;}  # P580=start time
  OPTIONAL{?statement pq:P582 ?endDate.}    # P582=end time
  OPTIONAL{?statement pq:P1350 ?played.}    # P1350=number of matches played
  OPTIONAL{?statement pq:P1351 ?points.}    # P1351=number of points/goals/set scored
 
  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],mul,en". } # just to get the label right
}
ORDER BY ?player ?startYear
')
write.csv(careers, file.path("out", "irish_intnl_players_careers.csv"))
print(apply(careers, 2, class))
print.data.frame(careers[1:10,])

# extract club network
idx <- which(is.na(careers$startYear))
el <- NA
filt_careers <- careers[-idx,]
current_player <- filt_careers[1, "player"]
last_club <- filt_careers[1, "clubLabel"]
row <- 2
# loop over each career step
while(row <= nrow(filt_careers)) {
  cat("Processing career step ", row, "/", nrow(filt_careers), "\n", sep="")
  cat(filt_careers$player[row], ", ", filt_careers$clubLabel[row], "\n", sep="")

  # next step of the previous player
  if (filt_careers$player[row] == current_player) {
    new_club <- filt_careers$clubLabel[row]
    if(last_club != new_club) {
      if (all(is.na(el))) {
        el <- matrix(c(last_club, new_club), nrow = 1, ncol = 2)
        weights <- 1
        colnames(el) <- c("From", "To")
      } else {
        idx <- which(el[,"From"] == last_club & el[,"To"] == new_club)
        if (length(idx) == 0) {
          el <- rbind(el, c(last_club, new_club))
          weights <- c(weights, 1)
        } else {
          weights[idx] <- weights[idx] + 1
        }
      }
      last_club <- new_club
    }
  } else {
    # starting to process a different player
    current_player <- filt_careers$player[row]
    last_club <- filt_careers$clubLabel[row]
  }
  row <- row + 1
}
# init graph
g <- graph_from_edgelist(el, directed=TRUE)
plot(g)
write.graph(g, file = file.path("out", "irish_intnl_transfers.graphml"), format = "graphml")
