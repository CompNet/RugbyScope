* Data retrieval
  * manually check that all teams from main pro leagues are present
    maybe just plot the number of clubs from each country?

* Network extraction
  * improve removal of amateur clubs
    currently removing clubs without an affiliation and a competition
    but that is probably too strict (ex. it removes many English clubs)
  * remove university clubs (cf Japan)
    'university' could be used to select them, 
    but some clubs use this word without being tied to a uni anymore (ex. Lyon)
    Also, some country use university clubs as amateur clubs (JP, ZA)

* Stats
  * active players stats with *filtering* process
  * fix country colors (ireland=green, france=blue, etc.)
  * histo number of teams by country, distinguishing WD/non-WD



* NOTES
  - filtering players depending on whether they played for a club: 
    many are missed
    but if their club is not indicated... maybe they are not important enough?
  - if we just focus on pro clubs, the careers will be very incomplete, the sequences very short (not useful)
    > must try to include amateur/university clubs too
  - must be able to assess the completeness of the data
    > list all clubs (at least in tier 1 countries)
  - Wikidata = manual vs. DBpedia = auto extraction of info box
    src: https://stackoverflow.com/questions/33862336/how-to-extract-information-from-a-wikipedia-infobox
  - DBpedia data:
    - seems possible to get the players' career, but this requires "careerStation" to be filled on DBP
      https://dbpedia.org/page/Alexandre_Lacazette
      it is the case for footballers, but apparently not for rugby players
      see on https://dbpedia.org/sparql :
PREFIX dbpedia: <http://dbpedia.org/resource/>
SELECT 
  ?playerName ?station
WHERE
{ ?player rdf:type dbo:RugbyPlayer.
#{ BIND(dbpedia:Antoine_Dupont AS ?player).
#{ BIND(dbpedia:Alexandre_Lacazette AS ?player).
  ?player rdfs:label ?playerName;
              dbo:careerStation ?station.
}
ORDER BY ?playerName
    - other DBpedia problem: the live version does not seem to work anymore
    > is the DB even updated nowadays?
  - retrieve infobox data  
    - Python lib to extract infobox data (seems old): https://github.com/siznax/wptools
    - New WP API "enterprise": 
      - https://enterprise.wikimedia.com/blog/structured-contents-wikipedia-infobox/ 
      - https://enterprise.wikimedia.com/docs/on-demand/#article-structured-contents-beta
    - There's also a way using Panda: https://gist.github.com/aculich/b34868c098d94d614515
