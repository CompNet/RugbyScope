* Data retrieveal
  1. Update WD tables with latest queries
  2. Merge DBP in WD (when missing data)
  3. Normalize all heterogeneous fields (position, country, etc.)
  4. Club names should be normalized too? (or at least, normalizing function used ad hoc)
  5. Query WP for each player in our table, trying to connect with WD entities using WP page URLs.
* Stats
  * active players stats with *filtering* process
  * fix country colors (ireland=green, france=blue, etc.)
  * histo number of teams by country, distinguishing WD/non-WD



* NOTES
  - Filtering players depending on whether they played for a club: many are missed
    But if their club is not indicated... maybe they are not important enough?
    We should get them with WP, though. So better to keep them.
  - If we just focus on pro clubs, the careers will be very incomplete, the sequences very short (not useful)
    > include amateur/university clubs too
  - Must be able to assess the completeness of the data, in order to get long enough sequences
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
