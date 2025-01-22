* Stats
  * active players stats with *filtering* process
  * fix country colors (ireland=green, france=blue, etc.)
  * histo number of teams by country, distinguishing WD/non-WD

- rename all the "club" variables to "team", for consistency
- normalize the type of team:
  - club/franchise
  - national senior
  - national senior B
  - youth, all age categories


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
      see for instance https://dbpedia.org/page/Alexandre_Lacazette
      it is the case for footballers, but apparently not for rugby players
    - other DBpedia problem: the live version does not seem to work anymore
    > is the DB even updated nowadays?
  - retrieve infobox data  
    - Python lib to extract infobox data (seems old): https://github.com/siznax/wptools
    - New WP API "enterprise": 
      - https://enterprise.wikimedia.com/blog/structured-contents-wikipedia-infobox/ 
      - https://enterprise.wikimedia.com/docs/on-demand/#article-structured-contents-beta
    - There's also a way using Panda: https://gist.github.com/aculich/b34868c098d94d614515
  - Possible databases:
    - All.Rugby: seems to include transfers; looks incomplete but worth exploring.
      https://all.rugby/
    - Archie's Rugby Union Database: commercial, doesn't work very well, and test-focused
      https://www.archiesrugbyuniondatabase.com/    - ESPN Scrum: apparently dead
      https://www.espn.com/rugby/
    - It's Rugby: very good, but commercial
      
    - Pick & Go: only international matches (and Super Rugby), commercial access
      https://www.lassen.co.nz/pickandgo.php
    - RugbyStats.com: this one seems dead
      http://www.rugbydata.com/
    - Rugby Database: incomplete, but free access (focus on NZ?)
      https://www.rugbydatabase.co.nz/
    - StatBunker's Rugby Stats: seems good enough, covers the major competitions
      They also have the referees, maybe there's something to do with that...
      https://rugby.statbunker.com/players/PlayerDisciplineVsClubs?player_id=8586
