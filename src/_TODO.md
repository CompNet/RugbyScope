* Stats
  * active players stats with *filtering* process
  * fix country colors (ireland=green, france=blue, etc.)
  * histo number of teams by country, distinguishing WD/non-WD


- career steps with missing end date: 
  try to fill based on next step start date?


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
      They state explicitly that it's not possible to use their data like we want
      https://all.rugby/
    - Archie's Rugby Union Database: commercial, doesn't work very well, and test-focused
      https://www.archiesrugbyuniondatabase.com/
    - ESPN Scrum: seems to be the best, but does not work anymore, and didn't find any dump
      but the focus seems to be on international matches: "Our unrivalled record of international rugby, produced by statistician Stuart Farmer, includes every Test match ever played plus individual records for every player and thanks to the powerful Statsguru tool you are now able to analyse this wealth of information like never before."
      http://stats.espnscrum.com/statsguru/rugby/stats/index.html
      https://www.espn.com/rugby/
    + It's Rugby: very good, but commercial (no legal notice on the website, though). 
      Maybe we could ask them to retrieve their publicly accessible data?
      https://www.itsrugby.co.uk/
      Seems (partially) scrapped here: https://www.kaggle.com/datasets/patricknaylor/its-rugby-player-data
    - Pick & Go: only international matches (and Super Rugby), commercial access
      https://www.lassen.co.nz/pickandgo.php
    ~ Rugby Database: incomplete, but free access (focus on NZ?)
      But the info is incorrect for the few cases I checked (no castres for dupont, and he starts in 2020 instead of 2017 at tlse...)
      https://www.rugbydatabase.co.nz/
    - RugbyPass: seems very basic, no career steps
      https://www.rugbypass.com/
    - RugbyStats.com: this one seems dead
      http://www.rugbydata.com/
    ~ StatBunker's Rugby Stats: seems good enough, covers the major competitions
      They also have the referees, maybe there's something to do with that...
      Not up to date for certain competitions (ProD2 stops in 2022), seems English-oriented
      No info about data rights
      https://rugby.statbunker.com/players/getPlayerHistory?player_id=8586
    - Ultimate Rugby: not great, seems like a copy of WP data
      https://www.ultimaterugby.com/antoine-dupont
