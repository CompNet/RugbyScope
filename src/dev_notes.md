# DATA EXTRACTION
* If we just focus on pro clubs, the careers will be very incomplete, the sequences very short (not useful)
  * [x] include amateur/university clubs too
* Must be able to assess the completeness of the data, in order to get long enough sequences
  * [x] list and add all clubs from tier 1 countries
* Wikidata = manual vs. DBpedia = auto extraction of info box
  * src: https://stackoverflow.com/questions/33862336/how-to-extract-information-from-a-wikipedia-infobox
* DBpedia data:
  * Seems possible to get the players' career, but this requires "careerStation" to be filled on DBP
    * see for instance https://dbpedia.org/page/Alexandre_Lacazette
    * it is the case for footballers, but apparently not for rugby players
  * Other DBpedia problem: the live version does not seem to work anymore. Is the DB even updated nowadays?
* Retrieve WP infobox data  
  * Python lib to extract infobox data (seems old): https://github.com/siznax/wptools
  * New WP API "enterprise": 
    * https://enterprise.wikimedia.com/blog/structured-contents-wikipedia-infobox/ 
    * https://enterprise.wikimedia.com/docs/on-demand/#article-structured-contents-beta
  * There's also a way using Panda: https://gist.github.com/aculich/b34868c098d94d614515


# DATABASE
* **Note:** we should have kept stint order and loan indications (esp. to extract networks)
  some stints cannot be ordered based on dates alone, e.g. 2015-2015 at X and 2015-2015 at Y
* [ ] Script to build the proper SQL database
* [ ] Loans:
  * [ ] Write a better function to order stints, that takes loans and intermediary stints into account (see section below)
  * [ ] write an algo to split appropriately loans and such, and list the cases that could not be split properly, then fix them
* Complement data:
  * [ ] Senior focus:
    * [ ] Remove stints and parts of stints before 18 yo (without changing stats), using birthdate (this should be a distinct version of the DB)
    * [ ] Handle cases where there are stints without dates and the first dated stints starts >18yo (could remove the updated stints)
  * [ ] Longer term:
    * [ ] Cross-ref with other data sources, to complement missing dates (esp. NA-NA in career start), see list below (all.rugby and so on)
    * [ ] Leverage an LLM to retrive missing information from WP articles' body, and other textual sources

# STATS & PLOTS
* Player stats
  * [x] Distribution of players by country
  * [x] Distribution of weight/height depending on position
  * [x] Evolution of the number of players based on birthdate (or activity?) and country
  * [x] Evolution of the number of stints based on start date and country
  * [x] Evolution of the height / weight over time
  * [ ] For larger position granularities, we could disaggregate: replace "Prop" by both types of prop, and so on
  * [x] Use WP links to plot data sources (static and dynamic)
  * [x] Cross-ref sources and countries: is there a match?
* Team stats
  * [x] Distribution of teams by country
  * [x] Use WP links to plot data sources (static and dynamic)
  * [x] Cross-ref sources and countries: is there a match?
* Stint stats
  * [x] Distribution of stints by source and country (both players and teams)
  * [x] Distribution of stints by source
    * [x] Find a way to show how redundant they are
  * [x] Cross-ref sources and countries: is there a match?
* [ ] Add WW1 & WW2 to the evolution plots
  * [ ] Japanes worldcup date? (to explain increase in jaWP?)

# DATA AUGMENTATION FOR NET EXTRACTION
For net extraction, we need all the dates to be filled, and we can allow more approximation when estimating them.
* [ ] merge junior/senior consecutive stints with the same team (as this distinction is not useful for network extraction)
* [ ] split overlapping stints: 
  * [ ] clubs/franchises: must be handled separalely: assume loans, but also look for specific situations like xxxx-2015 at X, then 2015-2015 at Y, then 2015-xxxx at X again
  * [ ] regions: parallel system, look for overlaps between regions rather than regions vs. clubs
  * [ ] invitational teams: no overlapping problem, completely distinct system
  * [ ] national teams: overlapping with youth teams is possible, handle them in parallel
* [ ] missing start years:
    * [ ] first stint of the career: use the existing DB script, but without the existing constraints (cf. comments in script)
    * [ ] other stints: use end year of the preceding stint, if present. otherwise: average stint duration for this team? or for the considered player?
* [ ] missing end years:
    * [ ] last stint of the career: use an approximate age limit : 30 years? average career duration? 2026 for stints starting in 2024 and after?
    * [ ] other stints: leverage start year of the following stint, if present. otherwise: average stint duration for this team? or for the considered player?

# BETTER STINT ORDER
* current method: 
  1. replace NA startyears by 0000 and NA endyears 9999
  2. sort by first by startyear, then by endyear, then by team
  Observations: 
  * NA-NA grouped at the beginning, which is ok
  * loan are handled correctly, as 2000-2005 comes before 2000-2002
  * but situations like 2010-2015 @X, 2015-2015 @Y and 2015-2018 @X are not properly treated: this is not a loan, but a short intermediary stint
  * club, regional, invitational and national stints are mixed > we should separate them (in this order)

# ALTERNATIVE DATABASES
These are the alternative databases, which could potentially be used to check the completeness of ours, and/or complement it:
* **All.Rugby:** https://all.rugby/
  * Seems to include transfers; looks incomplete but worth exploring
  * They state explicitly that it's not possible to use their data like we want
* **Archie's Rugby Union Database:** https://www.archiesrugbyuniondatabase.com/
  * Commercial, doesn't work very well, and test-focused
* **ESPN Scrum:** http://stats.espnscrum.com/statsguru/rugby/stats/index.html
  * Seems to be the best, but does not work anymore, and no dump found online
  * But the focus seems to be on international matches: "Our unrivalled record of international rugby, produced by statistician Stuart Farmer, includes every Test match ever played plus individual records for every player and thanks to the powerful Statsguru tool you are now able to analyse this wealth of information like never before."
  * Alt URL: https://www.espn.com/rugby/
* **It's Rugby:** https://www.itsrugby.co.uk/
  * Seems very good, but commercial
  * No legal notice on the website, though
  * Maybe we could ask them the authorization to retrieve their publicly accessible data?
  * Seems to have been (partially) scrapped here: https://www.kaggle.com/datasets/patricknaylor/its-rugby-player-data
* **Pick & Go:** https://www.lassen.co.nz/pickandgo.php
  * Only international matches (and Super Rugby), commercial access
* **Rugby Database:** https://www.rugbydatabase.co.nz/
  * Incomplete, but free access (focus on NZ though?)
  * But the info is incorrect for the few cases I checked (no Castres for Dupont, and he starts in 2020 instead of 2017 at Toulouse...)
* **RugbyPass:** https://www.rugbypass.com/
  * Seems very basic, no career steps
* **RugbyStats.com:** http://www.rugbydata.com/
  * This one seems dead
* **StatBunker's Rugby Stats:** 
  * Seems good enough, covers the major competitions
  * They also have the referees, maybe there's something to do with that...
  * Not up to date for certain competitions (ProD2 stops in 2022), seems English-oriented
  * No explicit info about data rights on the website
  * Do not see any contact info either
  * CEO: https://www.linkedin.com/in/stephen-mccormack-5929a02b/
  * LinkedIn: https://www.linkedin.com/in/statbunker-live-stats-52a016a6/?originalSubdomain=uk
  * Twitter: https://x.com/Statbunker
  * Site contact form: https://rugby.statbunker.com/usual/Feedback
  https://rugby.statbunker.com/players/getPlayerHistory?player_id=8586
* **Ultimate Rugby:** https://www.ultimaterugby.com
  * Not great, seems like a copy of WP data, eg. https://www.ultimaterugby.com/antoine-dupont
