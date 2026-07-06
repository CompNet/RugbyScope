# DATABASE
* Note: we should have kept stint order and loan indications (esp. to extract networks)
  some stints cannot be ordered based on dates alone, e.g. 2015-2015 at X and 2015-2015 at Y
* [ ] Loans:
  * [ ] Write a better function to order stints, that tales loan and intermediary stint into account
  * [ ] write an algo to split appropriately loans and such, and list the cases that could not be split properly, then fix them
* Complement data:
  * [ ] Senior focus:
    * [ ] Remove stints and parts of stints before 18 yo (without changing stats), using birthdate (this should be a separate DB)
    * [ ] Handle cases where there are stints without dates and the first dated stints starts >18yo (could remove the updated stints)
  * [ ] Longer term:
    * [ ] Cross-ref with other data sources, to complement missing dates (esp. NA-NA in career start), ex. https://www.allrugby.com/
    * [ ] Leverage LLM to retrive missing information from WP articles' body, and other textual sources

# STATS
* Player stats
  * [x] distribution of players by country
  * [x] distribution of weight/height depending on position
  * [x] evolution of the number of players based on birthdate (or activity?) and country
  * [x] evolution of the number of stints based on start date and country
  * [x] evolution of the height / weight over time
  * [ ] for larger position granularities, we could replace "Prop" by both types of prop, and so on
  * [ ] use WP links to plot data sources (static and dynamic)
* Team stats
  * [x] distribution of teams by country
  * [ ] use WP links to plot data sources (static and dynamic)
* Stint stats
  * [ ] distribution of stints by source and country (both players and teams)
  * [x] distribution of stints by source
    * [x] find a way to show how redundant they are
  * [ ] compare evolution of number of player/team/stint *by data source*

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

# Notes on a better stint order
- current method: 
  1. replace NA startyears by 0000 and NA endyears 9999
  2. sort by first by startyear, then by endyear, then by team
  Observations: 
  - NA-NA grouped at the beginning, which is ok
  - loan are handled correctly, as 2000-2005 comes before 2000-2002
  - but situations like 2010-2015 @X, 2015-2015 @Y and 2015-2018 @X are not properly treated: this is not a loan, but a short intermediary stint
  - club, regional, invitational and national stints are mixed > we should separate them (in this order)
