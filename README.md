# RugbyScope v1.2.0
Extraction and analysis of Rugby Union transfer networks

* Copyright 2024-2026 Vincent Labatut & David O'Sullivan

RugbyScope is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation. For source availability and license information see `licence.txt`

* Lab site: http://lia.univ-avignon.fr/ -- https://www.ul.ie/macsi
* GitHub repo: https://github.com/CompNet/RugbyScope
* Data: https://doi.org/10.5281/zenodo.21943537
* Contact: Vincent Labatut <vincent.labatut@univ-avignon.fr>, David O'Sullivan <david.osullivan@ul.ie>

-----------------------------------------------------------------------

## Description
This collection of `Python` and `R` scripts create the RugbyScope database from seven online data sources: Wikidata, DBpedia, and 5 language editions of Wikipedia (English, French, Italian, Japanese, and Spanish). RugbyScope is structured around three main rugby union-related entities: players, teams, and stints, with a focus on *male* players (due to the sparsity of available female player information). It covers all the data present in the sources at the time of retrieval. The scripts include the extraction of raw data from these sources, and their merging in a single database, solving a certain number of conflicts between them. 

<p align="center">
  <img src="./logo_rugbyscope.svg" width="30%">
</p>

This version of the database contains two variants of the rugby player stints:
* `full`: all the stints retrieved from the sources.
* `major`: only the stints played while above 18 year old.

Note that the retrieval part of this source code is provided mainly for documentation purposes: it was originally tailored to work with a certain configuration of our sources, taken
at a certain point in time. These sources are live material, and it is very unlikely that scripts will still work in the future, due to their unpredictable evolution. The RugbyScope data is also available on [Zenodo](https://doi.org/10.5281/zenodo.21943537).

If you use this source code or the associated data, please cite [[LO'26](#references)]:

```bibtex
@TechReport{Labatut2026,
  author      = {Labatut, Vincent and O'Sullivan, David J. P.},
  title       = {{R}ugby{S}cope: a Transfer-Oriented Database of Rugby Union Teams and Players},
  institution = {Avignon Universit\'e and University of Limerick},
  year        = {2026},
  type        = {Technical Report},
}
```



## Organization
This repository contains the following folders:
* `data`: input and output folders
  * `dbpedia`: intermediary files retrieved from DBpedia
  * `fusion`: tables resulting from the merging of all sources
<!--  * `networks`: various networks extracted from RugbyScope -->
  * `references`: various reference files used throughout the processing of raw data
  * `stats`: plots and CSV files produced during the descriptive analysis of RugbyScope
  * `wikidata`: intermediary files retrieved from Wikidata
  * `wikipedia`: intermediary files retrieved from the 5 considered language editions of wikipedia
  * `players.csv`, `stints.csv`, `teams.csv`: current version of RugbyScope, CSV format
  * `rugbyscope.sql`: current version of RugbyScope, SQL dump
  * `rugbyscope.sqlite`: current version of RugbyScope, binary SQLite dump
* `log`: log files
* `queries`: SPARQL queries used to retrieve the raw data from the knowledge bases
  * `dbpedia`: SPARQL queries used to retrieve the raw data from DBpedia
  * `wikidata`: SPARQL queries used to retrieve the raw data from Wikidata
* `src`: source code
  * `common`: various scripts and functions used by other scripts
  * `dbpedia`: extract and normalize data from DBpedia using the SPARQL queries
  * `fusion`: merge data retrieved from the 5 Wikipedia language editions
  * `sqlite`: create the proper relational DB based on the CSV files
  * `stats`: perform the descriptive analysis of RugbyScope, procude plots and stats
  * `wikidata`: extract and normalize data from Wikidata using the SPARQL queries
  * `wikipedia`: extract and normalize data from the 5 language editions of Wikipedia
* `build_database.R`: main script, that extract the data from each source, merge them, and conduct their descriptive analysis
* `install_packages.R`: install all the required `R` packages



## Installation

### `R` Environment
Install `R` and the required packages:

1. Install the [`R` language](https://www.r-project.org/)
2. Download this project from GitHub and unzip.
3. Install the required packages: 
   1. Open the `R` console.
   2. Set the current directory as the working directory, using `setwd("<my directory>")`.
   3. Run the install script `src/install_packages.R`.

### `Python` Environment
Install `Python` and the required packages:

1. Install the [`Python` language](https://www.python.org)
2. Execute `pip install -r requirements.txt` to install the required packages.



## Usage
To apply the extraction, cleaning and merging process described in the data paper [[LO'26](#references)]:

1. Open the `R` console.
2. Set the current directory as the working directory, using `setwd("<my directory>")`.
   3. Run the main script `src/build_database.R`.

The produced database goes in folder `data`, whereas the plots and stats resulting from its descriptive analysis go in folder `data/stats` (cf. Section [Organization](#organization)). In addition to this GitHub repository, the RugbyScope data is also available on [Zenodo](https://doi.org/10.5281/zenodo.21943537).

Note that it is *very unlikely* that these scripts will work in the future, due to the unpredictable evolution of the online sources from which our database is extracted (cf. Section [Description](#description)).



## Dependencies

### `R` Environment
Tested with `R` version 4.5.2, with the following packages:
* [`circlize`](https://cran.r-project.org/package=circlize): version 0.4.18
* [`corrplot`](https://cran.r-project.org/package=corrplot): version xx0.95xx
* [`DataExplorer`](https://cran.r-project.org/package=DataExplorer): version 0.9.0
* [`DBI`](https://cran.r-project.org/package=DBI): version 1.3.0
* [`dplyr`](https://cran.r-project.org/package=dplyr): version 1.2.1
* [`ggplot2`](https://cran.r-project.org/package=ggplot2): version 4.0.3
* [`httr`](https://cran.r-project.org/package=httr): version 1.4.8
* [`httr2`](https://cran.r-project.org/package=httr2): version 1.3.0
* [`igraph`](https://cran.r-project.org/package=igraph): version 2.3.3
* [`jsonlite`](https://cran.r-project.org/package=jsonlite): version 2.0.0
* [`magrittr`](https://cran.r-project.org/package=magrittr): version 2.0.5
* [`pheatmap`](https://cran.r-project.org/package=pheatmap): version 1.0.13
* [`polyglotr`](https://cran.r-project.org/package=polyglotr): version 1.7.4
* [`purrr`](https://cran.r-project.org/package=purrr): version 1.2.2
* [`RColorBrewer`](https://cran.r-project.org/package=RColorBrewer): version 1.1-3
* [`readr`](https://cran.r-project.org/package=readr): version 2.2.0
* [`readtext`](https://cran.r-project.org/package=readtext): version 0.92.1
* [`RSQLite`](https://cran.r-project.org/package=RSQLite): version 3.53.3
* [`skimr`](https://cran.r-project.org/package=skimr): version 2.2.2
* [`stringi`](https://cran.r-project.org/package=stringi): version 1.8.7
* [`stringr`](https://cran.r-project.org/package=stringr): version 1.6.0
* [`summarytools`](https://cran.r-project.org/package=summarytools): version 1.1.5
* [`tidyr`](https://cran.r-project.org/package=tidyr): version 1.3.2
* [`UpSetR`](https://cran.r-project.org/package=UpSetR): version 1.4.1
* [`viridis`](https://cran.r-project.org/package=viridis): version 0.6.5
* [`WikidataR`](https://cran.r-project.org/src/contrib/Archive/WikidataR/): version 2.3.3

### `Python` Environment
Tested with `Python` 3.13, with the following packages:
* [`beautifulsoup4`](https://pypi.org/project/beautifulsoup4/): version 4.15.0
* [`matplotlib`](https://pypi.org/project/matplotlib/): version 3.10.6
* [`numpy`](https://pypi.org/project/numpy/): version 2.3.2
* [`pandas`](https://pypi.org/project/pandas/): version 3.0.3
* [`requests`](https://pypi.org/project/requests/): version 2.32.5
* [`wptools`](https://pypi.org/project/wptools/): version 0.4.17



## Changelog
* `v1.0.0`: original version
* `v1.0.1`: improved the order of stints so that they match Wikipedia's, and also a few manual corrections.
* `v1.1.0`: a number of manual corrections, and two distinct variants of the DB: `full` (all data retrieved from the sources) vs. `major` (does not contain under-18 stints, and some stint years are inferred). 
* `v1.1.1`: fixed missing years in Barbarian stints, and other manual corrections. 
* `v1.2.0`: renamed `country` to `nation` for the sake of consistency. 



## References
* **[LO'26]** V. Labatut & D. O'Sullivan. *RugbyScope: a Transfer-Oriented Database of Rugby Union Teams and Players*, in submission, 2026. <!--⟨[hal-xxxxxxxx](https://hal.archives-ouvertes.fr/hal-xxxxxxxx)⟩ - DOI: [10.1142/S0219525922400033](http://doi.org/10.1142/S0219525922400033) -->
