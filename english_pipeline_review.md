# `src/wikipedia/english/` — Pre-upload review

A read-only review of the English-Wikipedia scraping code ahead of the shared GitHub upload.
**No source files were modified.** This document only describes what is there, how it fits together, and what to consider before pushing.

Folder reviewed: `src/wikipedia/english/`
Date: 2026-07-10

---

## 1. Quick answer: what to upload vs. what is a test file

The folder holds three kinds of file: the **production pipeline** (keep), **superseded/duplicate versions** (pick one), and **experimental / one-player debugging scripts** (the `x_` files — not for upload). There is also compiled bytecode that should not be in git at all.

### Upload — production pipeline

| File | Role |
|---|---|
| `_setup.py` | Core engine. ~45 functions (scrape, parse, clean, convert). Imported by every other script via `from _setup import *`. **Essential.** |
| `01_get_capped_players_urls.py` | Step 1 — builds the per-nation lists of player profile URLs. |
| `02_download_all_capped_players_profiles.py` | Step 2 — download + parse + standardise + combine, for **capped** players. |
| `03_download_all_wikidate_players.py` | Step 3 — same as step 2, but for the **Wikidata** player list. |
| `04_02_complie_json_to_csv.py` | Step 4 — combined JSON → `player_info` + `stint_info` CSVs. Newest of the three `04` versions. |
| `clean_tables.R` | Step 5 (collaborator, Vincent) — cleans the raw CSV tables. |
| `integrate_data.R` | Step 6 (collaborator, Vincent) — merges the clean tables into the fusion tables. |

### Decide before upload — duplicate / superseded versions

| File | Situation | Suggested action |
|---|---|---|
| `02_02_download_all_capped_players_profiles.py` | **Newer, better** downloader than `02`: proper User-Agent, rate limiting, exponential backoff, resume (skips already-downloaded files). But only the *download* stage is active — the parse/standardise/combine functions are commented out. | Not a drop-in replacement for `02` yet. Either port `02_02`'s improved downloader into `02`, or keep `02_02` only for downloading and run `02` for the later stages. Don't upload both as-is without a note. |
| `04_complie_json_to_csv.py` | Oldest `04`. Points at `..._test.json` / `..._x.csv` (test outputs). Currently the **only `04` in git**. | Superseded — drop in favour of `04_02`. |
| `04_01_complie_json_to_csv.py` | Middle `04`. Points at `..._2.csv`. | Superseded — drop in favour of `04_02`. |

The three `04` files are byte-for-byte identical except for the input/output filenames. See §4 for the fix (one parameterised file).

### Do NOT upload — experimental / debugging scripts

All `x_`-prefixed files are ad-hoc scripts for testing the parser on a single player or one tricky field. They import `_setup` and re-run pipeline code with hardcoded example URLs. They are not part of the pipeline.

`x_test.py`, `x_testing.py`, `x_testing_benprescott.py`, `x_testing_dan_coles.py`, `x_testing_jimmy.py`, `x_testing_large_number_mathces_played.py`, `x_testing_profile_builder.py`, `x_testing_stint.py`, `x_testing_weight.py`, `x_03_creating datasets.py`, `x_04_scrapping_wikidata_pro_players.py`, `x_fixing_footnote.py`, `x_fixing_match_numbers`

Recommendation: move these to a `sandbox/` or `scratch/` folder that is git-ignored, or delete the ones you no longer need. If any encode useful test cases (e.g. the weight/footnote/match-number edge cases), consider promoting them into a proper `tests/` folder later — but that is a separate job.

### Your call — QA script

| File | Note |
|---|---|
| `__checking_data.R` | Your data-QA / sanity-checking script (counts, overlaps, plots on the output CSVs). Useful but exploratory. Keep it if you want the checks version-controlled — ideally in a `checks/` subfolder — otherwise leave it out. |

### Remove from git — should never have been tracked

| Item | Note |
|---|---|
| `__pycache__/_setup.cpython-311.pyc` | Compiled Python bytecode. It is **currently committed**. Add `__pycache__/` (or `*.pyc`) to `.gitignore` and untrack it: `git rm -r --cached src/wikipedia/english/__pycache__`. |

---

## 2. Already in git vs. new

Worth knowing before you push: several test files and the bytecode are **already committed** from earlier, so "getting ready for upload" is partly about *removing* things, not just adding.

**Already tracked (and modified since last commit):** `_setup.py`, `01_…py`, `02_download…py`, `03_…py`, `04_complie…py`, `clean_tables.R`, `integrate_data.R`, `__pycache__/_setup…pyc`, and the test files `x_03_creating datasets.py`, `x_04_scrapping_wikidata_pro_players.py`, `x_testing.py`, `x_testing_benprescott.py`, `x_testing_dan_coles.py`, `x_testing_jimmy.py`, `x_testing_profile_builder.py`, `x_testing_stint.py`, `x_testing_weight.py`.

**New / untracked:** `02_02_download…py`, `04_01_complie…py`, `04_02_complie…py`, `__checking_data.R`, `x_fixing_footnote.py`, `x_fixing_match_numbers`, `x_test.py`, `x_testing_large_number_mathces_played.py`.

So the test scripts and the `.pyc` are already public in the repo history; deciding to drop them means an explicit removal commit.

---

## 3. How the files work together (the pipeline)

Everything hangs off one shared module, `_setup.py`. Each numbered script imports it (`from _setup import *`), reads an input list, and writes to `data/wikipedia/english/`. Scripts run **top-to-bottom** (there is no `if __name__ == "__main__"` guard — the functions are both defined and called in the same file), and all paths are relative to the **repository root**, so they must be run from there.

```
                        ┌─────────────────────────────────────────┐
                        │  _setup.py  (shared engine, ~45 funcs)   │
                        │  scraping · parsing · cleaning · CSV     │
                        └─────────────────────────────────────────┘
                            ▲          ▲          ▲          ▲
             from _setup import *  (imported by every script below)

  INPUT LISTS                         PIPELINE                             OUTPUT
  ───────────                         ────────                            ──────

  International_players_          01_get_capped_players_urls.py
  list_by_contry.csv        ─▶    scrape each nation's "capped         profile_links/
                                  players" wiki table                  profile_links_<Nation>.csv
                                                                         │
                                                                         ▼
                                  02_download_all_capped_             <Nation>/player_html/*.html
                                  players_profiles.py                 <Nation>/PP/*.json
                                  download HTML → parse → fix →       <Nation>/PP_cleaned/*.json
                                  standardise → combine                  │
                                  (02_02 = improved downloader only)     ▼
                                                                       all_capped_players_profile.json

  Wikidata_players.csv      ─▶    03_download_all_wikidate_           <Nation>_2/PP_cleaned/*.json
                                  players.py                             │
                                  (same stages, Wikidata list)           ▼
                                                                       all_wikidata_players_profile_4.json
                                                                         │
                                                                         ▼
  all_*_players_profile.json ─▶   04_02_complie_json_to_csv.py       PP_player_info_*.csv
                                  flatten JSON → two tables          PP_stint_info_*.csv
                                                                         │
                                                                         ▼  (manual rename/move to raw/)
                                  clean_tables.R                      raw/player_info4.csv,
                                  load raw tables, normalise          raw/stint_info3.csv → players.csv,
                                  names/teams/positions/locations       stints.csv
                                                                         │
                                                                         ▼
                                  integrate_data.R                    data/fusion/*_eswp.csv
                                  merge into the shared fusion tables
```

### Stage by stage

**Step 1 — `01_get_capped_players_urls.py`.** Reads `data/wikipedia/International_players_list_by_contry.csv` (one row per nation, with the URL of that nation's "list of capped players" page). For each nation it scrapes the wiki table and writes `data/wikipedia/english/profile_links/profile_links_<Nation>.csv` (player name + Wikipedia URL).

**Step 2 — `02_download_all_capped_players_profiles.py`.** The heart of the scrape, run as a sequence of stages inside the one file:
- `get_all_htmls()` — download each player's HTML into `<Nation>/player_html/`.
- `get_player_profile_info_1st()` / `..._2nd()` — parse the infobox into JSON (using `_setup`'s `scrap_rugby_wiki_standard`, `parse_rugby_player_info`, etc.), written to `<Nation>/PP/`.
- `fix_small_issues_file()` — footnotes, birth/death place, number formatting.
- `standardise_player_profile_info()` — normalise into `<Nation>/PP_cleaned/`.
- `combine_all_jsons_finds()` — concatenate every nation's cleaned JSON into `all_capped_players_profile.json`.

**`02_02_download_all_capped_players_profiles.py`** is a partial rewrite of *only* the first stage: a polite, resumable downloader (`fetch_html` with retries/backoff, a descriptive User-Agent, and a skip-if-exists check). The downstream parse/standardise/combine functions are present but commented out, so it does not replace `02` on its own.

**Step 3 — `03_download_all_wikidate_players.py`.** The same six stages as step 2, but seeded from `data/wikipedia/Wikidata_players.csv` and writing to `<Nation>_2/…`, ending in `all_wikidata_players_profile_4.json`. It is essentially a copy of `02` with different input/output paths (see §4 — a candidate for de-duplication).

**Step 4 — `04_02_complie_json_to_csv.py`.** Loads a combined JSON and flattens it into two tidy tables: `create_player_info()` → one row per player (id, name, DOB/place, death, height, weight, positions); `create_row_stint()` → one row per career stint (team, years, apps, points, stint type). Writes `PP_player_info_*.csv` and `PP_stint_info_*.csv`.

**Steps 5–6 — `clean_tables.R`, `integrate_data.R` (Vincent).** `clean_tables.R` loads the raw tables and normalises names, teams, positions, and locations (using `src/common/norm_*.R`). `integrate_data.R` then merges the cleaned English-WP tables into the shared `data/fusion/*_eswp.csv` tables. Both begin with a purpose header and `source("src/common/…")` — a cleaner style worth mirroring in the Python files.

### One gap worth documenting
The Python step 4 writes `PP_player_info_*.csv` / `PP_stint_info_*.csv` into `data/wikipedia/english/`, but `clean_tables.R` reads `raw/player_info4.csv` and `raw/stint_info3.csv`. There is a **manual rename/move step** between Python output and R input that no script captures. Note it in the README (or add a tiny script) so a collaborator can reproduce the chain.

---

## 4. Formatting & tidy-up suggestions

These are suggestions only — nothing here has been changed. Roughly in priority order for a shared repo.

**Naming**
1. **Typo in every `04` filename:** `complie` → `compile`.
2. **`x_fixing_match_numbers` has no file extension** — should be `.py` (it is a Python script). As-is it won't be recognised as Python by GitHub or editors.
3. **`x_03_creating datasets.py` contains a space** — spaces in filenames break command-line runs and imports. Use `x_03_creating_datasets.py`.
4. **Version suffixes are inconsistent** (`_2`, `_4`, `_x`, `_test`, `02_02`, `04_01`, `04_02`). Prefer one working file per step and let git history hold old versions, rather than copies living side by side.

**Structure / de-duplication**
5. **`02` and `03` are near-identical** (only the input list and output paths differ). Consider a single parameterised script/function taking the input CSV + output folder, called twice. Same for the three `04` files — one file with the filename as a parameter.
6. **`_setup.py` is ~1,600 lines** mixing scraping, parsing, cleaning, and CSV building. Optional, larger job: split into a small package (e.g. `scrape.py`, `parse.py`, `clean.py`, `io.py`). Fine to defer.

**Runnability / hygiene**
7. **`from _setup import *`** is a wildcard import (the `# noqa: F403` shows the linter already flags it). Explicit imports make it clear where each function comes from and avoid name clashes.
8. **`sys.path.append("./src/wikipedia/english/")`** is repeated in most scripts and depends on the current working directory. A package layout, a `conftest`/`PYTHONPATH` note, or a small path-bootstrap module is sturdier.
9. **No `if __name__ == "__main__":` guard.** Scripts execute their pipeline the moment they're imported — which is exactly why the `x_` scripts re-run scraping on import. Wrapping the top-level calls in a `main()` guard makes them safe to import and easier to test.
10. **Hardcoded relative paths** (`./data/...`) assume the repo root is the working directory. Document this clearly, or centralise paths in one config block/file.
11. **`.gitignore`:** add `__pycache__/` and `*.pyc`, then untrack the committed `.pyc`. Consider ignoring a `sandbox/`/`scratch/` folder for the `x_` scripts.

**Documentation / consistency**
12. **Add a short docstring/header to each numbered script** stating purpose, inputs, outputs, and run order — the R files already do this well; the Python files carry only the auto-generated Spyder "Created on … @author" header.
13. **Double-underscore prefixes** (`__checking_data.R`) are an unusual convention; a single `_` or a `checks/` folder reads more clearly.
14. **Public contact details:** `02_02`'s User-Agent embeds a personal email and a GitHub URL. That's reasonable scraping etiquette, but be aware it becomes public once the repo is shared.

---

## 5. Suggested folder shape after tidy-up

Illustrative only — not applied.

```
src/wikipedia/english/
  _setup.py
  01_get_capped_players_urls.py
  02_download_all_capped_players_profiles.py     # with 02_02's improved downloader merged in
  03_download_all_wikidata_players.py            # (also fixes "wikidate" typo)
  04_compile_json_to_csv.py                      # single, parameterised; "compile" spelling
  clean_tables.R
  integrate_data.R
  checks/
    checking_data.R                              # was __checking_data.R
  sandbox/                                        # git-ignored
    x_testing_*.py  …                            # all the x_ experiments
```
