# BiasLens — Gender & Language Bias Explorer

**Live App:** [https://pvparekh22.shinyapps.io/BiasLens-SOC360-CSS/](https://pvparekh22.shinyapps.io/BiasLens-SOC360-CSS/)

![BiasLens Screenshot](ss12.png)

---

## Overview
BiasLens is an interactive R Shiny application that analyzes gendered language patterns in Wikipedia biography abstracts. It identifies words disproportionately associated with men or women and visualizes these associations in a clean, explorable interface.

![BiasLens Screenshot](ss11.png)

The app allows users to explore male- and female-associated words, view detailed tables of word statistics (including TF IDF summaries), and download filtered results for further analysis. This project was developed as a final project for SOC360 — Computational Social Science at Rutgers University.

---

## Features
- **Interactive Bar Charts:** Explore top male- and female-associated words with hover tooltips showing counts and ratios.
- **Ratio Distribution Plot:** Histogram of gender-association strength 
- **Word Cloud:** Visual overview of frequent gendered terms, colored by dominant association.
- **Word Table:** Full statistics table sortable and searchable.
- **Raw Data Preview:** View sampled cleaned biographies with pronoun flags and source URLs.
- **Filters and Controls:** Customize analysis by minimum word frequency, top N words, dominance filtering, and dominance ratio thresholds.
- **Data Export:** Download filtered stats as CSV or download top lists as a zipped CSV package.
- **Polished UI:** Shiny themes, custom CSS, and responsive layout for a modern presentation.

---

## Methodology
BiasLens uses two complementary ideas: document frequency for association and TF IDF for interpretability.

### 1. Gender labeling with pronoun counts
Each biography abstract is lowercased and pronouns are counted:
- Male pronouns: `he`, `him`, `his`
- Female pronouns: `she`, `her`, `hers`

A biography is labeled:
- `male` if male pronouns > female pronouns
- `female` if female pronouns > male pronouns
- dropped if tied or both are zero (ambiguous or neutral)

The dataset is then balanced by downsampling so the number of male and female biographies is equal.

### 2. Tokenization and cleaning
- Tokenize with `tidytext::unnest_tokens`
- Keep only alphabetic tokens (`^[a-z]+$`)
- Remove stop words via `tidytext::stop_words`
- Remove common first names extracted from biography titles to reduce name driven artifacts

### 3. Word level statistics
For each word:
- **Document frequency** counts how many distinct male vs. female biographies contain the word overall
- **Association ratios** are computed from document counts:
  - `male_ratio = male_docs / (male_docs + female_docs)`
  - `female_ratio = female_docs / (male_docs + female_docs)`
- **TF IDF** is computed per word per biography using `bind_tf_idf`, then summed by gender:
  - `total_tfidf`, `male_tfidf`, `female_tfidf`

Association is primarily shown using document based ratios, while TF IDF helps interpret importance and prominence.

---

## Tech Stack
- **R**, **Shiny**
- **tidyverse**, **tidytext**, **stringr**, **jsonlite**
- **Plotly** (interactive charts), **DT** (interactive tables), **wordcloud2**
- **shinythemes**, **shinyWidgets**, **shinyjs**
- Custom **CSS** for styling and UI polish

---

## Project Structure

├── app.R # Shiny application (UI + server)
├── preprocess.R # Preprocessing pipeline that generates RDS files
├── people_0.ndjson # Input dataset (NDJSON)
├── pds.rds # Processed biographies used by the app
├── gws.rds # Word level gender stats used by the app
├── tmw.rds # Top male associated word list
├── fmw.rds # Top female associated word list
└── www/
└── lens.png # Logo used in the U


