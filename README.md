# BiasLens — Gender & Language Bias Explorer

![BiasLens Screenshot](screenshotsite.png)
![BiasLens Screenshot](screenshotsite2.png)


**Live App:** [https://pvparekh22.shinyapps.io/BiasLens-SOC360-CSS/](https://pvparekh22.shinyapps.io/BiasLens-SOC360-CSS/)

---

## Overview
BiasLens is an interactive R Shiny application that analyzes gendered language patterns in Wikipedia biography abstracts. It identifies words disproportionately associated with men or women and visualizes these associations in a user-friendly interface.

The app allows users to explore male- and female-associated words, view detailed tables of word statistics, and download filtered data for further analysis. It was developed as a final project for SOC360 — Computational Social Science at Rutgers University.

---

## Features
- **Interactive Bar Charts:** Explore top male- and female-associated words with hover tooltips showing counts and ratios.
- **Word Table:** Full statistics table sortable by counts, ratios, or dominance.
- **Raw Data Preview:** View cleaned Wikipedia biographies with pronoun flags.
- **Filters & Controls:** Customize analysis by minimum word frequency, top N words, gender dominance, and ratio thresholds.
- **Data Export:** Download filtered stats as CSV or top lists as a zipped CSV package.
- **Responsive Design:** Works across desktop and mobile browsers.

---

## Methodology
- Biographies were preprocessed (cleaning, stopword removal, tokenization).
- For each word, overall counts and male/female occurrence ratios were computed.
- Words were classified as male- or female-dominant based on frequency and ratio thresholds.
- Visualizations allow interactive exploration of gender bias in language.

---

## Tech Stack
- **R**, **Shiny**, **tidyverse**, **ggplot2**, **Plotly**, **DT**
- **CSS** for styling and interactive UI enhancements

---

## Usage
1. Clone the repository:
```bash
git clone https://github.com/<your-username>/BiasLens-SOC360-CSS.git
