library(tidyverse)
library(jsonlite)
library(tidytext)
library(stringr)

`%||%` <- function(x, y) if (!is.null(x)) x else y

# ---------------------------------------------------------
# PARAMETERS
# ---------------------------------------------------------
path <- "people_0.ndjson"
subset_lines <- 250000   # bump this later (e.g., 30000+)

# ---------------------------------------------------------
# READ NDJSON (subset)
# ---------------------------------------------------------
con <- file(path, "r")
lines <- readLines(con, n = subset_lines)
close(con)

parse_safe <- function(line) {
  tryCatch({
    d <- jsonlite::fromJSON(line)
    tibble(
      name     = d$name     %||% NA_character_,
      abstract = d$abstract %||% NA_character_,
      url      = d$url      %||% NA_character_
    )
  }, error = function(e) NULL)
}

people_df <- map_dfr(lines, parse_safe) %>%
  mutate(
    name      = str_squish(replace_na(name, "")),
    abstract  = str_squish(replace_na(abstract, "")),
    url       = str_squish(replace_na(url, "")),
    lower_abs = tolower(abstract)
  )

# ---------------------------------------------------------
# PRONOUN COUNTS + MAJORITY RULE (+ has_male / has_female)
# ---------------------------------------------------------
count_occ <- function(text, pattern) {
  str_count(text, regex(pattern, ignore_case = TRUE))
}

people_df <- people_df %>%
  mutate(
    male_pron   = count_occ(lower_abs, "\\bhe\\b|\\bhim\\b|\\bhis\\b"),
    female_pron = count_occ(lower_abs, "\\bshe\\b|\\bher\\b|\\bhers\\b"),
    gender_flag = case_when(
      male_pron   > female_pron ~ "male",
      female_pron > male_pron   ~ "female",
      TRUE                      ~ NA_character_   # tie or zero pronouns => drop
    ),
    has_male   = male_pron   > 0,
    has_female = female_pron > 0
  ) %>%
  filter(!is.na(gender_flag))   # drop ambiguous biographies

# ---------------------------------------------------------
# BALANCE MALE / FEMALE BIOS (For cleaner distribution)
# ---------------------------------------------------------
gender_counts <- people_df %>%
  count(gender_flag, name = "n")

print(gender_counts)  # see how balanced the raw data is

n_min <- min(gender_counts$n)

set.seed(123)  
people_df <- people_df %>%
  group_by(gender_flag) %>%
  slice_sample(n = n_min) %>%   # same number of male/female bios
  ungroup() %>%
  mutate(row_id = row_number())

# ---------------------------------------------------------
# TOKENIZATION
# ---------------------------------------------------------
tokens <- people_df %>%
  select(row_id, abstract, gender_flag, name) %>%
  unnest_tokens(word, abstract) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%   # keep simple alphabetic tokens
  anti_join(stop_words, by = "word")

# ---------------------------------------------------------
# REMOVE FIRST NAMES (proffessor suggestion)
# any first name that appears >= 5 times as first token
# ---------------------------------------------------------
first_name_counts <- people_df %>%
  transmute(
    first = name %>%
      tolower() %>%
      str_extract("^[a-z]+")
  ) %>%
  filter(!is.na(first), first != "") %>%
  count(first, name = "Freq")

first_names <- first_name_counts %>%
  filter(Freq >= 5) %>%
  pull(first)

tokens <- tokens %>%
  filter(!word %in% first_names)

# TERM FREQUENCY PER DOCUMENT

tokens_count <- tokens %>%
  count(row_id, word, name = "n")

# ---------------------------------------------------------
# TF-IDF 
# ---------------------------------------------------------
tokens_tfidf <- tokens_count %>%
  bind_tf_idf(term = word, document = row_id, n = n)

# ---------------------------------------------------------
# DOC FREQUENCY BY GENDER 
# number of DISTINCT biographies per gender that contain the word
# ---------------------------------------------------------
tokens_count_gender <- tokens_count %>%
  left_join(people_df %>% select(row_id, gender_flag), by = "row_id")

doc_stats <- tokens_count_gender %>%
  group_by(word) %>%
  summarise(
    doc_count   = n_distinct(row_id),                          # total docs
    male_docs   = n_distinct(row_id[gender_flag == "male"]),   # docs w/ word in male bios
    female_docs = n_distinct(row_id[gender_flag == "female"]), # docs w/ word in female bios
    .groups     = "drop"
  )

# ---------------------------------------------------------
# TF-IDF SUMS BY GENDER 
# ---------------------------------------------------------
tfidf_gender <- tokens_tfidf %>%
  left_join(people_df %>% select(row_id, gender_flag), by = "row_id") %>%
  group_by(word) %>%
  summarise(
    total_tfidf  = sum(tf_idf, na.rm = TRUE),
    male_tfidf   = sum(tf_idf[gender_flag == "male"],   na.rm = TRUE),
    female_tfidf = sum(tf_idf[gender_flag == "female"], na.rm = TRUE),
    .groups      = "drop"
  )

# ---------------------------------------------------------
# FINAL GENDER WORD STATS:
#   - male_ratio / female_ratio based on DOC COUNTS
#   - TF-IDF columns for interpretation
# ---------------------------------------------------------
gender_word_stats <- doc_stats %>%
  left_join(tfidf_gender, by = "word") %>%
  mutate(
    count        = doc_count,
    male_count   = male_docs,
    female_count = female_docs,
    total_gender_docs = male_docs + female_docs,
    male_ratio   = if_else(total_gender_docs > 0,
                           male_docs   / total_gender_docs, NA_real_),
    female_ratio = if_else(total_gender_docs > 0,
                           female_docs / total_gender_docs, NA_real_)
  ) %>%
  filter(
    !is.na(male_ratio),
    !is.na(female_ratio),
    count >= 20       
  ) %>%
  select(
    word,
    count,
    male_count,
    female_count,
    male_ratio,
    female_ratio,
    doc_count,
    male_docs,
    female_docs,
    total_tfidf,
    male_tfidf,
    female_tfidf
  ) %>%
  arrange(desc(abs(male_ratio - female_ratio)))

# ---------------------------------------------------------
# TOP LISTS  
# ---------------------------------------------------------
top_male_words <- gender_word_stats %>%
  arrange(desc(male_ratio)) %>%
  slice_head(n = 25)

top_female_words <- gender_word_stats %>%
  arrange(desc(female_ratio)) %>%
  slice_head(n = 25)

# ---------------------------------------------------------
# PRE-SAVE RDS FOR SHINYAPP
# ---------------------------------------------------------
saveRDS(people_df,         "pds.rds")
saveRDS(gender_word_stats, "gws.rds")
saveRDS(top_male_words,    "tmw.rds")
saveRDS(top_female_words,  "fmw.rds")


print(gender_word_stats %>% slice_head(n = 20))


