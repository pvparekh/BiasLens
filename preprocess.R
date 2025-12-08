library(tidyverse)
library(jsonlite)
library(tidytext)
library(stringr)

# --------------------------

# Parameters

# --------------------------

path <- "people_0.ndjson"   # full dataset
subset_lines <- 300000       # number of lines to process

# --------------------------

# Read subset of lines

# --------------------------

con <- file(path, "r")
lines <- readLines(con, n = subset_lines)
close(con)

# --------------------------

# Parse JSON safely

# --------------------------

parse_safe <- function(line) {
tryCatch({
d <- jsonlite::fromJSON(line)
tibble(
name = d$name %||% NA_character_,
abstract = d$abstract %||% NA_character_,
url = d$url %||% NA_character_
)
}, error = function(e) NULL)
}

people_df <- map_dfr(lines, parse_safe) %>%
  mutate(
    name = str_squish(replace_na(name, "")),
    abstract = str_squish(replace_na(abstract, "")),
    url = str_squish(replace_na(url, "")),
    has_male = str_detect(tolower(abstract), "\\bhe\\b|\\bhim\\b|\\bhis\\b"),
    has_female = str_detect(tolower(abstract), "\\bshe\\b|\\bher\\b|\\bhers\\b"),
    row_id = row_number()
  )


# --------------------------

# Tokenize abstracts

# --------------------------

tokens <- people_df %>%
  select(row_id, abstract) %>%
  unnest_tokens(word, abstract) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  anti_join(stop_words, by = "word") %>%
  left_join(people_df %>% select(row_id, has_male, has_female), by = "row_id")


# --------------------------

# Compute gender word stats

# --------------------------

gender_word_stats <- tokens %>%
  group_by(word) %>%
  summarise(
    count = n(),                     # how many times this word appears overall
    male_count = sum(has_male),      # how many times the word appeared in a male-flagged bio
    female_count = sum(has_female),  # how many times in a female-flagged bio
    male_ratio = male_count / count, # proportion in male bios
    female_ratio = female_count / count # proportion in female bios
  ) %>%
  filter(count >= 100)                 # only keep words that appear at least 100 times


# --------------------------

# Top words 

# --------------------------

top_male_words <- gender_word_stats %>% arrange(desc(male_ratio)) %>% slice(50:75)
top_female_words <- gender_word_stats %>% arrange(desc(female_ratio)) %>% slice(1:25)



# --------------------------

# Save RDS for Shiny app

# --------------------------

saveRDS(people_df, "pds.rds")
saveRDS(gender_word_stats, "gws.rds")
saveRDS(top_male_words, "tmw.rds")
saveRDS(top_female_words, "fmw.rds")




gender_word_stats %>% 
  filter(count >= 20, female_ratio > male_ratio) %>% 
  arrange(desc(female_ratio)) %>% 
  slice_head(n=30) %>% 
  select(word, count, male_ratio, female_ratio)

