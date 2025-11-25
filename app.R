library(tidyverse)
library(jsonlite)
library(ndjson)
library(tidyr)
library(readr)
library(data.table)
library(purrr)
library(kableExtra)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(dplyr)
library(shiny)
library(tidytext)
library(DT)
library(rsconnect)



# ---------------------------
# load & parse dataset
# ---------------------------

path <- "people_small.ndjson"
lines <- readLines(path)

parse_safe <- function(line) {
  tryCatch({
    data <- jsonlite::fromJSON(line)
    tibble(
      name = data$name %||% NA_character_,
      abstract = data$abstract %||% NA_character_,
      url = data$url %||% NA_character_
    )
  }, error = function(e) NULL)
}

people_df <- map_dfr(lines, parse_safe) %>%
  mutate(
    name = str_squish(name),
    abstract = str_squish(replace_na(abstract, "")),
    url = str_squish(replace_na(url, ""))
  ) %>%
  mutate(
    has_male = str_detect(tolower(abstract), "\\bhe\\b|\\bhim\\b|\\bhis\\b"),
    has_female = str_detect(tolower(abstract), "\\bshe\\b|\\bher\\b|\\bhers\\b"),
    row_id = row_number()
  )

# ---------------------------
# tokenize
# ---------------------------
tokens <- people_df %>%
  select(row_id, abstract) %>%
  unnest_tokens(word, abstract) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  anti_join(stop_words, by = "word") %>%
  left_join(
    people_df %>% select(row_id, has_male, has_female),
    by = "row_id"
  )

# ---------------------------
# find gender word associations
# ---------------------------
gender_word_stats <- tokens %>%
  group_by(word) %>%
  summarise(
    count = n(),
    male_count = sum(has_male, na.rm = TRUE),
    female_count = sum(has_female, na.rm = TRUE),
    male_ratio = male_count / count,
    female_ratio = female_count / count
  ) %>%
  filter(count >= 20) %>%
  arrange(desc(abs(male_ratio - female_ratio)))

top_male_words <- gender_word_stats %>% arrange(desc(male_ratio)) %>% slice(1:20)
top_female_words <- gender_word_stats %>% arrange(desc(female_ratio)) %>% slice(1:20)

# ---------------------------
# SHINY UI
# ---------------------------
ui <- fluidPage(
  titlePanel("Gender & Language Bias in Wikipedia Biographies"),
  
  tabsetPanel(
    
    tabPanel("Overview",
             br(),
             p("This app analyzes gendered patterns in Wikipedia biographies, 
                detecting pronouns ('he', 'she') and identifying words that strongly 
                associate with male vs female referenced biographies.")
    ),
    
    tabPanel("Top Gendered Words",
             fluidRow(
               column(6,
                      h3("Male-associated Words"),
                      plotOutput("malePlot")
               ),
               column(6,
                      h3("Female-associated Words"),
                      plotOutput("femalePlot")
               )
             )
    ),
    
    tabPanel("Word Table",
             br(),
             DTOutput("wordTable")
    ),
    
    tabPanel("Raw Data",
             br(),
             DTOutput("rawTable")
    )
  )
)

# ---------------------------
# SERVER
# ---------------------------
server <- function(input, output, session) {
  
  # male words plot
  output$malePlot <- renderPlot({
    ggplot(top_male_words, aes(x = reorder(word, male_ratio), y = male_ratio)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top Male-Associated Words",
           x = "Word",
           y = "Male Ratio")
  })
  
  # female words plot
  output$femalePlot <- renderPlot({
    ggplot(top_female_words, aes(x = reorder(word, female_ratio), y = female_ratio)) +
      geom_col(fill = "pink") +
      coord_flip() +
      labs(title = "Top Female-Associated Words",
           x = "Word",
           y = "Female Ratio")
  })
  
  # table of gender word stats
  output$wordTable <- renderDT({
    gender_word_stats %>%
      arrange(desc(abs(male_ratio - female_ratio))) %>%
      datatable(options = list(pageLength = 15))
  })
  
  # table of raw data
  output$rawTable <- renderDT({
    people_df %>%
      select(name, abstract, has_male, has_female, url) %>%
      datatable(options = list(pageLength = 10))
  })
}

shinyApp(ui, server)
