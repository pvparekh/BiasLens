library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(DT)

# ---------- load precomputed data ----------
people_df <- readRDS("pds.rds")
gender_word_stats <- readRDS("gws.rds")
top_male_words <- readRDS("tmw.rds")
top_female_words <- readRDS("fmw.rds")

if (!"word" %in% colnames(gender_word_stats)) stop("gws.rds must contain a 'word' column")
if (!all(c("count","male_count","female_count","male_ratio","female_ratio") %in% colnames(gender_word_stats))) {
  stop("gws.rds must contain count,male_count,female_count,male_ratio,female_ratio columns")
}

# ---------- UI ----------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  tags$head(
    # page / browser title
    tags$title("BiasLens — Gender & Language Bias"),
    # Google font for the title
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@700;800&display=swap"),
    tags$style(HTML("
      /* page background */
      body { background: linear-gradient(to right, #f1f5f9, #ffffff); }

      /* generic card */
      .card {
        background: linear-gradient(135deg, #ffffff, #e9f0f7);
        border-radius: 12px;
        padding: 16px;
        box-shadow: 0 6px 18px rgba(0,0,0,0.08);
        margin-bottom: 16px;
        transition: all 0.18s ease-in-out;
      }
      .card:hover { transform: translateY(-3px); box-shadow: 0 10px 25px rgba(0,0,0,0.12); }

      /* HERO (title + desc + stats) - pink -> blue dynamic gradient */
      .hero-card {
        background: linear-gradient(135deg, #ff85b3 0%, #7ac7f4 100%);
        color: #ffffff;
        border-radius: 12px;
        padding: 20px 24px;
        box-shadow: 0 12px 30px rgba(99,102,241,0.12);
        position: relative;
        overflow: visible;
      }

      /* Title / description spacing & typography */
      .hero-card .big-title {
        font-family: 'Montserrat', system-ui, -apple-system, 'Segoe UI', Roboto, 'Helvetica Neue', Arial;
        font-weight: 800;
        font-size: 34px;
        margin: 0 0 12px 0;
        line-height: 1.04;
        text-align: center;
        color: #ffffff;
        /* drop-in with subtle bounce and hold final state */
        opacity: 0;
        transform: translateY(-60px) scale(0.99);
        animation-name: dropInBounce;
        animation-duration: 0.9s;
        animation-timing-function: cubic-bezier(.22,.9,.35,1);
        animation-fill-mode: forwards;         /* keep final state */
        animation-iteration-count: 1;          /* play once */
      }
      @keyframes dropInBounce {
        0%   { opacity: 0; transform: translateY(-60px) scale(0.98); }
        65%  { opacity: 1; transform: translateY(8px)   scale(1.02); }
        85%  { opacity: 1; transform: translateY(-4px)  scale(0.995); }
        100% { opacity: 1; transform: translateY(0px)   scale(1); } /* explicit final state */
      }

      .big-title .bias-blue { color: #3b82f6; }   /* softer/brighter blue for 'Bias' */
      .big-title .lens-white { color: #ffffff; }  /* 'Lens' remains white */

      .hero-card .subtle {
        color: rgba(255,255,255,0.95);
        font-size: 15px;
        margin: 0 0 14px 0;
        text-align: center;
      }

      /* decorative overlapping ovals/circles behind the quick-stats */
      .hero-decor-left, .hero-decor-right {
        position: absolute;
        width: 140px;
        height: 140px;
        border-radius: 999px;
        z-index: 0;
        filter: blur(10px);
        opacity: 0.18;
        pointer-events: none;
      }
      .hero-decor-left {
        background: radial-gradient(circle at 30% 30%, rgba(255,255,255,0.18), rgba(255,255,255,0.02));
        left: 10%;
        bottom: 8px;
        transform: translate(-20%, 10%);
      }
      .hero-decor-right {
        background: radial-gradient(circle at 70% 70%, rgba(0,0,0,0.12), rgba(255,255,255,0.02));
        right: 12%;
        bottom: 12px;
        transform: translate(20%, 6%);
      }

      /* quick stats pill */
      .hero-stats-wrap { position: relative; z-index: 2; display: inline-block; }
      .quick-stats {
        display: inline-flex;
        gap: 20px;
        background: rgba(255,255,255,0.10);
        border: 1px solid rgba(255,255,255,0.12);
        padding: 10px 18px;
        border-radius: 999px;
        font-weight: 700;
        color: #ffffff;
        align-items: center;
        justify-content: center;
      }
      .quick-stats .stat { text-align: center; }
      .quick-stats .stat .stat-label { font-size: 12px; opacity: 0.95; display:block; line-height: 1; }
      .quick-stats .stat .stat-value { font-size: 18px; display:block; margin-top:4px; line-height: 1.05; }

      /* refresh button */
      .refresh-btn {
        position: absolute;
        top: 12px;
        right: 12px;
        transition: transform 0.18s ease-in-out, box-shadow 0.18s ease-in-out;
        z-index: 3;
      }
      .refresh-btn:hover {
        transform: scale(1.18);
        box-shadow: 0 8px 20px rgba(0,0,0,0.18);
      }

      /* sidebar styling */
      .sidebar {
        background: linear-gradient(180deg, #ffffff, #fbfdff);
        border-radius: 16px;
        padding: 18px 14px;
        box-shadow: 0 6px 18px rgba(0,0,0,0.06);
        margin-bottom: 12px;
      }
      .sidebar .sidebar-title {
        font-weight: 900;
        font-size: 18px;
        margin-bottom: 12px;
        letter-spacing: 0.2px;
      }
      .sidebar .form-group { margin-bottom: 12px; }
      .sidebar .control-label { font-weight: 700; }
      .sidebar .download-btn { display: block; width: 100%; margin-bottom: 12px; }

      /* main panel tweak */
      .main-panel { padding-left: 6px; }

      /* tabs: bold text default, hover turns non-active tabs darker gray + scale */
      .nav-tabs > li > a,
      .nav-tabs > li > a:link,
      .nav-tabs > li > a:visited {
        color: #000000 !important;
        font-weight: 700 !important;
        transition: color 0.12s ease, transform 0.18s ease-in-out;
        display: inline-block;
      }
      /* only change color on hover for non-active tabs (darker gray) */
      .nav-tabs > li:not(.active) > a:hover {
        color: #666666 !important;   /* darker gray on hover for non-active */
        transform: scale(1.05);
      }
      /* keep active tab blue (unchanged on hover) */
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: #1e66b3 !important;
      }
      .nav-tabs > li.active > a { background: rgba(0,0,0,0.03); border-radius: 8px 8px 0 0; }

      /* buttons pop */
      .btn { transition: transform 0.18s ease-in-out, box-shadow 0.18s ease-in-out; display:inline-block; }
      .btn:hover { transform: scale(1.05); }

      /* table row hover */
      table.dataTable tbody tr:hover { background-color: rgba(255,235,205,0.5) !important; }

      /* responsive */
      @media (max-width: 992px) {
        .hero-card .big-title { font-size: 26px; }
        .sidebar { margin-bottom: 10px; }
      }
    "))
  ),
  
  # ---------- Header ----------
  fluidRow(
    column(12,
           div(class = "hero-card card",
               # decorative circles
               div(class = "hero-decor-left"),
               div(class = "hero-decor-right"),
               
               # refresh (small icon button)
               actionBttn(inputId = "refresh", label = NULL, style = "simple", color = "primary", size = "xs",
                          icon = icon("sync"), class = "refresh-btn"),
               
               # title, subtitle, quick stats
               div(style = "text-align:center; padding-top:6px; padding-bottom:6px;",
                   # Title: Bias (blue) + Lens (white) in one word
                   h1(HTML('<span class="big-title"><span class="bias-blue">Bias</span><span class="lens-white">Lens</span></span>')),
                   p("Interactive explorer using Wikipedia biography data for analyzing word and gender association.", class = "subtle"),
                   div(class = "hero-stats-wrap",
                       div(class = "quick-stats",
                           div(class = "stat",
                               span(class = "stat-label", "Biographies:"),
                               span(class = "stat-value", format(nrow(people_df), big.mark = ","))
                           ),
                           div(class = "stat",
                               span(class = "stat-label", "Unique words:"),
                               span(class = "stat-value", format(nrow(gender_word_stats), big.mark = ","))
                           )
                       )
                   )
               )
           )
    )
  ),
  
  # ---------- Main body ----------
  fluidRow(
    column(2,
           div(class = "sidebar",
               div(class = "sidebar-title", "Filters & Controls"),
               sliderInput("min_count", "Minimum word frequency", min = 100, max = max(2000, na.rm = TRUE),
                           value = 100, step = 10, width = "100%"),
               numericInput("top_n", "Top words to show per chart", value = 25, min = 5, max = 100, step = 5, width = "100%"),
               checkboxInput("dominance_only", "Show only gender-dominant words", value = TRUE),
               checkboxInput("use_ratio_cut", "Require ratio >= (dominance threshold)", value = TRUE),
               conditionalPanel(condition = "input.use_ratio_cut == true",
                                sliderInput("min_ratio", "Minimum dominant ratio", min = 0.5, max = 1, value = 0.7, step = 0.01, width = "100%")
               ),
               hr(),
               h5("Export / Downloads"),
               downloadButton("download_filtered", "Download filtered stats (CSV)", class = "download-btn"),
               downloadButton("download_top", "Download current top lists (zip CSVs)", class = "download-btn"),
               p(class = "small-note", "Download the filtered table or top lists.")
           )
    ),
    
    column(10, class = "main-panel",
           tabsetPanel(
             tabPanel("Top Gendered Words",
                      br(),
                      fluidRow(
                        column(6,
                               div(class = "card male-section",
                                   h4("Male-associated (interactive)"),
                                   plotlyOutput("malePlot", height = "420px"),
                                   p(class = "small-note", "Hover on bars for counts*")
                               )
                        ),
                        column(6,
                               div(class = "card female-section",
                                   h4("Female-associated (interactive)"),
                                   plotlyOutput("femalePlot", height = "420px"),
                                   p(class = "small-note", "Hover on bars for counts*")
                               )
                        )
                      ),
                      br(),
                      fluidRow(
                        column(12,
                               div(class = "card",
                                   h4("Top lists preview"),
                                   fluidRow(
                                     column(6, DTOutput("topMaleTable")),
                                     column(6, DTOutput("topFemaleTable"))
                                   )
                               )
                        )
                      )
             ),
             
             tabPanel("Word Table",
                      br(),
                      div(class = "card",
                          h4("Full word statistics"),
                          DTOutput("wordTable", height = "600px")
                      )
             ),
             
             tabPanel("Raw Data",
                      br(),
                      div(class = "card",
                          h4("Sample biographies"),
                          p(class = "small-note", "Showing the cleaned biographies with pronoun flags (paginated)."),
                          DTOutput("rawTable", height = "600px")
                      )
             ),
             
             tabPanel("About",
                      br(),
                      div(class = "card",
                          h4("Methodology"),
                          tags$p("This project (SOC360 — Computational Social Science, Rutgers) analyzes gendered language patterns in Wikipedia biography abstracts by identifying words disproportionately associated with men or women."),
                          tags$p("Biographies were preprocessed (cleaning, stopword removal, tokenization). For each word, overall counts and male/female occurrence ratios were computed to assess gender association."),
                          tags$p("The interactive tool supports filtering by frequency and dominance; users can explore male- and female-associated words via bar charts, inspect the full statistics table, and download filtered or top lists for further analysis."),
                          tags$p("This project draws inspiration from:"),
                          tags$p(tags$em("Bolukbasi, T., Chang, K.-W., Zou, J., Saligrama, V., & Kalai, A. (2016)."),
                                 " Man is to Computer Programmer as Woman is to Homemaker? Debiasing Word Embeddings. Proceedings of NeurIPS 2016."),
                          tags$p("The purpose is to provide a clear, data-driven way to examine language bias in widely available text sources.")
                      )
             )
           )
    )
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  observeEvent(input$refresh, {
    # small debounce so UI shows a reload notification
    shiny::invalidateLater(1000, session)
    try({
      gender_word_stats <<- readRDS("gws.rds")
      people_df <<- readRDS("pds.rds")
      showNotification("Reloaded RDS files (gws/pds).", type = "message")
    }, silent = TRUE)
  })
  
  filtered_stats <- reactive({
    req(gender_word_stats)
    df <- gender_word_stats %>% filter(count >= input$min_count)
    if (input$dominance_only) {
      df <- df %>% mutate(dominant = ifelse(male_ratio >= female_ratio, "male", "female"))
      df <- df %>% filter(dominant %in% c("male","female"))
      if (input$use_ratio_cut) {
        df <- df %>% filter(
          (dominant == "male" & male_ratio >= input$min_ratio) |
            (dominant == "female" & female_ratio >= input$min_ratio)
        )
      }
    }
    df
  })
  
  top_male_reactive <- reactive({
    df <- filtered_stats()
    df %>%
      mutate(score = male_ratio - female_ratio) %>%
      arrange(desc(score), desc(count)) %>%
      filter(male_ratio >= female_ratio) %>%
      slice_head(n = input$top_n) %>%
      mutate(word = fct_reorder(word, male_ratio))
  })
  
  top_female_reactive <- reactive({
    df <- filtered_stats()
    df %>%
      mutate(score = female_ratio - male_ratio) %>%
      arrange(desc(score), desc(count)) %>%
      filter(female_ratio >= male_ratio) %>%
      slice_head(n = input$top_n) %>%
      mutate(word = fct_reorder(word, female_ratio))
  })
  
  output$malePlot <- renderPlotly({
    df <- top_male_reactive()
    if (nrow(df) == 0) {
      return(ggplotly(ggplot() + geom_blank() + labs(title = "No male-dominant words")))
    }
    p <- ggplot(df, aes(x = word, y = male_ratio,
                        text = paste0("word: ", word, "<br>count: ", count,
                                      "<br>male_count: ", male_count, "<br>male_ratio: ", round(male_ratio,3)))) +
      geom_col(aes(fill = male_ratio), show.legend = FALSE) + coord_flip() +
      labs(x = NULL, y = "Male ratio") + scale_fill_viridis_c(option = "C")
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l = 120))
  })
  
  output$femalePlot <- renderPlotly({
    df <- top_female_reactive()
    if (nrow(df) == 0) {
      return(ggplotly(ggplot() + geom_blank() + labs(title = "No female-dominant words")))
    }
    p <- ggplot(df, aes(x = word, y = female_ratio,
                        text = paste0("word: ", word, "<br>count: ", count,
                                      "<br>female_count: ", female_count, "<br>female_ratio: ", round(female_ratio,3)))) +
      geom_col(aes(fill = female_ratio), show.legend = FALSE) + coord_flip() +
      labs(x = NULL, y = "Female ratio") + scale_fill_viridis_c(option = "A")
    ggplotly(p, tooltip = "text") %>% layout(margin = list(l = 120))
  })
  
  output$topMaleTable <- renderDT({
    top_male_reactive() %>% select(word, count, male_count, female_count, male_ratio, female_ratio) %>%
      datatable(rownames = FALSE, options = list(pageLength = 10, dom = 'tp'))
  })
  output$topFemaleTable <- renderDT({
    top_female_reactive() %>% select(word, count, male_count, female_count, male_ratio, female_ratio) %>%
      datatable(rownames = FALSE, options = list(pageLength = 10, dom = 'tp'))
  })
  
  output$wordTable <- renderDT({
    df <- filtered_stats() %>% arrange(desc(abs(male_ratio - female_ratio))) %>% mutate(diff = male_ratio - female_ratio)
    datatable(df, rownames = FALSE, extensions = c('Buttons','Scroller'),
              options = list(dom = 'Bfrtip', buttons = c('csv','excel','pageLength'), pageLength = 25, deferRender = TRUE, scrollY = 600))
  }, server = TRUE)
  
  output$rawTable <- renderDT({
    people_df %>% select(name, abstract, has_male, has_female, url) %>%
      datatable(rownames = FALSE, options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE))
  }, server = TRUE)
  
  output$download_filtered <- downloadHandler(
    filename = function() { paste0("gender_word_stats_filtered_", Sys.Date(), ".csv") },
    content = function(file) { readr::write_csv(filtered_stats(), file) }
  )
  
  output$download_top <- downloadHandler(
    filename = function() { paste0("top_gender_lists_", Sys.Date(), ".zip") },
    content = function(file) {
      tmpdir <- tempdir()
      f1 <- file.path(tmpdir, "top_male.csv")
      f2 <- file.path(tmpdir, "top_female.csv")
      readr::write_csv(top_male_reactive(), f1)
      readr::write_csv(top_female_reactive(), f2)
      oldwd <- setwd(tmpdir); on.exit(setwd(oldwd))
      zip::zipr(zipfile = file, files = c("top_male.csv", "top_female.csv"))
    }
  )
}

# ---------- RUN APP ----------
shinyApp(ui, server)



