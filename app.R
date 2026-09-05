library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(DT)
library(wordcloud2)

# ============================================================
# Creating Welcoming Spaces — Editorial Review Analysis
# Public, data-free version
#
# No proposal submissions, reviewer identities, reviewer notes,
# decisions, credentials, URLs, or private workbooks are included
# in this repository. To run locally, place an authorized review
# workbook in data/. The app reads only local files.
#
# Analysis layers remain separate:
# 1. Author-supplied section and approach
# 2. Recurring/project-developed themes
# 3. Reviewer-entered keywords, notes, and first-pass decisions
# 4. Five-criterion editorial rubric
# 5. Collection-level editorial judgment
#
# Theme counts and overlap are descriptive evidence. They do not
# automatically determine rubric scores or editorial decisions.
# ============================================================

clean_text <- function(x) {
  str_squish(replace_na(as.character(x), ""))
}

title_key_fn <- function(x) {
  clean_text(x) |>
    str_to_lower() |>
    str_replace_all("&", "and") |>
    str_replace_all("[^a-z0-9]+", " ") |>
    str_squish()
}

first_existing <- function(df, nms) {
  hit <- nms[nms %in% names(df)]
  if (!length(hit)) return(rep("", nrow(df)))
  df[[hit[1]]]
}

normalize_decision <- function(x) {
  z <- str_to_lower(str_squish(as.character(x)))

  case_when(
    str_detect(z, "maybe") ~ "Maybe",
    str_detect(z, "\\?") ~ "Maybe",
    z %in% c("y", "yes") ~ "Yes",
    z %in% c("n", "no") ~ "No",
    TRUE ~ NA_character_
  )
}

# ---- Project-developed analytic vocabulary ------------------
# ACRL-aligned for this project; not official ACRL subject headings.

theme_codebook <- tribble(
  ~family, ~theme, ~pattern,
  "Student Experience", "Belonging", "\\bbelong\\w*\\b|\\bmattering\\b",
  "Student Experience", "Well-Being", "\\bwell[- ]?being\\b|\\bwellness\\b|\\bmental health\\b|\\bstress\\b",
  "Student Experience", "Student Engagement", "\\bstudent engagement\\b|\\bengag\\w*\\b",
  "Student Experience", "Student Voice", "\\bstudent voice\\b|\\bco[- ]?creat\\w*\\b|\\bparticipatory\\b",
  "Access and Inclusion", "Accessibility", "\\baccessib\\w*\\b|\\buniversal design\\b|\\bbarrier\\w*\\b",
  "Access and Inclusion", "Inclusion / Equity", "\\binclusi\\w*\\b|\\bequit\\w*\\b|\\brepresentation\\b",
  "Access and Inclusion", "Neurodiversity / Sensory", "\\bneurodiv\\w*\\b|\\bsensory\\b",
  "Library Space", "Learning / Study Spaces", "\\bstudy space\\w*\\b|\\blearning space\\w*\\b",
  "Library Space", "Space Design", "\\bdesign\\w*\\b|\\bredesign\\w*\\b|\\brenovat\\w*\\b|\\bfurniture\\b",
  "Library Space", "Wayfinding / First Impressions", "\\bwayfinding\\b|\\bfirst impression\\w*\\b|\\bentrance\\b",
  "Assessment and UX", "Assessment / Evidence", "\\bassessment\\b|\\bsurvey\\w*\\b|\\binterview\\w*\\b|\\bfocus group\\w*\\b|\\busage data\\b|\\bevidence[- ]?informed\\b",
  "Assessment and UX", "User Experience", "\\buser experience\\b|\\bux\\b|\\bethnograph\\w*\\b|\\busability\\b",
  "Community and Partnerships", "Campus Partnerships", "\\bcampus partner\\w*\\b|\\bstudent affairs\\b|\\bstudent government\\b|\\bcollaborat\\w*\\b|\\bpartnership\\w*\\b",
  "Community and Partnerships", "Community Engagement", "\\bcommunity engagement\\b|\\boutreach\\b|\\bcommunity building\\b",
  "Services and Programming", "Programming", "\\bprogramming\\b|\\bprograming\\b|\\bprograms?\\b",
  "Services and Programming", "Collections Engagement", "\\bdisplay\\w*\\b|\\bcollection\\w*\\b|\\bleisure reading\\b|\\bspecial collections\\b",
  "Digital Library", "Digital / Virtual Space", "\\bdigital\\b|\\bvirtual\\b|\\bonline\\b|\\bwebsite\\b",
  "Digital Library", "Emerging Technology", "\\bvirtual reality\\b|\\bvr\\b|\\btechnology\\b",
  "Organizational Practice", "Intentional Practice", "\\bintentional\\w*\\b|\\breflective practice\\b",
  "Organizational Practice", "Adaptability / Transferability", "\\badapt\\w*\\b|\\btransfer\\w*\\b|\\breplicab\\w*\\b|\\bscalab\\w*\\b",
  "Organizational Practice", "Low-Cost / Resource-Conscious", "\\blow[- ]?cost\\b|\\bshoestring\\b|\\bbudget\\b"
)

# ---- Editorial rubric ---------------------------------------

rubric <- tribble(
  ~criterion, ~weight, ~prompt,
  "Contribution", 35, "What will a reader understand, see differently, or think more deeply about?",
  "Adaptability", 25, "Can readers in other contexts draw useful lessons and adapt them?",
  "Grounding", 20, "Is the basis clear and appropriate: research, theory, assessment, reflective practice, lived experience, or professional expertise?",
  "Clarity", 10, "Is the proposed chapter's focus, key ideas, and direction clear?",
  "Lasting Relevance", 10, "What remains useful beyond the immediate project, technology, institution, or moment?"
)

# ---- Locate authorized local workbook -----------------------

xlsx <- list.files(
  "data",
  pattern = "\\.xlsx$",
  full.names = TRUE,
  ignore.case = TRUE
)

workbook_path <- if (length(xlsx)) xlsx[1] else NA_character_

# ---- Load and organize workbook -----------------------------

load_book <- function(path) {
  validate(
    need(
      !is.na(path) && file.exists(path),
      "Place an authorized review workbook in data/. No workbook is distributed with the public repository."
    )
  )

  sh <- excel_sheets(path)

  proposal_sheet <- if ("Form Responses 1" %in% sh) {
    "Form Responses 1"
  } else {
    sh[1]
  }

  review_sheet <- if ("Proposal Reviews" %in% sh) {
    "Proposal Reviews"
  } else {
    NA_character_
  }

  raw <- read_excel(path, sheet = proposal_sheet)

  p <- tibble(
    proposal_id = seq_len(nrow(raw)),
    first_name = clean_text(first_existing(raw, c("First Name *", "First Name"))),
    last_name = clean_text(first_existing(raw, c("Last Name *", "Last Name"))),
    title = clean_text(first_existing(raw, c("Tentative Chapter Title *", "Proposal Title", "Title"))),
    proposal = clean_text(first_existing(raw, c("Proposal Description * (500 words max)", "Proposal Description"))),
    approach = clean_text(first_existing(raw, c("Approach *", "Approach"))),
    section = clean_text(first_existing(raw, c("Which section best fits your chapter? *", "Section of book", "Section"))),
    author_comments = clean_text(first_existing(raw, c("Additional Comments (optional)", "Additional Comments")))
  ) |>
    mutate(
      author = str_squish(str_c(first_name, last_name, sep = " ")),
      title_key = title_key_fn(title),
      qualitative_text = str_to_lower(str_c(title, proposal, author_comments, sep = " "))
    )

  if (!is.na(review_sheet)) {
    rr0 <- read_excel(path, sheet = review_sheet)

    rr <- tibble(
      review_row = seq_len(nrow(rr0)),
      title = clean_text(first_existing(rr0, c("Proposal Title", "Tentative Chapter Title *"))),
      reviewer = str_to_lower(clean_text(first_existing(rr0, c("First Pass Person", "First Pass")))),
      decision = normalize_decision(first_existing(rr0, c("First Pass? (Y/N)", "Decision"))),
      review_note = clean_text(first_existing(rr0, c("If no, Why?", "Review Note", "Notes"))),
      keywords = clean_text(first_existing(rr0, c("Keywords, Tags, other notes", "Keywords", "Tags")))
    ) |>
      mutate(
        title_key = title_key_fn(title),
        coding_score = as.integer(review_note != "") + as.integer(keywords != "")
      )

    dup <- rr |>
      count(reviewer, title_key, name = "n_rows") |>
      filter(n_rows > 1)

    current <- rr |>
      group_by(reviewer, title_key) |>
      arrange(desc(coding_score), desc(review_row), .by_group = TRUE) |>
      slice(1) |>
      ungroup()

    p <- p |>
      left_join(
        current |>
          select(title_key, reviewer, decision, review_note, keywords),
        by = "title_key"
      )
  } else {
    rr <- tibble()
    dup <- tibble()
    p <- p |>
      mutate(
        reviewer = NA_character_,
        decision = NA_character_,
        review_note = NA_character_,
        keywords = NA_character_
      )
  }

  tags_detected <- crossing(
    p |>
      select(proposal_id, title, author, section, approach, qualitative_text),
    theme_codebook
  ) |>
    mutate(
      hit = str_detect(
        qualitative_text,
        regex(pattern, ignore_case = TRUE)
      )
    ) |>
    filter(hit) |>
    distinct(proposal_id, title, author, section, approach, family, theme)

  list(
    p = p,
    rr = rr,
    dup = dup,
    tags = tags_detected,
    proposal_sheet = proposal_sheet,
    review_sheet = review_sheet,
    book = basename(path)
  )
}

# ============================================================
# UI
# ============================================================

ui <- page_sidebar(
  title = "Creating Welcoming Spaces — Editorial Review Analysis",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  sidebar = sidebar(
    selectInput("reviewer", "Reviewer", "All"),
    selectInput("section", "Section", "All"),
    selectInput("approach", "Approach", "All"),
    selectInput("decision", "First-pass decision", c("All", "Yes", "Maybe", "No")),
    hr(),
    p(
      class = "text-muted",
      "Descriptive evidence supports review; tags and counts do not make editorial decisions."
    )
  ),

  navset_card_tab(
    nav_panel(
      "Overview",
      layout_columns(
        value_box("Proposals", textOutput("nprop")),
        value_box("Sections", textOutput("nsec")),
        value_box("Approaches", textOutput("napp")),
        value_box("Duplicate review groups", textOutput("ndup")),
        col_widths = c(3, 3, 3, 3)
      ),
      layout_columns(
        card(card_header("Proposals by section"), plotOutput("section_plot")),
        card(card_header("First-pass decisions"), plotOutput("decision_plot")),
        col_widths = c(7, 5)
      ),
      card(
        card_header("Interpretation"),
        p("This view keeps author-supplied structure, project-developed themes, reviewer coding, and rubric judgment separate."),
        p(strong("Shared vocabulary is not the same thing as duplication."))
      )
    ),

    nav_panel(
      "Sections & Approaches",
      layout_columns(
        card(card_header("Sections"), plotOutput("section_plot2", height = 420)),
        card(card_header("Approaches"), plotOutput("approach_plot", height = 420)),
        col_widths = c(6, 6)
      ),
      card(card_header("Section × approach"), DTOutput("section_approach"))
    ),

    nav_panel(
      "Themes & Tags",
      card(
        card_header("Recurring project-developed themes"),
        p(class = "text-muted", "ACRL-aligned analytic vocabulary; not official ACRL subject headings."),
        wordcloud2Output("theme_cloud", height = "500px")
      ),
      card(card_header("Theme × section"), DTOutput("theme_section")),
      card(card_header("Reviewer-entered keywords / tags"), DTOutput("keyword_table"))
    ),

    nav_panel(
      "Proposal Review",
      layout_columns(
        card(
          card_header("Choose proposal"),
          selectInput("pick", NULL, character(0)),
          uiOutput("meta"),
          hr(),
          uiOutput("review")
        ),
        card(
          card_header("Analytic profile"),
          h6("Detected themes"),
          uiOutput("ptags"),
          hr(),
          h6("Proposal description"),
          div(style = "max-height:460px; overflow-y:auto;", uiOutput("ptext"))
        ),
        col_widths = c(4, 8)
      )
    ),

    nav_panel(
      "Rubric",
      card(card_header("Proposal being reviewed"), card_body(uiOutput("rubric_proposal_context"))),
      layout_columns(
        card(
          card_header("Five scored criteria"),
          p("1 = does not yet meet; 2 = partially meets; 3 = meets well; 4 = exceeds."),
          sliderInput("c1", "Contribution — 35%", 1, 4, 3, 1),
          sliderInput("c2", "Adaptability — 25%", 1, 4, 3, 1),
          sliderInput("c3", "Grounding — 20%", 1, 4, 3, 1),
          sliderInput("c4", "Clarity — 10%", 1, 4, 3, 1),
          sliderInput("c5", "Lasting Relevance — 10%", 1, 4, 3, 1),
          value_box("Weighted score", textOutput("score"))
        ),
        card(card_header("Reading prompts"), DTOutput("rubric_table")),
        col_widths = c(5, 7)
      ),
      card(
        card_header("Collection-level questions"),
        tags$ol(
          tags$li(strong("Strengthen: "), "Does it make the collection stronger?"),
          tags$li(strong("Duplicate: "), "Does another proposal cover the same ground better?"),
          tags$li(strong("Gap: "), "Does it fill an important thematic gap?"),
          tags$li(strong("Represent: "), "Does it broaden the volume's balance?"),
          tags$li(strong("Return: "), "Will readers cite it and come back to it?")
        ),
        p(class = "text-muted", "Scores support comparison; they do not assemble the book.")
      )
    ),

    nav_panel(
      "Decision Path",
      card(
        card_header("Editorial decision path"),
        tags$ol(
          tags$li(strong("Grounded? "), "Enough basis to evaluate?"),
          tags$li(strong("Meaningful contribution? "), "Adds something worth having?"),
          tags$li(strong("Adaptable and durable? "), "Useful beyond this exact project?"),
          tags$li(strong("Distinct from overlap? "), "Different contribution from nearby proposals?"),
          tags$li(strong("Strengthens collection? "), "Improves the book as an integrated work?")
        ),
        tags$blockquote("A strong edited volume is a curated scholarly conversation, not a stack of good chapters.")
      ),
      card(card_header("Proposals with substantial thematic overlap"), DTOutput("overlap"))
    ),

    nav_panel(
      "Method",
      card(
        card_header("First-pass review workflow"),
        p(strong("How to use this review app")),
        p("Use this app to read proposals, compare themes, review overlap, and apply the editorial rubric."),
        p("Keep the authoritative editorial record outside this public repository.")
      ),
      card(
        card_header("Analysis layers"),
        tags$ol(
          tags$li("Preserve author-supplied section and approach."),
          tags$li("Describe recurring proposal language and project-developed themes."),
          tags$li("Keep reviewer keywords, notes, and decisions as a separate human-coded layer."),
          tags$li("Use five weighted rubric criteria for individual proposals."),
          tags$li("Use five collection questions for editorial curation."),
          tags$li("Never convert tag frequency or overlap automatically into a score or decision.")
        )
      ),
      card(card_header("Data QA"), verbatimTextOutput("qa"))
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  d <- reactive(load_book(workbook_path))

  observe({
    p <- d()$p

    updateSelectInput(
      session,
      "reviewer",
      choices = c("All", sort(unique(na.omit(p$reviewer))))
    )

    updateSelectInput(
      session,
      "section",
      choices = c("All", sort(unique(p$section[p$section != ""])))
    )

    updateSelectInput(
      session,
      "approach",
      choices = c("All", sort(unique(p$approach[p$approach != ""])))
    )

    updateSelectInput(
      session,
      "pick",
      choices = setNames(p$proposal_id, p$title)
    )
  })

  f <- reactive({
    p <- d()$p

    if (input$reviewer != "All") p <- filter(p, reviewer == input$reviewer)
    if (input$section != "All") p <- filter(p, section == input$section)
    if (input$approach != "All") p <- filter(p, approach == input$approach)
    if (input$decision != "All") p <- filter(p, decision == input$decision)

    p
  })

  output$nprop <- renderText(nrow(f()))
  output$nsec <- renderText(n_distinct(f()$section[f()$section != ""]))
  output$napp <- renderText(n_distinct(f()$approach[f()$approach != ""]))
  output$ndup <- renderText(nrow(d()$dup))

  barplotter <- function(df, var) {
    ggplot(df, aes(x = reorder({{ var }}, n), y = n)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Proposals") +
      theme_minimal(base_size = 12)
  }

  output$section_plot <- renderPlot({
    f() |>
      filter(section != "") |>
      count(section) |>
      barplotter(section)
  })

  output$section_plot2 <- renderPlot({
    f() |>
      filter(section != "") |>
      count(section) |>
      barplotter(section)
  })

  output$approach_plot <- renderPlot({
    f() |>
      filter(approach != "") |>
      count(approach) |>
      barplotter(approach)
  })

  output$decision_plot <- renderPlot({
    f() |>
      filter(!is.na(decision)) |>
      count(decision) |>
      ggplot(aes(x = decision, y = n)) +
      geom_col() +
      labs(x = NULL, y = "Proposals") +
      theme_minimal(base_size = 12)
  })

  output$section_approach <- renderDT({
    f() |>
      filter(section != "", approach != "") |>
      count(section, approach, name = "n") |>
      pivot_wider(names_from = approach, values_from = n, values_fill = 0) |>
      datatable(options = list(scrollX = TRUE), rownames = FALSE)
  })

  tf <- reactive({
    d()$tags |>
      filter(proposal_id %in% f()$proposal_id)
  })

  output$theme_cloud <- renderWordcloud2({
    cloud_data <- tf() |>
      distinct(proposal_id, theme) |>
      count(theme, name = "freq", sort = TRUE)

    req(nrow(cloud_data) > 0)

    wordcloud2(
      cloud_data,
      size = 0.25,
      minSize = 3,
      gridSize = 6,
      rotateRatio = 0,
      ellipticity = 0.8,
      backgroundColor = "white"
    )
  })

  output$theme_section <- renderDT({
    tf() |>
      distinct(proposal_id, section, theme) |>
      count(theme, section, name = "n") |>
      pivot_wider(names_from = section, values_from = n, values_fill = 0) |>
      datatable(options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })

  output$keyword_table <- renderDT({
    f() |>
      filter(!is.na(keywords), keywords != "") |>
      select(title, reviewer, decision, keywords, review_note) |>
      datatable(options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  sel <- reactive({
    req(input$pick)
    d()$p |>
      filter(proposal_id == as.integer(input$pick)) |>
      slice(1)
  })

  output$meta <- renderUI({
    x <- sel()
    tagList(
      tags$p(tags$b(x$title)),
      tags$p(x$author),
      tags$p(tags$b("Section: "), x$section),
      tags$p(tags$b("Approach: "), x$approach)
    )
  })

  output$review <- renderUI({
    x <- sel()
    tagList(
      tags$p(tags$b("Decision: "), ifelse(is.na(x$decision), "Not recorded", x$decision)),
      tags$p(tags$b("Keywords/tags: "), ifelse(is.na(x$keywords) || x$keywords == "", "None recorded", x$keywords)),
      tags$p(tags$b("Reviewer note: "), ifelse(is.na(x$review_note) || x$review_note == "", "None recorded", x$review_note))
    )
  })

  output$ptags <- renderUI({
    x <- sel()

    z <- d()$tags |>
      filter(proposal_id == x$proposal_id) |>
      arrange(family, theme)

    if (!nrow(z)) return(tags$p("No analytic themes detected."))

    tagList(
      lapply(
        split(z$theme, z$family),
        function(v) tags$p(paste(unique(v), collapse = "; "))
      )
    )
  })

  output$ptext <- renderUI(tags$p(sel()$proposal))

  output$rubric_table <- renderDT({
    rubric |>
      transmute(
        Criterion = criterion,
        Weight = paste0(weight, "%"),
        `Reading prompt` = prompt
      ) |>
      datatable(options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })

  output$rubric_proposal_context <- renderUI({
    x <- sel()
    req(nrow(x) == 1)

    tagList(
      h4(x$title),
      p(strong("Author: "), x$author),
      p(strong("Section: "), x$section, "  |  ", strong("Approach: "), x$approach),
      p(strong("Current first-pass decision: "), ifelse(is.na(x$decision), "Not recorded", x$decision))
    )
  })

  output$score <- renderText({
    round(
      input$c1 / 4 * 35 +
        input$c2 / 4 * 25 +
        input$c3 / 4 * 20 +
        input$c4 / 4 * 10 +
        input$c5 / 4 * 10,
      1
    )
  })

  output$overlap <- renderDT({
    z <- tf() |>
      distinct(proposal_id, title, theme)

    overlap_pairs <- z |>
      inner_join(
        z,
        by = "theme",
        suffix = c("_a", "_b"),
        relationship = "many-to-many"
      ) |>
      filter(proposal_id_a < proposal_id_b) |>
      group_by(proposal_id_a, proposal_id_b, title_a, title_b) |>
      summarise(
        shared_themes = n_distinct(theme),
        themes = paste(sort(unique(theme)), collapse = "; "),
        .groups = "drop"
      ) |>
      arrange(desc(shared_themes), title_a, title_b) |>
      filter(shared_themes >= 4) |>
      select(title_a, title_b, shared_themes, themes) |>
      rename(
        `Proposal A` = title_a,
        `Proposal B` = title_b,
        `Shared themes` = shared_themes,
        `Themes in common` = themes
      )

    datatable(
      overlap_pairs,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$qa <- renderText({
    paste(
      "Workbook:", d()$book,
      "\nProposal sheet:", d()$proposal_sheet,
      "\nReview sheet:", ifelse(is.na(d()$review_sheet), "not found", d()$review_sheet),
      "\nProposal rows:", nrow(d()$p),
      "\nUnique titles:", n_distinct(d()$p$title_key),
      "\nReview rows:", nrow(d()$rr),
      "\nDuplicate reviewer/title groups flagged:", nrow(d()$dup),
      "\nUnmatched review titles:", sum(!d()$rr$title_key %in% d()$p$title_key),
      "\nReview records without normalized decision:", sum(is.na(d()$p$decision)),
      "\n\nOverlap is descriptive, not a duplicate decision.",
      "\nRubric scores support comparison; collection-level judgment makes the final selection."
    )
  })
}

shinyApp(ui = ui, server = server)
