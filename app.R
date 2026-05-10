# app.R - Downs & Black checklist with HTML export + risk-of-bias plot ----

library(shiny)
library(bslib)
library(ggplot2)
library(base64enc)

# ----------------------- Questions database ---------------------------

questions_db <- list(
  # Reporting (1-10)
  list(id = "q1",  number = 1,  section = "Reporting",
       text = "1. Is the hypothesis/aim/objective of the study clearly described?", max = 1),
  list(id = "q2",  number = 2,  section = "Reporting",
       text = "2. Are the main outcomes to be measured clearly described in the Introduction or
Methods section? If the main outcomes are first mentioned in the Results
section, the question should be answered no.", max = 1),
  list(id = "q3",  number = 3,  section = "Reporting",
       text = "3. Are the characteristics of the patients included in the study clearly described?
In cohort studies and trials, inclusion and/or exclusion criteria should be given.
In case-control studies, a case-definition and the source for controls should be
given.", max = 1),
  list(id = "q4",  number = 4,  section = "Reporting",
       text = "4. Are the interventions of interest clearly described? Treatments and placebo
(where relevant) that are to be compared should be clearly described.", max = 1),
  list(id = "q5",  number = 5,  section = "Reporting",
       text = "5. Are the distributions of principal confounders in each group of subjects to be
compared clearly described? A list of principal confounders is provided.", max = 2),
  list(id = "q6",  number = 6,  section = "Reporting",
       text = "6. Are the main findings of the study clearly described? Simple outcome data
(including denominators and numerators) should be reported for all major
findings so that the reader can check the major analyses and conclusions. (This
question does not cover statistical tests which are considered below).", max = 1),
  list(id = "q7",  number = 7,  section = "Reporting",
       text = "7. Does the study provide estimates of the random variability in the data for the
main outcomes? In non-normally distributed data the interquartile range of
results should be reported. In normally distributed data the standard error,
standard deviation or confidence intervals should be reported. If the
distribution of the data is not described, it must be assumed that the estimates
used were appropriate and the question should be answered yes.", max = 1),
  list(id = "q8",  number = 8,  section = "Reporting",
       text = "8. Have all important adverse events that may be a consequence of the
intervention been reported? This should be answered yes if the study
demonstrates that there was a comprehensive attempt to measure adverse
events. (A list of possible adverse events is provided).", max = 1),
  list(id = "q9",  number = 9,  section = "Reporting",
       text = "9. Have the characteristics of patients lost to follow-up been described? This
should be answered yes where there were no losses to follow-up or where
losses to follow-up were so small that findings would be unaffected by their
inclusion. This should be answered no where a study does not report the
number of patients lost to follow-up.", max = 1),
  list(id = "q10", number = 10, section = "Reporting",
       text = "10. Have actual probability values been reported (e.g. 0.035 rather than <0.05)
for the main outcomes except where the probability value is less than 0.001?", max = 1),
  
  # External validity (11-13)
  list(id = "q11", number = 11, section = "External validity",
       text = "11. Were the subjects asked to participate in the study representative of the entire
population from which they were recruited? The study must identify the source
population for patients and describe how the patients were selected. Patients
would be representative if they comprised the entire source population, an
unselected sample of consecutive patients, or a random sample. Random
sampling is only feasible where a list of all members of the relevant population
exists. Where a study does not report the proportion of the source population
from which the patients are derived, the question should be answered as unable
to determine.", max = 1),
  list(id = "q12", number = 12, section = "External validity",
       text = "12. Were those subjects who were prepared to participate representative of the
entire population from which they were recruited? The proportion of those
asked who agreed should be stated. Validation that the sample was
representative would include demonstrating that the distribution of the main
confounding factors was the same in the study sample and the source
population.", max = 1),
  list(id = "q13", number = 13, section = "External validity",
       text = "13. Were the staff, places, and facilities where the patients were treated,
representative of the treatment the majority of patients receive? For the
question to be answered yes the study should demonstrate that the intervention
was representative of that in use in the source population. The question should
be answered no if, for example, the intervention was undertaken in a specialist
centre unrepresentative of the hospitals most of the source population would
attend.", max = 1),
  
  # Internal validity - Bias (14-20)
  list(id = "q14", number = 14, section = "Internal validity - Bias",
       text = "14. Was an attempt made to blind study subjects to the intervention they have
received? For studies where the patients would have no way of knowing which
intervention they received, this should be answered yes.", max = 1),
  list(id = "q15", number = 15, section = "Internal validity - Bias",
       text = "15. Was an attempt made to blind those measuring the main outcomes of the intervention?", max = 1),
  list(id = "q16", number = 16, section = "Internal validity - Bias",
       text = "16. If any of the results of the study were based on “data dredging”, was this made
clear? Any analyses that had not been planned at the outset of the study should
be clearly indicated. If no retrospective unplanned subgroup analyses were
reported, then answer yes.", max = 1),
  list(id = "q17", number = 17, section = "Internal validity - Bias",
       text = "17. In trials and cohort studies, do the analyses adjust for different lengths of
follow-up of patients, or in case-control studies, is the time period between the
intervention and outcome the same for cases and controls? Where follow-up
was the same for all study patients the answer should be yes. If different
lengths of follow-up were adjusted for by, for example, survival analysis the
answer should be yes. Studies where differences in follow-up are ignored
should be answered no.", max = 1),
  list(id = "q18", number = 18, section = "Internal validity - Bias",
       text = "18. Were the statistical tests used to assess the main outcomes appropriate? The
statistical techniques used must be appropriate to the data. For example
nonparametric methods should be used for small sample sizes. Where little
statistical analysis has been undertaken but where there is no evidence of bias,
the question should be answered yes. If the distribution of the data (normal or
not) is not described it must be assumed that the estimates used were
appropriate and the question should be answered yes.", max = 1),
  list(id = "q19", number = 19, section = "Internal validity - Bias",
       text = "19. Was compliance with the intervention/s reliable? Where there was noncompliance
with the allocated treatment or where there was contamination of
one group, the question should be answered no. For studies where the effect of
any misclassification was likely to bias any association to the null, the question
should be answered yes.", max = 1),
  list(id = "q20", number = 20, section = "Internal validity - Bias",
       text = "20. Were the main outcome measures used accurate (valid and reliable)? For
studies where the outcome measures are clearly described, the question should
be answered yes. For studies which refer to other work or that demonstrates the
outcome measures are accurate, the question should be answered as yes.", max = 1),
  
  # Internal validity - Confounding (21-26)
  list(id = "q21", number = 21, section = "Internal validity - Confounding",
       text = "21. Were the patients in different intervention groups (trials and cohort studies) or
were the cases and controls (case-control studies) recruited from the same
population? For example, patients for all comparison groups should be
selected from the same hospital. The question should be answered unable to
determine for cohort and case-control studies where there is no information concerning the source of patients included in the study.", max = 1),
  list(id = "q22", number = 22, section = "Internal validity - Confounding",
       text = "22. Were study subjects in different intervention groups (trials and cohort studies)
or were the cases and controls (case-control studies) recruited over the same
period of time? For a study which does not specify the time period over which
patients were recruited, the question should be answered as unable to
determine.", max = 1),
  list(id = "q23", number = 23, section = "Internal validity - Confounding",
       text = "23. Were study subjects randomized to intervention groups? Studies which state
that subjects were randomized should be answered yes except where method of
randomization would not ensure random allocation. For example alternate
allocation would score no because it is predictable.", max = 1),
  list(id = "q24", number = 24, section = "Internal validity - Confounding",
       text = "24. Was the randomized intervention assignment concealed from both patients and
health care staff until recruitment was complete and irrevocable? All nonrandomized
studies should be answered no. If assignment was concealed from
patients but not from staff, it should be answered no.", max = 1),
  list(id = "q25", number = 25, section = "Internal validity - Confounding",
       text = "25. Was there adequate adjustment for confounding in the analyses from which the
main findings were drawn? This question should be answered no for trials if:
the main conclusions of the study were based on analyses of treatment rather
than intention to treat; the distribution of known confounders in the different
treatment groups was not described; or the distribution of known confounders
differed between the treatment groups but was not taken into account in the
analyses. In non-randomized studies if the effect of the main confounders was
not investigated or confounding was demonstrated but no adjustment was
made in the final analyses the question should be answered as no.", max = 1),
  list(id = "q26", number = 26, section = "Internal validity - Confounding",
       text = "26. Were losses of patients to follow-up taken into account? If the numbers of
patients lost to follow-up are not reported, the question should be answered as
unable to determine. If the proportion lost to follow-up was too small to affect
the main findings, the question should be answered yes.", max = 1),
  
  # Power (27 - modified)
  list(id = "q27", number = 27, section = "Power",
       text = "27. Did the study have sufficient power to detect a clinically important effect
where the probability value for a difference being due to chance is less than
5%? Sample sizes have been calculated to detect a difference of x% and y%.", max = 1)
)

section_levels <- c(
  "Reporting",
  "External validity",
  "Internal validity - Bias",
  "Internal validity - Confounding",
  "Power"
)

# ---------------------- Helper functions ------------------------------

semaphore_color <- function(pct) {
  if (is.na(pct)) {
    "#bdc3c7"              # grey
  } else if (pct >= 0.80) {
    "#27ae60"              # green
  } else if (pct >= 0.50) {
    "#f1c40f"              # yellow
  } else {
    "#e74c3c"              # red
  }
}

semaphore_label <- function(pct) {
  if (is.na(pct)) {
    "Not available"
  } else if (pct >= 0.80) {
    "High quality"
  } else if (pct >= 0.50) {
    "Moderate quality"
  } else {
    "Low quality"
  }
}

risk_text <- function(pct) {
  if (is.na(pct)) {
    "Not available"
  } else if (pct >= 0.80) {
    "Low risk of bias"
  } else if (pct >= 0.50) {
    "Some concerns"
  } else {
    "High risk of bias"
  }
}

risk_category <- function(pct) {
  if (is.na(pct)) {
    "Not available"
  } else if (pct >= 0.80) {
    "Low risk"
  } else if (pct >= 0.50) {
    "Some concerns"
  } else {
    "High risk"
  }
}

# ----------------------------- UI -------------------------------------

ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "litera"),
  titlePanel("Downs & Black Checklist Template"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Article information"),
      textInput("article_name", "Article name"),
      textInput("author_name", "Authors"),
      textInput("evaluator_name", "Evaluator name"),
      hr(),
      div(
        style = "text-align: center; background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
        h3("Total score"),
        h1(textOutput("total_score"), style = "color: #2c3e50; font-weight: bold; margin: 0;"),
        span("/ 28 points", style = "color: #7f8c8d;")
      ),
      uiOutput("total_semaphore"),
      uiOutput("section_semaphores"),
      hr(),
      downloadButton("download_html", "Download checklist report (HTML)", class = "btn-block btn-primary"),
      hr(),
      tags$p(
        "Based on modified Downs & Black checklist. Reference: Downs, S. H., & Black, N. (1998). The feasibility of creating a checklist for the assessment of the methodological quality both of randomised and non-randomised studies of health care interventions. Journal of epidemiology and community health, 52(6), 377–384. https://doi.org/10.1136/jech.52.6.377.",
        style = "font-size: 11px; color: #7f8c8d;"
      )
    ),
    
    mainPanel(
      width = 9,
      h4("Section risk-of-bias summary"),
      plotOutput("section_plot", height = "260px"),
      hr(),
      fluidRow(
        style = "background-color: #34495e; color: white; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
        column(6, strong("Checklist item / Question")),
        column(2, strong("Checkpoint (Score)")),
        column(4, strong("Notes / Justification"))
      ),
      uiOutput("questions_ui")
    )
  )
)

# ---------------------------- SERVER ----------------------------------

server <- function(input, output, session) {
  
  # Render questions grouped by section
  output$questions_ui <- renderUI({
    sections <- section_levels
    
    tagList(
      lapply(sections, function(sec) {
        sec_questions <- Filter(function(q) q$section == sec, questions_db)
        
        tagList(
          tags$h4(sec, style = "margin-top: 20px; font-weight: bold; color: #2c3e50;"),
          lapply(sec_questions, function(q) {
            
            choices_list <- if (q$max == 2) {
              c("No (0)" = 0, "Partially (1)" = 1, "Yes (2)" = 2)
            } else {
              # Para itens 11–27, o PDF prevê "Unable to determine = 0"
              if (q$number >= 11 && q$number <= 27) {
                c("No (0)" = 0, "Unable to determine (0)" = 0, "Yes (1)" = 1)
              } else {
                c("No (0)" = 0, "Yes (1)" = 1)
              }
            }
            
            bg_color <- ifelse(q$number %% 2 == 0, "#f9f9f9", "#ffffff")
            
            fluidRow(
              style = paste0(
                "background-color:", bg_color,
                "; padding: 10px; border-bottom: 1px solid #eee; align-items: center; display: flex;"
              ),
              column(6, span(q$text, style = "font-size: 14px;")),
              column(
                2,
                radioButtons(
                  inputId = paste0("score_", q$id),
                  label = NULL,
                  choices = choices_list,
                  selected = 0,
                  inline = FALSE
                )
              ),
              column(
                4,
                textAreaInput(
                  inputId = paste0("note_", q$id),
                  label = NULL,
                  placeholder = "Notes / Justification...",
                  rows = 2,
                  resize = "vertical"
                )
              )
            )
          })
        )
      })
    )
  })
  
  # Reactive data frame with responses
  responses_df <- reactive({
    do.call(
      rbind,
      lapply(questions_db, function(q) {
        x_score <- input[[paste0("score_", q$id)]]
        x_note  <- input[[paste0("note_", q$id)]]
        
        data.frame(
          article   = input$article_name,
          authors   = input$author_name,
          evaluator = input$evaluator_name,
          section   = q$section,
          number    = q$number,
          question  = q$text,
          max       = q$max,
          score     = if (is.null(x_score) || x_score == "") 0 else as.numeric(x_score),
          note      = if (is.null(x_note)) "" else as.character(x_note),
          stringsAsFactors = FALSE
        )
      })
    )
  })
  
  # Section and total summaries
  section_summary <- reactive({
    df <- responses_df()
    if (nrow(df) == 0) return(NULL)
    
    sections <- section_levels[section_levels %in% df$section]
    res <- lapply(sections, function(sec) {
      sub <- df[df$section == sec, ]
      score_sum <- sum(sub$score, na.rm = TRUE)
      max_sum   <- sum(sub$max,   na.rm = TRUE)
      pct <- if (max_sum > 0) score_sum / max_sum else NA_real_
      data.frame(
        section = sec,
        score   = score_sum,
        max     = max_sum,
        pct     = pct,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, res)
  })
  
  total_summary <- reactive({
    df <- responses_df()
    score_sum <- sum(df$score, na.rm = TRUE)
    max_sum   <- sum(df$max,   na.rm = TRUE)
    pct <- if (max_sum > 0) score_sum / max_sum else NA_real_
    list(score = score_sum, max = max_sum, pct = pct)
  })
  
  # Total score text
  output$total_score <- renderText({
    ts <- total_summary()
    ts$score
  })
  
  # Overall semaphore in UI
  output$total_semaphore <- renderUI({
    ts <- total_summary()
    pct <- ts$pct
    col <- semaphore_color(pct)
    lab <- semaphore_label(pct)
    
    div(
      style = "margin-top: 10px;",
      tags$div("Overall methodological quality", style = "font-weight: bold; font-size: 12px;"),
      tags$div(
        style = "display: flex; align-items: center; gap: 8px; margin-top: 4px;",
        tags$div(
          style = paste0(
            "width: 18px; height: 18px; border-radius: 50%; ",
            "background-color:", col, "; border: 1px solid #7f8c8d;"
          )
        ),
        span(
          sprintf("%d / %d (%.0f%%) - %s",
                  ts$score, ts$max, ifelse(is.na(pct), 0, pct * 100), lab),
          style = "font-size: 11px; color: #2c3e50;"
        )
      )
    )
  })
  
  # Section semaphores in UI
  output$section_semaphores <- renderUI({
    ss <- section_summary()
    if (is.null(ss) || nrow(ss) == 0) return(NULL)
    
    tagList(
      tags$h5("Section scores", style = "margin-top: 15px; margin-bottom: 6px;"),
      lapply(seq_len(nrow(ss)), function(i) {
        pct <- ss$pct[i]
        col <- semaphore_color(pct)
        lab <- semaphore_label(pct)
        
        div(
          style = "display: flex; align-items: center; gap: 8px; margin-bottom: 4px;",
          tags$div(
            style = paste0(
              "width: 14px; height: 14px; border-radius: 50%; ",
              "background-color:", col, "; border: 1px solid #7f8c8d;"
            )
          ),
          span(
            sprintf("%s: %d / %d (%.0f%%) - %s",
                    ss$section[i], ss$score[i], ss$max[i],
                    ifelse(is.na(pct), 0, pct * 100), lab),
            style = "font-size: 10.5px; color: #2c3e50;"
          )
        )
      })
    )
  })
  
  # GGPlot risk-of-bias style plot for UI
  output$section_plot <- renderPlot({
    ss <- section_summary()
    if (is.null(ss) || nrow(ss) == 0) return(NULL)
    
    df_plot <- ss
    df_plot$risk_cat <- factor(
      sapply(df_plot$pct, risk_category),
      levels = c("Low risk", "Some concerns", "High risk", "Not available")
    )
    
    df_plot$section <- factor(df_plot$section, levels = section_levels)
    
    ggplot(df_plot, aes(x = section, y = pct, fill = risk_cat)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(
        limits = c(0, 1),
        breaks = c(0, 0.25, 0.5, 0.75, 1),
        labels = function(x) paste0(round(x * 100), "%")
      ) +
      scale_fill_manual(
        values = c(
          "Low risk" = "#27ae60",
          "Some concerns" = "#f1c40f",
          "High risk" = "#e74c3c",
          "Not available" = "#bdc3c7"
        ),
        drop = FALSE
      ) +
      labs(
        x = NULL,
        y = "Percentage of maximum score",
        fill = "Risk of bias"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "bottom",
        axis.text.y = element_text(size = 10)
      )
  })
  
  # HTML download with embedded plot
  output$download_html <- downloadHandler(
    filename = function() {
      name_clean <- ifelse(
        nzchar(input$article_name),
        gsub("[^[:alnum:]]", "_", input$article_name),
        "DownsBlack_report"
      )
      paste0(name_clean, ".html")
    },
    content = function(file) {
      df <- responses_df()
      
      df$section <- factor(df$section, levels = section_levels)
      
      ord <- order(df$section, df$number)
      df <- df[ord, ]
      
      ss <- section_summary()
      ts <- total_summary()
      
      art  <- ifelse(nzchar(input$article_name),  input$article_name,  "Not specified")
      auth <- ifelse(nzchar(input$author_name),   input$author_name,   "Not specified")
      eval <- ifelse(nzchar(input$evaluator_name),input$evaluator_name,"Not specified")
      total_score <- ts$score
      total_max   <- ts$max
      total_pct   <- ts$pct
      
      total_quality <- semaphore_label(total_pct)
      total_risk    <- risk_text(total_pct)
      
      # CSS for HTML
      css <- "
        body { font-family: Arial, sans-serif; margin: 30px; }
        h1, h2, h3 { color: #2c3e50; }
        table { border-collapse: collapse; width: 100%; margin-bottom: 25px; }
        th, td { border: 1px solid #ccc; padding: 6px 8px; font-size: 12px; vertical-align: top; }
        th { background-color: #f0f0f0; text-align: left; }
        .meta { margin-bottom: 20px; }
        .meta p { margin: 2px 0; }
        .total-box {
          border: 1px solid #ddd;
          padding: 10px;
          display: inline-block;
          margin-top: 10px;
          background-color: #f8f9fa;
        }
      "
      
      html <- paste0(
        "<!DOCTYPE html>",
        "<html><head><meta charset='UTF-8'>",
        "<title>Downs &amp; Black Checklist Report</title>",
        "<style>", css, "</style>",
        "</head><body>"
      )
      
      # Header + summary text
      html <- paste0(
        html,
        "<h1>Downs &amp; Black Checklist Report</h1>",
        "<div class='meta'>",
        "<p><strong>Article:</strong> ", art, "</p>",
        "<p><strong>Authors:</strong> ", auth, "</p>",
        "<p><strong>Evaluator:</strong> ", eval, "</p>",
        "</div>",
        "<div class='total-box'>",
        "<p><strong>Total score:</strong> ", total_score, " / ", total_max,
        " (", ifelse(is.na(total_pct), "NA", sprintf("%.0f%%", total_pct * 100)), ")</p>",
        "<p><strong>Overall methodological quality:</strong> ", total_quality, "</p>",
        "<p><strong>Risk of bias summary report:</strong> ", total_risk, "</p>",
        "</div>",
        "<hr/>"
      )
      
      # Build risk-of-bias plot for sections and embed as base64 image
      if (!is.null(ss) && nrow(ss) > 0) {
        df_plot <- ss
        df_plot$risk_cat <- factor(
          sapply(df_plot$pct, risk_category),
          levels = c("Low risk", "Some concerns", "High risk", "Not available")
        )
        
        p <- ggplot(df_plot, aes(x = section, y = pct, fill = risk_cat)) +
          geom_col() +
          coord_flip() +
          scale_y_continuous(
            limits = c(0, 1),
            breaks = c(0, 0.25, 0.5, 0.75, 1),
            labels = function(x) paste0(round(x * 100), "%")
          ) +
          scale_fill_manual(
            values = c(
              "Low risk" = "#27ae60",
                     "Some concerns" = "#f1c40f",
                     "High risk" = "#e74c3c",
                     "Not available" = "#bdc3c7"
        ),
        drop = FALSE
      ) +
        labs(
          x = NULL,
          y = "Percentage of maximum score",
          fill = "Risk of bias"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          legend.position = "bottom",
          axis.text.y = element_text(size = 10)
        )
      
      plot_file <- tempfile(fileext = ".png")
      ggsave(plot_file, plot = p, width = 7, height = 3, dpi = 150)
      
      img_data <- base64enc::dataURI(file = plot_file, mime = "image/png")
      
      html <- paste0(
        html,
        "<h2>Section risk-of-bias summary</h2>",
        "<img src='", img_data,
        "' alt='Section risk-of-bias summary plot' style='max-width:100%; height:auto;'/>",
        "<hr/>"
      )
    }
    
    # Detailed checklist per section
      sections <- section_levels[section_levels %in% as.character(df$section)]    
    for (sec in sections) {
      sub <- df[df$section == sec, ]
      
      html <- paste0(
        html,
        "<h2>", sec, "</h2>",
        "<table>",
        "<tr>",
        "<th>Item</th>",
        "<th>Question</th>",
        "<th>Max score</th>",
        "<th>Score</th>",
        "<th>Notes / Justification</th>",
        "</tr>"
      )
      
      for (i in seq_len(nrow(sub))) {
        html <- paste0(
          html,
          "<tr>",
          "<td>", sub$number[i], "</td>",
          "<td>", sub$question[i], "</td>",
          "<td>", sub$max[i], "</td>",
          "<td>", sub$score[i], "</td>",
          "<td>", sub$note[i], "</td>",
          "</tr>"
        )
      }
      
      html <- paste0(html, "</table>")
    }
    
    html <- paste0(
      html,
      "<hr/>",
      "<p><em>Reference: Downs SH, Black N. ",
      "The feasibility of creating a checklist for the assessment of the methodological quality ",
      "both of randomised and non-randomised studies of health care interventions. ",
      "J Epidemiol Community Health. 1998;52(6):377-84. doi: https://doi.org/10.1136/jech.52.6.377.</em></p>",
      "</body></html>"
    )
    
    writeLines(html, file, useBytes = TRUE)
},
contentType = "text/html"
  )
  }

# Launch app
shinyApp(ui = ui, server = server)