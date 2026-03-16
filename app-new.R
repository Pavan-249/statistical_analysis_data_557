# ------------------------------------------------------------------------------
# Statistical Analysis of Gender Pay Disparities in University Faculty Salaries
# DATA 557 W26 Final Project - Shiny App
# ------------------------------------------------------------------------------

library(shiny)
library(tidyverse)
library(lmtest)
library(sandwich)
library(scales)
library(plotly)

# ------------------------------------------------------------------------------
# Data loading and preprocessing 
# ------------------------------------------------------------------------------
salary <- read.table(
  "salary.txt",
  header = TRUE,
  sep = "",
  stringsAsFactors = FALSE,
  na.strings = "NA"
)

salary <- salary %>%
  mutate(
    sex = factor(sex, levels = c("M", "F")),
    deg = factor(deg),
    field = factor(field),
    rank = factor(rank, levels = c("Assist", "Assoc", "Full")),
    admin = factor(admin, levels = c(0, 1)),
    startyr = as.integer(startyr),
    year = as.integer(year),
    yrdeg = as.integer(yrdeg),
    salary = as.numeric(salary),
    experience = year - startyr
  )

salary_clean <- salary %>%
  filter(experience >= 0, !is.na(rank))

sex_colours <- c("M" = "#4472C4", "F" = "#E05C5C")

# Derived datasets
start_salary <- salary_clean %>%
  filter(year == startyr) %>%
  mutate(log_salary = log(salary))

promotion_data <- salary_clean %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(
    prev_rank = lag(rank),
    prev_salary = lag(salary),
    prev_year = lag(year)
  ) %>%
  ungroup() %>%
  filter(prev_rank == "Assoc", rank == "Full") %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    raise_amount = salary - prev_salary,
    raise_pct = (salary - prev_salary) / prev_salary
  )

ever_assoc_ids <- salary_clean %>%
  group_by(id) %>%
  filter(any(rank == "Assoc")) %>%
  pull(id) %>%
  unique()

person_level <- salary_clean %>%
  filter(id %in% ever_assoc_ids) %>%
  arrange(id, year) %>%
  group_by(id) %>%
  summarise(
    sex = first(sex),
    deg = first(deg),
    field = first(field),
    yrdeg = first(yrdeg),
    promoted = as.integer(any(rank == "Full")),
    yr_first_assoc = min(year[rank == "Assoc"], na.rm = TRUE),
    yr_first_full = ifelse(any(rank == "Full"),
                           min(year[rank == "Full"], na.rm = TRUE), NA_real_),
    yrs_as_assoc = ifelse(any(rank == "Full"),
                          yr_first_full - yr_first_assoc, NA_real_),
    sal_assoc_entry = salary[rank == "Assoc"][which.min(year[rank == "Assoc"])],
    sal_assoc_last = salary[rank == "Assoc"][which.max(year[rank == "Assoc"])],
    sal_full_entry = ifelse(any(rank == "Full"),
                            salary[rank == "Full"][which.min(year[rank == "Full"])],
                            NA_real_),
    .groups = "drop"
  )

latest_salary <- salary_clean %>%
  arrange(id, year) %>%
  group_by(id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    log_salary = log(salary),
    experience = year - startyr
  )

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .instruction { font-size: 0.9em; color: #555; margin-bottom: 1em; }
      .section-title { margin-top: 1.2em; margin-bottom: 0.4em; }
      .sidebar-filter { min-width: 160px; }
    "))
  ),
  tags$div(
    class = "uw-banner",
    tags$img(src = "University-of-Washington-Logo.png", alt = "UW", height = "48px"),
    tags$span("University of Washington: DATA 557", style = "color:white; font-weight:bold;")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 2,
      class = "sidebar-filter",
      h5("Filter plots by sex", style = "margin-top: 0;"),
      radioButtons(
        "sex_filter",
        label = NULL,
        choices = c("Both" = "Both", "Male only" = "M", "Female only" = "F"),
        selected = "Both"
      ),
      hr(),
      p("Use the filter to compare groups or focus on one. Hover on plots for values.", class = "instruction")
    ),
    mainPanel(
      width = 10,
      navbarPage(
        title = "Explore Gender Pay trends in faculty salaries",
        tabPanel("Home",
                 fluidPage(
                   h2("Gender pay disparities in faculty salaries"),
                   p(em("DATA 557 — Explore how we analyzed the data and what we found.")),
                   p(strong("Authors:"), "Maanya Cola Bharath, Roxanne Dimadi, Ha Tien Nguyen, Hannah Sun, Pavan Suresh, Lay Yang"),
                   p("Use the tabs above to move through the analysis step by step. The sidebar filter applies to all plots: choose Both to compare men and women, or Male only / Female only to focus on one group. Hover on any plot for exact values.", class = "instruction"),
                   hr(),
                   h3("What we asked"),
                   tags$ul(
                     tags$li("Is there a gap in starting salaries?"),
                     tags$li("Do promotion raises differ by gender (Associate → Full)?"),
                     tags$li("Is there an overall salary gap after controlling for rank, field, experience?"),
                     tags$li("What limits generalizing these results?")
                   ),
                   p("All tests use α = 0.05, two-sided."),
                   h3("Data"),
                   p("One U.S. university, 1976–1995. Variables: sex, salary, rank, field, degree year, hire year, admin. Experience = year − hire year.")
                 )
        ),
        tabPanel("Explore the data",
                 fluidPage(
                   h3("Exploratory analysis"),
                   p("We first look at the shape of the data: how many records we have by sex, how average salary changes over time, and how salary differs by rank. These plots motivate why we control for rank and field in later steps."),
                   p("Tip: Use the sidebar to show both groups or focus on one. Hover on plots for values.", class = "instruction"),
                   fluidRow(
                     column(6, plotlyOutput("eda_obs_by_sex")),
                     column(6, plotlyOutput("eda_salary_time"))
                   ),
                   fluidRow(
                     column(12, plotlyOutput("eda_salary_rank"))
                   )
                 )
        ),
        tabPanel("Starting salaries",
                 fluidPage(
                   h3("First-year salaries at hire"),
                   p("We ask whether pay at hire differs by gender. We keep only each person’s first year of employment. The Distributions tab shows raw patterns by sex, field, and rank; the Tests & regression tab gives formal tests and models that control for rank, field, degree year, and hire year."),
                   p("Use the sidebar filter to compare or focus on one sex in the plots below.", class = "instruction"),
                   tabsetPanel(
                     tabPanel("Distributions",
                              fluidRow(
                                column(6, plotlyOutput("q1_hist_sex")),
                                column(6, plotlyOutput("q1_box_field"))
                              ),
                              fluidRow(
                                column(12, plotlyOutput("q1_box_rank_field"))
                              )
                     ),
                     tabPanel("Tests & regression",
                              fluidRow(
                                column(12, p("Welch t-test (difference in mean starting salary by sex):", class = "instruction")),
                                column(12, verbatimTextOutput("q1_ttest"))
                              ),
                              fluidRow(
                                column(12, p("Regression controlling for rank, field, degree year, hire year:", class = "section-title")),
                                column(12, verbatimTextOutput("q1_lm"))
                              ),
                              fluidRow(
                                column(12, p("Log-salary model (robust SE); coefficient for sex ≈ % difference:", class = "section-title")),
                                column(12, verbatimTextOutput("q1_lm_log_robust"))
                              )
                     )
                   )
                 )
        ),
        tabPanel("Promotion raises",
                 fluidPage(
                   h3("Salary increase at Associate → Full promotion"),
                   p("We look at the salary increase when someone is promoted from Associate to Full Professor—one row per person at their first such promotion. You can explore the distribution of dollar and percent raises, the statistical tests, and regression results that control for field and pre-promotion salary."),
                   p("The sidebar filter applies to the raise plot; try Male only or Female only to see each group’s distribution.", class = "instruction"),
                   tabsetPanel(
                     tabPanel("Raise by sex",
                              plotlyOutput("q2_raise_box")
                     ),
                     tabPanel("Tests",
                              fluidRow(
                                column(12, p("Welch t-test (dollar raise):", class = "section-title")),
                                column(12, verbatimTextOutput("q2_ttest_dollar"))
                              ),
                              fluidRow(
                                column(12, p("Welch t-test (percent raise):", class = "section-title")),
                                column(12, verbatimTextOutput("q2_ttest_pct"))
                              ),
                              fluidRow(
                                column(12, p("Wilcoxon rank-sum (percent raise):", class = "section-title")),
                                column(12, verbatimTextOutput("q2_wilcox"))
                              )
                     ),
                     tabPanel("Regression",
                              fluidRow(
                                column(12, p("Dollar raise ~ sex + field + yrdeg + pre-promotion salary (robust SE):", class = "section-title")),
                                column(12, verbatimTextOutput("q2_lm_raise"))
                              ),
                              fluidRow(
                                column(12, p("Percent raise ~ same predictors (robust SE):", class = "section-title")),
                                column(12, verbatimTextOutput("q2_lm_pct"))
                              )
                     ),
                     tabPanel("Promotion rates",
                              fluidRow(
                                column(12, p("Among those who were ever Associate Professors, share promoted to Full:", class = "instruction")),
                                column(12, tableOutput("q2_promotion_rates")),
                                column(12, verbatimTextOutput("q2_prop_test"))
                              )
                     )
                   )
                 )
        ),
        tabPanel("Overall salary",
                 fluidPage(
                   h3("Latest salary per person (cross-section)"),
                   p("We take each person’s most recent salary (one row per faculty) and ask whether a gender gap remains after controlling for rank, field, experience, and administrative role. Below are three model specifications: a baseline linear model, a log-salary model (so the sex coefficient is roughly a % difference), and a model that lets the gap differ for administrators."),
                   p("These results summarize the overall picture; the sidebar filter does not change the models (they use all data).", class = "instruction"),
                   fluidRow(
                     column(12, p("Baseline model (robust SE):", class = "section-title")),
                     column(12, verbatimTextOutput("q3_lm_robust"))
                   ),
                   fluidRow(
                     column(12, p("Log-salary (≈ % difference for sex; robust SE):", class = "section-title")),
                     column(12, verbatimTextOutput("q3_lm_log_robust"))
                   ),
                   fluidRow(
                     column(12, p("Log-salary with sex × admin (gap for non-admin vs admin; robust SE):", class = "section-title")),
                     column(12, verbatimTextOutput("q3_lm_interaction_robust"))
                   )
                 )
        ),
        tabPanel("Takeaways",
                 fluidPage(
                   p("This page summarizes limitations and what we found across the analyses in the tabs above."),
                   h3("Limitations"),
                   p("Single university, 1976–1995; no productivity/teaching data; observational; linearity and independence assumptions."),
                   h3("Summary"),
                   p("Starting salaries: gap for women (~4.85% lower after controls)."),
                   p("Promotion raises: no significant difference in raise size at Associate→Full; but women were less likely promoted and had ~5–6% lower pay within rank."),
                   p("Overall: women ~$409 less or ~5.7% (log model); gap larger among administrators."),
                   p("Disparities show up in starting pay, promotion rates, and within-rank pay rather than in the raise at promotion itself.")
                 )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive sex filter: "Both", "M", or "F"
  sex_filter <- reactive(input$sex_filter)
  
  # Helper: filter a dataframe by sex when filter is not "Both"
  filter_sex <- function(df, col = "sex") {
    if (sex_filter() == "Both") return(df)
    df %>% filter(!!sym(col) == sex_filter())
  }
  
  # EDA
  output$eda_obs_by_sex <- renderPlotly({
    d <- salary_clean %>% filter_sex()
    if (nrow(d) == 0) return(plotly::plotly_empty())
    p1 <- d %>%
      count(sex) %>%
      ggplot(aes(x = sex, y = n, fill = sex)) +
      geom_col(width = 0.6) +
      scale_fill_manual(values = sex_colours, drop = TRUE) +
      labs(title = if (sex_filter() != "Both") paste("Observations (", if (sex_filter() == "M") "Male" else "Female", " only)") else "Number of Observations by Sex",
           x = "Sex", y = "Number of Records")
    ggplotly(p1)
  })
  
  output$eda_salary_time <- renderPlotly({
    d <- salary_clean %>% filter_sex()
    if (nrow(d) == 0) return(plotly::plotly_empty())
    p2 <- d %>%
      group_by(year, sex) %>%
      summarise(mean_salary = mean(salary), .groups = "drop") %>%
      ggplot(aes(x = year, y = mean_salary, color = sex)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = sex_colours, drop = TRUE) +
      labs(title = if (sex_filter() != "Both") paste("Average Salary Over Time (", if (sex_filter() == "M") "Male" else "Female", " only)") else "Average Salary Over Time by Sex",
           x = "Year", y = "Average Salary")
    ggplotly(p2)
  })
  
  output$eda_salary_rank <- renderPlotly({
    d <- salary_clean %>% filter_sex()
    if (nrow(d) == 0) return(plotly::plotly_empty())
    p3 <- ggplot(d, aes(x = sex, y = salary, fill = sex)) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_manual(values = sex_colours, drop = TRUE) +
      facet_wrap(~ rank, scales = "free_y") +
      labs(title = if (sex_filter() != "Both") paste("Salary Within Rank (", if (sex_filter() == "M") "Male" else "Female", " only)") else "Salary by Sex Within Rank",
           x = "Sex", y = "Monthly Salary")
    ggplotly(p3)
  })
  
  # Starting salaries
  output$q1_hist_sex <- renderPlotly({
    d <- start_salary %>% filter_sex()
    if (nrow(d) == 0) return(plotly::plotly_empty())
    p4 <- ggplot(d, aes(x = salary, fill = sex)) +
      geom_histogram(alpha = 0.8, bins = 25, position = "identity") +
      scale_fill_manual(values = sex_colours, drop = TRUE) +
      facet_wrap(~ sex, scales = "free_y") +
      labs(title = if (sex_filter() != "Both") paste("Starting Salary (", if (sex_filter() == "M") "Male" else "Female", " only)") else "Starting Salary Distribution by Sex",
           x = "Starting Salary", y = "Count")
    ggplotly(p4)
  })
  
  output$q1_box_field <- renderPlotly({
    d <- start_salary %>% filter_sex()
    if (nrow(d) == 0) return(plotly::plotly_empty())
    p5 <- ggplot(d, aes(x = field, y = salary, fill = sex)) +
      geom_boxplot(alpha = 0.8, position = position_dodge(width = 0.75)) +
      scale_fill_manual(values = sex_colours, drop = TRUE) +
      labs(title = if (sex_filter() != "Both") paste("Starting Salary by Field (", if (sex_filter() == "M") "Male" else "Female", " only)") else "Starting Salary by Field and Sex",
           x = "Academic Field", y = "Starting Salary", fill = "Sex")
    ggplotly(p5)
  })
  
  output$q1_box_rank_field <- renderPlotly({
    d <- start_salary %>% filter_sex()
    if (nrow(d) == 0) return(plotly::plotly_empty())
    p6 <- ggplot(d, aes(x = sex, y = salary, fill = sex)) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_manual(values = sex_colours, drop = TRUE) +
      facet_grid(rank ~ field) +
      labs(title = if (sex_filter() != "Both") paste("Starting Salary by Rank & Field (", if (sex_filter() == "M") "Male" else "Female", " only)") else "Starting Salary by Sex, Rank, and Field",
           x = "Sex", y = "Starting Salary") +
      theme_minimal()
    ggplotly(p6)
  })
  
  output$q1_ttest <- renderPrint({
    t.test(salary ~ sex, data = start_salary)
  })
  
  q1_lm <- reactive({
    lm(salary ~ sex + rank + field + yrdeg + startyr, data = start_salary)
  })
  
  output$q1_lm <- renderPrint({
    summary(q1_lm())
  })
  
  q1_lm_log <- reactive({
    lm(log_salary ~ sex + rank + field + yrdeg + startyr, data = start_salary)
  })
  
  output$q1_lm_log_robust <- renderPrint({
    coeftest(q1_lm_log(), vcov = vcovHC(q1_lm_log(), type = "HC1"))
  })
  
  # Promotion raises
  output$q2_raise_box <- renderPlotly({
    d <- promotion_data %>% filter_sex()
    if (nrow(d) == 0) return(plotly::plotly_empty())
    promotion_long <- d %>%
      select(sex, raise_amount, raise_pct) %>%
      pivot_longer(cols = c(raise_amount, raise_pct), names_to = "metric", values_to = "value")
    p7 <- ggplot(promotion_long, aes(x = sex, y = value, fill = sex)) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_manual(values = sex_colours, drop = TRUE) +
      facet_wrap(~ metric, scales = "free_y",
                 labeller = as_labeller(c(raise_amount = "Dollar Raise", raise_pct = "Percent Raise"))) +
      labs(title = if (sex_filter() != "Both") paste("Raise at Promotion (", if (sex_filter() == "M") "Male" else "Female", " only)") else "Raise at Promotion by Sex", x = "Sex", y = NULL)
    ggplotly(p7)
  })
  
  output$q2_ttest_dollar <- renderPrint({
    t.test(raise_amount ~ sex, data = promotion_data)
  })
  
  output$q2_ttest_pct <- renderPrint({
    t.test(raise_pct ~ sex, data = promotion_data)
  })
  
  output$q2_wilcox <- renderPrint({
    wilcox.test(raise_pct ~ sex, data = promotion_data)
  })
  
  q2_lm_raise <- reactive({
    lm(raise_amount ~ sex + field + yrdeg + prev_salary, data = promotion_data)
  })
  
  output$q2_lm_raise <- renderPrint({
    coeftest(q2_lm_raise(), vcov = vcovHC(q2_lm_raise(), type = "HC1"))
  })
  
  q2_lm_pct <- reactive({
    lm(raise_pct ~ sex + field + yrdeg + prev_salary, data = promotion_data)
  })
  
  output$q2_lm_pct <- renderPrint({
    coeftest(q2_lm_pct(), vcov = vcovHC(q2_lm_pct(), type = "HC1"))
  })
  
  output$q2_promotion_rates <- renderTable({
    person_level %>%
      group_by(sex) %>%
      summarise(
        n = n(),
        n_promoted = sum(promoted),
        promotion_rate = round(mean(promoted), 3),
        .groups = "drop"
      )
  })
  
  output$q2_prop_test <- renderPrint({
    ct <- table(person_level$sex, person_level$promoted)
    chisq.test(ct, correct = FALSE)
  })
  
  # Overall salary models
  q3_lm <- reactive({
    lm(salary ~ sex + rank + field + experience + admin, data = latest_salary)
  })
  
  output$q3_lm_robust <- renderPrint({
    coeftest(q3_lm(), vcov = vcovHC(q3_lm(), type = "HC1"))
  })
  
  q3_lm_log <- reactive({
    lm(log_salary ~ sex + rank + field + experience + admin, data = latest_salary)
  })
  
  output$q3_lm_log_robust <- renderPrint({
    coeftest(q3_lm_log(), vcov = vcovHC(q3_lm_log(), type = "HC1"))
  })
  
  q3_lm_interaction <- reactive({
    lm(log_salary ~ sex * admin + rank + field + experience, data = latest_salary)
  })
  
  output$q3_lm_interaction_robust <- renderPrint({
    coeftest(q3_lm_interaction(), vcov = vcovHC(q3_lm_interaction(), type = "HC1"))
  })
}

# ------------------------------------------------------------------------------
# Run app
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)