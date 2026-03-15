# app.R
# Simple interactive explorer for the salary data
library(shiny)
library(tidyverse)
library(scales)
#-------------------------------
# Data loading & preparation
#-------------------------------
salary <- read.table(
  "salary.txt",
  header = TRUE,
  sep = "",
  stringsAsFactors = FALSE,
  na.strings = "NA"
)
salary <- salary %>%
  mutate(
    sex   = factor(sex, levels = c("M", "F")),
    deg   = factor(deg),
    field = factor(field),
    rank  = factor(rank, levels = c("Assist", "Assoc", "Full")),
    admin = factor(admin, levels = c(0, 1)),
    startyr   = as.integer(startyr),
    year      = as.integer(year),
    yrdeg     = as.integer(yrdeg),
    salary    = as.numeric(salary),
    experience = year - startyr
  )
salary_clean <- salary %>%
  filter(experience >= 0, !is.na(rank))
sex_colours <- c("M" = "#4472C4", "F" = "#E05C5C")
# Subsets used in the app (parallel to Rmd structure)
start_salary <- salary_clean %>%
  filter(year == startyr)
promotion_data <- salary_clean %>%
  arrange(id, year) %>%
  group_by(id) %>%
  mutate(
    prev_rank  = lag(rank),
    prev_salary = lag(salary),
    prev_year   = lag(year)
  ) %>%
  ungroup() %>%
  filter(prev_rank == "Assoc", rank == "Full") %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    raise_amount = salary - prev_salary,
    raise_pct    = (salary - prev_salary) / prev_salary
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
#-------------------------------
# UI
#-------------------------------
ui <- navbarPage(
  "Faculty Salary Explorer",
  tabPanel(
    "Overview",
    fluidPage(
      br(),
      fluidRow(
        column(
          4,
          h4("Dataset Summary"),
          tableOutput("overview_summary")
        ),
        column(
          4,
          h4("Counts by Sex & Rank"),
          tableOutput("overview_sex_rank")
        ),
        column(
          4,
          h4("Counts by Field"),
          tableOutput("overview_field")
        )
      )
    )
  ),
  tabPanel(
    "Q1: Starting Salary",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Controls"),
          selectInput(
            "q1_field",
            "Field (discipline):",
            choices = c("All", sort(unique(start_salary$field))),
            selected = "All"
          ),
          checkboxGroupInput(
            "q1_sex",
            "Sex:",
            choices = levels(start_salary$sex),
            selected = levels(start_salary$sex)
          ),
          width = 3
        ),
        mainPanel(
          h4("Starting Salary by Sex"),
          plotOutput("q1_box_sex", height = "300px"),
          br(),
          h4("Starting Salary by Hire Year and Sex"),
          plotOutput("q1_box_year", height = "300px"),
          br(),
          h4("Starting Salary Summary Table"),
          tableOutput("q1_summary_tbl")
        )
      )
    )
  ),
  tabPanel(
    "Q2: Promotion Raises",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Controls"),
          checkboxGroupInput(
            "q2_sex",
            "Sex:",
            choices = levels(promotion_data$sex),
            selected = levels(promotion_data$sex)
          ),
          radioButtons(
            "q2_metric",
            "Raise metric:",
            choices = c("Dollar raise" = "amount", "Percent raise" = "percent"),
            selected = "amount"
          ),
          width = 3
        ),
        mainPanel(
          h4("Salary Increase at Promotion (Associate → Full)"),
          plotOutput("q2_box", height = "300px"),
          br(),
          h4("Summary by Sex"),
          tableOutput("q2_summary_tbl")
        )
      )
    )
  ),
  tabPanel(
    "Q3: Latest Salary",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Controls"),
          checkboxGroupInput(
            "q3_rank",
            "Rank:",
            choices = levels(latest_salary$rank),
            selected = levels(latest_salary$rank)
          ),
          checkboxGroupInput(
            "q3_sex",
            "Sex:",
            choices = levels(latest_salary$sex),
            selected = levels(latest_salary$sex)
          ),
          width = 3
        ),
        mainPanel(
          h4("Latest Salary by Sex Within Rank"),
          plotOutput("q3_box_rank", height = "300px"),
          br(),
          h4("Latest Salary Density by Sex Within Rank"),
          plotOutput("q3_density_rank", height = "300px"),
          br(),
          h4("Summary Table"),
          tableOutput("q3_summary_tbl")
        )
      )
    )
  )
)
#-------------------------------
# Server
#-------------------------------
server <- function(input, output, session) {
  #---------------------------
  # Overview
  #---------------------------
  output$overview_summary <- renderTable({
    salary_clean %>%
      summarise(
        n_records = n(),
        n_people  = n_distinct(id),
        min_year  = min(year, na.rm = TRUE),
        max_year  = max(year, na.rm = TRUE)
      )
  })
  output$overview_sex_rank <- renderTable({
    salary_clean %>%
      count(sex, rank) %>%
      pivot_wider(names_from = rank, values_from = n, values_fill = 0)
  })
  output$overview_field <- renderTable({
    salary_clean %>%
      count(field, sort = TRUE)
  })
  #---------------------------
  # Q1: Starting salary
  #---------------------------
  q1_filtered <- reactive({
    dat <- start_salary
    if (input$q1_field != "All") {
      dat <- dat %>% filter(field == input$q1_field)
    }
    if (length(input$q1_sex) > 0) {
      dat <- dat %>% filter(sex %in% input$q1_sex)
    }
    dat
  })
  output$q1_box_sex <- renderPlot({
    dat <- q1_filtered()
    ggplot(dat, aes(x = sex, y = salary, fill = sex)) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_manual(values = sex_colours) +
      labs(
        x = "Sex",
        y = "Starting salary",
        title = "Starting Salary by Sex"
      )
  })
  output$q1_box_year <- renderPlot({
    dat <- q1_filtered()
    ggplot(dat, aes(x = factor(startyr), y = salary, fill = sex)) +
      geom_boxplot(outlier.alpha = 0.3) +
      scale_fill_manual(values = sex_colours) +
      labs(
        x = "Hire year",
        y = "Starting salary",
        title = "Starting Salary by Hire Year and Sex"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  output$q1_summary_tbl <- renderTable({
    q1_filtered() %>%
      group_by(sex) %>%
      summarise(
        n            = n(),
        mean_salary  = round(mean(salary, na.rm = TRUE), 1),
        median_salary = round(median(salary, na.rm = TRUE), 1),
        sd_salary    = round(sd(salary, na.rm = TRUE), 1),
        .groups = "drop"
      )
  })
  #---------------------------
  # Q2: Promotion raises
  #---------------------------
  q2_filtered <- reactive({
    dat <- promotion_data
    if (length(input$q2_sex) > 0) {
      dat <- dat %>% filter(sex %in% input$q2_sex)
    }
    dat
  })
  output$q2_box <- renderPlot({
    dat <- q2_filtered()
    if (input$q2_metric == "amount") {
      ggplot(dat, aes(x = sex, y = raise_amount, fill = sex)) +
        geom_boxplot(alpha = 0.8) +
        scale_fill_manual(values = sex_colours) +
        labs(
          x = "Sex",
          y = "Dollar raise at promotion",
          title = "Salary Increase at Promotion (Associate → Full) by Sex"
        )
    } else {
      ggplot(dat, aes(x = sex, y = raise_pct, fill = sex)) +
        geom_boxplot(alpha = 0.8) +
        scale_fill_manual(values = sex_colours) +
        scale_y_continuous(labels = percent_format(accuracy = 1)) +
        labs(
          x = "Sex",
          y = "Percent raise at promotion",
          title = "Percent Raise at Promotion by Sex"
        )
    }
  })
  output$q2_summary_tbl <- renderTable({
    dat <- q2_filtered()
    dat %>%
      group_by(sex) %>%
      summarise(
        n              = n(),
        mean_raise     = round(mean(raise_amount, na.rm = TRUE), 1),
        median_raise   = round(median(raise_amount, na.rm = TRUE), 1),
        mean_pct_raise = percent(mean(raise_pct, na.rm = TRUE), accuracy = 0.1),
        median_pct_raise = percent(median(raise_pct, na.rm = TRUE), accuracy = 0.1),
        .groups = "drop"
      )
  })
  #---------------------------
  # Q3: Latest salary
  #---------------------------
  q3_filtered <- reactive({
    dat <- latest_salary
    if (length(input$q3_rank) > 0) {
      dat <- dat %>% filter(rank %in% input$q3_rank)
    }
    if (length(input$q3_sex) > 0) {
      dat <- dat %>% filter(sex %in% input$q3_sex)
    }
    dat
  })
  output$q3_box_rank <- renderPlot({
    dat <- q3_filtered()
    ggplot(dat, aes(x = sex, y = salary, fill = sex)) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_manual(values = sex_colours) +
      facet_wrap(~ rank, scales = "free_y") +
      labs(
        x = "Sex",
        y = "Latest salary",
        title = "Latest Salary by Sex Within Rank"
      )
  })
  output$q3_density_rank <- renderPlot({
    dat <- q3_filtered()
    ggplot(dat, aes(x = salary, fill = sex)) +
      geom_density(alpha = 0.4) +
      scale_fill_manual(values = sex_colours) +
      facet_wrap(~ rank, scales = "free") +
      labs(
        x = "Latest salary",
        y = "Density",
        title = "Latest Salary Density by Sex Within Rank"
      )
  })
  output$q3_summary_tbl <- renderTable({
    q3_filtered() %>%
      group_by(rank, sex) %>%
      summarise(
        n            = n(),
        mean_salary  = round(mean(salary, na.rm = TRUE), 1),
        median_salary = round(median(salary, na.rm = TRUE), 1),
        .groups = "drop"
      ) %>%
      arrange(rank, sex)
  })
}
shinyApp(ui = ui, server = server)