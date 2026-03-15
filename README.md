# Faculty Salary Explorer

An interactive Shiny app built for DATA 557 that explores salary equity across academic disciplines, ranks, and career stages.

## Live App

**[https://pavan249.shinyapps.io/faculty-salary-explorer/](https://pavan249.shinyapps.io/faculty-salary-explorer/)**

## What It Does

The app analyzes a longitudinal faculty salary dataset spanning over a decade, asking four core questions:

| Tab | Question |
|-----|----------|
| **Q1: Starting Salary** | Do male and female faculty begin their careers at the same pay? |
| **Q2: Promotion Raises** | Is the salary bump at promotion equal for everyone? |
| **Q3: Latest Salary** | How do current salaries compare across ranks and sex? |
| **Q4: Career Arc** | Does the salary gap widen, narrow, or hold steady over a full career? |

## Running Locally

```r
# Install dependencies
install.packages(c("shiny", "tidyverse", "scales"))

# Launch
shiny::runApp("app.R")
```

## Project Documents

All milestone reports are in the `app-inputs/` folder and linked from the app's Overview page.

## Course

DATA 557 — University of Washington
