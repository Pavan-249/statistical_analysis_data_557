# Faculty Salary Explorer

An interactive Shiny app built for DATA 557 that explores salary equity across academic disciplines, ranks, and career stages.

## Live App

**[https://pavan249.shinyapps.io/faculty-salary-explorer/](https://pavan249.shinyapps.io/faculty-salary-explorer/)**

## What It Does

The app explores a longitudinal faculty salary dataset from 1976 to 1995 through four core questions about sex-based wage disparities in academia:

| Tab | Question |
|-----|----------|
| **Q1: Starting Salary** | Do men and women differ in starting salary at the time they enter the faculty? |
| **Q2: Promotion Raises** | Do men and women receive different salary increases when promoted from Associate Professor to Full Professor? |
| **Q3: Overall Salary** | In the most recent observed salary for each faculty member, do salary differences by sex remain after accounting for other factors? |
| **Q4: Generalizability** | What limitations of the dataset and analysis affect how broadly these results can be interpreted? |

## Running Locally

```r
# Install dependencies
install.packages(c("shiny", "tidyverse", "scales"))

# Launch
shiny::runApp("app.R")
```

## Course

DATA 557 — University of Washington
