# Test Navigator

A guided Shiny app for choosing, checking, running and reporting a statistical test, step by step:
data → question → look at the data → check each assumption → run the test → report.

Covers parametric tests (t-tests, ANOVA, repeated-measures ANOVA, Pearson), rank-based tests
(Wilcoxon, sign test, Mann–Whitney, Kruskal–Wallis with Dunn, Friedman, Spearman, Kendall) and
tests for counts (binomial, chi-square, Fisher, trend, McNemar), with seven built-in teaching datasets.

MPH (GM51010) Clinical Statistics · Atanu Bhattacharjee, University of Dundee

## Run locally

```r
install.packages(c("shiny", "bslib", "ggplot2"))
shiny::runApp()
```

## Deploy on Posit Connect Cloud

Publish from this repository, choose `app.R` as the primary file; dependencies are in `renv.lock`.
