# =============================================================================
#  Test Navigator — choosing, checking and running statistical tests, step by step
#  MPH (GM51010) Clinical Statistics · Session 9 companion app
#  Author: Atanu Bhattacharjee
#  Packages: shiny, bslib, ggplot2 (all other calculations use base R)
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)

# ---------------------------------------------------------------------------
# Colours (light theme, colour figures)
# ---------------------------------------------------------------------------
COL <- list(navy = "#00365F", blue = "#1E7FB0", sky = "#7FBFE0", amber = "#C98A12",
            green = "#2E8B57", red = "#C0392B", purple = "#7B5EA7", grey = "#63737F")
PAL <- c("#1E7FB0", "#C98A12", "#2E8B57", "#C0392B", "#7B5EA7", "#7FBFE0", "#8C6D46", "#4D4D4D")

theme_app <- function() {
  theme_minimal(base_size = 14) +
    theme(plot.title = element_text(face = "bold", colour = COL$navy, size = 14),
          plot.subtitle = element_text(colour = COL$grey, size = 11),
          panel.grid.minor = element_blank(),
          legend.position = "top", legend.title = element_blank(),
          strip.text = element_text(face = "bold", colour = COL$navy))
}

# ---------------------------------------------------------------------------
# Toy datasets (fixed seeds so every student sees the same numbers)
# ---------------------------------------------------------------------------
make_datasets <- function() {
  out <- list()

  # 1. Community hypertension programme ------------------------------------
  set.seed(PROG_SEED)
  n <- 40
  arm <- factor(rep(c("Usual care", "Intervention"), each = 20), levels = c("Usual care", "Intervention"))
  education <- factor(sample(rep(c("No formal", "Primary", "Secondary", "Tertiary"), c(8, 14, 12, 6))),
                      levels = c("No formal", "Primary", "Secondary", "Tertiary"), ordered = TRUE)
  ed_eff <- c(8, 4, -2, -8)[as.integer(education)]
  sbp0 <- round(152 + ed_eff + rnorm(n, 0, 8))
  drop3 <- ifelse(arm == "Intervention", 8, 3) + rnorm(n, 0, 4)
  drop6 <- ifelse(arm == "Intervention", 13, 4) + rnorm(n, 0, 5)
  sbp3 <- round(sbp0 - drop3); sbp6 <- round(sbp0 - drop6)
  visits <- c(sample(c(1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 6, 12)),
              sample(c(2, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 7, 7, 8, 9, 10, 15)))
  adherence <- pmin(5, pmax(1, round(ifelse(arm == "Intervention", 3.8, 2.9) + rnorm(n, 0, 0.9))))
  controlled <- factor(ifelse(sbp6 < 140, "Yes", "No"), levels = c("No", "Yes"))
  out$programme <- list(
    title = "Community hypertension programme",
    story = "Forty adults with high blood pressure were randomised: 20 to a self-management intervention and 20 to usual care. Blood pressure was measured at the start, at 3 months and at 6 months; clinic visits, adherence and education were recorded.",
    data = data.frame(id = sprintf("P%02d", 1:n), arm = arm, sex = factor(sample(c("Female", "Male"), n, TRUE)),
                      age = round(rnorm(n, 58, 9)), education = education,
                      sbp_baseline = sbp0, sbp_3m = sbp3, sbp_6m = sbp6, visits = visits,
                      adherence = adherence, controlled = controlled),
    types = c(id = "id", arm = "cat", sex = "cat", age = "num", education = "ord", sbp_baseline = "num",
              sbp_3m = "num", sbp_6m = "num", visits = "num", adherence = "ord", controlled = "cat"),
    labels = c(id = "Patient ID", arm = "Arm", sex = "Sex", age = "Age (years)", education = "Highest education",
               sbp_baseline = "Systolic BP at start (mmHg)", sbp_3m = "Systolic BP at 3 months (mmHg)",
               sbp_6m = "Systolic BP at 6 months (mmHg)", visits = "Clinic visits in six months",
               adherence = "Adherence score (1 = poor, 5 = excellent)", controlled = "BP controlled at 6 months (below 140 mmHg)"))

  # 2. Hospital stay on two wards ------------------------------------------
  set.seed(11)
  los_a <- round(rlnorm(14, log(5), 0.45)); los_b <- round(rlnorm(14, log(8), 0.45)); los_b[14] <- 46
  out$stay <- list(
    title = "Length of hospital stay on two wards",
    story = "Days in hospital after hip-fracture surgery for 14 patients on each of two wards. One Ward B patient developed a complication and stayed 46 days.",
    data = data.frame(id = sprintf("H%02d", 1:28), ward = factor(rep(c("Ward A", "Ward B"), each = 14)),
                      stay_days = c(los_a, los_b)),
    types = c(id = "id", ward = "cat", stay_days = "num"),
    labels = c(id = "Patient ID", ward = "Ward", stay_days = "Length of stay (days)"))

  # 3. Pain before and after physiotherapy ---------------------------------
  set.seed(5)
  mu <- 55 + rnorm(16, 0, 14)
  out$pain <- list(
    title = "Pain before and after physiotherapy",
    story = "Sixteen patients with chronic low back pain scored their pain (0–100) before and after six weeks of physiotherapy, and rated their satisfaction (1–5) at both times.",
    data = data.frame(id = sprintf("B%02d", 1:16), pain_before = round(pmin(100, mu + rnorm(16, 0, 5))),
                      pain_after = round(pmax(0, mu - 9 + rnorm(16, 0, 6))),
                      satisfaction_before = sample(1:3, 16, TRUE), satisfaction_after = sample(2:5, 16, TRUE),
                      sex = factor(sample(c("Female", "Male"), 16, TRUE))),
    types = c(id = "id", pain_before = "num", pain_after = "num", satisfaction_before = "ord",
              satisfaction_after = "ord", sex = "cat"),
    labels = c(id = "Patient ID", pain_before = "Pain score before (0–100)", pain_after = "Pain score after (0–100)",
               satisfaction_before = "Satisfaction before (1–5)", satisfaction_after = "Satisfaction after (1–5)", sex = "Sex"))

  # 4. Waiting times at four health centres --------------------------------
  set.seed(21)
  wait <- unlist(lapply(c(10, 14, 18, 24), function(m) round(rexp(9, 1 / m) + 3)))
  wait[9] <- 95
  out$waiting <- list(
    title = "Waiting times at four health centres",
    story = "Days from referral to first physiotherapy appointment for 9 patients at each of four health centres. One patient at Centre 1 waited 95 days.",
    data = data.frame(id = sprintf("W%02d", 1:36), centre = factor(rep(paste("Centre", 1:4), each = 9)),
                      wait_days = wait, rating = sample(1:5, 36, TRUE)),
    types = c(id = "id", centre = "cat", wait_days = "num", rating = "ord"),
    labels = c(id = "Patient ID", centre = "Health centre", wait_days = "Waiting time (days)",
               rating = "Rating of the service (1–5)"))

  # 5. Smoking cessation ---------------------------------------------------
  quit <- c(rep("Yes", 14), rep("No", 22), rep("Yes", 6), rep("No", 26))
  out$smoking <- list(
    title = "Smoking cessation: counselling or leaflet",
    story = "Smokers at a community pharmacy received structured counselling (36 people) or a leaflet (32 people). The outcome is whether they reported quitting at six months.",
    data = data.frame(id = sprintf("S%02d", 1:68), support = factor(rep(c("Counselling", "Leaflet"), c(36, 32))),
                      quit = factor(quit, levels = c("No", "Yes"))),
    types = c(id = "id", support = "cat", quit = "cat"),
    labels = c(id = "Person ID", support = "Type of support", quit = "Quit at six months"))

  # 6. Wheeze in winter and summer (paired yes/no) -------------------------
  w <- c(rep("Yes", 18), rep("Yes", 14), rep("No", 4), rep("No", 24))
  s <- c(rep("Yes", 18), rep("No", 14), rep("Yes", 4), rep("No", 24))
  out$wheeze <- list(
    title = "Wheeze in winter and in summer",
    story = "Sixty children with asthma were asked in winter and again in summer whether they had wheezed in the past week. The same children answered twice.",
    data = data.frame(id = sprintf("C%02d", 1:60), wheeze_winter = factor(w, levels = c("No", "Yes")),
                      wheeze_summer = factor(s, levels = c("No", "Yes"))),
    types = c(id = "id", wheeze_winter = "cat", wheeze_summer = "cat"),
    labels = c(id = "Child ID", wheeze_winter = "Wheeze in winter", wheeze_summer = "Wheeze in summer"))

  # 7. Tiny pilot ----------------------------------------------------------
  out$pilot <- list(
    title = "Tiny pilot: a new wound dressing",
    story = "A pilot study with only 4 patients per group. Every wound under the new dressing healed faster than every wound under the standard dressing.",
    data = data.frame(id = sprintf("D%d", 1:8), dressing = factor(rep(c("New", "Standard"), each = 4)),
                      healing_days = c(9, 10, 12, 13, 15, 17, 18, 21)),
    types = c(id = "id", dressing = "cat", healing_days = "num"),
    labels = c(id = "Patient ID", dressing = "Dressing", healing_days = "Days to complete healing"))
  out
}
PROG_SEED <- 349
DATASETS <- make_datasets()

# Suggested questions for each toy dataset ----------------------------------
EXAMPLES <- list(
  programme = list(
    "Clinic visits: intervention vs usual care (two groups)" = list(q = "two", y = "visits", g = "arm"),
    "Blood pressure at 6 months: intervention vs usual care" = list(q = "two", y = "sbp_6m", g = "arm"),
    "Blood pressure at start vs 6 months (same people)" = list(q = "paired", a = "sbp_baseline", b = "sbp_6m"),
    "Blood pressure at start, 3 and 6 months" = list(q = "repeated", reps = c("sbp_baseline", "sbp_3m", "sbp_6m")),
    "Blood pressure at start by education (four groups)" = list(q = "k", y = "sbp_baseline", g = "education"),
    "Adherence score by arm (ordered score)" = list(q = "two", y = "adherence", g = "arm"),
    "BP controlled (yes/no) by arm" = list(q = "two", y = "controlled", g = "arm"),
    "BP controlled (yes/no) by education (ordered groups)" = list(q = "k", y = "controlled", g = "education"),
    "Age vs blood pressure at start (relationship)" = list(q = "corr", cx = "age", cy = "sbp_baseline"),
    "Mean blood pressure at start against 140 mmHg" = list(q = "one", y = "sbp_baseline", mu = 140)),
  stay = list("Length of stay: Ward A vs Ward B" = list(q = "two", y = "stay_days", g = "ward"),
              "Median stay against 7 days" = list(q = "one", y = "stay_days", mu = 7)),
  pain = list("Pain before vs after (same people)" = list(q = "paired", a = "pain_before", b = "pain_after"),
              "Satisfaction before vs after (ordered score)" = list(q = "paired", a = "satisfaction_before", b = "satisfaction_after"),
              "Pain after physiotherapy by sex" = list(q = "two", y = "pain_after", g = "sex")),
  waiting = list("Waiting time across four centres" = list(q = "k", y = "wait_days", g = "centre"),
                 "Service rating across four centres" = list(q = "k", y = "rating", g = "centre")),
  smoking = list("Quitting by type of support" = list(q = "two", y = "quit", g = "support"),
                 "Quit rate against 20%" = list(q = "one", y = "quit", p0 = 0.2)),
  wheeze = list("Wheeze in winter vs summer (same children)" = list(q = "paired", a = "wheeze_winter", b = "wheeze_summer")),
  pilot = list("Healing time: new vs standard dressing" = list(q = "two", y = "healing_days", g = "dressing"))
)

QUESTIONS <- c(
  "Compare one group with a fixed value" = "one",
  "Compare two separate groups" = "two",
  "Compare the same people at two times (paired)" = "paired",
  "Compare three or more separate groups" = "k",
  "Compare the same people at three or more times" = "repeated",
  "Relationship between two numeric variables" = "corr")
TYPES <- c("A measurement or count (e.g. mmHg, days, visits)" = "num",
           "An ordered score (e.g. 1–5, none/mild/severe)" = "ord",
           "A category (e.g. yes/no, ward)" = "cat")

# ---------------------------------------------------------------------------
# Small helpers
# ---------------------------------------------------------------------------
fmt_p <- function(p) {
  if (is.null(p) || is.na(p)) return("not available")
  if (p < 0.0001) return("< 0.0001")
  if (p < 0.001) return(formatC(p, format = "f", digits = 4))
  formatC(p, format = "f", digits = 3)
}
f2 <- function(x, d = 2) ifelse(is.na(x), "–", formatC(x, format = "f", digits = d))
skewness <- function(x) { x <- x[!is.na(x)]; n <- length(x); if (n < 3 || sd(x) == 0) return(NA); (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^1.5 }
kurt_ex <- function(x) { x <- x[!is.na(x)]; n <- length(x); if (n < 4 || sd(x) == 0) return(NA); (sum((x - mean(x))^4) / n) / (sum((x - mean(x))^2) / n)^2 - 3 }
n_outliers <- function(x) { q <- quantile(x, c(.25, .75), na.rm = TRUE); i <- diff(q); sum(x < q[1] - 1.5 * i | x > q[2] + 1.5 * i, na.rm = TRUE) }
sw_test <- function(x) { x <- x[!is.na(x)]; if (length(x) < 3 || length(x) > 5000 || length(unique(x)) < 3) return(NA); tryCatch(shapiro.test(x)$p.value, error = function(e) NA) }
size_word <- function(v, cuts, words = c("very small", "small", "medium", "large")) {
  if (is.na(v)) return("not available"); words[findInterval(abs(v), cuts) + 1] }
lc1 <- function(s) if (grepl("^[A-Z][a-z]", s)) paste0(tolower(substr(s, 1, 1)), substring(s, 2)) else s
cap1 <- function(s) paste0(toupper(substr(s, 1, 1)), substring(s, 2))
z_from_p <- function(p) qnorm(1 - min(max(p, 1e-300), 1) / 2)

# A check (assumption) record
chk <- function(step, name, what, result, status, why) list(step = step, name = name, what = what, result = result, status = status, why = why)

# ---------------------------------------------------------------------------
# Build the analysis frame from a specification
# spec: q, type, y, g, a, b, reps, cx, cy, mu, p0
# ---------------------------------------------------------------------------
build_frame <- function(d, spec) {
  q <- spec$q; tp <- spec$type
  conv <- function(v) if (tp == "cat") droplevels(factor(v)) else suppressWarnings(as.numeric(v))
  if (q == "one") {
    y <- conv(d[[spec$y]]); y <- y[!is.na(y)]
    return(list(y = y))
  }
  if (q %in% c("two", "k")) {
    df <- data.frame(y = conv(d[[spec$y]]), g = droplevels(factor(d[[spec$g]])))
    df <- df[complete.cases(df), ]
    df$g <- droplevels(df$g)
    if (tp == "cat") df$y <- droplevels(df$y)
    return(df)
  }
  if (q == "paired") {
    df <- data.frame(a = conv(d[[spec$a]]), b = conv(d[[spec$b]]))
    df <- df[complete.cases(df), ]
    if (tp == "cat") { lv <- union(levels(factor(df$a)), levels(factor(df$b))); df$a <- factor(df$a, lv); df$b <- factor(df$b, lv) }
    return(df)
  }
  if (q == "repeated") {
    m <- sapply(spec$reps, function(v) suppressWarnings(as.numeric(d[[v]])))
    m <- m[complete.cases(m), , drop = FALSE]
    if (!is.null(spec$replabs)) colnames(m) <- spec$replabs
    return(list(m = m, id = seq_len(nrow(m))))
  }
  if (q == "corr") {
    df <- data.frame(x = suppressWarnings(as.numeric(d[[spec$cx]])), y = suppressWarnings(as.numeric(d[[spec$cy]])))
    return(df[complete.cases(df), ])
  }
}

# Problems that stop the analysis (returned as text, or NULL when fine)
frame_problem <- function(fr, spec) {
  q <- spec$q; tp <- spec$type
  if (q == "one") {
    if (length(fr$y) < 3) return("Fewer than 3 values: choose another variable.")
    if (tp == "cat" && nlevels(fr$y) < 2) return("This variable has only one category.")
    if (tp != "cat" && all(is.na(fr$y))) return("This variable is not numeric. Change the outcome type to 'A category'.")
  }
  if (q %in% c("two", "k")) {
    if (nrow(fr) < 4) return("Too few complete rows.")
    k <- nlevels(fr$g)
    if (q == "two" && k != 2) return(sprintf("The grouping variable has %d groups. Choose 'Compare three or more separate groups', or another grouping variable.", k))
    if (q == "k" && k < 3) return("The grouping variable has only two groups. Choose 'Compare two separate groups'.")
    if (any(table(fr$g) < 2)) return("Every group needs at least 2 people.")
    if (tp != "cat" && all(is.na(fr$y))) return("The outcome is not numeric. Change the outcome type to 'A category'.")
    if (tp == "cat" && nlevels(fr$y) < 2) return("The outcome has only one category.")
  }
  if (q == "paired") { if (nrow(fr) < 3) return("Fewer than 3 complete pairs."); if (tp == "cat" && nlevels(fr$a) != 2) return("For yes/no outcomes measured twice, both variables need exactly two categories.") }
  if (q == "repeated") { if (ncol(fr$m) < 3) return("Choose three or more time points."); if (nrow(fr$m) < 3) return("Fewer than 3 people with all time points.") }
  if (q == "corr") { if (nrow(fr) < 4) return("Fewer than 4 complete pairs of values.") }
  NULL
}

# ---------------------------------------------------------------------------
# Candidate tests for each question and outcome type
# ---------------------------------------------------------------------------
TEST_INFO <- list(
  t1 = list(name = "One-sample t-test", kind = "Parametric"),
  w1 = list(name = "Wilcoxon signed-rank test (one sample)", kind = "Non-parametric"),
  sign1 = list(name = "Sign test (one sample)", kind = "Non-parametric"),
  binom = list(name = "Binomial test", kind = "Categorical"),
  gof = list(name = "Chi-square goodness-of-fit test", kind = "Categorical"),
  t2 = list(name = "Independent t-test (Student, equal variances)", kind = "Parametric"),
  welch = list(name = "Welch t-test (unequal variances)", kind = "Parametric"),
  mw = list(name = "Mann–Whitney U test", kind = "Non-parametric"),
  tp = list(name = "Paired t-test", kind = "Parametric"),
  wp = list(name = "Wilcoxon signed-rank test (paired)", kind = "Non-parametric"),
  signp = list(name = "Sign test (paired)", kind = "Non-parametric"),
  mcn = list(name = "McNemar test", kind = "Categorical"),
  mcnx = list(name = "Exact McNemar test", kind = "Categorical"),
  aov = list(name = "One-way ANOVA", kind = "Parametric"),
  waov = list(name = "Welch ANOVA (unequal variances)", kind = "Parametric"),
  kw = list(name = "Kruskal–Wallis test", kind = "Non-parametric"),
  rm = list(name = "Repeated-measures ANOVA", kind = "Parametric"),
  fr = list(name = "Friedman test", kind = "Non-parametric"),
  chi = list(name = "Chi-square test of association", kind = "Categorical"),
  yates = list(name = "Chi-square test with Yates correction", kind = "Categorical"),
  fisher = list(name = "Fisher's exact test", kind = "Categorical"),
  trend = list(name = "Chi-square test for trend", kind = "Categorical"),
  pear = list(name = "Pearson correlation", kind = "Parametric"),
  spear = list(name = "Spearman rank correlation", kind = "Non-parametric"),
  kend = list(name = "Kendall's tau", kind = "Non-parametric"))

candidates <- function(spec, fr = NULL) {
  q <- spec$q; tp <- spec$type
  switch(q,
    one = switch(tp, num = c("t1", "w1", "sign1"), ord = c("w1", "sign1"),
                 cat = if (!is.null(fr) && nlevels(fr$y) == 2) c("binom", "gof") else "gof"),
    two = switch(tp, num = c("t2", "welch", "mw"), ord = "mw", cat = c("chi", "fisher", "yates")),
    paired = switch(tp, num = c("tp", "wp", "signp"), ord = c("wp", "signp"), cat = c("mcn", "mcnx")),
    k = switch(tp, num = c("aov", "waov", "kw"), ord = "kw",
               cat = { o <- c("chi", "fisher"); if (!is.null(fr) && nlevels(fr$y) == 2) o <- c(o, "trend"); o }),
    repeated = switch(tp, num = c("rm", "fr"), ord = "fr", cat = character(0)),
    corr = switch(tp, num = c("pear", "spear", "kend"), ord = c("spear", "kend"), cat = character(0)))
}

# ---------------------------------------------------------------------------
# Assumption checks, step by step, and the recommended test
# ---------------------------------------------------------------------------
normality_check <- function(x, label, step) {
  n <- length(x); p <- sw_test(x); sk <- skewness(x)
  if (is.na(p)) {
    return(chk(step, paste("Shape:", label), "Is the shape roughly a bell (normal)?",
               sprintf("n = %d; too few distinct values for a Shapiro–Wilk test.", n), "warn",
               "Too few or too many tied values to judge the shape. Look at the Q–Q plot; a rank-based test is the safer choice."))
  }
  ok <- p >= 0.05 && (is.na(sk) || abs(sk) < 1)
  clt <- n >= 30 && !is.na(sk) && abs(sk) < 2
  status <- if (ok) "ok" else if (clt) "warn" else "fail"
  why <- if (ok) "The points sit close to the line on the Q–Q plot and the test finds no clear departure from a bell shape."
         else if (clt) sprintf("Some departure from a bell shape, but with %d values the average itself is close to bell-shaped (central limit theorem), so a t-test or ANOVA is usually still acceptable.", n)
         else "Clear departure from a bell shape (skew or outliers) with a modest sample size. A rank-based test is the safer choice."
  chk(step, paste("Shape:", label), "Is the shape roughly a bell (normal)?",
      sprintf("n = %d; Shapiro–Wilk p = %s; skewness = %s", n, fmt_p(p), f2(sk)), status, why)
}

outlier_check <- function(x, label, step) {
  k <- n_outliers(x)
  chk(step, paste("Outliers:", label), "Are there values far from the rest?",
      sprintf("%d value(s) beyond 1.5 × IQR from the quartiles", k), if (k == 0) "ok" else "warn",
      if (k == 0) "No unusual values." else "Check these values are real (not data-entry errors). Real extreme values pull the mean and SD; rank-based tests are not affected by how extreme they are.")
}

assess <- function(fr, spec) {
  q <- spec$q; tp <- spec$type; ch <- list(); rec <- NULL; reason <- ""
  add <- function(x) ch[[length(ch) + 1]] <<- x
  st <- 1
  # Step 1: design / independence
  design_txt <- switch(q,
    one = "One group of different people, each measured once.",
    two = "Two groups of different people; nobody is in both groups.",
    paired = "The same people measured twice (or matched pairs): the two values belong together.",
    k = "Three or more groups of different people; nobody is in more than one group.",
    repeated = "The same people measured at three or more times.",
    corr = "Two values measured on each person.")
  add(chk(st, "Design and independence", "Are the observations independent in the way the test assumes?", design_txt, "info",
          "This is decided by how the study was done, not by the data. If people are clustered (e.g. several patients per GP practice), these simple tests are not enough.")); st <- st + 1
  # Step 2: type of outcome
  type_txt <- c(num = "A measurement or count: differences between values have meaning.",
                ord = "An ordered score: the order has meaning but the gaps between scores may not be equal.",
                cat = "A category: people are counted in each category.")[tp]
  add(chk(st, "Kind of outcome", "What kind of values does the outcome take?", type_txt, "info",
          switch(tp, num = "Means and t-tests are possible if the shape allows.", ord = "Use rank-based tests and medians; an average of scores is hard to interpret.",
                 cat = "Use tests for counts in a table: chi-square, Fisher, McNemar or binomial."))); st <- st + 1

  if (tp == "cat") {
    if (q == "one") {
      tab <- table(fr$y); n <- sum(tab)
      if (length(tab) == 2) { rec <- "binom"; reason <- "A yes/no outcome compared with a fixed proportion: the binomial test gives an exact p-value." }
      else {
        E <- n / length(tab)
        add(chk(st, "Expected counts", "Is every expected count at least 5?", sprintf("Expected count in each category = %s", f2(E, 1)), if (E >= 5) "ok" else "fail",
                if (E >= 5) "Large enough for the chi-square approximation." else "Too small: combine categories or collect more data.")); st <- st + 1
        rec <- "gof"; reason <- "Several categories compared with equal proportions."
      }
      add(chk(st, "Sample size", "How many people?", sprintf("n = %d", n), "info", "Exact tests work at any size, but small samples can only detect large differences."))
    }
    if (q %in% c("two", "k")) {
      tab <- table(fr$g, fr$y)
      E <- suppressWarnings(chisq.test(tab, correct = FALSE)$expected)
      low <- mean(E < 5); mn <- min(E)
      ok <- low <= 0.2 && mn >= 1
      add(chk(st, "Expected counts", "Expected count = row total × column total ÷ grand total. Are they large enough?",
              sprintf("Smallest expected count = %s; %d of %d cells (%s%%) below 5", f2(mn, 1), sum(E < 5), length(E), f2(100 * low, 0)),
              if (ok) "ok" else "fail",
              if (ok) "No more than 1 cell in 5 below 5 and none below 1: the chi-square test is reliable." else "Too many small expected counts: the chi-square approximation is unreliable. Use Fisher's exact test.")); st <- st + 1
      add(chk(st, "Each person counted once", "Is every person in exactly one cell?", sprintf("Table total = %d people", sum(tab)), "info",
              "Counts must be people, not percentages, and nobody may appear twice. For the same people measured twice, use McNemar.")); st <- st + 1
      rec <- if (ok) "chi" else "fisher"
      reason <- if (ok) "Counts in a table with large enough expected counts." else "Some expected counts are below 5, so the probability is computed exactly."
      if (q == "k" && nlevels(fr$y) == 2 && isTRUE(spec$ordered_groups)) {
        add(chk(st, "Ordered groups", "Are the groups in a natural order?", "Yes: the groups are ordered and the outcome is yes/no.", "info",
                "The chi-square test for trend asks whether the proportion rises or falls steadily across the ordered groups; it is more powerful than the ordinary chi-square for this question."))
      }
    }
    if (q == "paired") {
      tab <- table(fr$a, fr$b); b <- tab[1, 2]; c <- tab[2, 1]
      add(chk(st, "Discordant pairs", "How many people changed category? Only they carry information.",
              sprintf("Changed one way: %d; changed the other way: %d; total %d", b, c, b + c), if (b + c >= 25) "ok" else "warn",
              if (b + c >= 25) "Enough changes for the ordinary McNemar test." else "Fewer than 25 changes: use the exact McNemar test.")); st <- st + 1
      rec <- if (b + c >= 25) "mcn" else "mcnx"; reason <- "The same people measured twice on a yes/no outcome."
    }
    return(list(checks = ch, rec = rec, reason = reason))
  }

  if (q %in% c("repeated", "corr") && tp == "cat") return(list(checks = ch, rec = NULL, reason = "Not available for categories."))

  if (q == "one") {
    x <- fr$y
    if (tp == "num") {
      nc <- normality_check(x, "the values", st); add(nc); st <- st + 1
      add(outlier_check(x, "the values", st)); st <- st + 1
      rec <- if (nc$status != "fail") "t1" else "w1"
      reason <- if (rec == "t1") "The values are roughly bell-shaped (or the sample is large enough)." else "The values are not bell-shaped and the sample is modest."
    } else { rec <- "w1"; reason <- "An ordered score: use ranks." }
    add(chk(st, "Symmetry for Wilcoxon", "For the Wilcoxon test: are the values roughly symmetric around their median?",
            sprintf("Skewness = %s", f2(skewness(x))), if (is.na(skewness(x)) || abs(skewness(x)) < 1) "ok" else "warn",
            "Wilcoxon assumes a symmetric shape around the median. If the shape is strongly lopsided, the sign test makes no shape assumption at all."))
  }

  if (q %in% c("two", "k")) {
    gs <- split(fr$y, fr$g); ns <- sapply(gs, length)
    if (tp == "num") {
      allok <- TRUE; anyfail <- FALSE
      for (nm in names(gs)) { nc <- normality_check(gs[[nm]], nm, st); add(nc); if (nc$status == "fail") anyfail <- TRUE }
      st <- st + 1
      for (nm in names(gs)) add(outlier_check(gs[[nm]], nm, st))
      st <- st + 1
      sds <- sapply(gs, sd); ratio <- max(sds) / max(min(sds), 1e-9)
      med <- ave(fr$y, fr$g, FUN = median)
      lev <- tryCatch(anova(lm(abs(fr$y - med) ~ fr$g))$`Pr(>F)`[1], error = function(e) NA)
      eqok <- (is.na(lev) || lev >= 0.05) && ratio < 2
      add(chk(st, "Equal spread", "Do the groups have similar spread (standard deviation)?",
              sprintf("SDs: %s; largest ÷ smallest = %s; Levene (Brown–Forsythe) p = %s",
                      paste(sprintf("%s %s", names(sds), f2(sds, 1)), collapse = ", "), f2(ratio), fmt_p(lev)),
              if (eqok) "ok" else "warn",
              if (eqok) "Spreads are similar: the standard t-test or ANOVA is fine." else "Spreads differ: use the Welch version, which does not assume equal spread.")); st <- st + 1
      add(chk(st, "Group sizes", "How many people in each group?", paste(sprintf("%s: %d", names(ns), ns), collapse = "; "),
              if (min(ns) >= 15) "ok" else "warn",
              if (min(ns) >= 15) "Reasonable group sizes." else "Small groups: the shape is hard to judge and both kinds of test have limited power.")); st <- st + 1
      if (!anyfail) {
        rec <- if (q == "two") (if (eqok) "t2" else "welch") else (if (eqok) "aov" else "waov")
        reason <- paste("Each group is roughly bell-shaped (or large enough);", if (eqok) "spreads are similar." else "spreads differ, so the Welch version is used.")
      } else {
        rec <- if (q == "two") "mw" else "kw"
        reason <- "At least one group is clearly not bell-shaped with a modest sample size, so a rank-based test is used."
      }
    } else {
      rec <- if (q == "two") "mw" else "kw"; reason <- "An ordered score: use ranks."
      add(chk(st, "Group sizes", "How many people in each group?", paste(sprintf("%s: %d", names(ns), ns), collapse = "; "), "info", "")); st <- st + 1
    }
    iqrs <- sapply(gs, IQR); sks <- sapply(gs, skewness)
    add(chk(st, "Similar shape for the rank test", "For reading a rank test as a difference in medians: do the groups have a similar shape?",
            sprintf("IQRs: %s; skewness: %s", paste(f2(iqrs, 1), collapse = ", "), paste(f2(sks), collapse = ", ")), "info",
            "If shapes are similar, a significant rank test means the medians differ. If not, it means one group tends to have higher values.")); st <- st + 1
    if (q == "two" && min(ns) <= 6) { mp <- 2 / choose(sum(ns), ns[1])
      add(chk(st, "Smallest possible p for the rank test", "With very small groups, can the rank test reach 0.05 at all?",
              sprintf("With %d and %d per group the smallest exact Mann–Whitney p is %s", ns[1], ns[2], fmt_p(mp)), if (mp > 0.05) "fail" else "warn",
              if (mp > 0.05) "Even perfect separation cannot give p below 0.05: the study is too small for a rank test." else "The rank test can only just reach small p-values; even perfect separation gives this p and no smaller.")) }
  }

  if (q == "paired") {
    d <- fr$b - fr$a
    if (tp == "num") {
      nc <- normality_check(d, "the differences", st); add(nc); st <- st + 1
      add(outlier_check(d, "the differences", st)); st <- st + 1
      rec <- if (nc$status != "fail") "tp" else "wp"
      reason <- if (rec == "tp") "The within-person differences are roughly bell-shaped." else "The within-person differences are not bell-shaped."
    } else { rec <- "wp"; reason <- "An ordered score: use ranks of the differences." }
    nz <- sum(d != 0)
    add(chk(st, "Number of non-zero differences", "How many people changed?", sprintf("%d of %d pairs changed; %d unchanged", nz, length(d), length(d) - nz),
            if (nz >= 6) "ok" else "fail",
            if (nz >= 6) "Enough pairs." else "With fewer than 6 changing pairs the Wilcoxon test cannot reach p < 0.05.")); st <- st + 1
    add(chk(st, "Symmetry of the differences", "For Wilcoxon: are the differences roughly symmetric?", sprintf("Skewness of differences = %s", f2(skewness(d))),
            if (is.na(skewness(d)) || abs(skewness(d)) < 1) "ok" else "warn", "If the differences are strongly lopsided, the sign test is the assumption-free choice."))
  }

  if (q == "repeated") {
    m <- fr$m
    if (tp == "num") {
      res <- as.vector(m - rowMeans(m) - matrix(colMeans(m), nrow(m), ncol(m), byrow = TRUE) + mean(m))
      nc <- normality_check(res, "the residuals", st); add(nc); st <- st + 1
      sph <- sphericity(m)
      add(chk(st, "Sphericity", "Are the variances of all the differences between time points similar?",
              sprintf("Mauchly p = %s; Greenhouse–Geisser epsilon = %s", fmt_p(sph$p), f2(sph$gg)),
              if (is.na(sph$p) || sph$p >= 0.05) "ok" else "warn",
              if (is.na(sph$p) || sph$p >= 0.05) "No evidence against sphericity." else "Sphericity is doubtful: the Greenhouse–Geisser corrected p-value is reported.")); st <- st + 1
      rec <- if (nc$status != "fail") "rm" else "fr"
      reason <- if (rec == "rm") "Residuals are roughly bell-shaped." else "Residuals are not bell-shaped."
    } else { rec <- "fr"; reason <- "An ordered score measured repeatedly: use ranks within each person." }
    add(chk(st, "Complete data", "Does everyone have every time point?", sprintf("%d people with all %d time points", nrow(m), ncol(m)), "info",
            "People with any missing time point are left out of these tests."))
  }

  if (q == "corr") {
    if (tp == "num") {
      n1 <- normality_check(fr$x, "first variable", st); add(n1)
      n2 <- normality_check(fr$y, "second variable", st); add(n2); st <- st + 1
      ox <- n_outliers(fr$x) + n_outliers(fr$y)
      add(chk(st, "Outliers", "Are there points far from the rest?", sprintf("%d value(s) beyond 1.5 × IQR", ox), if (ox == 0) "ok" else "warn",
              "A single extreme point can create or hide a Pearson correlation.")); st <- st + 1
      add(chk(st, "Straight-line pattern", "Does the scatter plot follow a roughly straight line?", "Judge from the scatter plot with its smooth curve.", "info",
              "Pearson measures straight-line association; Spearman measures any steadily rising or falling pattern.")); st <- st + 1
      rec <- if (n1$status != "fail" && n2$status != "fail" && ox == 0) "pear" else "spear"
      reason <- if (rec == "pear") "Both variables roughly bell-shaped with no outliers." else "Shape or outliers argue for the rank-based correlation."
    } else { rec <- "spear"; reason <- "Ordered scores: use rank correlation." }
    add(chk(st, "Number of pairs", "How many people?", sprintf("n = %d", nrow(fr)), if (nrow(fr) >= 10) "ok" else "warn", "Correlations from fewer than 10 people are very imprecise."))
  }
  list(checks = ch, rec = rec, reason = reason)
}

sphericity <- function(m) {
  k <- ncol(m)
  C <- contr.poly(k)
  Y <- m %*% C
  S <- cov(Y)
  ev <- eigen(S, only.values = TRUE)$values
  gg <- sum(ev)^2 / ((k - 1) * sum(ev^2))
  p <- tryCatch(mauchly.test(lm(m ~ 1), X = ~1)$p.value, error = function(e) NA)
  list(p = p, gg = gg)
}

# ---------------------------------------------------------------------------
# Running each test: returns a common structure
# ---------------------------------------------------------------------------
res <- function(test, h0, h1, stat_lab, stat, df = NA, p, est_lab = NULL, est = NULL, ci = NULL,
                eff_lab = NULL, eff = NULL, eff_word = NULL, formula = "", rcode = "", sentence = "", extra = NULL, note = NULL)
  list(test = test, h0 = h0, h1 = h1, stat_lab = stat_lab, stat = stat, df = df, p = p, est_lab = est_lab, est = est, ci = ci,
       eff_lab = eff_lab, eff = eff, eff_word = eff_word, formula = formula, rcode = rcode, sentence = sentence, extra = extra, note = note)

dunn_test <- function(y, g) {
  r <- rank(y); N <- length(y); ties <- table(y); T <- sum(ties^3 - ties)
  lv <- levels(g); mr <- tapply(r, g, mean); n <- table(g)
  out <- NULL
  for (i in 1:(length(lv) - 1)) for (j in (i + 1):length(lv)) {
    se <- sqrt((N * (N + 1) / 12 - T / (12 * (N - 1))) * (1 / n[i] + 1 / n[j]))
    z <- (mr[i] - mr[j]) / se
    out <- rbind(out, data.frame(Comparison = paste(lv[i], "vs", lv[j]), `Mean rank difference` = round(mr[i] - mr[j], 2),
                                 z = round(z, 2), p = 2 * pnorm(-abs(z)), check.names = FALSE))
  }
  out$`p (Holm adjusted)` <- p.adjust(out$p, "holm")
  out$p <- sapply(out$p, fmt_p); out$`p (Holm adjusted)` <- sapply(out$`p (Holm adjusted)`, fmt_p)
  out
}

run_test <- function(code, fr, spec, d = NULL) {
  lab <- lc1(spec$ylab %||% "the outcome"); glab <- lc1(spec$glab %||% "group")
  yv <- spec$y; gv <- spec$g
  switch(code,
  # ----- one sample -----
  t1 = {
    x <- fr$y; mu <- spec$mu; tt <- t.test(x, mu = mu); dd <- (mean(x) - mu) / sd(x)
    res(TEST_INFO$t1$name, sprintf("The population mean equals %s.", mu), sprintf("The population mean differs from %s.", mu),
        "t", tt$statistic, tt$parameter, tt$p.value, "Mean", mean(x), tt$conf.int, "Cohen's d", dd, size_word(dd, c(.2, .5, .8)),
        "t = (mean − μ₀) ÷ (SD ÷ √n)", sprintf("t.test(d$%s, mu = %s)", yv, mu),
        sprintf("The mean %s was %s (95%% CI %s to %s; SD %s; n = %d), compared with %s: one-sample t = %s, df = %d, p = %s; Cohen's d = %s.",
                lab, f2(mean(x), 1), f2(tt$conf.int[1], 1), f2(tt$conf.int[2], 1), f2(sd(x), 1), length(x), mu, f2(tt$statistic), as.integer(tt$parameter), fmt_p(tt$p.value), f2(dd)))
  },
  w1 = {
    x <- fr$y; mu <- spec$mu
    wt <- suppressWarnings(wilcox.test(x, mu = mu, conf.int = TRUE, exact = !any(duplicated(x - mu)) && length(x) < 50))
    r <- z_from_p(wt$p.value) / sqrt(sum(x != mu))
    res(TEST_INFO$w1$name, sprintf("Values are symmetric around %s.", mu), sprintf("Values are centred away from %s.", mu),
        "V (sum of ranks above μ₀)", wt$statistic, NA, wt$p.value, "Hodges–Lehmann estimate (pseudo-median)", wt$estimate, wt$conf.int,
        "r", r, size_word(r, c(.1, .3, .5)), "Rank |value − μ₀|, sum ranks of values above μ₀",
        sprintf("wilcox.test(d$%s, mu = %s, conf.int = TRUE)", yv, mu),
        sprintf("The median %s was %s (IQR %s to %s; n = %d), compared with %s: Wilcoxon signed-rank V = %s, p = %s, r = %s.",
                lab, f2(median(x), 1), f2(quantile(x, .25), 1), f2(quantile(x, .75), 1), length(x), mu, f2(wt$statistic, 1), fmt_p(wt$p.value), f2(r)))
  },
  sign1 = {
    x <- fr$y; mu <- spec$mu; k <- sum(x > mu); n <- sum(x != mu); bt <- binom.test(k, n)
    res(TEST_INFO$sign1$name, sprintf("Half the values lie above %s.", mu), sprintf("More or fewer than half lie above %s.", mu),
        "Number above μ₀", k, NA, bt$p.value, "Proportion above μ₀", k / n, bt$conf.int, NULL, NULL, NULL,
        "Count values above μ₀; compare with a binomial(n, 0.5)", sprintf("binom.test(sum(d$%s > %s), sum(d$%s != %s))", yv, mu, yv, mu),
        sprintf("%d of %d values of %s were above %s (median %s): sign test p = %s.", k, n, lab, mu, f2(median(x), 1), fmt_p(bt$p.value)))
  },
  binom = {
    tab <- table(fr$y); lv <- names(tab); succ <- lv[length(lv)]; k <- tab[succ]; n <- sum(tab); p0 <- spec$p0
    bt <- binom.test(k, n, p = p0)
    res(TEST_INFO$binom$name, sprintf("The true proportion '%s' is %s.", succ, p0), sprintf("The true proportion '%s' differs from %s.", succ, p0),
        "Number of successes", k, NA, bt$p.value, sprintf("Proportion '%s'", succ), k / n, bt$conf.int, "Difference from p₀", k / n - p0, NULL,
        "Exact probability from the binomial distribution", sprintf("binom.test(%d, %d, p = %s)", k, n, p0),
        sprintf("%d of %d (%s%%, 95%% CI %s%% to %s%%) were '%s', compared with %s%%: exact binomial p = %s.",
                k, n, f2(100 * k / n, 1), f2(100 * bt$conf.int[1], 1), f2(100 * bt$conf.int[2], 1), succ, f2(100 * p0, 0), fmt_p(bt$p.value)))
  },
  gof = {
    tab <- table(fr$y); ct <- chisq.test(tab); w <- sqrt(ct$statistic / sum(tab))
    res(TEST_INFO$gof$name, "All categories are equally common.", "Some categories are more common than others.",
        "χ²", ct$statistic, ct$parameter, ct$p.value, NULL, NULL, NULL, "Cohen's w", w, size_word(w, c(.1, .3, .5)),
        "χ² = Σ (O − E)² ÷ E with E = n ÷ number of categories", sprintf("chisq.test(table(d$%s))", yv),
        sprintf("Counts across categories of %s (%s) differed from equal proportions: χ² = %s, df = %d, p = %s.",
                lab, paste(sprintf("%s %d", names(tab), tab), collapse = ", "), f2(ct$statistic), as.integer(ct$parameter), fmt_p(ct$p.value)))
  },
  # ----- two groups -----
  t2 = , welch = {
    eq <- code == "t2"; tt <- t.test(y ~ g, data = fr, var.equal = eq); gs <- split(fr$y, fr$g); lv <- names(gs)
    m <- sapply(gs, mean); s <- sapply(gs, sd); n <- sapply(gs, length)
    sp <- if (eq) sqrt(((n[1] - 1) * s[1]^2 + (n[2] - 1) * s[2]^2) / (sum(n) - 2)) else sqrt((s[1]^2 + s[2]^2) / 2)
    dd <- (m[2] - m[1]) / sp
    res(TEST_INFO[[code]]$name, "The two population means are equal.", "The two population means differ.",
        "t", tt$statistic, tt$parameter, tt$p.value, sprintf("Difference in means (%s − %s)", lv[2], lv[1]), m[2] - m[1], -rev(tt$conf.int),
        "Cohen's d", dd, size_word(dd, c(.2, .5, .8)),
        if (eq) "t = (mean₂ − mean₁) ÷ (pooled SD × √(1/n₁ + 1/n₂))" else "t = (mean₂ − mean₁) ÷ √(SD₁²/n₁ + SD₂²/n₂)",
        sprintf("t.test(%s ~ %s, data = d, var.equal = %s)", yv, gv, eq),
        sprintf("Mean %s was %s (SD %s) in %s and %s (SD %s) in %s; difference %s (95%% CI %s to %s); %s t = %s, df = %s, p = %s; Cohen's d = %s; n = %d and %d.",
                lab, f2(m[1], 1), f2(s[1], 1), lv[1], f2(m[2], 1), f2(s[2], 1), lv[2], f2(m[2] - m[1], 1), f2(-tt$conf.int[2], 1), f2(-tt$conf.int[1], 1),
                if (eq) "independent" else "Welch", f2(tt$statistic), f2(tt$parameter, 1), fmt_p(tt$p.value), f2(dd), n[1], n[2]))
  },
  mw = {
    gs <- split(fr$y, fr$g); lv <- names(gs); n <- sapply(gs, length)
    exact <- !any(duplicated(fr$y)) && sum(n) < 50
    wt <- suppressWarnings(wilcox.test(gs[[2]], gs[[1]], conf.int = TRUE, exact = exact))
    U <- min(wt$statistic, prod(n) - wt$statistic)
    r <- z_from_p(wt$p.value) / sqrt(sum(n)); rb <- 2 * wt$statistic / prod(n) - 1
    rk <- rank(fr$y); mr <- tapply(rk, fr$g, mean)
    md <- sapply(gs, median); q1 <- sapply(gs, quantile, .25); q3 <- sapply(gs, quantile, .75)
    ex <- data.frame(Group = lv, n = as.integer(n), Median = f2(md, 1), `Quartiles` = paste(f2(q1, 1), "to", f2(q3, 1)), `Mean rank` = f2(mr, 2), check.names = FALSE)
    res(TEST_INFO$mw$name, "Values in the two groups come from the same distribution.", "Values in one group tend to be higher than in the other.",
        "U", U, NA, wt$p.value, sprintf("Hodges–Lehmann shift (%s − %s)", lv[2], lv[1]), wt$estimate, wt$conf.int, "r", r, size_word(r, c(.1, .3, .5)),
        "Rank all values together; U = R − n(n + 1)/2 for each group; take the smaller",
        sprintf("wilcox.test(%s ~ %s, data = d, conf.int = TRUE)", yv, gv),
        sprintf("%s was higher in %s (median %s, quartiles %s to %s) than in %s (median %s, quartiles %s to %s); Mann–Whitney U = %s, p = %s, r = %s; n = %d and %d.",
                cap1(lab), if (md[2] >= md[1]) lv[2] else lv[1], f2(max(md), 1), f2(q1[which.max(md)], 1), f2(q3[which.max(md)], 1),
                if (md[2] >= md[1]) lv[1] else lv[2], f2(min(md), 1), f2(q1[which.min(md)], 1), f2(q3[which.min(md)], 1), f2(U, 1), fmt_p(wt$p.value), f2(r), n[1], n[2]),
        extra = ex, note = if (!exact) "Ties or a large sample: p uses the normal approximation with continuity correction (as R does)." else "Exact p-value (no ties).")
  },
  # ----- paired -----
  tp = {
    dlt <- fr$b - fr$a; tt <- t.test(fr$b, fr$a, paired = TRUE); dz <- mean(dlt) / sd(dlt)
    res(TEST_INFO$tp$name, "The mean within-person change is zero.", "The mean within-person change is not zero.",
        "t", tt$statistic, tt$parameter, tt$p.value, "Mean change (second − first)", mean(dlt), tt$conf.int, "Cohen's dz", dz, size_word(dz, c(.2, .5, .8)),
        "t = mean change ÷ (SD of changes ÷ √n)", sprintf("t.test(d$%s, d$%s, paired = TRUE)", spec$b, spec$a),
        sprintf("Mean change from %s to %s was %s (95%% CI %s to %s; SD %s; n = %d pairs); paired t = %s, df = %d, p = %s; Cohen's dz = %s.",
                spec$alab, spec$blab, f2(mean(dlt), 1), f2(tt$conf.int[1], 1), f2(tt$conf.int[2], 1), f2(sd(dlt), 1), length(dlt), f2(tt$statistic), as.integer(tt$parameter), fmt_p(tt$p.value), f2(dz)))
  },
  wp = {
    dlt <- fr$b - fr$a; nz <- dlt[dlt != 0]
    exact <- !any(duplicated(abs(nz))) && length(nz) < 50 && length(nz) == length(dlt)
    wt <- suppressWarnings(wilcox.test(fr$b, fr$a, paired = TRUE, conf.int = TRUE, exact = exact))
    r <- z_from_p(wt$p.value) / sqrt(length(nz))
    rk <- rank(abs(nz)); Wp <- sum(rk[nz > 0]); Wm <- sum(rk[nz < 0])
    ex <- data.frame(Quantity = c("Pairs", "Pairs with no change (dropped)", "Increases", "Decreases", "W⁺ (sum of ranks of increases)", "W⁻ (sum of ranks of decreases)", "Median change"),
                     Value = c(length(dlt), sum(dlt == 0), sum(dlt > 0), sum(dlt < 0), f2(Wp, 1), f2(Wm, 1), f2(median(dlt), 1)))
    res(TEST_INFO$wp$name, "Changes are symmetric around zero.", "Changes tend to be positive or negative.",
        "V (= W⁺)", wt$statistic, NA, wt$p.value, "Hodges–Lehmann estimate of the change", wt$estimate, wt$conf.int, "r", r, size_word(r, c(.1, .3, .5)),
        "Rank |change|, add the ranks of increases (W⁺) and of decreases (W⁻)", sprintf("wilcox.test(d$%s, d$%s, paired = TRUE, conf.int = TRUE)", spec$b, spec$a),
        sprintf("The median change from %s to %s was %s (IQR %s to %s); %d of %d decreased and %d increased; Wilcoxon signed-rank V = %s, p = %s, r = %s.",
                spec$alab, spec$blab, f2(median(dlt), 1), f2(quantile(dlt, .25), 1), f2(quantile(dlt, .75), 1), sum(dlt < 0), length(dlt), sum(dlt > 0), f2(wt$statistic, 1), fmt_p(wt$p.value), f2(r)),
        extra = ex, note = if (!exact) "Ties or zero changes present: p uses the normal approximation (as R does)." else "Exact p-value.")
  },
  signp = {
    dlt <- fr$b - fr$a; k <- sum(dlt > 0); n <- sum(dlt != 0); bt <- binom.test(k, n)
    res(TEST_INFO$signp$name, "Increases and decreases are equally likely.", "One direction of change is more common.",
        "Number of increases", k, NA, bt$p.value, "Proportion of increases", k / n, bt$conf.int, NULL, NULL, NULL,
        "Count increases among non-zero changes; compare with binomial(n, 0.5)", sprintf("binom.test(sum(d$%s > d$%s), sum(d$%s != d$%s))", spec$b, spec$a, spec$b, spec$a),
        sprintf("%d of %d people increased and %d decreased from %s to %s; sign test p = %s.", k, n, n - k, spec$alab, spec$blab, fmt_p(bt$p.value)))
  },
  mcn = , mcnx = {
    tab <- table(fr$a, fr$b); b <- tab[1, 2]; c <- tab[2, 1]; lv <- rownames(tab)
    if (code == "mcn") { mt <- mcnemar.test(tab, correct = TRUE); stat <- mt$statistic; df <- 1; p <- mt$p.value }
    else { bt <- binom.test(b, b + c); stat <- b; df <- NA; p <- bt$p.value }
    pa <- mean(fr$a == lv[2]); pb <- mean(fr$b == lv[2])
    res(TEST_INFO[[code]]$name, "Changing one way is as likely as changing the other way.", "Changes go more often in one direction.",
        if (code == "mcn") "χ² (with continuity correction)" else "Changes one way", stat, df, p,
        sprintf("Proportion '%s': first vs second", lv[2]), pb - pa, NULL, "Odds ratio of change (b ÷ c)", b / max(c, 0.5), NULL,
        if (code == "mcn") "χ² = (|b − c| − 1)² ÷ (b + c), using only the people who changed" else "Exact binomial test on b out of b + c changes",
        if (code == "mcn") sprintf("mcnemar.test(table(d$%s, d$%s))", spec$a, spec$b) else sprintf("binom.test(%d, %d)", b, b + c),
        sprintf("'%s' was reported by %s%% (%s) and %s%% (%s) of %d people; %d changed from '%s' to '%s' and %d the other way; %s p = %s.",
                lv[2], f2(100 * pa, 1), lc1(spec$alab), f2(100 * pb, 1), lc1(spec$blab), nrow(fr), b, lv[1], lv[2], c, if (code == "mcn") "McNemar" else "exact McNemar", fmt_p(p)),
        extra = { e <- as.data.frame.matrix(tab); names(e) <- paste(spec$blab, "=", names(e)); cbind(` ` = paste(spec$alab, "=", rownames(e)), e) })
  },
  # ----- k groups -----
  aov = {
    fit <- aov(y ~ g, data = fr); a <- summary(fit)[[1]]; ss <- a$`Sum Sq`; eta <- ss[1] / sum(ss)
    tk <- TukeyHSD(fit)$g
    ex <- data.frame(Comparison = rownames(tk), Difference = f2(tk[, 1], 2), `95% CI` = paste(f2(tk[, 2], 2), "to", f2(tk[, 3], 2)), `p (Tukey adjusted)` = sapply(tk[, 4], fmt_p), check.names = FALSE)
    gm <- tapply(fr$y, fr$g, mean); gs <- tapply(fr$y, fr$g, sd)
    res(TEST_INFO$aov$name, "All group means are equal.", "At least one group mean differs.",
        "F", a$`F value`[1], paste(a$Df[1], "and", a$Df[2]), a$`Pr(>F)`[1], NULL, NULL, NULL, "η² (eta squared)", eta, size_word(eta, c(.01, .06, .14)),
        "F = variance between groups ÷ variance within groups", sprintf("summary(aov(%s ~ %s, data = d)); TukeyHSD(aov(%s ~ %s, data = d))", yv, gv, yv, gv),
        sprintf("Mean %s differed across %s (%s); one-way ANOVA F(%d, %d) = %s, p = %s, η² = %s.",
                lab, glab, paste(sprintf("%s %s (SD %s)", names(gm), f2(gm, 1), f2(gs, 1)), collapse = "; "), a$Df[1], a$Df[2], f2(a$`F value`[1]), fmt_p(a$`Pr(>F)`[1]), f2(eta)),
        extra = ex, note = "Pairwise comparisons: Tukey's honestly significant difference, adjusted for multiple comparisons.")
  },
  waov = {
    ot <- oneway.test(y ~ g, data = fr, var.equal = FALSE); fit <- aov(y ~ g, data = fr); ss <- summary(fit)[[1]]$`Sum Sq`; eta <- ss[1] / sum(ss)
    gm <- tapply(fr$y, fr$g, mean); gs <- tapply(fr$y, fr$g, sd)
    res(TEST_INFO$waov$name, "All group means are equal.", "At least one group mean differs.",
        "Welch F", ot$statistic, paste(f2(ot$parameter[1], 0), "and", f2(ot$parameter[2], 1)), ot$p.value, NULL, NULL, NULL, "η² (from ordinary ANOVA)", eta, size_word(eta, c(.01, .06, .14)),
        "Welch F weights each group by n ÷ variance", sprintf("oneway.test(%s ~ %s, data = d, var.equal = FALSE)", yv, gv),
        sprintf("Mean %s differed across %s (%s); Welch ANOVA F(%s, %s) = %s, p = %s.",
                lab, glab, paste(sprintf("%s %s (SD %s)", names(gm), f2(gm, 1), f2(gs, 1)), collapse = "; "), f2(ot$parameter[1], 0), f2(ot$parameter[2], 1), f2(ot$statistic), fmt_p(ot$p.value)))
  },
  kw = {
    kt <- kruskal.test(y ~ g, data = fr); N <- nrow(fr); eps <- kt$statistic / (N - 1)
    md <- tapply(fr$y, fr$g, median); mr <- tapply(rank(fr$y), fr$g, mean)
    ex <- dunn_test(fr$y, fr$g)
    res(TEST_INFO$kw$name, "All groups come from the same distribution.", "At least one group tends to have higher or lower values.",
        "H", kt$statistic, kt$parameter, kt$p.value, NULL, NULL, NULL, "ε² (epsilon squared)", eps, size_word(eps, c(.01, .08, .26)),
        "Rank all values; H = 12/[N(N+1)] × Σ R²/n − 3(N+1), compared with χ² on k − 1 df",
        sprintf("kruskal.test(%s ~ %s, data = d)", yv, gv),
        sprintf("%s differed across %s (medians %s; mean ranks %s); Kruskal–Wallis H = %s, df = %d, p = %s, ε² = %s.",
                cap1(lab), glab, paste(sprintf("%s %s", names(md), f2(md, 1)), collapse = ", "), paste(f2(mr, 1), collapse = ", "),
                f2(kt$statistic), as.integer(kt$parameter), fmt_p(kt$p.value), f2(eps)),
        extra = ex, note = "Pairwise comparisons: Dunn's test with Holm adjustment for multiple comparisons.")
  },
  # ----- repeated -----
  rm = {
    m <- fr$m; k <- ncol(m); n <- nrow(m)
    long <- data.frame(y = as.vector(m), time = factor(rep(colnames(m), each = n), levels = colnames(m)), id = factor(rep(1:n, k)))
    gm <- mean(m); ss_t <- n * sum((colMeans(m) - gm)^2); ss_s <- k * sum((rowMeans(m) - gm)^2); ss_tot <- sum((m - gm)^2); ss_e <- ss_tot - ss_t - ss_s
    df1 <- k - 1; df2 <- (k - 1) * (n - 1); F <- (ss_t / df1) / (ss_e / df2); p <- pf(F, df1, df2, lower.tail = FALSE)
    sph <- sphericity(m); pgg <- pf(F, df1 * sph$gg, df2 * sph$gg, lower.tail = FALSE); peta <- ss_t / (ss_t + ss_e)
    ex <- data.frame(Time = colnames(m), Mean = f2(colMeans(m), 1), SD = f2(apply(m, 2, sd), 1))
    res(TEST_INFO$rm$name, "The mean is the same at every time point.", "The mean changes over time.",
        "F", F, paste(df1, "and", df2), p, NULL, NULL, NULL, "Partial η²", peta, size_word(peta, c(.01, .06, .14)),
        "F = variance between time points ÷ residual variance (after removing differences between people)",
        sprintf("summary(aov(y ~ time + Error(id/time), data = long))"),
        sprintf("The mean%s changed over time (%s); repeated-measures ANOVA F(%d, %d) = %s, p = %s (Greenhouse–Geisser corrected p = %s), partial η² = %s; n = %d.",
                if (lab == "the outcome") "" else paste0(" ", lab), paste(sprintf("%s %s", colnames(m), f2(colMeans(m), 1)), collapse = ", "), df1, df2, f2(F), fmt_p(p), fmt_p(pgg), f2(peta), n),
        extra = ex, note = sprintf("Greenhouse–Geisser epsilon = %s; corrected p = %s. Use the corrected p if sphericity is doubtful.", f2(sph$gg), fmt_p(pgg)))
  },
  fr = {
    m <- fr$m; ft <- friedman.test(m); W <- ft$statistic / (nrow(m) * (ncol(m) - 1))
    ex <- data.frame(Time = colnames(m), Median = f2(apply(m, 2, median), 1), `Mean rank within person` = f2(colMeans(t(apply(m, 1, rank))), 2), check.names = FALSE)
    res(TEST_INFO$fr$name, "The distribution is the same at every time point.", "Values tend to change over time.",
        "Friedman χ²", ft$statistic, ft$parameter, ft$p.value, NULL, NULL, NULL, "Kendall's W", W, size_word(W, c(.1, .3, .5)),
        "Rank the values within each person; compare the rank totals of the time points", "friedman.test(as.matrix(d[, c(...time columns...)]))",
        sprintf("Values%s changed over time (medians %s); Friedman χ² = %s, df = %d, p = %s, Kendall's W = %s; n = %d.",
                if (lab == "the outcome") "" else paste(" of", lab), paste(sprintf("%s %s", colnames(m), f2(apply(m, 2, median), 1)), collapse = ", "), f2(ft$statistic), as.integer(ft$parameter), fmt_p(ft$p.value), f2(W), nrow(m)),
        extra = ex)
  },
  # ----- categorical association -----
  chi = , yates = , fisher = , trend = {
    tab <- table(fr$g, fr$y); N <- sum(tab); lv <- colnames(tab); succ <- lv[length(lv)]
    if (code %in% c("chi", "yates")) { ct <- suppressWarnings(chisq.test(tab, correct = code == "yates")); stat <- ct$statistic; df <- ct$parameter; p <- ct$p.value; slab <- "χ²" }
    if (code == "fisher") { ft <- if (all(dim(tab) == 2)) fisher.test(tab) else fisher.test(tab, simulate.p.value = nrow(tab) * ncol(tab) > 20, B = 10000); stat <- NA; df <- NA; p <- ft$p.value; slab <- "(exact: no statistic)" }
    if (code == "trend") { pt <- prop.trend.test(tab[, succ], rowSums(tab)); stat <- pt$statistic; df <- 1; p <- pt$p.value; slab <- "χ² for trend" }
    chi0 <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic); V <- sqrt(chi0 / (N * (min(dim(tab)) - 1)))
    pr <- tab[, succ] / rowSums(tab)
    est_lab <- NULL; est <- NULL; ci <- NULL
    if (all(dim(tab) == 2)) {
      rd <- pr[2] - pr[1]; se <- sqrt(pr[1] * (1 - pr[1]) / sum(tab[1, ]) + pr[2] * (1 - pr[2]) / sum(tab[2, ]))
      est_lab <- sprintf("Risk difference: '%s' in %s − %s", succ, rownames(tab)[2], rownames(tab)[1]); est <- rd; ci <- rd + c(-1, 1) * 1.96 * se
    }
    ex <- data.frame(Group = rownames(tab), n = as.integer(rowSums(tab)), `Number '%s'` = as.integer(tab[, succ]), `Percent` = f2(100 * pr, 1), check.names = FALSE)
    names(ex)[3] <- sprintf("Number '%s'", succ)
    if (all(dim(tab) == 2)) { a <- tab[1, 2]; b <- tab[1, 1]; c <- tab[2, 2]; dd <- tab[2, 1]
      ex2 <- sprintf("Relative risk (%s vs %s) = %s; odds ratio = %s", rownames(tab)[2], rownames(tab)[1], f2(pr[2] / pr[1]), f2((c / dd) / max(a / b, 1e-9))) } else ex2 <- NULL
    res(TEST_INFO[[code]]$name, sprintf("%s and %s are not associated.", glab, lab), if (code == "trend") "The proportion rises or falls steadily across the ordered groups." else sprintf("%s and %s are associated.", glab, lab),
        slab, stat, df, p, est_lab, est, ci, "Cramér's V", V, size_word(V, c(.1, .3, .5)),
        switch(code, chi = "χ² = Σ (O − E)² ÷ E over all cells", yates = "χ² = Σ (|O − E| − 0.5)² ÷ E", fisher = "Probability of every table with the same totals (hypergeometric)", trend = "Tests a straight-line trend in the proportion across ordered groups"),
        switch(code, chi = sprintf("chisq.test(table(d$%s, d$%s), correct = FALSE)", gv, yv), yates = sprintf("chisq.test(table(d$%s, d$%s), correct = TRUE)", gv, yv),
               fisher = sprintf("fisher.test(table(d$%s, d$%s))", gv, yv), trend = sprintf("prop.trend.test(x = successes, n = totals)")),
        sprintf("'%s' was recorded in %s; %s p = %s%s; Cramér's V = %s; N = %d.",
                succ, paste(sprintf("%d of %d (%s%%) in %s", tab[, succ], rowSums(tab), f2(100 * pr, 1), rownames(tab)), collapse = " and "),
                TEST_INFO[[code]]$name, fmt_p(p), if (code %in% c("chi", "yates", "trend")) sprintf(" (%s = %s, df = %d)", slab, f2(stat), as.integer(df)) else "",
                f2(V), N),
        extra = ex, note = ex2)
  },
  # ----- correlation -----
  pear = , spear = , kend = {
    meth <- c(pear = "pearson", spear = "spearman", kend = "kendall")[code]
    ct <- suppressWarnings(cor.test(fr$x, fr$y, method = meth, exact = FALSE))
    est <- ct$estimate
    res(TEST_INFO[[code]]$name, "There is no association (correlation = 0).", "There is an association (correlation ≠ 0).",
        c(pear = "t", spear = "S", kend = "z")[code], ct$statistic, if (code == "pear") ct$parameter else NA, ct$p.value,
        c(pear = "Pearson r", spear = "Spearman ρ (rho)", kend = "Kendall τ (tau)")[code], est, ct$conf.int,
        "Correlation (size)", est, size_word(est, c(.1, .3, .5)),
        c(pear = "r = covariance ÷ (SD of x × SD of y)", spear = "Pearson correlation of the ranks", kend = "(concordant − discordant pairs) ÷ total pairs")[code],
        sprintf("cor.test(d$%s, d$%s, method = '%s')", spec$cx, spec$cy, meth),
        sprintf("%s and %s were %s correlated (%s = %s%s; n = %d); p = %s.", spec$cxlab, spec$cylab, if (est >= 0) "positively" else "negatively",
                c(pear = "r", spear = "ρ", kend = "τ")[code], f2(est), if (!is.null(ct$conf.int)) sprintf(", 95%% CI %s to %s", f2(ct$conf.int[1]), f2(ct$conf.int[2])) else "",
                nrow(fr), fmt_p(ct$p.value)))
  })
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

# ---------------------------------------------------------------------------
# Plots
# ---------------------------------------------------------------------------
wrap15 <- function(x) sapply(x, function(z) paste(strwrap(z, 16), collapse = "\n"))
plot_look <- function(fr, spec, which = "main") {
  q <- spec$q; tp <- spec$type; ylab <- spec$ylab %||% "Value"
  if (tp == "cat") {
    if (q == "one") {
      tab <- as.data.frame(prop.table(table(fr$y))); names(tab) <- c("cat", "p")
      return(ggplot(tab, aes(p * 100, reorder(cat, p))) + geom_segment(aes(x = 0, xend = p * 100, yend = cat), colour = COL$sky, linewidth = 1.2) +
               geom_point(size = 5, colour = COL$blue) + labs(x = "Percent of people", y = NULL) + theme_app())
    }
    if (q == "paired") {
      tab <- as.data.frame(table(First = fr$a, Second = fr$b))
      return(ggplot(tab, aes(Second, First)) + geom_point(aes(size = Freq), colour = COL$blue, alpha = .8) + geom_text(aes(label = Freq), colour = "white", fontface = "bold") +
               scale_size_area(max_size = 28, guide = "none") + labs(x = spec$blab, y = spec$alab, subtitle = "Number of people in each combination") + theme_app())
    }
    tab <- as.data.frame(prop.table(table(fr$g, fr$y), 1)); names(tab) <- c("g", "y", "p")
    return(ggplot(tab, aes(p * 100, g, colour = y)) + geom_line(aes(group = g), colour = "grey80", linewidth = 1) + geom_point(size = 5) +
             scale_colour_manual(values = PAL) + labs(x = "Percent within group", y = NULL, subtitle = paste("Percentage in each category of", lc1(ylab))) + theme_app())
  }
  if (q == "one") {
    df <- data.frame(y = fr$y)
    p <- ggplot(df, aes(y, 0)) + geom_boxplot(width = .4, fill = "#EAF4FA", colour = COL$sky, outlier.shape = NA) +
      geom_jitter(height = .12, width = 0, size = 3, colour = COL$blue, alpha = .8) + geom_vline(xintercept = spec$mu, colour = COL$red, linetype = 2) +
      annotate("text", x = spec$mu, y = .32, label = paste("value tested:", spec$mu), colour = COL$red, hjust = -0.05) +
      labs(x = ylab, y = NULL, subtitle = "Each dot is one person · dashed line: the value tested") + theme_app() + theme(axis.text.y = element_blank())
    return(p)
  }
  if (q %in% c("two", "k")) {
    p <- ggplot(fr, aes(g, y, colour = g)) + geom_boxplot(width = .45, fill = "#F5FAFD", outlier.shape = NA, colour = "grey60") +
      geom_jitter(width = .12, height = 0, size = 3, alpha = .85) + stat_summary(fun = mean, geom = "point", shape = 23, size = 4, fill = "white", colour = COL$navy) +
      scale_colour_manual(values = PAL, guide = "none") + labs(x = NULL, y = ylab, subtitle = "Each dot is one person · box: middle half with median · diamond: mean") + scale_x_discrete(labels = wrap15) + theme_app()
    return(p)
  }
  if (q == "paired") {
    df <- data.frame(id = seq_len(nrow(fr)), a = fr$a, b = fr$b); df$dir <- ifelse(df$b > df$a, "Increased", ifelse(df$b < df$a, "Decreased", "No change"))
    long <- rbind(data.frame(id = df$id, t = spec$alab, v = df$a, dir = df$dir), data.frame(id = df$id, t = spec$blab, v = df$b, dir = df$dir))
    long$t <- factor(long$t, levels = c(spec$alab, spec$blab))
    return(ggplot(long, aes(t, v, group = id, colour = dir)) + geom_line(linewidth = .9, alpha = .8) + geom_point(size = 2.8) +
             scale_colour_manual(values = c(Increased = COL$red, Decreased = COL$blue, `No change` = COL$grey)) +
             labs(x = NULL, y = ylab, subtitle = "Each line is one person") + scale_x_discrete(labels = wrap15) + theme_app())
  }
  if (q == "repeated") {
    m <- fr$m; long <- data.frame(id = rep(seq_len(nrow(m)), ncol(m)), t = factor(rep(colnames(m), each = nrow(m)), levels = colnames(m)), v = as.vector(m))
    med <- data.frame(t = factor(colnames(m), levels = colnames(m)), v = apply(m, 2, median))
    return(ggplot(long, aes(t, v)) + geom_line(aes(group = id), colour = COL$sky, alpha = .6) + geom_point(colour = COL$sky, size = 2) +
             geom_line(data = med, aes(group = 1), colour = COL$navy, linewidth = 1.6) + geom_point(data = med, colour = COL$navy, size = 4) +
             labs(x = NULL, y = "Value", subtitle = "Each pale line is one person · dark line joins the medians") + scale_x_discrete(labels = wrap15) + theme_app())
  }
  if (q == "corr") {
    return(ggplot(fr, aes(x, y)) + geom_point(size = 3, colour = COL$blue, alpha = .85) + geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = COL$amber, linewidth = 1) +
             geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = COL$navy, linetype = 2, linewidth = .8) +
             labs(x = spec$cxlab, y = spec$cylab, subtitle = "Each dot is one person · amber: smooth curve · dashed: straight line") + theme_app())
  }
}

plot_shape <- function(fr, spec) {
  q <- spec$q; tp <- spec$type
  if (tp == "cat") return(NULL)
  vals <- switch(q, one = data.frame(v = fr$y, g = "All values"), two = , k = data.frame(v = fr$y, g = fr$g),
                 paired = data.frame(v = fr$b - fr$a, g = "Differences (second − first)"),
                 repeated = { m <- fr$m; data.frame(v = as.vector(m - rowMeans(m) - matrix(colMeans(m), nrow(m), ncol(m), byrow = TRUE) + mean(m)), g = "Residuals") },
                 corr = rbind(data.frame(v = fr$x, g = spec$cxlab), data.frame(v = fr$y, g = spec$cylab)))
  ggplot(vals, aes(sample = v, colour = g)) + stat_qq_line(colour = "grey55", linetype = 2) + stat_qq(size = 2.6, alpha = .85) +
    facet_wrap(~g, scales = "free") + scale_colour_manual(values = PAL, guide = "none") +
    labs(x = "Expected if bell-shaped", y = "Observed values",
         subtitle = "Close to the dashed line = bell-shaped · curving away at the ends = skew or long tails") + theme_app()
}

plot_ecdf <- function(fr, spec) {
  q <- spec$q; tp <- spec$type
  if (tp == "cat" || !(q %in% c("one", "two", "k", "paired"))) return(NULL)
  vals <- switch(q, one = data.frame(v = fr$y, g = "All values"), two = , k = data.frame(v = fr$y, g = fr$g), paired = data.frame(v = fr$b - fr$a, g = "Differences"))
  ggplot(vals, aes(v, colour = g)) + stat_ecdf(geom = "step", linewidth = 1.1) + geom_point(stat = "ecdf", size = 1.8) + scale_colour_manual(values = PAL) +
    labs(x = if (q == "paired") "Difference (second − first)" else spec$ylab, y = "Proportion at or below",
         subtitle = "A curve further to the right means higher values") + theme_app()
}

describe <- function(fr, spec) {
  q <- spec$q; tp <- spec$type
  sumrow <- function(x, nm) data.frame(Group = nm, n = length(x), Mean = f2(mean(x), 1), SD = f2(sd(x), 1), Median = f2(median(x), 1),
                                       Quartiles = paste(f2(quantile(x, .25), 1), "to", f2(quantile(x, .75), 1)), Range = paste(f2(min(x), 0), "to", f2(max(x), 0)),
                                       Skew = f2(skewness(x), 2), check.names = FALSE)
  if (tp == "cat") {
    if (q == "one") { t <- table(fr$y); return(data.frame(Category = names(t), n = as.integer(t), Percent = f2(100 * as.numeric(prop.table(t)), 1))) }
    if (q == "paired") { t <- table(fr$a, fr$b); df <- as.data.frame.matrix(t); df <- cbind(` ` = paste(spec$alab, "=", rownames(df)), df); names(df)[-1] <- paste(spec$blab, "=", names(df)[-1]); return(df) }
    t <- table(fr$g, fr$y); df <- as.data.frame.matrix(t); pc <- round(100 * prop.table(t, 1), 1)
    out <- data.frame(Group = rownames(df), check.names = FALSE)
    for (j in colnames(t)) out[[j]] <- sprintf("%d (%s%%)", t[, j], pc[, j])
    out$Total <- rowSums(t); return(out)
  }
  switch(q, one = sumrow(fr$y, "All"),
         two = , k = do.call(rbind, lapply(levels(fr$g), function(l) sumrow(fr$y[fr$g == l], l))),
         paired = rbind(sumrow(fr$a, spec$alab), sumrow(fr$b, spec$blab), sumrow(fr$b - fr$a, "Difference")),
         repeated = do.call(rbind, lapply(colnames(fr$m), function(cn) sumrow(fr$m[, cn], cn))),
         corr = rbind(sumrow(fr$x, spec$cxlab), sumrow(fr$y, spec$cylab)))
}

# =============================================================================
#  TEACHING CONTENT: what to do, what to avoid, pictures of each assumption
# =============================================================================
STEP_COL <- list(
  s1 = c(main = "#1E7FB0", pale = "#EAF4FA", name = "Choose the data"),
  s2 = c(main = "#7B5EA7", pale = "#F2EDF9", name = "Ask your question"),
  s3 = c(main = "#138D90", pale = "#E6F5F5", name = "Look at the data"),
  s4 = c(main = "#C98A12", pale = "#FDF4E1", name = "Check the assumptions"),
  s5 = c(main = "#2E8B57", pale = "#E8F5EE", name = "Run the test"),
  s6 = c(main = "#D35454", pale = "#FCEDED", name = "Report the result"))

DODONT <- list(
  s1 = list(do = c("Read the story: who was studied, and how", "Check what kind of values each variable holds", "Note how many people there are"),
            dont = c("Start testing before you know what each column means", "Treat an ID number as a measurement")),
  s2 = list(do = c("Decide the question from the study design, before looking at results", "Say whether the groups are separate people or the same people", "Choose the kind of outcome: measurement, ordered score or category"),
            dont = c("Pick the question that gives the smallest p-value", "Treat before-and-after values on the same people as two separate groups")),
  s3 = list(do = c("Look at every person as a dot before any test", "Compare the middle (median) and the spread of each group", "Look for long tails, piles at zero and extreme values"),
            dont = c("Rely on the mean alone when the data are lopsided", "Delete unusual values just because they are unusual")),
  s4 = list(do = c("Go through the checks one at a time", "Use the plots as well as the formal tests", "Decide the test from the checks, then stop changing it"),
            dont = c("Treat Shapiro–Wilk p > 0.05 as proof of a bell shape", "Run both kinds of test and keep the one with the smaller p")),
  s5 = list(do = c("Run the test the checks suggested", "Read the effect size and the confidence interval, not only p", "Look at the pairwise comparisons only after an overall difference"),
            dont = c("Say 'no difference' when p ≥ 0.05; say 'no clear evidence of a difference'", "Report p < 0.05 instead of the exact p-value")),
  s6 = list(do = c("Give a summary for each group that matches the test", "Give the test name, its statistic, the exact p and the size of the difference", "Say why you chose this test"),
            dont = c("Report means alongside a rank test", "Leave out the number of people")))

IFNOT <- c(
  design = "If people are clustered (patients within practices, repeated visits), these simple tests are not enough: a mixed model is needed.",
  kind = "Ordered scores go to rank tests; categories go to tests for counts.",
  shape = "Use the rank-based (non-parametric) version of the test, or transform skewed data (e.g. log) and re-check.",
  outlier = "First check it is not a data-entry error. If it is real, keep it and use a rank-based test, which is not pulled by it.",
  spread = "Use the Welch version (Welch t-test, Welch ANOVA), which does not assume equal spread.",
  size = "With small groups the shape cannot be judged; prefer the rank test and report results as preliminary.",
  simshape = "If shapes differ, report the rank test as 'one group tends to have higher values' rather than 'the medians differ'.",
  expected = "Use Fisher's exact test, or combine sparse categories if that makes sense.",
  counted = "If the same people appear twice, use McNemar's test instead.",
  discordant = "With fewer than 25 changes, use the exact McNemar test.",
  symmetry = "If the differences are strongly lopsided, the sign test makes no assumption about shape.",
  nonzero = "With fewer than 6 people changing, no rank test can reach p < 0.05: the study is too small.",
  sphericity = "Report the Greenhouse–Geisser corrected p-value, or use the Friedman test.",
  linear = "If the pattern is curved but steadily rising or falling, use Spearman's correlation.",
  complete = "People with a missing time point are left out; consider a mixed model if many are missing.",
  minp = "Collect more data: no rank test can show a difference at this size.",
  ordered = "When groups are ordered, the chi-square test for trend asks the sharper question.")

check_key <- function(name) {
  if (grepl("^Shape", name)) return("shape"); if (grepl("^Outliers", name)) return("outlier")
  k <- c("Design and independence" = "design", "Kind of outcome" = "kind", "Equal spread" = "spread", "Group sizes" = "size",
         "Similar shape for the rank test" = "simshape", "Expected counts" = "expected", "Each person counted once" = "counted",
         "Discordant pairs" = "discordant", "Symmetry for Wilcoxon" = "symmetry", "Symmetry of the differences" = "symmetry",
         "Number of non-zero differences" = "nonzero", "Sphericity" = "sphericity", "Straight-line pattern" = "linear",
         "Complete data" = "complete", "Smallest possible p for the rank test" = "minp", "Ordered groups" = "ordered",
         "Number of pairs" = "size", "Sample size" = "size")
  if (name %in% names(k)) k[[name]] else "other"
}
check_group <- function(name) sub("^(Shape|Outliers): ", "", name)

# Teaching pictures: what "met" and "not met" look like ----------------------
illus <- function(key) {
  set.seed(7)
  lab <- c("Met", "Not met")
  th <- theme_app() + theme(axis.text = element_blank(), axis.title = element_text(size = 10), panel.grid = element_blank(),
                            strip.text = element_text(size = 13), plot.subtitle = element_text(size = 10))
  fcol <- scale_colour_manual(values = c(Met = COL$green, `Not met` = COL$red), guide = "none")
  if (key == "shape") {
    a <- qnorm(ppoints(40)); b <- sort(rexp(40)) * 1.4
    df <- rbind(data.frame(t = a, v = sort(a + rnorm(40, 0, .12)), f = "Met"), data.frame(t = a, v = (b - mean(b)) / sd(b), f = "Not met"))
    df$f <- factor(df$f, lab)
    return(ggplot(df, aes(t, v, colour = f)) + geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey55") + geom_point(size = 2.4) + facet_wrap(~f) + fcol + th +
             labs(x = "Expected if bell-shaped", y = "Observed", subtitle = "Met: points on the line · Not met: points curve away (skew)"))
  }
  if (key %in% c("outlier", "symmetry")) {
    a <- c(rnorm(18, 10, 1.5)); b <- if (key == "outlier") c(a[-18], 24) else c(rexp(18, .3) + 6)
    df <- rbind(data.frame(v = a, f = "Met"), data.frame(v = b, f = "Not met")); df$f <- factor(df$f, lab)
    return(ggplot(df, aes(v, 0, colour = f)) + geom_jitter(height = .15, width = 0, size = 3) + facet_wrap(~f, scales = "free_x") + fcol + th + ylim(-.5, .5) +
             labs(x = "Values (each dot is one person)", y = NULL, subtitle = if (key == "outlier") "Not met: one value far from all the others" else "Not met: a long tail on one side"))
  }
  if (key == "spread") {
    df <- rbind(data.frame(g = rep(c("A", "B"), each = 15), v = c(rnorm(15, 10, 2), rnorm(15, 13, 2)), f = "Met"),
                data.frame(g = rep(c("A", "B"), each = 15), v = c(rnorm(15, 10, 1), rnorm(15, 13, 6)), f = "Not met")); df$f <- factor(df$f, lab)
    return(ggplot(df, aes(g, v, colour = f)) + geom_jitter(width = .1, size = 2.4) + stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), geom = "errorbar", width = .25, colour = COL$navy) +
             facet_wrap(~f) + fcol + th + theme(axis.text.x = element_text()) + labs(x = "Group", y = "Value", subtitle = "Navy bars: ± 1 SD · Not met: one group far more spread out"))
  }
  if (key %in% c("size", "minp", "nonzero")) {
    df <- rbind(data.frame(x = runif(40), y = runif(40), f = "Met"), data.frame(x = runif(5), y = runif(5), f = "Not met")); df$f <- factor(df$f, lab)
    return(ggplot(df, aes(x, y, colour = f)) + geom_point(size = 3.5) + facet_wrap(~f) + fcol + th +
             labs(x = NULL, y = NULL, subtitle = "Each dot is one person · Not met: too few people to judge the shape or reach small p-values"))
  }
  if (key == "simshape") {
    x <- seq(0, 20, .1)
    df <- rbind(data.frame(x = x, d = dnorm(x, 8, 2), g = "A", f = "Met"), data.frame(x = x, d = dnorm(x, 11, 2), g = "B", f = "Met"),
                data.frame(x = x, d = dnorm(x, 8, 1.2), g = "A", f = "Not met"), data.frame(x = x, d = dexp(x, .25) * .45, g = "B", f = "Not met")); df$f <- factor(df$f, lab)
    return(ggplot(df, aes(x, d, colour = g)) + geom_line(linewidth = 1.3) + facet_wrap(~f) + scale_colour_manual(values = c(A = COL$blue, B = COL$amber)) + th +
             labs(x = "Values", y = NULL, subtitle = "Met: same shape, shifted · Not met: different shapes"))
  }
  if (key %in% c("expected", "counted", "discordant")) {
    df <- rbind(data.frame(r = rep(c("Group 1", "Group 2"), 2), c = rep(c("Yes", "No"), each = 2), e = c(12.5, 9.5, 17.5, 20.5), f = "Met"),
                data.frame(r = rep(c("Group 1", "Group 2"), 2), c = rep(c("Yes", "No"), each = 2), e = c(2.1, 3.4, 17.9, 26.6), f = "Not met")); df$f <- factor(df$f, lab)
    df$ok <- df$e >= 5
    return(ggplot(df, aes(c, r)) + geom_tile(aes(fill = ok), alpha = .35, colour = "white", linewidth = 2) + geom_text(aes(label = e), size = 5, colour = COL$navy) +
             scale_fill_manual(values = c(`TRUE` = COL$green, `FALSE` = COL$red), guide = "none") + facet_wrap(~f) + th + theme(axis.text = element_text()) +
             labs(x = NULL, y = NULL, subtitle = "Expected counts: green ≥ 5 · red below 5"))
  }
  if (key == "sphericity") {
    t <- 1:3; df <- NULL
    for (i in 1:10) { df <- rbind(df, data.frame(id = i, t = t, v = 10 + rnorm(1, 0, 2) - t + rnorm(3, 0, .5), f = "Met"), data.frame(id = i, t = t, v = 10 - t * c(1, 1, 1 + rnorm(1, 0, 2)) + rnorm(3, 0, .3), f = "Not met")) }
    df$f <- factor(df$f, lab)
    return(ggplot(df, aes(t, v, group = id, colour = f)) + geom_line() + geom_point() + facet_wrap(~f) + fcol + th +
             labs(x = "Time point", y = "Value", subtitle = "Not met: changes spread out much more between some time points"))
  }
  if (key == "linear") {
    x <- runif(40, 0, 10)
    df <- rbind(data.frame(x = x, y = 2 + x + rnorm(40), f = "Met"), data.frame(x = x, y = 10 - (x - 5)^2 / 2 + rnorm(40, 0, .8), f = "Not met")); df$f <- factor(df$f, lab)
    return(ggplot(df, aes(x, y, colour = f)) + geom_point(size = 2.4) + facet_wrap(~f, scales = "free_y") + fcol + th + labs(x = "First variable", y = "Second variable", subtitle = "Not met: a curved pattern"))
  }
  if (key == "design") {
    df <- rbind(data.frame(x = c(runif(12, 0, 4), runif(12, 6, 10)), y = runif(24), cl = "none", f = "Met"),
                data.frame(x = rep(c(1.5, 5, 8.5), each = 8) + rnorm(24, 0, .4), y = rep(c(.3, .7, .4), each = 8) + rnorm(24, 0, .07), cl = "c", f = "Not met")); df$f <- factor(df$f, lab)
    return(ggplot(df, aes(x, y, colour = f)) + geom_point(size = 3) + facet_wrap(~f) + fcol + th +
             labs(x = NULL, y = NULL, subtitle = "Met: separate, unrelated people · Not met: people bunched in clusters (e.g. practices)"))
  }
  NULL
}

# Our data, for the check being looked at ------------------------------------
check_plot <- function(key, grp, fr, spec) {
  q <- spec$q
  if (spec$type == "cat") {
    if (key %in% c("expected", "counted") && q %in% c("two", "k")) {
      tab <- table(fr$g, fr$y); E <- suppressWarnings(chisq.test(tab, correct = FALSE)$expected)
      df <- data.frame(r = rep(rownames(tab), ncol(tab)), c = rep(colnames(tab), each = nrow(tab)), O = as.vector(tab), E = as.vector(E))
      return(ggplot(df, aes(c, r)) + geom_tile(aes(fill = E >= 5), alpha = .35, colour = "white", linewidth = 2) +
               geom_text(aes(label = sprintf("O = %d   E = %.1f", O, E)), colour = COL$navy, size = 4.2) +
               scale_fill_manual(values = c(`TRUE` = COL$green, `FALSE` = COL$red), guide = "none") +
               labs(x = lc1(spec$ylab), y = lc1(spec$glab), subtitle = "O = observed count · E = expected if unrelated · red: E below 5") + theme_app())
    }
    return(plot_look(fr, spec))
  }
  if (key == "shape") {
    v <- switch(q, one = fr$y, two = , k = fr$y[fr$g == grp], paired = fr$b - fr$a,
                repeated = { m <- fr$m; as.vector(m - rowMeans(m) - matrix(colMeans(m), nrow(m), ncol(m), byrow = TRUE) + mean(m)) },
                corr = if (grp == "first variable") fr$x else fr$y)
    return(ggplot(data.frame(v = v), aes(sample = v)) + stat_qq_line(colour = "grey55", linetype = 2) + stat_qq(colour = COL$amber, size = 3) +
             labs(x = "Expected if bell-shaped", y = "Observed", subtitle = paste("Q–Q plot:", grp)) + theme_app())
  }
  if (key %in% c("outlier", "symmetry", "nonzero")) {
    v <- switch(q, one = fr$y, two = , k = fr$y[fr$g == grp], paired = fr$b - fr$a, fr$y)
    qq <- quantile(v, c(.25, .75)); i <- diff(qq); out <- v < qq[1] - 1.5 * i | v > qq[2] + 1.5 * i
    return(ggplot(data.frame(v = v, out = out), aes(v, 0)) + geom_boxplot(width = .4, outlier.shape = NA, fill = "#FFF8EA", colour = "grey60") +
             geom_jitter(aes(colour = out), height = .1, width = 0, size = 3.4) + scale_colour_manual(values = c(`FALSE` = COL$blue, `TRUE` = COL$red), labels = c("typical", "beyond 1.5 × IQR")) +
             labs(x = if (q == "paired") "Difference (second − first)" else spec$ylab, y = NULL, subtitle = if (q %in% c("two", "k")) grp else NULL) + theme_app() + theme(axis.text.y = element_blank()))
  }
  if (key == "spread" || key == "simshape" || key == "size") {
    if (q %in% c("two", "k")) return(plot_look(fr, spec))
  }
  if (key == "sphericity" || key == "complete") return(plot_look(fr, spec))
  if (key == "linear" || (q == "corr")) return(plot_look(fr, spec))
  plot_look(fr, spec)
}

# Small pictures of each question type ---------------------------------------
QUESTION_HELP <- c(
  one = "One group, compared with a fixed value (e.g. is the average blood pressure above 140?).",
  two = "Two groups of different people (e.g. intervention vs usual care).",
  paired = "The same people measured twice, before and after (or matched pairs).",
  k = "Three or more groups of different people (e.g. four education levels).",
  repeated = "The same people measured at three or more times.",
  corr = "Do two numbers rise and fall together in the same people?")

question_pic <- function(q) {
  set.seed(3)
  th <- theme_void() + theme(plot.margin = margin(4, 4, 4, 4))
  switch(q,
    one = ggplot(data.frame(x = rnorm(15, 5), y = 0), aes(x, y)) + geom_point(colour = COL$blue, size = 3) + geom_vline(xintercept = 4, colour = COL$red, linetype = 2) + th,
    two = ggplot(data.frame(x = rep(1:2, each = 10) + rnorm(20, 0, .08), y = c(rnorm(10, 4), rnorm(10, 6))), aes(x, y, colour = factor(round(x)))) + geom_point(size = 3) + scale_colour_manual(values = PAL, guide = "none") + th,
    paired = { a <- rnorm(8, 6); ggplot(data.frame(id = rep(1:8, 2), t = rep(1:2, each = 8), v = c(a, a - 1 + rnorm(8, 0, .3))), aes(t, v, group = id)) + geom_line(colour = COL$purple) + geom_point(colour = COL$purple, size = 2.5) + th },
    k = ggplot(data.frame(x = rep(1:4, each = 8) + rnorm(32, 0, .08), y = rep(c(4, 5, 6, 5.5), each = 8) + rnorm(32, 0, .6)), aes(x, y, colour = factor(round(x)))) + geom_point(size = 2.6) + scale_colour_manual(values = PAL, guide = "none") + th,
    repeated = { a <- rnorm(8, 6); ggplot(data.frame(id = rep(1:8, 3), t = rep(1:3, each = 8), v = c(a, a - .8, a - 1.4) + rnorm(24, 0, .2)), aes(t, v, group = id)) + geom_line(colour = COL$green) + geom_point(colour = COL$green, size = 2.3) + th },
    corr = { x <- rnorm(25); ggplot(data.frame(x = x, y = x + rnorm(25, 0, .6)), aes(x, y)) + geom_point(colour = COL$amber, size = 2.6) + th })
}

# =============================================================================
#  THE PATH MAP: the whole decision route, with the student's route lit up
# =============================================================================
TASKS <- list(
  s1 = c("Pick a dataset", "Read the study story", "Check each variable's kind", "Press “Use this dataset”"),
  s2 = c("Pick the question card", "Pick the variables", "Confirm the kind of outcome", "Press “Submit my question”"),
  s3 = c("Look at every dot", "Compare medians and spread", "Read the Q–Q plot", "Press “Check the assumptions”"),
  s4 = c("Read the check", "Compare your data with the picture", "Note the verdict", "Press “Next check” until the end"),
  s5 = c("Keep the suggested test", "Press “Run the test”", "Read p, effect size and CI", "Press “Write the report”"),
  s6 = c("Copy the sentence", "Keep the R code", "Check all parts are present", "Start a new question if you wish"))

FAMILY <- c(t1 = "par", t2 = "par", welch = "par", tp = "par", aov = "par", waov = "par", rm = "par", pear = "par",
            w1 = "rank", sign1 = "rank", mw = "rank", wp = "rank", signp = "rank", kw = "rank", fr = "rank", spear = "rank", kend = "rank",
            binom = "cnt", gof = "cnt", chi = "cnt", yates = "cnt", fisher = "cnt", trend = "cnt", mcn = "cnt", mcnx = "cnt")

wrap_lines <- function(x, width) strwrap(x, width)

path_svg <- function(type = NULL, q = NULL, test = NULL, known = FALSE) {
  cols <- list(
    list(title = "1 · Kind of outcome", x = 8, w = 175, col = "#1E7FB0", pale = "#EAF4FA",
         nodes = c(num = "Measurement or count", ord = "Ordered score", cat = "Category (yes/no …)")),
    list(title = "2 · Study design", x = 228, w = 205, col = "#7B5EA7", pale = "#F2EDF9",
         nodes = c(one = "One group vs a value", two = "Two separate groups", paired = "Same people, twice",
                   k = "Three or more groups", repeated = "Same people, 3+ times", corr = "Two numbers together")),
    list(title = "3 · What the checks found", x = 478, w = 205, col = "#C98A12", pale = "#FDF4E1",
         nodes = c(bell = "Bell-shaped, enough people", skew = "Skewed or outliers", ordr = "Ordered score → ranks", cnt = "Counts in a table")),
    list(title = "4 · Family of test", x = 718, w = 212, col = "#2E8B57", pale = "#E8F5EE",
         nodes = c(par = "Parametric (means)", rank = "Rank-based (non-parametric)", cnt = "Tests for counts")))
  fam <- if (known && !is.null(test)) FAMILY[[test]] else NULL
  chk <- if (is.null(fam)) NULL else if (fam == "par") "bell" else if (fam == "cnt") "cnt" else if (identical(type, "ord")) "ordr" else "skew"
  sel <- list(type, q, chk, fam)
  H <- 300; top <- 46; nh <- 30
  ypos <- function(n) { gap <- (H - top - 8 - n * nh) / max(n - 1, 1); top + (0:(n - 1)) * (nh + gap) }
  out <- c(sprintf('<svg viewBox="0 0 1100 %d" width="100%%" xmlns="http://www.w3.org/2000/svg" style="font-family: Inter, Segoe UI, Arial, sans-serif">', H))
  centres <- list()
  for (i in seq_along(cols)) {
    cc <- cols[[i]]; ys <- ypos(length(cc$nodes)); names(ys) <- names(cc$nodes)
    centres[[i]] <- ys
    out <- c(out, sprintf('<text x="%d" y="22" font-size="13" font-weight="700" fill="%s" letter-spacing=".5">%s</text>', cc$x, cc$col, toupper(cc$title)))
  }
  # edges first
  for (i in 1:3) {
    a <- sel[[i]]; b <- sel[[i + 1]]
    if (is.null(a) || !(a %in% names(centres[[i]]))) next
    x1 <- cols[[i]]$x + cols[[i]]$w; y1 <- centres[[i]][[a]] + nh / 2
    for (nm in names(centres[[i + 1]])) {
      x2 <- cols[[i + 1]]$x; y2 <- centres[[i + 1]][[nm]] + nh / 2; mx <- (x1 + x2) / 2
      on <- !is.null(b) && nm == b
      out <- c(out, sprintf('<path d="M%d,%.1f C%.1f,%.1f %.1f,%.1f %d,%.1f" fill="none" stroke="%s" stroke-width="%s" %s/>',
                            x1, y1, mx, y1, mx, y2, x2, y2, if (on) cols[[i + 1]]$col else "#D3DBE1", if (on) "3.5" else "1.2", if (on) "" else 'stroke-dasharray="4 4"'))
    }
  }
  if (!is.null(fam)) {
    x1 <- cols[[4]]$x + cols[[4]]$w; y1 <- centres[[4]][[fam]] + nh / 2
    out <- c(out, sprintf('<path d="M%d,%.1f C%d,%.1f %d,150 952,150" fill="none" stroke="#D35454" stroke-width="3.5"/>', x1, y1, x1 + 12, y1, 940))
  }
  # nodes
  for (i in seq_along(cols)) {
    cc <- cols[[i]]
    for (nm in names(cc$nodes)) {
      y <- centres[[i]][[nm]]; on <- !is.null(sel[[i]]) && nm == sel[[i]]; unknown <- is.null(sel[[i]])
      fill <- if (on) cc$pale else "#FFFFFF"; stroke <- if (on) cc$col else "#DDE3E8"; tc <- if (on) "#00365F" else if (unknown) "#7F8E99" else "#A9B5BE"
      mark <- if (on) "✓ " else if (!unknown) "✗ " else ""
      mc <- if (on) cc$col else "#E3A6A0"
      out <- c(out, sprintf('<rect x="%d" y="%.1f" width="%d" height="%d" rx="15" fill="%s" stroke="%s" stroke-width="%s"/>', cc$x, y, cc$w, nh, fill, stroke, if (on) 2.5 else 1.2),
               sprintf('<text x="%d" y="%.1f" font-size="12.3" fill="%s" font-weight="%s"><tspan fill="%s" font-weight="800">%s</tspan>%s</text>',
                       cc$x + 12, y + 19.5, tc, if (on) "700" else "500", mc, mark, htmltools::htmlEscape(cc$nodes[[nm]])))
    }
  }
  # final test box
  tname <- if (!is.null(test) && known) TEST_INFO[[test]]$name else "Decided after the checks"
  lines <- wrap_lines(tname, 17)
  out <- c(out, sprintf('<text x="952" y="22" font-size="13" font-weight="700" fill="#D35454">5 · THE TEST</text>'),
           sprintf('<rect x="952" y="%d" width="142" height="%d" rx="16" fill="%s" stroke="%s" stroke-width="2.5"/>', 150 - 12 - 10 * length(lines), 24 + 20 * length(lines),
                   if (known) "#FCEDED" else "#FFFFFF", if (known) "#D35454" else "#DDE3E8"))
  for (j in seq_along(lines)) out <- c(out, sprintf('<text x="1023" y="%d" text-anchor="middle" font-size="14" font-weight="%s" fill="%s">%s</text>',
                                                    150 - 10 * length(lines) + 20 * (j - 1) + 6, if (known) "800" else "500", if (known) "#00365F" else "#7F8E99", htmltools::htmlEscape(lines[j])))
  out <- c(out, '</svg>')
  HTML(paste(out, collapse = "\n"))
}

# =============================================================================
#  USER INTERFACE
# =============================================================================
app_css <- "
  :root { --ink:#00365F; --text:#2C4A63; --muted:#6B7F8E; --line:#E3EAF0; --bg:#F6F8FB; }
  body { background: var(--bg); color: var(--text); }
  .shell { display: grid; grid-template-columns: 300px 1fr; min-height: 100vh; }
  .side { background: #FFFFFF; border-right: 1px solid var(--line); padding: 26px 20px; position: sticky; top: 0; align-self: start; height: 100vh; overflow-y: auto; }
  .container-fluid { padding: 0 !important; }
  .brand { display: flex; align-items: center; gap: 12px; margin-bottom: 6px; }
  .brand .logo { width: 42px; height: 42px; border-radius: 12px; background: conic-gradient(#1E7FB0 0 17%, #7B5EA7 0 34%, #138D90 0 50%, #C98A12 0 67%, #2E8B57 0 84%, #D35454 0); }
  .brand h1 { font-size: 21px; margin: 0; color: var(--ink); font-weight: 800; letter-spacing: -.01em; }
  .brand-sub { color: var(--muted); font-size: 13px; margin-bottom: 22px; }
  .stepper { position: relative; }
  .stepper a { display: flex; gap: 12px; text-decoration: none; padding: 10px 10px; border-radius: 14px; margin-bottom: 4px; align-items: flex-start; }
  .stepper a:hover { background: #F3F6F9; }
  .stepper a.cur { background: var(--pale); }
  .stepper .num { flex: 0 0 34px; height: 34px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-weight: 800; color: #fff; font-size: 15px; }
  .stepper .ttl { font-weight: 700; color: var(--ink); font-size: 14.5px; line-height: 1.2; }
  .stepper .st { font-size: 12.3px; color: var(--muted); margin-top: 2px; }
  .side-note { margin-top: 18px; background: #F6F8FB; border-radius: 14px; padding: 12px 14px; font-size: 12.5px; color: var(--muted); }
  .main { padding: 26px 34px 40px; max-width: 1350px; }
  .head { display: flex; gap: 18px; align-items: center; margin-bottom: 14px; }
  .head .big { width: 58px; height: 58px; border-radius: 18px; display: flex; align-items: center; justify-content: center; color: #fff; font-size: 26px; font-weight: 800; }
  .head .k { font-size: 12px; font-weight: 800; letter-spacing: .14em; text-transform: uppercase; }
  .head h2 { margin: 0; font-size: 28px; color: var(--ink); font-weight: 800; letter-spacing: -.01em; }
  .head .why { color: var(--muted); font-size: 15px; }
  .tasks { display: grid; grid-template-columns: repeat(4, 1fr); gap: 10px; margin: 6px 0 16px; }
  .task { background: #fff; border: 1px solid var(--line); border-radius: 14px; padding: 10px 12px; display: flex; gap: 10px; align-items: center; font-size: 13.5px; font-weight: 600; color: var(--ink); }
  .task .n { flex: 0 0 26px; height: 26px; border-radius: 50%; display: flex; align-items: center; justify-content: center; color: #fff; font-weight: 800; font-size: 13px; }
  .pathcard { background: #fff; border: 1px solid var(--line); border-radius: 18px; padding: 14px 18px 6px; margin-bottom: 16px; }
  .pathcard .lab { font-size: 12px; font-weight: 800; letter-spacing: .12em; color: var(--muted); text-transform: uppercase; display: flex; justify-content: space-between; }
  .dorow { display: grid; grid-template-columns: 1fr 1fr; gap: 12px; margin-bottom: 18px; }
  .dd { border-radius: 16px; padding: 12px 16px; }
  .dd h6 { font-weight: 800; letter-spacing: .1em; text-transform: uppercase; font-size: 12px; margin-bottom: 4px; }
  .dd ul { margin: 0; padding: 0; list-style: none; }
  .dd li { margin: 4px 0; padding-left: 26px; position: relative; font-size: 14px; }
  .dd.do { background: #E8F5EE; } .dd.do h6 { color: #2E8B57; } .dd.do li:before { content: '✓'; position: absolute; left: 4px; color: #2E8B57; font-weight: 900; }
  .dd.dont { background: #FCEDED; } .dd.dont h6 { color: #C0392B; } .dd.dont li:before { content: '✗'; position: absolute; left: 4px; color: #C0392B; font-weight: 900; }
  .card { border: 1px solid var(--line); box-shadow: 0 2px 10px rgba(0,54,95,.04); border-radius: 18px; overflow: visible; }
  .card-header { font-weight: 700; color: var(--ink); background: #fff; border-bottom: 1px solid var(--line); border-radius: 18px 18px 0 0 !important; font-size: 15px; }
  .btn-go { font-weight: 700; padding: 12px 26px; border-radius: 26px; border: none; color: #fff; font-size: 15.5px; box-shadow: 0 4px 12px rgba(0,0,0,.08); }
  .btn-go:hover { filter: brightness(1.05); color: #fff; }
  .btn-back { font-weight: 600; padding: 11px 20px; border-radius: 26px; background: #fff; border: 1px solid #C9D6E0; color: #4A6378; }
  .navrow { display: flex; justify-content: space-between; align-items: center; margin: 22px 0 10px; padding-top: 16px; border-top: 1px solid var(--line); }
  .qcard .shiny-input-container, .tcard .shiny-input-container { width: 100% !important; }
  .qcard .shiny-options-group { display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px; }
  .tcard .shiny-options-group { display: grid; grid-template-columns: 1fr; gap: 8px; }
  .qcard .radio, .tcard .radio { margin: 0; }
  .qcard .radio label, .tcard .radio label { display: block; border: 2px solid #E6E0F2; border-radius: 14px; padding: 12px 14px; background: #fff; cursor: pointer; font-weight: 600; color: var(--ink); height: 100%; }
  .qcard .radio label:has(input:checked), .tcard .radio label:has(input:checked) { border-color: #7B5EA7; background: #F2EDF9; }
  .verdict { border-radius: 16px; padding: 14px 18px; margin-bottom: 12px; }
  .verdict .badge-s { display: inline-block; padding: 5px 14px; border-radius: 14px; color: #fff; font-weight: 800; font-size: 13.5px; margin-right: 8px; }
  .mono { font-family: Consolas, Menlo, monospace; font-size: 13px; background: rgba(255,255,255,.8); padding: 4px 8px; border-radius: 6px; display: inline-block; margin: 6px 0; }
  .pill-row { display: flex; gap: 8px; flex-wrap: wrap; margin-bottom: 14px; align-items: center; }
  .pill-row .p { width: 32px; height: 32px; border-radius: 50%; display: inline-flex; align-items: center; justify-content: center; color: #fff; font-weight: 800; font-size: 13px; }
  .rec { background: #E8F5EE; border-radius: 16px; padding: 16px 20px; }
  .rec .t { font-size: 24px; color: #2E8B57; font-weight: 800; }
  .kpi { border-radius: 16px; padding: 16px; text-align: center; }
  .kpi .v { font-size: 30px; color: var(--ink); font-weight: 800; }
  .kpi .l { font-size: 12.5px; color: #4A6378; }
  .sentence { background: #FCEDED; border-radius: 16px; padding: 18px 22px; font-size: 17px; color: var(--ink); line-height: 1.55; }
  .code { background: #F3F6F8; border-radius: 12px; padding: 12px 14px; font-family: Consolas, Menlo, monospace; font-size: 13px; white-space: pre-wrap; }
  .story { border-radius: 16px; padding: 14px 18px; background: #EAF4FA; color: var(--ink); }
  .warnbox { background: #FDF4E1; border-radius: 14px; padding: 12px 16px; color: #7A5507; font-weight: 600; margin-top: 12px; }
  .muted { color: var(--muted); font-size: 13px; }
  table.table { font-size: 13.5px; } table.table thead th { background: #EEF2F5; color: var(--ink); }
"

head_block <- function(id, n, why) {
  c0 <- STEP_COL[[id]]
  tagList(
    div(class = "head", div(class = "big", style = paste0("background:", c0["main"]), n),
        div(div(class = "k", style = paste0("color:", c0["main"]), sprintf("Step %d of 6", n)), h2(c0["name"]), div(class = "why", why))),
    div(class = "tasks", lapply(seq_along(TASKS[[id]]), function(i)
      div(class = "task", span(class = "n", style = paste0("background:", c0["main"]), i), TASKS[[id]][i]))))
}
dodont <- function(id) {
  d <- DODONT[[id]]
  div(class = "dorow", div(class = "dd do", h6("Do"), tags$ul(lapply(d$do, tags$li))), div(class = "dd dont", h6("Avoid"), tags$ul(lapply(d$dont, tags$li))))
}
pathcard <- function(id) div(class = "pathcard", div(class = "lab", span("Your path through the decision"), span(style = "text-transform:none; letter-spacing:0; font-weight:600", "coloured ✓ = your route · grey ✗ = not this route")), uiOutput(id))
go_btn <- function(id, label, step) actionButton(id, label, class = "btn-go", style = paste0("background:", STEP_COL[[step]]["main"]))
back_btn <- function(id, label = "← Back") actionButton(id, label, class = "btn-back")

ui <- page_fluid(
  theme = bs_theme(version = 5, primary = "#1E7FB0", fg = "#2C4A63", bg = "#FFFFFF",
                   base_font = font_google("Inter", local = FALSE), heading_font = font_google("Inter", local = FALSE), "font-size-base" = "0.95rem"),
  tags$head(tags$style(HTML(app_css))),
  div(class = "shell",
    div(class = "side",
        div(class = "brand", div(class = "logo"), h1("Test Navigator")),
        div(class = "brand-sub", "Choose, check, run and report a statistical test — one step at a time."),
        uiOutput("stepper"),
        div(class = "side-note", HTML("<b>How to use</b><br>Work down the steps. Each step tells you what to do now, what to avoid, and shows your path through the decision. Buttons at the bottom move you on."))),
    div(class = "main",
      navset_hidden(id = "wiz",
        nav_panel_hidden("s1",
          head_block("s1", 1, "Know your data before anything else."), dodont("s1"),
          layout_columns(col_widths = c(4, 8),
            card(card_header("Dataset"),
                 selectInput("dataset", NULL, choices = c(setNames(names(DATASETS), sapply(DATASETS, `[[`, "title")), "Upload your own CSV file" = "upload"), width = "100%"),
                 conditionalPanel("input.dataset == 'upload'", fileInput("file", NULL, accept = ".csv"), div(class = "muted", "CSV with a header row; one row per person.")),
                 uiOutput("story"),
                 downloadButton("dl", "Download this dataset (CSV)", class = "btn-outline-primary btn-sm")),
            card(card_header("Variables and their kind"), tableOutput("vars"), card_header("First rows"), div(style = "overflow-x:auto", tableOutput("head")))),
          div(class = "navrow", span(), go_btn("to2", "Use this dataset  →", "s1"))),
        nav_panel_hidden("s2",
          head_block("s2", 2, "The design of the study decides the question — not the results."), pathcard("path2"), dodont("s2"),
          card(card_header("Ready-made question (optional)"), selectInput("example", NULL, choices = NULL, width = "100%")),
          card(card_header("Which question?"),
               div(class = "qcard", radioButtons("question", NULL, choiceNames = lapply(names(QUESTIONS), function(n) span(n)), choiceValues = unname(QUESTIONS), selected = "two")),
               layout_columns(col_widths = c(3, 9), plotOutput("qpic", height = 110), uiOutput("qhelp"))),
          layout_columns(col_widths = c(6, 6),
            card(card_header("Which variables?"),
                 conditionalPanel("input.question == 'one' || input.question == 'two' || input.question == 'k'", selectInput("y", "Outcome (what was measured)", choices = NULL, width = "100%")),
                 conditionalPanel("input.question == 'two' || input.question == 'k'", selectInput("g", "Groups (who is compared)", choices = NULL, width = "100%")),
                 conditionalPanel("input.question == 'paired'", selectInput("a", "First measurement", choices = NULL, width = "100%"), selectInput("b", "Second measurement", choices = NULL, width = "100%")),
                 conditionalPanel("input.question == 'repeated'", selectizeInput("reps", "Measurements in time order (3 or more)", choices = NULL, multiple = TRUE, width = "100%")),
                 conditionalPanel("input.question == 'corr'", selectInput("cx", "First variable", choices = NULL, width = "100%"), selectInput("cy", "Second variable", choices = NULL, width = "100%")),
                 conditionalPanel("input.question == 'one' && input.type != 'cat'", numericInput("mu", "Value to compare with", value = 0)),
                 conditionalPanel("input.question == 'one' && input.type == 'cat'", numericInput("p0", "Proportion to compare with (0 to 1)", value = 0.5, min = 0, max = 1, step = 0.05))),
            card(card_header("What kind of outcome is it?"),
                 div(class = "tcard", radioButtons("type", NULL, choices = TYPES, selected = "num")),
                 div(class = "muted", "Set from the variable automatically. Change it if, for example, a 1–5 score is really an ordered score."))),
          uiOutput("submit_msg"),
          div(class = "navrow", back_btn("back1"), go_btn("submit", "Submit my question  →", "s2"))),
        nav_panel_hidden("s3",
          head_block("s3", 3, "Look at every person before any test. The picture decides more than any p-value."), pathcard("path3"), dodont("s3"),
          layout_columns(col_widths = c(6, 6),
            card(card_header("Each person on one picture"), plotOutput("p_look", height = 360)),
            card(card_header("Summary numbers"), tableOutput("desc"),
                 div(class = "muted", "Mean and SD describe a bell shape. Median and quartiles (a quarter and three-quarters of the way up the sorted values) describe any shape. Skew near 0 = symmetric; above 1 = long right tail."))),
          layout_columns(col_widths = c(6, 6),
            card(card_header("Is the shape a bell? (Q–Q plot)"), plotOutput("p_qq", height = 300)),
            card(card_header("Cumulative distribution (ECDF)"), plotOutput("p_ecdf", height = 300))),
          div(class = "navrow", back_btn("back2"), go_btn("to4", "Check the assumptions  →", "s3"))),
        nav_panel_hidden("s4",
          head_block("s4", 4, "One assumption at a time: what it means, what met and not met look like, and what your data show."), pathcard("path4"), dodont("s4"),
          uiOutput("pills"),
          layout_columns(col_widths = c(5, 7),
            div(uiOutput("a_text"), card(card_header("What met and not met look like"), plotOutput("a_illus", height = 230))),
            div(uiOutput("a_verdict"), card(card_header("Your data"), plotOutput("a_data", height = 320)))),
          div(class = "navrow", div(back_btn("back3"), " ", actionButton("aprev", "◀  Previous check", class = "btn-back")),
              div(actionButton("anext", "Next check  ▶", class = "btn-go", style = "background:#C98A12"), " ", uiOutput("to5_ui", inline = TRUE)))),
        nav_panel_hidden("s5",
          head_block("s5", 5, "Run the test the checks chose. Other tests are shown only to learn from."), pathcard("path5"), dodont("s5"),
          layout_columns(col_widths = c(5, 7),
            card(card_header("Choose the test, then press Run"), selectInput("test", NULL, choices = NULL, width = "100%"), uiOutput("testnote"),
                 go_btn("run", "Run the test  ▶", "s5")),
            card(card_header("What is being tested"), uiOutput("hyp"))),
          uiOutput("result_ui"),
          div(class = "navrow", back_btn("back4"), go_btn("to6", "Write the report  →", "s5"))),
        nav_panel_hidden("s6",
          head_block("s6", 6, "One complete sentence, and the code so anyone can repeat it."), pathcard("path6"), dodont("s6"),
          layout_columns(col_widths = c(7, 5),
            div(card(card_header("Your result in one sentence"), uiOutput("sentence")), card(card_header("The same analysis in R"), uiOutput("rcode"))),
            card(card_header("Which test? The whole table"), tableOutput("grid"))),
          card(card_header("Glossary"), tableOutput("gloss")),
          div(class = "navrow", back_btn("back5"), go_btn("restart", "Start a new question  ↺", "s6")))
      )))
)
# =============================================================================
#  SERVER
# =============================================================================
guess_types <- function(d) {
  sapply(names(d), function(v) {
    x <- d[[v]]
    if (is.numeric(x)) { u <- length(unique(na.omit(x))); if (u <= 2) "cat" else if (u <= 7 && all(x == round(x), na.rm = TRUE)) "ord" else "num" }
    else if (length(unique(x)) == length(x) && length(x) > 10) "id" else "cat"
  })
}
STATUS <- list(ok = c("✓", "#2E8B57", "#E8F5EE", "Met"), warn = c("!", "#C98A12", "#FDF4E1", "Take care"),
               fail = c("✗", "#C0392B", "#FCEDED", "Not met"), info = c("i", "#1E7FB0", "#EAF4FA", "Decide from the design"))

server <- function(input, output, session) {
  rv <- reactiveValues(step = "s1", spec = NULL, ai = 1, result = NULL, submit_msg = NULL, checked = FALSE)
  goto <- function(s) { rv$step <- s; nav_select("wiz", s) }

  output$stepper <- renderUI({
    st <- c(s1 = { m <- tryCatch(meta(), error = function(e) NULL); if (is.null(m)) "Choose a dataset" else sprintf("%s · %d people", m$title, nrow(m$data)) },
            s2 = if (is.null(rv$spec)) "Not submitted yet" else names(QUESTIONS)[QUESTIONS == rv$spec$q],
            s3 = if (is.null(rv$spec)) "After the question" else "Plots and summary numbers",
            s4 = if (is.null(rv$spec)) "After the question" else { ch <- tryCatch(checks(), error = function(e) list()); sprintf("%d checks · %d not met%s", length(ch), sum(sapply(ch, `[[`, "status") == "fail"), if (rv$checked) " · done" else "") },
            s5 = if (!is.null(rv$result)) sprintf("%s · p = %s", rv$result$test, fmt_p(rv$result$p)) else if (rv$checked) "Ready to run" else "After the checks",
            s6 = if (!is.null(rv$result)) "Sentence ready" else "After the test")
    done <- c(s1 = rv$step != "s1" || !is.null(rv$spec), s2 = !is.null(rv$spec), s3 = rv$checked || rv$ai > 1, s4 = rv$checked, s5 = !is.null(rv$result), s6 = FALSE)
    div(class = "stepper", lapply(1:6, function(i) {
      id <- paste0("s", i); c0 <- STEP_COL[[id]]; cur <- rv$step == id
      bg <- if (cur) c0["main"] else if (done[[id]]) "#2E8B57" else "#C9D6E0"
      actionLink(paste0("go", i), class = if (cur) "cur" else "", style = paste0("--pale:", c0["pale"]),
                 label = tagList(span(class = "num", style = paste0("background:", bg), if (done[[id]] && !cur) "✓" else i),
                                 div(div(class = "ttl", style = if (cur) paste0("color:", c0["main"]) else "", c0["name"]), div(class = "st", st[[id]]))))
    }))
  })
  path_now <- function(live = FALSE) {
    if (live || is.null(rv$spec)) return(path_svg(type = input$type, q = input$question))
    tst <- if (!is.null(rv$result)) rv$result$code else tryCatch(assessed()$rec, error = function(e) NULL)
    path_svg(type = rv$spec$type, q = rv$spec$q, test = tst, known = rv$checked || !is.null(rv$result))
  }
  output$path2 <- renderUI(path_now(live = TRUE))
  output$path3 <- renderUI(path_now()); output$path4 <- renderUI(path_now()); output$path5 <- renderUI(path_now()); output$path6 <- renderUI(path_now())
  lapply(1:6, function(i) observeEvent(input[[paste0("go", i)]], {
    if (i >= 3 && is.null(rv$spec)) { goto("s2"); return() }
    if (i >= 5 && !rv$checked && is.null(rv$result)) { showNotification("Go through every assumption check first.", type = "warning"); goto("s4"); return() }
    if (i == 6 && is.null(rv$result)) { goto("s5"); return() }
    goto(paste0("s", i)) }))
  observeEvent(input$to2, goto("s2")); observeEvent(input$back1, goto("s1")); observeEvent(input$back2, goto("s2"))
  observeEvent(input$to4, { rv$ai <- 1; goto("s4") }); observeEvent(input$back3, goto("s3")); observeEvent(input$back4, goto("s4"))
  observeEvent(input$back5, goto("s5")); observeEvent(input$to6, { if (is.null(rv$result)) showNotification("Press 'Run the test' first.", type = "warning") else goto("s6") })
  observeEvent(input$restart, { rv$spec <- NULL; rv$result <- NULL; rv$checked <- FALSE; goto("s2") })

  # ---- data
  meta <- reactive({
    if (input$dataset != "upload") return(DATASETS[[input$dataset]])
    req(input$file)
    d <- tryCatch(read.csv(input$file$datapath, stringsAsFactors = TRUE, check.names = TRUE), error = function(e) NULL)
    validate(need(!is.null(d) && ncol(d) >= 2, "Could not read the file. Use a CSV file with a header row and at least two columns."))
    list(title = input$file$name, story = "Your own data. Check that each variable's kind is right.", data = d, types = guess_types(d), labels = setNames(names(d), names(d)))
  })
  observeEvent(meta(), {
    m <- meta(); tp <- m$types; nm <- names(tp)
    lab <- setNames(nm, paste0(m$labels[nm], "  [", nm, "]"))
    keep <- function(ok) lab[tp[lab] %in% ok]
    grp <- keep(c("cat", "ord")); grp <- grp[sapply(grp, function(v) length(unique(na.omit(m$data[[v]]))) <= 12)]
    ych <- keep(c("num", "ord", "cat")); ynum <- keep(c("num", "ord"))
    updateSelectInput(session, "y", choices = ych, selected = if (length(ynum)) ynum[1] else ych[1])
    updateSelectInput(session, "g", choices = grp)
    updateSelectInput(session, "a", choices = ych); updateSelectInput(session, "b", choices = ych, selected = ych[min(2, length(ych))])
    updateSelectizeInput(session, "reps", choices = ynum, selected = character(0))
    updateSelectInput(session, "cx", choices = ynum); updateSelectInput(session, "cy", choices = ynum, selected = ynum[min(2, length(ynum))])
    ex <- if (input$dataset != "upload") EXAMPLES[[input$dataset]] else list()
    updateSelectInput(session, "example", choices = c("— build your own question below —" = "", setNames(names(ex), names(ex))), selected = if (length(ex)) names(ex)[1] else "")
    rv$spec <- NULL; rv$result <- NULL
  })
  observeEvent(input$example, {
    req(input$example != "", input$dataset != "upload")
    e <- EXAMPLES[[input$dataset]][[input$example]]; req(e)
    updateRadioButtons(session, "question", selected = e$q)
    for (v in c("y", "g", "a", "b", "cx", "cy")) if (!is.null(e[[v]])) updateSelectInput(session, v, selected = e[[v]])
    if (!is.null(e$reps)) updateSelectizeInput(session, "reps", selected = e$reps)
    if (!is.null(e$mu)) updateNumericInput(session, "mu", value = e$mu)
    if (!is.null(e$p0)) updateNumericInput(session, "p0", value = e$p0)
  })
  observe({
    m <- meta(); q <- input$question
    v <- switch(q, one = input$y, two = input$y, k = input$y, paired = input$a, repeated = input$reps[1], corr = input$cy)
    req(v, v %in% names(m$types)); t0 <- m$types[[v]]; if (t0 == "id") t0 <- "cat"
    if (q %in% c("repeated", "corr") && t0 == "cat") t0 <- "num"
    updateRadioButtons(session, "type", selected = t0)
  })

  output$story <- renderUI({ m <- meta(); div(class = "story", style = "margin:10px 0", h5(m$title), p(m$story),
                                               div(class = "muted", sprintf("%d people · %d variables", nrow(m$data), ncol(m$data)))) })
  output$vars <- renderTable({
    m <- meta(); nm <- names(m$types)
    kind <- c(num = "Measurement or count", ord = "Ordered score", cat = "Category", id = "Identifier")[m$types]
    ex <- sapply(nm, function(v) { x <- m$data[[v]]; if (is.numeric(x)) paste0(f2(min(x, na.rm = TRUE), 0), " to ", f2(max(x, na.rm = TRUE), 0)) else paste(head(levels(factor(x)), 4), collapse = ", ") })
    data.frame(Variable = nm, Meaning = unname(m$labels[nm]), Kind = unname(kind), Values = unname(ex))
  }, spacing = "s")
  output$head <- renderTable({ h <- head(meta()$data, 8); for (v in names(h)) if (is.numeric(h[[v]])) h[[v]] <- format(h[[v]], digits = 4, drop0trailing = TRUE); h }, spacing = "xs")
  output$dl <- downloadHandler(filename = function() paste0(if (input$dataset == "upload") "my_data" else input$dataset, ".csv"),
                               content = function(f) write.csv(meta()$data, f, row.names = FALSE))

  # ---- step 2
  output$qpic <- renderPlot(question_pic(input$question), bg = "transparent")
  output$qhelp <- renderUI(div(style = "padding-top:18px; font-size:16px; color:#00365F", QUESTION_HELP[[input$question]]))

  current_spec <- function() {
    m <- meta(); L <- m$labels; q <- input$question
    s <- list(q = q, type = input$type, y = input$y, g = input$g, a = input$a, b = input$b, reps = input$reps, cx = input$cx, cy = input$cy,
              mu = input$mu %||% 0, p0 = input$p0 %||% 0.5)
    lb <- function(v) if (!is.null(v) && v %in% names(L)) unname(L[[v]]) else v
    s$ylab <- if (q %in% c("one", "two", "k")) lb(input$y) else if (q == "paired") trimws(gsub("\\s+", " ", gsub("\\sat [^(]*|\\b(before|after)\\b", " ", lb(input$a)))) else "the outcome"
    s$glab <- lb(input$g); s$alab <- lb(input$a); s$blab <- lb(input$b); s$cxlab <- lb(input$cx); s$cylab <- lb(input$cy)
    s$replabs <- if (length(input$reps)) unname(sapply(input$reps, lb)) else NULL
    s$ordered_groups <- !is.null(input$g) && input$g %in% names(m$data) && (is.ordered(m$data[[input$g]]) || identical(m$types[[input$g]], "ord"))
    s
  }
  spec_problem <- function(s) {
    m <- meta(); q <- s$q
    need_vars <- switch(q, one = s$y, two = c(s$y, s$g), k = c(s$y, s$g), paired = c(s$a, s$b), repeated = s$reps, corr = c(s$cx, s$cy))
    if (length(need_vars) == 0 || any(!nzchar(need_vars)) || any(!need_vars %in% names(m$data))) return("Choose the variables for your question.")
    if (q == "paired" && s$a == s$b) return("Choose two different measurements.")
    if (q == "corr" && s$cx == s$cy) return("Choose two different variables.")
    if (q %in% c("two", "k") && s$y == s$g) return("The outcome and the groups must be different variables.")
    if (q %in% c("repeated", "corr") && s$type == "cat") return("This question needs numbers or ordered scores, not categories. Change the kind of outcome.")
    if (q == "repeated" && length(s$reps) < 3) return("Choose three or more measurements, in time order.")
    fr <- tryCatch(build_frame(m$data, s), error = function(e) NULL)
    if (is.null(fr)) return("These variables cannot be analysed together. Check the kind of outcome.")
    frame_problem(fr, s)
  }
  observeEvent(input$submit, {
    s <- current_spec(); p <- spec_problem(s)
    if (!is.null(p)) { rv$submit_msg <- p; return() }
    rv$submit_msg <- NULL; rv$spec <- s; rv$result <- NULL; rv$ai <- 1; rv$checked <- FALSE; goto("s3")
  })
  output$submit_msg <- renderUI({ if (!is.null(rv$submit_msg)) div(class = "warnbox", "Before going on: ", rv$submit_msg) })

  frame <- reactive({ req(rv$spec); build_frame(meta()$data, rv$spec) })
  assessed <- reactive(assess(frame(), rv$spec))
  cands <- reactive(candidates(rv$spec, frame()))

  # ---- step 3
  output$p_look <- renderPlot({ req(rv$spec); plot_look(frame(), rv$spec) }, res = 96)
  output$p_qq <- renderPlot({ req(rv$spec); g <- plot_shape(frame(), rv$spec); validate(need(!is.null(g), "Shape plots are for measurements and scores, not categories.")); g }, res = 96)
  output$p_ecdf <- renderPlot({ req(rv$spec); g <- plot_ecdf(frame(), rv$spec); validate(need(!is.null(g), "Not used for this question.")); g }, res = 96)
  output$desc <- renderTable({ req(rv$spec); describe(frame(), rv$spec) }, spacing = "s")

  # ---- step 4: one check at a time
  checks <- reactive(assessed()$checks)
  observeEvent(input$anext, { n <- length(checks()); rv$ai <- min(n + 1, rv$ai + 1); if (rv$ai > n) rv$checked <- TRUE })
  observeEvent(input$aprev, { rv$ai <- max(1, rv$ai - 1) })
  output$pills <- renderUI({
    req(rv$spec); ch <- checks()
    div(class = "pill-row", span(style = "color:#7A5507; font-weight:800; padding-right:4px", "CHECKS"),
        lapply(seq_along(ch), function(i) { s <- STATUS[[ch[[i]]$status]]
          span(class = "p", title = ch[[i]]$name, style = paste0("background:", if (i <= rv$ai) s[2] else "#C9D6E0", if (i == rv$ai) "; box-shadow: 0 0 0 3px #FDF4E1, 0 0 0 5px #C98A12" else ""), if (i <= rv$ai) s[1] else i) }),
        span(style = paste0("padding:6px 14px; border-radius:16px; color:#fff; font-weight:800; font-size:13px; background:", if (rv$ai > length(ch)) "#2E8B57" else "#C9D6E0"), "Suggested test"))
  })
  cur_check <- reactive({ ch <- checks(); if (rv$ai <= length(ch)) ch[[rv$ai]] else NULL })
  output$a_text <- renderUI({
    req(rv$spec); c <- cur_check(); ch <- checks()
    if (is.null(c)) {
      a <- assessed()
      return(tagList(div(class = "rec", div(class = "muted", style = "color:#2E8B57; font-weight:800; letter-spacing:.1em", "ALL CHECKS DONE · SUGGESTED TEST"),
                         div(class = "t", TEST_INFO[[a$rec]]$name), div(a$reason)),
                     br(), card(card_header("Summary of the checks"),
                                tags$table(class = "table", tags$tbody(lapply(ch, function(x) { s <- STATUS[[x$status]]
                                  tags$tr(tags$td(span(style = paste0("color:", s[2], "; font-weight:800"), s[1])), tags$td(x$name), tags$td(span(style = paste0("color:", s[2]), s[4]))) }))))))
    }
    k <- check_key(c$name)
    card(card_header(sprintf("Check %d of %d", rv$ai, length(ch))),
         h4(c$name, style = "color:#00365F; font-family:Georgia,serif"),
         div(style = "font-size:15px; margin-bottom:8px", tags$b("The question: "), c$what),
         div(class = "dd do", style = "margin-bottom:8px", h6("Why it matters"), div(c$why)),
         if (!is.null(IFNOT[k]) && !is.na(IFNOT[k])) div(class = "dd dont", h6("If it is not met"), div(IFNOT[[k]])))
  })
  output$a_illus <- renderPlot({
    req(rv$spec); c <- cur_check()
    if (is.null(c)) return(plot_look(frame(), rv$spec))
    g <- illus(check_key(c$name))
    validate(need(!is.null(g), "This check is decided from the study design; there is nothing to plot."))
    g
  }, res = 96)
  output$a_verdict <- renderUI({
    req(rv$spec); c <- cur_check()
    if (is.null(c)) return(div(class = "verdict", style = "background:#E8F5EE", div(class = "badge-s", style = "background:#2E8B57", "Ready"),
                               div(style = "margin-top:8px", "Every check has been looked at. Go on to run the suggested test.")))
    s <- STATUS[[c$status]]
    div(class = "verdict", style = paste0("background:", s[3]),
        div(class = "badge-s", style = paste0("background:", s[2]), paste(s[1], s[4])),
        div(class = "mono", c$result))
  })
  output$a_data <- renderPlot({ req(rv$spec); c <- cur_check(); if (is.null(c)) return(plot_look(frame(), rv$spec))
    check_plot(check_key(c$name), check_group(c$name), frame(), rv$spec) }, res = 96)
  output$to5_ui <- renderUI({ req(rv$spec); if (rv$ai > length(checks())) go_btn("to5", "Go to the test →", "s4") else span(class = "muted", "  Go through every check to continue.") })
  observeEvent(input$to5, goto("s5"))

  # ---- step 5
  observe({
    req(rv$spec); cc <- cands(); rec <- assessed()$rec
    lab <- setNames(cc, sapply(cc, function(k) paste0(TEST_INFO[[k]]$name, "  ·  ", TEST_INFO[[k]]$kind, if (identical(k, rec)) "  ·  suggested" else "")))
    updateSelectInput(session, "test", choices = lab, selected = rec %||% cc[1])
  })
  output$recbox <- renderUI({ req(rv$spec); a <- assessed()
    div(class = "rec", style = "margin-bottom:14px", div(class = "muted", style = "color:#2E8B57; font-weight:800; letter-spacing:.1em", "THE CHECKS SUGGEST"), div(class = "t", TEST_INFO[[a$rec]]$name), div(a$reason)) })
  output$testnote <- renderUI({ req(rv$spec, input$test %in% cands()); rec <- assessed()$rec
    div(class = "muted", style = "margin-bottom:10px", if (identical(input$test, rec)) "This is the suggested test." else HTML(sprintf("The checks suggested <b>%s</b>. Running this one is for comparison.", TEST_INFO[[rec]]$name))) })
  observeEvent(input$run, { req(rv$spec, input$test %in% cands()); r <- run_test(input$test, frame(), rv$spec); r$code <- input$test; rv$result <- r })
  observeEvent(input$test, rv$result <- NULL)
  output$hyp <- renderUI({
    req(rv$spec, input$test %in% cands()); r <- run_test(input$test, frame(), rv$spec)
    tagList(div(class = "dd do", style = "margin-bottom:8px; background:#F5FAFD", HTML(paste0("<b>Null hypothesis (H₀):</b> ", r$h0))),
            div(class = "dd do", style = "margin-bottom:8px; background:#F5FAFD", HTML(paste0("<b>Alternative (H₁):</b> ", r$h1))),
            div(class = "dd do", style = "background:#F5FAFD", HTML(paste0("<b>How the statistic is built:</b> ", r$formula))))
  })
  output$result_ui <- renderUI({
    r <- rv$result
    if (is.null(r)) return(div(class = "warnbox", style = "background:#E8F5EE; color:#2E8B57", "Press “Run the test ▶” to see the result."))
    kp <- function(v, l, bg) div(class = "kpi", style = paste0("background:", bg), div(class = "v", v), div(class = "l", l))
    plain <- if (r$p < 0.05) sprintf("If there were truly no difference, a result at least this extreme would happen in fewer than %s in 100 studies. That is unusual, so the data give evidence against H₀.", max(1, ceiling(r$p * 100)))
             else sprintf("A result like this would happen in about %s in 100 studies even with no real difference, so the data do not give clear evidence against H₀. This is not proof of no difference.", round(r$p * 100))
    tagList(
      layout_columns(col_widths = c(4, 4, 4),
        kp(if (is.na(r$stat)) "exact" else f2(as.numeric(r$stat)), paste("Test statistic:", r$stat_lab), "#EAF4FA"),
        kp(fmt_p(r$p), "p-value", if (r$p < 0.05) "#E8F5EE" else "#FDF4E1"),
        kp(if (is.null(r$eff)) "–" else f2(as.numeric(r$eff)), paste(r$eff_lab %||% "Effect size", if (!is.null(r$eff_word)) paste0("· ", r$eff_word) else ""), "#F2EDF9")),
      br(),
      layout_columns(col_widths = c(6, 6),
        card(card_header("What the result means, in plain words"), div(style = "font-size:15px", plain),
             if (!is.null(r$est)) div(style = "margin-top:10px", tags$b(r$est_lab, ": "), f2(as.numeric(r$est)), if (!is.null(r$ci)) sprintf(" (95%% confidence interval %s to %s)", f2(r$ci[1]), f2(r$ci[2]))),
             if (!is.null(r$note)) div(class = "muted", style = "margin-top:10px", r$note)),
        card(card_header("Extra detail"), if (!is.null(r$extra)) tableOutput("extra") else div(class = "muted", "No further detail for this test."))),
      card(card_header("Every possible test for this question, side by side (for learning)"), tableOutput("compare"),
           div(class = "muted", "In a real analysis choose one test before seeing any p-value. Picking whichever gives the smaller p raises false alarms.")))
  })
  output$extra <- renderTable({ req(rv$result); rv$result$extra }, spacing = "s")
  output$compare <- renderTable({
    req(rv$spec); cc <- cands(); rec <- assessed()$rec
    do.call(rbind, lapply(cc, function(k) {
      r <- tryCatch(run_test(k, frame(), rv$spec), error = function(e) NULL); if (is.null(r)) return(NULL)
      data.frame(Test = TEST_INFO[[k]]$name, Kind = TEST_INFO[[k]]$kind, Statistic = if (is.na(r$stat)) "exact" else paste(r$stat_lab, "=", f2(as.numeric(r$stat))),
                 `p-value` = fmt_p(r$p), `Effect size` = if (is.null(r$eff)) "–" else paste(r$eff_lab, "=", f2(as.numeric(r$eff))), Suggested = if (identical(k, rec)) "✓" else "", check.names = FALSE)
    }))
  }, spacing = "s")

  # ---- step 6
  output$sentence <- renderUI({ req(rv$result); div(class = "sentence", paste0("“", rv$result$sentence, "”")) })
  output$rcode <- renderUI({
    req(rv$result); s <- rv$spec
    file <- if (input$dataset == "upload") "my_data.csv" else paste0(input$dataset, ".csv")
    code <- paste0("d <- read.csv(\"", file, "\", stringsAsFactors = TRUE)\n", rv$result$rcode)
    div(class = "code", code)
  })
  output$grid <- renderTable(data.frame(
    Question = c("One group vs a value", "Two separate groups", "Same people, twice", "Three or more groups", "Same people, 3+ times", "Two numbers together"),
    `Bell-shaped measurement` = c("One-sample t", "t-test (Welch)", "Paired t", "ANOVA (Welch)", "Repeated-measures ANOVA", "Pearson"),
    `Skewed or ordered score` = c("Wilcoxon / sign", "Mann–Whitney", "Wilcoxon / sign", "Kruskal–Wallis", "Friedman", "Spearman / Kendall"),
    Category = c("Binomial / goodness of fit", "Chi-square / Fisher", "McNemar", "Chi-square / trend", "—", "Chi-square / Fisher"), check.names = FALSE), spacing = "s")
  output$gloss <- renderTable(data.frame(Term = c("Parametric test", "Non-parametric (rank) test", "Bell-shaped (normal)", "Skew", "Outlier", "Q–Q plot", "Shapiro–Wilk test", "Levene test",
                                                  "Rank", "Median", "Quartiles", "IQR", "Expected count", "p-value", "95% confidence interval", "Effect size", "Paired data", "Sphericity", "Post hoc test"),
    Meaning = c("Assumes a shape for the data (usually the bell curve) and compares means.", "Uses the order of the values, not the values; no bell shape needed.",
                "Most values near the middle, fewer further out, evenly on both sides.", "How lopsided the values are; 0 is symmetric.", "A value far from the rest (beyond 1.5 × IQR from the quartiles).",
                "Observed values against those expected if bell-shaped; a straight line means a bell shape.", "Tests whether data could come from a bell shape; small p suggests not.",
                "Tests whether groups have the same spread; small p suggests not.", "Position of a value when all values are sorted (1 = smallest).", "The middle value when sorted.",
                "Values a quarter and three-quarters of the way up the sorted list.", "Q3 − Q1: the spread of the middle half.", "Row total × column total ÷ grand total: the count expected if unrelated.",
                "Probability of a result at least this extreme if the null hypothesis were true.", "Range of values for the true effect compatible with the data.",
                "How big the difference is, separate from p.", "Two measurements that belong together (same person, or matched).", "Equal variance of the differences between every pair of time points.",
                "Pairwise comparisons after an overall test, adjusted for multiple testing.")), spacing = "s")
}

shinyApp(ui, server)
