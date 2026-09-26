# =============================================================================
#  Session 2 - Frequency Distributions and Displaying Data
#  Interactive classroom demonstration (MPH - Fundamentals of Statistics)
#  Community Hypertension Control Programme, n = 40
#
#  Run locally:   shiny::runApp()      (from the folder containing app.R)
#  Packages:      shiny, bslib, ggplot2, readxl, writexl
#  The class dataset is built in. It can be downloaded as Excel (or CSV) from the
#  sidebar, edited or kept as is, and uploaded back (.xlsx, .xls or .csv); every
#  tab then runs on the uploaded data.
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(readxl)
library(writexl)

# -----------------------------------------------------------------------------
# 1. Class dataset (built in, so the app runs with no external files)
# -----------------------------------------------------------------------------
hyp_builtin <- read.csv(text = trimws('
ID,Sex,Arm,Education,Smoker,Age,BMI,SBP_Base,SBP_6mo,Adherence,Visits,BP_Controlled,Responded
1,F,Usual care,No formal,No,54,26.4,159,160,3,2,No,No
2,M,Usual care,Primary,Yes,61,25.5,150,150,1,3,No,No
3,F,Usual care,Primary,No,47,22.4,148,139,4,1,Yes,No
4,F,Usual care,Secondary,No,58,27.3,145,146,2,3,No,No
5,M,Usual care,No formal,Yes,66,34.6,174,179,2,1,No,No
6,F,Usual care,Secondary,No,52,23.0,141,147,3,1,No,No
7,M,Usual care,Tertiary,Yes,44,24.8,140,139,4,7,Yes,No
8,F,Usual care,Primary,No,63,30.1,165,160,1,2,No,No
9,M,Usual care,Secondary,Yes,49,24.8,152,168,1,2,No,No
10,F,Usual care,No formal,No,57,29.1,149,156,1,2,No,No
11,F,Usual care,Primary,No,71,29.8,167,163,3,12,No,No
12,M,Usual care,Secondary,Yes,45,21.3,141,150,4,1,No,No
13,F,Usual care,Tertiary,No,50,24.1,147,159,3,3,No,No
14,M,Usual care,Primary,No,59,30.4,151,151,4,3,No,No
15,F,Usual care,No formal,No,68,30.5,174,171,4,4,No,No
16,M,Usual care,Secondary,Yes,42,29.6,148,142,2,4,No,No
17,F,Usual care,Primary,No,55,29.8,163,154,3,4,No,No
18,F,Usual care,Tertiary,No,48,29.2,145,147,1,5,No,No
19,M,Usual care,Secondary,Yes,64,25.2,153,138,3,3,Yes,Yes
20,F,Usual care,Primary,No,53,27.3,171,169,3,2,No,No
21,M,Intervention,No formal,Yes,60,27.0,160,146,5,9,No,Yes
22,F,Intervention,Primary,No,46,30.2,157,134,5,5,Yes,Yes
23,F,Intervention,Secondary,No,56,24.0,153,155,4,6,No,No
24,M,Intervention,Primary,Yes,51,28.0,152,141,5,9,No,Yes
25,F,Intervention,Tertiary,No,43,26.6,140,131,4,4,Yes,No
26,F,Intervention,No formal,No,82,27.2,159,148,4,15,No,Yes
27,M,Intervention,Secondary,Yes,62,26.2,149,131,5,5,Yes,Yes
28,F,Intervention,Primary,No,49,25.7,160,145,5,2,No,Yes
29,M,Intervention,Secondary,No,57,26.3,149,125,5,2,Yes,Yes
30,F,Intervention,Primary,No,44,24.3,157,143,5,5,No,Yes
31,M,Intervention,No formal,Yes,65,22.4,165,151,5,7,No,Yes
32,F,Intervention,Tertiary,No,41,27.6,140,131,4,8,Yes,No
33,F,Intervention,Primary,No,53,31.2,163,152,5,4,No,Yes
34,M,Intervention,Secondary,Yes,59,27.4,152,138,5,4,Yes,Yes
35,F,Intervention,Primary,No,47,21.1,147,127,4,5,Yes,Yes
36,M,Intervention,No formal,Yes,69,26.5,161,144,3,5,No,Yes
37,F,Intervention,Secondary,No,50,26.7,155,142,3,4,No,Yes
38,M,Intervention,Tertiary,No,45,26.7,150,144,5,7,No,No
39,F,Intervention,Primary,No,58,18.5,154,132,4,6,Yes,Yes
40,M,Intervention,Secondary,Yes,54,28.6,162,153,4,6,No,No
'), stringsAsFactors = FALSE, strip.white = TRUE)

# Tidy any loaded table: data frame, syntactic names, no empty rows/columns,
# whole-number columns stored as integers, character values trimmed
clean_df <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- make.names(trimws(names(df)), unique = TRUE)
  for (v in names(df)) {
    x <- df[[v]]
    if (is.character(x)) { x <- trimws(x); x[x == ""] <- NA }
    if (inherits(x, c("POSIXct", "Date"))) x <- as.character(x)
    if (is.numeric(x) && all(is.na(x) | abs(x - round(x)) < 1e-9)) x <- as.integer(round(x))
    df[[v]] <- x
  }
  keep_c <- vapply(df, function(x) any(!is.na(x)), logical(1))
  df <- df[, keep_c, drop = FALSE]
  df[rowSums(!is.na(df)) > 0, , drop = FALSE]
}

read_any <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) {
    sh <- readxl::excel_sheets(path)
    df <- readxl::read_excel(path, sheet = if ("Data" %in% sh) "Data" else sh[1])
  } else {
    df <- read.csv(path, stringsAsFactors = FALSE, strip.white = TRUE)
  }
  clean_df(df)
}

hyp_builtin <- clean_df(hyp_builtin)

# -----------------------------------------------------------------------------
# 2. Variable metadata
# -----------------------------------------------------------------------------
pal <- c("#2A9D8F", "#E76F51", "#4C8BF5", "#E9A93A", "#9B72CF",
         "#43AA8B", "#F28C4E", "#E0609A")
col_mean   <- "#E76F51"
col_median <- "#4C8BF5"
col_fence  <- "#9B72CF"
ramp_cols  <- function(n) grDevices::colorRampPalette(
  c("#2A9D8F", "#43AA8B", "#4C8BF5", "#9B72CF", "#E0609A", "#E76F51", "#E9A93A"))(max(1, n))
quarter_cols <- c("Lowest quarter" = "#4C8BF5", "Second quarter" = "#2A9D8F",
                  "Third quarter" = "#E9A93A", "Top quarter" = "#E76F51", "Flagged" = "#E0609A")

share_bar <- function(p, col) sprintf("<span class='share'><span style='width:%.1f%%;background:%s'></span></span>", p, col)

tile <- function(val, lab, k) div(class = paste0("tile tile-", k), div(class = "tile-val", val), div(class = "tile-lab", lab))

hdr <- function(txt, k) card_header(txt, class = paste0("hdr hdr-", k))

known_scales <- c(ID = "identifier", Sex = "nominal", Arm = "nominal",
                  Education = "ordinal", Smoker = "nominal", Age = "ratio",
                  BMI = "ratio", SBP_Base = "ratio", SBP_6mo = "ratio",
                  Adherence = "ordinal", Visits = "ratio",
                  BP_Controlled = "nominal", Responded = "nominal")

known_levels <- list(
  Sex = c("F", "M"),
  Arm = c("Usual care", "Intervention"),
  Education = c("No formal", "Primary", "Secondary", "Tertiary"),
  Smoker = c("No", "Yes"),
  Adherence = as.character(1:5),
  BP_Controlled = c("No", "Yes"),
  Responded = c("No", "Yes")
)

level_labels <- list(
  Adherence = c("1" = "1 Never", "2" = "2 Rarely", "3" = "3 Sometimes",
                "4" = "4 Usually", "5" = "5 Always"),
  Sex = c(F = "Female", M = "Male")
)

var_labels <- c(ID = "Patient ID", Sex = "Sex", Arm = "Treatment arm",
                Education = "Education", Smoker = "Current smoker",
                Age = "Age (years)", BMI = "BMI (kg/m\u00b2)",
                SBP_Base = "Baseline systolic BP (mmHg)",
                SBP_6mo = "Systolic BP at 6 months (mmHg)",
                Adherence = "Medication adherence (1\u20135)",
                Visits = "Clinic visits",
                BP_Controlled = "BP controlled at 6 months",
                Responded = "Responded")

vlab <- function(v) if (v %in% names(var_labels)) unname(var_labels[v]) else v

scale_of <- function(df, v) {
  if (v %in% names(known_scales)) return(unname(known_scales[v]))
  if (tolower(v) %in% c("id", "patient_id", "subject_id", "pid")) return("identifier")
  if (is.numeric(df[[v]])) "ratio" else "nominal"
}

vars_of <- function(df, types) {
  v <- names(df)
  v[vapply(v, function(z) scale_of(df, z) %in% types, logical(1))]
}

lab_choices <- function(v) setNames(v, vapply(v, vlab, character(1)))

pick <- function(ch, pref) if (length(ch) == 0) character(0) else if (pref %in% ch) pref else ch[1]

get_levels <- function(df, v) {
  x <- df[[v]]
  obs <- unique(as.character(x[!is.na(x)]))
  kl <- known_levels[[v]]
  if (!is.null(kl) && all(obs %in% kl)) return(kl)
  if (is.numeric(x)) return(as.character(sort(unique(x[!is.na(x)]))))
  sort(obs)
}

as_cat <- function(df, v) {
  lv <- get_levels(df, v)
  f <- factor(as.character(df[[v]]), levels = lv)
  lab <- level_labels[[v]]
  if (!is.null(lab) && all(lv %in% names(lab))) levels(f) <- unname(lab[lv])
  f
}

# -----------------------------------------------------------------------------
# 3. Statistical helpers
# -----------------------------------------------------------------------------
dec_places <- function(x) {
  x <- x[!is.na(x)]
  for (d in 0:4) if (all(abs(round(x, d) - x) < 1e-8)) return(d)
  4
}

fmt <- function(v, d) formatC(v, format = "f", digits = d)
fmtn <- function(v) vapply(v, function(z) if (is.na(z)) "\u2014" else format(round(z, 2), nsmall = 0), character(1))

# Adjusted Fisher-Pearson skewness (G1), as reported on the slides
skew_G1 <- function(x) {
  x <- x[!is.na(x)]; n <- length(x)
  if (n < 3 || sd(x) == 0) return(NA_real_)
  m <- mean(x)
  g1 <- mean((x - m)^3) / mean((x - m)^2)^1.5
  g1 * sqrt(n * (n - 1)) / (n - 2)
}

shape_verdict <- function(s) {
  if (is.na(s)) return("\u2014")
  if (abs(s) < 0.5) "Approximately symmetric"
  else if (s > 0) "Positively (right) skewed"
  else "Negatively (left) skewed"
}

# Sturges' rule
sturges <- function(x) {
  x <- x[!is.na(x)]; n <- length(x)
  k <- 1 + 3.322 * log10(n)
  rng <- max(x) - min(x)
  list(n = n, k = k, rng = rng, w_raw = rng / k, min = min(x), max = max(x))
}

nice_width <- function(w_raw, u) {
  if (!is.finite(w_raw) || w_raw <= 0) return(u)
  p <- 10^floor(log10(w_raw))
  cand <- c(1, 2, 5, 10) * p
  max(cand[which.min(abs(cand - w_raw))], u)
}

default_w <- function(x) {
  x <- x[!is.na(x)]
  u <- 10^-dec_places(x)
  nice_width(sturges(x)$w_raw, u)
}

default_start <- function(x, w) floor(min(x, na.rm = TRUE) / w + 1e-9) * w

# Grouped frequency distribution with stated limits, boundaries and midpoints
grouped <- function(x, w, start = NULL) {
  x <- x[!is.na(x)]
  d <- dec_places(x); u <- 10^-d
  w <- max(w, u)
  if (is.null(start) || is.na(start)) start <- default_start(x, w)
  K <- floor((max(x) - start) / w + 1e-9) + 1
  K <- max(1, min(K, 80))
  lower <- start + (0:(K - 1)) * w
  upper <- lower + w - u
  brks  <- start - u / 2 + (0:K) * w
  idx <- findInterval(x, brks)
  inside <- idx >= 1 & idx <= K
  f <- tabulate(idx[inside], nbins = K)
  list(lower = lower, upper = upper, lb = brks[-(K + 1)], ub = brks[-1],
       mid = (lower + upper) / 2, f = f, cf = cumsum(f), n = length(x),
       K = K, d = d, u = u, w = w, start = start, brks = brks,
       uncovered = sum(!inside), min = min(x), max = max(x))
}

grouped_df <- function(g) {
  d <- g$d
  out <- data.frame(
    "Class (stated limits)" = paste0(fmt(g$lower, d), " \u2013 ", fmt(g$upper, d)),
    "Class boundaries" = paste0(fmt(g$lb, d + 1), " \u2013 ", fmt(g$ub, d + 1)),
    "Midpoint" = fmt(g$mid, d + 1),
    "f" = as.character(g$f),
    "rf (%)" = fmt(100 * g$f / g$n, 1),
    "cf" = as.character(g$cf),
    "c%" = fmt(100 * g$cf / g$n, 1),
    "Share" = share_bar(100 * g$f / g$n, ramp_cols(g$K)),
    check.names = FALSE)
  rbind(out, data.frame("Class (stated limits)" = "Total", "Class boundaries" = "",
                        "Midpoint" = "", "f" = as.character(sum(g$f)),
                        "rf (%)" = fmt(100 * sum(g$f) / g$n, 1), "cf" = "\u2014",
                        "c%" = "\u2014", "Share" = "", check.names = FALSE))
}

tally_html <- function(k) {
  if (k == 0) return("")
  five <- k %/% 5; rest <- k %% 5
  paste0(paste(rep("<span class='tally5'>||||</span>", five), collapse = " "),
         if (five > 0 && rest > 0) " " else "", strrep("|", rest))
}

integer_breaks <- function(l) { b <- pretty(l); b[b == round(b)] }

theme_class <- function(fs) {
  theme_minimal(base_size = fs) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.subtitle = element_text(colour = "#55616B"),
          axis.title = element_text(colour = "#33414D"),
          plot.title = element_text(face = "bold", size = rel(1), colour = "#1E6F65"),
          panel.grid.major.y = element_line(colour = "#E6ECF2"),
          panel.background = element_rect(fill = "#FBFCFF", colour = NA),
          legend.position = "top",
          plot.background = element_rect(fill = "white", colour = NA))
}

hist_plot <- function(g, xlab, fs, fill = NULL) {
  d <- data.frame(xmin = g$lb, xmax = g$ub, f = g$f, cls = factor(seq_len(g$K)))
  fills <- if (is.null(fill)) ramp_cols(g$K) else rep(fill, g$K)
  p <- ggplot(d) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = f, fill = cls),
              colour = "white", linewidth = 0.7) +
    scale_fill_manual(values = fills, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08)), breaks = integer_breaks) +
    labs(x = xlab, y = "Frequency (f)") +
    theme_class(fs)
  if (g$K <= 16) {
    p <- p + scale_x_continuous(breaks = g$brks, labels = fmt(g$brks, g$d + 1))
    if (g$K > 7) p <- p + theme(axis.text.x = element_text(angle = 40, hjust = 1))
  }
  p
}

cat_counts <- function(f) {
  f <- f[!is.na(f)]
  tb <- table(f)
  data.frame(level = factor(names(tb), levels = names(tb)),
             n = as.integer(tb), pct = 100 * as.integer(tb) / sum(tb))
}

legit_displays <- function(scale) {
  switch(scale,
         nominal = "Frequency table without cumulative columns; bar chart (any order); pie for 2\u20133 categories",
         ordinal = "Frequency table with cumulative columns; bar chart kept in natural order",
         ratio = , interval = "Grouped frequency table; histogram; stem-and-leaf; polygon; ogive; boxplot",
         identifier = "None \u2014 a label, not a measurement")
}

dict_df <- function(df) {
  data.frame(Variable = names(df),
             Description = vapply(names(df), vlab, ""),
             Scale = vapply(names(df), function(v) scale_of(df, v), ""),
             "Distinct values" = vapply(df, function(x) length(unique(x[!is.na(x)])), integer(1)),
             "Legitimate summaries and displays" = vapply(names(df), function(v) legit_displays(scale_of(df, v)), ""),
             check.names = FALSE, row.names = NULL)
}

note <- function(...) conditionalPanel("input.show_notes", div(class = "teach-note", ...))

# -----------------------------------------------------------------------------
# 4. Checkpoint questions (from the session slides)
# -----------------------------------------------------------------------------
cps <- list(
  list(q = "A cumulative frequency column would be meaningless for which of these variables?",
       opts = c(A = "Adherence score (1\u20135)", B = "Baseline systolic blood pressure",
                C = "Current smoker (Yes / No)", D = "Number of clinic visits"),
       ans = "C",
       why = "Cumulating means counting everything \u201cat or below\u201d a value, which requires ordered categories. Smoker is nominal \u2014 Yes is not above or below No. The other three are ordinal or ratio and cumulate legitimately."),
  list(q = "A class of \u201c150 \u2013 159 mmHg\u201d is recorded to the nearest whole mmHg. What is its class width?",
       opts = c(A = "9", B = "9.5", C = "10", D = "It cannot be determined"),
       ans = "C",
       why = "Width is measured between boundaries: 159.5 \u2212 149.5 = 10, or equivalently 160 \u2212 150. Answer A subtracts the stated limits and forgets that both 150 and 159 are inside the class."),
  list(q = "You want to compare the education profile of 20 intervention patients against 20 usual-care patients. Which display is best?",
       opts = c(A = "Two separate pie charts, one per arm", B = "A clustered bar chart of counts",
                C = "A clustered bar chart of percentages within each arm", D = "A single histogram of education"),
       ans = "C",
       why = "Percentages within each arm keep the comparison valid whatever the group sizes, and clustering puts the two profiles side by side. B works only because both arms happen to have n = 20. D is wrong outright \u2014 education is ordinal."),
  list(q = "For a boxplot of clinic visits, Q1 = 2 and Q3 = 6. Which value is flagged as an outlier?",
       opts = c(A = "9", B = "11", C = "12", D = "15"),
       ans = "D",
       why = "IQR = 6 \u2212 2 = 4, so the upper fence is 6 + 1.5 \u00d7 4 = 12. Only values beyond the fence are flagged: 15 qualifies, 12 sits exactly on the fence and does not."),
  list(q = "A histogram of clinic visits has a long tail stretching to the right. How is this distribution described, and what follows?",
       opts = c(A = "Negatively skewed; report the mean", B = "Positively skewed; the mean will exceed the median",
                C = "Positively skewed; the mean will fall below the median", D = "Bimodal; report both modes"),
       ans = "B",
       why = "Skew is named for the direction of the tail. Extreme high values drag the mean upward and leave the median almost untouched \u2014 mean 4.58 against a median of 4.0 in our data.")
)

# -----------------------------------------------------------------------------
# 5. Initial choices
# -----------------------------------------------------------------------------
init_all   <- vars_of(hyp_builtin, c("nominal", "ordinal", "ratio", "interval"))
init_cat   <- vars_of(hyp_builtin, c("nominal", "ordinal"))
init_quant <- vars_of(hyp_builtin, c("ratio", "interval"))
init_num   <- init_all[vapply(init_all, function(v) is.numeric(hyp_builtin[[v]]), logical(1))]
grp_choices <- function(catv) c("None" = "", lab_choices(catv))

measure_choices <- function(df) {
  q <- vars_of(df, c("ratio", "interval"))
  cv <- vars_of(df, c("nominal", "ordinal"))
  bin <- cv[vapply(cv, function(v) "Yes" %in% df[[v]], logical(1))]
  c(setNames(paste0("mean:", q), paste("Mean", vapply(q, vlab, ""))),
    setNames(paste0("pct:", bin), paste("% Yes \u2014", vapply(bin, vlab, ""))))
}

# -----------------------------------------------------------------------------
# 6. User interface
# -----------------------------------------------------------------------------
app_css <- "
body { background: linear-gradient(180deg, #F2FBF8 0%, #F8F5FF 45%, #FFF7EE 100%) fixed; }
.navbar { background: linear-gradient(90deg, #DDF4EE 0%, #E6EDFF 35%, #FBE6F0 70%, #FFEFDC 100%) !important;
          border-bottom: 2px solid #CDEBE3; }
.navbar-brand { font-weight: 700; color: #1E6F65 !important; }
.navbar .nav-link { font-weight: 600; border-radius: 999px; padding: .35rem .85rem !important; margin: 0 .1rem; }
.navbar .nav-item:nth-child(7n+1) .nav-link { color: #1E7F73 !important; }
.navbar .nav-item:nth-child(7n+2) .nav-link { color: #C4532F !important; }
.navbar .nav-item:nth-child(7n+3) .nav-link { color: #2F66C8 !important; }
.navbar .nav-item:nth-child(7n+4) .nav-link { color: #A8740F !important; }
.navbar .nav-item:nth-child(7n+5) .nav-link { color: #7447B0 !important; }
.navbar .nav-item:nth-child(7n+6) .nav-link { color: #B53A72 !important; }
.navbar .nav-item:nth-child(7n+7) .nav-link { color: #2B8A5C !important; }
.navbar .nav-link.active { background: rgba(255,255,255,.85); box-shadow: 0 1px 4px rgba(60,80,100,.12); }
.bslib-sidebar-layout > .sidebar { background: #F6F2FF; }
.bslib-page-navbar > .bslib-sidebar-layout > .sidebar { background: linear-gradient(180deg, #F3EEFF 0%, #EAF7F3 100%); }
.card { border-radius: 14px; border: 1px solid #E3E9F0; box-shadow: 0 2px 10px rgba(80,100,130,.07); }
.hdr { font-weight: 700; border-bottom-width: 3px !important; border-bottom-style: solid !important; }
.hdr-1 { background: #E2F5EF; border-bottom-color: #2A9D8F !important; color: #1E6F65; }
.hdr-2 { background: #FDEBE4; border-bottom-color: #E76F51 !important; color: #A9472B; }
.hdr-3 { background: #E7EEFE; border-bottom-color: #4C8BF5 !important; color: #2F5FB8; }
.hdr-4 { background: #FFF3D9; border-bottom-color: #E9A93A !important; color: #8E6210; }
.hdr-5 { background: #F1E9FB; border-bottom-color: #9B72CF !important; color: #6B45A0; }
.hdr-6 { background: #FCE6F0; border-bottom-color: #E0609A !important; color: #A33165; }
.nav-tabs .nav-link { font-weight: 600; color: #50606E; }
.nav-tabs .nav-item:nth-child(6n+1) .nav-link.active { color: #1E7F73; border-top: 3px solid #2A9D8F; background: #F0FAF7; }
.nav-tabs .nav-item:nth-child(6n+2) .nav-link.active { color: #C4532F; border-top: 3px solid #E76F51; background: #FFF4F0; }
.nav-tabs .nav-item:nth-child(6n+3) .nav-link.active { color: #2F66C8; border-top: 3px solid #4C8BF5; background: #F2F6FF; }
.nav-tabs .nav-item:nth-child(6n+4) .nav-link.active { color: #A8740F; border-top: 3px solid #E9A93A; background: #FFF9EC; }
.nav-tabs .nav-item:nth-child(6n+5) .nav-link.active { color: #7447B0; border-top: 3px solid #9B72CF; background: #F7F2FD; }
.nav-tabs .nav-item:nth-child(6n+6) .nav-link.active { color: #B53A72; border-top: 3px solid #E0609A; background: #FDF1F6; }
.btn-outline-primary { border-radius: 999px; }
.btn-dl { border-radius: 999px; font-weight: 600; margin-bottom: .45rem; width: 100%; }
.btn-dl-1 { background: #E2F5EF; border: 1px solid #2A9D8F; color: #1E6F65; }
.btn-dl-2 { background: #E7EEFE; border: 1px solid #4C8BF5; color: #2F5FB8; }
.btn-dl-3 { background: #FDEBE4; border: 1px solid #E76F51; color: #A9472B; }
.btn-dl:hover { filter: brightness(0.97); }
.src-chip { display: inline-block; padding: .25rem .7rem; border-radius: 999px; font-size: .9rem; font-weight: 600;
            margin-top: .3rem; }
.src-builtin { background: #E2F5EF; color: #1E6F65; border: 1px solid #9ED8CC; }
.src-upload  { background: #FFF3D9; color: #8E6210; border: 1px solid #F0CF8A; }
.tiles { display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: .8rem; margin-bottom: 1rem; }
.tile { border-radius: 14px; padding: .8rem 1rem; border: 1px solid; }
.tile-val { font-size: 2rem; font-weight: 800; line-height: 1.1; }
.tile-lab { font-size: .95rem; color: #50606E; }
.tile-1 { background: #E2F5EF; border-color: #A7DDD2; } .tile-1 .tile-val { color: #1E7F73; }
.tile-2 { background: #E7EEFE; border-color: #B5CBFA; } .tile-2 .tile-val { color: #2F66C8; }
.tile-3 { background: #FDEBE4; border-color: #F5BFAE; } .tile-3 .tile-val { color: #C4532F; }
.tile-4 { background: #FFF3D9; border-color: #F0CF8A; } .tile-4 .tile-val { color: #A8740F; }
.tile-5 { background: #F1E9FB; border-color: #D3BFEE; } .tile-5 .tile-val { color: #7447B0; }
.share { display: inline-block; width: 150px; height: 14px; background: #EEF2F5; border-radius: 7px;
         overflow: hidden; vertical-align: middle; }
.share > span { display: block; height: 100%; border-radius: 7px; }
.table-striped > tbody > tr:nth-of-type(odd) > * { --bs-table-accent-bg: #F4FBF9; }
.stem-l { color: #2A9D8F; font-weight: 700; } .stem-b { color: #9AA7B2; } .stem-r { color: #E76F51; font-weight: 600; }
.stem-h { color: #7447B0; }
.teach-note { background: #FFF8E6; border-left: 4px solid #E9A93A; padding: .65rem .95rem;
              border-radius: 6px; margin: .5rem 0 .9rem; font-size: 1.02rem; }
.warn-note  { background: #FDEEE8; border-left: 4px solid #E76F51; padding: .65rem .95rem;
              border-radius: 6px; margin: .4rem 0 .8rem; }
.ok-note    { background: #E9F6F2; border-left: 4px solid #2A9D8F; padding: .65rem .95rem;
              border-radius: 6px; margin: .4rem 0 .8rem; }
.rawvals { font-family: ui-monospace, Menlo, Consolas, monospace; font-size: 1.3rem;
           line-height: 2.1; background: #FBF9FF; padding: 1rem 1.2rem; border: 1px solid #E6DDF7;
           border-radius: 10px; }
.rawvals span { display: inline-block; min-width: 2.6em; text-align: center; margin: .1rem; padding: 0 .25rem;
                border-radius: 6px; font-weight: 700; }
.table { font-size: 1.05rem; }
.table thead th { background: #EEF1F3; }
.table tr:last-child td { font-weight: 600; }
.no-total .table tr:last-child td { font-weight: 400; }
.tally { font-family: ui-monospace, Menlo, Consolas, monospace; letter-spacing: .05em; color: #2A9D8F; font-weight: 700; }
.tally5 { text-decoration: line-through; text-decoration-thickness: 2px; text-decoration-color: #E76F51; }
.stem pre { font-size: 1.3rem; line-height: 1.5; background: #FBF9FF; border: 1px solid #E6DDF7; border-radius: 10px; padding: 1rem 1.2rem; }
.working p { font-family: ui-monospace, Menlo, Consolas, monospace; font-size: 1.05rem; margin-bottom: .35rem; }
.checks li { margin-bottom: .25rem; }
.big-q { font-size: 1.12rem; font-weight: 600; }
"

ui <- page_navbar(
  title = "Session 2 \u00b7 Frequency distributions and displaying data",
  id = "main",
  bg = "#EAF5F1",
  inverse = FALSE,
  fillable = FALSE,
  theme = bs_theme(version = 5, bg = "#FFFFFF", fg = "#1F2A33",
                   primary = "#2A9D8F", secondary = "#E76F51",
                   base_font = font_collection("Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
                   "font-size-base" = "1.02rem"),
  header = tags$head(tags$style(HTML(app_css))),
  sidebar = sidebar(
    title = "Class controls", width = 270, open = "desktop",
    sliderInput("fs", "Text size on plots", min = 12, max = 28, value = 17, step = 1),
    checkboxInput("show_notes", "Show teaching notes", TRUE),
    hr(),
    tags$strong("1. Download the data"),
    downloadButton("dl_xlsx", "Excel (.xlsx)", class = "btn-sm btn-dl btn-dl-1"),
    downloadButton("dl_csv", "CSV (.csv)", class = "btn-sm btn-dl btn-dl-2"),
    tags$strong("2. Upload it back"),
    fileInput("upload", NULL, accept = c(".xlsx", ".xls", ".csv"),
              buttonLabel = "Browse\u2026", placeholder = "Excel or CSV"),
    helpText("Excel: the sheet named Data is read (otherwise the first sheet)."),
    actionButton("use_builtin", "Back to the class dataset", class = "btn-sm btn-dl btn-dl-3"),
    uiOutput("data_info")
  ),

  # ---- The data --------------------------------------------------------------
  nav_panel(
    "The data",
    uiOutput("tiles"),
    card(
      hdr("Forty values. What can you actually see?", 1),
      layout_sidebar(
        sidebar = sidebar(
          selectInput("raw_var", "Variable", choices = lab_choices(init_all), selected = "Adherence"),
          radioButtons("raw_order", "Show values in",
                       c("Patient-ID order" = "id", "Sorted order" = "sorted"))
        ),
        uiOutput("raw_vals"),
        uiOutput("raw_caption"),
        note("Give the class ten seconds with the raw row. What is the most common value? ",
             "Do most patients do well or badly? Are the two arms different? None of this is readable by eye. ",
             "The move: stop looking at cases and start counting values.")
      )
    ),
    card(hdr("Variables, measurement scales and legitimate displays", 2), tableOutput("dict")),
    card(hdr("The data matrix: rows are patients, columns are variables", 3),
         div(class = "no-total", style = "max-height: 520px; overflow-y: auto;", tableOutput("data_tbl")))
  ),

  # ---- Ungrouped frequency table --------------------------------------------
  nav_panel(
    "Frequency table",
    card(
      hdr("Ungrouped frequency distribution: f, rf, cf and c%", 4),
      layout_sidebar(
        sidebar = sidebar(
          selectInput("ft_var", "Variable", choices = lab_choices(init_all), selected = "Adherence"),
          checkboxInput("ft_tally", "Show tally column", TRUE)
        ),
        uiOutput("ft_warn"),
        tableOutput("ft_tbl"),
        uiOutput("ft_read"),
        note("The tally is a teaching device, not a deliverable. Two checks every time: ",
             "\u03a3f = N, and the last c% is exactly 100.0. ",
             "Ask what 42.5 in the c% column means for adherence: 42.5% scored 3 or below, not 42.5% scored 3.")
      )
    )
  ),

  # ---- Grouped frequency table ----------------------------------------------
  nav_panel(
    "Grouped table",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("gt_var", "Quantitative variable", choices = lab_choices(init_quant), selected = "SBP_Base"),
        numericInput("gt_w", "Class width", value = 5, min = 0.1, step = 1),
        numericInput("gt_start", "Lower limit of the first class", value = 140, step = 1),
        actionButton("gt_reset", "Use the suggested width and start", class = "btn-sm btn-outline-primary")
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(hdr("Sturges\u2019 rule: a starting point, not an answer", 5), div(class = "working", uiOutput("gt_sturges"))),
        card(hdr("Six rules for class intervals", 6), uiOutput("gt_checks"))
      ),
      card(hdr("Grouped frequency distribution", 1), tableOutput("gt_tbl")),
      card(hdr("The same classes as a histogram", 2), plotOutput("gt_hist", height = "380px")),
      note("Width is boundary to boundary, not upper minus lower stated limit: 140\u2013144 has width 5, not 4. ",
           "Try width 7 or a start above the minimum and watch the checks fail. Group for display; keep the raw data for analysis.")
    )
  ),

  # ---- Categorical displays ---------------------------------------------------
  nav_panel(
    "Categorical displays",
    navset_card_tab(
      nav_panel(
        "Bar chart",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("bar_var", "Categorical variable", choices = lab_choices(init_cat), selected = "Education"),
            radioButtons("bar_order", "Bar order", c("Natural order" = "natural", "Sorted by frequency" = "freq")),
            radioButtons("bar_y", "Bar height", c("Count" = "n", "Percentage" = "pct"))
          ),
          uiOutput("bar_warn"),
          plotOutput("bar_plot", height = "420px"),
          note("The bar chart and its frequency table are the same object. Gaps between bars say \u201cseparate categories\u201d. ",
               "Never sort an ordinal variable by frequency \u2014 it throws away the order the scale carries.")
        )
      ),
      nav_panel(
        "Pie or bar?",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("pie_var", "Categorical variable", choices = lab_choices(init_cat), selected = "Education"),
            checkboxInput("pie_lab", "Label slices with percentages", FALSE)
          ),
          layout_columns(col_widths = c(6, 6),
                         plotOutput("pie_plot", height = "420px"),
                         plotOutput("pie_bar", height = "420px")),
          note("Ask the room which slice is second largest, first from the pie, then from the bars. ",
               "People judge lengths well and angles badly. Pies are acceptable for two or three categories that form a meaningful whole.")
        )
      ),
      nav_panel(
        "Clustered bars",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("cl_var", "Variable on the axis", choices = lab_choices(init_cat), selected = "Adherence"),
            selectInput("cl_grp", "Cluster by", choices = lab_choices(init_cat), selected = "Arm"),
            radioButtons("cl_y", "Bar height", c("Counts" = "n", "% within each group" = "pct"))
          ),
          plotOutput("cl_plot", height = "440px"),
          note("Adherence by arm: every patient scoring 5 is in the intervention arm; every patient scoring 1 or 2 is in usual care. ",
               "Percentages within each group keep the comparison fair when group sizes differ.")
        )
      ),
      nav_panel(
        "Cross-tabulation",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("ct_row", "Rows", choices = lab_choices(init_cat), selected = "Education"),
            selectInput("ct_col", "Columns", choices = lab_choices(init_cat), selected = "BP_Controlled"),
            radioButtons("ct_pct", "Show",
                         c("Counts" = "none", "Row %" = "row", "Column %" = "col", "% of grand total" = "total"),
                         selected = "none")
          ),
          tableOutput("ct_tbl"),
          uiOutput("ct_read"),
          note("Every cell has three possible percentages: of the row, of the column and of the grand total. ",
               "They answer different questions. Choose one deliberately and label it. Whether a gradient is real is a Session 10 question.")
        )
      )
    )
  ),

  # ---- Quantitative displays --------------------------------------------------
  nav_panel(
    "Quantitative displays",
    navset_card_tab(
      nav_panel(
        "Bar chart \u2260 histogram",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("bh_var", "Quantitative variable", choices = lab_choices(init_quant), selected = "SBP_Base"),
            numericInput("bh_w", "Class width", value = 5, min = 0.1, step = 1)
          ),
          layout_columns(col_widths = c(6, 6),
                         plotOutput("bh_bar", height = "400px"),
                         plotOutput("bh_hist", height = "400px")),
          note("The single most common chart error in student work. Gaps assert that the variable is categorical; ",
               "touching bars say this is one continuous scale, and each bar spans the class boundaries.")
        )
      ),
      nav_panel(
        "Histogram and bin width",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("hw_var", "Quantitative variable", choices = lab_choices(init_quant), selected = "SBP_Base"),
            sliderInput("hw_w", "Bin width", min = 1, max = 17, value = 5, step = 1),
            checkboxInput("hw_compare", "Compare half, current and double width", FALSE)
          ),
          plotOutput("hw_plot", height = "430px"),
          note("The histogram is a choice you make, not a fact you find. Drag the slider from 1 to 17 and watch the shape change character. ",
               "Report the bin width, and distrust any histogram whose story changes with it.")
        )
      ),
      nav_panel(
        "Stem-and-leaf",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("sl_var", "Quantitative variable", choices = lab_choices(init_quant), selected = "SBP_Base"),
            sliderInput("sl_scale", "Stem scale (length of the display)", min = 0.5, max = 3, value = 1, step = 0.5)
          ),
          div(class = "stem", uiOutput("sl_out")),
          note("Rotate it 90\u00b0 anticlockwise and you have the histogram \u2014 but every value is still readable. ",
               "Scale 1 splits each stem into two lines (leaves 0\u20134 and 5\u20139); scale 0.5 gives one line per stem.")
        )
      ),
      nav_panel(
        "Polygon and ogive",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("po_var", "Quantitative variable", choices = lab_choices(init_quant), selected = "SBP_Base"),
            numericInput("po_w", "Class width", value = 5, min = 0.1, step = 1),
            selectInput("po_grp", "Overlay polygons by", choices = grp_choices(init_cat), selected = ""),
            sliderInput("po_p", "Read a percentile off the ogive", min = 1, max = 99, value = 50, step = 1)
          ),
          layout_columns(col_widths = c(6, 6),
                         plotOutput("po_poly", height = "400px"),
                         plotOutput("po_ogive", height = "400px")),
          uiOutput("po_read"),
          note("The polygon joins class midpoints and is anchored at zero one class beyond each end \u2014 useful for overlaying two groups. ",
               "The ogive plots c% at each upper boundary; read across at 50% and down to find the median.")
        )
      ),
      nav_panel(
        "Boxplot and outliers",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("bx_var", "Quantitative variable", choices = lab_choices(init_quant), selected = "Visits"),
            selectInput("bx_grp", "Side by side by", choices = grp_choices(init_cat), selected = ""),
            checkboxInput("bx_fence", "Show the 1.5 \u00d7 IQR fences", TRUE),
            checkboxInput("bx_pts", "Show individual patients", FALSE)
          ),
          plotOutput("bx_plot", height = "auto"),
          tableOutput("bx_tbl"),
          note("Fences: Q1 \u2212 1.5 \u00d7 IQR and Q3 + 1.5 \u00d7 IQR. Whiskers stop at the most extreme value still inside the fences. ",
               "Flagged is not wrong: the 82-year-old and the patient with 15 visits are real. The rule tells you where to look, not what to delete.")
        )
      )
    )
  ),

  # ---- Shape --------------------------------------------------------------------
  nav_panel(
    "Shape",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("sh_var", "Variable", choices = lab_choices(init_num), selected = "Visits"),
        selectInput("sh_grp", "Also show each group of", choices = grp_choices(init_cat), selected = ""),
        numericInput("sh_w", "Bin width", value = 2, min = 0.1, step = 1),
        checkboxInput("sh_dens", "Overlay a smooth outline", FALSE)
      ),
      card(hdr("Shape, mean and median", 3), plotOutput("sh_plot", height = "auto")),
      card(hdr("Numerical companions", 4), tableOutput("sh_tbl")),
      note("Skew is named for the tail, not the bulk. A long right tail pulls the mean above the median. ",
           "Try SBP at 6 months pooled, then split by treatment arm: two peaks nearly always mean two populations have been pooled. ",
           "Rule of thumb used here: |G1| < 0.5 approximately symmetric. The picture comes first; a skewness near zero does not rule out two peaks.")
    )
  ),

  # ---- Misleading graphs --------------------------------------------------------
  nav_panel(
    "Misleading graphs",
    navset_card_tab(
      nav_panel(
        "Truncated axis",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("mx_measure", "Quantity plotted", choices = measure_choices(hyp_builtin), selected = "mean:SBP_6mo"),
            selectInput("mx_grp", "Grouped by", choices = lab_choices(init_cat), selected = "Arm"),
            sliderInput("mx_start", "Truncated axis starts at", min = 0, max = 130, value = 125, step = 1)
          ),
          layout_columns(col_widths = c(6, 6),
                         plotOutput("mx_honest", height = "400px"),
                         plotOutput("mx_trunc", height = "400px")),
          uiOutput("mx_read"),
          note("Same data, same bars, one changed axis. A truncated axis is not always dishonest, but it must be declared, ",
               "and bar charts in particular should start at zero because the eye reads bar length.")
        )
      ),
      nav_panel(
        "Unequal class widths",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("uq_var", "Quantitative variable", choices = lab_choices(init_quant), selected = "SBP_Base"),
            numericInput("uq_w", "Class width before merging", value = 5, min = 0.1, step = 1),
            sliderInput("uq_merge", "Merge the top classes into one", min = 2, max = 6, value = 4, step = 1),
            radioButtons("uq_h", "Bar height",
                         c("Count (misleading)" = "count", "Frequency density = f \u00f7 width" = "density"))
          ),
          plotOutput("uq_plot", height = "420px"),
          note("The eye reads area, not height. A wide merged class drawn at its full count looks far bigger than it is. ",
               "Either keep widths equal, or plot frequency density so that area is proportional to frequency.")
        )
      )
    )
  ),

  # ---- Checkpoints ----------------------------------------------------------------
  nav_panel(
    "Checkpoints",
    do.call(layout_columns, c(list(col_widths = c(6, 6)), lapply(seq_along(cps), function(i) {
        cp <- cps[[i]]
        card(
          hdr(paste("Checkpoint", i, "of", length(cps)), (i - 1) %% 6 + 1),
          div(class = "big-q", cp$q),
          radioButtons(paste0("cp_", i), NULL,
                       choiceNames = paste0(names(cp$opts), ".  ", cp$opts),
                       choiceValues = names(cp$opts), selected = character(0)),
          actionButton(paste0("cp_go_", i), "Check answer", class = "btn-sm btn-outline-primary"),
          uiOutput(paste0("cp_fb_", i))
        )
      })))
  )
)

# -----------------------------------------------------------------------------
# 7. Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  dat <- reactiveVal(hyp_builtin)
  src <- reactiveVal("builtin")
  fs  <- reactive(input$fs)

  observeEvent(input$upload, {
    f <- input$upload
    df <- tryCatch(read_any(f$datapath, f$name), error = function(e) NULL)
    if (is.null(df) || ncol(df) < 2 || nrow(df) < 3) {
      showNotification("That file could not be read: it needs a header row and at least two columns.", type = "error")
      return()
    }
    dat(df); src(f$name)
    showNotification(sprintf("Loaded %s: %d rows \u00d7 %d columns. Every tab now uses this file.",
                             f$name, nrow(df), ncol(df)), type = "message", duration = 6)
  })
  observeEvent(input$use_builtin, { dat(hyp_builtin); src("builtin") })

  output$dl_xlsx <- downloadHandler(
    filename = function() "hypertension-dataset.xlsx",
    content = function(file) writexl::write_xlsx(list(Data = dat(), Dictionary = dict_df(dat())), file)
  )
  output$dl_csv <- downloadHandler(
    filename = function() "hypertension-dataset.csv",
    content = function(file) write.csv(dat(), file, row.names = FALSE, na = "")
  )

  output$data_info <- renderUI({
    df <- dat()
    tagList(
      if (src() == "builtin") span(class = "src-chip src-builtin", "Class dataset (built in)")
      else span(class = "src-chip src-upload", paste("Uploaded:", src())),
      helpText(sprintf("%d rows \u00d7 %d columns", nrow(df), ncol(df)))
    )
  })

  output$tiles <- renderUI({
    df <- dat()
    allv <- vars_of(df, c("nominal", "ordinal", "ratio", "interval"))
    div(class = "tiles",
        tile(nrow(df), "patients (rows)", 1),
        tile(length(allv), "variables to analyse", 2),
        tile(length(vars_of(df, "nominal")), "nominal", 3),
        tile(length(vars_of(df, "ordinal")), "ordinal", 4),
        tile(length(vars_of(df, c("ratio", "interval"))), "interval / ratio", 5))
  })

  # refresh every selector when the data change
  observeEvent(dat(), {
    df <- dat()
    allv <- vars_of(df, c("nominal", "ordinal", "ratio", "interval"))
    catv <- vars_of(df, c("nominal", "ordinal"))
    qv   <- vars_of(df, c("ratio", "interval"))
    numv <- allv[vapply(allv, function(v) is.numeric(df[[v]]), logical(1))]
    upd <- function(id, ch, pref) updateSelectInput(session, id, choices = lab_choices(ch), selected = pick(ch, pref))
    upd("raw_var", allv, "Adherence"); upd("ft_var", allv, "Adherence")
    upd("gt_var", qv, "SBP_Base"); upd("bh_var", qv, "SBP_Base"); upd("hw_var", qv, "SBP_Base")
    upd("sl_var", qv, "SBP_Base"); upd("po_var", qv, "SBP_Base"); upd("bx_var", qv, "Visits")
    upd("uq_var", qv, "SBP_Base"); upd("sh_var", numv, "Visits")
    upd("bar_var", catv, "Education"); upd("pie_var", catv, "Education")
    upd("cl_var", catv, "Adherence"); upd("cl_grp", catv, "Arm")
    upd("ct_row", catv, "Education"); upd("ct_col", catv, "BP_Controlled"); upd("mx_grp", catv, "Arm")
    for (id in c("po_grp", "bx_grp", "sh_grp")) updateSelectInput(session, id, choices = grp_choices(catv), selected = "")
    mc <- measure_choices(df)
    updateSelectInput(session, "mx_measure", choices = mc, selected = pick(unname(mc), "mean:SBP_6mo"))
  }, ignoreInit = TRUE)

  numvar <- function(v) {
    req(v, v %in% names(dat()))
    x <- dat()[[v]]
    validate(need(is.numeric(x), "Choose a numeric variable."))
    x[!is.na(x)]
  }

  # ---- The data ---------------------------------------------------------------
  output$raw_vals <- renderUI({
    v <- req(input$raw_var); df <- dat(); x <- df[[v]]
    if (input$raw_order == "sorted") x <- sort(x)
    lv <- if (is.numeric(x)) sort(unique(x[!is.na(x)])) else get_levels(df, v)
    cols <- setNames(ramp_cols(length(lv)), as.character(lv))
    tint <- function(h) grDevices::adjustcolor(h, alpha.f = 0.16)
    div(class = "rawvals", HTML(paste(vapply(as.character(x), function(z) {
      if (is.na(z)) return("<span style='color:#9AA7B2'>NA</span>")
      sprintf("<span style='color:%s;background:%s'>%s</span>", cols[[z]], tint(cols[[z]]), htmltools::htmlEscape(z))
    }, ""), collapse = " ")))
  })
  output$raw_caption <- renderUI({
    v <- req(input$raw_var); df <- dat(); x <- df[[v]]
    tags$p(style = "margin-top:.6rem; color:#55616B;",
           sprintf("%s \u2014 all %d patients, %s. %d distinct values; scale: %s.",
                   vlab(v), sum(!is.na(x)),
                   if (input$raw_order == "sorted") "sorted" else "in patient-ID order",
                   length(unique(x[!is.na(x)])), scale_of(df, v)))
  })
  output$dict <- renderTable({
    d <- dict_df(dat())
    sc_col <- c(nominal = "#C4532F", ordinal = "#A8740F", ratio = "#2F66C8", interval = "#2F66C8", identifier = "#7A8791")
    d$Scale <- sprintf("<span style='color:%s;font-weight:700'>%s</span>", sc_col[d$Scale], d$Scale)
    d
  }, striped = TRUE, spacing = "s", sanitize.text.function = function(x) x)
  output$data_tbl <- renderTable(dat(), striped = TRUE, spacing = "xs", digits = 1)

  # ---- Frequency table -------------------------------------------------------
  output$ft_warn <- renderUI({
    v <- req(input$ft_var); df <- dat(); sc <- scale_of(df, v)
    nd <- length(unique(df[[v]][!is.na(df[[v]])]))
    if (sc == "nominal") {
      div(class = "warn-note", strong(vlab(v)), " is nominal: there is no \u201cat or below\u201d, so the cumulative columns are left empty.")
    } else if (sc %in% c("ratio", "interval") && nd > 15) {
      div(class = "warn-note", sprintf("%d distinct values in %d patients: this table is the raw list wearing a table\u2019s clothes. Use the Grouped table tab.",
                                       nd, sum(!is.na(df[[v]]))))
    }
  })
  output$ft_tbl <- renderTable({
    v <- req(input$ft_var); df <- dat(); sc <- scale_of(df, v)
    f <- as_cat(df, v); tb <- table(f); n <- sum(tb)
    cum_ok <- sc != "nominal"
    out <- data.frame(Value = htmltools::htmlEscape(names(tb)), check.names = FALSE)
    if (isTRUE(input$ft_tally)) out$Tally <- paste0("<span class='tally'>", vapply(as.integer(tb), tally_html, ""), "</span>")
    out$f <- as.character(as.integer(tb))
    out[["rf (%)"]] <- fmt(100 * as.integer(tb) / n, 1)
    out$cf <- if (cum_ok) as.character(cumsum(as.integer(tb))) else ""
    out[["c%"]] <- if (cum_ok) fmt(100 * cumsum(as.integer(tb)) / n, 1) else ""
    out$Share <- share_bar(100 * as.integer(tb) / n, ramp_cols(length(tb)))
    tot <- out[1, ]; tot[1, ] <- ""
    tot$Value <- "Total"; tot$f <- as.character(n); tot[["rf (%)"]] <- fmt(100, 1)
    tot$cf <- "\u2014"; tot[["c%"]] <- "\u2014"
    rbind(out, tot)
  }, sanitize.text.function = function(x) x, align = "l", striped = TRUE)
  output$ft_read <- renderUI({
    v <- req(input$ft_var); df <- dat(); sc <- scale_of(df, v)
    f <- as_cat(df, v); tb <- table(f); n <- sum(tb); p <- 100 * as.integer(tb) / n
    modes <- names(tb)[tb == max(tb)]
    items <- list(tags$li(sprintf("Modal category: %s (f = %d, %.1f%%).", paste(modes, collapse = " and "), max(tb), max(p))))
    if (sc != "nominal" && length(tb) >= 4) {
      k <- length(tb); cp <- cumsum(p)
      items <- c(items, list(
        tags$li(sprintf("%.1f%% are at or below \u201c%s\u201d (c%% column).", cp[2], names(tb)[2])),
        tags$li(sprintf("%.1f%% are in the top two categories (\u201c%s\u201d or \u201c%s\u201d).",
                        p[k - 1] + p[k], names(tb)[k - 1], names(tb)[k]))))
    }
    items <- c(items, list(tags$li(sprintf("Checks: \u03a3f = %d = N; final c%% = %s.", n,
                                           if (sc != "nominal") "100.0" else "not applicable for a nominal variable"))))
    div(class = "ok-note", strong("Reading the table"), tags$ul(style = "margin-bottom:0", items))
  })

  # ---- Grouped table ------------------------------------------------------------
  set_width_start <- function(wid, sid, v) {
    x <- numvar(v); w <- default_w(x); u <- 10^-dec_places(x)
    updateNumericInput(session, wid, value = w, min = u, step = if (u < 1) u * 10 else 1)
    if (!is.null(sid)) updateNumericInput(session, sid, value = default_start(x, w), step = if (u < 1) u * 10 else 1)
  }
  observeEvent(input$gt_var, set_width_start("gt_w", "gt_start", input$gt_var))
  observeEvent(input$gt_reset, set_width_start("gt_w", "gt_start", input$gt_var))
  observeEvent(input$bh_var, set_width_start("bh_w", NULL, input$bh_var))
  observeEvent(input$po_var, set_width_start("po_w", NULL, input$po_var))
  observeEvent(input$uq_var, set_width_start("uq_w", NULL, input$uq_var))
  observeEvent(input$sh_var, set_width_start("sh_w", NULL, input$sh_var))
  observeEvent(input$hw_var, {
    x <- numvar(input$hw_var); u <- 10^-dec_places(x); w <- default_w(x)
    mx <- max(u * 2, ceiling((max(x) - min(x)) / 2))
    updateSliderInput(session, "hw_w", min = u, max = mx, value = min(w, mx), step = u)
  })

  gt <- reactive({
    x <- numvar(input$gt_var)
    w <- input$gt_w; s <- input$gt_start
    validate(need(is.numeric(w) && !is.na(w) && w > 0, "Enter a positive class width."),
             need(is.numeric(s) && !is.na(s), "Enter the lower limit of the first class."))
    grouped(x, w, s)
  })

  output$gt_sturges <- renderUI({
    x <- numvar(input$gt_var); s <- sturges(x); d <- dec_places(x); u <- 10^-d
    tagList(
      tags$p(sprintf("k = 1 + 3.322 \u00d7 log\u2081\u2080(%d) = 1 + 3.322 \u00d7 %.3f = %.2f", s$n, log10(s$n), s$k)),
      tags$p(sprintf("range = %s \u2212 %s = %s", fmt(s$max, d), fmt(s$min, d), fmt(s$rng, d))),
      tags$p(sprintf("w \u2248 %s / %.2f = %.2f  \u2192  convenient width %s", fmt(s$rng, d), s$k, s$w_raw,
                     format(nice_width(s$w_raw, u)))),
      tags$p(sprintf("\u2192 about %d or %d classes", floor(s$k), ceiling(s$k))),
      div(style = "font-family: inherit; color:#55616B; margin-top:.4rem;",
          "Sturges assumes roughly normal data. For a skewed variable such as clinic visits, follow the shape, not the formula.")
    )
  })

  output$gt_checks <- renderUI({
    g <- gt()
    lead <- g$w / 10^floor(log10(g$w))
    conv <- any(abs(lead - c(1, 2, 2.5, 5, 10)) < 1e-9)
    chk <- function(ok, txt) tags$li(HTML(paste0(if (ok) "<span style='color:#2A9D8F;font-weight:700'>\u2713</span> "
                                                  else "<span style='color:#E76F51;font-weight:700'>\u2717</span> ", txt)))
    inner_empty <- if (g$K > 2) sum(g$f[2:(g$K - 1)] == 0) else 0
    tags$ul(class = "checks", style = "list-style:none; padding-left:0;",
            chk(TRUE, "Mutually exclusive: boundaries are half a unit outside the stated limits, so no value can sit on two classes"),
            chk(g$uncovered == 0, sprintf("Exhaustive: %d of %d values fall in a class", g$n - g$uncovered, g$n)),
            chk(TRUE, sprintf("Equal width: every class is %s wide", format(g$w))),
            chk(g$K >= 5 && g$K <= 15, sprintf("Between 5 and 15 classes: %d classes", g$K)),
            chk(conv, sprintf("Convenient width: %s", format(g$w))),
            chk(g$start <= g$min && g$f[1] > 0 && g$f[g$K] > 0,
                sprintf("Starts at or below the minimum (%s) with no empty class at either end", format(g$min))),
            if (inner_empty > 0) tags$li(style = "color:#55616B", sprintf("(%d empty class%s inside the range)", inner_empty, if (inner_empty > 1) "es" else ""))
    )
  })

  output$gt_tbl <- renderTable(grouped_df(gt()), striped = TRUE, align = "l", sanitize.text.function = function(x) x)
  output$gt_hist <- renderPlot({
    g <- gt()
    hist_plot(g, vlab(input$gt_var), fs()) +
      labs(title = sprintf("%d classes of width %s; bars span the class boundaries", g$K, format(g$w)))
  }, res = 96)

  # ---- Bar chart -------------------------------------------------------------------
  cat_bar <- function(d, xlab, y = "n", fs) {
    d$y <- if (y == "pct") d$pct else d$n
    d$lab <- if (y == "pct") sprintf("%.1f%%", d$pct) else as.character(d$n)
    ggplot(d, aes(level, y, fill = level)) +
      geom_col(width = 0.62) +
      geom_text(aes(label = lab), vjust = -0.45, size = fs / 3) +
      scale_fill_manual(values = rep_len(pal, nrow(d)), guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
      labs(x = xlab, y = if (y == "pct") "Percentage of patients" else "Frequency (f)") +
      theme_class(fs)
  }
  output$bar_warn <- renderUI({
    v <- req(input$bar_var); sc <- scale_of(dat(), v)
    if (input$bar_order == "freq" && sc == "ordinal")
      div(class = "warn-note", strong(vlab(v)), " is ordinal. Sorting by frequency destroys the order the scale carries \u2014 put it back in natural order.")
    else if (input$bar_order == "freq" && sc == "nominal")
      div(class = "ok-note", strong(vlab(v)), " is nominal, so sorting by frequency loses nothing: there is no natural order.")
  })
  output$bar_plot <- renderPlot({
    v <- req(input$bar_var); d <- cat_counts(as_cat(dat(), v))
    if (input$bar_order == "freq") d$level <- factor(d$level, levels = d$level[order(-d$n)])
    cat_bar(d, vlab(v), input$bar_y, fs()) + labs(title = paste(vlab(v), "\u2014 all", sum(d$n), "patients"))
  }, res = 96)

  # ---- Pie vs bar -------------------------------------------------------------------
  output$pie_plot <- renderPlot({
    v <- req(input$pie_var); d <- cat_counts(as_cat(dat(), v))
    p <- ggplot(d, aes(x = 1, y = n, fill = level)) +
      geom_col(width = 1, colour = "white", linewidth = 1) +
      coord_polar(theta = "y", direction = -1) +
      scale_fill_manual(values = rep_len(pal, nrow(d))) +
      labs(fill = NULL, title = "Pie chart: compare the angles") +
      theme_void(base_size = fs()) +
      theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5))
    if (isTRUE(input$pie_lab))
      p <- p + geom_text(aes(label = sprintf("%.1f%%", pct)), position = position_stack(vjust = 0.5), size = fs() / 3)
    p
  }, res = 96)
  output$pie_bar <- renderPlot({
    v <- req(input$pie_var); d <- cat_counts(as_cat(dat(), v))
    cat_bar(d, vlab(v), "n", fs()) + labs(title = "Bar chart: compare the lengths")
  }, res = 96)

  # ---- Clustered bars -----------------------------------------------------------------
  output$cl_plot <- renderPlot({
    v <- req(input$cl_var); g <- req(input$cl_grp)
    validate(need(v != g, "Choose two different variables."))
    df <- dat()
    tb <- as.data.frame(table(x = as_cat(df, v), g = as_cat(df, g)))
    tot <- ave(tb$Freq, tb$g, FUN = sum)
    pct <- input$cl_y == "pct"
    tb$y <- if (pct) 100 * tb$Freq / tot else tb$Freq
    tb$lab <- if (pct) sprintf("%.0f%%", tb$y) else as.character(tb$Freq)
    ggplot(tb, aes(x, y, fill = g)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      geom_text(aes(label = lab), position = position_dodge(width = 0.8), vjust = -0.4, size = fs() / 3.4) +
      scale_fill_manual(values = rep_len(pal, nlevels(tb$g))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
      labs(x = vlab(v), y = if (pct) paste("% within each", tolower(vlab(g))) else "Frequency (f)",
           fill = vlab(g), title = paste(vlab(v), "by", tolower(vlab(g)))) +
      theme_class(fs())
  }, res = 96)

  # ---- Cross-tabulation -----------------------------------------------------------------
  ct <- reactive({
    r <- req(input$ct_row); cc <- req(input$ct_col)
    validate(need(r != cc, "Choose two different variables."))
    table(as_cat(dat(), r), as_cat(dat(), cc))
  })
  output$ct_tbl <- renderTable({
    tb <- ct(); mode <- input$ct_pct
    rs <- rowSums(tb); cs <- colSums(tb); N <- sum(tb)
    pc <- function(v) sprintf("%.1f%%", v)
    if (mode == "none") {
      body <- cbind(matrix(as.character(tb), nrow(tb)), as.character(rs))
      last <- c(as.character(cs), as.character(N))
    } else if (mode == "row") {
      body <- cbind(matrix(pc(100 * prop.table(tb, 1)), nrow(tb)), sprintf("100.0%% (n = %d)", rs))
      last <- c(pc(100 * cs / N), sprintf("100.0%% (n = %d)", N))
    } else if (mode == "col") {
      body <- cbind(matrix(pc(100 * prop.table(tb, 2)), nrow(tb)), pc(100 * rs / N))
      last <- c(sprintf("100.0%% (n = %d)", cs), sprintf("100.0%% (n = %d)", N))
    } else {
      body <- cbind(matrix(pc(100 * tb / N), nrow(tb)), pc(100 * rs / N))
      last <- c(pc(100 * cs / N), sprintf("100.0%% (n = %d)", N))
    }
    out <- rbind(body, last)
    out <- data.frame(c(rownames(tb), "Total"), out, check.names = FALSE, stringsAsFactors = FALSE)
    names(out) <- c(vlab(input$ct_row), colnames(tb), "Row total")
    out
  }, striped = TRUE, align = "l")
  output$ct_read <- renderUI({
    tb <- ct(); mode <- input$ct_pct; r <- vlab(input$ct_row); cc <- vlab(input$ct_col)
    q <- switch(mode,
                none = "Counts: the raw material. Percentages come next \u2014 but of what base?",
                row = sprintf("Row %%: what proportion of each %s group falls in each %s category?", tolower(r), tolower(cc)),
                col = sprintf("Column %%: within each %s category, how is %s distributed?", tolower(cc), tolower(r)),
                total = sprintf("%% of grand total: what share of all %d patients sits in each cell?", sum(tb)))
    extra <- NULL
    if (mode == "row" && ncol(tb) >= 2) {
      last <- colnames(tb)[ncol(tb)]; rp <- 100 * prop.table(tb, 1)[, ncol(tb)]; rs <- rowSums(tb)
      lo <- which.min(rp); hi <- which.max(rp)
      extra <- tags$p(style = "margin:.3rem 0 0", sprintf("\u201c%s\u201d ranges from %.1f%% (%s, %d of %d) to %.1f%% (%s, %d of %d). Always report the base.",
                                                        last, rp[lo], rownames(tb)[lo], tb[lo, ncol(tb)], rs[lo],
                                                        rp[hi], rownames(tb)[hi], tb[hi, ncol(tb)], rs[hi]))
    }
    div(class = "ok-note", q, extra)
  })

  # ---- Bar chart vs histogram -----------------------------------------------------
  bh <- reactive({
    x <- numvar(input$bh_var); w <- input$bh_w
    validate(need(is.numeric(w) && !is.na(w) && w > 0, "Enter a positive class width."))
    grouped(x, w)
  })
  output$bh_bar <- renderPlot({
    g <- bh()
    d <- data.frame(cls = factor(paste0(fmt(g$lower, g$d), "\u2013", fmt(g$upper, g$d)),
                                 levels = paste0(fmt(g$lower, g$d), "\u2013", fmt(g$upper, g$d))), f = g$f)
    ggplot(d, aes(cls, f, fill = cls)) + geom_col(width = 0.6) +
      scale_fill_manual(values = grDevices::colorRampPalette(c("#F4A08A", "#E76F51", "#C94F6D"))(nrow(d)), guide = "none") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.08)), breaks = integer_breaks) +
      labs(x = paste(vlab(input$bh_var), "(stated class limits)"), y = "Frequency (f)",
           title = "Wrong: gaps between bars", subtitle = "Gaps assert separate categories") +
      theme_class(fs()) + theme(axis.text.x = element_text(angle = 40, hjust = 1))
  }, res = 96)
  output$bh_hist <- renderPlot({
    hist_plot(bh(), paste(vlab(input$bh_var), "(class boundaries)"), fs()) +
      labs(title = "Right: bars touch", subtitle = "One continuous scale")
  }, res = 96)

  # ---- Histogram and bin width -----------------------------------------------------
  output$hw_plot <- renderPlot({
    x <- numvar(input$hw_var); w <- req(input$hw_w); u <- 10^-dec_places(x)
    ws <- if (isTRUE(input$hw_compare)) unique(c(max(u, w / 2), w, 2 * w)) else w
    rects <- do.call(rbind, lapply(ws, function(wi) {
      g <- grouped(x, wi)
      data.frame(xmin = g$lb, xmax = g$ub, f = g$f, col = ramp_cols(g$K),
                 panel = sprintf("Width %s  (%d classes)", format(signif(wi, 4)), g$K))
    }))
    rects$panel <- factor(rects$panel, levels = unique(rects$panel))
    p <- ggplot(rects) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = f, fill = col), colour = "white", linewidth = 0.5) +
      scale_fill_identity() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.08)), breaks = integer_breaks) +
      labs(x = vlab(input$hw_var), y = "Frequency (f)") + theme_class(fs())
    if (length(ws) > 1) p + facet_wrap(~panel, nrow = 1) else p + labs(title = levels(rects$panel))
  }, res = 96)

  # ---- Stem-and-leaf -----------------------------------------------------------------------
  output$sl_out <- renderUI({
    x <- numvar(input$sl_var)
    lines <- capture.output(stem(x, scale = input$sl_scale))
    html <- vapply(lines, function(l) {
      m <- regmatches(l, regexec("^(\\s*-?[0-9.]+) \\| ?(.*)$", l))[[1]]
      if (length(m) == 3)
        sprintf("<span class='stem-l'>%s</span> <span class='stem-b'>|</span> <span class='stem-r'>%s</span>",
                htmltools::htmlEscape(m[2]), htmltools::htmlEscape(m[3]))
      else sprintf("<span class='stem-h'>%s</span>", htmltools::htmlEscape(l))
    }, "")
    tags$pre(HTML(paste(c(sprintf("<span class='stem-h'><b>%s</b>  (n = %d)</span>",
                                  htmltools::htmlEscape(vlab(input$sl_var)), length(x)), html), collapse = "\n")))
  })

  # ---- Polygon and ogive ---------------------------------------------------------------------
  po <- reactive({
    x <- numvar(input$po_var); w <- input$po_w
    validate(need(is.numeric(w) && !is.na(w) && w > 0, "Enter a positive class width."))
    grouped(x, w)
  })
  output$po_poly <- renderPlot({
    g <- po(); v <- input$po_var; grp <- input$po_grp
    xs <- c(g$mid[1] - g$w, g$mid, g$mid[g$K] + g$w)
    if (is.null(grp) || grp == "") {
      d <- data.frame(x = xs, y = c(0, g$f, 0))
      hd <- data.frame(xmin = g$lb, xmax = g$ub, f = g$f)
      ggplot() +
        geom_rect(data = hd, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = f), fill = "#D8EFEA", colour = "white") +
        geom_line(data = d, aes(x, y), colour = pal[1], linewidth = 1.2) +
        geom_point(data = d, aes(x, y), colour = pal[1], size = 3) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.08)), breaks = integer_breaks) +
        labs(x = paste(vlab(v), "(class midpoints)"), y = "Frequency (f)", title = "Frequency polygon") +
        theme_class(fs())
    } else {
      df <- dat(); gf <- as_cat(df, grp); xv <- df[[v]]
      d <- do.call(rbind, lapply(levels(gf), function(l) {
        xi <- xv[gf == l & !is.na(gf) & !is.na(xv)]
        idx <- findInterval(xi, g$brks); f <- tabulate(idx[idx >= 1 & idx <= g$K], g$K)
        data.frame(x = xs, y = 100 * c(0, f, 0) / max(1, length(xi)), grp = sprintf("%s (n = %d)", l, length(xi)))
      }))
      ggplot(d, aes(x, y, colour = grp)) + geom_line(linewidth = 1.2) + geom_point(size = 3) +
        scale_colour_manual(values = rep_len(pal, length(unique(d$grp)))) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
        labs(x = paste(vlab(v), "(class midpoints)"), y = "% within group", colour = NULL,
             title = paste("Polygons overlaid by", tolower(vlab(grp)))) +
        theme_class(fs())
    }
  }, res = 96)
  ogive_read <- reactive({
    g <- po(); y <- c(0, 100 * g$cf / g$n)
    xv <- approx(y, g$brks, xout = input$po_p, ties = min)$y
    list(g = g, y = y, xv = xv)
  })
  output$po_ogive <- renderPlot({
    o <- ogive_read(); g <- o$g; p <- input$po_p
    d <- data.frame(x = g$brks, y = o$y)
    ggplot(d, aes(x, y)) +
      geom_area(fill = "#FDEBE4", alpha = 0.9) +
      annotate("segment", x = g$brks[1], xend = o$xv, y = p, yend = p, colour = col_median, linetype = "dashed", linewidth = 0.8) +
      annotate("segment", x = o$xv, xend = o$xv, y = p, yend = 0, colour = col_median, linetype = "dashed", linewidth = 0.8) +
      geom_line(colour = pal[2], linewidth = 1.2) + geom_point(colour = pal[2], size = 3) +
      annotate("point", x = o$xv, y = p, colour = col_median, size = 4) +
      annotate("label", x = o$xv, y = p, label = sprintf("P%d \u2248 %s", p, fmt(o$xv, g$d + 1)),
               hjust = -0.1, vjust = 1.3, size = fs() / 3.2, label.size = 0, fill = "#EEF4FE") +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25), expand = expansion(mult = c(0, 0.02))) +
      labs(x = paste(vlab(input$po_var), "(upper class boundaries)"), y = "Cumulative %", title = "Ogive") +
      theme_class(fs())
  }, res = 96)
  output$po_read <- renderUI({
    o <- ogive_read(); x <- numvar(input$po_var); p <- input$po_p
    div(class = "ok-note",
        sprintf("Reading across at %d%% and down: the %d%s percentile from the grouped ogive is about %s. From the raw values it is %s. The ogive estimate is only as fine as the classes.",
                p, p, if (p %% 10 == 1 && p != 11) "st" else if (p %% 10 == 2 && p != 12) "nd" else if (p %% 10 == 3 && p != 13) "rd" else "th",
                fmt(o$xv, o$g$d + 1), fmtn(quantile(x, p / 100, type = 7))))
  })

  # ---- Boxplot ------------------------------------------------------------------------------
  bx_data <- reactive({
    v <- req(input$bx_var); df <- dat(); x <- df[[v]]
    validate(need(is.numeric(x), "Choose a numeric variable."))
    grp <- input$bx_grp
    g <- if (is.null(grp) || grp == "") factor(rep("All patients", nrow(df))) else as_cat(df, grp)
    keep <- !is.na(x) & !is.na(g)
    data.frame(val = x[keep], g = droplevels(g[keep]))
  })
  bx_stats <- reactive({
    d <- bx_data()
    do.call(rbind, lapply(levels(d$g), function(l) {
      x <- d$val[d$g == l]; q <- quantile(x, c(.25, .5, .75), type = 7); iqr <- q[3] - q[1]
      lf <- q[1] - 1.5 * iqr; uf <- q[3] + 1.5 * iqr
      fl <- sort(x[x < lf | x > uf])
      data.frame(g = l, n = length(x), min = min(x), q1 = q[1], med = q[2], q3 = q[3], max = max(x),
                 iqr = iqr, lf = lf, uf = uf, flagged = if (length(fl)) paste(fl, collapse = ", ") else "none",
                 row.names = NULL)
    }))
  })
  output$bx_plot <- renderPlot({
    d <- bx_data(); s <- bx_stats(); v <- input$bx_var
    s$yid <- seq_len(nrow(s))
    single <- nrow(s) == 1
    p <- ggplot(d, aes(x = val, y = g)) +
      geom_boxplot(aes(fill = g), width = 0.45, coef = 1.5, alpha = 0.35, colour = "#3A4A57",
                   outlier.colour = col_mean, outlier.size = 3.5) +
      scale_fill_manual(values = rep_len(pal, nrow(s)), guide = "none") +
      scale_x_continuous(expand = expansion(mult = 0.14)) +
      labs(x = vlab(v), y = NULL) + theme_class(fs()) +
      theme(panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = "#ECEFF1"))
    if (isTRUE(input$bx_pts)) {
      dq <- merge(d, s[, c("g", "q1", "med", "q3", "lf", "uf")], by = "g")
      dq$quarter <- ifelse(dq$val < dq$lf | dq$val > dq$uf, "Flagged",
                    ifelse(dq$val < dq$q1, "Lowest quarter", ifelse(dq$val < dq$med, "Second quarter",
                    ifelse(dq$val <= dq$q3, "Third quarter", "Top quarter"))))
      dq$quarter <- factor(dq$quarter, levels = names(quarter_cols))
      p <- p + geom_jitter(data = dq, aes(colour = quarter), height = 0.1, width = 0, alpha = 0.85, size = 2.6) +
        scale_colour_manual(values = quarter_cols, name = NULL, drop = TRUE)
    }
    if (isTRUE(input$bx_fence)) {
      p <- p +
        geom_segment(data = s, aes(x = lf, xend = lf, y = yid - 0.38, yend = yid + 0.38), inherit.aes = FALSE,
                     colour = col_fence, linetype = "dashed", linewidth = 0.9) +
        geom_segment(data = s, aes(x = uf, xend = uf, y = yid - 0.38, yend = yid + 0.38), inherit.aes = FALSE,
                     colour = col_fence, linetype = "dashed", linewidth = 0.9) +
        geom_text(data = s, aes(x = uf, y = yid + 0.43, label = paste("upper fence", fmtn(uf))), inherit.aes = FALSE,
                  colour = col_fence, size = fs() / 3.6, vjust = 0, hjust = 1) +
        geom_text(data = s, aes(x = lf, y = if (single) yid + 0.43 else yid - 0.43, label = paste("lower fence", fmtn(lf))),
                  inherit.aes = FALSE, colour = col_fence, size = fs() / 3.6, vjust = if (single) 0 else 1, hjust = 0)
    }
    p <- p + theme(legend.position = "bottom")
    if (single) {
      five <- data.frame(x = c(s$min, s$q1, s$med, s$q3, s$max),
                         lab = c("Min", "Q1", "Median", "Q3", "Max"),
                         y = c(0.64, 0.40, 0.64, 0.40, 0.64))
      five$txt <- paste0(five$lab, "\n", fmtn(five$x))
      p <- p + geom_text(data = five, aes(x = x, y = y, label = txt), inherit.aes = FALSE,
                         size = fs() / 3.4, colour = "#33414D", lineheight = 0.9) +
        scale_y_discrete(expand = expansion(add = c(0.85, 0.6)))
    }
    p
  }, res = 96, height = function() {
    g <- input$bx_grp
    if (is.null(g) || g == "") 420 else 220 + 150 * length(get_levels(dat(), g))
  })
  output$bx_tbl <- renderTable({
    s <- bx_stats()
    data.frame(Group = s$g, n = s$n, Min = fmtn(s$min), Q1 = fmtn(s$q1), Median = fmtn(s$med), Q3 = fmtn(s$q3),
               Max = fmtn(s$max), IQR = fmtn(s$iqr), "Lower fence" = fmtn(s$lf), "Upper fence" = fmtn(s$uf),
               "Flagged values" = s$flagged, check.names = FALSE)
  }, striped = TRUE, align = "l")

  # ---- Shape ----------------------------------------------------------------------------------
  sh_data <- reactive({
    v <- req(input$sh_var); df <- dat(); x <- df[[v]]
    validate(need(is.numeric(x), "Choose a numeric variable."))
    grp <- input$sh_grp
    out <- data.frame(val = x[!is.na(x)], panel = "All patients")
    if (!is.null(grp) && grp != "") {
      gf <- as_cat(df, grp)
      for (l in levels(gf)) {
        xi <- x[gf == l & !is.na(gf) & !is.na(x)]
        if (length(xi)) out <- rbind(out, data.frame(val = xi, panel = paste0(vlab(grp), ": ", l)))
      }
    }
    out$panel <- factor(out$panel, levels = unique(out$panel))
    out
  })
  output$sh_plot <- renderPlot({
    d <- sh_data(); w <- input$sh_w
    validate(need(is.numeric(w) && !is.na(w) && w > 0, "Enter a positive bin width."))
    g0 <- grouped(d$val, w)
    rects <- do.call(rbind, lapply(levels(d$panel), function(pn) {
      xi <- d$val[d$panel == pn]; idx <- findInterval(xi, g0$brks)
      data.frame(xmin = g0$lb, xmax = g0$ub, f = tabulate(idx[idx >= 1 & idx <= g0$K], g0$K), panel = pn)
    }))
    rects$panel <- factor(rects$panel, levels = levels(d$panel))
    cent <- do.call(rbind, lapply(levels(d$panel), function(pn) {
      xi <- d$val[d$panel == pn]
      data.frame(panel = pn, stat = c("Mean", "Median"), value = c(mean(xi), median(xi)))
    }))
    cent$panel <- factor(cent$panel, levels = levels(d$panel))
    fills <- c("#8FCFC3", rep_len(pal[-1], max(0, nlevels(d$panel) - 1)))
    p <- ggplot() +
      geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = f, fill = panel), colour = "white", linewidth = 0.5) +
      geom_vline(data = cent, aes(xintercept = value, colour = stat, linetype = stat), linewidth = 1.1) +
      scale_fill_manual(values = setNames(fills, levels(d$panel)), guide = "none") +
      scale_colour_manual(values = c(Mean = col_mean, Median = col_median), name = NULL) +
      scale_linetype_manual(values = c(Mean = "solid", Median = "longdash"), name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.08)), breaks = integer_breaks) +
      labs(x = vlab(input$sh_var), y = "Frequency (f)") + theme_class(fs())
    if (isTRUE(input$sh_dens))
      p <- p + geom_density(data = d, aes(x = val, y = after_stat(count) * w), colour = "#3A4A57", linewidth = 0.9,
                            fill = NA, adjust = 1)
    if (nlevels(d$panel) > 1) p <- p + facet_wrap(~panel, ncol = 1)
    p
  }, res = 96, height = function() {
    g <- input$sh_grp
    if (is.null(g) || g == "") 440 else 300 + 230 * length(get_levels(dat(), g))
  })
  output$sh_tbl <- renderTable({
    d <- sh_data()
    do.call(rbind, lapply(levels(d$panel), function(pn) {
      xi <- d$val[d$panel == pn]; s <- skew_G1(xi)
      data.frame(Panel = pn, n = length(xi), Mean = fmt(mean(xi), 2), Median = fmtn(median(xi)),
                 "Mean \u2212 median" = fmt(mean(xi) - median(xi), 2),
                 "Skewness (G1)" = if (is.na(s)) "\u2014" else fmt(s, 2),
                 "Shape (rule of thumb)" = shape_verdict(s), check.names = FALSE)
    }))
  }, striped = TRUE, align = "l")

  # ---- Truncated axis -----------------------------------------------------------------------
  mx_vals <- reactive({
    m <- req(input$mx_measure); grp <- req(input$mx_grp); df <- dat()
    type <- sub(":.*", "", m); v <- sub("^[^:]*:", "", m)
    req(v %in% names(df), grp %in% names(df))
    gf <- as_cat(df, grp)
    vals <- vapply(levels(gf), function(l) {
      xi <- df[[v]][gf == l & !is.na(gf)]
      if (type == "mean") mean(xi, na.rm = TRUE) else 100 * mean(xi == "Yes", na.rm = TRUE)
    }, numeric(1))
    list(d = data.frame(g = factor(levels(gf), levels = levels(gf)), y = vals), type = type, v = v, grp = grp,
         ylab = if (type == "mean") paste("Mean", vlab(v)) else paste("% Yes \u2014", vlab(v)))
  })
  observeEvent(mx_vals(), {
    lo <- min(mx_vals()$d$y, na.rm = TRUE)
    mx <- max(0, floor(lo * 0.98))
    updateSliderInput(session, "mx_start", min = 0, max = mx, value = floor(lo * 0.9))
  })
  mx_plot <- function(start, title) {
    m <- mx_vals(); d <- m$d; top <- max(d$y) * 1.08
    lab <- if (m$type == "mean") fmt(d$y, 1) else sprintf("%.0f%%", d$y)
    ggplot(d, aes(g, y, fill = g)) + geom_col(width = 0.55) +
      geom_text(aes(label = lab), vjust = -0.4, size = fs() / 3) +
      scale_fill_manual(values = rep_len(pal, nrow(d)), guide = "none") +
      coord_cartesian(ylim = c(start, top), expand = FALSE) +
      labs(x = vlab(m$grp), y = m$ylab, title = title) + theme_class(fs())
  }
  output$mx_honest <- renderPlot(mx_plot(0, "Axis from zero"), res = 96)
  output$mx_trunc <- renderPlot({
    s <- req(input$mx_start)
    mx_plot(s, sprintf("Axis starting at %s", format(s))) +
      theme(axis.line.y = element_line(colour = col_mean, linewidth = 1.2))
  }, res = 96)
  output$mx_read <- renderUI({
    m <- mx_vals(); y <- m$d$y; s <- input$mx_start
    hi <- max(y); lo <- min(y)
    req(lo > 0, hi > s, lo > s)
    real <- hi / lo; look <- (hi - s) / (lo - s)
    div(class = if (look > 1.5 * real) "warn-note" else "ok-note",
        sprintf("In the data the larger bar is %.2f times the smaller. On the truncated axis it looks %.2f times as tall.", real, look))
  })

  # ---- Unequal class widths ----------------------------------------------------------------
  observeEvent(list(input$uq_var, input$uq_w), {
    x <- numvar(input$uq_var); w <- input$uq_w; req(is.numeric(w), !is.na(w), w > 0)
    K <- grouped(x, w)$K
    if (K >= 3) updateSliderInput(session, "uq_merge", min = 2, max = K - 1, value = min(max(2, input$uq_merge), K - 1))
  })
  output$uq_plot <- renderPlot({
    x <- numvar(input$uq_var); w <- input$uq_w
    validate(need(is.numeric(w) && !is.na(w) && w > 0, "Enter a positive class width."))
    g <- grouped(x, w)
    validate(need(g$K >= 3, "This width leaves fewer than three classes \u2014 choose a smaller width."))
    m <- min(max(2, input$uq_merge), g$K - 1); keep <- g$K - m
    d <- data.frame(xmin = c(g$lb[seq_len(keep)], g$lb[keep + 1]),
                    xmax = c(g$ub[seq_len(keep)], g$ub[g$K]),
                    f = c(g$f[seq_len(keep)], sum(g$f[(keep + 1):g$K])))
    d$width <- d$xmax - d$xmin
    d$merged <- c(rep("Equal-width classes", keep), "Merged wide class")
    dens <- input$uq_h == "density"
    d$h <- if (dens) d$f / d$width else d$f
    d$lab <- if (dens) sprintf("f = %d\nwidth %s", d$f, format(d$width)) else sprintf("f = %d", d$f)
    ggplot(d) +
      geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = h, fill = merged), colour = "white", linewidth = 0.7) +
      geom_text(aes(x = (xmin + xmax) / 2, y = h, label = lab), vjust = -0.3, size = fs() / 3.5, lineheight = 0.9) +
      scale_fill_manual(values = c("Equal-width classes" = pal[1], "Merged wide class" = pal[2]), name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
      scale_x_continuous(breaks = c(d$xmin, d$xmax[nrow(d)]), labels = fmt(c(d$xmin, d$xmax[nrow(d)]), g$d + 1)) +
      labs(x = vlab(input$uq_var), y = if (dens) "Frequency density (f \u00f7 width)" else "Frequency (f)",
           title = if (dens) "Area now represents frequency" else "Height = count: the wide class is exaggerated") +
      theme_class(fs())
  }, res = 96)

  # ---- Checkpoints -----------------------------------------------------------------------
  lapply(seq_along(cps), function(i) {
    output[[paste0("cp_fb_", i)]] <- renderUI({
      req(input[[paste0("cp_go_", i)]] > 0)
      sel <- isolate(input[[paste0("cp_", i)]]); cp <- cps[[i]]
      if (is.null(sel) || !length(sel)) return(div(class = "teach-note", "Choose an option first."))
      if (sel == cp$ans) div(class = "ok-note", strong(paste0("Correct \u2014 ", cp$ans, ". ")), cp$why)
      else div(class = "warn-note", strong(paste0("Not ", sel, ". The answer is ", cp$ans, ". ")), cp$why)
    })
  })
}

shinyApp(ui, server)
