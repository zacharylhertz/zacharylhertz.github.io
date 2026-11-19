# Shared helpers for the predictions pages (index.qmd, notes/<id>.qmd).
# Reads predictions.yaml and scores every entry.
#
# Scoring rules:
#   call    the side the prediction backed. confidence >= 50 backs "Yes" (a
#           50% call backs the statement as written), < 50 backs "No".
#   p       stated probability of the backed side (always >= 0.5); a 30% "Yes"
#           counts as a 70% "No", as in the old _scorecard.R.
#   status  Correct / Wrong / Open / Void.
#   brier   (confidence/100 - [outcome == "Yes"])^2 for resolved entries.
#           This equals (p - [backed side came true])^2.

# Quiet loads: version-mismatch warnings would otherwise print on the page.
suppressWarnings(suppressPackageStartupMessages({
  library(yaml)
  library(htmltools)
}))

pred_effort_levels <- c("Quick take", "Considered", "Deep dive")
pred_status_levels <- c("Correct", "Wrong", "Open", "Void")

# Find predictions.yaml from predictions/ or from predictions/notes/.
pred_yaml_path <- function() {
  for (p in c("predictions.yaml", "../predictions.yaml")) if (file.exists(p)) return(p)
  stop("predictions.yaml not found from ", getwd())
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

pred_load <- function(path = pred_yaml_path()) {
  raw <- yaml::read_yaml(path)
  d <- do.call(rbind, lapply(seq_along(raw), function(i) {
    x <- raw[[i]]
    data.frame(
      order = i,
      id = as.character(x$id %||% sprintf("prediction-%02d", i)),
      prediction = as.character(x$prediction %||% ""),
      criteria = as.character(x$criteria %||% ""),
      confidence = as.numeric(x$confidence),
      effort = as.character(x$effort %||% ""),
      basis = as.character(x$basis %||% ""),
      made = as.character(x$made %||% ""),
      resolves = as.character(x$resolves %||% ""),
      outcome = as.character(x$outcome %||% "Open"),
      categories = paste(unlist(x$categories), collapse = ", "),
      source = as.character(x$source %||% ""),
      result = as.character(x$result %||% ""),
      notes = as.character(x$notes %||% ""),
      stringsAsFactors = FALSE
    )
  }))
  d$call <- ifelse(d$confidence >= 50, "Yes", "No")
  d$p <- pmax(d$confidence, 100 - d$confidence) / 100
  d$resolved <- d$outcome %in% c("Yes", "No")
  d$status <- ifelse(d$outcome == "Void", "Void",
              ifelse(!d$resolved, "Open",
              ifelse(d$call == d$outcome, "Correct", "Wrong")))
  d$brier <- ifelse(d$resolved, (d$confidence / 100 - (d$outcome == "Yes"))^2, NA_real_)
  # Calibration: did the backed side come true? A 50% entry is counted on its
  # "Yes" side, as in the old _scorecard.R.
  d$came_true <- ifelse(d$resolved,
                        ifelse(d$confidence >= 50, d$outcome == "Yes", d$outcome == "No"),
                        NA)
  d
}

pred_brier <- function(d) mean(d$brier[d$resolved])

pred_counts <- function(d) {
  n <- table(factor(d$status, levels = pred_status_levels))
  c(total = nrow(d), resolved = sum(d$resolved), as.list(n)) |> unlist()
}

# One row for all resolved predictions, then one per effort level.
pred_score_rows <- function(d) {
  r <- d[d$resolved, ]
  groups <- c(list(All = rep(TRUE, nrow(r))),
              setNames(lapply(pred_effort_levels, function(e) r$effort == e), pred_effort_levels))
  do.call(rbind, lapply(names(groups), function(g) {
    i <- groups[[g]]
    data.frame(group = g, n = sum(i),
               correct = sum(r$status[i] == "Correct"),
               wrong = sum(r$status[i] == "Wrong"),
               brier = if (any(i)) mean(r$brier[i]) else NA_real_)
  }))
}

pred_calibration <- function(d) {
  r <- d[d$resolved, ]
  lo <- c(50, 60, 70, 80, 90)
  out <- do.call(rbind, lapply(lo, function(a) {
    b <- r[100 * r$p >= a & 100 * r$p < a + 10, ]
    data.frame(
      bin = sprintf("%d–%d%%", a, a + 9),
      lo = a,
      n = nrow(b),
      stated = if (nrow(b)) mean(b$p) else NA_real_,
      share_true = if (nrow(b)) mean(b$came_true) else NA_real_
    )
  }))
  out
}

# ---- HTML pieces -----------------------------------------------------------

esc <- function(x) htmltools::htmlEscape(x)

# Result as a plain word, coloured by status; the hover text gives the result.
pred_badge <- function(status, title = NULL) {
  cls <- tolower(gsub(" ", "-", status))
  t <- if (!is.null(title) && nzchar(title)) sprintf(' title="%s"', esc(title)) else ""
  sprintf('<span class="pred-status pred-status-%s"%s>%s</span>', cls, t, esc(status))
}

pred_fmt3 <- function(x) ifelse(is.na(x), "–", sprintf("%.3f", x))
pred_fmtpct <- function(x) ifelse(is.na(x), "–", sprintf("%.0f%%", 100 * x))

pred_scorecard_html <- function(d) {
  sr <- pred_score_rows(d)
  rows <- paste(sprintf(
    '<tr%s><td>%s</td><td class="num">%d</td><td class="num">%d</td><td class="num">%d</td><td class="num">%s</td></tr>',
    ifelse(sr$group == "All", ' class="pred-score-all"', ""),
    ifelse(sr$group == "All", "All predictions", esc(sr$group)),
    sr$n, sr$correct, sr$wrong, pred_fmt3(sr$brier)), collapse = "")
  paste0(
    '<div class="pred-score-wrap"><table class="table table-sm pred-score-table">',
    '<thead><tr><th></th><th class="num">Resolved</th><th class="num">Correct</th><th class="num">Wrong</th><th class="num">Brier score</th></tr></thead>',
    '<tbody>', rows, '</tbody></table></div>',
    '<p class="pred-note">The <a href="https://en.wikipedia.org/wiki/Brier_score">Brier Score</a> is a measure of mean squared error for predictions, where 1 is perfect inaccuracy and 0 is perfect accuracy. A perpetual fence-sitter who assigns a probability of 50% every time would have a Brier score of 0.25. Lower is better.</p>'
  )
}

pred_calibration_table_html <- function(d) {
  cal <- pred_calibration(d)
  rows <- paste(sprintf('<tr><td>%s</td><td class="num">%d</td><td class="num">%s</td><td class="num">%s</td></tr>',
                        cal$bin, cal$n, pred_fmtpct(cal$stated), pred_fmtpct(cal$share_true)), collapse = "")
  paste0('<div class="pred-cal-wrap"><table class="table table-sm pred-cal-table">',
         '<thead><tr><th>Stated probability</th><th class="num">Resolved</th><th class="num">Average stated</th><th class="num">Share that came true</th></tr></thead>',
         '<tbody>', rows, '</tbody></table></div>')
}

pred_calibration_plot <- function(d) {
  suppressWarnings(suppressPackageStartupMessages(library(ggplot2)))
  cal <- pred_calibration(d)
  cal <- cal[cal$n > 0, ]
  cal$label <- sprintf("n = %d", cal$n)
  # Labels sit up and to the left of each point (every bin so far lies on or
  # above the 45-degree line, so this keeps them off the line). The offset
  # grows with the point's radius (scale_size_area: radius ~ sqrt(n)).
  cal$lx <- 100 * cal$stated - (0.9 + 2.1 * sqrt(cal$n / max(cal$n)))
  cal$ly <- 100 * cal$share_true + 1.2
  ink <- "#343a40"; muted <- "#6c757d"; accent <- "#495057"
  ggplot(cal, aes(x = 100 * stated, y = 100 * share_true)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = muted, linewidth = 0.4) +
    geom_point(aes(size = n), colour = accent, alpha = 0.85) +
    geom_text(aes(x = lx, y = ly, label = label), colour = ink,
              size = 3.6, hjust = 1, vjust = 0.5) +
    scale_size_area(max_size = 9, guide = "none") +
    scale_x_continuous(breaks = seq(50, 100, 10), labels = function(x) paste0(x, "%")) +
    scale_y_continuous(breaks = seq(50, 100, 10), labels = function(x) paste0(x, "%")) +
    coord_equal(xlim = c(50, 100), ylim = c(50, 100), expand = TRUE, clip = "off") +
    labs(x = "Stated probability (average in bin)", y = "Share that came true") +
    theme_minimal(base_size = 13) +
    theme(text = element_text(colour = ink),
          axis.text = element_text(colour = muted),
          panel.grid.minor = element_blank(),
          plot.margin = margin(8, 12, 8, 8))
}

pred_table_html <- function(d) {
  d <- d[order(-as.numeric(as.Date(d$made)), d$order), ]
  eff_rank <- match(d$effort, pred_effort_levels)
  rows <- paste(sprintf(paste0(
    '<tr class="pred-row pred-row-%s" data-order="%d" data-topic="%s" data-status="%s" data-conf="%s" data-effort="%d" data-made="%s">',
    '<td class="pred-topic">%s</td>',
    '<td class="pred-text"><a href="notes/%s.html">%s</a></td>',
    '<td class="pred-conf num">%s</td>',
    '<td class="pred-eff">%s</td>',
    '<td class="pred-made">%s</td>',
    '<td class="pred-result">%s</td></tr>'),
    tolower(gsub(" ", "-", d$status)), d$order, esc(d$categories), esc(d$status),
    d$confidence, ifelse(is.na(eff_rank), 0L, eff_rank), d$made,
    esc(d$categories), esc(d$id), esc(d$prediction), d$confidence, esc(d$effort), d$made,
    mapply(pred_badge, d$status, d$result)), collapse = "\n")
  th <- function(key, label, cls = "") sprintf(
    '<th class="%s" data-key="%s"><button type="button" class="pred-sort">%s<span class="pred-sort-ind" aria-hidden="true"></span></button></th>',
    cls, key, label)
  topics <- sort(unique(trimws(unlist(strsplit(d$categories, ",")))))
  topics <- topics[nzchar(topics)]
  paste0(
    '<div class="pred-filters">',
    '<div class="pred-filter"><label for="pred-topic-filter">Topic</label>',
    '<select id="pred-topic-filter" class="form-select form-select-sm"><option value="">All topics</option>',
    paste(sprintf('<option value="%s">%s</option>', esc(topics), esc(topics)), collapse = ""),
    '</select></div>',
    '<div class="pred-filter"><label for="pred-result-filter">Result</label>',
    '<select id="pred-result-filter" class="form-select form-select-sm"><option value="">All</option>',
    paste(sprintf('<option value="%s">%s</option>', pred_status_levels[1:3], pred_status_levels[1:3]), collapse = ""),
    '</select></div>',
    '<div class="pred-shown" aria-live="polite"></div>',
    '</div>',
    '<div class="pred-table-wrap"><table class="table pred-table" id="pred-table" data-quarto-disable-processing="true">',
    '<thead><tr>',
    th("topic", "Topic", "pred-topic"), th("text", "Prediction", "pred-text"),
    th("conf", "Probability (%)", "pred-conf num"), th("effort", "Effort", "pred-eff"),
    th("made", "Made", "pred-made"), th("status", "Result", "pred-result"),
    '</tr></thead><tbody>\n', rows, '\n</tbody></table></div>'
  )
}

pred_card_html <- function(id, d = pred_load()) {
  x <- d[d$id == id, ]
  if (nrow(x) != 1) stop("No unique prediction with id ", id)
  row <- function(label, value) sprintf('<dt>%s</dt><dd>%s</dd>', label, value)
  src <- if (nzchar(x$source)) sprintf('<a href="%s">%s</a>', esc(x$source), esc(x$source)) else "–"
  paste0(
    '<div class="pred-card">',
    '<div class="pred-card-head">', pred_badge(x$status),
    sprintf('<span class="pred-card-prob">%s%% chance of Yes</span>', x$confidence), '</div>',
    '<dl class="pred-card-fields">',
    row("Effort", esc(x$effort)),
    row("Basis", esc(x$basis)),
    row("Made", esc(x$made)),
    row("Resolves by", esc(x$resolves)),
    row("Resolves Yes if", esc(x$criteria)),
    row("Outcome", esc(x$outcome)),
    row("Result", if (nzchar(x$result)) esc(x$result) else "–"),
    row("Source", src),
    row("Case for / against (as pre-registered)", esc(x$notes)),
    '</dl></div>'
  )
}

# Print raw HTML from an `output: asis` chunk without Pandoc touching it.
pred_cat_html <- function(html) cat("\n```{=html}\n", html, "\n```\n\n", sep = "")
