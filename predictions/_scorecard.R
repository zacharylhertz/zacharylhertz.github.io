# Scorecard code removed from index.qmd on 2026-11-02 at ZH's request; kept for when predictions resolve.
# It rendered a Brier score and a calibration table from predictions.yaml.
library(yaml)
p <- yaml::read_yaml("predictions.yaml")
d <- do.call(rbind, lapply(p, function(x) data.frame(
  confidence = as.numeric(x$confidence),
  outcome = x$outcome,
  stringsAsFactors = FALSE
)))
res <- d[d$outcome %in% c("Yes", "No"), ]
# Fold predictions stated below 50% into their complement, so a 30% "Yes"
# counts as a 70% "No". Each prediction then has a stated chance >= 50%.
res$p <- ifelse(res$confidence >= 50, res$confidence, 100 - res$confidence) / 100
res$right <- ifelse(res$confidence >= 50, res$outcome == "Yes", res$outcome == "No")
brier <- mean((res$p - res$right)^2)
n_open <- sum(d$outcome == "Open")
bins <- c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0001)
labs <- c("50-59%", "60-69%", "70-79%", "80-89%", "90-99%")
res$bin <- cut(res$p, bins, labels = labs, right = FALSE)
tab <- do.call(rbind, lapply(split(res, res$bin), function(b) data.frame(
  n = nrow(b),
  stated = if (nrow(b)) sprintf("%.0f%%", 100 * mean(b$p)) else "",
  came_true = if (nrow(b)) sprintf("%.0f%%", 100 * mean(b$right)) else ""
)))
tab <- cbind(Confidence = rownames(tab), tab)
knitr::kable(tab, row.names = FALSE, align = "lrrr",
  col.names = c("Stated confidence", "Resolved", "Average stated", "Share that came true"))
