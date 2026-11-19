# Create a stub page notes/<id>.qmd for every entry in predictions.yaml that
# does not have one yet. NEVER overwrites an existing file, so pages with
# hand-written notes are safe. Run from predictions/:
#   Rscript _make_notes.R

library(yaml)

entries <- yaml::read_yaml("predictions.yaml")
dir.create("notes", showWarnings = FALSE)

yaml_str <- function(x) paste0('"', gsub('"', '\\\\"', gsub("\\\\", "\\\\\\\\", x)), '"')

made <- 0
for (x in entries) {
  id <- x$id
  if (is.null(id) || !nzchar(id)) stop("Entry without id: ", x$prediction)
  f <- file.path("notes", paste0(id, ".qmd"))
  if (file.exists(f)) next
  lines <- c(
    "---",
    paste0("title: ", yaml_str(x$prediction)),
    paste0("pred-id: ", id),
    "toc: false",
    "format:",
    "  html:",
    "    css: ../predictions.css",
    "---",
    "",
    "```{r}",
    "#| echo: false",
    "#| output: asis",
    'source("../_predictions.R")',
    sprintf('pred_cat_html(pred_card_html("%s"))', id),
    "```",
    "",
    "## Notes",
    "",
    "<!-- Write the longer case here. -->",
    "",
    "[← All predictions](../index.qmd)",
    ""
  )
  writeLines(lines, f, useBytes = FALSE)
  made <- made + 1
}
message(made, " stub(s) created; ", length(entries) - made, " already existed.")
