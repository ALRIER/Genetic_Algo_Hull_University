# verify_clean.R
# Checks that all R files parse and that module-tracking helpers do not control flow.
# Run from the repository root: Rscript verify_clean.R
# This is a syntax and structure check, not a numerical equivalence test.

files <- list.files("code", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
cat(sprintf("Checking %d R files...\n\n", length(files)))

ok <- TRUE
for (f in files) {
  res <- tryCatch({ parse(f); "OK" },
                  error = function(e) paste("PARSE ERROR:", conditionMessage(e)))
  if (res != "OK") { ok <- FALSE; cat(sprintf("  [FAIL] %s -> %s\n", f, res)) }
}
if (ok) cat("\n[OK] All R files parse successfully.\n")

# is_module_done should appear only in fallback guards or in its own definition.
cat("\nis_module_done uses outside the standard fallback guard:\n")
flagged <- FALSE
for (f in files) {
  ln <- readLines(f, warn = FALSE)
  hits <- grep("is_module_done", ln, value = TRUE)
  bad  <- hits[!grepl("!exists\\(\"is_module_done\"", hits) & !grepl("is_module_done\\s*<-", hits)]
  if (length(bad)) { flagged <- TRUE; cat(sprintf("  %s: %s\n", f, paste(bad, collapse=" | "))) }
}
if (!flagged) cat("  (none; module tracking does not control execution flow)\n")

cat("\nReminder: numerical equivalence requires running the original and cleaned\n")
cat("pipelines and comparing checksums for representative result CSVs.\n")
