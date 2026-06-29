# verify_clean.R
# Verifica integridad sintáctica del repo limpio y que el bloque de tracking no controla flujo.
# Ejecutar desde la raíz de final_repo:  Rscript verify_clean.R
# NOTA: esto NO prueba equivalencia numérica; para eso corre el pipeline y compara md5 de CSV.

files <- list.files("code", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
cat(sprintf("Verificando %d archivos...\n\n", length(files)))

ok <- TRUE
for (f in files) {
  res <- tryCatch({ parse(f); "OK" },
                  error = function(e) paste("PARSE ERROR:", conditionMessage(e)))
  if (res != "OK") { ok <- FALSE; cat(sprintf("  [FALLA] %s -> %s\n", f, res)) }
}
if (ok) cat("\n[OK] Todos los archivos parsean correctamente.\n")

# is_module_done solo debe aparecer dentro de if(!exists(...)) o en su propia definicion
cat("\nUsos de is_module_done que NO sean el guard estandar:\n")
flagged <- FALSE
for (f in files) {
  ln <- readLines(f, warn = FALSE)
  hits <- grep("is_module_done", ln, value = TRUE)
  bad  <- hits[!grepl("!exists\\(\"is_module_done\"", hits) & !grepl("is_module_done\\s*<-", hits)]
  if (length(bad)) { flagged <- TRUE; cat(sprintf("  %s: %s\n", f, paste(bad, collapse=" | "))) }
}
if (!flagged) cat("  (ninguno: confirmado que el tracking nunca controla flujo)\n")

cat("\nRecordatorio: la prueba definitiva es ejecutar el pipeline con codigo original\n")
cat("y con el limpio, y comparar el md5sum de un CSV de resultados.\n")
