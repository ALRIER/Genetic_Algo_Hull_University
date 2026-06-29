# TEST_08_equivalencia.R  — valida 08_LocationEstimators.R refactorizado (fases 1 y 3)
# El refactor SOLO elimina copias muertas de 8 funciones definidas dos veces.
# Edita ORIG_1 y ORIG_3 con las rutas a tus 08 ORIGINALES.
ORIG_1 <- "ruta/ORIGINAL/discovery_nest10/08_LocationEstimators.R"     # <-- EDITA
NEW_1  <- "code/1_discovery_nest10/08_LocationEstimators.R"
ORIG_3 <- "ruta/ORIGINAL/validation_nest26/08_LocationEstimators.R"    # <-- EDITA
NEW_3  <- "code/3_validation_nest26/08_LocationEstimators.R"

check_pair <- function(orig, new, tag) {
  cat("\n==== ", tag, " ====\n")
  stopifnot(tryCatch({parse(orig);parse(new);TRUE}, error=function(e){cat("no parsea:",conditionMessage(e),"\n");FALSE}))
  eO<-new.env(); eN<-new.env(); sys.source(orig,eO); sys.source(new,eN)
  oO<-sort(ls(eO)); oN<-sort(ls(eN)); ok<-identical(oO,oN)
  if(!ok) cat("Objetos difieren | solo ORIG:",setdiff(oO,oN)," solo NEW:",setdiff(oN,oO),"\n")
  for(nm in oN){ fO<-get(nm,eO); fN<-get(nm,eN)
    if(is.function(fO) && !identical(deparse(fO),deparse(fN))){ok<-FALSE;cat("DIFIERE:",nm,"\n")} }
  rates<-c(NA,0,0.01,0.05,0.15,0.30); scales<-c(NA,0,1,3,6,12); ns<-c(10,30,100,500,5000)
  cf<-function(fn,xs) identical(sapply(xs,get(fn,eO)),sapply(xs,get(fn,eN)))
  for(fn in c(".regime_rate_bin",".regime_scale_bin",".regime_sample_size_bin")){
    xs<-if(fn==".regime_rate_bin")rates else if(fn==".regime_scale_bin")scales else ns
    if(!cf(fn,xs)){ok<-FALSE;cat(fn,"difiere funcionalmente\n")} }
  cat(if(ok)"OK EQUIVALENTE\n" else "DIFERENCIAS - no usar\n"); ok
}
r1<-check_pair(ORIG_1,NEW_1,"FASE 1 nest10")
r3<-check_pair(ORIG_3,NEW_3,"FASE 3 nest26")
cat(if(r1&&r3)"\n>> AMBOS 08 EQUIVALENTES\n" else "\n>> revisar\n")
