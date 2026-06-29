# Code Cleanup Record — Genetic_Algo_Hull_University

This document records code-maintenance changes applied to the R files in this repository.
The guiding rule is simple: change presentation and maintainability only
(comments, diagnostic scaffolding, duplicate non-numerical helpers), not the
numerical core. Modified files were checked with diffs that isolate executable
changes from comments, blank lines, and module-tracking fallbacks.

Before replacing any production archive, numerical equivalence should still be
checked by running representative reference and current pipelines and comparing
checksums for representative CSV outputs.

---

## Qué se cambió y por qué

### 1. Bloque de seguimiento de módulos (el cambio principal)
El código original repetía en casi todos los módulos un bloque de ~22 líneas que
redefinía `.MOD_STATUS`, `mark_module_done()` e `is_module_done()`. Ese bloque es
**puramente diagnóstico**: imprime mensajes `[MODULE DONE]` y guarda marcas de tiempo.

**Verificación de seguridad:** se comprobó en todo el código que `is_module_done()`
solo se invoca dentro de `if (!exists("is_module_done", ...))` — es decir, jamás se
usa para decidir si se ejecuta un cálculo. No controla flujo numérico. Por tanto,
reemplazar la copia duplicada por un *fallback* no-op de pocas líneas no altera ningún
resultado.

- La **definición real** de estas funciones vive en `00_utils_debug_io.R` (canónico),
  que se carga primero en una ejecución normal. **Ahí se conserva intacta.**
- En cada módulo que la duplicaba, el bloque se reemplazó por un guard mínimo:
  ```r
  if (!exists("mark_module_done", mode = "function", inherits = TRUE)) {
    mark_module_done <- function(module_id, extra = NULL) invisible(TRUE)
    is_module_done   <- function(module_id) FALSE
  }
  ```
- Se eliminaron las llamadas sueltas `mark_module_done("...")` (solo imprimían).

### 2. Helpers de depuración muertos en `00_utils_debug_io.R`
Se eliminaron `.debug_topk_diagnose()` y `.debug_stack()` tras verificar que **no se
invocan en ningún archivo** de su fase. Son utilidades de diagnóstico sin uso.

### 3. Comentarios
Se tradujeron algunos comentarios de español/spanglish a inglés y se aclararon
variables opacas. Ningún comentario afecta la ejecución.

---

## Estado por fase

| Fase | Carpeta | Archivos | Estado |
|------|---------|----------|--------|
| 1 | `code/1_discovery_nest10/` | 12 | Limpiados + verificados |
| 2 | `code/2_fixed_weight_validation_nest10/` | 9 | Ya limpios — sin cambios (byte-idénticos) |
| 3 | `code/3_validation_nest26/` | 11 | Limpiados + verificados |
| 4 | `code/4_fixed_weight_validation_nest26/` | 9 | Ya limpios — sin cambios (byte-idénticos) |
| 5 | `code/5_realworld_external_battery/` | 10 | Limpiados + verificados |
| 6 | `code/6_random_dirichlet_abstain_audit/` | 1 | Ya limpio — sin cambios |

Total: 52 archivos físicos, 48 contenidos únicos.

---

## Decisión sobre archivos repetidos: fases autocontenidas (opción elegida)

Se optó por **mantener cada fase autocontenida**: no se creó carpeta `common/` ni se
tocaron las llamadas `source()`. Esto es 100 % seguro respecto a la carga de módulos.

Hay **4 pares de archivos idénticos** entre fases. Se conservan a propósito como copias
para que cada fase corra de forma independiente:

| Archivo (idéntico) | Aparece en |
|--------------------|------------|
| `02_scenarios_sampling.R` | fase 1 y fase 3 |
| `03_distributions_params.R` | fase 1 y fase 3 |
| `10_distributional_diagnostics.R` | fase 1 y fase 3 |
| `02_expanded_benchmarks.R` | fase 2 y fase 4 |

`random_dirichlet_gate_audit.R` reside físicamente dentro de la fase 4 en el repo
original; aquí se ubica como su propia carpeta `6_random_dirichlet_abstain_audit/`
(contenido único, una sola copia).

Todos los demás archivos con el mismo nombre entre fases tienen **contenido distinto**
(p. ej. `04_estimators_registry.R` difiere entre N_EST=10, N_EST=26 y realworld), por
lo que deben existir por separado.

---

## Verification Method

Para cada archivo modificado se comparó contra el original ignorando: comentarios,
líneas en blanco y el bloque de seguimiento de módulos (probado como no-numérico).
Resultado en todos los casos: **código no-tracking idéntico**.

La verificación numérica definitiva (ejecutar en R y comparar CSV de resultados) queda
del lado del usuario.

---

## Corrección importante (verificación final)

Durante la verificación final contra el ZIP completo se detectó que **3-4 archivos de
la fase 5 (realworld)** existían en dos versiones distintas: una versión antigua (que
se había tomado por error de una copia previa) y la versión vigente del ZIP.

- `13_realworld_external_battery.R` — versión vigente: 1699 líneas (la previa tenía 654).
- `realworld_external_battery_utils.R` — versión vigente del ZIP.
- `run_realworld_external_battery.R` — versión vigente del ZIP.
- `generate_external_report.R` — versión vigente del ZIP.

**Cómo se determinó cuál es la correcta:** los CSV de resultados reales del proyecto
(p. ej. `external_methods_audit_flags.csv`) contienen columnas que **solo** produce la
versión nueva (FDR `fdr_policy`, `oracle_audit_secondary_only`, `wins_saturation_cap`,
`futility_stop_enabled`, `depth_probe_secondary_only`). Por tanto la versión del ZIP es
la que generó los resultados publicados. `clean_repo` ahora contiene esa versión correcta.

Estos 4 archivos no tenían bloque de tracking ni comentarios en español, así que se
incluyeron tal cual (sin modificaciones), igual que las fases 2 y 4.

## Verificación final — resumen

- 52 archivos del ZIP representados en `clean_repo`.
- 48 contenidos únicos; 4 pares idénticos conservados a propósito (fases autocontenidas).
- Comparación final contra el ZIP: los únicos cambios fuera del bloque de tracking son
  los **intencionales y documentados**:
  - poda de `.debug_topk_diagnose` / `.debug_stack` en los tres `00_utils_debug_io.R`
    (verificados como no usados);
  - eliminación de `isTRUE(TRUE) &` en `04_estimators_registry.R` (fase 1), lógicamente neutro.
- No quedan divergencias inesperadas.

---

## Refactor real — reducción de complejidad (no cosmética)

### `08_LocationEstimators.R` (fases 1 y 3) — APLICADO
Se encontraron **8 funciones definidas dos veces** en el mismo archivo. En R la segunda
definición sobrescribe a la primera antes de que el archivo se use, y se verificó que la
primera copia **nunca se invoca** entre ambas definiciones → es código muerto inalcanzable.

Funciones afectadas: `.regime_rate_bin`, `.regime_scale_bin`, `.regime_sample_size_bin`,
`.add_regime_columns`, `.allowed_benchmarks_for_distribution`, `.summarize_one_regime`,
`build_regime_discovery_summary`, `select_regimes_for_specialist_training`.

- Fase 1: 3647 → 3374 líneas (−273)
- Fase 3: 4147 → 3874 líneas (−273)
- Verificado: todas las funciones activas quedan **idénticas**; cada una definida 1 sola vez.
- Test de equivalencia en R: `TEST_08_equivalencia.R` (ejecutar antes de aceptar).

### Barrido de duplicados en los 52 archivos
Los **únicos** duplicados top-level en todo el proyecto son los del `08` (arriba) y un
caso en `05_ga_core.R` de la fase 3 (ver abajo). El resto del código no tiene funciones
redefinidas.

### `05_ga_core.R` (fase 3) — NO TOCADO (requiere decisión en R)
`summarize_scenario` está definida **dos veces** y, a diferencia del `08`, **ambas
versiones están en uso**:
- Copia 1 (L157, 157 líneas) = idéntica a la versión de la fase 1; la usa la llamada
  interna de la L1165.
- Copia 2 (L1558, 49 líneas) = versión más corta y distinta; sobrescribe a la copia 1
  tras cargarse el módulo, por lo que la llamada desde `07_fitness_objectives.R` (L992)
  usaría esta versión reducida.

Eliminar cualquiera de las dos **cambiaría comportamiento** según el orden de carga.
Esto NO es código muerto y se deja **sin modificar**. Recomendación: decidir en R cuál
versión es la deseada y comparar resultados antes de unificar. Posible bug latente.
