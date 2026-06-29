#!/usr/bin/env bash
set -euo pipefail

SOURCE_ROOT="${1:-/home/alrier/Documents/Final_Results_15June2026}"
STAGING_ROOT="${2:-/home/alrier/Documents/zenodo_stage_genetic_algo}"
LABEL_FILTER="${3:-}"

mkdir -p "$STAGING_ROOT/archives"
mkdir -p "$STAGING_ROOT/manifests"

manifest_csv="$STAGING_ROOT/manifests/source_tree_sizes.csv"
if [[ -z "$LABEL_FILTER" ]]; then
  printf 'label,source_path,size_bytes\n' > "$manifest_csv"
fi

while IFS='|' read -r label relpath; do
  if [[ -n "$LABEL_FILTER" && "$label" != "$LABEL_FILTER" ]]; then
    continue
  fi
  src="$SOURCE_ROOT/$relpath"
  out="$STAGING_ROOT/archives/${label}.tar.zst"
  size_bytes="$(du -sb "$src" | awk '{print $1}')"
  if ! rg -q "^${label}," "$manifest_csv" 2>/dev/null; then
    printf '%s,%s,%s\n' "$label" "$src" "$size_bytes" >> "$manifest_csv"
  fi
  if [[ -f "$out" ]]; then
    printf '[SKIP] %s already exists\n' "$out"
    continue
  fi
  printf '[PACK] %s\n' "$label"
  tar --zstd -cf "$out" -C "$SOURCE_ROOT" "$relpath"
done <<'EOF'
00_readme_and_guides|00_README_AND_GUIDES
01_discovery_monte_carlo_60_80|01_DISCOVERY_MONTE_CARLO_60_80
02_fixed_weight_validation_nest10_raw|02_ORIGINAL_POST_DISCOVERY_FIXED_WEIGHT_VALIDATION_NEST10
03_expanded_discovery_nest26|03_EXPANDED_DISCOVERY_NEST26
04_fixed_weight_validation_nest26_raw|04_EXPANDED_POST_DISCOVERY_FIXED_WEIGHT_VALIDATION_NEST26
05_evidence_taxonomy_nest26|05_EVIDENCE_TAXONOMY_NEST26
06_realworld_external_battery_40datasets|06_REALWORLD_EXTERNAL_BATTERY_40DATASETS
07_code_archive|07_CODE_ARCHIVE
08_random_dirichlet_abstain_audit|08_RANDOM_DIRICHLET_ABSTAIN_AUDIT
top_level_docs|README.md
top_level_realworld_analysis_md|analisis_resultados_real_world.md
top_level_realworld_analysis_pdf|analisis_resultados_real_world.pdf
top_level_realworld_analysis_docx|analisis_resultados_real_world.docx
EOF

(
  cd "$STAGING_ROOT"
  sha256sum archives/* > manifests/archives_sha256.txt
)

printf '[DONE] Archives in %s/archives\n' "$STAGING_ROOT"
