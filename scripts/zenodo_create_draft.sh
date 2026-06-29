#!/usr/bin/env bash
set -euo pipefail

TOKEN_FILE="${1:-/home/alrier/Documents/.zenodo_token}"
METADATA_JSON="${2:-/home/alrier/Documents/zenodo_stage_genetic_algo/zenodo_metadata.json}"

token="$(cat "$TOKEN_FILE")"

curl -sS \
  -H "Authorization: Bearer $token" \
  -H "Content-Type: application/json" \
  -X POST \
  https://zenodo.org/api/deposit/depositions \
  -d @"$METADATA_JSON"
echo
