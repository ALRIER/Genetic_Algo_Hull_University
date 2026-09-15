#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

TOKEN_FILE="${1:?token file required}"
METADATA_JSON="${2:-$REPO_ROOT/.zenodo.json}"

token="$(<"$TOKEN_FILE")"

curl --fail --silent --show-error \
  -H "Authorization: Bearer $token" \
  -H "Content-Type: application/json" \
  -X POST \
  https://zenodo.org/api/deposit/depositions \
  -d @"$METADATA_JSON"
echo
