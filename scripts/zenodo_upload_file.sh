#!/usr/bin/env bash
set -euo pipefail

TOKEN_FILE="${1:?token file required}"
BUCKET_URL="${2:?bucket URL required}"
FILE_PATH="${3:?file path required}"

token="$(<"$TOKEN_FILE")"
filename="$(basename "$FILE_PATH")"

curl --fail --progress-bar \
  -H "Authorization: Bearer $token" \
  --upload-file "$FILE_PATH" \
  "${BUCKET_URL}/${filename}"
echo
