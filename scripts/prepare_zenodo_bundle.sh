#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
SOURCE_ROOT="${1:-$REPO_ROOT}"
STAGING_ROOT="${2:?staging directory required}"

mkdir -p "$STAGING_ROOT"

ARCHIVE_PATH="$STAGING_ROOT/genetic_algo_hull_university.tar.zst"
CHECKSUM_PATH="$STAGING_ROOT/genetic_algo_hull_university.sha256"

printf '[PACK] %s\n' "$SOURCE_ROOT"
tar --zstd \
  --exclude='.git' \
  --exclude='*.RDS' \
  --exclude='*.rds' \
  -cf "$ARCHIVE_PATH" \
  -C "$SOURCE_ROOT" .

sha256sum "$ARCHIVE_PATH" > "$CHECKSUM_PATH"
printf '[DONE] %s\n' "$ARCHIVE_PATH"
