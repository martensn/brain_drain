#!/bin/bash
set -u
OUT="/d/Users/martensn/BRAIN_DRAIN/migration/data_manifest_hashes.tsv"
> "$OUT"
total=$(wc -l < "/d/Users/martensn/BRAIN_DRAIN/migration/data_manifest_raw.tsv")
i=0
while IFS=$'\t' read -r size mtime path; do
  i=$((i+1))
  if [ "$size" -eq 0 ]; then
    hash="EMPTY_FILE"
  else
    hash=$(sha256sum "$path" 2>/dev/null | awk '{print $1}')
    if [ -z "$hash" ]; then hash="HASH_ERROR"; fi
  fi
  printf '%s\t%s\t%s\t%s\n' "$hash" "$size" "$mtime" "$path" >> "$OUT"
  if [ $((i % 25)) -eq 0 ]; then
    echo "progress: $i / $total files hashed" >&2
  fi
done < "/d/Users/martensn/BRAIN_DRAIN/migration/data_manifest_raw.tsv"
echo "DONE: hashed $i files" >&2
