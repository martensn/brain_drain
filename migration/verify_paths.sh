#!/bin/bash
set -u
DATA_ROOT="/p/BRAIN_DRAIN/Data"
IN="/tmp/rewritten_paths.txt"
OUT="/d/Users/martensn/BRAIN_DRAIN/migration/verify_paths_report.tsv"
> "$OUT"

total=0
ok=0
missing=0
bad_zip=0

while IFS= read -r relpath; do
  total=$((total+1))
  path="$DATA_ROOT/$relpath"
  if [ ! -f "$path" ]; then
    echo -e "MISSING\t$relpath\t" >> "$OUT"
    missing=$((missing+1))
    continue
  fi
  ext="${relpath##*.}"
  size=$(stat -c%s "$path" 2>/dev/null)
  case "$ext" in
    zip|xlsx)
      if unzip -l "$path" > /dev/null 2>&1; then
        echo -e "OK_ZIP\t$relpath\t${size} bytes, valid archive" >> "$OUT"
        ok=$((ok+1))
      else
        echo -e "BAD_ZIP\t$relpath\t${size} bytes, INVALID archive structure" >> "$OUT"
        bad_zip=$((bad_zip+1))
      fi
      ;;
    csv|txt)
      lines=$(wc -l < "$path" 2>/dev/null)
      echo -e "OK_TEXT\t$relpath\t${size} bytes, ${lines} lines" >> "$OUT"
      ok=$((ok+1))
      ;;
    rds)
      echo -e "PENDING_RDS\t$relpath\t${size} bytes" >> "$OUT"
      ;;
    *)
      echo -e "OK_OTHER\t$relpath\t${size} bytes, extension .$ext (existence only)" >> "$OUT"
      ok=$((ok+1))
      ;;
  esac
done < "$IN"

echo "total=$total ok=$ok missing=$missing bad_zip=$bad_zip" >&2
