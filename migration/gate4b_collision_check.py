import csv
import re
import os
from collections import defaultdict

MAP_CSV = "D:/Users/martensn/BRAIN_DRAIN/migration/data_migration_map.csv"
REFS_CSV = "D:/Users/martensn/BRAIN_DRAIN/migration/code_path_refs.csv"

# basename -> list of old_paths (excluding junk/deleted, excluding subsamples since those aren't moving)
basename_to_paths = defaultdict(list)
with open(MAP_CSV, encoding="utf-8") as f:
    for row in csv.DictReader(f):
        if row["bucket"] in ("_review/junk",):
            continue
        b = os.path.basename(row["old_path"])
        basename_to_paths[b].append(row["old_path"])

ambiguous_basenames = {b: paths for b, paths in basename_to_paths.items() if len(paths) > 1}

STRING_RE = re.compile(r'"([^"]+)"')

total_refs = 0
ambiguous_refs = 0
ambiguous_rows = []

with open(REFS_CSV, encoding="utf-8") as f:
    for row in csv.DictReader(f):
        total_refs += 1
        text = row["raw_text"]
        literals = STRING_RE.findall(text)
        hit = False
        matched_basenames = set()
        for lit in literals:
            base = lit.split("/")[-1]
            if base in ambiguous_basenames:
                hit = True
                matched_basenames.add(base)
        if hit:
            ambiguous_refs += 1
            ambiguous_rows.append((row["file"], row["line"], sorted(matched_basenames)))

print(f"Total call sites: {total_refs}")
print(f"Ambiguous (filename matches >1 distinct Data file): {ambiguous_refs}")
print(f"Distinct ambiguous basenames involved: {len(ambiguous_basenames)}")
print()
for b, paths in sorted(ambiguous_basenames.items()):
    print(f"  {b}: {len(paths)} candidates -> {paths}")
print()
print("Call sites hitting an ambiguous basename:")
for f, line, basenames in ambiguous_rows:
    print(f"  {f}:{line}  ({', '.join(basenames)})")
