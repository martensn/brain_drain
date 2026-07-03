import csv
import os
import sys
import shutil

MAP_CSV = "D:/Users/martensn/BRAIN_DRAIN/migration/data_migration_map.csv"
LOG_CSV = "D:/Users/martensn/BRAIN_DRAIN/migration/move_log.csv"
DRY_RUN = "--execute" not in sys.argv

with open(MAP_CSV, encoding="utf-8") as f:
    rows = list(csv.DictReader(f))

to_move = [r for r in rows if r["bucket"] != "subsamples (unchanged)"]

# 1. figure out every destination directory we need
dest_dirs = sorted(set(os.path.dirname(r["new_path"]) for r in to_move))

print(f"{'[DRY RUN] ' if DRY_RUN else ''}Files to move: {len(to_move)} of {len(rows)} total")
print(f"Destination directories to create: {len(dest_dirs)}")

if not DRY_RUN:
    for d in dest_dirs:
        os.makedirs(d, exist_ok=True)

log_rows = []
moved = 0
skipped_missing = 0
skipped_exists = 0
errors = 0

for r in to_move:
    old_path = r["old_path"]
    new_path = r["new_path"]
    expected_size = int(r["size"])

    if not os.path.exists(old_path):
        skipped_missing += 1
        log_rows.append({"old_path": old_path, "new_path": new_path, "status": "SKIP_MISSING_SOURCE", "detail": ""})
        continue

    if os.path.exists(new_path):
        skipped_exists += 1
        log_rows.append({"old_path": old_path, "new_path": new_path, "status": "SKIP_DEST_EXISTS", "detail": ""})
        continue

    if DRY_RUN:
        moved += 1
        continue

    try:
        shutil.move(old_path, new_path)
        actual_size = os.path.getsize(new_path)
        if actual_size != expected_size:
            errors += 1
            log_rows.append({"old_path": old_path, "new_path": new_path, "status": "SIZE_MISMATCH",
                              "detail": f"expected {expected_size}, got {actual_size}"})
        else:
            moved += 1
            log_rows.append({"old_path": old_path, "new_path": new_path, "status": "OK", "detail": f"{actual_size} bytes"})
    except Exception as e:
        errors += 1
        log_rows.append({"old_path": old_path, "new_path": new_path, "status": "ERROR", "detail": str(e)})

print(f"Moved{'  (would move)' if DRY_RUN else ''}: {moved}")
print(f"Skipped (source missing): {skipped_missing}")
print(f"Skipped (dest already exists): {skipped_exists}")
print(f"Errors: {errors}")

if not DRY_RUN:
    with open(LOG_CSV, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=["old_path", "new_path", "status", "detail"])
        writer.writeheader()
        for lr in log_rows:
            writer.writerow(lr)
    print(f"Log written to {LOG_CSV}")
