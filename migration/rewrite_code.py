import csv
import os
import re
import sys

REPO_ROOT = "D:/Users/martensn/BRAIN_DRAIN"
CODE_ROOT = f"{REPO_ROOT}/Code"
MAP_CSV = f"{REPO_ROOT}/migration/data_migration_map.csv"
SCOPE_FILE = f"{REPO_ROOT}/migration/scope_files.txt"
DATA_ROOT = "P:/BRAIN_DRAIN/Data"
APPLY = "--apply" in sys.argv

# --- build old-relpath -> new-relpath lookup, excluding subsamples (unchanged) ---
lookup = {}
with open(MAP_CSV, encoding="utf-8") as f:
    for row in csv.DictReader(f):
        if row["bucket"] == "subsamples (unchanged)":
            continue
        old_rel = os.path.relpath(row["old_path"], DATA_ROOT).replace("\\", "/")
        new_rel = os.path.relpath(row["new_path"], DATA_ROOT).replace("\\", "/")
        lookup[old_rel] = new_rel

with open(SCOPE_FILE, encoding="utf-8") as f:
    scope_files = [line.strip() for line in f if line.strip()]

# matches a RUN of one or more consecutive lines that are each either an
# active or commented-out `directory <- "..."` / `directory = "..."` assignment
DIRECTORY_BLOCK_RE = re.compile(
    r'(?:^[ \t]*#?[ \t]*directory\s*(?:<-|=)\s*"[^"]*"[ \t]*\n)+',
    re.MULTILINE,
)

SNIPPET = (
    'library(dotenv)\n'
    'library(here)\n'
    'load_dot_env(here::here(".env"))\n'
    'directory <- Sys.getenv("BRAIN_DRAIN_ROOT")  # kept for any Outputs/-only uses not touched by this reorg\n'
    'data_dir  <- file.path(directory, "Data")\n'
    'out_dir   <- file.path(directory, "Outputs")\n'
)

FILEPATH_CALL_RE = re.compile(r'file\.path\(\s*(directory|data_dir)\s*,\s*([^)]*)\)')
LITERAL_RE = re.compile(r'^"([^"]*)"$')
SIMPLE_VAR_ASSIGN_RE = re.compile(r'^[ \t]*([A-Za-z_][A-Za-z0-9_.]*)\s*(?:<-|=)\s*"([^"]*)"[ \t]*$', re.MULTILINE)
IDENT_RE = re.compile(r'^[A-Za-z_][A-Za-z0-9_.]*$')


def build_var_dict(text):
    """Map varname -> literal value, but ONLY for variables assigned via a
    simple `x <- "literal"` exactly once in the file -- ambiguous (0 or 2+
    assignments) variables are left out entirely, so callers must leave
    any reference to them unresolved rather than guess."""
    counts = {}
    values = {}
    for m in SIMPLE_VAR_ASSIGN_RE.finditer(text):
        name, val = m.group(1), m.group(2)
        counts[name] = counts.get(name, 0) + 1
        values[name] = val
    return {k: v for k, v in values.items() if counts[k] == 1}


def split_args(args_text):
    # arguments here are always either "literal" or bare_identifier -- no
    # nested calls/commas-in-strings in this codebase's file.path() usage,
    # so a plain comma split is safe.
    return [a.strip() for a in args_text.split(",") if a.strip()]


def resolve_literal_path(directory_arg, literal_args_text, var_dict):
    tokens = split_args(literal_args_text)
    if not tokens:
        return None
    parts = []
    for tok in tokens:
        lit_m = LITERAL_RE.match(tok)
        if lit_m:
            parts.append(lit_m.group(1))
        elif IDENT_RE.match(tok) and tok in var_dict:
            parts.append(var_dict[tok])
        else:
            return None  # dynamic expression or unresolvable/ambiguous variable -- leave untouched
    joined = "/".join(parts)
    if directory_arg == "directory":
        joined = re.sub(r'^Data/?', '', joined)
    return joined


def make_rewriter(var_dict, counter):
    def rewrite_filepath_call(match):
        directory_arg, args_text = match.group(1), match.group(2)
        rel = resolve_literal_path(directory_arg, args_text, var_dict)
        if rel is None or rel not in lookup:
            return match.group(0)
        counter[0] += 1
        return f'file.path(data_dir,"{lookup[rel]}")'
    return rewrite_filepath_call


report = []
for path in scope_files:
    rel_script = os.path.relpath(path, CODE_ROOT).replace("\\", "/")
    with open(path, encoding="utf-8") as f:
        original = f.read()

    text = original
    var_dict = build_var_dict(original)

    m = DIRECTORY_BLOCK_RE.search(text)
    swapped_directory = False
    if m:
        text = text[:m.start()] + SNIPPET + text[m.end():]
        swapped_directory = True

    n_calls_before = len(FILEPATH_CALL_RE.findall(text))
    counter = [0]
    text = FILEPATH_CALL_RE.sub(make_rewriter(var_dict, counter), text)

    if text != original:
        report.append({
            "file": rel_script, "swapped_directory": swapped_directory,
            "filepath_calls_seen": n_calls_before, "filepath_calls_rewritten": counter[0],
        })
        if APPLY:
            with open(path, "w", encoding="utf-8", newline="\n") as f:
                f.write(text)

print(f"{'APPLIED' if APPLY else 'DRY RUN'} -- {len(report)} of {len(scope_files)} files changed")
for r in report:
    print(f"  {r['file']}: directory swapped={r['swapped_directory']}, "
          f"file.path calls seen={r['filepath_calls_seen']}, rewritten={r['filepath_calls_rewritten']}")
