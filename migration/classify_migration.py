import csv
import os
import re
from collections import defaultdict

DATA_ROOT = "P:/BRAIN_DRAIN/Data"
MANIFEST = "D:/Users/martensn/BRAIN_DRAIN/migration/data_manifest_raw.tsv"
OUT = "D:/Users/martensn/BRAIN_DRAIN/migration/data_migration_map.csv"

def to_windows_path(p):
    # data_manifest_raw.tsv was built via git-bash `find`, so it has
    # /p/BRAIN_DRAIN/... paths -- python.exe is a native Windows binary
    # and needs P:/BRAIN_DRAIN/... instead.
    return p.replace("/p/BRAIN_DRAIN", "P:/BRAIN_DRAIN", 1) if p.startswith("/p/BRAIN_DRAIN") else p

# matched against basename only (folder location doesn't matter)
JUNK_BASENAME_PATTERNS = [
    r"^\.DS_Store$",
    r"^~\$.*\.xlsx$",
    r"\.zip\.download$",
]
# matched anywhere in the full relative path (folder/structural cruft)
JUNK_PATH_PATTERNS = [
    r"AUHelperService",
    r"\.ArchiveServiceTemp\.",
    r"^07/Latin-Modern-Roman/",  # bundled LaTeX/PDF font files, not data
    r"\.zip\.download(/|$)",  # incomplete downloads, including everything nested inside
]

# (regex, bucket, confidence, note) -- matched against basename via re.fullmatch unless noted
BASENAME_RULES = [
    (r"ELSI_csv_export_.*", "raw/nces", "high", "NCES ELSI export"),
    (r"Public\.csv|Private\.csv|Public15\.xlsx|Private15\.xlsx", "raw/nces", "high", "NCES CCD school universe files"),
    (r"WholeData Data Dictionary\.pdf", "raw/nces", "medium", "NCES/CCD data dictionary (zero-byte)"),
    (r"tabn318\.10\.xlsx", "raw/nces", "high", "NCES Digest of Education Statistics table"),

    (r"colleges_ipeds_.*", "raw/ipeds", "high", "IPEDS survey component export"),
    (r"ipeds_hd2021\.csv|ipeds_adm2021\.csv|ipeds_ef2020c\.csv|ipeds_hd2020_dictionary\.xlsx|IPEDS codebook\.xlsx",
     "raw/ipeds", "high", "IPEDS HD/EF/admissions files"),
    (r"ipeds_deg_awarded\.rds|ipeds_deg_awarded_field", "intermediate", "medium", "IPEDS degree-award data reshaped/summarized"),

    (r"EducationDataPortal_.*datadictionary\.csv", "raw/urban_institute", "high", "Urban Institute Ed Data Portal"),
    (r"Postsecondary\.csv", "raw/urban_institute", "medium", "likely Urban Institute Ed Data Portal (zero-byte)"),

    (r"ACSDT1Y.*", "raw/census_acs", "high", "ACS Detailed Tables"),
    (r"State_of_Residence_By_Place_of_Birth.*", "raw/census_acs", "high", "ACS nativity/migration table"),

    (r"CBSACrosswalk\.csv|msa_2020\.(csv|xlsx)|unified_cbsa\.csv|counties\.xlsx|ct_counties\.xlsx|zipcodes\.csv|cbsa_2020\.csv",
     "raw/census_geo", "high", "Census/OMB geography crosswalk"),
    (r"CW\d{4}.*\.xlsx", "raw/ipeds", "high", "IPEDS master crosswalk linking subunits to institutional identifiers, confirmed by user"),

    (r"cbp\d{2}co\.txt", "raw/cbp", "high", "Census County Business Patterns bulk file"),
    (r"efsy_panel_naics\.csv", "raw/cbp", "high", "external EFSY imputed CBP panel"),

    (r"chetty_college_inc_dist\.xlsx|chetty_closed_colleges\.csv|chetty_college\.xlsx",
     "raw/chetty_oi", "high", "Opportunity Insights / Chetty mobility data"),
    (r"mrc_table.*", "raw/chetty_oi", "high", "Mobility Report Card table"),
    (r"col_users$", "raw/chetty_oi", "high", "misleadingly named -- confirmed Chetty mobility data via header, not Revelio"),

    (r"bls_laus_data\.csv", "raw/bls", "high", "BLS LAUS bulk download"),
    (r"soc_2010_to_2018_crosswalk\.xlsx", "raw/bls", "high", "BLS SOC classification crosswalk"),
    (r"OccupationalPrestigeRatings\.csv", "raw/bls", "medium", "occupational prestige scale (raw source table)"),

    (r"barrons01\.pdf|barrons_raw\.xlsx|barrons_mod_endowment\.xlsx|barrons_mod\.xlsx",
     "raw/rankings_membership", "high", "Barron's college selectivity rankings"),
    (r"AAU( 2)?\.xlsx", "raw/rankings_membership", "high", "AAU membership list"),

    (r"Grawe\.xlsx", "raw/other", "high", "Nathan Grawe demographic projections (zero-byte)"),

    (r"2026\.04\.09_education\.csv|2026\.04\.09_demographics\.csv\.zip|STATES_AK-MD\.csv",
     "raw/revelio", "high", "Revelio raw education/demographics panel"),
    (r"2026\.04\.(09|10)_pos_\d{4}\.csv\.zip", "raw/revelio", "high", "Revelio raw position-history panel (large file)"),
    (r"cleaned_pos_educ_STATES_.*\.csv", "raw/revelio", "medium", "Revelio position+education panel, lightly cleaned"),
    (r"raw_microdata\.csv", "raw/revelio", "high", "Revelio raw microdata sample"),
    (r"education_0000_part_00\.csv|education_cleaned\.csv", "raw/revelio", "medium", "Revelio education export (zero-byte)"),
    (r"raw_educ_STATES.*", "raw/revelio", "high", "Revelio raw education panel, chunked by state"),

    (r"occupational_prestige\.csv", "intermediate", "medium", "merge of raw prestige scale + BLS employment + Revelio shares"),
    (r"raw_deg_award", "intermediate", "high", "valid RDS (no extension), written by new/00_alias_generation.R, read by 09_plots.Rmd"),

    (r"cc_alias.*|col_alias.*|hs_alias.*|cc_strings.*|col_strings.*|hs_strings.*|col_gt\.csv|hs_gt\.csv|col_ct.*"
     r"|col_wo_rsid.*|rsid_wo_col.*|dupe_match_rsid.*|cc_rsid_crosswalk.*|col_rsid_crosswalk.*|hs_rsid_crosswalk.*"
     r"|rsid_id_dup_crosswalk.*|unmatched_cc.*|unmatched_col.*|unmatched_hs.*|matched_col.*|matched_hs.*"
     r"|raw_unmatched_strings.*|hs_col_classification\.csv|classification_sample_1\.csv|llm_1_unk.*|llm_1_us.*"
     r"|misfit.*|institution_crosswalk\.csv|inst_counts\.csv|uwdbhdlkbnzhaqxd\.csv|match_problems\.xlsx"
     r"|persistent_match_issues\.xlsx|both_demo\.rds|cc\.rds|col_embed\.rds|hs_embed\.rds",
     "intermediate", "high", "entity-resolution / name-matching pipeline output"),

    (r"both_final\.rds|both_orig\.rds|input_col\.rds|input_col_m\.rds|schools\.rds|colleges\.rds|ed_wide\.rds"
     r"|state_fips\.rds|education\.xlsx|microdata_sample\.csv",
     "intermediate", "high", "merged/matched panel, model input"),

    (r"msa_string_cleaning.*|shares\.csv|shifts\.csv|w_share\.csv|cbsa_shock\.csv|li_inst\.csv|local_stocks\.csv"
     r"|state_stocks\.csv|return\.csv|return_occp\.csv|inst_names\.csv|institutional_characteristics\.csv"
     r"|intercensal\.csv|net_cbsa\.csv|net_state\.csv|occp_inst_group\.csv|recruitment\.csv|tie_stocks.*"
     r"|total_deg\.csv|all_deg\.csv|business_deg\.csv|engineering_deg\.csv|CollegeMarketShares_state\.csv"
     r"|cohort_return_.*|cohort_x_grad.*|diff_cbsa\.csv|local_differences.*|destination_cbsa\.csv|destination_state\.csv"
     r"|fraction\.csv|numerator\.csv|soc_ba\.csv|soc_merge\.csv|soc_li_distribution\.csv|instate_li\.csv"
     r"|local_ties_.*|hs_shock_year_.*|return_by_year\.csv|return_levels_.*|no_prestige_soc\.csv|inst_levels\.csv"
     r"|sample_values\.csv|col_shock\.csv|hs_shock\.csv",
     "intermediate", "medium", "per-stage pipeline intermediate"),

    (r"regression_data\.csv|coef_data.*|group_semi_elasticities\.csv|bin_.*|all_bin_shares_.*"
     r"|directed_\d{4}_\d{4}\.csv|returners_.*|summary_demographics.*|all_binary.*|actual_cohort\.csv|totals_cbsa.*",
     "results", "medium", "regression/figure-ready output"),

    (r"outsample_values|restrict_values", "_review/needs_inspection", "high", "extensionless file, content not yet verified"),

    (r"be_sch\.csv|be_sch_full\.csv|be_sch_grad\.csv", "intermediate", "high",
     "user-created alias/crosswalk for academic subunits, supplementing raw Revelio data (confirmed by user)"),
    (r"occupation\.xlsx", "raw/bls", "high", "BLS occupation classification file (confirmed by user)"),
    (r"pf\.xlsx", "raw/other", "high",
     "user-created list of public flagship institutions -- a manually authored reference input, not a script-derived intermediate (confirmed by user)"),
    (r"finance_raw\.csv", "intermediate", "high",
     "user's modification of an IPEDS finance file, not raw despite the name (confirmed by user)"),
    (r"ipeds_finance_troubleshoot\.xlsx", "intermediate", "high",
     "a more extensive user modification of IPEDS finance data (confirmed by user)"),
]

# rules that DO need folder context (ambiguous basename, or only meaningful within a specific stage folder)
PATH_RULES = [
    (r"^01/Crosswalks/mrc_table", "raw/chetty_oi", "high", "Mobility Report Card table"),
    (r"^Paper/chetty_sel_dist_stats\.xlsx$", "results", "medium", "paper-ready summary stat derived from Chetty data, not raw itself"),
    (r"^make_transfer_inventory\.py$|^transfer_code\.csv$|^transfer_inventory\.csv$|^transfer_outputs\.csv$",
     "_review/junk", "high", "filesystem inventory tooling, unrelated to analysis data"),
]

def classify(relpath, basename, size):
    for pattern, bucket, conf, note in PATH_RULES:
        if re.search(pattern, relpath):
            return (bucket, conf, note)
    if size == 0:
        return ("_review/needs_inspection", "high", "zero-byte file")
    for pattern, bucket, conf, note in BASENAME_RULES:
        if re.fullmatch(pattern, basename):
            return (bucket, conf, note)
    return (None, None, None)

def is_junk(relpath, basename):
    for pat in JUNK_BASENAME_PATTERNS:
        if re.search(pat, basename):
            return True
    for pat in JUNK_PATH_PATTERNS:
        if re.search(pat, relpath):
            return True
    return False

rows = []
with open(MANIFEST, encoding="utf-8") as f:
    reader = csv.reader(f, delimiter="\t")
    for size_s, mtime, fullpath_raw in reader:
        size = int(size_s)
        fullpath = to_windows_path(fullpath_raw)
        relpath = os.path.relpath(fullpath, DATA_ROOT).replace("\\", "/")
        basename = os.path.basename(relpath)

        if relpath.startswith(("pooled/", "pre_2000/", "post_2000/", "great_recession/")):
            rows.append({
                "old_path": fullpath, "new_path": fullpath, "size": size,
                "bucket": "subsamples (unchanged)", "confidence": "high",
                "note": "user-confirmed intentional shared structure, not touched by source-based reorg",
            })
            continue

        if is_junk(relpath, basename):
            rows.append({
                "old_path": fullpath, "new_path": f"{DATA_ROOT}/_review/junk/{basename}", "size": size,
                "bucket": "_review/junk", "confidence": "high",
                "note": "OS/Office/download cruft, consolidated here for deletion in Phase 7",
                "_relpath": relpath, "_basename": basename,
            })
            continue

        bucket, conf, note = classify(relpath, basename, size)

        if bucket is None:
            rows.append({
                "old_path": fullpath, "new_path": "", "size": size,
                "bucket": "UNCLASSIFIED", "confidence": "none",
                "note": "no rule matched -- needs manual classification",
            })
            continue

        new_path = f"{DATA_ROOT}/{bucket}/{basename}"
        rows.append({"old_path": fullpath, "new_path": new_path, "size": size,
                      "bucket": bucket, "confidence": conf, "note": note,
                      "_relpath": relpath, "_basename": basename})

# disambiguate genuine collisions (excluding junk, which is never "moved" anywhere)
dest_map = defaultdict(list)
for r in rows:
    if r["new_path"] and r["new_path"] != "(DELETE)":
        dest_map[r["new_path"]].append(r)

def stage_suffix(relpath):
    parts = relpath.split("/")
    if len(parts) > 1 and re.fullmatch(r"0[1-7]", parts[0]):
        return parts[0]
    return "top"

for new_path, group in dest_map.items():
    if len(group) <= 1:
        continue
    for r in group:
        stem, ext = os.path.splitext(r["new_path"])
        suffix = stage_suffix(r["_relpath"])
        r["new_path"] = f"{stem}__{suffix}{ext}"
        r["confidence"] = "COLLISION-RESOLVED"
        r["note"] = (f"{r['note']} || was a basename collision with "
                      f"{len(group)-1} other file(s); disambiguated with a "
                      f"__{suffix} stage suffix pending your review -- sizes differ "
                      f"substantially across the group, these are NOT simple duplicates")

# safety net: the stage suffix alone can still collide (e.g. two files under the
# same numbered folder but different subfolders) -- guarantee uniqueness with a
# numeric fallback for anything still colliding after the pass above
final_dest_map = defaultdict(list)
for r in rows:
    if r["new_path"]:
        final_dest_map[r["new_path"]].append(r)
for new_path, group in final_dest_map.items():
    if len(group) <= 1:
        continue
    for i, r in enumerate(group, start=1):
        stem, ext = os.path.splitext(r["new_path"])
        r["new_path"] = f"{stem}-{i}{ext}"
        r["confidence"] = "COLLISION-RESOLVED"
        r["note"] = f"{r['note']} || still collided after stage-suffixing; appended numeric fallback -{i}"

for r in rows:
    r.pop("_relpath", None)
    r.pop("_basename", None)

with open(OUT, "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=["old_path", "new_path", "size", "bucket", "confidence", "note"])
    writer.writeheader()
    for r in rows:
        writer.writerow(r)

total = len(rows)
by_bucket = defaultdict(int)
by_conf = defaultdict(int)
for r in rows:
    by_bucket[r["bucket"]] += 1
    by_conf[r["confidence"]] += 1

print(f"Total files: {total}")
print("\nBy bucket:")
for k, v in sorted(by_bucket.items(), key=lambda x: -x[1]):
    print(f"  {k}: {v}")
print("\nBy confidence:")
for k, v in sorted(by_conf.items(), key=lambda x: -x[1]):
    print(f"  {k}: {v}")
