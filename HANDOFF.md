# Handoff — BRAIN_DRAIN Data/ reorg + Code/ path rewrite

## Status
All 7 phases of the reorg plan are complete and pushed to GitHub
(github.com/martensn/brain_drain, private, branch `master`, tag
`pre-reorg-baseline`). Data/ reorganized by source on P:\BRAIN_DRAIN;
Code/ moved to this repo and rewritten to use dotenv/here config
instead of hardcoded machine paths. Full plan file (with the council
review that revised it) is at
D:\Users\martensn\.claude\plans\golden-cuddling-manatee.md.

## Next steps
- [ ] Commit the finished post-move hash manifest once
      migration/hash_data_postmove.log reaches "DONE" (was at 425/575
      files as of session end, running in background) -- just
      `git add migration/data_manifest_hashes_postmove.tsv
      migration/verify_paths_report.tsv && git commit`.
- [ ] Regenerate or restore `Data/intermediate/hs_alias_resp.rds` --
      confirmed truncated, pre-existing, not caused by this reorg.
      Consumed by `Code/new/02_embed_new.R` (the canonical script).
- [ ] Install real analysis packages (tidyverse, data.table, sf,
      tidycensus, etc.) into R 4.6.0 -- or use the HPC cluster --
      before actually running the pipeline. R 4.6.0 currently has only
      dotenv/here; the packages that exist on this machine sit under
      R 4.5.2, which has no working executable.
- [ ] `Data/06/microdata.csv` and `microdata_compressed.csv` don't
      exist yet -- they get created the next time `05_merge.Rmd` runs
      (already routed to intermediate/ in the rewritten code).

## Key files
- `README.md` — project layout, setup steps, Data/ taxonomy
- `.env` / `.env.example` — BRAIN_DRAIN_ROOT + secrets config
- `migration/data_migration_map.csv` — full file-by-file reorg mapping
- `migration/rewrite_code.py` — the path-rewrite script (reusable if
  another reorg is ever needed)
- `Code/_config_snippet.R` — canonical config-loading snippet

## Decisions
- Code/ lives here (D:\Users\martensn\BRAIN_DRAIN), not on the
  P:\BRAIN_DRAIN network share -- git couldn't init there (ownership).
  Data/ and Outputs/ stay on P:\BRAIN_DRAIN, referenced via
  BRAIN_DRAIN_ROOT in .env.
- Physical Data/ moves done as same-volume renames by Claude directly
  (not Globus) -- confirmed instant regardless of file size once
  tested.
- Config uses dotenv + here (installed into R 4.6.0), not a pure
  here()-only scheme -- user's preference.
- Code/Old/ excluded from the rewrite (unmaintained legacy).
- byte-identical files across pooled/pre_2000/post_2000/great_recession
  are intentional by design, not touched.
