
# Requires col_p1-col_p4 (and data_dir) already in the session from running
# new/02_embed_new.R first (canonical producer; supersedes new/01_col_hs_crosswalk.R).

col_p5 = readRDS(file.path(data_dir,"col_embed.rds")) %>%
  group_by(university_name,rsid,unitid,opeid,system_indicator) %>%
  summarize(N = sum(N))



still = col_p1 %>%
  filter(!rsid %in% col_p2$rsid) %>%
  filter(!rsid %in% col_p3$rsid) %>%
  filter(!rsid %in% col_p4$rsid) %>%
  filter(!rsid %in% col_p5$rsid)