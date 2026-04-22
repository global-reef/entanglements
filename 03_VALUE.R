#### 0.3 VALUATION #####

library(tidyverse)

fish.dfg <- entanglements_joined
str(fish.dfg)
glimpse(fish.dfg)

### tier 1 - direct market values #####

fish.dfg <- fish.dfg %>%
  mutate(
    tier1_value_min_thb  = n_indiv * price_min_thb,
    tier1_value_mean_thb = n_indiv * price_mean_thb,
    tier1_value_max_thb  = n_indiv * price_max_thb,
    tier1_priced = !is.na(price_mean_thb),
    tier1_price_match = case_when(
      !is.na(price_mean_thb) ~ "priced",
      TRUE ~ "unpriced"
    )
  )

### tier 3 - aesthetic / socio-economic proxy #####

fish.dfg <- fish.dfg %>%
  mutate(
    tier3_aesthetic_index = n_indiv * aesthetic_score_mean,
    tier3_scored = !is.na(aesthetic_score_mean),
    tier3_match_confidence = case_when(
      assumption_level == "direct" ~ "high",
      assumption_level == "proxy"  ~ "medium",
      TRUE                         ~ "low"
    ),
    tier3_match_type = case_when(
      Match == "L" ~ "langlois_direct",
      Match == "T" ~ "tribot_direct",
      Match == "P" ~ "proxy",
      Match == "B" ~ "broad_proxy",
      TRUE ~ "unknown"
    )
  )

### row-level valuation output #####

fish_valued <- fish.dfg %>%
  select(
    survey_id, date, site, location, province, gear_type,
    taxon_raw, sci_name, common_name, genus, family,
    taxon_level, taxon_confidence,
    n_alive, n_dead, n_indiv, status,
    trophic_group, herbivore_class, predator_class,
    functional_group, category, ecosystem_role, ecosystem_benefit,
    price_min_thb, price_mean_thb, price_max_thb, price_sd_thb, price_n,
    tier1_priced, tier1_price_match,
    tier1_value_min_thb, tier1_value_mean_thb, tier1_value_max_thb,
    tribot_2018_taxon, langlois_2022_taxon,
    tribot_2018_score, langlois_2022_score, aesthetic_score_mean,
    score_source, assumption_level, Match,
    tier3_scored, tier3_match_type, tier3_match_confidence,
    tier3_aesthetic_index
  )

### summary tables #####

valuation_totals <- fish_valued %>%
  summarise(
    n_rows = n(),
    total_indiv = sum(n_indiv, na.rm = TRUE),
    n_tier1_priced = sum(tier1_priced, na.rm = TRUE),
    prop_tier1_priced = mean(tier1_priced, na.rm = TRUE),
    tier1_total_min_thb  = sum(tier1_value_min_thb, na.rm = TRUE),
    tier1_total_mean_thb = sum(tier1_value_mean_thb, na.rm = TRUE),
    tier1_total_max_thb  = sum(tier1_value_max_thb, na.rm = TRUE),
    n_tier3_scored = sum(tier3_scored, na.rm = TRUE),
    prop_tier3_scored = mean(tier3_scored, na.rm = TRUE),
    tier3_total_aesthetic_index = sum(tier3_aesthetic_index, na.rm = TRUE)
  )

valuation_by_taxon <- fish_valued %>%
  group_by(sci_name, common_name, family, functional_group, ecosystem_benefit) %>%
  summarise(
    n_records = n(),
    total_indiv = sum(n_indiv, na.rm = TRUE),
    tier1_priced_n = sum(tier1_priced, na.rm = TRUE),
    tier1_value_min_thb  = sum(tier1_value_min_thb, na.rm = TRUE),
    tier1_value_mean_thb = sum(tier1_value_mean_thb, na.rm = TRUE),
    tier1_value_max_thb  = sum(tier1_value_max_thb, na.rm = TRUE),
    tier3_scored_n = sum(tier3_scored, na.rm = TRUE),
    tier3_aesthetic_index = sum(tier3_aesthetic_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(tier1_value_mean_thb))

valuation_by_site <- fish_valued %>%
  group_by(site, location, province) %>%
  summarise(
    n_records = n(),
    total_indiv = sum(n_indiv, na.rm = TRUE),
    tier1_value_min_thb  = sum(tier1_value_min_thb, na.rm = TRUE),
    tier1_value_mean_thb = sum(tier1_value_mean_thb, na.rm = TRUE),
    tier1_value_max_thb  = sum(tier1_value_max_thb, na.rm = TRUE),
    tier3_aesthetic_index = sum(tier3_aesthetic_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(tier1_value_mean_thb))

tier1_missing <- fish_valued %>%
  filter(!tier1_priced) %>%
  count(taxon_raw, sci_name, common_name, sort = TRUE)

tier3_missing <- fish_valued %>%
  filter(!tier3_scored) %>%
  count(taxon_raw, sci_name, common_name, sort = TRUE)

tier3_confidence_summary <- fish_valued %>%
  group_by(tier3_match_confidence) %>%
  summarise(
    n_records = n(),
    total_indiv = sum(n_indiv, na.rm = TRUE),
    tier3_aesthetic_index = sum(tier3_aesthetic_index, na.rm = TRUE),
    .groups = "drop"
  )

### save outputs #####

# row-level cleaned valuation file
write_csv(fish_valued, file.path(data_clean_dir, "fish_valued.csv"))

# tables for manuscript / inspection
write_csv(valuation_totals, file.path(tables_dir, "valuation_totals.csv"))
write_csv(valuation_by_taxon, file.path(tables_dir, "valuation_by_taxon.csv"))
write_csv(valuation_by_site, file.path(tables_dir, "valuation_by_site.csv"))
write_csv(tier1_missing, file.path(tables_dir, "tier1_missing_prices.csv"))
write_csv(tier3_missing, file.path(tables_dir, "tier3_missing_scores.csv"))
write_csv(tier3_confidence_summary, file.path(tables_dir, "tier3_confidence_summary.csv"))

# optional rds versions
save_obj(fish_valued, "fish_valued.rds", dir = summ_dir)
save_obj(valuation_totals, "valuation_totals.rds", dir = summ_dir)
save_obj(valuation_by_taxon, "valuation_by_taxon.rds", dir = summ_dir)
save_obj(valuation_by_site, "valuation_by_site.rds", dir = summ_dir)

# text summary
writeLines(
  c(
    "VALUATION SUMMARY",
    paste0("Rows: ", nrow(fish_valued)),
    paste0("Total individuals: ", sum(fish_valued$n_indiv, na.rm = TRUE)),
    paste0("Tier 1 priced rows: ", sum(fish_valued$tier1_priced, na.rm = TRUE)),
    paste0("Tier 1 mean total (THB): ", round(sum(fish_valued$tier1_value_mean_thb, na.rm = TRUE), 2)),
    paste0("Tier 3 scored rows: ", sum(fish_valued$tier3_scored, na.rm = TRUE)),
    paste0("Tier 3 total aesthetic index: ", round(sum(fish_valued$tier3_aesthetic_index, na.rm = TRUE), 2))
  ),
  con = file.path(stats_dir, "valuation_summary.txt")
)

### plots #####

theme_set(theme_minimal(base_size = 12))

# 1. top taxa by tier 1 mean value
plot_t1_taxa <- valuation_by_taxon %>%
  filter(!is.na(sci_name), tier1_value_mean_thb > 0) %>%
  slice_max(order_by = tier1_value_mean_thb, n = 10) %>%
  ggplot(aes(x = reorder(sci_name, tier1_value_mean_thb), y = tier1_value_mean_thb)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Tier 1 mean value (THB)",
    title = "Top taxa by direct market value"
  )

ggsave(
  filename = file.path(plots_dir, "tier1_top_taxa_mean_value.png"),
  plot = plot_t1_taxa,
  width = 8,
  height = 5,
  dpi = 300
)

# 2. top sites by tier 1 mean value
plot_t1_sites <- valuation_by_site %>%
  filter(tier1_value_mean_thb > 0) %>%
  slice_max(order_by = tier1_value_mean_thb, n = 10) %>%
  ggplot(aes(x = reorder(site, tier1_value_mean_thb), y = tier1_value_mean_thb)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Tier 1 mean value (THB)",
    title = "Sites with highest direct market value loss"
  )

ggsave(
  filename = file.path(plots_dir, "tier1_top_sites_mean_value.png"),
  plot = plot_t1_sites,
  width = 8,
  height = 5,
  dpi = 300
)

# 3. top taxa by tier 3 aesthetic index
plot_t3_taxa <- valuation_by_taxon %>%
  filter(!is.na(sci_name), tier3_aesthetic_index > 0) %>%
  slice_max(order_by = tier3_aesthetic_index, n = 10) %>%
  ggplot(aes(x = reorder(sci_name, tier3_aesthetic_index), y = tier3_aesthetic_index)) +
  geom_col() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Tier 3 aesthetic index",
    title = "Top taxa by aesthetic value proxy"
  )

ggsave(
  filename = file.path(plots_dir, "tier3_top_taxa_aesthetic_index.png"),
  plot = plot_t3_taxa,
  width = 8,
  height = 5,
  dpi = 300
)

# 4. coverage plot for tier 1 vs tier 3
coverage_df <- tibble(
  tier = c("Tier 1", "Tier 3"),
  covered = c(
    sum(fish_valued$tier1_priced, na.rm = TRUE),
    sum(fish_valued$tier3_scored, na.rm = TRUE)
  ),
  total = nrow(fish_valued)
) %>%
  mutate(prop = covered / total)

plot_coverage <- ggplot(coverage_df, aes(x = tier, y = prop)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = NULL,
    y = "Coverage",
    title = "Valuation coverage by tier"
  )

ggsave(
  filename = file.path(plots_dir, "valuation_coverage_by_tier.png"),
  plot = plot_coverage,
  width = 6,
  height = 4,
  dpi = 300
)

# 5. tier 3 contribution by confidence
plot_t3_conf <- tier3_confidence_summary %>%
  ggplot(aes(x = tier3_match_confidence, y = tier3_aesthetic_index)) +
  geom_col() +
  labs(
    x = "Tier 3 match confidence",
    y = "Tier 3 aesthetic index",
    title = "Aesthetic value contribution by match confidence"
  )

ggsave(
  filename = file.path(plots_dir, "tier3_confidence_contribution.png"),
  plot = plot_t3_conf,
  width = 6,
  height = 4,
  dpi = 300
)

### print #####

print(valuation_totals)
print(valuation_by_taxon, n = Inf)
print(valuation_by_site, n = Inf)
print(tier1_missing, n = Inf)
print(tier3_missing, n = Inf)
print(tier3_confidence_summary, n = Inf)