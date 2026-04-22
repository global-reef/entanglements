#### 0.4 SCENARIO EXTRAPOLATION #####

library(tidyverse)

### empirical anchors from MARsCI ####
n_dfg_total <- 606
n_entanglement_events <- 36
mean_net_area_m2 <- 4.31

p_entanglement_per_gear <- n_entanglement_events / n_dfg_total

### observed value per entanglement event ####
# assumes fish_valued already exists from valuation script

event_values <- fish_valued %>%
  mutate(
    tier1_value_min_thb  = replace_na(tier1_value_min_thb, 0),
    tier1_value_mean_thb = replace_na(tier1_value_mean_thb, 0),
    tier1_value_max_thb  = replace_na(tier1_value_max_thb, 0)
  )

event_summary <- event_values %>%
  summarise(
    n_records = n(),
    total_indiv = sum(n_indiv, na.rm = TRUE),
    mean_event_value_min_thb  = mean(tier1_value_min_thb, na.rm = TRUE),
    mean_event_value_mean_thb = mean(tier1_value_mean_thb, na.rm = TRUE),
    mean_event_value_max_thb  = mean(tier1_value_max_thb, na.rm = TRUE),
    total_event_value_min_thb  = sum(tier1_value_min_thb, na.rm = TRUE),
    total_event_value_mean_thb = sum(tier1_value_mean_thb, na.rm = TRUE),
    total_event_value_max_thb  = sum(tier1_value_max_thb, na.rm = TRUE)
  )

### expected THB loss per effective retained gear item ####
value_per_gear <- event_summary %>%
  transmute(
    p_entanglement_per_gear = p_entanglement_per_gear,
    expected_value_per_gear_min_thb  = p_entanglement_per_gear * mean_event_value_min_thb,
    expected_value_per_gear_mean_thb = p_entanglement_per_gear * mean_event_value_mean_thb,
    expected_value_per_gear_max_thb  = p_entanglement_per_gear * mean_event_value_max_thb
  )

print(value_per_gear)

### scenario inputs ####

### literature-based gear-loss conversion ####
richardson_net_loss_low_m2  <- 1813
richardson_net_loss_high_m2 <- 58131
richardson_net_loss_mid_m2  <- mean(c(richardson_net_loss_low_m2,
                                      richardson_net_loss_high_m2))

mehrotra_mean_net_area_m2 <- 4.31

scenario_inputs <- tribble(
  ~scenario, ~active_vessels, ~raw_net_loss_m2_per_vessel_year,
  "Low",      10000,           richardson_net_loss_low_m2,
  "Medium",   40000,           richardson_net_loss_mid_m2,
  "High",     57000,           richardson_net_loss_high_m2
) %>%
  mutate(
    raw_loss_items_per_vessel_year =
      raw_net_loss_m2_per_vessel_year / mehrotra_mean_net_area_m2
  )
### retention scenarios ####
# proportion of lost gear assumed to be retained, persistent, and relevant to reef survey systems

retention_inputs <- tribble(
  ~retention_scenario, ~retention_factor,
  "Low_retention",      0.05,
  "Medium_retention",   0.10,
  "High_retention",     0.20
)

### combined national scenarios ####
scenario_outputs <- scenario_inputs %>%
  crossing(retention_inputs) %>%
  crossing(value_per_gear) %>%
  mutate(
    effective_loss_items_per_vessel_year = raw_loss_items_per_vessel_year * retention_factor,
    effective_aldfg_items_per_year = active_vessels * effective_loss_items_per_vessel_year,
    
    total_tier1_loss_min_thb  = effective_aldfg_items_per_year * expected_value_per_gear_min_thb,
    total_tier1_loss_mean_thb = effective_aldfg_items_per_year * expected_value_per_gear_mean_thb,
    total_tier1_loss_max_thb  = effective_aldfg_items_per_year * expected_value_per_gear_max_thb
  ) %>%
  select(
    scenario,
    active_vessels,
    raw_loss_items_per_vessel_year,
    retention_scenario,
    retention_factor,
    effective_loss_items_per_vessel_year,
    effective_aldfg_items_per_year,
    p_entanglement_per_gear,
    expected_value_per_gear_min_thb,
    expected_value_per_gear_mean_thb,
    expected_value_per_gear_max_thb,
    total_tier1_loss_min_thb,
    total_tier1_loss_mean_thb,
    total_tier1_loss_max_thb
  )

print(scenario_outputs, n = Inf)

### compact table for manuscript ####
scenario_table <- scenario_outputs %>%
  mutate(
    across(
      c(effective_aldfg_items_per_year,
        total_tier1_loss_min_thb,
        total_tier1_loss_mean_thb,
        total_tier1_loss_max_thb),
      round, 0
    )
  ) %>%
  select(
    scenario,
    retention_scenario,
    active_vessels,
    effective_aldfg_items_per_year,
    total_tier1_loss_min_thb,
    total_tier1_loss_mean_thb,
    total_tier1_loss_max_thb
  )

print(scenario_table, n = Inf)

### optional entanglement-probability sensitivity ####
p_sensitivity <- tribble(
  ~p_label,       ~p_ent,
  "Observed",      36 / 606,
  "Lower_50pct",  (36 / 606) * 0.5,
  "Upper_150pct", (36 / 606) * 1.5
)

scenario_sensitivity <- scenario_inputs %>%
  crossing(retention_inputs) %>%
  crossing(event_summary) %>%
  crossing(p_sensitivity) %>%
  mutate(
    effective_loss_items_per_vessel_year = raw_loss_items_per_vessel_year * retention_factor,
    effective_aldfg_items_per_year = active_vessels * effective_loss_items_per_vessel_year,
    expected_value_per_gear_mean_thb = p_ent * mean_event_value_mean_thb,
    total_tier1_loss_mean_thb = effective_aldfg_items_per_year * expected_value_per_gear_mean_thb
  ) %>%
  select(
    scenario,
    retention_scenario,
    p_label,
    active_vessels,
    raw_loss_items_per_vessel_year,
    retention_factor,
    effective_aldfg_items_per_year,
    expected_value_per_gear_mean_thb,
    total_tier1_loss_mean_thb
  )

print(scenario_sensitivity, n = Inf)

### save outputs ####
write_csv(value_per_gear, file.path(tables_dir, "value_per_gear.csv"))
write_csv(event_summary, file.path(tables_dir, "event_value_summary.csv"))
write_csv(scenario_outputs, file.path(tables_dir, "scenario_outputs_tier1_retention.csv"))
write_csv(scenario_table, file.path(tables_dir, "scenario_table_tier1_retention.csv"))
write_csv(scenario_sensitivity, file.path(tables_dir, "scenario_sensitivity_tier1_retention.csv"))

save_obj(value_per_gear, "value_per_gear.rds", dir = summ_dir)
save_obj(event_summary, "event_value_summary.rds", dir = summ_dir)
save_obj(scenario_outputs, "scenario_outputs_tier1_retention.rds", dir = summ_dir)

writeLines(
  c(
    "SCENARIO EXTRAPOLATION SUMMARY",
    paste0("Total DFG items (MARsCI): ", n_dfg_total),
    paste0("Fish entanglement events: ", n_entanglement_events),
    paste0("P(entanglement | gear): ", round(p_entanglement_per_gear, 4)),
    paste0("Mean net area (m2): ", mean_net_area_m2),
    paste0("Mean event Tier 1 THB: ", round(event_summary$mean_event_value_mean_thb, 2)),
    paste0("Expected Tier 1 THB per retained gear: ", round(value_per_gear$expected_value_per_gear_mean_thb, 2))
  ),
  con = file.path(stats_dir, "scenario_extrapolation_summary.txt")
)

### plots ####
theme_set(theme_minimal(base_size = 12))

# 1. main scenario plot
plot_scenarios <- scenario_outputs %>%
  ggplot(aes(x = retention_scenario, y = total_tier1_loss_mean_thb, fill = scenario)) +
  geom_col(position = "dodge") +
  labs(
    x = "Retention scenario",
    y = "Estimated annual Tier 1 loss (THB)",
    title = "Annual fisheries loss scenarios from retained ALDFG"
  )

ggsave(
  filename = file.path(plots_dir, "scenario_tier1_loss_retention.png"),
  plot = plot_scenarios,
  width = 8,
  height = 5,
  dpi = 300
)

# 2. heatmap
plot_heatmap <- scenario_outputs %>%
  ggplot(aes(x = retention_scenario, y = scenario, fill = total_tier1_loss_mean_thb)) +
  geom_tile() +
  labs(
    x = "Retention scenario",
    y = "Vessel / loss scenario",
    fill = "THB",
    title = "Heatmap of annual fisheries loss scenarios"
  )

ggsave(
  filename = file.path(plots_dir, "scenario_tier1_loss_heatmap.png"),
  plot = plot_heatmap,
  width = 7,
  height = 4,
  dpi = 300
)

# 3. expected THB per gear
plot_per_gear <- value_per_gear %>%
  pivot_longer(
    cols = starts_with("expected_value_per_gear"),
    names_to = "metric",
    values_to = "thb"
  ) %>%
  ggplot(aes(x = metric, y = thb)) +
  geom_col() +
  labs(
    x = NULL,
    y = "Expected THB per gear item",
    title = "Expected direct market loss per retained ALDFG item"
  ) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(
  filename = file.path(plots_dir, "expected_value_per_gear.png"),
  plot = plot_per_gear,
  width = 7,
  height = 4,
  dpi = 300
)


### FINAL VALUATION SUMMARY ####

# exchange rate (set explicitly)
thb_to_usd <- 1 / 35   # ~35 THB = 1 USD
usd_to_thb <- 35

# extract key values
mean_event_value <- event_summary$mean_event_value_mean_thb
mean_value_per_gear <- value_per_gear$expected_value_per_gear_mean_thb

# scenario bounds (THB)
total_min_thb  <- min(scenario_outputs$total_tier1_loss_min_thb, na.rm = TRUE)
total_mean_thb <- mean(scenario_outputs$total_tier1_loss_mean_thb, na.rm = TRUE)
total_max_thb  <- max(scenario_outputs$total_tier1_loss_max_thb, na.rm = TRUE)

# convert to USD
total_min_usd  <- total_min_thb  * thb_to_usd
total_mean_usd <- total_mean_thb * thb_to_usd
total_max_usd  <- total_max_thb  * thb_to_usd

# gear totals
gear_min <- min(scenario_outputs$effective_aldfg_items_per_year, na.rm = TRUE)
gear_max <- max(scenario_outputs$effective_aldfg_items_per_year, na.rm = TRUE)

summary_lines <- c(
  "==============================",
  "ALDFG ECONOMIC VALUATION SUMMARY",
  "==============================",
  "",
  "EMPIRICAL BASE (MARsCI DATA):",
  paste0("Total DFG items assessed: ", n_dfg_total),
  paste0("Fish entanglement events: ", n_entanglement_events),
  paste0("P(entanglement | gear): ", round(p_entanglement_per_gear, 4)),
  "",
  "TIER 1: DIRECT MARKET VALUE",
  paste0("Mean value per entanglement event: ",
         format(round(mean_event_value, 2), big.mark = ","), " THB (",
         format(round(mean_event_value * thb_to_usd, 2), big.mark = ","), " USD)"),
  paste0("Expected value per ALDFG item: ",
         format(round(mean_value_per_gear, 2), big.mark = ","), " THB (",
         format(round(mean_value_per_gear * thb_to_usd, 2), big.mark = ","), " USD)"),
  "",
  "SCENARIO ASSUMPTIONS:",
  paste0("Fishing vessels range: ",
         min(scenario_inputs$active_vessels), " – ",
         max(scenario_inputs$active_vessels)),
  paste0("Net loss rates (Richardson et al. 2022): 1,813 – 58,131 m2 per vessel per year"),
  paste0("Converted using mean net size (Mehrotra et al.): ",
         mean_net_area_m2, " m2 per item"),
  paste0("Retention factor range: ",
         min(retention_inputs$retention_factor), " – ",
         max(retention_inputs$retention_factor)),
  paste0("Exchange rate used: 1 USD = ", usd_to_thb, " THB"),
  "",
  "ESTIMATED ALDFG LOAD:",
  paste0("Effective ALDFG items per year: ",
         format(round(gear_min, 0), big.mark = ","), " – ",
         format(round(gear_max, 0), big.mark = ",")),
  "",
  "NATIONAL ECONOMIC IMPACT:",
  paste0("Lower bound: ",
         format(round(total_min_thb, 0), big.mark = ","), " THB (",
         format(round(total_min_usd, 0), big.mark = ","), " USD)"),
  paste0("Central (mean): ",
         format(round(total_mean_thb, 0), big.mark = ","), " THB (",
         format(round(total_mean_usd, 0), big.mark = ","), " USD)"),
  paste0("Upper bound: ",
         format(round(total_max_thb, 0), big.mark = ","), " THB (",
         format(round(total_max_usd, 0), big.mark = ","), " USD)"),
  "",
  "INTERPRETATION:",
  "Values represent scenario-based estimates of annual fisheries loss",
  "associated with reef-retained ALDFG, not total national loss.",
  "Results are sensitive to gear loss rates and retention assumptions.",
  ""
)

cat(paste(summary_lines, collapse = "\n"))

writeLines(
  summary_lines,
  con = file.path(stats_dir, "valuation_summary_final.txt")
)
