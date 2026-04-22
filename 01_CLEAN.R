### 01. CLEAN #####

library(readxl)
library(lubridate)
library(janitor)
library(tidyverse)

### 00. load core sheets ####
xlsx_path <- "data_raw/MARsci-Entangled-Fauna-Raw-Data.xlsx"

fish <- read_excel(xlsx_path, sheet = "FishDataset")
category <- read_excel(xlsx_path, sheet = "Animals CategoryCode")
esvd <- read_excel(xlsx_path, sheet = "ESVD Ecosysterm Service Values")
full <- read_excel(xlsx_path, sheet = "Full Dataset")

### 01. raw entanglement data ==> clean ##### 
entanglements_clean <- fish %>%
  rename(
    survey_id = Survey,
    date = Date,
    site = Site,
    location = Location,
    province = Province,
    gear_type = Gear,
    depth_min_m = `Depth MIN (m)`,
    depth_max_m = `Depth MAX (m)`,
    taxon_raw = `Full Name`,
    family_raw = Family,
    category_raw = Category,
    n_alive = `Living animals`,
    n_dead = `Dead animals`,
    n_indiv = `Total Number of Animals`
  ) %>%
  mutate(
    date = parse_date_time(date, orders = c("dmy", "ymd", "mdy")),
    
    taxon_raw = str_squish(str_replace_all(taxon_raw, "[\r\n]+", " ")),
    
    gear_type = case_when(
      str_detect(gear_type, regex("net", TRUE)) ~ "net",
      str_detect(gear_type, regex("rope", TRUE)) ~ "rope",
      str_detect(gear_type, regex("line", TRUE)) ~ "line",
      str_detect(gear_type, regex("cage", TRUE)) ~ "cage",
      str_detect(gear_type, regex("hook", TRUE)) ~ "hook_lure",
      TRUE ~ "other"
    ),
    
    status = case_when(
      n_alive > 0 & n_dead > 0 ~ "mixed",
      n_alive > 0 ~ "alive",
      n_dead > 0 ~ "dead",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(n_indiv > 0) %>%
  select(
    survey_id, date, site, location, province,
    gear_type, depth_min_m, depth_max_m,
    taxon_raw, family_raw, category_raw,
    n_alive, n_dead, n_indiv, status
  )

write_csv(entanglements_clean, "data_clean/entanglements_clean.csv")

### 02. category + function lookup from FishDataset ####
category_lookup <- read_excel(
  xlsx_path,
  sheet = "FishDataset"
) %>%
  rename_with(~ tolower(gsub("[ /()-]+", "_", .x))) %>%
  transmute(
    taxon_raw = str_trim(full_name),
    family = str_trim(family),
    category_code = str_trim(category),
    category = case_when(
      category_code == "A" ~ "Predator - Midwater",
      category_code == "B" ~ "Predator - Demersal",
      category_code == "C" ~ "Midwater Schooling",
      category_code == "D" ~ "Reef Specific",
      category_code == "E" ~ "Breams",
      category_code == "F" ~ "Puffer/Porcupine",
      category_code == "G" ~ "Eels",
      category_code == "H" ~ "Misc.",
      category_code == "I" ~ "Unidentified",
      TRUE ~ NA_character_
    ),
    functional_group = na_if(str_trim(functional_group), ""),
    ecosystem_role = na_if(str_trim(ecosystem_role), ""),
    ecosystem_benefit = na_if(str_trim(ecosystem_benefit), "")
  ) %>%
  distinct() %>%
  arrange(category_code, family, taxon_raw)

write_csv(category_lookup, "data_clean/category_lookup.csv")

### 03. functional_lookup ####
functional_lookup <- entanglements_clean %>%
  distinct(taxon_raw, family_raw, category_raw) %>%
  left_join(
    category_lookup,
    by = c("taxon_raw")
  ) %>%
  mutate(
    trophic_group = case_when(
      str_detect(functional_group, regex("herbivore", TRUE)) ~ "herbivore",
      str_detect(functional_group, regex("planktivore", TRUE)) ~ "planktivore",
      str_detect(functional_group, regex("piscivore|mesopredator|predator", TRUE)) ~ "piscivore",
      str_detect(functional_group, regex("invertivore", TRUE)) ~ "invertivore",
      category_code %in% c("A", "B") ~ "piscivore",
      category_code == "C" ~ "planktivore",
      TRUE ~ NA_character_
    ),
    herbivore_class = case_when(
      trophic_group == "herbivore" & str_detect(taxon_raw, regex("rabbitfish|sigan", TRUE)) ~ "high",
      trophic_group == "herbivore" ~ "medium",
      TRUE ~ NA_character_
    ),
    predator_class = case_when(
      trophic_group == "piscivore" & category_code == "A" ~ "midwater_predator",
      trophic_group == "piscivore" & category_code == "B" ~ "demersal_predator",
      trophic_group == "piscivore" ~ "predator",
      TRUE ~ NA_character_
    )
  )

write_csv(functional_lookup, "data_raw/functional_lookup.csv")
# small edits made manually ("??" -> NA) and then saved to data_clean

### 04. taxon lookup ####
taxon_lookup <- entanglements_clean %>%
  distinct(taxon_raw, family) %>%
  arrange(family, taxon_raw) %>%
  mutate(
    sci_name = NA_character_,
    common_name = NA_character_,
    genus = NA_character_,
    taxon_level = NA_character_,
    taxon_confidence = NA_character_,
    notes = NA_character_
  ) %>%
  select(
    taxon_raw, sci_name, common_name, family, genus,
    taxon_level, taxon_confidence, notes
  )

write_csv(taxon_lookup, "data_raw/taxon_lookup.csv")

### 05. aesthetic values ####
aesthetic_lookup <- read_csv("data_raw/aesthetic_source_table.csv", show_col_types = FALSE) %>%
  rename(
    tribot_2018_taxon = `FISH SPECIES - TRIBOT 2018`,
    langlois_2022_taxon = `FISH SPECIES - LANGLOIS 2022`,
    equivalent_taxon = EQUIVALENT,
    quantity = QUANTITY,
    tribot_2018_score = `TRIBOT AESTHETIC VALUE`,
    langlois_2022_score = `LANGLIOS AESTHETIC VALUE`,
    aesthetic_score_mean = `AVERAGE AESTHETIC VALUE`,
    notes = NOTES
  ) %>%
  mutate(
    score_source = "Tribot2018;Langlois2022",
    assumption_level = case_when(
      equivalent_taxon == "Unknown Fish" ~ "derived",
      TRUE ~ "proxy"
    )
  ) %>%
  select(
    equivalent_taxon,
    tribot_2018_taxon,
    langlois_2022_taxon,
    quantity,
    tribot_2018_score,
    langlois_2022_score,
    aesthetic_score_mean,
    score_source,
    assumption_level,
    notes
  )

# write_csv(aesthetic_lookup, "data_raw/aesthetic_lookup.csv")

## MANUALLY annotated lookup table with equivalent matches logic ### 
aesthetic_lookup <- read_csv("data_raw/aesthetic_lookup.csv", show_col_types = FALSE) %>%
  
  mutate(
    # --- recompute score ---
    aesthetic_score_mean = case_when(
      Match %in% c("B", "P") ~ rowMeans(cbind(tribot_2018_score, langlois_2022_score), na.rm = TRUE),
      Match == "L" ~ langlois_2022_score,
      Match == "T" ~ tribot_2018_score,
      TRUE ~ NA_real_
    ),
    
    # --- correct source ---
    score_source = case_when(
      Match == "B" ~ "Tribot et al. 2018; Langlois et al. 2022 (exact)",
      Match == "P" ~ "Tribot et al. 2018; Langloiset al. 2022 (proxy)",
      Match == "L" ~ "Langlois et al. 2022",
      Match == "T" ~ "Tribot et al. 2018",
      TRUE ~ NA_character_
    ),
    
    # --- correct assumption level ---
    assumption_level = case_when(
      Match == "B" ~ "direct",
      Match %in% c("L", "T", "P") ~ "proxy",
      TRUE ~ "derived"
    ),
    
    # --- optional: improve notes ---
    notes = case_when(
      Match == "P" & is.na(notes) ~ "Proxy match; averaged Tribot and Langlois",
      Match == "L" & is.na(notes) ~ "Langlois value used",
      Match == "T" & is.na(notes) ~ "Tribot value used",
      TRUE ~ notes
    )
  )

write_csv(aesthetic_lookup, "data_raw/aesthetic_lookup.csv") 

### 06. ESVD values  ####
esvd_raw <- tribble(
  ~service,      ~value_usd, ~country,       ~reference,
  "recreation",  1016,       "Indonesia",    "Rumahorbo et al. 2020",
  "recreation",  59624,      "Indonesia",    "Windayati et al. 2022",
  "recreation",  224.5,      "Philippines",  "Samonte-Tan and Armedilla 2004",
  "recreation",  27.6,       "Philippines",  "Samonte-Tan and Armedilla 2004",
  "recreation",  0.1,        "Philippines",  "Samonte-Tan and Armedilla 2004",
  "recreation",  170.1,      "Philippines",  "Samonte-Tan and Armedilla 2004",
  "recreation",  120.5,      "Korea",        "Park et al.",
  "recreation",  433.2,      "Vietnam",      "Lan et al.",
  "recreation",  867.8,      "Vietnam",      "Lan et al.",
  "recreation",  2806,       "Vietnam",      "Lan et al.",
  "recreation",  11.3,       "Indonesia",    "Yasir Haya and Fujii",
  "recreation",  5.2,        "Indonesia",    "Yasir Haya and Fujii",
  "recreation",  16771,      "Japan",        "Nohara et al."
)


esvd_lookup <- esvd_raw %>%
  group_by(service) %>%
  summarise(
    esvd_low  = min(value_usd, na.rm = TRUE),
    esvd_mid  = median(value_usd, na.rm = TRUE),
    esvd_high = max(value_usd, na.rm = TRUE),
    n_values  = n(),
    .groups = "drop"
  )

esvd_lookup
write_csv(esvd_raw, "data_clean/esdv_raw.csv")
write_csv(esvd_lookup, "data_clean/esdv_lookup.csv")

### 07. all ALDFG ####

all_aldfg_clean <- full %>%
  rename_with(~tolower(gsub(" ", "_", .x)))

write_csv(all_aldfg_clean, "data_clean/all_aldfg_clean.csv")

### 08. fun coefs (SCAFFOLD ONLY) ####

function_coefficients <- entanglements_clean %>%
  distinct(sci_name, family) %>%
  mutate(
    coefficient_type = NA_character_,
    coefficient_value = NA_real_,
    coefficient_units = NA_character_,
    study_region = NA_character_,
    citation_key = NA_character_,
    assumption_level = NA_character_,
    notes = NA_character_
  )

write_csv(function_coefficients, "data_raw/function_coefficients.csv")

# read once
coefs_raw <- read_csv("data_raw/fun_coefs_raw.csv")

# build species + family summaries in one go
coef_lookup <- coefs_raw %>%
  filter(family == "Siganidae", coef_units == "g_per_hour") %>%
  group_by(sci_name) %>%
  summarise(
    coef_low  = min(coef_value, na.rm = TRUE),
    coef_mid  = median(coef_value, na.rm = TRUE),
    coef_high = max(coef_value, na.rm = TRUE),
    .groups = "drop"
  )

# family fallback (single row)
family_fallback <- coefs_raw %>%
  filter(family == "Siganidae", coef_units == "g_per_hour") %>%
  summarise(
    coef_low  = min(coef_value, na.rm = TRUE),
    coef_mid  = median(coef_value, na.rm = TRUE),
    coef_high = max(coef_value, na.rm = TRUE)
  )

write_csv(coef_lookup, "data_clean/coef_lookup.csv")

### 09. mass lookup (SCAFFOLD) ####

mass_lookup <- entanglements_clean %>%
  distinct(sci_name, family, taxon_raw) %>%
  mutate(
    mean_mass_kg = NA_real_,
    mass_source = NA_character_,
    region = NA_character_,
    notes = NA_character_
  )

write_csv(mass_lookup, "data_raw/mass_lookup.csv")

### 10. reference lookup (EMPTY) ####

write_csv(
  tibble(
    citation_key = character(),
    full_citation = character(),
    year = integer(),
    region = character(),
    metric_type = character(),
    comments = character()
  ),
  "data_raw/reference_lookup.csv"
)


### 11. market values ####
market_values_summary <- read_csv("data_raw/market_values.csv", show_col_types = FALSE) %>%
  
  pivot_longer(
    cols = starts_with("thb_"),
    values_to = "price_thb",
    values_drop_na = TRUE
  ) %>%
  
  filter(price_thb > 0) %>%
  
  group_by(taxon_raw) %>%
  summarise(
    price_min_thb = min(price_thb, na.rm = TRUE),
    price_max_thb = max(price_thb, na.rm = TRUE),
    price_mean_thb = mean(price_thb, na.rm = TRUE),
    price_sd_thb = sd(price_thb, na.rm = TRUE),
    price_n = n(),
    .groups = "drop"
  )
write_csv(market_values_summary, "data_clean/market_values_summary.csv")

market_values_long <- read_csv("data_raw/market_values.csv", show_col_types = FALSE) %>%
  
  pivot_longer(
    cols = starts_with("thb_"),
    values_to = "price_thb",
    values_drop_na = TRUE
  ) %>%
  
  filter(price_thb > 0)
write_csv(market_values_long, "data_clean/market_values_long.csv")