## 01.5_JOIN ##

### RESET ENVIRONMENT ####
rm(list = ls())

library(tidyverse)

### LOAD CLEAN DATA ####
data_clean_dir <- "data_clean"

entanglements_clean <- read_csv(file.path(data_clean_dir, "entanglements_clean.csv"), show_col_types = FALSE)
category_lookup     <- read_csv(file.path(data_clean_dir, "category_lookup.csv"), show_col_types = FALSE)
functional_lookup   <- read_csv(file.path(data_clean_dir, "functional_lookup.csv"), show_col_types = FALSE)
taxon_lookup        <- read_csv(file.path(data_clean_dir, "taxon_lookup.csv"), show_col_types = FALSE)
all_aldfg_clean     <- read_csv(file.path(data_clean_dir, "all_aldfg_clean.csv"), show_col_types = FALSE)
aesthetic_lookup    <- read_csv("data_clean/aesthetic_lookup.csv", show_col_types = FALSE)
market_values_summ  <- read_csv("data_clean/market_values_summary.csv", show_col_types = FALSE) 

# sanity checks 
glimpse(entanglements_clean)
glimpse(taxon_lookup)

nrow(entanglements_clean)
n_distinct(entanglements_clean$taxon_raw)

nrow(taxon_lookup)
n_distinct(taxon_lookup$taxon_raw)

taxon_lookup %>%
  count(taxon_raw) %>%
  filter(n > 1)


### joins #### 
entanglements_joined <- entanglements_clean %>%
  left_join(taxon_lookup, by = "taxon_raw")

# check for failures 
entanglements_joined %>%
  filter(is.na(sci_name)) %>%
  distinct(taxon_raw)

# sanity checks before big joins 
nrow(functional_lookup)
n_distinct(functional_lookup$taxon_raw)

functional_lookup %>%
  count(taxon_raw) %>%
  filter(n > 1)

nrow(category_lookup)
n_distinct(category_lookup$taxon_raw)

category_lookup %>%
  count(taxon_raw) %>%
  filter(n > 1)

# decouple duplicates in cat lookup 
category_lookup <- category_lookup %>%
  distinct()
# resolve grouper mismatch 
category_lookup <- category_lookup %>%
  distinct() %>%
  filter(!(taxon_raw == "Grouper" & functional_group == "HTLP"))

# decouple duplicates in fungroup lookup 
functional_lookup <- functional_lookup %>%
  distinct()
# resolve grouper mismatch 
functional_lookup <- functional_lookup %>%
  distinct() %>%
  filter(!(taxon_raw == "Grouper" & functional_group == "HTLP"))

# quick rename 
taxon_lookup <- taxon_lookup %>%
  rename(family_std = family) # only needs to be run once 


# join functional + category lookup tables to entanglement dataset ## 
entanglements_joined <- entanglements_joined %>%
  left_join(functional_lookup, by = "taxon_raw") %>%
  left_join(category_lookup, by = "taxon_raw")

# join aesthetic 
entanglements_joined <- entanglements_joined %>%
  left_join(
    aesthetic_lookup,
    by = c("taxon_raw" = "taxon_raw")
  )

entanglements_joined %>%
  summarise(
    total = n(),
    with_aesthetic = sum(!is.na(aesthetic_score_mean))
  )

# coalesce columns 
entanglements_joined <- entanglements_joined %>%
  mutate(
    functional_group = coalesce(functional_group.x, functional_group.y),
    category = coalesce(category.x, category.y),
    ecosystem_role = coalesce(ecosystem_role.x, ecosystem_role.y),
    ecosystem_benefit = coalesce(ecosystem_benefit.x, ecosystem_benefit.y)
  ) %>%
  select(
    -ends_with(".x"),
    -ends_with(".y")
  )

# join market values 
entanglements_joined <- entanglements_joined %>%
  left_join(market_values_summ, by = "taxon_raw")

# coverage
entanglements_joined %>%
  summarise(
    total = n(),
    with_price = sum(!is.na(price_mean_thb)),
    missing_price = sum(is.na(price_mean_thb))
  )

# what didn’t match
entanglements_joined %>%
  filter(is.na(price_mean_thb)) %>%
  distinct(taxon_raw, common_name, sci_name)

# final checks 
entanglements_joined %>%
  count(survey_id, taxon_raw, date) %>%
  filter(n > 1)
entanglements_joined %>%
  summarise(
    missing_sci = sum(is.na(sci_name)),
    missing_func = sum(is.na(functional_group)),
    missing_category = sum(is.na(category)),
    missing_aesthetic = sum(is.na(aesthetic_score_mean))
  )


# couple summaries 
summary(entanglements_joined$n_indiv)
table(entanglements_joined$gear_type)
entanglements_joined %>% count(taxon_raw, sort = TRUE)
