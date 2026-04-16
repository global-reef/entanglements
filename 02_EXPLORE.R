## 02_EXPLORE ## 

# response vs gear - look for overdispersion 
entanglements_joined %>%
  group_by(gear_type) %>%
  summarise(
    mean_n = mean(n_indiv),
    var_n = var(n_indiv),
    n = n()
  )
# no overdispersion but VERY skewed

# distribution shape 
ggplot(entanglements_joined, aes(x = n_indiv)) +
  geom_histogram(binwidth = 1)
# indeed, very skewed 

# taxon vs counts 
entanglements_joined %>%
  group_by(taxon_raw) %>%
  summarise(
    mean_n = mean(n_indiv),
    total_n = sum(n_indiv),
    n_obs = n()
  ) %>%
  arrange(desc(total_n))

# price coverage vs taxa 
entanglements_joined %>%
  group_by(taxon_raw) %>%
  summarise(
    has_price = any(!is.na(price_mean_thb)),
    n = n()
  )
# missing prices 
entanglements_joined %>%
  filter(is.na(price_mean_thb)) %>%
  distinct(common_name) %>%
  arrange(common_name)
