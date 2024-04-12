#load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)

#input fp
input_fp <- 'Input Data/ELF_Merged/final/merged_country_final_2006_onwards_'
#output fp
output_fp <- paste0(cur_date, 'Results/scratchpad/')
dir.create(output_fp, showWarnings = F)
output_fp_sumtables <- paste0(output_fp, 'sumtables/')
dir.create(output_fp_sumtables, showWarnings = F)


pop <- read.csv('Input Data/eurostat/pop_transformed.csv') %>%
  filter(year == 2019) %>%
  dplyr::select(country_short, total) %>%
  rename(pop_total = total)

### Summary table: immigr native differences across outcomes ###
h_table_immigrant <- data.frame(
)


for(country in countries_to_analyze){
  print(country)
  if(country == 'UK') next
  
  country_df <- arrow::read_feather(paste0(input_fp, country, '.feather')) %>%
    mutate(any_brainwaste = case_when(uemp == 1 | is_unemployed == 1 | overed_1sd_hat_isced == 1 ~ 1, ### CHECK this is the key line ###
                                      is.na(uemp) | is.na(is_unemployed) | is.na(overed_1sd_hat_isced) ~ NA,
                                      .default = 0))
  country_df_17 <- country_df %>%
    filter(REFYEAR >= 2017)
  
  lab_comp_17 <- country_df_17 %>%
    group_by(COUNTRY, is_immigrant) %>%
    summarise(any_brainwaste = mean(any_brainwaste, na.rm = T)) %>%
    mutate(is_immigrant = ifelse(is_immigrant == 0, 'any_brainwaste_native', 'any_brainwaste_immigrant')) %>%
    filter(!is.na(is_immigrant)) %>%
    pivot_wider(values_from = any_brainwaste, names_from = 'is_immigrant') %>%
    mutate(any_brainwaste_diff = any_brainwaste_immigrant - any_brainwaste_native)
  
  h_table_immigrant <- h_table_immigrant %>%
    bind_rows(lab_comp_17)
}

vars_to_aggregate <- c("any_brainwaste_immigrant", "any_brainwaste_native", "any_brainwaste_diff")

h_table_immigrant2 <- h_table_immigrant %>%
  left_join(pop, by = c('COUNTRY' = 'country_short'))

weighted_means <- numeric(length(vars_to_aggregate))
for (i in seq_along(vars_to_aggregate)) {
  weighted_means[i] <- weighted.mean(h_table_immigrant2[[vars_to_aggregate[i]]], h_table_immigrant2$pop_total, na.rm = T)
}
names(weighted_means) <- vars_to_aggregate
weighted_means <- as.data.frame(weighted_means) %>%
  t() %>%
  as.data.frame() %>%
  mutate(COUNTRY = 'EU')

h_table_immigrant <- h_table_immigrant %>%
  bind_rows(weighted_means) %>%
  dplyr::select(-ends_with('NA'))


write.csv(h_table_immigrant, paste0(output_fp_sumtables, 'any_brainwaste.csv'))
