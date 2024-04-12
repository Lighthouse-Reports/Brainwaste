#load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(openxlsx)
library(stringi)
library(stringr)

#input fp
input_fp <- 'Input Data/ELF_Merged/final/merged_country_final_2006_onwards_'
#output fp
output_fp <- paste0(cur_date, 'Results/scratchpad/')
dir.create(output_fp, showWarnings = F)
output_fp_sumtables <- paste0(output_fp, 'sumtables/')
dir.create(output_fp_sumtables, showWarnings = F)



#load external data
# 2022 EURES shortage occupation, based on: https://www.ela.europa.eu/sites/default/files/2023-03/eures-labour-shortages-report-2022.pdf
shortage22 <- read.xlsx('Input Data/Config/EURES_shortage_2022.xlsx') %>%
  mutate(ISCO08_short = str_extract(ISCO08, '^\\d{3}'), #extract first 3D of ISCO code to allow merging with country_df
         Country = case_when(Country %in% c('LV', 'LT', 'EE') ~ 'Baltics',
                             Country %in% c('PL', 'CZ', 'SK', 'HU') ~ 'Visegrad',
                             Country %in% c('RO', 'BG') ~ 'eu07',
                             Country %in% c('HR', 'SI') ~ 'yugo',
                             .default = Country)) %>%
  dplyr::select(Country, ISCO08_short)

pop <- read.csv('Input Data/eurostat/pop_transformed.csv') %>%
  filter(year == 2019) %>%
  dplyr::select(country_short, total) %>%
  rename(pop_total = total)


### Summary table: immigr native differences across outcomes ###
h_table_immigrant <- data.frame()

h_table_college <- data.frame()

educ_table <- data.frame()


for(country in countries_to_analyze){
  print(country)
  shortage_country <- shortage22 %>%
    filter(Country == country)
  
  country_df_college <- arrow::read_feather(paste0(input_fp, country, '.feather')) %>%
    mutate(high_skilled = case_when(skill_level == 'high' ~ 1,
                                    skill_level == 'low' ~ 0,
                                    .default = 0),
          shortage_eures = case_when(ISCO08_3D %in% shortage_country$ISCO08_short ~ 1,
                                    !is.na(ISCO08_3D) ~ 0,
                                    .default = NA)) %>%
    filter(REFYEAR > 2016)
  
  country_df_17 <- arrow::read_feather(paste0(input_fp, country, '_ALL.feather')) %>%
    mutate(high_skilled = case_when(skill_level == 'high' ~ 1,
                                    skill_level == 'low' ~ 0,
                                    .default = 0),
           shortage_eures = case_when(ISCO08_3D %in% shortage_country$ISCO08_short ~ 1,
                                      !is.na(ISCO08_3D) ~ 0,
                                      .default = NA)) %>%
    filter(REFYEAR > 2016)
  
  
  lab_comp_17 <- country_df_17 %>%
    group_by(COUNTRY, is_immigrant) %>%
    summarise(underemployment_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              unemployed_mean = mean(is_unemployed, na.rm = T),
              skilled_mean = mean(high_skilled, na.rm = T),
              income_mean = mean(INCDECIL, na.rm = T),
              shortage_mean = mean(shortage_eures, na.rm = T),
              regulated_mean = mean(regulated_profession, na.rm = T)) %>%
    mutate(is_immigrant = ifelse(is_immigrant == 0, 'native', 'immigrant')) %>%
    pivot_wider(values_from = ends_with('mean'), names_from = 'is_immigrant') %>%
    mutate(underemployment_diff = underemployment_mean_immigrant - underemployment_mean_native,
           overed_diff = overed_mean_immigrant - overed_mean_native,
           unemployed_diff = unemployed_mean_immigrant - unemployed_mean_native,
           skilled_diff = skilled_mean_immigrant - skilled_mean_native,
           income_diff = income_mean_immigrant - income_mean_native,
           shortage_diff = shortage_mean_immigrant - shortage_mean_native,
           regulated_diff = regulated_mean_immigrant - regulated_mean_native)
  
  fr <- ' ~ '
  
  if(n_distinct(country_df_17[,'is_immigrant'])>1) fr <- paste0(fr, 'is_immigrant + ')
  if(n_distinct(country_df_17[,'SEX'])>1) fr <-paste0(fr, 'SEX + ')
  if(n_distinct(country_df_17[,'age_years'])>1) fr <-paste0(fr, 'age_years + age_years^2 + ')
  if(n_distinct(country_df_17[,'HHNBCHILD'])>1) fr <-paste0(fr, 'HHNBCHILD + ')
  if(n_distinct(country_df_17[,'is_partnered'])>1) fr <-paste0(fr, 'is_partnered + ')
  if(n_distinct(country_df_17[,'hat_isced'])>1) fr <-paste0(fr, 'hat_isced + ')
  if(n_distinct(country_df_17[,'REGION_2D'])>1) fr <-paste0(fr, 'as.factor(REGION_2D) + ')
  if(n_distinct(country_df_17[,'REFYEAR'])>1) fr <-paste0(fr, 'as.factor(REFYEAR)')
 
  if(n_distinct(country_df_17[,'uemp'])>1){
    fit_underemp_17 <- lm(data = country_df_17, formula = paste0('uemp', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(underemp_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(term == 'is_immigrant') %>%
      dplyr::select(estimate, underemp_significance) %>%
      rename(underemp_regression = estimate)
    lab_comp_17 <- bind_cols(lab_comp_17, fit_underemp_17) 
  }
  
  if(n_distinct(country_df_17[,'overed_1sd_hat_isced'])>1){
    fit_overed_17 <- lm(data = country_df_17, formula = paste0('overed_1sd_hat_isced', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(overed_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(term == 'is_immigrant') %>%
      dplyr::select(estimate, overed_significance) %>%
      rename(overed_regression = estimate)
    lab_comp_17 <- bind_cols(lab_comp_17, fit_overed_17) 
  }
  
  if(n_distinct(country_df_17[,'is_unemployed'])>1){
    fit_unemployed_17 <- lm(data = country_df_17, formula = paste0('is_unemployed', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(unemployed_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(term == 'is_immigrant') %>%
      dplyr::select(estimate, unemployed_significance) %>%
      rename(unemployed_regression = estimate)
    lab_comp_17 <- bind_cols(lab_comp_17, fit_unemployed_17) 
  }
  
  if(n_distinct(country_df_17[,'high_skilled'])>1){
    fit_skilled_17 <- lm(data = country_df_17, formula = paste0('high_skilled', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(skilled_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(term == 'is_immigrant') %>%
      dplyr::select(estimate, skilled_significance) %>%
      rename(skilled_regression = estimate)
    lab_comp_17 <- bind_cols(lab_comp_17, fit_skilled_17) 
  }
  
  if(n_distinct(country_df_17[,'INCDECIL'])>1){
    fit_income_17 <- lm(data = country_df_17, formula = paste0('INCDECIL', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(income_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(term == 'is_immigrant') %>%
      dplyr::select(estimate, income_significance) %>%
      rename(income_regression = estimate)
    lab_comp_17 <- bind_cols(lab_comp_17, fit_income_17) 
  }
  
  if(n_distinct(country_df_17[,'shortage_eures'])>1){
    fit_shortage_17 <- lm(data = country_df_17, formula = paste0('shortage_eures', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(shortage_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(term == 'is_immigrant') %>%
      dplyr::select(estimate, shortage_significance) %>%
      rename(shortage_regression = estimate)
    lab_comp_17 <- bind_cols(lab_comp_17, fit_shortage_17) 
  }
  
  if(n_distinct(country_df_17[,'regulated_profession'])>1){
    fit_regulated_17 <- lm(data = country_df_17, formula = paste0('regulated_profession', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(regulated_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(term == 'is_immigrant') %>%
      dplyr::select(estimate, regulated_significance) %>%
      rename(regulated_regression = estimate)
    lab_comp_17 <- bind_cols(lab_comp_17, fit_regulated_17)
  }
   
  
  lab_comp_17 <- lab_comp_17 %>%
    dplyr::select(COUNTRY, starts_with('under'), starts_with('over'), starts_with('unemp'), starts_with('skilled'), 
           starts_with('income'), starts_with('shortage'), starts_with('regulated'))
  
  h_table_immigrant <- h_table_immigrant %>%
    bind_rows(lab_comp_17)
  
  
  lab_comp_college <- country_df_college %>%
    group_by(COUNTRY, is_immigrant) %>%
    summarise(underemployment_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              unemployed_mean = mean(is_unemployed, na.rm = T),
              skilled_mean = mean(high_skilled, na.rm = T),
              income_mean = mean(INCDECIL, na.rm = T),
              shortage_mean = mean(shortage_eures, na.rm = T),
              regulated_mean = mean(regulated_profession, na.rm = T)) %>%
    mutate(is_immigrant = ifelse(is_immigrant == 0, 'native', 'immigrant')) %>%
    pivot_wider(values_from = ends_with('mean'), names_from = 'is_immigrant') %>%
    mutate(underemployment_diff = underemployment_mean_immigrant - underemployment_mean_native,
           overed_diff = overed_mean_immigrant - overed_mean_native,
           unemployed_diff = unemployed_mean_immigrant - unemployed_mean_native,
           skilled_diff = skilled_mean_immigrant - skilled_mean_native,
           income_diff = income_mean_immigrant - income_mean_native,
           shortage_diff = shortage_mean_immigrant - shortage_mean_native,
           regulated_diff = regulated_mean_immigrant - regulated_mean_native)
  
  share_college_educated <- data.frame(COUNTRY = country,
                                       share_college = nrow(country_df_college)/nrow(country_df_17))
  lab_comp_college <- left_join(lab_comp_college, share_college_educated, by = 'COUNTRY')
  
  if(n_distinct(country_df_college[,'uemp'])>1){
    fit_underemp_college <- lm(data = country_df_college, formula = paste0('uemp', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(underemp_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(term == 'is_immigrant') %>%
      dplyr::select(estimate, underemp_significance) %>%
      rename(underemp_regression = estimate)
    lab_comp_college <- bind_cols(lab_comp_college, fit_underemp_college) 
  }
  
  if(n_distinct(country_df_college[,'overed_1sd_hat_isced'])>1){
    fit_overed_college <- lm(data = country_df_college, formula = paste0('overed_1sd_hat_isced', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(overed_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(term == 'is_immigrant') %>%
      dplyr::select(estimate, overed_significance) %>%
      rename(overed_regression = estimate)
    lab_comp_college <- bind_cols(lab_comp_college, fit_overed_college) 
  }
  
  
  if(n_distinct(country_df_college[,'is_unemployed'])>1){
    fit_unemployed_college <- lm(data = country_df_college, formula = paste0('is_unemployed', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(unemployed_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(term == 'is_immigrant') %>%
      dplyr::select(estimate, unemployed_significance) %>%
      rename(unemployed_regression = estimate)
    lab_comp_college <- bind_cols(lab_comp_college, fit_unemployed_college)
  }
   
  
  if(n_distinct(country_df_college[,'high_skilled'])>1){
    fit_skilled_college <- lm(data = country_df_college, formula = paste0('high_skilled', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(skilled_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(term == 'is_immigrant') %>%
      dplyr::select(estimate, skilled_significance) %>%
      rename(skilled_regression = estimate)
    lab_comp_college <- bind_cols(lab_comp_college, fit_skilled_college) 
  }
  
  
  if(n_distinct(country_df_college[,'INCDECIL'])>1){
    fit_income_college <- lm(data = country_df_college, formula = paste0('INCDECIL', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(income_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(term == 'is_immigrant') %>%
      dplyr::select(estimate, income_significance) %>%
      rename(income_regression = estimate)
    lab_comp_college <- bind_cols(lab_comp_college, fit_income_college) 
  }
  
  
  if(n_distinct(country_df_college[,'shortage_eures'])>1){
    fit_shortage_college <- lm(data = country_df_college, formula = paste0('shortage_eures', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(shortage_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(term == 'is_immigrant') %>%
      dplyr::select(estimate, shortage_significance) %>%
      rename(shortage_regression = estimate)
    lab_comp_college <- bind_cols(lab_comp_college, fit_shortage_college) 
  }
  
  
  if(n_distinct(country_df_college[,'regulated_profession'])>1){
    fit_regulated_college <- lm(data = country_df_college, formula = paste0('regulated_profession', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(regulated_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(term == 'is_immigrant') %>%
      dplyr::select(estimate, regulated_significance) %>%
      rename(regulated_regression = estimate)
    lab_comp_college <- bind_cols(lab_comp_college, fit_regulated_college)
  }
   
  
  lab_comp_college <- lab_comp_college %>%
    dplyr::select(COUNTRY, starts_with('under'), starts_with('over'), starts_with('unemp'), starts_with('skilled'), 
                  starts_with('income'), starts_with('shortage'), starts_with('regulated'), share_college)
  
  h_table_college <- h_table_college %>%
    bind_rows(lab_comp_college)
  
 educ_brainwaste <- country_df_17 %>%
   group_by(COUNTRY, hatfield1d, is_immigrant) %>%
   summarise(underemployment_mean = mean(uemp, na.rm = T),
             overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
             unemployed_mean = mean(is_unemployed, na.rm = T),
             count = n()) %>%
   filter(!is.na(hatfield1d)) %>%
   mutate(is_immigrant = ifelse(is_immigrant == 0, 'native', 'immigrant')) %>%
   pivot_wider(values_from = c(ends_with('mean'), 'count'), names_from = 'is_immigrant') %>%
   mutate(underemployment_diff = underemployment_mean_immigrant - underemployment_mean_native,
          overed_diff = overed_mean_immigrant - overed_mean_native,
          unemployed_diff = unemployed_mean_immigrant - unemployed_mean_native) %>%
   dplyr::select(c('COUNTRY', 'hatfield1d', ends_with('diff'), 'count_immigrant'))
 educ_brainwaste$sum_immigrant <- sum(educ_brainwaste$count_immigrant, na.rm = T)
 educ_brainwaste$share_immigrant <- educ_brainwaste$count_immigrant/educ_brainwaste$sum_immigrant
 
 educ_table <- educ_table %>%
   bind_rows(educ_brainwaste)
}

vars_to_aggregate <- c("underemployment_mean_native", "underemployment_mean_immigrant", "underemployment_diff", "underemp_regression",
                       "overed_mean_native", "overed_mean_immigrant", "overed_diff", "overed_regression",
                       "unemployed_mean_native", "unemployed_mean_immigrant", "unemployed_diff", "unemployed_regression", 
                       "skilled_mean_native", "skilled_mean_immigrant", "skilled_diff", "skilled_regression",
                       "income_mean_native", "income_mean_immigrant", "income_diff", "income_regression",
                       "shortage_mean_native", "shortage_mean_immigrant", "shortage_diff", "shortage_regression",
                       "regulated_mean_native", "regulated_mean_immigrant", "regulated_diff", "regulated_regression")

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




h_table_college2 <- h_table_college %>%
  left_join(pop, by = c('COUNTRY' = 'country_short')) %>%
  mutate(college_pop = share_college * pop_total)

weighted_means <- numeric(length(vars_to_aggregate))
for (i in seq_along(vars_to_aggregate)) {
  weighted_means[i] <- weighted.mean(h_table_college2[[vars_to_aggregate[i]]], h_table_college2$college_pop, na.rm = T)
}
names(weighted_means) <- vars_to_aggregate
weighted_means <- as.data.frame(weighted_means) %>%
  t() %>%
  as.data.frame() %>%
  mutate(COUNTRY = 'EU')

h_table_college <- h_table_college %>%
  bind_rows(weighted_means) %>%
  dplyr::select(-ends_with('NA'))

write.csv(h_table_immigrant, paste0(output_fp_sumtables, 'hypothesis_table_immigrants.csv'))
write.csv(h_table_college, paste0(output_fp_sumtables, 'hypothesis_table_college.csv'))


# Eva's table
#load data
earnings_penalty <- read.csv(paste0(cur_date, 'Results/h3_2/earnings_potential_immigrants_2020.csv')) %>%
  dplyr::select(country, uemp_penalty, overed_penalty)
employed_immigrants <- read.csv('Input Data/eurostat/employed_immigrants_transformed.csv') %>%
  dplyr::select(country_short, count)

#merge with true population counts
educ_table <- educ_table %>%
  left_join(employed_immigrants, by = c('COUNTRY' = 'country_short'))
#calculate immigrants affected by brainwaste
educ_table <- educ_table %>%
  mutate(count_affected_underemployment = round(underemployment_diff * share_immigrant * count, digits = 0),
         count_affected_overeducation = round(overed_diff * share_immigrant*count, digits = 0),
         count_affected_unemployment = round(unemployed_diff * share_immigrant * count, digits = 0))
#multiply immigrants affected by brainwaste with brainwaste wage penalty
educ_table <- educ_table %>%
  left_join(earnings_penalty, by = c('COUNTRY' = 'country')) %>%
  mutate(earnings_lost_underemployment = count_affected_underemployment*uemp_penalty,
         earnings_lost_overeducation = count_affected_overeducation*overed_penalty)
#keep only relevant cols
educ_table <- educ_table %>%
  mutate(hatfield_text = case_when(hatfield1d == 0 ~ 'basic',
                            hatfield1d == 1 ~ 'education',
                            hatfield1d == 2 ~ 'arts/humanities',
                            hatfield1d == 3 ~ 'social sciences',
                            hatfield1d == 4 ~ 'business/law',
                            hatfield1d == 5 ~ 'natural sciences',
                            hatfield1d == 6 ~ 'ict',
                            hatfield1d == 7 ~ 'engineering',
                            hatfield1d == 8 ~ 'agriculture',
                            hatfield1d == 9 ~ 'health',
                            hatfield1d == 10 ~ 'services',
                            .default = 'Unknown Field')) %>%
  dplyr::rename(count_immigrant_field = count_immigrant,
                count_immigrant_elf_total = sum_immigrant,
                share_immigrant_elf = share_immigrant,
                count_immigrant_working_total = count,
                )

write.csv(educ_table, paste0(output_fp_sumtables, 'earnings_potential_field_education.csv'))
