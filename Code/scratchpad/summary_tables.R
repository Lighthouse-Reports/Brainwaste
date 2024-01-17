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



### Summary table: immigr native differences across outcomes ###
h_table_immigrant <- data.frame(
)

h_table_college <- data.frame(
)

educ_table <- data.frame()


for(country in countries_to_analyze){
  country_df <- arrow::read_feather(paste0(input_fp, country, '.feather')) %>%
    mutate(high_skilled = case_when(skill_level == 'high' ~ 1,
                                    skill_level == 'low' ~ 0,
                                    .default = 0))
  country_df_17 <- country_df %>%
    filter(REFYEAR >= 2017)
  country_df_college <- country_df_17 %>%
    filter(is_college_educated == 1)
  country_df_21 <- country_df %>%
    filter(REFYEAR == 2021)
  
  lab_comp_17 <- country_df_17 %>%
    group_by(COUNTRY, is_immigrant) %>%
    summarise(underemployment_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              unemployed_mean = mean(is_unemployed, na.rm = T),
              skilled_mean = mean(high_skilled, na.rm = T),
              income_mean = mean(INCDECIL, na.rm = T),
              shortage_mean = mean(shortage, na.rm = T),
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
  
  fr <- ' ~ is_immigrant + SEX + age_years + age_years^2 + is_partnered + hat_isced + HHNBCHILD + as.factor(REGION_2D) + as.factor(REFYEAR)'
  
  fit_underemp_17 <- lm(data = country_df_17, formula = paste0('uemp', fr)) %>%
    tidy(conf.int = T, conf.level = 0.95) %>%
    as.data.frame() %>%
    mutate(underemp_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
    filter(term == 'is_immigrant') %>%
    dplyr::select(estimate, underemp_significance) %>%
    rename(underemp_regression = estimate)
  lab_comp_17 <- bind_cols(lab_comp_17, fit_underemp_17) 
  
  fit_overed_17 <- lm(data = country_df_17, formula = paste0('overed_1sd_hat_isced', fr)) %>%
    tidy(conf.int = T, conf.level = 0.95) %>%
    as.data.frame() %>%
    mutate(overed_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
    filter(term == 'is_immigrant') %>%
    dplyr::select(estimate, overed_significance) %>%
    rename(overed_regression = estimate)
  lab_comp_17 <- bind_cols(lab_comp_17, fit_overed_17) 
  
  fit_unemployed_17 <- lm(data = country_df_17, formula = paste0('is_unemployed', fr)) %>%
    tidy(conf.int = T, conf.level = 0.95) %>%
    as.data.frame() %>%
    mutate(unemployed_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
    filter(term == 'is_immigrant') %>%
    dplyr::select(estimate, unemployed_significance) %>%
    rename(unemployed_regression = estimate)
  lab_comp_17 <- bind_cols(lab_comp_17, fit_unemployed_17) 
  
  fit_skilled_17 <- lm(data = country_df_17, formula = paste0('high_skilled', fr)) %>%
    tidy(conf.int = T, conf.level = 0.95) %>%
    as.data.frame() %>%
    mutate(skilled_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
    filter(term == 'is_immigrant') %>%
    dplyr::select(estimate, skilled_significance) %>%
    rename(skilled_regression = estimate)
  lab_comp_17 <- bind_cols(lab_comp_17, fit_skilled_17) 
  
  fit_income_17 <- lm(data = country_df_17, formula = paste0('INCDECIL', fr)) %>%
    tidy(conf.int = T, conf.level = 0.95) %>%
    as.data.frame() %>%
    mutate(income_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
    filter(term == 'is_immigrant') %>%
    dplyr::select(estimate, income_significance) %>%
    rename(income_regression = estimate)
  lab_comp_17 <- bind_cols(lab_comp_17, fit_income_17) 
  
  fit_shortage_17 <- lm(data = country_df_17, formula = paste0('shortage', fr)) %>%
    tidy(conf.int = T, conf.level = 0.95) %>%
    as.data.frame() %>%
    mutate(shortage_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
    filter(term == 'is_immigrant') %>%
    dplyr::select(estimate, shortage_significance) %>%
    rename(shortage_regression = estimate)
  lab_comp_17 <- bind_cols(lab_comp_17, fit_shortage_17) 
  
  fit_regulated_17 <- lm(data = country_df_17, formula = paste0('regulated_profession', fr)) %>%
    tidy(conf.int = T, conf.level = 0.95) %>%
    as.data.frame() %>%
    mutate(regulated_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
    filter(term == 'is_immigrant') %>%
    dplyr::select(estimate, regulated_significance) %>%
    rename(regulated_regression = estimate)
  lab_comp_17 <- bind_cols(lab_comp_17, fit_regulated_17) 
  
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
              shortage_mean = mean(shortage, na.rm = T),
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
  
  fr <- ' ~ is_immigrant + SEX + age_years + age_years^2 + is_partnered + hat_isced + HHNBCHILD + as.factor(REGION_2D) + as.factor(REFYEAR)'
  
  fit_underemp_college <- lm(data = country_df_college, formula = paste0('uemp', fr)) %>%
    tidy(conf.int = T, conf.level = 0.95) %>%
    as.data.frame() %>%
    mutate(underemp_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
    filter(term == 'is_immigrant') %>%
    dplyr::select(estimate, underemp_significance) %>%
    rename(underemp_regression = estimate)
  lab_comp_college <- bind_cols(lab_comp_college, fit_underemp_college) 
  
  fit_overed_college <- lm(data = country_df_college, formula = paste0('overed_1sd_hat_isced', fr)) %>%
    tidy(conf.int = T, conf.level = 0.95) %>%
    as.data.frame() %>%
    mutate(overed_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
    filter(term == 'is_immigrant') %>%
    dplyr::select(estimate, overed_significance) %>%
    rename(overed_regression = estimate)
  lab_comp_college <- bind_cols(lab_comp_college, fit_overed_college) 
  
  fit_unemployed_college <- lm(data = country_df_college, formula = paste0('is_unemployed', fr)) %>%
    tidy(conf.int = T, conf.level = 0.95) %>%
    as.data.frame() %>%
    mutate(unemployed_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
    filter(term == 'is_immigrant') %>%
    dplyr::select(estimate, unemployed_significance) %>%
    rename(unemployed_regression = estimate)
  lab_comp_college <- bind_cols(lab_comp_college, fit_unemployed_college) 
  
  fit_skilled_college <- lm(data = country_df_college, formula = paste0('high_skilled', fr)) %>%
    tidy(conf.int = T, conf.level = 0.95) %>%
    as.data.frame() %>%
    mutate(skilled_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
    filter(term == 'is_immigrant') %>%
    dplyr::select(estimate, skilled_significance) %>%
    rename(skilled_regression = estimate)
  lab_comp_college <- bind_cols(lab_comp_college, fit_skilled_college) 
  
  fit_income_college <- lm(data = country_df_college, formula = paste0('INCDECIL', fr)) %>%
    tidy(conf.int = T, conf.level = 0.95) %>%
    as.data.frame() %>%
    mutate(income_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
    filter(term == 'is_immigrant') %>%
    dplyr::select(estimate, income_significance) %>%
    rename(income_regression = estimate)
  lab_comp_college <- bind_cols(lab_comp_college, fit_income_college) 
  
  fit_shortage_college <- lm(data = country_df_college, formula = paste0('shortage', fr)) %>%
    tidy(conf.int = T, conf.level = 0.95) %>%
    as.data.frame() %>%
    mutate(shortage_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
    filter(term == 'is_immigrant') %>%
    dplyr::select(estimate, shortage_significance) %>%
    rename(shortage_regression = estimate)
  lab_comp_college <- bind_cols(lab_comp_college, fit_shortage_college) 
  
  fit_regulated_college <- lm(data = country_df_college, formula = paste0('regulated_profession', fr)) %>%
    tidy(conf.int = T, conf.level = 0.95) %>%
    as.data.frame() %>%
    mutate(regulated_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
    filter(term == 'is_immigrant') %>%
    dplyr::select(estimate, regulated_significance) %>%
    rename(regulated_regression = estimate)
  lab_comp_college <- bind_cols(lab_comp_college, fit_regulated_college) 
  
  lab_comp_college <- lab_comp_college %>%
    dplyr::select(COUNTRY, starts_with('under'), starts_with('over'), starts_with('unemp'), starts_with('skilled'), 
                  starts_with('income'), starts_with('shortage'), starts_with('regulated'))
  
  h_table_college <- h_table_immigrant %>%
    bind_rows(lab_comp_college)
  
 educ_brainwaste <- country_df_21 %>%
   group_by(COUNTRY, hatfield1d, is_immigrant) %>%
   summarise(underemployment_mean = mean(uemp, na.rm = T),
             overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
             unemployed_mean = mean(is_unemployed, na.rm = T),
             count = n()) %>%
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

write.csv(h_table_immigrants, paste0(output_fp_sumtables, 'hypothesis_table_immigrants.csv'))
write.csv(h_table_college, paste0(output_fp_sumtables, 'hypothesis_table_college.csv'))


#TODO Eva's table
#load data
earnings_penalty <- read.csv(paste0(cur_date, 'Results/h3_2/earnings_potential_immigrants.csv'))
employed_immigrants <- read.csv('Input Data/eurostat/employed_immigrants_transformed.csv')

#merge with true population counts
#calculate true number of immigrants by field of education
#calculate immigrants affected by brainwaste
#multiply immigrants affected by brainwaste with brainwaste wage penalty
#keep only relevant rows
