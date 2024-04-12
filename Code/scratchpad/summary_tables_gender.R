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


for(country in countries_to_analyze){
  
  country_df <- arrow::read_feather(paste0(input_fp, country, '.feather')) %>%
    mutate(high_skilled = case_when(skill_level == 'high' ~ 1,
                                    skill_level == 'low' ~ 0,
                                    .default = 0),
           shortage_eures = case_when(ISCO08_3D %in% shortage_country$ISCO08_short ~ 1,
                                      !is.na(ISCO08_3D) ~ 0,
                                      .default = NA))
  country_df_17 <- country_df %>%
    filter(REFYEAR >= 2017)
  
  lab_comp_17 <- country_df_17 %>%
    group_by(COUNTRY, is_immigrant, SEX) %>%
    summarise(underemployment_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              unemployed_mean = mean(is_unemployed, na.rm = T),
              skilled_mean = mean(high_skilled, na.rm = T),
              income_mean = mean(INCDECIL, na.rm = T),
              shortage_mean = mean(shortage_eures, na.rm = T),
              regulated_mean = mean(regulated_profession, na.rm = T)) %>%
    mutate(is_immigrant = ifelse(is_immigrant == 0, 'native', 'immigrant'),
           SEX = ifelse(SEX == 1, 'man', 'woman')) %>%
    filter(!is.na(is_immigrant), !is.na(SEX)) %>%
    pivot_wider(values_from = ends_with('mean'), names_from = 'is_immigrant') %>%
    pivot_wider(values_from = ends_with(c('immigrant', 'native')), names_from = 'SEX') %>%
    mutate(underemployment_diff_man = underemployment_mean_immigrant_man - underemployment_mean_native_man,
           overed_diff_man = overed_mean_immigrant_man - overed_mean_native_man,
           unemployed_diff_man = unemployed_mean_immigrant_man - unemployed_mean_native_man,
           skilled_diff_man = skilled_mean_immigrant_man - skilled_mean_native_man,
           income_diff_man = income_mean_immigrant_man - income_mean_native_man,
           shortage_diff_man = shortage_mean_immigrant_man - shortage_mean_native_man,
           regulated_diff_man = regulated_mean_immigrant_man - regulated_mean_native_man,
           underemployment_diff_woman = underemployment_mean_immigrant_woman - underemployment_mean_native_woman,
           overed_diff_woman = overed_mean_immigrant_woman - overed_mean_native_woman,
           unemployed_diff_woman = unemployed_mean_immigrant_woman - unemployed_mean_native_woman,
           skilled_diff_woman = skilled_mean_immigrant_woman - skilled_mean_native_woman,
           income_diff_woman = income_mean_immigrant_woman - income_mean_native_woman,
           shortage_diff_woman = shortage_mean_immigrant_woman - shortage_mean_native_woman,
           regulated_diff_woman = regulated_mean_immigrant_woman - regulated_mean_native_woman,

           underemployment_diff_woman_man = underemployment_diff_woman - underemployment_diff_man,
           overed_diff_woman_man = overed_diff_woman - overed_diff_man,
           unemployed_diff_woman_man = unemployed_diff_woman - unemployed_diff_man,
           skilled_diff_woman_man = skilled_diff_woman - skilled_diff_man,
           income_diff_woman_man = income_diff_woman - income_diff_man,
           shortage_diff_woman_man = shortage_diff_woman - shortage_diff_man,
           regulated_diff_woman_man = regulated_diff_woman - regulated_diff_man
           )

  fr <- ' ~ '
  
  if(nrow(unique(country_df_17[,'is_immigrant']))>1) fr <- paste0(fr, 'is_immigrant * SEX + ')
  if(nrow(unique(country_df_17[,'SEX']))>1) fr <-paste0(fr, 'SEX + ')
  if(nrow(unique(country_df_17[,'age_years']))>1) fr <-paste0(fr, 'age_years + age_years^2 + ')
  if(nrow(unique(country_df_17[,'HHNBCHILD']))>1) fr <-paste0(fr, 'HHNBCHILD + ')
  if(nrow(unique(country_df_17[,'is_partnered']))>1) fr <-paste0(fr, 'is_partnered + ')
  if(nrow(unique(country_df_17[,'hat_isced']))>1) fr <-paste0(fr, 'hat_isced + ')
  if(nrow(unique(country_df_17[,'REGION_2D']))>1) fr <-paste0(fr, 'as.factor(REGION_2D) + ')
  if(nrow(unique(country_df_17[,'REFYEAR']))>1) fr <-paste0(fr, 'as.factor(REFYEAR)')
  
  if(nrow(unique(country_df_17[,'uemp']))>1){
    fit_underemp_17 <- lm(data = country_df_17, formula = paste0('uemp', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(underemp_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(grepl(':', term)) %>%
      dplyr::select(estimate, underemp_significance) %>%
      rename(underemp_regression = estimate)
    lab_comp_17 <- bind_cols(lab_comp_17, fit_underemp_17) 
  }
  
  if(nrow(unique(country_df_17[,'overed_1sd_hat_isced']))>1){
    fit_overed_17 <- lm(data = country_df_17, formula = paste0('overed_1sd_hat_isced', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(overed_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(grepl(':', term)) %>%
      dplyr::select(estimate, overed_significance) %>%
      rename(overed_regression = estimate)
    lab_comp_17 <- bind_cols(lab_comp_17, fit_overed_17) 
  }
  
  if(nrow(unique(country_df_17[,'is_unemployed']))>1){
    fit_unemployed_17 <- lm(data = country_df_17, formula = paste0('is_unemployed', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(unemployed_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(grepl(':', term)) %>%
      dplyr::select(estimate, unemployed_significance) %>%
      rename(unemployed_regression = estimate)
    lab_comp_17 <- bind_cols(lab_comp_17, fit_unemployed_17) 
  }
  
  if(nrow(unique(country_df_17[,'high_skilled']))>1){
    fit_skilled_17 <- lm(data = country_df_17, formula = paste0('high_skilled', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(skilled_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(grepl(':', term)) %>%
      dplyr::select(estimate, skilled_significance) %>%
      rename(skilled_regression = estimate)
    lab_comp_17 <- bind_cols(lab_comp_17, fit_skilled_17) 
  }
  
  if(nrow(unique(country_df_17[,'INCDECIL']))>1){
    fit_income_17 <- lm(data = country_df_17, formula = paste0('INCDECIL', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(income_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(grepl(':', term)) %>%
      dplyr::select(estimate, income_significance) %>%
      rename(income_regression = estimate)
    lab_comp_17 <- bind_cols(lab_comp_17, fit_income_17) 
  }
  
  if(nrow(unique(country_df_17[,'shortage_eures']))>1){
    fit_shortage_17 <- lm(data = country_df_17, formula = paste0('shortage_eures', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(shortage_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(grepl(':', term)) %>%
      dplyr::select(estimate, shortage_significance) %>%
      rename(shortage_regression = estimate)
    lab_comp_17 <- bind_cols(lab_comp_17, fit_shortage_17) 
  }
  
  if(nrow(unique(country_df_17[,'regulated_profession']))>1){
    fit_regulated_17 <- lm(data = country_df_17, formula = paste0('regulated_profession', fr)) %>%
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame() %>%
      mutate(regulated_significance = ifelse(p.value < 0.05, 'significant', 'not_significant')) %>%
      filter(grepl(':', term)) %>%
      dplyr::select(estimate, regulated_significance) %>%
      rename(regulated_regression = estimate)
    lab_comp_17 <- bind_cols(lab_comp_17, fit_regulated_17)
  }
  
  
  lab_comp_17 <- lab_comp_17 %>%
    dplyr::select(COUNTRY, starts_with('under'), starts_with('over'), starts_with('unemp'), starts_with('skilled'), 
                  starts_with('income'), starts_with('shortage'), starts_with('regulated'))
  
  h_table_immigrant <- h_table_immigrant %>%
    bind_rows(lab_comp_17)
}

# vars_to_aggregate <- c("underemployment_diff_woman_man", "underemp_regression",
#                        "overed_diff_woman_man", "overed_regression",
#                        "unemployed_diff_woman_man", "unemployed_regression", 
#                        "skilled_diff_woman_man", "skilled_regression",
#                        "income_diff_woman_man", "income_regression",
#                        "shortage_diff_woman_man", "shortage_regression",
#                        "regulated_diff_woman_man", "regulated_regression")
# 
# h_table_immigrant2 <- h_table_immigrant %>%
#   left_join(pop, by = c('COUNTRY' = 'country_short'))
# 
# weighted_means <- numeric(length(vars_to_aggregate))
# for (i in seq_along(vars_to_aggregate)) {
#   weighted_means[i] <- weighted.mean(h_table_immigrant2[[vars_to_aggregate[i]]], h_table_immigrant2$pop_total, na.rm = T)
# }
# names(weighted_means) <- vars_to_aggregate
# weighted_means <- as.data.frame(weighted_means) %>%
#   t() %>%
#   as.data.frame() %>%
#   mutate(COUNTRY = 'EU')

h_table_immigrant <- h_table_immigrant %>%
  #bind_rows(weighted_means) %>%
  dplyr::select(-ends_with('NA'))




write.csv(h_table_immigrant, paste0(output_fp_sumtables, 'hypothesis_table_gender.csv'))
