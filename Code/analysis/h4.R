#load libraries
library(dplyr)
library(tidyr)
library(readr)
library(feather)
library(openxlsx)
library(ggplot2)
library(arrow)
library(broom)
library(scales)
library(stringr)

input_fp <- 'Input Data/ELF_Merged/final/merged_country_final_2006_onwards_'
output_fp <- paste0(cur_date, 'Results/h4/')
dir.create(output_fp, showWarnings = F)

df_educ_abroad_desc_master <- data.frame()
df_educ_abroad_regr_master <- data.frame()
df_recognition_desc_master <- data.frame()
df_recognition_regr_master <- data.frame()
df_lang_desc_master <- data.frame()
df_lang_regr_master <- data.frame()
df_langcour_desc_master <- data.frame()
df_langcour_regr_master <- data.frame()
df_residence_desc_master <- data.frame()
df_residence_regr_master <- data.frame()
df_migreas_desc_master <- data.frame()
df_migreas_regr_master <- data.frame()

for(country in countries_to_analyze){ 
  print(country)
  cur_df <- arrow::read_feather(paste0(input_fp, country, '.feather'))
  
  cur_df <- cur_df %>%
    mutate(educated_abroad_factor = case_when(is_immigrant == 0 ~ 'native',
                                              is_immigrant == 1 & educated_abroad == 0 ~ 'immigrant_host_educ',
                                              is_immigrant == 1 & educated_abroad == 1 ~ 'immigrant_send_educ',
                                              .default = NA),
           educated_abroad_factor = relevel(as.factor(educated_abroad_factor), 'native'), #TODO: might need to reoder levels
           received_recognition_factor = case_when(is_immigrant == 0 ~ 'native',
                                                   is_immigrant == 1 & received_recognition == 0 ~ 'immigrant_no_recog',
                                                   is_immigrant == 1 & received_recognition == 1 ~ 'immigrant_yes_recog',
                                                   is_immigrant == 1 ~ 'immigrant_NA_recog',
                                                   .default = NA),
           received_recognition_factor = relevel(as.factor(received_recognition_factor), 'native'),
           host_lang_factor = case_when(is_immigrant == 0 ~'native',
                                         is_immigrant == 1 & LANGHOST %in% c(1, 2) ~'immigrant_prof',
                                         is_immigrant == 1 & LANGHOST %in% c(3, 4, 5) ~'immigrant_some',
                                         is_immigrant == 1 ~'immigrant_NA',
                                         .default = NA),
           host_lang_factor = relevel(as.factor(host_lang_factor), 'native'),
           yearesid_10_years_factor = case_when(is.na(yearesid_10_years) & is_immigrant == 0 ~ 'native',
                                                yearesid_10_years == 'More than 10y' ~ 'immigrant_gt_10',
                                                yearesid_10_years == 'Less than 10y' ~ 'immigrant_lt_10',
                                                .default = yearesid_10_years),
           yearesid_10_years_factor = relevel(as.factor(yearesid_10_years_factor), 'native'),
           langcour_factor = case_when(is.na(LANGCOUR) & is_immigrant == 0 ~ 'native',
                                        LANGCOUR %in% c(1, 2) ~ 'immigr_yes_langcour',
                                        LANGCOUR %in% c(3, 5) ~ 'immigr_no_langcour',
                                        LANGCOUR == 4 ~ 'immigr_langcour_sufficient',
                                        .default = NA),
           langcour_factor = relevel(as.factor(langcour_factor), 'native'),
           migreas_factor = case_when(is.na(MIGREAS) & is_immigrant == 0 ~ 'native',
                                       MIGREAS %in% c(1,2) ~ 'immigr_employment',
                                       MIGREAS == 3 ~ 'immigr_family',
                                       MIGREAS == 4 ~ 'immigr_educ',
                                       MIGREAS == 6 ~ 'immigr_asylum',
                                       !is.na(MIGREAS) ~ 'immigr_other',
                                       .default = NA),
           migreas_factor = relevel(as.factor(migreas_factor), 'native')
           )
  cur_df_17 <- cur_df %>% #for educated_abroad, yeares
    filter(REFYEAR > 2016)
  
  cur_df_21 <- cur_df %>% #received recognition, host_lang_factor
    filter(REFYEAR == 2021)
  
  df_educ_abroad_desc <- cur_df_17 %>%
    group_by(COUNTRY, educated_abroad_factor) %>%
    summarise(uemp_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              unemp_mean = mean(is_unemployed, na.rm = T),
              n = n())
  df_educ_abroad_desc_master <- df_educ_abroad_desc_master %>%
    bind_rows(df_educ_abroad_desc)
  
  df_residence_desc <- cur_df_17 %>%
    group_by(COUNTRY, yearesid_10_years_factor) %>%
    summarise(uemp_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              unemp_mean = mean(is_unemployed, na.rm = T),
              n = n())
  df_residence_desc_master <- df_residence_desc_master %>%
    bind_rows(df_residence_desc)
  
  df_recognition_desc <- cur_df_21 %>%
    group_by(COUNTRY, received_recognition_factor) %>%
    summarise(uemp_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              unemp_mean = mean(is_unemployed, na.rm = T),
              n = n())
  df_recognition_desc_master <- df_recognition_desc_master %>%
    bind_rows(df_recognition_desc)
  
  df_lang_desc <- cur_df_21 %>%
    group_by(COUNTRY, host_lang_factor) %>%
    summarise(uemp_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              unemp_mean = mean(is_unemployed, na.rm = T),
              n = n())
  df_lang_desc_master <- df_lang_desc_master %>%
    bind_rows(df_lang_desc)
  
  df_langcour_desc <- cur_df_21 %>%
    group_by(COUNTRY, langcour_factor) %>%
    summarise(uemp_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              unemp_mean = mean(is_unemployed, na.rm = T),
              n = n())
  df_langcour_desc_master <- df_langcour_desc_master %>%
    bind_rows(df_langcour_desc)
  
  df_migreas_desc <- cur_df_21 %>%
    group_by(COUNTRY, migreas_factor) %>%
    summarise(uemp_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              unemp_mean = mean(is_unemployed, na.rm = T),
              n = n())
  df_migreas_desc_master <-df_migreas_desc_master %>%
    bind_rows(df_migreas_desc)
  
  outcome_vars <- c('uemp', 'overed_1sd_hat_isced', 'is_unemployed')
  control_vars <- c('SEX', 'age_years', 'is_partnered', 'hat_isced', 'HHNBCHILD', 'REGION_2D', 'REFYEAR')
  for(outcome in outcome_vars){
    control_vec_17 <- ''
    control_vec_21 <- ''
    for(control in control_vars){
      if(n_distinct(cur_df_17[[control]]) > 1) {
        control_vec_17 <- paste0(control_vec_17, ' + ', control)
      }
      if(n_distinct(cur_df_21[[control]]) > 1) {
        control_vec_21 <- paste0(control_vec_21, ' + ', control)
      }
    }
    control_vec_17 <- str_replace_all(control_vec_17, c('age_years' = 'age_years + age_years^2',
                                                        'REGION_2D' = 'as.factor(REGION_2D)',
                                                        'REFYEAR' = 'as.factor(REFYEAR)'))
    
    control_vec_21 <- str_replace_all(control_vec_21, c('age_years' = 'age_years + age_years^2',
                                                        'REGION_2D' = 'as.factor(REGION_2D)',
                                                        'REFYEAR' = 'as.factor(REFYEAR)'))
    #educated abroad regression
    formula_educ_abroad <- paste0(outcome, ' ~ educated_abroad_factor', control_vec_17)
    fit_educ_abroad <- try(lm(data = cur_df_17, formula = formula_educ_abroad))
    if(is(fit_educ_abroad, 'try-error')) next
    
    fit_educ_abroad_df <- fit_educ_abroad %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      filter(grepl('educated_abroad_factor', term)) %>%
      mutate(term = str_replace(term, 'educated_abroad_factor', ''),
             COUNTRY = country,
             dv = outcome) %>%
      as.data.frame()
    
    df_educ_abroad_regr_master <- df_educ_abroad_regr_master %>%
      bind_rows(fit_educ_abroad_df)
    
    # received recognition regression
    formula_received_recognition <- paste0(outcome, ' ~ received_recognition_factor', control_vec_21)
    fit_received_recognition <- try(lm(data = cur_df_21, formula = formula_received_recognition))
    if(is(fit_received_recognition, 'try-error')) next
    
    fit_received_recognition_df <- fit_received_recognition %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      filter(grepl('received_recognition_factor', term)) %>%
      mutate(term = str_replace(term, 'received_recognition_factor', ''),
             COUNTRY = country,
             dv = outcome) %>%
      as.data.frame()
    
    df_recognition_regr_master <- df_recognition_regr_master %>%
      bind_rows(fit_received_recognition_df)
    
    # host lang skills regression
    formula_lang <- paste0(outcome, ' ~ host_lang_factor', control_vec_21)
    fit_lang <- try(lm(data = cur_df_21, formula = formula_lang))
    if(is(fit_lang, 'try-error')) next
    
    fit_lang_df <- fit_lang %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      filter(grepl('host_lang_factor', term)) %>%
      mutate(term = str_replace(term, 'host_lang_factor', ''),
             COUNTRY = country,
             dv = outcome) %>%
      as.data.frame()
    
    df_lang_regr_master <- df_lang_regr_master %>%
      bind_rows(fit_lang_df)
    
    # time in country regression
    formula_residence <- paste0(outcome, ' ~ yearesid_10_years_factor', control_vec_17)
    fit_residence <- try(lm(data = cur_df_17, formula = formula_residence))
    if(is(fit_residence, 'try-error')) next
    
    fit_residence_df <- fit_residence %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      filter(grepl('yearesid_10_years_factor', term)) %>%
      mutate(term = str_replace(term, 'yearesid_10_years_factor', ''),
             COUNTRY = country,
             dv = outcome) %>%
      as.data.frame()
    
    df_residence_regr_master <- df_residence_regr_master %>%
      bind_rows(fit_residence_df)
    
    #language course regression
    formula_langcour <- paste0(outcome, ' ~ langcour_factor', control_vec_21)
    fit_langcour <- try(lm(data = cur_df_21, formula = formula_langcour))
    if(is(fit_langcour, 'try-error')) next
    
    fit_langcour_df <- fit_langcour %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      filter(grepl('langcour_factor', term)) %>%
      mutate(term = str_replace(term, 'langcour_factor', ''),
             COUNTRY = country,
             dv = outcome) %>%
      as.data.frame()
    
    df_langcour_regr_master <- df_langcour_regr_master %>%
      bind_rows(fit_langcour_df)
    
    #reason for migration regression
    formula_migreas <- paste0(outcome, ' ~ migreas_factor', control_vec_21)
    fit_migreas <- try(lm(data = cur_df_21, formula = formula_migreas))
    if(is(fit_migreas, 'try-error')) next
    
    fit_migreas_df <- fit_migreas %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      filter(grepl('migreas_factor', term)) %>%
      mutate(term = str_replace(term, 'migreas_factor', ''),
             COUNTRY = country,
             dv = outcome) %>%
      as.data.frame()
    
    df_migreas_regr_master <- df_migreas_regr_master %>%
      bind_rows(fit_migreas_df)
  }
}

pop <- read.csv('Input Data/eurostat/pop_transformed.csv') %>%
  filter(year == 2019) %>%
  dplyr::select(country_short, total) %>%
  rename(pop_total = total)

htable_college <- read.csv(paste0(cur_date, 'Results/scratchpad/sumtables/hypothesis_table_college.csv')) %>%
  dplyr::select(COUNTRY, share_college)

pop <- htable_college %>%
  left_join(pop, by = c('COUNTRY' = 'country_short')) %>%
  mutate(pop_college = pop_total * share_college)

df_educ_abroad_desc_EU <- df_educ_abroad_desc_master %>%
  left_join(pop, by = 'COUNTRY') %>%
  group_by(educated_abroad_factor) %>%
  summarise(uemp_mean = weighted.mean(uemp_mean, pop_college, na.rm = T),
            overed_mean = weighted.mean(overed_mean, pop_college, na.rm = T),
            unemp_mean = weighted.mean(unemp_mean, pop_college, na.rm = T)) %>%
  mutate(COUNTRY = 'EU')

df_educ_abroad_desc_master <- df_educ_abroad_desc_master %>%
  bind_rows(df_educ_abroad_desc_EU)

df_lang_desc_EU <- df_lang_desc_master %>%
  left_join(pop, by = 'COUNTRY') %>%
  group_by(host_lang_factor) %>%
  summarise(uemp_mean = weighted.mean(uemp_mean, pop_college, na.rm = T),
            overed_mean = weighted.mean(overed_mean, pop_college, na.rm = T),
            unemp_mean = weighted.mean(unemp_mean, pop_college, na.rm = T)) %>%
  mutate(COUNTRY = 'EU')

df_lang_desc_master <- df_lang_desc_master %>%
  bind_rows(df_lang_desc_EU)

df_recognition_desc_EU <- df_recognition_desc_master %>%
  left_join(pop, by = 'COUNTRY') %>%
  group_by(received_recognition_factor) %>%
  summarise(uemp_mean = weighted.mean(uemp_mean, pop_college, na.rm = T),
            overed_mean = weighted.mean(overed_mean, pop_college, na.rm = T),
            unemp_mean = weighted.mean(unemp_mean, pop_college, na.rm = T)) %>%
  mutate(COUNTRY = 'EU')

df_recognition_desc_master <- df_recognition_desc_master %>%
  bind_rows(df_recognition_desc_EU)

df_residence_desc_EU <- df_residence_desc_master %>%
  left_join(pop, by = 'COUNTRY') %>%
  group_by(yearesid_10_years_factor) %>%
  summarise(uemp_mean = weighted.mean(uemp_mean, pop_college, na.rm = T),
            overed_mean = weighted.mean(overed_mean, pop_college, na.rm = T),
            unemp_mean = weighted.mean(unemp_mean, pop_college, na.rm = T)) %>%
  mutate(COUNTRY = 'EU')

df_residence_desc_master <- df_residence_desc_master %>%
  bind_rows(df_residence_desc_EU)

df_langcour_desc_EU <- df_langcour_desc_master %>%
  left_join(pop, by = 'COUNTRY') %>%
  group_by(langcour_factor) %>%
  summarise(uemp_mean = weighted.mean(uemp_mean, pop_college, na.rm = T),
            overed_mean = weighted.mean(overed_mean, pop_college, na.rm = T),
            unemp_mean = weighted.mean(unemp_mean, pop_college, na.rm = T)) %>%
  mutate(COUNTRY = 'EU')

df_langcour_desc_master <- df_langcour_desc_master %>%
  bind_rows(df_langcour_desc_EU)

df_migreas_desc_EU <- df_migreas_desc_master %>%
  left_join(pop, by = 'COUNTRY') %>%
  group_by(migreas_factor) %>%
  summarise(uemp_mean = weighted.mean(uemp_mean, pop_college, na.rm = T),
            overed_mean = weighted.mean(overed_mean, pop_college, na.rm = T),
            unemp_mean = weighted.mean(unemp_mean, pop_college, na.rm = T)) %>%
  mutate(COUNTRY = 'EU')

df_migreas_desc_master <- df_migreas_desc_master %>%
  bind_rows(df_migreas_desc_EU)

write.csv(df_educ_abroad_desc_master, paste0(output_fp, 'educated_abroad_desc.csv'))
write.csv(df_educ_abroad_regr_master, paste0(output_fp, 'educated_abroad_regr.csv'))
write.csv(df_lang_desc_master, paste0(output_fp, 'host_lang_desc.csv'))
write.csv(df_lang_regr_master, paste0(output_fp, 'host_lang_regr.csv'))
write.csv(df_recognition_desc_master, paste0(output_fp, 'recognition_desc.csv'))
write.csv(df_recognition_regr_master, paste0(output_fp, 'recognition_regr.csv'))
write.csv(df_residence_desc_master, paste0(output_fp, 'residence_desc.csv'))
write.csv(df_residence_regr_master, paste0(output_fp, 'residence_regr.csv'))
write.csv(df_langcour_desc_master, paste0(output_fp, 'langcour_desc.csv'))
write.csv(df_langcour_regr_master, paste0(output_fp, 'langcour_regr.csv'))
write.csv(df_migreas_desc_master, paste0(output_fp, 'migreas_desc.csv'))
write.csv(df_migreas_regr_master, paste0(output_fp, 'migreas_regr.csv'))



