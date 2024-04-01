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

#set up fps
input_fp <- 'Input Data/ELF_Merged/final/merged_country_final_2006_onwards_'
output_fp <- paste0(cur_date, 'Results/h3_2/')
dir.create(output_fp, showWarnings = F)

cur_year <- 2020

gdp <- read.csv('Input Data/eurostat/gdp_transformed.csv') %>%
  dplyr::select(country_short, gdp)

master_df <- data.frame(country = character(),
                        real_wage_mean = numeric(),
                        real_wage_sd = numeric(),
                        basic_predicted_wage_mean = numeric(),
                        basic_predicted_wage_sd = numeric(),
                        educ_predicted_wage_mean = numeric(),
                        educ_predicted_wage_sd = numeric(),
                        basic_wage_diff_mean = numeric(),
                        basic_wage_diff_sd = numeric(),
                        educ_wage_diff_mean = numeric(),
                        educ_wage_diff_sd = numeric (),
                        diff_uemp_rate = numeric(),
                        diff_overed_rate = numeric(),
                        uemp_penalty = numeric(),
                        overed_penalty = numeric()
                        )

for(country in countries_to_analyze){
  print(country)
  country_df <- arrow::read_feather(paste0(input_fp, country, '_ALL.feather'))
  
  country_df <- country_df %>%
      mutate(hatfield1d = as.factor(hatfield1d),
             REGION_2D = as.factor(REGION_2D))
  
  native_df <- country_df %>%
    filter(REFYEAR == cur_year, is_immigrant == 0)
  
  fr <- 'wages_annual ~'
  if(nrow(unique(native_df[,'wages_annual']))<=1) next
  if(nrow(unique(native_df[,'age_years']))>1) fr <-paste0(fr, 'age_years + age_years^2')
  if(nrow(unique(native_df[,'SEX']))>1) fr <-paste0(fr, '+ SEX')
  if(nrow(unique(native_df[,'HHNBCHILD']))>1) fr <-paste0(fr, '+ HHNBCHILD')
  if(nrow(unique(native_df[,'is_partnered']))>1) fr <-paste0(fr, '+ is_partnered')
  if(nrow(unique(native_df[,'hat_isced']))>1) fr <-paste0(fr, '+ hat_isced')
  if(nrow(unique(native_df[,'REGION_2D']))>1) fr <-paste0(fr, '+ as.factor(REGION_2D)')
  
  wage_fit_basic <- try(lm(data = native_df, formula = fr))
  if(is(wage_fit_basic, 'try-error')) print('Wage fit basic failed')
  wage_fit_educ <- try(lm(data = native_df, formula = paste0(fr, '+ hatfield1d')))
  if(is(wage_fit_educ, 'try-error')) print('Wage fit educ failed')
  wage_fit_uemp <- try(lm(data = native_df, formula = paste0(fr, '+ uemp')))
  if(is(wage_fit_uemp, 'try-error')) print('Wage fit uemp failed')
  wage_fit_overed <- try(lm(data = native_df, formula = paste0(fr, '+ overed_1sd_hat_isced')))
  if(is(wage_fit_overed, 'try-error')) print('Wage fit overed failed')
  
  immigrant_df <- country_df %>%
    filter(REFYEAR == 2019, is_immigrant == 1)
  
  predicted_earnings_basic <- try(predict(wage_fit_basic, newdata = immigrant_df))
  if(is(predicted_earnings_basic, 'try-error')) next
  immigrant_df$predicted_earnings_basic <- predicted_earnings_basic
  predicted_earnings_educ <- try(predict(wage_fit_educ, newdata = immigrant_df))
  if(is(predicted_earnings_educ, 'try-error')) next
  immigrant_df$predicted_earnings_educ <- predicted_earnings_educ
  
  immigrant_df <- immigrant_df %>%
    mutate(wage_diff_basic = predicted_earnings_basic - wages_annual,
           wage_diff_educ = predicted_earnings_educ - wages_annual)
  
  cur_sum_table <- immigrant_df %>%
    summarise(real_wage_mean = mean(wages_annual, na.rm = T),
              real_wage_sd = sd(wages_annual, na.rm = T),
              basic_predicted_wage_mean = mean(predicted_earnings_basic, na.rm = T),
              basic_predicted_wage_sd = sd(predicted_earnings_basic, na.rm = T),
              educ_predicted_wage_mean = mean(predicted_earnings_educ, na.rm = T),
              educ_predicted_wage_sd = sd(predicted_earnings_educ, na.rm = T),
              basic_wage_diff_mean = mean(wage_diff_basic, na.rm = T),
              basic_wage_diff_sd = sd(wage_diff_basic, na.rm = T),
              educ_wage_diff_mean = mean(wage_diff_educ, na.rm = T),
              educ_wage_diff_sd = sd(wage_diff_educ, na.rm = T))
  
  cur_sum_table$diff_uemp_rate <-  mean(immigrant_df$uemp, na.rm = T) - mean(native_df$uemp, na.rm = T)
  cur_sum_table$diff_overed_rate <-  mean(immigrant_df$overed_1sd_hat_isced, na.rm = T) - mean(native_df$overed_1sd_hat_isced, na.rm = T)
  cur_sum_table$uemp_penalty <- wage_fit_uemp$coefficients['uemp']
  cur_sum_table$overed_penalty <- wage_fit_overed$coefficients['overed_1sd_hat_isced']
  cur_sum_table$country <- country
  
  master_df <- master_df %>%
    bind_rows(cur_sum_table)
}


employed_immigrants <- read.csv('Input Data/eurostat/employed_immigrants_transformed.csv') %>%
  dplyr::select(country_short, count) %>%
  rename(employed_immigrants = count) %>%
  mutate(employed_immigrants = as.numeric(employed_immigrants))
  

master_df <- master_df %>%
  left_join(employed_immigrants, by = c('country' = 'country_short')) %>%
  mutate(macro_potential_basic = basic_wage_diff_mean * employed_immigrants,
         macro_potential_educ = educ_wage_diff_mean * employed_immigrants,
         macro_potential_uemp = diff_uemp_rate * uemp_penalty * employed_immigrants *(-1),
         macro_potential_overed = diff_overed_rate * overed_penalty * employed_immigrants*(-1))

eu_row <- data.frame(country = 'EU',
                     real_wage_mean = mean(master_df$real_wage_mean, na.rm = T),
                     basic_predicted_wage_mean = mean(master_df$basic_predicted_wage_mean, na.rm = T),
                     educ_predicted_wage_mean = mean(master_df$educ_predicted_wage_mean, na.rm = T),
                     basic_wage_diff_mean = mean(master_df$basic_wage_diff_mean, na.rm = T),
                     educ_wage_diff_mean = mean(master_df$educ_wage_diff_mean, na.rm = T),
                     uemp_penalty = mean(master_df$uemp_penalty, na.rm = T),
                     overed_penalty = mean(master_df$overed_penalty, na.rm = T),
                     employed_immigrants = sum(master_df$employed_immigrants, na.rm = T),
                     macro_potential_basic = sum(master_df$macro_potential_basic, na.rm = T),
                     macro_potential_educ = sum(master_df$macro_potential_educ, na.rm = T),
                     macro_potential_uemp = sum(master_df$macro_potential_uemp, na.rm = T),
                     macro_potential_overed = sum(master_df$macro_potential_overed, na.rm = T))
master_df <- master_df %>%
  bind_rows(eu_row)


master_df_gdp <- master_df %>%
  left_join(gdp, by = c('country' = 'country_short')) %>%
  mutate(gdp = ifelse(country == 'EU', sum(gdp, na.rm = T), gdp)) %>%
  mutate(macro_potential_basic_share_gdp = macro_potential_basic/gdp,
            macro_potential_educ_share_gdp = macro_potential_educ/gdp,
            macro_potential_uemp_share_gdp = macro_potential_uemp/gdp,
            macro_potential_overed_share_gdp = macro_potential_overed/gdp)

write.csv(master_df_gdp, paste0(output_fp, 'earnings_potential_immigrants_', as.character(cur_year), '.csv'))


master_df_long <- master_df %>%
  filter(country != 'EU') %>%
  dplyr::select(c('country', starts_with('macro'))) %>%
  pivot_longer(cols = starts_with('macro'), names_to = 'potential_type', values_to = 'euro')

ggplot(master_df_long, aes(x = potential_type, y = euro))+
  geom_bar(stat = 'identity', position = 'dodge')+
  facet_wrap(.~country, scales = 'free_y')+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(paste0(output_fp, 'earnings_potential_immigrants', as.character(cur_year), '.png'), plot = last_plot(), width = 15, height = 20)
