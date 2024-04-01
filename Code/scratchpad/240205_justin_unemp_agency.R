#load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(data.table)
library(feather)
library(tidyverse)
library(openxlsx)
library(stringr)
library(arrow)

input_fp_desc_start <-  paste0(cur_date, 'Results/descriptives/')
input_fp_desc_end <- '/is_immigrant/job_through_employment_agency.csv'
input_fp_regr_start <- paste0(cur_date, 'Results/h1/')
input_fp_regr_end_basic <- '/is_immigrant/basic_interact_job_through_employment_agency.csv'
input_fp_regr_end_controls <- '/is_immigrant/full_control_interact_job_through_employment_agency.csv'

master_df_desc <- data.frame()
master_df_regr <- data.frame()

outcomes <- c('has_skilled_occupation', 'INCDECIL', 'is_temp',
              'is_unemployed', 'overed_1sd_hat_isced', 'regulated_profession',
              'uemp')

for(country in countries_to_analyze){
  print(country)
  cur_desc <- try(read.csv(paste0(input_fp_desc_start, country, input_fp_desc_end)))
  if(is(cur_desc, 'try-error')) next
  
  cur_desc <- cur_desc %>%
    filter(!is.na(job_through_employment_agency))
  
  master_df_desc <- master_df_desc %>%
    bind_rows(cur_desc)
  
  for(cur_outcome in outcomes){
    cur_regr_basic <- try(read.csv(paste0(input_fp_regr_start, country, '/', cur_outcome, input_fp_regr_end_basic)))
    if(is(cur_regr_basic, 'try-error')) next
    
    cur_regr_basic <- cur_regr_basic %>%
      filter(grepl(':', term)) %>%
      mutate(COUNTRY = country,
             outcome = cur_outcome,
             controls = 'basic')
    
    master_df_regr <- master_df_regr %>%
      bind_rows(cur_regr_basic)
    
    cur_regr_controls <- try(read.csv(paste0(input_fp_regr_start, country, '/', cur_outcome, input_fp_regr_end_controls)))
    if(is(cur_regr_controls, 'try-error')) next
    
    cur_regr_controls <- cur_regr_controls %>%
      filter(grepl(':', term)) %>%
      mutate(COUNTRY = country,
             outcome = cur_outcome,
             controls = 'controls')
    
    master_df_regr <- master_df_regr %>%
      bind_rows(cur_regr_controls)
    
  }
}

#save master files
output_fp <- paste0(cur_date, 'Results/scratchpad/unemp_agency/')
dir.create(output_fp, showWarnings = F)
write.csv(master_df_desc, paste0(output_fp, 'unemp_agency_descriptive.csv'))
write.csv(master_df_regr, paste0(output_fp, 'unemp_agency_regression.csv'))

master_df_desc <- master_df_desc %>%
  filter(n > 50, !is.na(is_immigrant))

master_df_regr <- master_df_regr %>%
  mutate(sig_level = case_when(p.value < 0.01 ~ '***',
                               p.value < 0.05 ~ '**',
                               p.value < 0.1 ~ '*',
                               .default = 'not sig'))
#loop over outcome vars
for(cur_outcome in outcomes){
  p <- ggplot(master_df_desc,  aes(x=REFYEAR, y=!!sym(paste0(cur_outcome, '_mean')), colour= as.factor(is_immigrant), shape = as.factor(job_through_employment_agency)))+
    geom_point()+
    facet_wrap(.~COUNTRY)+
    labs(title = paste0(cur_outcome, ' by Migr. Status and Found Last Job Through Agency'),
         x = 'Year', y = cur_outcome,
         color = 'is immigrant',
         shape = 'job through agency')
  ggsave(paste0(output_fp, cur_outcome, '_descriptive.png'), width = 8, height = 12, plot = last_plot())
  
  cur_regr <- master_df_regr %>%
    filter(outcome == cur_outcome)
  
  p <- ggplot(cur_regr, aes(x = controls, y = estimate, color = sig_level))+
    geom_point()+
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high))+
    facet_wrap(.~COUNTRY)+
    labs(title = paste0('Interaction Effect (Migr Status x Found Last Job Through Agency) on ', cur_outcome),
         x = 'Model Type', y = 'Effect Size',
         color = 'Significance level')
  ggsave(paste0(output_fp, cur_outcome, '_regression.png'), width = 8, height = 12, plot = last_plot())
}
#make separate plots for desc and regression
