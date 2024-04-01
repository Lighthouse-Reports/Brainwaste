#load libraries
library(dplyr)
library(tidyr)
library(readr)
library(arrow)
library(openxlsx)
library(stringr)
library(ggplot2)
library(eurostat)
library(sf)
library(wesanderson)
library(stringr)
library(ggh4x)
library(rex)
library(tidyverse)
library(tidytext)

setwd("/Users/justin-casimirbraun/Brainwaste")



fp_1 <- 'Input Data/ELF_Merged/final/merged_country_final_2006_onwards_'
fp_2 <- '.feather'
master_df_hatfield <- data.frame()
master_df_occupation <- data.frame()
master_df_occupation_hatfield <- data.frame()
for(country in countries_to_analyze){
  fp <- paste0(fp_1, country, fp_2)
  cur_df <- arrow::read_feather(fp)
  cur_df_hatfield <- cur_df %>%
    filter(REFYEAR > 2016, !is.na(hatfield1d)) %>%
    group_by(COUNTRY, is_immigrant, SEX, hatfield1d) %>%
    summarise(uemp_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              n = n())
  
  master_df_hatfield <- master_df_hatfield %>%
    bind_rows(cur_df_hatfield)
  
  cur_df_occupation <- cur_df %>%
    filter(REFYEAR > 2016, !is.na(ISCO08_3D)) %>%
    group_by(COUNTRY, SEX, is_immigrant, ISCO08_3D) %>%
    summarise(uemp_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              n = n())
  
  master_df_occupation <- master_df_occupation %>%
    bind_rows(cur_df_occupation)
  
  cur_df_hatfield_occupation <- cur_df %>%
    filter(REFYEAR > 2016, is_immigrant == 1, !is.na(hatfield1d), !is.na(ISCO08_3D)) %>%
    group_by(COUNTRY, SEX, hatfield1d, ISCO08_3D) %>%
    summarise(share_uemp = mean(uemp, na.rm = T),
              share_overed = mean(overed_1sd_hat_isced, na.rm = T),
              count = n()) 
  
  master_df_occupation_hatfield <- master_df_occupation_hatfield %>%
    bind_rows(cur_df_hatfield_occupation)
}

master_df_hatfield <- master_df_hatfield %>%
  filter(!is.na(hatfield1d), !is.na(is_immigrant)) %>%
  dplyr::mutate(hatfield_text = case_when(hatfield1d == 0 ~ 'basic',
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
                                          .default = 'Unknown Field'))

master_df_hatfield_native <- master_df_hatfield %>%
  filter(is_immigrant == 0) %>%
  rename(uemp_mean_native = uemp_mean,
         overed_mean_native = overed_mean,
         n_native = n) %>%
  group_by(COUNTRY, SEX) %>%
  mutate(share_native = n_native/sum(n_native, na.rm = T)) %>%
  ungroup()

master_df_hatfield_immigrant <- master_df_hatfield %>%
  filter(is_immigrant == 1) %>%
  rename(uemp_mean_immigrant = uemp_mean,
         overed_mean_immigrant = overed_mean,
         n_immigrant = n)%>%
  group_by(COUNTRY, SEX) %>%
  mutate(share_immigrant = n_immigrant/sum(n_immigrant, na.rm = T)) %>%
  ungroup()


master_df_hatfield_complete <- left_join(master_df_hatfield_native, dplyr::select(master_df_hatfield_immigrant, -hatfield_text), by = c('COUNTRY', 'SEX', 'hatfield1d')) %>%
  dplyr::select(-is_immigrant.x, -is_immigrant.y) %>%
  mutate(uemp_diff = uemp_mean_immigrant - uemp_mean_native,
         overed_diff = overed_mean_immigrant - overed_mean_native)

outpath <- paste0(cur_date, 'Results/scratchpad/rq03_01/')
dir.create(outpath, showWarnings = F)

write.csv(master_df_hatfield_complete, paste0(outpath, 'hatfield_overed_underemp_diff_SEX.csv'))

master_df_occupation_native <- master_df_occupation %>%
  filter(is_immigrant == 0) %>%
  rename(uemp_mean_native = uemp_mean,
         overed_mean_native = overed_mean,
         n_native = n) %>%
  dplyr::select(-is_immigrant) %>%
  group_by(COUNTRY, SEX) %>%
  mutate(share_native = n_native/sum(n_native, na.rm = T)) %>%
  ungroup()

master_df_occupation_immigrant <- master_df_occupation %>%
  filter(is_immigrant == 1) %>%
  rename(uemp_mean_immigrant = uemp_mean,
         overed_mean_immigrant = overed_mean,
         n_immigrant = n) %>%
  dplyr::select(-is_immigrant) %>%
  group_by(COUNTRY, SEX) %>%
  mutate(share_immigrant = n_immigrant/sum(n_immigrant, na.rm = T)) %>%
  ungroup()

isco <- read.xlsx('Input Data/scratchpad/rq_03_01/struct08.xlsx') %>%
  mutate(ISCO = as.numeric(ISCO))

master_df_occupation_complete <- left_join(master_df_occupation_native, master_df_occupation_immigrant, by = c('COUNTRY', 'SEX', 'ISCO08_3D')) %>%
  left_join(isco, by = c('ISCO08_3D' = 'ISCO')) %>%
  mutate(uemp_diff = uemp_mean_immigrant - uemp_mean_native,
         overed_diff = overed_mean_immigrant - overed_mean_native)


write.csv(master_df_occupation_complete, paste0(outpath, 'occupation_overed_underemp_diff_SEX.csv'))

master_df_occupation_hatfield <- master_df_occupation_hatfield %>%
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
  left_join(isco, by = c('ISCO08_3D' = 'ISCO')) %>%
  group_by(COUNTRY, SEX) %>%
  mutate(share = count/sum(count, na.rm = T)) %>%
  ungroup()
write.csv(master_df_occupation_hatfield, paste0(outpath, 'education_occupation_brainwaste_SEX.csv'))
