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

countries_to_analyze <- c('AT',
                          'BE', 'BG', 'CH',
                          'CY', 'CZ', 'DK', 'EE',
                          'EL', 'ES', 'FI', 'FR',
                          'HR', 'HU', 'IE', 'IS', 
                          'IT', 'LT', 'LU', 'LV',
                          'MT', 'NL', 'NO', 'PL',
                          'PT', 'RO', 'SE', 'SI',
                          'SK', 'UK'
)

fp_1 <- 'Input Data/ELF_Merged/final/merged_country_final_2006_onwards_'
fp_2 <- '.feather'
master_df_hatfield <- data.frame()
master_df_occupation <- data.frame()
master_df_occupation_hatfield <- data.frame()
for(country in 'EL'){
  fp <- paste0(fp_1, country, fp_2)
  cur_df <- arrow::read_feather(fp)
  cur_df_hatfield <- cur_df %>%
    filter(REFYEAR > 2016) %>%
    group_by(COUNTRY, is_immigrant, hatfield1d) %>%
    summarise(uemp_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              n = n())

  master_df_hatfield <- master_df_hatfield %>%
    bind_rows(cur_df_hatfield)

  cur_df_occupation <- cur_df %>%
    filter(REFYEAR > 2016) %>%
    group_by(COUNTRY, is_immigrant, ISCO08_3D) %>%
    summarise(uemp_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              n = n())
  master_df_occupation <- master_df_occupation %>%
    bind_rows(cur_df_occupation)
  
  cur_df_hatfield_occupation <- cur_df %>%
    filter(REFYEAR > 2016, is_immigrant == 1, !is.na(hatfield1d), !is.na(ISCO08_3D)) %>%
    group_by(COUNTRY, hatfield1d, ISCO08_3D) %>%
    summarise(share_uemp = mean(uemp, na.rm = T),
              share_overed = mean(overed_1sd_hat_isced, na.rm = T),
              count = n()) %>%
    filter(count > 20)
  
  master_df_occupation_hatfield <- master_df_occupation_hatfield %>%
    bind_rows(cur_df_hatfield_occupation)
}

master_df_hatfield <- master_df_hatfield %>%
  filter(!is.na(hatfield1d), !is.na(is_immigrant), n > 20) %>%
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
         n_native = n)

master_df_hatfield_immigrant <- master_df_hatfield %>%
  filter(is_immigrant == 1) %>%
  rename(uemp_mean_immigrant = uemp_mean,
         overed_mean_immigrant = overed_mean,
         n_immigrant = n)

master_df_hatfield_complete <- left_join(master_df_hatfield_native, select(master_df_hatfield_immigrant, -hatfield_text), by = c('COUNTRY', 'hatfield1d')) %>%
  select(-is_immigrant.x, -is_immigrant.y) %>%
  mutate(uemp_diff = uemp_mean_immigrant - uemp_mean_native,
         overed_diff = overed_mean_immigrant - overed_mean_native)

ggplot(master_df_hatfield_complete, aes(x = reorder_within(COUNTRY, uemp_diff, hatfield_text), y = uemp_diff))+
  scale_x_reordered()+
  geom_bar(stat = 'identity', position = 'dodge', width = 0.5)+
  facet_wrap(hatfield_text ~., ncol = 1, scales = 'free_x')+
  ggtitle('2017-2021: Difference in Underemployment between Immigrants and Natives')

outpath <- 'Results/scratchpad/rq03_01/'
dir.create(outpath, showWarnings = F)
ggsave(paste0(outpath, 'hatfield1d_uemp_country.png'), plot = last_plot(), width = 20, height = 30)  


ggplot(master_df_hatfield_complete, aes(x = reorder_within(COUNTRY, overed_diff, hatfield_text), y = overed_diff))+
  scale_x_reordered()+
  geom_bar(stat = 'identity', position = 'dodge', width = 0.5)+
  facet_wrap(hatfield_text ~., ncol = 1, scales = 'free_x')+
  ggtitle('2017-2021: Difference in Over-education between Immigrants and Natives')

ggsave(paste0(outpath, 'hatfield1d_overed_country.png'), plot = last_plot(), width = 20, height = 30)  

write.csv(master_df_hatfield_complete, paste0(outpath, 'hatfield_overed_underemp_diff.csv'))

master_df_occupation_native <- master_df_occupation %>%
  filter(is_immigrant == 0, n > 20) %>%
  rename(uemp_mean_native = uemp_mean,
         overed_mean_native = overed_mean,
         n_native = n) %>%
  select(-is_immigrant)

master_df_occupation_immigrant <- master_df_occupation %>%
  filter(is_immigrant == 1, n > 20) %>%
  rename(uemp_mean_immigrant = uemp_mean,
         overed_mean_immigrant = overed_mean,
         n_immigrant = n) %>%
  select(-is_immigrant)

isco <- read.xlsx(paste0(outpath, 'struct08.xlsx')) %>%
  mutate(ISCO = as.numeric(ISCO))
  
master_df_occupation_complete <- left_join(master_df_occupation_native, master_df_occupation_immigrant, by = c('COUNTRY', 'ISCO08_3D')) %>%
  left_join(isco, by = c('ISCO08_3D' = 'ISCO')) %>%
  mutate(uemp_diff = uemp_mean_immigrant - uemp_mean_native,
         overed_diff = overed_mean_immigrant - overed_mean_native)

top_occupation_uemp <- master_df_occupation_complete %>%
  group_by(COUNTRY) %>%
  slice_max(order_by = uemp_diff, n = 3) %>%
  filter(!is.na(n_immigrant), !is.na(Occupation_Text), !is.na(uemp_diff))

ggplot(top_occupation_uemp, aes(reorder_within(Occupation_Text, uemp_diff, COUNTRY), y = uemp_diff))+
  scale_x_reordered()+
  geom_bar(stat = 'identity', position = 'dodge', width = 0.5)+
  facet_wrap(COUNTRY ~., scales = 'free', ncol = 3)+
  ggtitle('2017-2021: Difference in Under-employment between Immigrants and Natives; Top 3 Occupations by country')+
  theme(axis.text.x = element_text(angle = 10, vjust = 1, hjust=1))

ggsave(paste0(outpath, 'isco_uemp_country.png'), plot = last_plot(), width = 20, height = 30)  

top_occupation_overed <- master_df_occupation_complete %>%
  group_by(COUNTRY) %>%
  slice_max(order_by = overed_diff, n = 3) %>%
  filter(!is.na(n_immigrant), !is.na(Occupation_Text), !is.na(overed_diff))

ggplot(top_occupation_overed, aes(reorder_within(Occupation_Text, overed_diff, COUNTRY), y = overed_diff))+
  scale_x_reordered()+
  geom_bar(stat = 'identity', position = 'dodge', width = 0.5)+
  facet_wrap(COUNTRY ~., scales = 'free', ncol = 3)+
  ggtitle('2017-2021: Difference in Over-education between Immigrants and Natives; Top 3 Occupations by country')+
  theme(axis.text.x = element_text(angle = 10, vjust = 1, hjust=1))

ggsave(paste0(outpath, 'isco_overed_country.png'), plot = last_plot(), width = 20, height = 30)  

write.csv(master_df_occupation_complete, paste0(outpath, 'occupation_overed_underemp_diff.csv'))

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
  left_join(isco, by = c('ISCO08_3D' = 'ISCO'))
write.csv(master_df_occupation_hatfield, paste0(outpath, 'education_occupation_brainwaste.csv'))
