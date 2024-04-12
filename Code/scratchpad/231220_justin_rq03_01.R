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
master_df_occupation_hatfield_natives <- data.frame()
for(country in countries_to_analyze){
  fp <- paste0(fp_1, country, fp_2)
  cur_df <- arrow::read_feather(fp)
  cur_df_hatfield <- cur_df %>%
    filter(REFYEAR > 2016, !is.na(hatfield1d)) %>%
    group_by(COUNTRY, is_immigrant, hatfield1d) %>%
    summarise(uemp_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              unemployed_mean = mean(is_unemployed, na.rm = T),
              temp_mean = mean(is_temp, na.rm = T),
              n = n())

  master_df_hatfield <- master_df_hatfield %>%
    bind_rows(cur_df_hatfield)

  cur_df_occupation <- cur_df %>%
    filter(REFYEAR > 2016, !is.na(ISCO08_3D)) %>%
    group_by(COUNTRY, is_immigrant, ISCO08_3D) %>%
    summarise(uemp_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              temp_mean = mean(is_temp, na.rm = T),
              n = n())
  
  master_df_occupation <- master_df_occupation %>%
    bind_rows(cur_df_occupation)
  
  cur_df_hatfield_occupation <- cur_df %>%
    filter(REFYEAR > 2016, is_immigrant == 1, !is.na(hatfield1d), !is.na(ISCO08_3D)) %>%
    group_by(COUNTRY, hatfield1d, ISCO08_3D) %>%
    summarise(share_uemp = mean(uemp, na.rm = T),
              share_overed = mean(overed_1sd_hat_isced, na.rm = T),
              share_temp = mean(is_temp, na.rm = T),
              count = n()) 
  
  master_df_occupation_hatfield <- master_df_occupation_hatfield %>%
    bind_rows(cur_df_hatfield_occupation)
  
  cur_df_hatfield_occupation_natives <- cur_df %>% #CHECK occupation education df for natives
    filter(REFYEAR > 2016, is_immigrant == 0, !is.na(hatfield1d), !is.na(ISCO08_3D)) %>%
    group_by(COUNTRY, hatfield1d, ISCO08_3D) %>%
    summarise(share_uemp = mean(uemp, na.rm = T),
              share_overed = mean(overed_1sd_hat_isced, na.rm = T),
              share_temp = mean(is_temp, na.rm = T),
              count = n()) %>%
    filter(count > 50) #included the filter here right away
  
  master_df_occupation_hatfield_natives <- master_df_occupation_hatfield_natives %>%
    bind_rows(cur_df_hatfield_occupation_natives)
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
         unemployed_mean_native = unemployed_mean,
         temp_mean_native = temp_mean,
         n_native = n) %>%
  group_by(COUNTRY) %>%
  mutate(share_native = n_native/sum(n_native, na.rm = T)) %>%
  ungroup()

master_df_hatfield_immigrant <- master_df_hatfield %>%
  filter(is_immigrant == 1) %>%
  rename(uemp_mean_immigrant = uemp_mean,
         overed_mean_immigrant = overed_mean,
         unemployed_mean_immigrant = unemployed_mean,
         temp_mean_immigrant = temp_mean,
         n_immigrant = n)%>%
  group_by(COUNTRY) %>%
  mutate(share_immigrant = n_immigrant/sum(n_immigrant, na.rm = T)) %>%
  ungroup()


master_df_hatfield_complete <- left_join(master_df_hatfield_native, dplyr::select(master_df_hatfield_immigrant, -hatfield_text), by = c('COUNTRY', 'hatfield1d')) %>%
  dplyr::select(-is_immigrant.x, -is_immigrant.y) %>%
  mutate(uemp_diff = uemp_mean_immigrant - uemp_mean_native,
         overed_diff = overed_mean_immigrant - overed_mean_native,
         unemployed_diff = unemployed_mean_immigrant - unemployed_mean_native,
         temp_diff = temp_mean_immigrant - temp_mean_native)

ggplot(master_df_hatfield_complete, aes(x = reorder_within(COUNTRY, uemp_diff, hatfield_text), y = uemp_diff))+
  scale_x_reordered()+
  geom_bar(stat = 'identity', position = 'dodge', width = 0.5)+
  facet_wrap(hatfield_text ~., ncol = 1, scales = 'free_x')+
  ggtitle('2017-2021: Difference in Underemployment between Immigrants and Natives')

outpath <- paste0(cur_date, 'Results/scratchpad/rq03_01/')
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
  filter(is_immigrant == 0) %>%
  rename(uemp_mean_native = uemp_mean,
         overed_mean_native = overed_mean,
         temp_mean_native = temp_mean,
         n_native = n) %>%
  dplyr::select(-is_immigrant) %>%
  group_by(COUNTRY) %>%
  mutate(share_native = n_native/sum(n_native, na.rm = T)) %>%
  ungroup()

master_df_occupation_immigrant <- master_df_occupation %>%
  filter(is_immigrant == 1) %>%
  rename(uemp_mean_immigrant = uemp_mean,
         overed_mean_immigrant = overed_mean,
         temp_mean_immigrant = temp_mean,
         n_immigrant = n) %>%
  dplyr::select(-is_immigrant) %>%
  group_by(COUNTRY) %>%
  mutate(share_immigrant = n_immigrant/sum(n_immigrant, na.rm = T)) %>%
  ungroup()

isco <- read.xlsx('Input Data/scratchpad/rq_03_01/struct08.xlsx') %>%
  mutate(ISCO = as.numeric(ISCO))
  
master_df_occupation_complete <- left_join(master_df_occupation_native, master_df_occupation_immigrant, by = c('COUNTRY', 'ISCO08_3D')) %>%
  left_join(isco, by = c('ISCO08_3D' = 'ISCO')) %>%
  mutate(uemp_diff = uemp_mean_immigrant - uemp_mean_native,
         overed_diff = overed_mean_immigrant - overed_mean_native,
         temp_diff = temp_mean_immigrant - temp_mean_native)

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
  left_join(isco, by = c('ISCO08_3D' = 'ISCO')) %>%
  group_by(COUNTRY) %>%
  mutate(share = count/sum(count, na.rm = T)) %>%
  ungroup()
write.csv(master_df_occupation_hatfield, paste0(outpath, 'education_occupation_brainwaste.csv'))


# CHECK: do the same processing on the native df as on the immigrant df just above

master_df_occupation_hatfield_natives <- master_df_occupation_hatfield_natives %>%
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
  group_by(COUNTRY) %>%
  mutate(share = count/sum(count, na.rm = T)) %>%
  ungroup()
write.csv(master_df_occupation_hatfield_natives, paste0(outpath, 'education_occupation_brainwaste_natives.csv'))
