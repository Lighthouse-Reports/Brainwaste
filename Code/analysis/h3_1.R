#load libraries
library(dplyr)
library(tidyr)
library(readr)
library(feather)
library(openxlsx)
library(stringr)
library(ggplot2)
library(arrow)
library(broom)
library(speedglm)
library(forcats)

#set up fps
input_fp <- 'Input Data/ELF_Merged/final/merged_country_final_2006_onwards_'
output_fp <- paste0(cur_date, 'Results/h3_1/')
dir.create(output_fp, showWarnings = F)

#load external data


master_df <- data.frame(
  COUNTRY = character(),
  hatfield1d = numeric(),
  unemployed_immigrant = numeric(),
  underemployed_immigrant = numeric(), 
  overed_immigrant = numeric(),
  count_immigrant = numeric(),
  unemployed_native = numeric(),
  underemployed_native = numeric(),
  overed_native = numeric(),
  count_native = numeric(),
  unemployed_diff = numeric(),
  underemployed_diff = numeric(),
  overed_diff=numeric(),
  shortage = numeric()
)


for(country in countries_to_analyze){
  country_df <- arrow::read_feather(paste0(input_fp, country, '.feather'))
  immigrant_brainwaste <- country_df %>%
    filter(REFYEAR > 2016, is_immigrant == 1) %>%
    group_by(COUNTRY, hatfield1d) %>%
    summarise(unemployed_immigrant = 100*mean(is_unemployed, na.rm = T),
              underemployed_immigrant = 100*mean(uemp, na.rm = T),
              overed_immigrant = 100*mean(overed_1sd_hat_isced, na.rm = T),
              count_immigrant = n()) %>%
    filter(!is.na(hatfield1d))
  native_brainwaste <- country_df %>%
    filter(REFYEAR > 2016, is_immigrant == 0) %>%
    group_by(COUNTRY, hatfield1d) %>%
    summarise(unemployed_native = 100*mean(is_unemployed, na.rm = T),
              underemployed_native = 100*mean(uemp, na.rm = T),
              overed_native = 100*mean(overed_1sd_hat_isced, na.rm = T),
              count_native = n()) %>%
    filter(!is.na(hatfield1d)) 
  
  brainwaste_combined <- left_join(immigrant_brainwaste, native_brainwaste, by = c('COUNTRY', 'hatfield1d')) %>%
    mutate(unemployed_diff = unemployed_immigrant - unemployed_native,
           underemployed_diff = underemployed_immigrant - underemployed_native,
           overed_diff = overed_immigrant - overed_native)
  
  shortage_educ <- country_df %>%
    group_by(COUNTRY, hatfield1d) %>%
    summarise(shortage = mean(shortage, na.rm = T))
  
  cur_df <- left_join(brainwaste_combined, shortage_educ, by = c('COUNTRY', 'hatfield1d'))
  
  master_df <- master_df %>%
    bind_rows(cur_df)
}

master_df <- master_df %>%
  filter(!is.na(hatfield1d), count_immigrant > 20) %>%
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
write.csv(master_df, paste0(output_fp, 'education_brainwaste_shortage.csv'))


### visualize ###
master_long <- master_df %>%
  dplyr::select(COUNTRY, hatfield_text, unemployed_diff, underemployed_diff, overed_diff, shortage) %>%
  pivot_longer(cols = c(ends_with('diff'), 'shortage'), names_to = 'metric', values_to = 'value')

master_long %>%
  mutate(metric = factor(metric, levels = c('shortage', 'unemployed_diff', 'underemployed_diff', 'overed_diff'))) %>%
  ggplot(aes(x = hatfield_text, y = value, fill = metric))+
    geom_bar(stat = 'identity', position = 'dodge')+
    facet_grid(COUNTRY~.)+
    labs(x = 'Field of Education',
         y = '%',
         title = 'Shortages and Brainwaste Differentials between Natives\nand Immigrants by Field of Education')

ggsave(paste0(output_fp, 'education_brainwaste_shortage.png'), plot = last_plot(), width = 15, height = 30)



