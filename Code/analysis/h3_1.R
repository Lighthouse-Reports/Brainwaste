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
# 2022 EURES shortage occupation, based on: https://www.ela.europa.eu/sites/default/files/2023-03/eures-labour-shortages-report-2022.pdf
shortage22 <- read.xlsx('Input Data/Config/EURES_shortage_2022.xlsx') %>%
  mutate(ISCO08_short = str_extract(ISCO08, '^\\d{3}'), #extract first 3D of ISCO code to allow merging with country_df
         Country = case_when(Country %in% c('LV', 'LT', 'EE') ~ 'Baltics',
                             Country %in% c('PL', 'CZ', 'SK', 'HU') ~ 'Visegrad',
                             Country %in% c('RO', 'BG') ~ 'eu07',
                             Country %in% c('HR', 'SI') ~ 'yugo',
                             .default = Country)) %>%
  dplyr::select(Country, ISCO08_short)

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
  shortage_nace = numeric(),
  shortage_eures = numeric()
)


for(country in countries_to_analyze){
  print(country)
  country_df <- arrow::read_feather(paste0(input_fp, country, '.feather'))
  shortage_country <- shortage22 %>%
    filter(Country == country)
  
  country_df <- country_df %>%
    mutate(shortage_eures = case_when(ISCO08_3D %in% shortage_country$ISCO08_short ~ 1,
                                      !is.na(ISCO08_3D) ~ 0,
                                      .default = NA))
  
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
    filter(REFYEAR > 2016, is_immigrant == 0) %>%
    group_by(COUNTRY, hatfield1d) %>%
    summarise(shortage_nace = mean(shortage, na.rm = T),
              shortage_eures = 100*mean(shortage_eures, na.rm = T))
  
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
  dplyr::select(COUNTRY, hatfield_text, unemployed_diff, underemployed_diff, overed_diff, shortage_eures, shortage_nace) %>%
  pivot_longer(cols = c(ends_with('diff'), starts_with('shortage')), names_to = 'metric', values_to = 'value')

master_long %>%
  dplyr::filter(metric != 'shortage_eures') %>%
  ggplot(aes(x = hatfield_text, y = value, fill = metric))+
    geom_bar(stat = 'identity', position = 'dodge')+
    facet_grid(COUNTRY~., scales = 'free_y')+
    labs(x = 'Field of Education',
         y = '%',
         title = 'Shortages (NACE) and Brainwaste Differentials between Natives\nand Immigrants by Field of Education')

ggsave(paste0(output_fp, 'education_brainwaste_shortage_nace.png'), plot = last_plot(), width = 15, height = 30)

master_long %>%
  dplyr::filter(metric != 'shortage_nace') %>%
  ggplot(aes(x = hatfield_text, y = value, fill = metric))+
  geom_bar(stat = 'identity', position = 'dodge')+
  facet_grid(COUNTRY~., scales = 'free_y')+
  labs(x = 'Field of Education',
       y = '%',
       title = 'Shortages (EURES) and Brainwaste Differentials between Natives\nand Immigrants by Field of Education')

ggsave(paste0(output_fp, 'education_brainwaste_shortage_eures.png'), plot = last_plot(), width = 15, height = 30)



