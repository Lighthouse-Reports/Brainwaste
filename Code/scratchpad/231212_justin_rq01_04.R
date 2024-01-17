#load libraries
library(dplyr)
library(tidyr)
library(readr)
library(feather)
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

input_fp <- 'Results/descriptives/'
master_df <- data.frame()
for(country in countries_to_analyze){
  cur_df <- read.csv(paste0(input_fp, country, '/is_immigrant/hatfield1d.csv')) %>%
    dplyr::select(COUNTRY, REFYEAR, is_immigrant, hatfield1d, n) %>%
    dplyr::filter(!is.na(hatfield1d), !is.na(is_immigrant))
  master_df <- dplyr::bind_rows(master_df, cur_df)
}

master_df <- master_df %>%
  group_by(COUNTRY, REFYEAR, is_immigrant) %>%
  mutate(share = n/sum(n, na.rm = T)) %>%
  dplyr::ungroup() %>%
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

ggplot(master_df, aes(x = hatfield_text, y = share, fill = as.factor(is_immigrant)))+
  geom_bar(position = 'dodge', stat = 'identity')+
  facet_grid(COUNTRY ~ REFYEAR)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

dir.create('Results/scratchpad/rq01_04/', showWarnings = F)
ggsave('Results/scratchpad/rq01_04/hatfield_country_year.png', plot = last_plot(), height = 25, width = 20)

write.xlsx(master_df, 'Results/scratchpad/rq01_04/hatfield_country_year.xlsx')

master_df_wide <- master_df %>%
  pivot_wider(id_cols = c('COUNTRY', 'REFYEAR', 'hatfield1d', 'hatfield_text'),
              names_from = c('is_immigrant'), values_from = c('share', 'n')) %>%
  dplyr::rename(share_native = share_0,
                share_immigrant = share_1,
                n_native = n_0,
                n_immigrant = n_1) %>%
  dplyr::mutate(share_diff = share_immigrant - share_native,
                share_ratio = share_immigrant/share_native)

write.xlsx(master_df_wide, 'Results/scratchpad/rq01_04/hatfield_country_year_wide.xlsx', overwrite = T)

for(yr in unique(master_df_wide$REFYEAR)){
  cur_df <- master_df_wide %>%
    filter(REFYEAR == yr)
  p <- ggplot(cur_df, aes(x = reorder_within(COUNTRY, share_diff, hatfield_text), y = share_diff, fill = (n_immigrant > 100)))+
    geom_bar(stat = 'identity', position = 'dodge')+
    scale_x_reordered()+
    facet_wrap(hatfield_text~., scales = 'free', ncol = 1) +
    labs(x = 'Country',
         y = 'Immigrant share - native share',
         label = 'Immigrant N > 100',
         title = paste0('Difference in Field of Education (immigrant share - native share) by Country: ', as.character(yr)))
  ggsave(paste0('Results/scratchpad/rq01_04/hatfield_country_share_difff_', as.character(yr), '.png'), 
         plot = last_plot(), height = 29, width = 21)
}

