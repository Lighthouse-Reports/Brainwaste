#load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(data.table)
library(feather)
library(tidyverse)
library(arrow)

setwd("/Users/justin-casimirbraun/Brainwaste")

#set up output files
dir.create(paste0(cur_date, 'Results/scratchpad/sample_size/'), showWarnings = F)

#loop over countries
for(country in countries_to_analyze){
  print(country)
  #define input fp
  fp <- paste0('Input Data/ELF_Merged/final/merged_country_final_2006_onwards_', country, '.feather')
  country_df <- arrow::read_feather(fp)
  
  temp_df <- country_df %>%
    dplyr::group_by(REFYEAR) %>%
    dplyr::count()
  
  write.csv(temp_df, paste0(cur_date, 'Results/scratchpad/sample_size/sample_size_year_', country, '.csv'))
}
