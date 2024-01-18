#load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(data.table)
library(arrow)
library(tidyverse)
library(openxlsx)
library(stringr)
library(hash)


input_fp <- 'Input Data/ELF_Merged/external/'

pop <- read.csv('Input Data/eurostat/pop_transformed.csv')

#define country groups
country_group_dic <- hash()
country_group_dic[['Baltics']] <- c('LV', 'LT', 'EE')
country_group_dic[['Visegrad']] <- c('PL', 'CZ', 'SK', 'HU')
country_group_dic[['eu07']] <- c('RO', 'BG')
country_group_dic[['yugo']] <- c('SI', 'HR')




cur_key <- 'Baltics'

for(cur_key in keys(country_group_dic)){
  cur_countries <- country_group_dic[[cur_key]]
  
  print(cur_key)
  print(cur_countries)
  
  #define yearly shares by country within group (individual country shares should add up to 1)
  pop_cur <- pop %>%
    filter(country_short %in% cur_countries) %>%
    group_by(year) %>%
    mutate(pop_group = sum(total, na.rm = T)) %>%
    ungroup() %>%
    mutate(share_country = total/pop_group)
  
  #load all data for group
  df_list <- list()
  for(cur_country in cur_countries){
    cur_df <- arrow::read_feather(paste0(input_fp, 'merged_country_external_2006_onwards_', cur_country, '.feather'))
    df_list <- append(df_list, list(cur_df))
  }
  combined_df <- rbindlist(df_list, use.names = TRUE, fill = TRUE) 
  
  #determine yearly number of obersvations by country
  cur_country_sample_size <- combined_df %>%
    group_by(COUNTRY, REFYEAR) %>%
    summarise(sample_size = n())
  
  pop_cur <- pop_cur %>%
    left_join(cur_country_sample_size, by = c('country_short' = 'COUNTRY', 'year' = 'REFYEAR'))
  
  #calculate sample size to downsample to
  pop_cur <- pop_cur %>%
    mutate(sampling_rate = sample_size/total) %>%
    group_by(year) %>%
    mutate(min_sampling_rate = min(order_by = sampling_rate, sampling_rate)) %>%
    ungroup() %>%
    mutate(target_sample_size = round(min_sampling_rate * total, digits = 0)) %>%
    filter(!is.na(target_sample_size))
  
  #downsample to match yearly number of observations determined
  new_df_list <- list()
  for(i in 1:nrow(pop_cur)) {
    row <- pop_cur[i,]
    set.seed(12345)
    cur_df <- combined_df %>%
      dplyr::filter(REFYEAR == row$year, COUNTRY == row$country_short) %>%
      dplyr::sample_n(size = row$target_sample_size, replace = F) %>%
      dplyr::mutate(COUNTRY_old = COUNTRY, #include COUNTRY_old var
                    COUNTRY = cur_key)
    
    new_df_list <- append(new_df_list, list(cur_df))
  }
  
  #merge all data again
  combined_df_new <- rbindlist(new_df_list, use.names = TRUE, fill = TRUE) 
  
  #save data
  arrow::write_feather(combined_df_new, paste0(input_fp, 'merged_country_external_2006_onwards_', cur_key, '.feather'))
  
  #change countries to analyze vector
  countries_to_analyze <- countries_to_analyze[!countries_to_analyze %in% cur_countries]
  countries_to_analyze <- c(countries_to_analyze, cur_key)
}
