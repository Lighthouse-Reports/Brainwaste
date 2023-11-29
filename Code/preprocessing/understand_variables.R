#load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(data.table)
library(feather)
library(tidyverse)

#set up output files
dir.create('Results/Raw_Variable_Counts', showWarnings = F)
dir.create('Results/Raw_Variable_Counts_Secure', showWarnings = F)

#loop over countries
for(country in countries_to_analyze){
  print(country)
  #define input fp
  fp <- paste0('Input Data/ELF_Merged/final/merged_country_final_2006_onwards_', country, '.feather')
  country_df <- read_feather(fp)
  
  #get list of variable names
  col_names_country_df <- colnames(country_df)
  col_names_country_df <- col_names_country_df[!col_names_country_df %in% c("REFYEAR", "COUNTRY")]
  
  #loop over variables
  for(col in col_names_country_df){
    if(length(unique(country_df[[col]])) > 15 & is.numeric(country_df[[col]])){
      country_df[[paste0(col, '_buckets')]] <- cut(country_df[[col]], breaks = 10)
      col_update <- paste0(col, '_buckets')
    } else {
      col_update <- col
    }
    #get counts by year and variable
    temp <- country_df %>%
      group_by_at(c("REFYEAR", 'COUNTRY', col_update)) %>%
      count() %>%
      group_by(REFYEAR) %>%
      mutate(share = n/sum(n, na.rm = T),
             var = col)
    temp[['var_value']] <- temp[[col_update]]
    temp <- temp %>%
      dplyr::select(-any_of(col_update))
    
    #save temp
    write.csv(temp, paste0('Results/Raw_Variable_Counts/', col, '_', country, '.csv'))
    
    #remove rows with fewer than 10 observations
    temp_sec <- temp %>%
      filter(n > 10)
    
    #save temp_sec
    write.csv(temp_sec, paste0('Results/Raw_Variable_Counts_Secure/', col, '_', country, '.csv'))
  }
}
