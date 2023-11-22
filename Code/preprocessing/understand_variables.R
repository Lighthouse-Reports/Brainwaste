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
  fp <- paste0('Input Data/ELF_Merged/country_merged/merged_country_2006_onwards_', country, '.feather')
  country_df <- read_feather(fp)
  
  #get list of variable names
  col_names_country_df <- colnames(country_df)
  col_names_country_df <- col_names_country_df[!col_names_country_df %in% c("REFYEAR", "COUNTRY")]
  
  #loop over variables
  for(col in col_names_country_df){
    
    #get counts by year and variable
    temp <- country_df %>%
      group_by_at(c("REFYEAR", col)) %>%
      count()
    
    #save temp
    write.csv(temp, paste0('Results/Raw_Variable_Counts/', col, '_', country, '.csv'))
    
    #remove rows with fewer than 10 observations
    temp_sec <- temp %>%
      filter(n > 10)
    
    #save temp_sec
    write.csv(temp_sec, paste0('Results/Raw_Variable_Counts_Secure/', col, '_', country, '.csv'))
  }
}
