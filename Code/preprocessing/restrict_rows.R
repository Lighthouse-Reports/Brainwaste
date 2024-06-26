#load libraries
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(arrow)
library(tidyverse)
library(openxlsx)
library(stringr)

#set up input and output fps
input_fp <- 'Input Data/ELF_Merged/external/merged_country_external_2006_onwards_'
output_fp <- 'Input Data/ELF_Merged/final/'
dir.create(output_fp, showWarnings = F)

#loop over countries
for(country in countries_to_analyze){
  print(country)
  #load country level ELF
  country_df <- arrow::read_feather(paste0(input_fp, country, '.feather'))
  
  #print dimensions of country df
  print(dim(country_df))
  
  #exclude observations
  country_df_all <- country_df %>%
    filter(age_years >= 18, age_years <= 67) #only keep respondents older than 18 and younger than 67

  country_df_college <- country_df_all %>%
    filter(is_college_educated == 1) 
  
  #print dimensions of country df after observations were excluded
  print(dim(country_df))
  
  #save country data
  write_feather(country_df_college, paste0(output_fp, 'merged_country_final_2006_onwards_', country, '.feather'))
  write_feather(country_df_all, paste0(output_fp, 'merged_country_final_2006_onwards_', country, '_ALL.feather'))
  
  #clean memory
  gc()
    
}