#load libraries
library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(feather)
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
  country_df <- read_feather(paste0(input_fp, country, '.feather'))
  
  #print dimensions of country df
  print(dim(country_df))
  
  #exclude observations
  country_df <- country_df %>%
    filter(age_years >= 18, age_years <= 67) #only keep respondents older than 18 and younger than 67
  
  #print dimensions of country df after observations were excluded
  print(dim(country_df))
  
  #save country data
  write_feather(country_df, paste0(output_fp, 'merged_country_final_2006_onwards_', country, '.feather'))
  
  #clean memory
  gc()
    
}