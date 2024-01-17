#load libraries
library(dplyr)
library(tidyr)
library(readr)
library(feather)
library(openxlsx)
library(stringr)
library(ggplot2)


#set up fps
input_fp <- 'Input Data/ELF_Merged/final/merged_country_final_2006_onwards_'
output_fp <- paste0(cur_date, 'Results/descriptives/')
dir.create(output_fp, showWarnings = F)

#load variable groups
var_path <- 'Input Data/Config/vars_descriptives.xlsx'

#labor market outcome vars
dependent_vars <- read.xlsx(var_path, sheet = 'dependent_vars')
dependent_vars <- dependent_vars$dependent_vars

#broad immigrant characteritics vars
immigrant_vars <- read.xlsx(var_path, sheet = 'immigrant_vars')
immigrant_vars <- immigrant_vars$immigrant_vars

#more specific immigrant characteristic vars
immigrant_chars <- read.xlsx(var_path, sheet = 'immigrant_chars')
immigrant_chars <- immigrant_chars$immigrant_chars

#personal characteristics vars
personal_chars <- read.xlsx(var_path, sheet = 'personal_chars')
personal_chars <- personal_chars$personal_chars

#region vars
region_vars <- read.xlsx(var_path, sheet = 'region_vars')
region_vars <- region_vars$region_vars

#always variables
always_vars <- read.xlsx(var_path, sheet = 'always_vars')
always_vars <- always_vars$always_vars


#loop over countries
for(country in countries_to_analyze){
  print(country)
  
  #load country df
  country_df <- read_feather(paste0(input_fp, country, '.feather'))
  gc() #clean memory
  
  #set up country specific output fp
  output_fp_country <- paste0(output_fp, country, '/')
  dir.create(output_fp_country, showWarnings = F)
  
  #loop over immigrant vars
  for(immigrant_var in immigrant_vars){
    
    #set up immigrant var output fp
    output_fp_immigr <- paste0(output_fp_country, immigrant_var, '/')
    dir.create(output_fp_immigr, showWarnings = F)
    
    #set up variables to pivot by
    group_by_vars <- c(immigrant_var, always_vars)
    outcome_vars <- c(dependent_vars)
    
    #group by immigrant and always vars
    basic_df <- country_df %>%
      group_by_at(group_by_vars) %>%
      #calculate means and sds for each outcome var
      summarise_at(outcome_vars, .funs = list(mean = ~mean(x = ., na.rm = T),
                                              sd = ~sd(x = ., na.rm = T)))
    
    #count number of obs
    basic_df_count <- country_df %>%
      group_by_at(group_by_vars) %>%
      count()
    
    #join outcome var and count dfs
    basic_df <- full_join(basic_df, basic_df_count, by = group_by_vars)
    
    #save basic df
    write.csv(basic_df, paste0(output_fp_immigr,  'basic.csv'))
    
    #loop over more specific characteristics
    chars_to_iterate <- c(immigrant_chars, personal_chars, region_vars)
    for(char in chars_to_iterate){
      
      #set up variables to pivot by
      group_by_vars_char <- c(immigrant_var, always_vars, char)
      
      
      char_df <- country_df %>%
        group_by_at(group_by_vars_char) %>%
        #calculate means and sds for each outcome var
        summarise_at(outcome_vars, .funs = list(mean = ~mean(x = ., na.rm = T),
                                                sd = ~sd(x = ., na.rm = T)))
      #count number of observations
      char_df_count <- country_df %>%
        group_by_at(group_by_vars_char) %>%
        count()
      
      #join outcome var and count dfs
      char_df <- full_join(char_df, char_df_count, by = group_by_vars_char)
      
      #save results
      write.csv(char_df, paste0(output_fp_immigr, char, '.csv'))
    }
  }
}




