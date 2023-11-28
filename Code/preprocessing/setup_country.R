#load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(data.table)
library(feather)
library(tidyverse)
library(openxlsx)

#define input fp
input_data_path = 'Input Data/231106_ELF_Raw/'

#get fps for all raw data files
old_files <- list.files(path=paste0(input_data_path, "YEAR_1983_2005/"), pattern = 'csv', recursive = TRUE) 
old_files <-paste0(input_data_path, "YEAR_1983_2005/", old_files)
new_files <- list.files(path=paste0(input_data_path, "YEAR_2006_2021/"), pattern = 'csv', recursive = TRUE)
new_files <-paste0(input_data_path, "YEAR_2006_2021/", new_files)
all_files <- union(old_files, new_files)

#define output fp
country_fp <- 'Input Data/ELF_Merged/country_merged'
dir.create(path = 'Input Data/ELF_Merged/country_merged', showWarnings = F)

#load config raw variables
raw_vars <- read.xlsx('Input Data/config/Raw_vars.xlsx')
raw_vars <- raw_vars$Name

#load config variables to keep
vars_to_keep <- read.xlsx('Input Data/config/Vars_to_keep.xlsx')
vars_to_keep <- vars_to_keep$Name

#loop over countries
for(country in countries_to_analyze){
  print(country)
  
  #get all file names for the current country
  new_files_to_keep <- new_files[grepl(pattern = paste0(country, '[0-9]{4}'), new_files)]
  
  #read files for the current country
  df_list <- list()
  for(f in new_files_to_keep){
    temp_df <- read.csv(f) %>%
      dplyr::select(any_of(raw_vars))
    df_list <- append(df_list, list(temp_df))
  }
  #merge country dataframes
  merged_df_country <- rbindlist(df_list, use.names = TRUE, fill = TRUE) 
  
  #print out vars missing from the country
  col_names_country_df <- colnames(merged_df_country)
  print('Raw variables missing')
  cols_missing <- raw_vars[!raw_vars %in% col_names_country_df]
  print(cols_missing)
  
  #set up missing variables with NA values
  for(col in cols_missing){
    merged_df_country[[col]] <- NA
  }
  
  #standardize variable types and merge variables that have different names in different years
  merged_df_country <- merged_df_country %>%
    mutate(AGECITI = AHM2008_AGECITI,
           COBFATH = case_when(!is.na(COBFATH) ~ as.character(COBFATH),
                               !is.na(AHM2008_COBFATH) ~ as.character(AHM2008_COBFATH),
                               !is.na(AHM2009_COBFATH) ~ as.character(AHM2009_COBFATH),
                               !is.na(AHM2014_COBFATH) ~ as.character(AHM2014_COBFATH),
                               .default = NA),
           COBMOTH = case_when(!is.na(COBMOTH) ~ as.character(COBMOTH),
                               !is.na(AHM2008_COBMOTH) ~ as.character(AHM2008_COBMOTH),
                               !is.na(AHM2009_COBMOTH) ~ as.character(AHM2009_COBMOTH),
                               !is.na(AHM2014_COBMOTH) ~ as.character(AHM2014_COBMOTH),
                               .default = NA),
           MIGREAS = case_when(!is.na(MIGREAS) ~ MIGREAS,
                               !is.na(AHM2008_MIGREAS) ~ AHM2008_MIGREAS,
                               !is.na(AHM2014_MIGREAS) ~ AHM2014_MIGREAS,
                               .default = NA),
           ESTQUAL = case_when(!is.na(AHM2008_ESTQUALI) ~ AHM2008_ESTQUALI,
                               !is.na(AHM2021_ESTQUAL) ~ AHM2021_ESTQUAL,
                               .default = NA),
           FINDMETH = case_when(!is.na(FINDMETH) ~ FINDMETH,
                                !is.na(AHM2009_FINDMETH) ~ AHM2009_FINDMETH,
                                !is.na(AHM2014_FINDMETH) ~ AHM2014_FINDMETH,
                                !is.na(AHM2016_FINDMETH) ~ AHM2016_FINDMETH,
                                !is.na(AHM2016_FINDMETH) ~ AHM2016_FINDMETH,
                                .default = NA),
           HATCNTR = AHM2021_HATCNTR,
           HATPAR = AHM2021_HATPAR,
           JOBSATISF = AHM2021_JOBSATISF,
           SKILLEQ = AHM2021_SKILLEQ,
           DISCRIMI = AHM2021_DISCRIMI,
           JOBOBSTA = AHM2021_JOBOBSTA,
           DURFIJOB = AHM2021_DURFIJOB,
           PRKNLANG = AHM2021_PRKNLANG,
           LANGHOST = case_when(!is.na(AHM2014_LANGHOST) ~ AHM2014_LANGHOST,
                                !is.na(AHM2021_LANGHOST) ~ AHM2021_LANGHOST,
                                .default = NA),
           LANGCOUR = case_when(!is.na(AHM2014_LANGCOUR) ~ AHM2014_LANGCOUR,
                                !is.na(AHM2021_LANGCOUR) ~ AHM2021_LANGCOUR,
                                .default = NA)
    ) %>%
    dplyr::select(all_of(vars_to_keep)) #keep variables defined in vars_to_keep
  
  #variables to keep that are missing
  print('Vars to keep missing')
  print(vars_to_keep[!vars_to_keep %in% colnames(merged_df_country)])
  
  #save country level data frame
  write_feather(merged_df_country, paste0(country_fp, '/merged_country_2006_onwards_', country, '.feather'))
  remove(merged_df_country)
  remove(df_list)
  remove(temp_df)
  gc()
}

