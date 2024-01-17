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

#set up fps
input_fp <- 'Input Data/ELF_Merged/operationalized/'
outut_fp <- 'Input Data/ELF_Merged/external/'
dir.create(outut_fp, showWarnings = F) #output directory

# #load 2022 EURES shortage occupation, based on: https://www.ela.europa.eu/sites/default/files/2023-03/eures-labour-shortages-report-2022.pdf
# shortage22 <- read.xlsx('Input Data/Config/EURES_shortage_2022.xlsx') %>%
#   mutate(ISCO08_short = str_extract(ISCO08, '^\\d{3}')) #extract first 3D of ISCO code to allow merging with country_df

# load NACE 1D shortage data
shortage <- read.csv('Input Data/eurostat/shortage_transformed.csv') %>%
  mutate(country_short = case_when(country_long == "Malta" ~ "MT",
                                   country_long == "Sweden" ~ "SE",
                                   country_long == "Norway" ~ "NO",
                                   country_long == "Austria" ~ "AT",
                                   country_long == "United Kingdom" ~ "UK",
                                   country_long == "Ireland" ~ "IE",
                                   country_long == "Luxembourg" ~ "LU",
                                   country_long == "Belgium" ~ "BE",
                                   country_long == "Romania" ~ "RO",
                                   country_long == "Portugal" ~ "PT",
                                   country_long == "Slovakia" ~ "SK",
                                   country_long == "Czechia" ~ "CZ",
                                   country_long == "Poland" ~ "PL",
                                   country_long == "Italy" ~ "IT",
                                   country_long == "France" ~ "FR",
                                   country_long == "Greece" ~ "EL",
                                   country_long == "Iceland" ~ "IS",
                                   country_long == "Croatia" ~ "HR",
                                   country_long == "Netherlands" ~ "NL",
                                   country_long == "Germany" ~ "DE",
                                   country_long == "Hungary" ~ "HU",
                                   country_long == "Bulgaria" ~ "BG",
                                   country_long == "Liechtenstein" ~ "LI",
                                   country_long == "Spain" ~ "ES",
                                   country_long == "Denmark" ~ "DK",
                                   country_long == "Switzerland" ~ "CH",
                                   country_long == "Slovenia" ~ "SI",
                                   country_long == "Finland" ~ "FI",
                                   country_long == "Latvia" ~ "LV",
                                   country_long == "Cyprus" ~ "CY",
                                   country_long == "Lithuania" ~ "LT",
                                   country_long == "Estonia" ~ "EE")) %>%
  filter(!is.na(country_short), !is.na(NACE2_1D))

#load regulated professions database, based on https://ec.europa.eu/growth/tools-databases/regprof/professions/profession/2100
regulated <- read.xlsx('Input Data/Config/regulated_professions.xlsx') %>%
  mutate(ISCO88 = as.numeric(ISCO88),
         #set up 2d country codes
         country_short = case_when(country == "Malta" ~ "MT",
                                   country == "Sweden" ~ "SE",
                                   country == "Norway" ~ "NO",
                                   country == "Austria" ~ "AT",
                                   country == "United Kingdom (archived data)" ~ "UK",
                                   country == "Ireland" ~ "IE",
                                   country == "Luxembourg" ~ "LU",
                                   country == "Belgium" ~ "BE",
                                   country == "Romania" ~ "RO",
                                   country == "Portugal" ~ "PT",
                                   country == "Slovakia" ~ "SK",
                                   country == "Czech Republic" ~ "CZ",
                                   country == "Poland" ~ "PL",
                                   country == "Italy" ~ "IT",
                                   country == "France" ~ "FR",
                                   country == "Greece" ~ "EL",
                                   country == "Iceland" ~ "IS",
                                   country == "Croatia" ~ "HR",
                                   country == "Netherlands" ~ "NL",
                                   country == "Germany" ~ "DE",
                                   country == "Hungary" ~ "HU",
                                   country == "Bulgaria" ~ "BG",
                                   country == "Liechtenstein" ~ "LI",
                                   country == "Spain" ~ "ES",
                                   country == "Denmark" ~ "DK",
                                   country == "Switzerland" ~ "CH",
                                   country == "Slovenia" ~ "SI",
                                   country == "Finland" ~ "FI",
                                   country == "Latvia" ~ "LV",
                                   country == "Cyprus" ~ "CY",
                                   country == "Lithuania" ~ "LT",
                                   country == "Estonia" ~ "EE")) %>%
  dplyr::filter(region == 'All Regions') %>%
  dplyr::select(-country)
#TODO: potentially exclude observations that apply on the subnational level

#NA for regulated df is 0, to prevent merging on NA professions in the ELF
regulated[is.na(regulated)] <- 0

#display number of regulated professions by country
regulated %>%
  group_by(country_short) %>%
  count() %>%
  ggplot(aes(x = reorder(country_short, -n), y = n))+
  geom_histogram(stat = 'identity')+
  ggtitle('Regulated professions by country') %>%
  print()

income_silc <- read.csv('Input Data/eurostat/income_transformed_SILC.csv') %>%
  mutate(country_short = case_when(country_long == "Malta" ~ "MT",
                                   country_long == "Sweden" ~ "SE",
                                   country_long == "Norway" ~ "NO",
                                   country_long == "Austria" ~ "AT",
                                   country_long == "United Kingdom" ~ "UK",
                                   country_long == "Ireland" ~ "IE",
                                   country_long == "Luxembourg" ~ "LU",
                                   country_long == "Belgium" ~ "BE",
                                   country_long == "Romania" ~ "RO",
                                   country_long == "Portugal" ~ "PT",
                                   country_long == "Slovakia" ~ "SK",
                                   country_long == "Czechia" ~ "CZ",
                                   country_long == "Poland" ~ "PL",
                                   country_long == "Italy" ~ "IT",
                                   country_long == "France" ~ "FR",
                                   country_long == "Greece" ~ "EL",
                                   country_long == "Iceland" ~ "IS",
                                   country_long == "Croatia" ~ "HR",
                                   country_long == "Netherlands" ~ "NL",
                                   country_long == "Germany" ~ "DE",
                                   country_long == "Hungary" ~ "HU",
                                   country_long == "Bulgaria" ~ "BG",
                                   country_long == "Liechtenstein" ~ "LI",
                                   country_long == "Spain" ~ "ES",
                                   country_long == "Denmark" ~ "DK",
                                   country_long == "Switzerland" ~ "CH",
                                   country_long == "Slovenia" ~ "SI",
                                   country_long == "Finland" ~ "FI",
                                   country_long == "Latvia" ~ "LV",
                                   country_long == "Cyprus" ~ "CY",
                                   country_long == "Lithuania" ~ "LT",
                                   country_long == "Estonia" ~ "EE")) %>%
  filter(!is.na(country_short), !is.na(decile), !is.na(income)) %>%
  rename(income_euro = income)

#loop over countries
for(country in countries_to_analyze){
  #load country level ELF
  country_df <- arrow::read_feather(paste0(input_fp, 'merged_country_op_2006_onwards_', country, '.feather'))
  
  print(country)
  
  country_df <- country_df %>%
    dplyr::left_join(shortage %>% dplyr::select(country_short, NACE2_1D, year, shortage), 
              by = c('COUNTRY' = 'country_short', 'NACE2_1D' = 'NACE2_1D', 'REFYEAR' = 'year'))
  
  # #construct shortage variable based on EURES 2022 data
  # country_df <- country_df %>%
  #   mutate(shortage_eures22 = case_when(REFYEAR == 2021 & ISCO08_3D %in% shortage22[shortage22$Country == country,]$ISCO08_short ~ 1,
  #                                       REFYEAR == 2021 & !(ISCO08_3D %in% shortage22[shortage22$Country == country,]$ISCO08_short) ~ 0,
  #                                       .default = NA))
  
  #construct shortage measure from ELF
  #based on http://www.anc.edu.ro/wp-content/uploads/2020/04/Analysis-of-shortage-and-surplus-occupations-based-on-national-and-Eurostat-force-survey-data.pdf
  #shortage is based on the ratio of people seeking jobs who previously worked in a given profession and people who have recently
  #started working in this profession
  
  #TODO: conduct sanity check and test different time frames for both 'looking' and 'found'
  
  # #count the number of people who have found work in a given profession in the last 2 years
  # shortage_found <- country_df %>%
  #   group_by(REFYEAR, YSTARTWK, ISCO88_3D, ISCO08_3D) %>%
  #   count() %>%
  #   ungroup() %>%
  #   filter(REFYEAR - YSTARTWK < 2) %>%
  #   group_by(REFYEAR, ISCO88_3D, ISCO08_3D) %>%
  #   summarise(count_found = sum(n, na.rm = T))
  
  # #count number of people who are looking for work in a given profession
  # shortage_looking <- country_df %>%
  #   filter(SEEKWORK == 1) %>%
  #   group_by(REFYEAR, ISCO88_3DPR, ISCO08_3DPR) %>%
  #   summarise(count_looking = n())
  # 
  # #calculate shortage ratio by dividing # looking over # found by profession
  # shortage_df <- full_join(shortage_found, shortage_looking, by = c('REFYEAR', 'ISCO88_3D' = 'ISCO88_3DPR', 'ISCO08_3D' = 'ISCO08_3DPR')) %>%
  #   mutate(shortage_ratio = count_looking/count_found) %>%
  #   dplyr::select(REFYEAR, ISCO88_3D, ISCO08_3D, shortage_ratio)
  
  # #save country level shortage data
  # write.csv(shortage_df, paste0(shortage_fp, 'shortage_ISCO_', country, '.csv'))
  
  # #construct shortage variable
  # country_df <- country_df %>%
  #   left_join(shortage_df, by = c('REFYEAR', 'ISCO88_3D', 'ISCO08_3D'))
  
  #construct regulated profession variable
  
  #construct df of regulated professions by country
  regulated_country <- regulated %>%
    filter(grepl(country, country_short))
  
  #construct variable which equals 1 when respondent works in regulated profession
  country_df <- country_df %>%
    mutate(regulated_profession = case_when(ISCO88_3D %in% regulated_country$ISCO88 ~ 1,
                                            ISCO08_3D %in% regulated_country$ISCO08 ~ 1,
                                            EMPSTAT == 1 ~ 0,
                                            .default = NA))
  
  
  #generate 1d hatfield (field of education) variable
  country_df$hatfield1d <- trunc(country_df$HATFIELD/10)
  
  #generate propensity to work in a regulated profession variable based on 
  #the share of people with a given level of education and field of education
  #who work in a regulated profession
  propensity_regulated <- country_df %>%
    filter(EMPSTAT == 1, !is.na(hatfield1d)) %>%
    group_by(hat_isced, hatfield1d) %>%
    summarise(propensity_regulated = mean(regulated_profession, na.rm = T),
              count = n()) %>%
    ungroup() %>%
    filter(count > 10) %>%
    dplyr::select(-count)
  
  #join country df with propensity_regulated variable
  country_df <- country_df %>%
    left_join(propensity_regulated, by = c('hatfield1d', 'hat_isced'))
  
  #generate propensity to work in shortage occupation using the same principle as for propensity_regulated
  propensity_shortage <- country_df %>%
    filter(EMPSTAT == 1, !is.na(hatfield1d)) %>%
    group_by(hat_isced, hatfield1d) %>%
    summarise(propensity_shortage = mean(shortage, na.rm = T),
              count = n()) %>%
    ungroup() %>%
    filter(count > 10) %>%
    dplyr::select(-count)
  country_df <- country_df %>%
    left_join(propensity_shortage, by = c('hatfield1d', 'hat_isced'))
  
  #merge with SILC income data
  country_df <- country_df %>%
    dplyr::left_join(income_silc %>% dplyr::select(country_short, year, decile, income_euro),
                     by = c('COUNTRY' = 'country_short', 'REFYEAR' = 'year', 'INCDECIL' = 'decile'))
  
  #save country level data
  write_feather(country_df, paste0(outut_fp, 'merged_country_external_2006_onwards_', country, '.feather'))
  #clean memory
  gc()
}

