#load libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(data.table)
library(feather)
library(tidyverse)
library(openxlsx)
library(stringr)

#set up fps
input_fp <- 'Input Data/ELF_Merged/operationalized/'
outut_fp <- 'Input Data/ELF_Merged/external/'
shortage_fp <- 'Results/Shortage_ratios/'
dir.create(outut_fp, showWarnings = F) #output directory
dir.create(shortage_fp, showWarnings = F) #shortage directory

#load 2022 EURES shortage occupation, based on: https://www.ela.europa.eu/sites/default/files/2023-03/eures-labour-shortages-report-2022.pdf
shortage22 <- read.xlsx('Input Data/Config/EURES_shortage_2022.xlsx') %>%
  mutate(ISCO08_short = str_extract(ISCO08, '^\\d{3}')) #extract first 3D of ISCO code to allow merging with country_df

#count number of shortage occupations by country
shortage22 %>%
  group_by(Country) %>%
  count() %>%
  arrange(desc(n)) %>%
  ggplot(aes(x = reorder(Country, -n), y = n)) +
  geom_histogram(stat = 'identity')+
  ggtitle('Shortage occupations by country') %>%
  print()

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
  select(-country)
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

#loop over countries
for(country in countries_to_analyze){
  #load country level ELF
  country_df <- read_feather(paste0(input_fp, 'merged_country_op_2006_onwards_', country, '.feather'))
  
  print(country)
  
  #construct shortage variable based on EURES 2022 data
  country_df <- country_df %>%
    mutate(shortage_eures22 = case_when(REFYEAR == 2021 & ISCO08_3D %in% shortage22[shortage22$Country == country,]$ISCO08_short ~ 1,
                                        REFYEAR == 2021 & !(ISCO08_3D %in% shortage22[shortage22$Country == country,]$ISCO08_short) ~ 0,
                                        .default = NA))
  
  #construct shortage measure from ELF
  #based on http://www.anc.edu.ro/wp-content/uploads/2020/04/Analysis-of-shortage-and-surplus-occupations-based-on-national-and-Eurostat-force-survey-data.pdf
  #shortage is based on the ratio of people seeking jobs who previously worked in a given profession and people who have recently
  #started working in this profession
  
  #TODO: conduct sanity check and test different time frames for both 'looking' and 'found'
  
  #count the number of people who have found work in a given profession in the last 2 years
  shortage_found <- country_df %>%
    group_by(REFYEAR, YSTARTWK, ISCO88_3D, ISCO08_3D) %>%
    count() %>%
    ungroup() %>%
    filter(REFYEAR - YSTARTWK < 2) %>%
    group_by(REFYEAR, ISCO88_3D, ISCO08_3D) %>%
    summarise(count_found = sum(n, na.rm = T))
  
  #count number of people who are looking for work in a given profession
  shortage_looking <- country_df %>%
    filter(SEEKWORK == 1) %>%
    group_by(REFYEAR, ISCO88_3DPR, ISCO08_3DPR) %>%
    summarise(count_looking = n())
  
  #calculate shortage ratio by dividing # looking over # found by profession
  shortage_df <- full_join(shortage_found, shortage_looking, by = c('REFYEAR', 'ISCO88_3D' = 'ISCO88_3DPR', 'ISCO08_3D' = 'ISCO08_3DPR')) %>%
    mutate(shortage_ratio = count_looking/count_found) %>%
    select(REFYEAR, ISCO88_3D, ISCO08_3D, shortage_ratio)
  
  #save country level shortage data
  write.csv(shortage_df, paste0(shortage_fp, 'shortage_ISCO_', country, '.csv'))
  
  #construct shortage variable
  country_df <- country_df %>%
    left_join(shortage_df, by = c('REFYEAR', 'ISCO88_3D', 'ISCO08_3D'))
  
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
    select(-count)
  
  #join country df with propensity_regulated variable
  country_df <- country_df %>%
    left_join(propensity_regulated, by = c('hatfield1d', 'hat_isced'))
  
  #TODO: merge with ISCO spreadsheet
  #TODO match income deciles to real euro values
  
  #save country level data
  write_feather(country_df, paste0(outut_fp, 'merged_country_external_2006_onwards_', country, '.feather'))
  #clean memory
  gc()
}

