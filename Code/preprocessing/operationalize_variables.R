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
library(arrow)

#set up in and out fps
input_fp <- 'Input Data/ELF_Merged/country_merged/merged_country_2006_onwards_'
result_fp <- 'Input Data/ELF_Merged/operationalized/'
dir.create(result_fp, showWarnings = F)

#load na values config file
na_vars <- read.xlsx('Input Data/Config/na_by_var.xlsx')

#loop over countries
for(country in countries_to_analyze){
  print(country)
  
  #load country df
  fp <- paste0(input_fp, country, '.feather')
  country_df <- arrow::read_feather(fp)
  
  #make all empty strings NA
  country_df[country_df == ""] <- NA
  
  colnames_country <- colnames(country_df)
  
  #set up cols of NA dataframe to match cols of country df
  for (col in colnames_country){
    if (! col %in% colnames(na_vars)){
      na_vars[[col]] <- NA
    }
    
    #apply NA values
    country_df[[col]] <- ifelse(country_df[[col]] %in% na_vars[[col]], NA, country_df[[col]])
  }
  
  #recode vars
  country_df <- country_df %>%
    mutate(COUNTRYW = ifelse(grepl("[0-9]{1,3}", COUNTRYW), NA, COUNTRYW),
           AGECITI = ifelse(grepl("[0-9]{4}", AGECITI), NA, AGECITI),
           HATPAR = ifelse(grepl("9", HATPAR), NA, HATPAR),
           ESTQUAL = case_when(REFYEAR == 2008 & ESTQUAL == 9 ~ NA,
                               REFYEAR == 2021 & ESTQUAL == 99 ~ NA,
                               .default = ESTQUAL))
  country_df <- country_df %>%
    mutate(YEARESID = ifelse(YEARESID %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), paste0('Y', YEARESID), YEARESID))
  
  #construct new variables
  country_df <- country_df %>%
    mutate(receives_unemployment = ifelse(REGISTER %in% c(1, 3), 1, 0),
           registered_at_unemp_agency = ifelse(REGISTER %in% c(1, 2), 1, 0),
           is_unemployed = ifelse(ILOSTAT == 2, 1, 0),
           is_partnered = ifelse(HHPARTNR == 1, 1, 0),
           multiple_jobs = ifelse(NUMJOB > 1, 1, 0),
           recognition = as.character(ESTQUAL),
           recognition = case_when(recognition == '1' ~ 'applied_completed',
                                   recognition == '2' & REFYEAR == 2008 ~ 'applied_ongoing_rejected',
                                   recognition %in% c('2', '3') & REFYEAR == 2021 ~ 'applied_ongoing_rejected',
                                   recognition == '3' & REFYEAR == 2008 ~ 'not_applied_host_educ',
                                   recognition == '4'  ~ 'not_applied_not_needed',
                                   recognition == '5' & REFYEAR == 2008 ~ 'not_applied_other',
                                   recognition == '5' & REFYEAR == 2021 ~ 'not_applied_not_aware',
                                   recognition == '6' & REFYEAR == 2021 ~ 'not_applied_costs',
                                   recognition %in% c('7', '8', '9') & REFYEAR == 2021 ~ 'not_applied_other',
                                   .default = recognition),
           received_recognition = case_when(recognition == 'applied_completed' ~ 1,
                                            !is.na(recognition) ~ 0,
                                            .default = NA),
           is_pt = case_when(FTPT == 2 ~ 1,
                             !is.na(FTPT) ~0,
                             .default = NA),
           is_temp = case_when(TEMP == 2 ~ 1,
                               !is.na(TEMP) ~ 0,
                               .default = NA),
           is_homework = case_when(HOMEWORK %in% c(1, 2) ~ 1,
                                !is.na(HOMEWORK) ~ 0,
                                .default = NA),
           is_shift = case_when(SHIFTWK == 1 ~ 1,
                             !is.na(SHIFTWK) ~ 0,
                             .default = NA),
           is_night = case_when(NIGHTWK %in% c(1, 2) ~ 1, 
                             !is.na(NIGHTWK) ~ 0,
                             .default = NA),
           is_evening = case_when(EVENWK %in% c(1, 2) ~ 1,
                                  !is.na(EVENWK) ~ 0,
                                  .default = NA)
           
    )
  
  # construct underemployment variables
  country_df <- country_df %>%
    group_by(REFYEAR, ISCO88_3D, ISCO08_3D) %>%
    mutate(hatage = AGE - REFYEAR - HATYEAR,
           hatage = ifelse(hatage > 30, NA, hatage),
           hat_isced = trunc(HATLEVEL / 100, digits = 0),
           is_college_educated = case_when(hat_isced %in% c(6, 7, 8) ~ 1,
                                           !is.na(hat_isced) ~ 0,
                                           .default = NA)) %>%
    mutate(overed_zscore_hat_isced = (hat_isced - mean(hat_isced, na.rm = T))/sd(hat_isced, na.rm = T),
           overed_zscore_hatage = (hatage - mean(hatage, na.rm = T))/sd(hatage, na.rm = T)) %>%
    ungroup() %>%
    mutate(overed_zscore_hat_isced = ifelse(is.na(ISCO88_3D) & is.na(ISCO08_3D), NA, overed_zscore_hat_isced),
           overed_zscore_hatage = ifelse(is.na(ISCO88_3D) & is.na(ISCO08_3D), NA, overed_zscore_hatage)) %>%
    mutate(overed_1sd_hat_isced = ifelse(overed_zscore_hat_isced > 1, 1, 0),
           overed_1sd_hatage = ifelse(overed_zscore_hatage > 1, 1, 0))
  # print('Correlation between ISCED and HATAGE overeducation measures:')
  # print(cor(country_df$overed_zscore_hat_isced, country_df$overed_zscore_hatage, use = 'complete.obs'))
  country_df$uemp <- ifelse(country_df$ILOSTAT == 1 & country_df$FTPT == 2 & country_df$WISHMORE == 2 & country_df$AVAILBLE == 1, 1, 0)
  
  #construct migration variables
  immigr_vec <- c('AFR_N', 'AFR_OTH', 'AME_S', 'ASI_E', 'ASI_NME', 'ASI_SSE', 'AME_C_CRB',
                  'AME_N', 'EFTA', 'EU15', 'EU27_2020', 'EUR_NEU27_2020_NEFTA', 'EUR_NEU28_NEFTA', 'NMS10', 'NMS3', 'OCE',
                  'UNKNOWN COUNTRY ABROAD')
  country_df$is_immigrant <- case_when(country_df$COUNTRYB == 'NAT' ~ 0,
                                       !is.na(country_df$COUNTRYB) ~ 1,
                                       .default = NA)
  country_df$is_second_gen <- case_when(country_df$COBFATH %in% immigr_vec | country_df$COBMOTH %in% immigr_vec ~ 1,
                                        country_df$COBFATH == 'NAT' | country_df$COBMOTH == 'NAT' ~ 0,
                                        .default = NA)
  country_df$is_global_south <- case_when(country_df$COUNTRYB %in% c('AFR_N', 'AFR_OTH', 'AME_S', 'ASI_E', 'ASI_NME', 'ASI_SSE', 'AME_C_CRB') ~ 'MIGR_GS',
                                          country_df$COUNTRYB %in% c('AME_N', 'EFTA', 'EU15', 'EU27_2020', 'EUR_NEU27_2020_NEFTA', 'EUR_NEU28_NEFTA', 'NMS10', 'NMS3', 'OCE') ~ 'MIGR_GN',
                                          country_df$COUNTRYB %in% c('NAT') ~ 'NAT',
                                          .default = NA)
  country_df <- country_df %>%
    mutate(years_in_country = case_when(YEARESID == 'Y0' ~ 0,
                                        YEARESID == 'Y1' ~ 1,
                                        YEARESID == 'Y2' ~ 2,
                                        YEARESID == 'Y3' ~ 3,
                                        YEARESID == 'Y4' ~ 4,
                                        YEARESID == 'Y5' ~ 5,
                                        YEARESID == 'Y6' ~ 6,
                                        YEARESID == 'Y7' ~ 7,
                                        YEARESID == 'Y8' ~ 8,
                                        YEARESID == 'Y9' ~ 9,
                                        YEARESID == 'Y10-14' ~ 12.5,
                                        YEARESID == 'Y15-19' ~ 17.5,
                                        YEARESID == 'Y20-24' ~ 22.5,
                                        YEARESID == 'Y25-29' ~ 27.5,
                                        YEARESID == 'Y30-34' ~ 32.5,
                                        YEARESID == 'Y35-39' ~ 37.5,
                                        YEARESID == 'Y40-44' ~ 42.5,
                                        YEARESID == 'Y45-49' ~ 47.5,
                                        YEARESID == 'Y50-54' ~ 52.5,
                                        YEARESID == 'Y55-59' ~ 57.5,
                                        YEARESID == 'Y60-64' ~ 62.5,
                                        YEARESID == 'Y65-69' ~ 67.5,
                                        YEARESID == 'Y70-74' ~ 72.5,
                                        .default = NA),
           yearesid_10_years = case_when(YEARESID %in% c('Y0', 'Y1', 'Y2', 'Y3', 'Y4', 'Y5', 'Y6', 'Y7', 'Y8', 'Y9') ~ 'Less than 10y',
                                         !is.na(YEARESID) ~ 'More than 10y',
                                         .default = NA),
           age_years = case_when(!is.na(AGE) ~ AGE,
                                 AGE_GRP == 'Y0-4' ~ 2.5,
                                 AGE_GRP == 'Y5-9' ~ 7.5,
                                 AGE_GRP == 'Y10-14' ~ 12.5,
                                 AGE_GRP == 'Y15-19' ~ 17.5,
                                 AGE_GRP == 'Y20-24' ~ 22.5,
                                 AGE_GRP == 'Y25-29' ~ 27.5,
                                 AGE_GRP == 'Y30-34' ~ 32.5,
                                 AGE_GRP == 'Y35-39' ~ 37.5,
                                 AGE_GRP == 'Y40-44' ~ 42.5,
                                 AGE_GRP == 'Y45-49' ~ 47.5,
                                 AGE_GRP == 'Y50-54' ~ 52.5,
                                 AGE_GRP == 'Y55-59' ~ 57.5,
                                 AGE_GRP == 'Y60-64' ~ 62.5,
                                 AGE_GRP == 'Y65-69' ~ 67.5,
                                 AGE_GRP == 'Y70-74' ~ 72.5,
                                 AGE_GRP == 'Y75-79' ~ 77.5,
                                 AGE_GRP == 'Y80-84' ~ 82.5,
                                 AGE_GRP == 'Y85-89' ~ 87.5,
                                 AGE_GRP == 'Y90-94' ~ 92.5,
                                 AGE_GRP == 'Y95-99' ~ 97.5,
                                 .default = NA
                                 ),
           age_grp_3 = case_when(age_years < 30 ~ 'below 30',
                                 age_years >= 30 & age_years < 50 ~ '30-50',
                                 age_years >= 50 ~ 'above 50'),
           ageresid_25 = case_when(AGERESID == 0 ~ 'native born',
                                   AGERESID %in% c('Y1-4','Y5-9', 'Y10-14', 'Y15-19', 'Y20-24') ~ 'below 25',
                                   !is.na(AGERESID) ~ 'above 25'),
                                   .default = NA) %>%
    mutate(educated_abroad = case_when(is_immigrant == 1 & (REFYEAR - years_in_country) >= HATYEAR ~ 1,
                                       is_immigrant == 1 & (REFYEAR - years_in_country) < HATYEAR ~ 0,
                                       .default = NA),
           previous_job_abroad = case_when((REFYEAR - years_in_country) >= YEARPR ~ 1,
                                           (REFYEAR - years_in_country) < YEARPR ~ 0,
                                           .default = NA))
  
  #construct treatment
 
  #construct treatment based on national legal changes (Italy Spain)
  #TODO check which countries the legislation actually applies to (EFTA yes or no?)
  #TODO operationalize for jobs in some way
  EU_ISCO08 <- c(216, 226, 221, 222, 225)
  EU_ISCO88 <- c(214, 222, 221, 223)
  EU_countries <- c('EUR_NEU28_NEFTA', 'EU15', 'NMS10', 'EUR_NEU27_2020_NEFTA', 'EU27_2020')
  country_df <- country_df %>%
    #construct treatment for EU directive (2008)
    mutate(treat_EU08 = case_when(COUNTRYB %in% EU_countries ~ 1,
                                  .default = 0),
           #construct treatment for EU directive (2013)
           treat_EU13 = case_when(COUNTRYB %in% EU_countries ~ 1,
                                  .default = 0),
           #construct country specific treatment
           treat_country = case_when(COUNTRY %in% c('ES', 'IT') & is_immigrant == 1 ~ 1,
                                     COUNTRY %in% c('ES', 'IT') & is_immigrant == 0 ~ 0,
                                     .default = NA),
           treat_EU08_post = ifelse(REFYEAR > 2008, 1, 0),
           treat_EU13_post = ifelse(REFYEAR > 2013, 1, 0),
           treat_country_post = case_when(COUNTRY == 'ES' & REFYEAR > 2014 ~ 1,
                                          COUNTRY == 'ES' & REFYEAR <= 2014 ~ 0,
                                          COUNTRY == 'IT' & REFYEAR > 2009 ~ 1,
                                          COUNTRY == 'IT' & REFYEAR <= 2009 ~ 0)
    )
  #skilled vs unskilled
  country_df <- country_df %>%
    mutate(skill_level = case_when(REFYEAR < 2011 & str_extract(ISCO88_3D, '^\\d{1}') %in% c(1, 2, 3) ~ 'high',
                                   REFYEAR >= 2011 & str_extract(ISCO08_3D, '^\\d{1}') %in% c(1, 2, 3) ~ 'high',
                                   REFYEAR < 2011 & str_extract(ISCO88_3D, '^\\d{1}') %in% c(4, 5, 6, 7, 8, 9) ~ 'low',
                                   REFYEAR >= 2011 & str_extract(ISCO08_3D, '^\\d{1}') %in% c(4, 5, 6, 7, 8, 9) ~ 'low',
                                   .default = NA))
  
  #save
  write_feather(country_df, paste0(result_fp, 'merged_country_op_2006_onwards_', country, '.feather'))
  gc()
}












