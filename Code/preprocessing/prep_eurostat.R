setwd("/Users/justin-casimirbraun/Brainwaste")

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tidyr)
library(hash)

#Clean shortage data
shortage <- read_excel('Input Data/eurostat/shortage_raw.xlsx', sheet = 3,na =  ":", skip = 9)
shortage_long <- shortage %>%
  select(-starts_with('...')) %>%
  rename(country_long = TIME...1,
         nace = TIME...2) %>%
  filter(country_long != 'GEO (Labels)') %>%
  pivot_longer(cols = starts_with('20'), names_to = 'year_quarter', values_to = 'shortage') %>%
  separate(year_quarter, c("year", "quarter"), sep = '-') %>%
  mutate(shortage = as.numeric(shortage)) %>%
  group_by(country_long, nace, year) %>%
  summarise(shortage = mean(shortage, na.rm = T)) %>%
  filter(!country_long %in% c('b', 'c', 'e', 'p', 'u')) %>%
  mutate(NACE2_1D = case_when(nace == 'Agriculture, forestry and fishing' ~ 'A',
                              nace == 'Mining and quarrying' ~ 'B',
                              nace == 'Manufacturing' ~ 'C',
                              nace == 'Electricity, gas, steam and air conditioning supply' ~ 'D',
                              nace == 'Water supply; sewerage, waste management and remediation activities' ~ 'E',
                              nace == 'Construction' ~ 'F',
                              nace == 'Wholesale and retail trade; repair of motor vehicles and motorcycles' ~ 'G',
                              nace == 'Transportation and storage' ~ 'H',
                              nace == 'Accommodation and food service activities' ~ 'I',
                              nace == 'Information and communication' ~ 'J',
                              nace == 'Financial and insurance activities' ~ 'K',
                              nace == 'Real estate activities' ~ 'L',
                              nace == 'Professional, scientific and technical activities' ~ 'M',
                              nace == 'Administrative and support service activities' ~ 'N',
                              nace == 'Public administration and defence; compulsory social security' ~ 'O',
                              nace == 'Education' ~ 'P',
                              nace == 'Human health and social work activities' ~ 'Q',
                              nace == 'Arts, entertainment and recreation' ~ 'R',
                              nace == 'Other service activities' ~ 'S',
                              nace == 'Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use' ~ 'T', #no matches
                              nace == 'Activities of extraterritorial organisations and bodies' ~ 'U', #no matches
                              .default = NA))

write.csv(shortage_long, 'Input Data/eurostat/shortage_transformed.csv')

#Clean population data
pop <- read_excel('Input Data/eurostat/pop_raw.xlsx', sheet = 3,na =  ":", skip = 9)
pop_clean <- pop %>%
  dplyr::select(-starts_with('...')) %>%
  rename(country_long = 'C_BIRTH (Labels)...1',
         year = 'C_BIRTH (Labels)...2',
         foreign = 'Foreign country',
         native = 'Reporting country',
         total = Total) %>%
  filter(country_long != 'GEO (Labels)') %>%
  dplyr::select(country_long, year, foreign, native, total) %>%
  mutate(foreign = as.numeric(foreign),
         native = as.numeric(native),
         total = as.numeric(total)) %>%
  filter(!country_long %in% c('Montenegro', 'North Macedonia', 'Albania',
                             'Serbia', 'Türkiye', 'Ukraine',
                             'Andorra', 'Armenia', 'Azerbaijan',
                             'Special value', 'Available flags:',
                             'bp', 'b', 'e', 'ep', 'p')) %>%
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
                                   country_long == "Estonia" ~ "EE"))

#TODO: country groups
country_group_dic <- hash()
country_group_dic[['Baltics']] <- c('LV', 'LT', 'EE')
country_group_dic[['Visegrad']] <- c('PL', 'CZ', 'SK', 'HU')
country_group_dic[['eu07']] <- c('RO', 'BG')
country_group_dic[['yugo']] <- c('SI', 'HR')

for(cur_key in keys(country_group_dic)){
  cur_countries <- country_group_dic[[cur_key]]
  
  print(cur_key)
  print(cur_countries)
  
  pop_cur <- pop_clean %>%
    filter(country_short %in% cur_countries) %>%
    group_by(year) %>%
    summarise(total = sum(total, na.rm = T),
              native = sum(native, na.rm = T),
              foreign = sum(native, na.rm = T)) %>%
    mutate(country_long = cur_key,
           country_short = cur_key)
  pop_clean <- pop_clean %>%
    bind_rows(pop_cur)
}

write.csv(pop_clean, 'Input Data/eurostat/pop_transformed.csv')


#clean income data
income_silc <- read_excel('Input Data/eurostat/income_raw_SILC.xlsx', sheet = 3,na =  ":", skip = 8)
income_silc_long <- income_silc %>%
  select(-starts_with('...')) %>%
  rename(country_long = 'QUANTILE (Labels)...1',
         year = 'QUANTILE (Labels)...2',
         d1 = 'Fifth percentile',
         d1_tc = 'First decile',
         d2_tc = 'Second decile',
         d3_tc = 'Third decile',
         d4_tc = 'Fourth decile',
         d5_tc = 'Fifth decile',
         d6_tc = 'Sixth decile',
         d7_tc = 'Seventh decile',
         d8_tc = 'Eighth decile',
         d9_tc = 'Ninth decile',
         d10 = 'Ninety-fifth percentile') %>%
  filter(!country_long %in% c('GEO (Labels)', 'Montenegro', 'North Macedonia', 'Albania',
                              'Serbia', 'Türkiye', 'Ukraine',
                              'Andorra', 'Armenia', 'Azerbaijan', 'Kosovo*',
                              'Special value', 'Available flags:',
                              'bp', 'b', 'e', 'ep', 'p', 'bu', 's', 'u'), !is.na(country_long)) %>%
  mutate_at(.vars = c('d1', 'd1_tc', 'd2_tc', 'd3_tc', 'd4_tc', 'd5_tc', 'd6_tc', 'd7_tc', 'd8_tc', 'd9_tc', 'd10'), .funs = as.numeric) %>%
  mutate(d2 = (d1_tc + d2_tc)/2,
         d3 = (d2_tc + d3_tc)/2,
         d4 = (d3_tc + d4_tc)/2,
         d5 = (d4_tc + d5_tc)/2,
         d6 = (d5_tc + d6_tc)/2,
         d7 = (d6_tc + d7_tc)/2,
         d8 = (d7_tc + d8_tc)/2,
         d9 = (d8_tc + d9_tc)/2) %>%
  select(country_long, year, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10) %>%
  pivot_longer(cols = starts_with('d'), names_to = 'decile', names_prefix = 'd', values_to = 'income')

write.csv(income_silc_long, 'Input Data/eurostat/income_transformed_SILC.csv')


#employed immigrants
employed_immigrants <- read_excel('Input Data/eurostat/employed_immigrants_raw.xlsx', sheet = 3,na =  ":", skip = 11) %>%
  dplyr::select(-starts_with('...')) %>%
  rename(country_long = TIME,
         count_thousands = '2019') %>%
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
                                   country_long == "Estonia" ~ "EE"),
         count = as.numeric(count_thousands)*1000) %>%
  filter(!is.na(country_short))

write.csv(employed_immigrants, 'Input Data/eurostat/employed_immigrants_transformed.csv')  

         