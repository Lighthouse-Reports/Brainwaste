#dataset: https://ec.europa.eu/eurostat/databrowser/bookmark/05ecd5bc-e53f-43dc-bc54-fd6266a4f98d?lang=en

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)

setwd("/Users/justin-casimirbraun/Brainwaste")

pop_for <- read_excel('Input Data/scratchpad/rq_01_01/migr_pop2ctz__custom_9252701_spreadsheet.xlsx', sheet = 3, na = ':', skip = 10) %>%
  select(-dplyr::starts_with('...'))
pop_for <- pop_for[!is.na(pop_for$'2019'),]
pop_for <- pop_for %>%
  pivot_longer(cols = starts_with('2'), names_to = 'year', values_to = 'foreigners') %>%
  rename(geo = TIME)

pop_total <- read_excel('Input Data/scratchpad/rq_01_01/migr_pop2ctz__custom_9252701_spreadsheet.xlsx', sheet = 4, na = ':', skip = 10) %>%
  select(-dplyr::starts_with('...'))
pop_total <- pop_total[!is.na(pop_total$'2019'),]
pop_total <- pop_total %>%
  pivot_longer(cols = starts_with('2'), names_to = 'year', values_to = 'total')%>%
  rename(geo = TIME)

pop_combined <- dplyr::left_join(pop_for, pop_total, by = c('geo', 'year')) %>%
  mutate(foreigners = as.numeric(foreigners),
         total = as.numeric(total),
         share_foreigners = foreigners/total)

eu_shares <- pop_combined %>%
  group_by(year) %>%
  summarise(foreigners = sum(foreigners, na.rm = T),
            total = sum(total, na.rm = T)) %>%
  mutate(geo = 'EU/EEA',
         share_foreigners = foreigners/total)
pop_combined <- pop_combined %>%
  bind_rows(eu_shares)

outpath <- 'Results/scratchpad/rq01_01/'
dir.create(outpath, showWarnings = F)
write.csv(pop_combined, paste0(outpath, 'year_country_immigr_total.csv'))

pop_combined %>%
  select(geo, year, foreigners) %>%
  pivot_wider(values_from = foreigners, names_from = year) %>%
  write.csv(paste0(outpath, 'year_country_immigr_wide.csv'))

pop_combined %>%
  select(geo, year, share_foreigners) %>%
  pivot_wider(values_from = share_foreigners, names_from = year) %>%
  write.csv(paste0(outpath, 'year_country_immigr_share_wide.csv'))
