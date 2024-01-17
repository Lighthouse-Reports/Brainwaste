#dataset: https://ec.europa.eu/eurostat/databrowser/bookmark/05ecd5bc-e53f-43dc-bc54-fd6266a4f98d?lang=en

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)

setwd("/Users/justin-casimirbraun/Brainwaste")

pop_immigr <- read_excel('Input Data/scratchpad/rq_01_02/migr_imm8__custom_9252925_spreadsheet.xlsx', sheet = 3, na =':', skip = 10) %>%
  select(-dplyr::starts_with('...'))
pop_immigr <- pop_immigr[!is.na(pop_immigr$'2019'),]
pop_immigr <- pop_immigr %>%
  pivot_longer(cols = starts_with('2'), names_to = 'year', values_to = 'immigrants')%>%
  rename(geo = TIME) %>%
  mutate(geo = ifelse(geo == 'European Union - 27 countries (from 2020)', 'EU/EEA', geo))


pop_total <- read_excel('Input Data/scratchpad/rq_01_01/migr_pop2ctz__custom_9252701_spreadsheet.xlsx', sheet = 4, na = ':', skip = 10) %>%
  select(-dplyr::starts_with('...'))
pop_total <- pop_total[!is.na(pop_total$'2019'),]
pop_total <- pop_total %>%
  pivot_longer(cols = starts_with('2'), names_to = 'year', values_to = 'total')%>%
  rename(geo = TIME)

pop_combined <- dplyr::left_join(pop_immigr, pop_total, by = c('geo', 'year')) %>%
  mutate(immigrants = as.numeric(immigrants),
         total = as.numeric(total),
         share_immigrants = immigrants/total) %>%
  filter(!geo %in% c('Montenegro', 'North Macedonia'))

outpath <- 'Results/scratchpad/rq01_02/'
dir.create(outpath, showWarnings = F)
write.csv(pop_combined, paste0(outpath, 'year_country_immigr_flows.csv'))

pop_combined %>%
  select(geo, year, immigrants) %>%
  pivot_wider(values_from = immigrants, names_from = year) %>%
  write.csv(paste0(outpath, 'year_country_immigr_flows_wide.csv'))

pop_combined %>%
  select(geo, year, share_immigrants) %>%
  pivot_wider(values_from = share_immigrants, names_from = year) %>%
  write.csv(paste0(outpath, 'year_country_immigr_flows_share_wide.csv'))
