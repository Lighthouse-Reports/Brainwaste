#load libraries
library(dplyr)
library(tidyr)
library(readr)
library(feather)
library(openxlsx)
library(stringr)
library(ggplot2)
library(eurostat)
library(sf)
library(wesanderson)
library(stringr)
library(ggh4x)
library(rex)
library(tidyverse)
library(tidytext)

setwd("/Users/justin-casimirbraun/Brainwaste")

countries_to_analyze <- c('AT',
                          'BE', 'BG', 'CH',
                          'CY', 'CZ', 'DK', 'EE',
                          'EL', 'ES', 'FI', 'FR',
                          'HR', 'HU', 'IE', 'IS', 
                          'IT', 'LT', 'LU', 'LV',
                          'MT', 'NL', 'NO', 'PL',
                          'PT', 'RO', 'SE', 'SI',
                          'SK', 'UK'
)


in_part1 <- 'Results/descriptives/'
in_part2 <- '/is_immigrant/REGION_2D.csv'
master_df <- data.frame()
for(country in countries_to_analyze){
  fp <- paste0(in_part1, country, in_part2)
  cur_df <- read.csv(fp)
  cur_df$REGION_2D <- as.character(cur_df$REGION_2D)
  master_df <- master_df %>%
    dplyr::bind_rows(cur_df)
}

master_df <- master_df %>%
  filter(REFYEAR == 2019, !is.na(is_immigrant))

#Generate map of Europe
#NUTS 2 map
map_2d <- eurostat::get_eurostat_geospatial(output_class = 'sf',
                                            resolution = '60',
                                            nuts_level = '2',
                                            year = '2021'
)
#NUTS 1 map
map_1d <- eurostat::get_eurostat_geospatial(output_class = 'sf',
                                            resolution = '60',
                                            nuts_level = '1',
                                            year = '2021'
) %>%
  mutate(NUTS_ID = paste0(NUTS_ID, '0'))

map_0d <- eurostat::get_eurostat_geospatial(output_class = 'sf',
                                            resolution = '60',
                                            nuts_level = '0',
                                            year = '2021'
) %>%
  mutate(NUTS_ID = paste0(NUTS_ID, '00'))
#bind nuts 1 and nuts 2 maps, so we always get some match
map <- bind_rows(map_2d, map_1d) %>%
  bind_rows(map_0d) %>%
  distinct(NUTS_ID, .keep_all = T)

master_df_map <- master_df %>%
  pivot_wider(id_cols = c('REGION_2D', 'COUNTRY'), names_from = is_immigrant, values_from = INCDECIL_mean, names_prefix = 'immigrant_') %>%
  mutate(Inc_diff = immigrant_0 - immigrant_1, 
         nuts = case_when(is.na(REGION_2D) ~ paste0(COUNTRY, '00'),
                          nchar(as.character(REGION_2D)) == 1 ~ paste0(COUNTRY, '0', as.character(REGION_2D)),
                          .default = paste0(COUNTRY, as.character(REGION_2D)))) %>%
  left_join(map, by = c('nuts' = 'NUTS_ID')) %>%
  st_as_sf()

ggplot(master_df_map)+
  geom_sf(aes(fill = Inc_diff))+
  scale_fill_gradientn(colours = wesanderson::wes_palette("Zissou1", 100, type = "continuous")) +
  coord_sf(xlim = c(-20, 35), ylim = c(33, 70))+
  labs(fill = 'Decile difference')+
  ggtitle('Native - Immigrant income difference 2019')

