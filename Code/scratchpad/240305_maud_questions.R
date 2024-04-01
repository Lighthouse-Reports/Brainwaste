#load libraries
library(dplyr)
library(tidyr)
library(readr)
library(arrow)
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
library(scales)

setwd("/Users/justin-casimirbraun/Brainwaste")

fp_1 <- 'Input Data/ELF_Merged/final/merged_country_final_2006_onwards_'
fp_2 <- '.feather'

college_educated_master_df <- data.frame()
gs_education_bw_master_df <- data.frame()
gs_sum_master_df <- data.frame()
region_master_df <- data.frame()

for(country in countries_to_analyze){
  print(country)
  fp_college <- paste0(fp_1, country, fp_2)
  cur_df_college <- arrow::read_feather(fp_college)
  fp_all <- paste0(fp_1, country, '_ALL', fp_2)
  cur_df_all <- arrow::read_feather(fp_all)
  
  
  college_educated_df <- cur_df_all %>%
    group_by(COUNTRY, REFYEAR, is_immigrant) %>%
    summarise(mean_college_educated = mean(is_college_educated, na.rm = T))
  college_educated_master_df <- college_educated_master_df %>%
    bind_rows(college_educated_df)
  
  gs_education_bw_df <- cur_df_college %>%
    filter(REFYEAR > 2016, !is.na(hatfield1d)) %>%
    group_by(COUNTRY, is_global_south, hatfield1d) %>%
    summarise(uemp_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              unemployed_mean = mean(is_unemployed, na.rm = T),
              temp_mean = mean(is_temp, na.rm = T),
              n = n()) %>%
    pivot_wider(names_from = is_global_south, values_from = c('uemp_mean', 'overed_mean', 'unemployed_mean', 'temp_mean', 'n'))
  
  gs_education_bw_master_df <- gs_education_bw_master_df %>%
    bind_rows(gs_education_bw_df)
  
  gs_sum_df <- cur_df_college %>%
    filter(REFYEAR > 2016) %>%
    group_by(COUNTRY, is_global_south) %>%
    summarise(uemp_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              unemployed_mean = mean(is_unemployed, na.rm = T),
              temp_mean = mean(is_temp, na.rm = T),
              n = n()) %>%
    pivot_wider(names_from = is_global_south, values_from = c('uemp_mean', 'overed_mean', 'unemployed_mean', 'temp_mean', 'n'))
  
  gs_sum_master_df <- gs_sum_master_df %>%
    bind_rows(gs_sum_df)
  
  if('COUNTRY_old' %in% names(cur_df_college)){
    cur_df_college$cur_country <- cur_df_college$COUNTRY_old
  } else {
    cur_df_college$cur_country <- cur_df_college$COUNTRY
  }
  
  region_df <- cur_df_college %>%
    filter(REFYEAR > 2016) %>%
    group_by(cur_country, is_immigrant, REGION_2D) %>%
    summarise(uemp_mean = mean(uemp, na.rm = T),
              overed_mean = mean(overed_1sd_hat_isced, na.rm = T),
              unemployed_mean = mean(is_unemployed, na.rm = T),
              temp_mean = mean(is_temp, na.rm = T),
              n = n()) %>%
    mutate(REGION_2D = as.character(REGION_2D)) %>%
    filter(!is.na(is_immigrant))
  
  region_master_df <- region_master_df %>%
    bind_rows(region_df)
  
}


output_fp <- paste0(cur_date, 'Results/scratchpad/240305_maud_questions/')
dir.create(output_fp, showWarnings = F)

college_educated_master_df <- college_educated_master_df %>%
  filter(!is.na(is_immigrant))
write.csv(college_educated_master_df, paste(output_fp, 'college_educated_country_year.csv'))
ggplot(college_educated_master_df, aes(x = REFYEAR, y = mean_college_educated, color = as.factor(is_immigrant)))+
  geom_point()+
  facet_wrap(.~COUNTRY)+
  labs(x = 'Year', y = '% college educated',
       color = 'Is immigrant', title = 'Share college educated by year, country, and migration status')+
  scale_y_continuous(labels = scales::percent)
ggsave(paste(output_fp, 'college_educated_country_year.png'), width = 10, height = 15, plot = last_plot())


gs_education_bw_master_df <- gs_education_bw_master_df %>%
  filter(!is.na(hatfield1d)) %>%
  dplyr::mutate(hatfield_text = case_when(hatfield1d == 0 ~ 'basic',
                                          hatfield1d == 1 ~ 'education',
                                          hatfield1d == 2 ~ 'arts/humanities',
                                          hatfield1d == 3 ~ 'social sciences',
                                          hatfield1d == 4 ~ 'business/law',
                                          hatfield1d == 5 ~ 'natural sciences',
                                          hatfield1d == 6 ~ 'ict',
                                          hatfield1d == 7 ~ 'engineering',
                                          hatfield1d == 8 ~ 'agriculture',
                                          hatfield1d == 9 ~ 'health',
                                          hatfield1d == 10 ~ 'services',
                                          .default = 'Unknown Field')) %>%
  dplyr::select(-ends_with('NA'))
write.csv(gs_education_bw_master_df, paste(output_fp, 'gs_education_brainwaste_metrics_country.csv'))

write.csv(gs_sum_master_df, paste0(output_fp, 'gs_sum_country.csv'))


# visualize regional results
map_new <- eurostat::get_eurostat_geospatial(output_class = 'sf',
                                             resolution = '60',
                                             nuts_level = 'all',
                                             year = '2021') %>%
  filter(LEVL_CODE %in% c(0,1,2)) %>%
  mutate(NUTS_ID = case_when(LEVL_CODE == 1 & nchar(NUTS_ID) == 3 ~ paste0(NUTS_ID, '0'),
                             .default = NUTS_ID))%>%
  group_by(NUTS_ID) %>%
  slice_head(n=1) %>%
  ungroup()

#merge with mapping data
output_fp_map <- paste0(output_fp, 'maps/')
dir.create(output_fp_map)

region_master_df_map <- region_master_df %>%
  filter(n > 50)%>%
  mutate(nuts = case_when(nchar(as.character(REGION_2D)) == 1 ~ paste0(cur_country, '0', as.character(REGION_2D)),
                          is.na(REGION_2D) ~ cur_country,
                          .default = paste0(cur_country, as.character(REGION_2D)))) %>%
  left_join(map_new, by = c('nuts' = 'NUTS_ID')) %>%
  st_as_sf()

#make map for all of Europe
p1 <- ggplot(region_master_df_map)+
  geom_sf(aes(fill = uemp_mean))+
  facet_grid(.~as.factor(is_immigrant))+
  scale_fill_distiller(palette = 'Spectral', labels = scales::percent)+
  labs(fill = 'Underemployment',
       title = 'Underemployment by natives (0) and immigrants (1)\n2017-2022 by NUTS region')+
  coord_sf(xlim = c(-15, 40), ylim = c(35, 70))

ggsave(paste0(output_fp_map, 'uemp_EU.png'), width = 12, height = 8, plot = p1)

p2 <- ggplot(region_master_df_map)+
  geom_sf(aes(fill = overed_mean))+
  facet_grid(.~as.factor(is_immigrant))+
  scale_fill_distiller(palette = 'Spectral', labels = scales::percent)+
  labs(fill = 'Overeducation',
       title = 'Overeducation by natives (0) and immigrants (1)\n2017-2022 by NUTS region')+
  coord_sf(xlim = c(-15, 40), ylim = c(35, 70))
ggsave(paste0(output_fp_map, 'overed_EU.png'), width = 12, height = 8, plot = p2)


p3 <- ggplot(region_master_df_map)+
  geom_sf(aes(fill = unemployed_mean))+
  facet_grid(.~as.factor(is_immigrant))+
  scale_fill_distiller(palette = 'Spectral', labels = scales::percent)+
  labs(fill = 'Unemployed',
       title = 'Unemployed by natives (0) and immigrants (1)\n2017-2022 by NUTS region')+
  coord_sf(xlim = c(-15, 40), ylim = c(35, 70))
ggsave(paste0(output_fp_map, 'unemployed_EU.png'), width = 12, height = 8, plot = p3)


for(country in region_master_df_map$cur_country){
  cur_df_map <- region_master_df_map %>%
    filter(cur_country == country) %>%
    st_as_sf()
  
  p <- ggplot(cur_df_map)+
    geom_sf(aes(fill = uemp_mean))+
    facet_grid(.~as.factor(is_immigrant))+
    scale_fill_distiller(palette = 'Spectral', labels = scales::percent)+
    labs(fill = 'Underemployment',
         title = paste0(country, 'Underemployment by natives (0) and immigrants (1)\n2017-2022 by NUTS region'))
  
  ggsave(paste0(output_fp_map, 'uemp', country, '.png'), width = 12, height = 8, plot = p)
  
  p <- ggplot(cur_df_map)+
    geom_sf(aes(fill = overed_mean))+
    facet_grid(.~as.factor(is_immigrant))+
    scale_fill_distiller(palette = 'Spectral', labels = scales::percent)+
    labs(fill = 'Overeducation',
         title = paste0(country, 'Overeducation by natives (0) and immigrants (1)\n2017-2022 by NUTS region'))
  ggsave(paste0(output_fp_map, 'overed', country, '.png'), width = 12, height = 8, plot = p)
  
  
  p <- ggplot(cur_df_map)+
    geom_sf(aes(fill = unemployed_mean))+
    facet_grid(.~as.factor(is_immigrant))+
    scale_fill_distiller(palette = 'Spectral', labels = scales::percent)+
    labs(fill = 'Unemployed',
         title = paste0(country, 'Unemployed by natives (0) and immigrants (1)\n2017-2022 by NUTS region'))
  ggsave(paste0(output_fp_map, 'unemployed', country, '.png'), width = 12, height = 8, plot = p)
}

#save csv
write.csv(region_master_df, paste0(output_fp, 'regional_bw_metrics.csv'))

#save geometry
saveRDS(region_master_df_map, paste0(output_fp, 'regional_bw_metrics.rds'))
