#load libraries
library(dplyr)
library(tidyr)
library(readr)
library(feather)
library(openxlsx)
library(ggplot2)
library(arrow)
library(broom)
library(scales)
library(sf)
library(geojsonsf)
library(jsonlite)


output_fp <- paste0(cur_date, 'Results/flourish/')
dir.create(output_fp, showWarnings = F)

countries_match <- data.frame(COUNTRY =  c('AT', 'BE', 'CH','CY', 'DK',
                                           'EL', 'ES', 'FI', 'FR', 'IE',
                                           'IS', 'IT', 'LU', 'MT', 'NL', 
                                           'NO', 'PT', 'SE', 'UK', 
                                           'Baltics', 'eu07', 'Visegrad', 'yugo', 'EU'),
                              country_long = c('Austria', 'Belgium', 'Switzerland', 'Cyprus', 'Denmark',
                                               'Greece', 'Spain', 'Finland', 'France', 'Ireland',
                                               'Iceland', 'Italy', 'Luxembourg', 'Malta', 'Netherlands',
                                               'Norway', 'Portugal', 'Sweden', 'UK*',
                                               'Baltics', 'Romania/Bulgaria', 'Visegrad', 'Croatia/Slovenia', 'Europe**')
                              )


order_df <- data.frame(metric_short = c('overed',
                                       'uemp',
                                       'unemployed',
                                       'skilled',
                                       'income', 
                                       'shortage',
                                       'regulated',
                                       'temp'),
                       metric_long = c('% over-qualified',
                                       '% under-employed',
                                       '% unemployed',
                                       '% in high-skilled occupation',
                                       'mean income decile',
                                       '% in shortage occupation',
                                       '% in regulated occupation',
                                       '% fixed-term contract'),
                       metric_diff = c('difference % over-qualified',
                                       'difference % under-employed',
                                       'difference % unemployed',
                                       'difference % in high-skilled occupation',
                                       'difference mean income decile',
                                       'difference % in shortage occupation',
                                       'difference % in regulated occupation',
                                       'difference % fixed-term contract'),
                       order = 1:8)

###Fig 1###
fig1_htable_college_raw <- read.csv(paste0(cur_date, 'Results/scratchpad/sumtables/hypothesis_table_college.csv'))

#merge full country names
fig1_htable_college_ready <- fig1_htable_college_raw %>%
  mutate_all(as.character) %>%
  dplyr::select(-X) %>%
  dplyr::left_join(countries_match, by = c('COUNTRY')) %>% #join full country names
  pivot_longer(!c(country_long, COUNTRY), names_to = 'name', values_to = 'vals') %>%
  separate(name, c('metric', 'indicator'), sep = '_', extra = 'merge') %>%
  filter(metric != 'share') %>%
  mutate(metric = case_when(metric == 'underemployment' ~ '% under-employed',
                            metric == 'underemp' ~ '% under-employed',
                            metric == 'overed' ~ '% over-qualified',
                            metric == 'unemployed' ~ '% unemployed',
                            metric == 'skilled' ~ '% in high-skilled occupation',
                            metric == 'income' ~ 'mean income decile',
                            metric == 'shortage' ~ '% in shortage occupation',
                            metric == 'regulated' ~ '% in regulated occupation',
                            .default = NA),
         indicator = case_when(indicator == 'mean_native' ~ 'Native (mean)', 
                               indicator == 'mean_immigrant' ~ 'Immigrant (mean)',
                               indicator == 'diff' ~ 'Raw Difference',
                               indicator == 'regression' ~ 'Controlled Difference',
                               .default = indicator)) %>%
  pivot_wider(values_from = 'vals', names_from = 'indicator') %>%
  mutate_at(.vars = c('Native (mean)', 'Immigrant (mean)', 'Raw Difference', 'Controlled Difference'), as.numeric) %>%
  mutate_at(.vars = c('Native (mean)', 'Immigrant (mean)', 'Raw Difference', 'Controlled Difference'), function(x) x*100) %>% #switch to percentages
  mutate(`Native (mean)` = case_when(metric == 'mean income decile' ~ `Native (mean)`/100,
                                     .default = `Native (mean)`),
         `Immigrant (mean)` = case_when(metric == 'mean income decile' ~ `Immigrant (mean)`/100,
                                     .default = `Immigrant (mean)`),
         `Raw Difference` = case_when(metric == 'mean income decile' ~ `Raw Difference`/100,
                                     .default = `Raw Difference`),
         `Controlled Difference` = case_when(metric == 'mean income decile' ~ `Controlled Difference`/100,
                                     .default = `Controlled Difference`)) %>%
  left_join(order_df, by = c('metric' = 'metric_long')) %>%
  arrange(order)
  
write.csv(fig1_htable_college_ready, paste0(output_fp, 'fig1_htable_college.csv'))


#Fig 1a#
EU_map <-  eurostat::get_eurostat_geospatial(output_class = 'sf',
                                                       resolution = '60',
                                                       nuts_level = '0',
                                                       year = '2021')
yugo_map <- EU_map %>% 
  filter(NUTS_ID %in% c('SI', 'HR')) %>%
  st_union() %>%
  as.data.frame() %>%
  mutate(NUTS_ID = 'yugo') %>%
  st_as_sf()

eu07_map <- EU_map %>% 
  filter(NUTS_ID %in% c('BG', 'RO')) %>%
  st_union() %>%
  as.data.frame() %>%
  mutate(NUTS_ID = 'eu07') %>%
  st_as_sf()

visegrad_map <- EU_map %>% 
  filter(NUTS_ID %in% c('PL', 'CZ', 'SK', 'HU')) %>%
  st_union() %>%
  as.data.frame() %>%
  mutate(NUTS_ID = 'Visegrad') %>%
  st_as_sf()

baltics_map <- EU_map %>% 
  filter(NUTS_ID %in% c('EE', 'LT', 'LV')) %>%
  st_union() %>%
  as.data.frame() %>%
  mutate(NUTS_ID = 'Baltics') %>%
  st_as_sf()  

EU_map <- EU_map %>%
  bind_rows(yugo_map, eu07_map, visegrad_map, baltics_map)

fig_1a_map <- fig1_htable_college_ready %>%
  dplyr::select(COUNTRY, country_long, metric, `Native (mean)`, `Immigrant (mean)`, `Raw Difference`, `Controlled Difference`, `significance`) %>%
  pivot_wider(names_from = 'metric', values_from =  c('Native (mean)', 'Immigrant (mean)', 'Raw Difference', 'Controlled Difference', 'significance')) %>%
  left_join(EU_map, by = c('COUNTRY' = 'NUTS_ID')) %>%
  filter(COUNTRY != 'EU') %>%
  st_as_sf()

st_write(fig_1a_map, paste0(output_fp, 'fig1a_bw_map.geojson'), overwrite = T)
  

###Fig 1###
fig1b_raw <- read.csv(paste0(cur_date, 'Results/scratchpad/sumtables/hypothesis_table_immigrants.csv'))

#merge full country names
fig1b_ready <- fig1b_raw %>%
  mutate_all(as.character) %>%
  dplyr::select(-X) %>%
  dplyr::left_join(countries_match, by = c('COUNTRY')) %>% #join full country names
  pivot_longer(!c(country_long, COUNTRY), names_to = 'name', values_to = 'vals') %>%
  separate(name, c('metric', 'indicator'), sep = '_', extra = 'merge') %>%
  filter(metric != 'share') %>%
  mutate(metric = case_when(metric == 'underemployment' ~ '% under-employed',
                            metric == 'underemp' ~ '% under-employed',
                            metric == 'overed' ~ '% over-qualified',
                            metric == 'unemployed' ~ '% unemployed',
                            metric == 'skilled' ~ '% in high-skilled occupation',
                            metric == 'income' ~ 'mean income decile',
                            metric == 'shortage' ~ '% in shortage occupation',
                            metric == 'regulated' ~ '% in regulated occupation',
                            .default = NA),
         indicator = case_when(indicator == 'mean_native' ~ 'Native (mean)', 
                               indicator == 'mean_immigrant' ~ 'Immigrant (mean)',
                               indicator == 'diff' ~ 'Raw Difference',
                               indicator == 'regression' ~ 'Controlled Difference',
                               .default = indicator)) %>%
  pivot_wider(values_from = 'vals', names_from = 'indicator') %>%
  mutate_at(.vars = c('Native (mean)', 'Immigrant (mean)', 'Raw Difference', 'Controlled Difference'), as.numeric) %>%
  mutate_at(.vars = c('Native (mean)', 'Immigrant (mean)', 'Raw Difference', 'Controlled Difference'), function(x) x*100) %>% #switch to percentages
  mutate(`Native (mean)` = case_when(metric == 'mean income decile' ~ `Native (mean)`/100,
                                     .default = `Native (mean)`),
         `Immigrant (mean)` = case_when(metric == 'mean income decile' ~ `Immigrant (mean)`/100,
                                        .default = `Immigrant (mean)`),
         `Raw Difference` = case_when(metric == 'mean income decile' ~ `Raw Difference`/100,
                                      .default = `Raw Difference`),
         `Controlled Difference` = case_when(metric == 'mean income decile' ~ `Controlled Difference`/100,
                                             .default = `Controlled Difference`)) %>%
  left_join(order_df, by = c('metric' = 'metric_long')) %>%
  arrange(order)

write.csv(fig1b_ready, paste0(output_fp, 'fig1b_htable_all_immigrants.csv'))

### Fig 2 ###
fig2_master_df <- data.frame()
fp1 <- paste0(cur_date, 'Results/descriptives/')
fp2 <- '/is_immigrant/basic.csv'
for(country in countries_to_analyze){
  cur_df <- read.csv(paste0(fp1, country, fp2))
  fig2_master_df <- fig2_master_df %>%
    bind_rows(cur_df)
}

fig2_master_df_ready <- fig2_master_df %>%
  filter(n > 50, !is.na(is_immigrant)) %>%
  dplyr::select(is_immigrant, REFYEAR, COUNTRY, ends_with('mean')) %>%
  pivot_longer(!c(is_immigrant, REFYEAR, COUNTRY), names_to = 'metric', values_to = 'vals') %>%
  mutate(vals = case_when(metric == 'INCDECIL_mean' ~ vals,
                          .default = vals *100),
    is_immigrant = case_when(is_immigrant == 0 ~ 'Native (Mean)',
                                  is_immigrant == 1 ~ 'Immigrant (Mean)',
                                  .default = NA),
         metric = case_when(metric == 'is_unemployed_mean' ~ '% unemployed',
                            metric == 'uemp_mean' ~ '% under-employed',
                            metric == 'is_temp_mean' ~ '% fixed-term contract',
                            metric == 'INCDECIL_mean' ~ 'mean income decile',
                            metric == 'overed_1sd_hat_isced_mean' ~ '% over-qualified',
                            metric == 'regulated_profession_mean' ~ '% in regulated occupation',
                            .default = NA)) %>%
  pivot_wider(names_from = 'is_immigrant', values_from = 'vals') %>%
  mutate(Difference = `Immigrant (Mean)` - `Native (Mean)`) %>%
  left_join(countries_match, by = 'COUNTRY') %>%
  filter(!(metric == '% in regulated occupation' & REFYEAR < 2012), !is.na(`Native (Mean)`)) %>%
  left_join(order_df, by = c('metric' = 'metric_long')) %>%
  arrange(order)

write.csv(fig2_master_df_ready, paste0(output_fp, 'fig2_time_country.csv'))


### Fig 3 ###
fig3_raw <- read.csv(paste0(cur_date, 'Results/scratchpad/240305_maud_questions/gs_sum_country.csv'))

fig3_ready <- fig3_raw %>%
  dplyr::select(-X) %>%
  pivot_longer(!COUNTRY, names_to = 'name', values_to = 'vals') %>%
  separate(name, c('metric', 'group'), sep = '_mean_', extra = 'merge') %>%
  filter(!is.na(vals), !is.na(group), group != 'NA') %>%
  mutate(vals = vals *100,
      metric = case_when(metric == 'uemp' ~ '% under-employed',
                            metric == 'overed' ~ '% over-qualified',
                            metric == 'unemployed' ~ '% unemployed',
                            metric == 'temp' ~ '% fixed-term contract',
                            .default = NA),
         group = case_when(group == 'MIGR_GN' ~ 'Global North migrants',
                           group == 'MIGR_GS' ~ 'Global South migrants',
                           group == 'NAT' ~ 'natives',
                           .default = NA)) %>%
  pivot_wider(names_from = group, values_from = vals) %>%
  mutate(`Difference Global South-Global North` = `Global South migrants` - `Global North migrants`) %>%
  left_join(countries_match, by = 'COUNTRY') %>%
  left_join(order_df, by = c('metric' = 'metric_long')) %>%
  arrange(order)

write.csv(fig3_ready, paste0(output_fp, 'fig3_gs_gn_country.csv'))  


### Fig 4 ###
fig4_raw <- read.csv(paste0(cur_date, 'Results/scratchpad/sumtables/hypothesis_table_gender.csv'))

fig4_ready <- fig4_raw %>%
  mutate_all(as.character) %>%
  dplyr::select(-X) %>%
  pivot_longer(!COUNTRY, names_to = 'name', values_to = 'vals') %>%
  separate(name, c('metric', 'group'), sep = '_', extra = 'merge') %>%
  mutate(metric = case_when(metric == 'underemployment' ~ '% under-employed',
                            metric == 'underemp' ~ '% under-employed',
                            metric == 'overed' ~ '% over-qualified',
                            metric == 'unemployed' ~ '% unemployed',
                            metric == 'skilled' ~ '% in high-skilled occupation',
                            metric == 'income' ~ 'mean income decile',
                            metric == 'shortage' ~ '% in shortage occupation',
                            metric == 'regulated' ~ '% in regulated occupation',
                            .default = NA),
         group = case_when(group == 'mean_immigrant_man' ~ 'immigrant men',
                           group == 'mean_immigrant_woman' ~ 'immigrant women',
                           group == 'mean_native_man' ~ 'native men',
                           group == 'mean_native_woman' ~ 'native women',
                            group == 'diff_man' ~ 'men: migrants - natives',
                           group == 'diff_woman' ~ 'women: migrants - natives',
                           group == 'diff_woman_man' ~ 'difference in gap between men and women',
                           .default = group)) %>%
  pivot_wider(names_from = 'group', values_from = 'vals') %>%
  mutate(`immigrant men` = case_when(metric != 'mean income decile' ~ as.numeric(`immigrant men`) *100,
                                                               .default = as.numeric(`immigrant men`)),
         `immigrant women` = case_when(metric != 'mean income decile' ~ as.numeric(`immigrant women`) *100,
                                     .default = as.numeric(`immigrant women`)),
         `native men` = case_when(metric != 'mean income decile' ~ as.numeric(`native men`) *100,
                                     .default = as.numeric(`native men`)),
         `native women` = case_when(metric != 'mean income decile' ~ as.numeric(`native women`) *100,
                                     .default = as.numeric(`native women`)),
         `men: migrants - natives` = case_when(metric != 'mean income decile' ~ as.numeric(`men: migrants - natives`) * 100,
                                              .default = as.numeric(`men: migrants - natives`)),
         `women: migrants - natives` = case_when(metric != 'mean income decile' ~ as.numeric(`women: migrants - natives`) *100,
                                              .default = as.numeric(`women: migrants - natives`)),
         `difference in gap between men and women` = case_when(metric != 'mean income decile' ~ as.numeric(`difference in gap between men and women`) *100,
                                              .default = as.numeric(`difference in gap between men and women`)),
         `regression` = case_when(metric != 'mean income decile' ~ as.numeric(`regression`) *100,
                                                               .default = as.numeric(`regression`))) %>%
  left_join(countries_match, by = 'COUNTRY') %>%
  left_join(order_df, by = c('metric' = 'metric_long')) %>%
  arrange(order)


write.csv(fig4_ready, paste0(output_fp, 'fig4_htable_gender.csv'))


### Fig 5 ###
fig5_raw <- readRDS(paste0(cur_date, 'Results/scratchpad/240305_maud_questions/regional_bw_metrics.rds'))

remove_list <- c('ES70', 'FRY3', 'PT20', 'PT30', 'FRY1', 'FRY2', 'FRY4')

fig5_ready <- fig5_raw %>%
  dplyr::select(NAME_LATN, nuts, is_immigrant, uemp_mean, overed_mean, unemployed_mean, temp_mean) %>%
  mutate(is_immigrant = case_when(is_immigrant == 0 ~ 'native',
                                  is_immigrant == 1 ~ 'immigrant'),
         overed_mean = overed_mean * 100,
         uemp_mean = uemp_mean * 100,
         unemployed_mean = unemployed_mean * 100,
         temp_mean = temp_mean * 100) %>%
  pivot_longer(cols = c('overed_mean', 'uemp_mean', 'unemployed_mean', 'temp_mean'), names_to = 'metric', values_to = 'vals') %>%
  mutate(group = paste0(is_immigrant, '_', metric)) %>%
  dplyr::select(-is_immigrant, -metric) %>%
  pivot_wider(names_from = 'group', values_from = 'vals') %>%
  mutate(uemp_diff = immigrant_uemp_mean - native_uemp_mean,
         overed_diff = immigrant_overed_mean - native_overed_mean,
         unemployed_diff =immigrant_unemployed_mean - native_unemployed_mean,
         temp_diff = immigrant_temp_mean - native_temp_mean) %>%
  filter(!(nuts %in% remove_list)) %>%
  rename('% under-employed difference' = 'uemp_diff',
         '% native under-employed' = 'native_uemp_mean',
         '% immigrant under-employed' = 'immigrant_uemp_mean',
         '% over-qualified difference' = 'overed_diff', 
         '% native over-qualified' = 'native_overed_mean',
         '% immigrant over-qualified' = 'immigrant_overed_mean',
         '% unemployed difference' =  'unemployed_diff',
         '% native unemployed' = 'native_unemployed_mean',
         '% immigrant unemployed' = 'immigrant_unemployed_mean',
         '% fixed-term contract difference' = 'temp_diff',
         '% native fixed-term contract' = 'native_temp_mean',
         '% immigrant fixed-term contract' = 'immigrant_temp_mean') %>%
  st_as_sf()
  

st_write(fig5_ready, paste0(output_fp, 'fig5_bw_regional.geojson'))

### Fig 6 ###
fig6_raw <- read.csv(paste0(cur_date, 'Results/scratchpad/rq03_01/hatfield_overed_underemp_diff.csv'))

fig6_ready <- fig6_raw %>%
  dplyr::select(-X) %>%
  filter(n_immigrant > 50, n_native > 50, hatfield_text != 'basic') %>%
  rename('difference % under-employed' = 'uemp_diff',
         'difference % over-qualified' = 'overed_diff',
         'difference % unemployed' = 'unemployed_diff',
         'difference % fixed term contract' = 'temp_diff',
         'field of education' = 'hatfield_text') %>%
  pivot_longer(cols = starts_with('difference'), names_to = 'metric_difference', values_to = 'Difference') %>%
  mutate(Difference = Difference *100)%>%
  left_join(countries_match, by = 'COUNTRY') %>%
  left_join(order_df, by = c('metric_difference' = 'metric_diff')) %>%
  arrange(order)

write.csv(fig6_ready, paste0(output_fp, 'fig6_education.csv'))

### Fig 7 ###
fig7_raw <- read.csv(paste0(cur_date, 'Results/scratchpad/rq03_01/occupation_overed_underemp_diff_SEX.csv'))

fig7_ready <- fig7_raw %>%
  dplyr::select(-X, -ends_with('diff')) %>%
  pivot_longer(cols = c(ends_with('native'), ends_with('immigrant')), names_to = 'metric', values_to = 'vals') %>%
  separate(metric, c('metric', 'group'), sep = '_', extra = 'merge') %>%
  mutate(vals = case_when(metric != 'n' ~ vals *100,
                          .default = vals)) %>%
  mutate(metric = case_when(metric == 'uemp' ~ '% under-employed',
                            metric == 'overed' ~ '% over-qualified',
                            metric == 'n' ~ 'sample size',
                            metric == 'share' ~ '% of population',
                            .default = NA),
         group = case_when(group == 'mean_native' ~ 'native',
                           group == 'mean_immigrant' ~ 'immigrant',
                           .default = group),
         SEX = case_when(SEX == 1 ~ 'men',
                         SEX == 2 ~ 'women',
                         .default = NA),
         group = paste0(group, ' ', SEX)) %>%
  pivot_wider(names_from = 'metric', values_from = 'vals') %>%
  left_join(countries_match, by = 'COUNTRY') %>%
  filter(`sample size` > 50)

write.csv(fig7_ready, paste0(output_fp, 'fig7_occupation.csv'))


### Fig 8 ###
fig8_raw <- read.csv(paste0(cur_date, 'Results/scratchpad/rq03_01/education_occupation_brainwaste.csv'))

fig8_ready <- fig8_raw %>%
  filter(count > 50, hatfield_text != 'basic') %>%
  left_join(countries_match, by = 'COUNTRY') %>%
  dplyr::select(country_long, hatfield_text, Occupation_Text, share_uemp, share_overed, share_temp, share) %>%
  mutate(share_uemp = 100*share_uemp,
         share_overed = 100*share_overed,
         share_temp = 100*share_temp,
         share = 100 * share) %>%
  rename('country' = 'country_long',
         'field of education' = 'hatfield_text',
         'occupation' = 'Occupation_Text',
         '% under-employed' = 'share_uemp',
         '% over-qualified' = 'share_overed',
         '% fixed term contract' = 'share_temp',
         '% population' = 'share')

write.csv(fig8_ready, paste0(output_fp, 'fig8_education_occupation.csv'))



### Fig 9 ###
fig9_raw <- read.csv(paste0(cur_date, 'Results/h3_2/earnings_potential_immigrants_2020.csv'))

fig9_ready <- fig9_raw %>%
  dplyr::select(country, basic_wage_diff_mean, educ_wage_diff_mean, uemp_penalty, overed_penalty,
                starts_with('macro')) %>%
  mutate(uemp_penalty = uemp_penalty *-1,
         overed_penalty = overed_penalty * -1,
         macro_potential_basic_share_gdp = macro_potential_basic_share_gdp * 100,
         macro_potential_educ_share_gdp = macro_potential_educ_share_gdp * 100,
         macro_potential_uemp_share_gdp = macro_potential_uemp_share_gdp * 100,
         macro_potential_overed_share_gdp = macro_potential_overed_share_gdp * 100) %>%
  pivot_longer(!country, names_to = 'names', values_to = 'vals') %>%
  mutate(group = case_when(names %in% c('basic_wage_diff_mean', 'educ_wage_diff_mean', 'uemp_penalty', 'overed_penalty') ~ 'individual earnings differences (EURO)',
                           names %in% c('macro_potential_basic', 'macro_potential_uemp', 'macro_potential_educ', 'macro_potential_overed') ~ 'national lost wages (EURO)',
                           names %in% c('macro_potential_basic_share_gdp', 'macro_potential_educ_share_gdp', 'macro_potential_uemp_share_gdp', 'macro_potential_overed_share_gdp') ~ 'lost GDP (%)',
                           .default = NA),
         model = case_when(grepl('basic', names) ~ 'basic wage model',
                           grepl('educ', names) ~ 'wage model controlling for education',
                           grepl('uemp', names) & group == 'individual earnings differences (EURO)' ~ 'under-employment penalty',
                           grepl('uemp', names) ~ 'immigrant % under-employment declines to native % under-employment',
                           grepl('overed', names) & group == 'individual earnings differences (EURO)'~ 'over-qualification penalty',
                           grepl('overed', names) ~ 'immigrant % over-qualification declines to native % over-qualification',
                           .default = NA)) %>%
  left_join(countries_match, by = c('country' = 'COUNTRY'))

write.csv(fig9_ready, paste0(output_fp, 'fig9_econ_potential.csv'))  

### Fig 10 ###
fig_10_descr_raw <- read.csv(paste0(cur_date, 'Results/h4/educated_abroad_desc.csv'))
fig_10_regr_raw <- read.csv(paste0(cur_date, 'Results/h4/educated_abroad_regr.csv'))

fig_10_regr <- fig_10_regr_raw %>%
  dplyr::select(term, estimate, p.value, COUNTRY, dv) %>%
  mutate(dv = case_when(dv == 'overed_1sd_hat_isced' ~ 'overed',
                        dv == 'is_unemployed' ~ 'unemp',
                        .default = dv))

fig_10_ready <- fig_10_descr_raw %>%
  dplyr::select(!X) %>%
  filter(!is.na(educated_abroad_factor), !(COUNTRY != 'EU' & n < 50)) %>%
  pivot_longer(cols = ends_with('mean'), names_to = 'metric', values_to = 'descriptive_mean') %>%
  mutate(metric = str_replace(metric, '_mean', '')) %>%
  left_join(fig_10_regr, by = c('educated_abroad_factor' = 'term', 'metric' = 'dv', 'COUNTRY')) %>%
  mutate(significance = case_when(p.value < 0.05 & (sign(descriptive_mean) == sign(estimate)) ~ 'difference from natives statistically significant',
                                  p.value < 0.05 & (sign(descriptive_mean) != sign(estimate)) ~ 'difference from natives statistically significant, but sign changed',
                                  p.value >= 0.05 ~ 'difference from natives not statistically significant',
                                  .default = NA),
         descriptive_mean = descriptive_mean *100,
         estimate = estimate*100,
         metric = case_when(metric == 'uemp' ~ '% under-employed',
                            metric == 'overed' ~ '% over-qualified',
                            metric == 'unemp' ~ '% unemployed',
                            .default = NA)
         ) %>%
  pivot_wider(names_from = 'educated_abroad_factor', values_from = c('descriptive_mean', 'estimate', 'p.value', 'significance', 'n')) %>%
  mutate(descr_diff_immigrant_host_educ_native = descriptive_mean_immigrant_host_educ - descriptive_mean_native,
         descr_diff_immigrant_send_educ_native = descriptive_mean_immigrant_send_educ - descriptive_mean_native,
         desc_diff_immigr_send_educ_immigr_host_educ = descriptive_mean_immigrant_send_educ - descriptive_mean_immigrant_host_educ) %>%
  rename('natives' = 'descriptive_mean_native',
         'immigrants educated in host country' = 'descriptive_mean_immigrant_host_educ',
         'immigrants educated abroad' = 'descriptive_mean_immigrant_send_educ',
         'Difference natives vs. immigrants educated in host country' = 'significance_immigrant_host_educ',
         'Difference natives vs. immigrants educated abroad' = 'significance_immigrant_send_educ',) %>%
  left_join(countries_match, by = 'COUNTRY') %>%
  left_join(order_df, by = c('metric' = 'metric_long')) %>%
  arrange(order)

write.csv(fig_10_ready, paste0(output_fp, 'fig10_educated_abroad.csv'))  
  
### Fig 11 ###
fig_11_descr_raw <- read.csv(paste0(cur_date, 'Results/h4/host_lang_desc.csv'))
fig_11_regr_raw <- read.csv(paste0(cur_date, 'Results/h4/host_lang_regr.csv'))

fig_11_regr <- fig_11_regr_raw %>%
  dplyr::select(term, estimate, p.value, COUNTRY, dv) %>%
  mutate(dv = case_when(dv == 'overed_1sd_hat_isced' ~ 'overed',
                        dv == 'is_unemployed' ~ 'unemp',
                        .default = dv))

fig_11_ready <- fig_11_descr_raw %>%
  dplyr::select(!X) %>%
  filter(!is.na(host_lang_factor),!(COUNTRY != 'EU' & n < 50)) %>%
  pivot_longer(cols = ends_with('mean'), names_to = 'metric', values_to = 'descriptive_mean') %>%
  mutate(metric = str_replace(metric, '_mean', '')) %>%
  left_join(fig_11_regr, by = c('host_lang_factor' = 'term', 'metric' = 'dv', 'COUNTRY')) %>%
  mutate(significance = case_when(p.value < 0.05 & (sign(descriptive_mean) == sign(estimate)) ~ 'difference from natives statistically significant',
                                  p.value < 0.05 & (sign(descriptive_mean) != sign(estimate)) ~ 'difference from natives statistically significant, but sign changed',
                                  p.value >= 0.05 ~ 'difference from natives not statistically significant',
                                  .default = NA),
         descriptive_mean = descriptive_mean *100,
         estimate = estimate*100,
         metric = case_when(metric == 'uemp' ~ '% under-employed',
                            metric == 'overed' ~ '% over-qualified',
                            metric == 'unemp' ~ '% unemployed',
                            .default = NA)
  ) %>%
  pivot_wider(names_from = 'host_lang_factor', values_from = c('descriptive_mean', 'estimate', 'p.value', 'significance', 'n')) %>%
  mutate(descr_diff_immigrant_prof_natives = descriptive_mean_immigrant_prof - descriptive_mean_native,
         descr_diff_immigrant_some_natives = descriptive_mean_immigrant_some - descriptive_mean_native,
         descr_diff_immigrant_some_immigrant_prof = descriptive_mean_immigrant_some - descriptive_mean_immigrant_prof) %>%
  rename('natives' = 'descriptive_mean_native',
         'immigrants, unknown language skills' = 'descriptive_mean_immigrant_NA',
         'immigrants, proficient in host language' = 'descriptive_mean_immigrant_prof',
         'immigrants, poor host language skills' = 'descriptive_mean_immigrant_some',
         'Difference natives vs. immigrants, unknown language skills' = 'significance_immigrant_NA',
         'Difference natives vs. immigrants, proficient in host language' = 'significance_immigrant_prof',
         'Difference natives vs. immigrants, poor host language skills' = 'significance_immigrant_some') %>%
  left_join(countries_match, by = 'COUNTRY') %>%
  left_join(order_df, by = c('metric' = 'metric_long')) %>%
  arrange(order)

write.csv(fig_11_ready, paste0(output_fp, 'fig11_host_lang.csv'))  

### Fig 11a ###
fig_11a_descr_raw <- read.csv(paste0(cur_date, 'Results/h4/langcour_desc.csv'))
fig_11a_regr_raw <- read.csv(paste0(cur_date, 'Results/h4/langcour_regr.csv'))

fig_11a_regr <- fig_11a_regr_raw %>%
  dplyr::select(term, estimate, p.value, COUNTRY, dv) %>%
  mutate(dv = case_when(dv == 'overed_1sd_hat_isced' ~ 'overed',
                        dv == 'is_unemployed' ~ 'unemp',
                        .default = dv))

fig_11a_ready <- fig_11a_descr_raw %>%
  dplyr::select(!X) %>%
  filter(!is.na(langcour_factor),!(COUNTRY != 'EU' & n < 50)) %>%
  pivot_longer(cols = ends_with('mean'), names_to = 'metric', values_to = 'descriptive_mean') %>%
  mutate(metric = str_replace(metric, '_mean', '')) %>%
  left_join(fig_11a_regr, by = c('langcour_factor' = 'term', 'metric' = 'dv', 'COUNTRY')) %>%
  mutate(significance = case_when(p.value < 0.05 & (sign(descriptive_mean) == sign(estimate)) ~ 'difference from natives statistically significant',
                                  p.value < 0.05 & (sign(descriptive_mean) != sign(estimate)) ~ 'difference from natives statistically significant, but sign changed',
                                  p.value >= 0.05 ~ 'difference from natives not statistically significant',
                                  .default = NA),
         descriptive_mean = descriptive_mean *100,
         estimate = estimate*100,
         metric = case_when(metric == 'uemp' ~ '% under-employed',
                            metric == 'overed' ~ '% over-qualified',
                            metric == 'unemp' ~ '% unemployed',
                            .default = NA)
  ) %>%
  pivot_wider(names_from = 'langcour_factor', values_from = c('descriptive_mean', 'estimate', 'p.value', 'significance', 'n')) %>%
  rename('natives' = 'descriptive_mean_native',
         'immigrants, no language course because sufficient skills' = 'descriptive_mean_immigr_langcour_sufficient',
         'immigrants, no language course' = 'descriptive_mean_immigr_no_langcour',
         'immigrants, took language course' = 'descriptive_mean_immigr_yes_langcour',
         'Difference natives vs. immigrants, no language course because sufficient skills' = 'significance_immigr_langcour_sufficient',
         'Difference natives vs. immigrants, no language course' = 'significance_immigr_no_langcour',
         'Difference natives vs. immigrants, took language course' = 'significance_immigr_yes_langcour') %>%
  left_join(countries_match, by = 'COUNTRY') %>%
  left_join(order_df, by = c('metric' = 'metric_long')) %>%
  arrange(order)

write.csv(fig_11a_ready, paste0(output_fp, 'fig11a_langcour.csv'))  



### Fig 12 ###
fig_12_descr_raw <- read.csv(paste0(cur_date, 'Results/h4/residence_desc.csv'))
fig_12_regr_raw <- read.csv(paste0(cur_date, 'Results/h4/residence_regr.csv'))

fig_12_regr <- fig_12_regr_raw %>%
  dplyr::select(term, estimate, p.value, COUNTRY, dv) %>%
  mutate(dv = case_when(dv == 'overed_1sd_hat_isced' ~ 'overed',
                        dv == 'is_unemployed' ~ 'unemp',
                        .default = dv))

fig_12_ready <- fig_12_descr_raw %>%
  dplyr::select(!X) %>%
  filter(!is.na(yearesid_10_years_factor), !(COUNTRY != 'EU' & n < 50)) %>%
  pivot_longer(cols = ends_with('mean'), names_to = 'metric', values_to = 'descriptive_mean') %>%
  mutate(metric = str_replace(metric, '_mean', '')) %>%
  left_join(fig_12_regr, by = c('yearesid_10_years_factor' = 'term', 'metric' = 'dv', 'COUNTRY')) %>%
  mutate(significance = case_when(p.value < 0.05 & (sign(descriptive_mean) == sign(estimate)) ~ 'difference from natives statistically significant',
                                  p.value < 0.05 & (sign(descriptive_mean) != sign(estimate)) ~ 'difference from natives statistically significant, but sign changed',
                                  p.value >= 0.05 ~ 'difference from natives not statistically significant',
                                  .default = NA),
         descriptive_mean = descriptive_mean *100,
         estimate = estimate*100,
         metric = case_when(metric == 'uemp' ~ '% under-employed',
                            metric == 'overed' ~ '% over-qualified',
                            metric == 'unemp' ~ '% unemployed',
                            .default = NA)
  ) %>%
  pivot_wider(names_from = 'yearesid_10_years_factor', values_from = c('descriptive_mean', 'estimate', 'p.value', 'significance', 'n')) %>%
  mutate(descr_diff_immigr_gt_10_natives = descriptive_mean_immigrant_gt_10 - descriptive_mean_native,
         descr_diff_immigr_lt_10_natives = descriptive_mean_immigrant_lt_10 - descriptive_mean_native,
         descr_diff_immigr_lt_10_immigr_gt_10 = descriptive_mean_immigrant_lt_10 - descriptive_mean_immigrant_gt_10)%>%
  rename('natives' = 'descriptive_mean_native',
         'immigrants, migrated more than 10 years ago' = 'descriptive_mean_immigrant_gt_10',
         'immigrants, migrated less than 10 years ago' = 'descriptive_mean_immigrant_lt_10',
         'Difference natives vs. immigrants, migrated more than 10 years ago' = 'significance_immigrant_gt_10',
         'Difference natives vs. immigrants, migrated less than 10 years ago' = 'significance_immigrant_lt_10') %>%
  left_join(countries_match, by = 'COUNTRY') %>%
  left_join(order_df, by = c('metric' = 'metric_long')) %>%
  arrange(order)

write.csv(fig_12_ready, paste0(output_fp, 'fig12_residence.csv'))  

### Fig 13 ###
fig_13_descr_raw <- read.csv(paste0(cur_date, 'Results/h4/migreas_desc.csv'))
fig_13_regr_raw <- read.csv(paste0(cur_date, 'Results/h4/migreas_regr.csv'))

fig_13_regr <- fig_13_regr_raw %>%
  dplyr::select(term, estimate, p.value, COUNTRY, dv) %>%
  mutate(dv = case_when(dv == 'overed_1sd_hat_isced' ~ 'overed',
                        dv == 'is_unemployed' ~ 'unemp',
                        .default = dv))

fig_13_ready <- fig_13_descr_raw %>%
  dplyr::select(!X) %>%
  filter(!is.na(migreas_factor), !(COUNTRY != 'EU' & n < 50)) %>%
  pivot_longer(cols = ends_with('mean'), names_to = 'metric', values_to = 'descriptive_mean') %>%
  mutate(metric = str_replace(metric, '_mean', '')) %>%
  left_join(fig_13_regr, by = c('migreas_factor' = 'term', 'metric' = 'dv', 'COUNTRY')) %>%
  mutate(significance = case_when(p.value < 0.05 & (sign(descriptive_mean) == sign(estimate)) ~ 'difference from natives statistically significant',
                                  p.value < 0.05 & (sign(descriptive_mean) != sign(estimate)) ~ 'difference from natives statistically significant, but sign changed',
                                  p.value >= 0.05 ~ 'difference from natives not statistically significant',
                                  .default = NA),
         descriptive_mean = descriptive_mean *100,
         estimate = estimate*100,
         metric = case_when(metric == 'uemp' ~ '% under-employed',
                            metric == 'overed' ~ '% over-qualified',
                            metric == 'unemp' ~ '% unemployed',
                            .default = NA)
  ) %>%
  pivot_wider(names_from = 'migreas_factor', values_from = c('descriptive_mean', 'estimate', 'p.value', 'significance', 'n')) %>%
  rename('natives' = 'descriptive_mean_native',
         'immigrants, asylum seekers' = 'descriptive_mean_immigr_asylum',
         'immigrants, education' = 'descriptive_mean_immigr_educ',
         'immigrants, economic' = 'descriptive_mean_immigr_employment',
         'immigrants, family' = 'descriptive_mean_immigr_family',
         'immigrants, other reason' = 'descriptive_mean_immigr_other',
         'Difference natives vs. immigrants, asylum seekers' = 'significance_immigr_asylum',
         'Difference natives vs. immigrants, education' = 'significance_immigr_educ',
         'Difference natives vs. immigrants, economic' = 'significance_immigr_employment',
         'Difference natives vs. immigrants, family' = 'significance_immigr_family',
         'Difference natives vs. immigrants, other reasons' = 'significance_immigr_other') %>%
  left_join(countries_match, by = 'COUNTRY') %>%
  left_join(order_df, by = c('metric' = 'metric_long')) %>%
  arrange(order)

write.csv(fig_13_ready, paste0(output_fp, 'fig13_migreas.csv'))  



### Fig 14 ###
fig_14_descr_raw <- read.csv(paste0(cur_date, 'Results/h4/recognition_desc.csv'))
fig_14_regr_raw <- read.csv(paste0(cur_date, 'Results/h4/recognition_regr.csv'))

fig_14_regr <- fig_14_regr_raw %>%
  dplyr::select(term, estimate, p.value, COUNTRY, dv) %>%
  mutate(dv = case_when(dv == 'overed_1sd_hat_isced' ~ 'overed',
                        dv == 'is_unemployed' ~ 'unemp',
                        .default = dv))

fig_14_ready <- fig_14_descr_raw %>%
  dplyr::select(!X) %>%
  filter(!is.na(received_recognition_factor), !(COUNTRY != 'EU' & n < 50)) %>%
  pivot_longer(cols = ends_with('mean'), names_to = 'metric', values_to = 'descriptive_mean') %>%
  mutate(metric = str_replace(metric, '_mean', '')) %>%
  left_join(fig_14_regr, by = c('received_recognition_factor' = 'term', 'metric' = 'dv', 'COUNTRY')) %>%
  mutate(significance = case_when(p.value < 0.05 & (sign(descriptive_mean) == sign(estimate)) ~ 'difference from natives statistically significant',
                                  p.value < 0.05 & (sign(descriptive_mean) != sign(estimate)) ~ 'difference from natives statistically significant, but sign changed',
                                  p.value >= 0.05 ~ 'difference from natives not statistically significant',
                                  .default = NA),
         descriptive_mean = descriptive_mean *100,
         estimate = estimate*100,
         metric = case_when(metric == 'uemp' ~ '% under-employed',
                            metric == 'overed' ~ '% over-qualified',
                            metric == 'unemp' ~ '% unemployed',
                            .default = NA)
  ) %>%
  pivot_wider(names_from = 'received_recognition_factor', values_from = c('descriptive_mean', 'estimate', 'p.value', 'significance', 'n')) %>%
  mutate(descr_diff_immigrant_no_recog_native = descriptive_mean_immigrant_no_recog - descriptive_mean_native,
         descr_diff_immigrant_yes_recog_native = descriptive_mean_immigrant_yes_recog - descriptive_mean_native,
         descr_diff_immigrant_no_recog_immigrant_yes_recog = descriptive_mean_immigrant_no_recog - descriptive_mean_immigrant_yes_recog) %>%
  rename('natives' = 'descriptive_mean_native',
         'immigrants, unknown recognition' = 'descriptive_mean_immigrant_NA_recog',
         'immigrants, no recognition of foreign credentials' = 'descriptive_mean_immigrant_no_recog',
         'immigrants, received recognition of foreign credentials' = 'descriptive_mean_immigrant_yes_recog',
         'Difference natives vs. immigrants, unknown recognition' = 'significance_immigrant_NA_recog',
         'Difference natives vs. immigrants, no recognition of foreign credentials' = 'significance_immigrant_no_recog',
         'Difference natives vs. immigrants, received recognition of foreign credentials' = 'significance_immigrant_yes_recog') %>%
  left_join(countries_match, by = 'COUNTRY') %>%
  left_join(order_df, by = c('metric' = 'metric_long')) %>%
  arrange(order)

write.csv(fig_14_ready, paste0(output_fp, 'fig14_recognition.csv'))  



### fig 15 ###
fig_15_raw <- data.frame()

#set up fps
input_fp_h2 <- paste0(cur_date, 'Results/h2')

#config fp
var_path <- 'Input Data/Config/vars_h2.xlsx'


#comp_groups
comp_groups <- c('immigr_comp', 'eu_native_comp', 'noneu_native_comp')

var_path <- 'Input Data/Config/vars_h2.xlsx'

treat_vars <- read.xlsx(var_path, sheet = 'treat_vars')

dependent_vars <- c('is_unemployed', 'regulated_profession', 'uemp')


for(country in countries_to_analyze){
  #loop over treatments
  for(i in 1:nrow(treat_vars)){
    treat_row <- treat_vars[i,]
    if(treat_row$country != 'ALL' & treat_row$country != country) next
    treat_var <- treat_row$name
    treat_year <- treat_row$year
    #loop over comparison groups
    for(cur_comp in comp_groups){
      if(treat_row[,cur_comp] == 0) next
      #loop over dependent variables
      for(dv in dependent_vars){
          #set up fp for model files
          fit_fp <- paste0(paste(input_fp_h2, country, treat_var, cur_comp, sep = '/'), '/', dv, '_', as.character(treat_year), '_control.csv')
          #load model file
          fit_df <- try(read.csv(fit_fp))
          if(is(fit_df, 'try-error')) {
            print(paste0(fit_fp, ': File read failed'))
            next
          }
          #only keep relevant rows (interaction of REFYEAR and treatment) and format columns
          fit_df_relevant <- fit_df %>%
            dplyr::filter(grepl(':', term, fixed = T)) %>%
            dplyr::select(term, estimate, p.value, conf.low, conf.high) %>%
            dplyr::rename(p_value = p.value,
                          lower_conf = conf.low,
                          upper_conf = conf.high) %>%
            dplyr::mutate(treat = treat_var,
                          dv = dv,
                          comparison_group = cur_comp,
                          country = country,
                          significance = case_when(p_value < 0.05 ~ 'significant',
                                                   .default = 'not significant'),
                          year = treat_year)
          #store model in mv_df
          fig_15_raw <- fig_15_raw %>%
            bind_rows(fit_df_relevant)
        }
    }
  }
}

fig_15_ready <- fig_15_raw %>%
  filter(!is.na(estimate)) %>%
  mutate(treat = case_when(treat == 'treat_EU08' ~ '2008 EU policy change',
                           treat == 'treat_EU08_automated' ~ '2008 EU policy change, automatic recognition occupations only',
                           treat == 'treat_EU13' ~ '2013 EU policy change',
                           treat == 'treat_country' ~ 'country specific policy change',
                           .default = NA),
         dv = case_when(dv == 'is_unemployed' ~ 'unemployed',
                        dv == 'regulated_profession' ~ 'regulated profession',
                        dv == 'uemp' ~ 'under-employed',
                        .default = NA),
         comparison_group = case_when(comparison_group == 'immigr_comp' ~ 'EU immigrants vs TCN immigrants',
                                      comparison_group == 'eu_native_comp' ~ 'EU immigrants vs natives',
                                      comparison_group == 'noneu_native_comp' ~ 'TCN immigrants vs natives',
                                      .default = NA)) %>%
  rename('treatment x post' = 'estimate',
         'treatment' = 'treat',
         'dependent variable' = 'dv',
         'comparison' = 'comparison_group') %>%
  left_join(countries_match, by = c('country' = 'COUNTRY'))

write.csv(fig_15_ready, paste0(output_fp, 'fig15_policy_impact.csv'))


### Fig 16 ###
fig16_raw <- read.csv(paste0(cur_date, 'Results/h3_1/education_brainwaste_shortage.csv'))

fig16_ready <- fig16_raw %>%
  filter(hatfield_text != 'basic', count_immigrant > 50) %>%
  dplyr::select(COUNTRY, underemployed_immigrant, overed_immigrant, unemployed_immigrant, hatfield_text, shortage_eures) %>%
  pivot_longer(!c(COUNTRY, hatfield_text), names_to = 'name', values_to = 'vals') %>%
  mutate(name = case_when(name == 'underemployed_immigrant' ~ '% under-employed',
                          name == 'overed_immigrant' ~ '% over-qualified',
                          name == 'unemployed_immigrant' ~ '% unemployed',
                          name == 'shortage_eures' ~ '% working in shortage occupation',
                          .default = name)) %>%
  left_join(countries_match, by = 'COUNTRY')

write.csv(fig16_ready, paste0(output_fp, 'fig16_shortage.csv'))

