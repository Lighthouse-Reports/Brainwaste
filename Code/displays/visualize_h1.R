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

#set up fps and dir
input_fp <- paste0(cur_date, 'Results/h1')
output_fp <- paste0(cur_date, 'Results/visualize/h1/')
dir.create(output_fp, showWarnings = F)

#function to extract and set up variables from the config sheet
extract_vars <- function(vartype, fp){
  cur_df <- read.xlsx(fp, sheet = vartype) %>%
    #set up factor variables
    mutate(vars_prepped = ifelse(is_cat == 1, paste0('as.factor(', vars, ')'), vars)) %>%
    #remove variables as specified
    filter(filter == 0)
  
  return(cur_df$vars_prepped)
}

#fp for config file
var_path <- 'Input Data/Config/vars_h1.xlsx'

#load dependent vars (labor market outcomes)
dependent_vars <- extract_vars('dependent_vars', var_path)

#load independent variables (immigrant characteristics)
independent_vars <- extract_vars('independent_vars', var_path)

#load interaction variables (additional personal characteristics)
interact_vars <- extract_vars('interact_vars', var_path)

#load basic personal controls
personal_controls_basic <- extract_vars('personal_controls_basic', var_path)

#load more extensive personal controls
personal_countrols_extensive <- extract_vars('personal_countrols_extensive', var_path)

#load regional fixed effects variable
region_fe <- extract_vars('region_fe', var_path)

#load time fixed effects variable
time_fe <- extract_vars('time_fe', var_path)

#load immigr variables
immigr_vars <- extract_vars('immigr_vars', var_path)

#load variables that are only available in single years
single_year_vars <- extract_vars('single_year_vars', var_path)

#set up vector with model types
control_vec_no_interact <- c('basic', 'controls_basic', 'controls_extensive', 'year_fe', 'year_region_fe_df')


#set up data frame to collect results from all models
mv_df <- data.frame(iv = character(),
                    iv_value = character(),
                    dv = character(),
                    estimate = numeric(),
                    lower_conf = numeric(),
                    upper_conf = numeric(),
                    p_value = numeric(),
                    controls = character(),
                    data_type = character(),
                    significance = character(),
                    country = character()
)
#loop over countries
for(country in countries_to_analyze){
  
  #loop over dependent variables
  for(dv in dependent_vars){
    
    #loop over independent variables
    for(ind_v in independent_vars){
      
      #loop over model types
      for(controls in control_vec_no_interact){
        #set up fp pattern
        fit_fp_pattern <- paste(input_fp, country, dv, ind_v, sep = '/')
        #retrieve matching files
        fit_fp_list <- list.files(path = fit_fp_pattern, pattern = paste0('*', controls, '.csv'), full.names = T)
        #next if there is no matching file (can happen if the model couldnt fit, e.g., due to too many NAs)
        if(length(fit_fp_list) == 0){
          next
        }else{
          fit_fp <- fit_fp_list[1]
        }
        
        #load data
        fit_df <- read.csv(fit_fp)
        
        #keep only relevant rows (matching the independent variable) and columns
        fit_df_relevant <- fit_df %>%
          dplyr::filter(grepl(ind_v, term, fixed = T)) %>%
          dplyr::select(term, estimate, p.value, conf.low, conf.high) %>%
          dplyr::rename(iv = term,
                        p_value = p.value,
                        lower_conf = conf.low,
                        upper_conf = conf.high) %>%
          dplyr::mutate(iv_value = sub(pattern = 'as.factor\\(.*\\)', replacement = '', iv), #extract value of iv
                        iv = ind_v,
                        dv = dv,
                        country = country,
                        controls = controls,
                        data_type = case_when(grepl('IMIGR_ONLY', fit_fp) ~ 'IMMIGR_ONLY',
                                              .default = 'FULL'),
                        significance = case_when(p_value < 0.01 ~ '***',
                                                 p_value < 0.05 ~ '**',
                                                 p_value < 0.1 ~ '*',
                                                 .default = 'not sig')) %>%
          dplyr::mutate(iv_value = ifelse(iv_value == iv, NA, iv_value)) %>%
          dplyr::select(dplyr::all_of(colnames(mv_df)))
        #bind rows
        mv_df <- bind_rows(mv_df, fit_df_relevant)
      }
    }
  }
}

#dependent var comparison
print('Dependent var comparison')
for(cur_country in countries_to_analyze){
  print(cur_country)
  output_fp_country <- paste0(output_fp, cur_country, '/')
  dir.create(output_fp_country, showWarnings = F)
  
  df_country <- mv_df %>%
    filter(country == cur_country)
  #loop over IVs (that are factor variables)
  for(cur_iv in unique(df_country$iv)){
    #restrict to a single IV
    cur_df <- df_country %>%
      filter(iv == cur_iv)

    #plot
    p <- ggplot(cur_df, aes(x = controls, y = estimate, color = significance))+
      geom_point(size = 2)+
      geom_errorbar(aes(ymin = lower_conf, ymax = upper_conf), width = .2, color = '#A9A9A9')+
      ggh4x::facet_grid2(dv ~ iv_value, scales = 'free', independent = 'y')+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      ggtitle(paste0(country, ': Impact of ', cur_iv, ' on labor market outcomes by model'))
    ggsave(paste0(output_fp_country, cur_iv, '.png'), plot = last_plot(), height = 15, width = 20)
  }

}

#country comp
output_fp_country_comp <- paste0(output_fp, 'country_comp/')
dir.create(output_fp_country_comp, showWarnings = F)
print('Country comp plots')
for(cur_iv in unique(mv_df$iv)){
  print(paste0('IV: ', cur_iv))
  for(cur_dv in unique(mv_df$dv)){
    cur_df <- mv_df %>%
      filter(iv == cur_iv, dv == cur_dv, controls %in% c('basic', 'year_region_fe_df'))
    p <- ggplot(cur_df, aes(x = iv_value, y = estimate, color = significance, shape = controls))+
      geom_point(size = 2)+
      geom_errorbar(aes(ymin = lower_conf, ymax = upper_conf), width = .2, color = '#A9A9A9')+
      facet_wrap(.~ country)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      ggtitle(paste0('Impact of ', cur_iv, ' on ', cur_dv, ' by model and country'))
    ggsave(paste0(output_fp_country_comp, cur_iv, '_', cur_dv, '.png'), width = 20, height = 20)
  }
}


### Interact models ###
#model types for interact models
control_vec_interact <- c('basic_interact_', 'full_control_interact_')
print('Interact plots')
#loop over countries
for(country in countries_to_analyze){
  print(country)
  #set up output fp
  output_fp_country <- paste0(output_fp, country, '/')
  #set up df to collect coefficients of interest for all models
  mv_df <- data.frame(iv = character(),
                      iv_value = character(),
                      interact = character(),
                      interact_value = character(),
                      dv = character(),
                      estimate = numeric(),
                      lower_conf = numeric(),
                      upper_conf = numeric(),
                      p_value = numeric(),
                      controls = character(),
                      data_type = character(),
                      significance = character()
  )
  #loop over DVs
  for(dv in dependent_vars){
    #loop over IVs
    for(ind_v in independent_vars){
      #loop over interact variables
      for(interact in interact_vars){
        #loop over model types
        for(controls in control_vec_interact){
          #fp pattern
          fit_fp_pattern <- paste(input_fp, country, dv, ind_v, sep = '/')
          #retrieve matching fiels
          fit_fp_list <- list.files(path = fit_fp_pattern, pattern =paste0('*', controls, rex(interact), '.csv'), full.names = T)
          #next if no files are available
          if(length(fit_fp_list) == 0){
            next
          }else{
            fit_fp <- fit_fp_list[1]
          }
          #load data
          fit_df <- read.csv(fit_fp)
          
          #restrict rows to coefficient of interest, and reshape columns
          fit_df_relevant <- fit_df %>%
            dplyr::filter(grepl(paste0(':'), term, fixed = T)) %>%
            dplyr::select(term, estimate, p.value, conf.low, conf.high) %>%
            dplyr::rename(p_value = p.value,
                          lower_conf = conf.low,
                          upper_conf = conf.high) %>%
            dplyr::mutate(iv = ind_v, 
                          iv_value =stringr::str_extract(term, paste0('(?<=', escape(ind_v), ').*?(?=\\:)')), #TODO
                          interact = interact,
                          interact_value = stringr::str_extract(term, paste0('(?<=', escape(interact), ').*?(?=$)')), #TODO
                          dv = dv,
                          controls = controls,
                          data_type = case_when(grepl('IMIGR_ONLY', fit_fp) ~ 'IMMIGR_ONLY',
                                                .default = 'FULL'),
                          significance = case_when(p_value < 0.01 ~ '***',
                                                   p_value < 0.05 ~ '**',
                                                   p_value < 0.1 ~ '*',
                                                   .default = 'not sig')) %>%
            dplyr::mutate(iv_value = ifelse(iv_value == iv, NA, iv_value)) %>%
            dplyr::select(dplyr::all_of(colnames(mv_df)))
          
          #combine results from all models in single df
          mv_df <- bind_rows(mv_df, fit_df_relevant)
        }
      }
    }
  }
  #loop over IVs
  for(ind_v in unique(mv_df$iv)){
    #set up output fp and dir
    output_fp_iv <- paste0(output_fp_country, ind_v, '_interact/')
    dir.create(output_fp_iv, showWarnings = F)
    
    #only keep rows matching current IV
    mv_df_iv <- mv_df %>%
      filter(iv == ind_v)
    
    #loop over interact vars
    for(interact_v in unique(mv_df_iv$interact)){
      #further restrict rows to match the interact_var
      mv_df_interact <- mv_df_iv %>%
        filter(interact == interact_v)
      #TODO: potentially as numeric interact value and order
      #plot
      p <- ggplot(mv_df_interact, aes(x = iv_value, y = estimate, color = significance, shape = controls))+
        geom_point(position = 'dodge', size = 2)+
        geom_errorbar(aes(ymin = lower_conf, ymax = upper_conf), width = .2, color = '#A9A9A9')+
        ggh4x::facet_grid2(dv ~ interact_value, scales = 'free', independent = 'y')+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ggtitle(paste0(country, ': Impact of ', ind_v, ' interacted with ', interact_v,' on labor market outcomes by model'))
      
      ggsave(paste0(output_fp_iv, interact_v, '.png'), plot = last_plot(), height = 15, width = 24)
  }
  }
}


