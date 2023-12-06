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

#set up fps
output_fp <- 'Results/visualize/h2/'
dir.create(output_fp, showWarnings = F)
input_fp <- 'Results/h2/'

#function to extract variables from config file
extract_vars <- function(vartype, fp){
  cur_df <- read.xlsx(fp, sheet = vartype) %>%
    mutate(vars_prepped = ifelse(is_cat == 1, paste0('as.factor(', vars, ')'), vars)) %>%
    filter(filter == 0)
  return(cur_df$vars_prepped)
}
#config fp
var_path <- 'Input Data/Config/vars_h2.xlsx'

#get dependent variables
dependent_vars <- extract_vars('dependent_vars', var_path)
#get basic personal control vars
personal_controls_basic <- extract_vars('personal_controls_basic', var_path)
#get region FEs
region_fe <- extract_vars('region_fe', var_path)
#get interaction variables
interact_vars <- extract_vars('interact_vars', var_path)


### Without interactives ###
#treatment variables
treatment_vars <- c('eu08_', 'eu13_', 'treatcountry_')
#comparison groups
comparisons <- c('immigr', 'eu_native', 'noneu_native')
#model types
control_types <- c('basic_', 'controls_')


#set up df to save all models
mv_df <- data.frame(treat = character(),
                    dv = character(),
                    estimate = numeric(),
                    lower_conf = numeric(),
                    upper_conf = numeric(),
                    p_value = numeric(),
                    controls = character(),
                    data_type = character(),
                    significance = character(),
                    country = character())
#loop over countries
for(country in countries_to_analyze){
  #loop over dependent variables
  for(dv in dependent_vars){
    #loop over treatment variables
    for(treat in treatment_vars){
      #loop over comparison groups
      for(comp in comparisons){
        #loop over model types
        for(control in control_types){
          #set up fp for model files
          fit_fp_pattern <- paste(input_fp, country, dv, sep = '/')
          #find all matching model files
          fit_fp_list <- list.files(path = fit_fp_pattern, pattern = paste0(treat, control, comp, '.csv'), full.names = T)
          #next if no matching model file was found
          if(length(fit_fp_list) == 0){
            next
          }else{
            fit_fp <- fit_fp_list[1]
          }
          #load model file
          fit_df <- read.csv(fit_fp)
          
          #only keep relevant rows (interaction of REFYEAR and treatment) and format columns
          fit_df_relevant <- fit_df %>%
            dplyr::filter(grepl(':', term, fixed = T)) %>%
            dplyr::select(term, estimate, p.value, conf.low, conf.high) %>%
            dplyr::rename(p_value = p.value,
                          lower_conf = conf.low,
                          upper_conf = conf.high) %>%
            dplyr::mutate(treat = treat,
                          dv = dv,
                          controls = control,
                          data_type = comp,
                          country = country,
                          significance = case_when(p_value < 0.01 ~ '***',
                                                   p_value < 0.05 ~ '**',
                                                   p_value < 0.1 ~ '*',
                                                   .default = 'not sig')) %>%
            dplyr::select(dplyr::all_of(colnames(mv_df)))
          #store model in mv_df
          mv_df <- bind_rows(mv_df, fit_df_relevant)
        }
      }
    }
  }
  
}

#country level plots
for(cur_country in countries_to_analyze){
  #set up output fp and dir
  output_fp_country <- paste0(output_fp, cur_country, '/')
  dir.create(output_fp_country, showWarnings = F)
  
  #subset by country
  df_country <- mv_df %>%
    filter(country == cur_country)
  
  #plot country level graph
  p <- ggplot(df_country, aes(x = data_type, y = estimate, shape = controls, color = significance))+
    geom_point(size = 2)+
    geom_errorbar(aes(ymin = lower_conf, ymax = upper_conf), width = .2, color = '#A9A9A9')+
    ggh4x::facet_grid2(dv ~ treat, scales = 'free', independent = 'y')+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle(paste0(country, ': Impact of policy changes by comparison group, controls, and labor market outcomes'))
  ggsave(paste0(output_fp_country, 'overview.png'), plot=last_plot(), width = 8, height = 12)
}

#Country comp
output_fp_country_comp <- paste0(output_fp, 'country_comp/')
for(cur_treat in unique(mv_df$treat)){
  for(cur_dv in unique(mv_df$dv)){
    cur_df <- mv_df %>%
      filter(treat == cur_treat, dv == cur_dv)
    
    #plot country level graph
    p <- ggplot(cur_df, aes(x = data_type, y = estimate, shape = controls, color = significance))+
      geom_point(size = 2)+
      geom_errorbar(aes(ymin = lower_conf, ymax = upper_conf), width = .2, color = '#A9A9A9')+
      facet_wrap(. ~ country)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      ggtitle(paste0('Impact of ', cur_treat, ' on ', cur_dv, 'by model and country'))
    ggsave(paste0(output_fp_country_comp, cur_treat, '_', cur_dv, '.png'), plot=last_plot(), width = 20, height = 20)
  }
}

###Interaction effects###
#loop over countries
for(country in countries_to_analyze){
  #set up output fp
  output_fp_country <- paste0(output_fp, country, '/')
  #set up df to save all the models
  mv_df <- data.frame(treat = character(),
                      dv = character(),
                      interact = character(),
                      interact_value = character(),
                      estimate = numeric(),
                      lower_conf = numeric(),
                      upper_conf = numeric(),
                      p_value = numeric(),
                      controls = character(),
                      data_type = character(),
                      significance = character())
  #loop over DVs
  for(dv in dependent_vars){
    #loop over interact_vars
    for(interact in interact_vars){
      #loop over treatment vars
      for(treat in treatment_vars){
        #loop over comparison groups
        for(comp in comparisons){
          #loop over model types
          for(control in control_types){
            #set up fp for model files
            fit_fp_pattern <- paste(input_fp, country, dv, interact, sep = '/')
            #find all model fps
            fit_fp_list <- list.files(path = fit_fp_pattern, pattern = paste0(treat, control, comp, '.csv'), full.names = T)
            #next if no matching fps were found
            if(length(fit_fp_list) == 0){
              next
            }else{
              fit_fp <- fit_fp_list[1]
            }
            #load data
            fit_df <- read.csv(fit_fp)
            #restrict rows to three way interaction effects and format columns
            fit_df_relevant <- fit_df %>%
              dplyr::filter(grepl("post:", term, fixed = T)) %>%
              dplyr::select(term, estimate, p.value, conf.low, conf.high) %>%
              dplyr::rename(p_value = p.value,
                            lower_conf = conf.low,
                            upper_conf = conf.high) %>%
              dplyr::mutate(treat = treat,
                            dv = dv,
                            interact = interact,
                            interact_value = stringr::str_extract(term, paste0('(?<=', escape(interact), ').*?(?=$)')),
                            controls = control,
                            data_type = comp,
                            significance = case_when(p_value < 0.01 ~ '***',
                                                     p_value < 0.05 ~ '**',
                                                     p_value < 0.1 ~ '*',
                                                     .default = 'not sig')) %>%
              dplyr::select(dplyr::all_of(colnames(mv_df))) %>%
              filter(!is.na(estimate))
            #store model in mv_df
            mv_df <- bind_rows(mv_df, fit_df_relevant)
          }
        }
      }
    }
  }
  #loop over interaction variables
  for(interact_var in unique(mv_df$interact)){
    #set up output fp and dir
    output_fp_interact <- paste0(output_fp_country, interact_var, '/')
    dir.create(output_fp_interact, showWarnings = F)
    
    #keep only rows matching current interaction var
    mv_df_interact <- mv_df %>%
      filter(interact == interact_var)
    #loop over data types
    for(data_type_val in unique(mv_df_interact$data_type)){
      #keep only rows matching current data types
      mv_df_data_type <- mv_df_interact %>%
        filter(data_type==data_type_val)
      #plot
      p <- ggplot(mv_df_data_type, aes(x = treat, y = estimate, shape = controls, color = significance))+
        geom_point(size = 2)+
        geom_errorbar(aes(ymin = lower_conf, ymax = upper_conf), width = .2, color = '#A9A9A9')+
        ggh4x::facet_grid2(dv ~ interact_value, scales = 'free', independent = 'y')+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ggtitle(paste0(country, ': Impact of labor market outcome by treatment, ', interact_var, ', and', data_type_val))
      #save
      ggsave(paste0(output_fp_interact, data_type_val, '.png'), plot=p, width = 20, height = 12)
    }
  }
}

