#load libraries
library(dplyr)
library(tidyr)
library(readr)
library(feather)
library(openxlsx)
library(stringr)
library(ggplot2)
library(arrow)
library(broom)
library(speedglm)

#set up fps
output_fp <- paste0(cur_date, 'Results/h2/')
dir.create(output_fp, showWarnings = F)
input_fp <- 'Input Data/ELF_Merged/final/merged_country_final_2006_onwards_'

#function to extract variables from config file
extract_vars <- function(vartype, fp){
  cur_df <- read.xlsx(fp, sheet = vartype) %>%
    mutate(vars_prepped = ifelse(is_cat == 1, paste0('as.factor(', vars, ')'), vars)) %>%
    filter(filter == 0)
  return(cur_df$vars_prepped)
}
#config fp
var_path <- 'Input Data/Config/vars_h2.xlsx'

treat_vars <- read.xlsx(var_path, sheet = 'treat_vars')

#get dependent variables
dependent_vars <- extract_vars('dependent_vars', var_path)
#get basic personal control vars
personal_controls_basic <- extract_vars('personal_controls_basic', var_path)
#get region FEs
region_fe <- extract_vars('region_fe', var_path)
#get interaction variables
interact_vars <- extract_vars('interact_vars', var_path)

#EU countries
EU_countries <- c('EFTA', 'EU15', 'EU27_2020', 'EUR_NEU27_2020_NEFTA', 'EUR_NEU28_NEFTA', 'NMS10', 'NMS3', 'NMS13', 'EU28', 'EUR_NEU28', 'EU27_2020', 'EUR_NEU27_2020')

#comp_groups
comp_groups <- c('immigr_comp', 'eu_native_comp', 'noneu_native_comp', 'eu_automated_comp',
                 'eu_native_automated_comp', 'immigr_automated_comp')

for(country in countries_to_analyze){
  print(country)
  #load country df
  country_df <- arrow::read_feather(paste0(input_fp, country, '.feather'))
  output_fp_country <- paste0(output_fp, country, '/')
  dir.create(output_fp_country, showWarnings = F)
  
  #loop over treatments
  for(i in 1:nrow(treat_vars)){
    
    treat_row <- treat_vars[i,]
    if(treat_row$country != 'ALL' & treat_row$country != country) next
    treat_var <- treat_row$name
    print(treat_var)
    
    output_fp_treat <- paste0(output_fp_country, treat_var, '/')
    dir.create(output_fp_treat, showWarnings = F)
    
    #loop over comparison groups
    for(cur_comp in comp_groups){
      print(paste0('Comp Group: ', cur_comp))
      if(treat_row[,cur_comp] == 0) next
      
      output_fp_comp <- paste0(output_fp_treat, cur_comp, '/')
      dir.create(output_fp_comp, showWarnings = F)
      
      #restrict sample
      cur_df <- data.frame()
      if(cur_comp == 'immigr_comp'){
        cur_df <- country_df %>%
          filter(is_immigrant == 1)
      }else if(cur_comp == 'eu_native_comp'){
        cur_df <- country_df %>%
          filter(COUNTRYB %in% c(EU_countries, 'NAT'))
      }else if(cur_comp == 'noneu_native_comp'){
        cur_df <- country_df %>%
          filter(!COUNTRYB %in% c(EU_countries))
      }else if(cur_comp == 'eu_automated_comp'){
        cur_df <- country_df %>%
          filter(COUNTRYB %in% c(EU_countries))
      }else if(cur_comp == 'eu_native_automated_comp'){
        cur_df <- country_df %>%
          filter(treat_EU08_automated == 1 | COUNTRYB == 'NAT')
      }else if(cur_comp == 'immigr_automated_comp'){
        cur_df <- country_df %>%
          filter(treat_EU08_automated == 1 | !(COUNTRYB %in% c(EU_countries, 'NAT')))
      }
      
      #loop over years
      for(year in 2007:2020){
        cur_df <- cur_df %>%
          mutate(post = ifelse(REFYEAR > year, 1, 0))
        
        for(dv in dependent_vars){
          #basic model
          formula_basic <- paste0(dv, ' ~ ', treat_var, ' * post')
          
          #run regression
          fit_basic <- try(lm(formula = formula_basic, data = cur_df))
          if(is(fit_basic, 'try-error')) next
          #transform fit to df
          fit_basic_df <- fit_basic %>% 
            tidy(conf.int = T, conf.level = 0.95) %>%
            as.data.frame()
          
          #save coefficients
          write.csv(fit_basic_df, paste0(output_fp_comp, paste0(dv, '_', as.character(year), '_basic.csv')))
          
          
          #control model
          formula_control <- paste(c(formula_basic, personal_controls_basic), collapse = ' + ')
          
          #run regression
          fit_control <- try(lm(formula = formula_control, data = cur_df))
          if(is(fit_control, 'try-error')) next
          #transform fit to df
          fit_control_df <- fit_control %>% 
            tidy(conf.int = T, conf.level = 0.95) %>%
            as.data.frame()
          
          #save coefficients
          write.csv(fit_control_df, paste0(output_fp_comp, paste0(dv, '_', as.character(year), '_control.csv')))
          
        }
      }
      if(length(interact_vars) != 0){
        cur_df <- cur_df %>%
          mutate(post = ifelse(REFYEAR > treat_row$year, 1, 0))
      }
      for(dv in dependent_vars){
        for(interact_var in interact_vars){
          print(paste0('Interact: ', interact_var))
          
          ### BASIC ###
          #set up formula
          formula_interact_basic <- paste0(dv, ' ~ ', treat_var, ' * post', ' * ', interact_var)
          
          #run regression
          fit_interact_basic <- try(lm(formula = formula_interact_basic, data = cur_df))
          if(is(fit_interact_basic, 'try-error')) next
          #transform fit to df
          fit_interact_basic_df <- fit_interact_basic %>% 
            tidy(conf.int = T, conf.level = 0.95) %>%
            as.data.frame()
          #save coefficients
          write.csv(fit_control_df, paste0(output_fp_comp, paste0(dv, '_', interact_var, '_basic.csv')))
          
          #control model
          formula_interact_control <- paste(c(formula_interact_basic, personal_controls_basic), collapse = ' + ')
          
          #run regression
          fit_interact_control <- try(lm(formula = formula_interact_control, data = cur_df))
          if(is(fit_interact_control, 'try-error')) next
          #transform fit to df
          fit_interact_control_df <- fit_interact_control %>% 
            tidy(conf.int = T, conf.level = 0.95) %>%
            as.data.frame()
          
          #save coefficients
          write.csv(fit_interact_control_df, paste0(output_fp_comp, paste0(dv, '_', interact_var, '_control.csv')))
          
        }
      }
    }
  }
}



### OLD VERSION ###
# 
# 
# #loop over countries
# for(country in countries_to_analyze){
#   print(country)
#   #load country df
#   country_df <- arrow::read_feather(paste0(input_fp, country, '.feather'))
#   
#   #create output fp and dir
#   output_fp_country <- paste0(output_fp, country, '/')
#   dir.create(output_fp_country, showWarnings = F)
#   
#   ### Set up dfs for various comparison groups ###
#   #set up immigrant only df
#   immigr_only_df <- country_df %>%
#     filter(is_immigrant == 1)
#   
#   #set up df with only EU immigrants and natives
#   eu_native_df <- country_df %>%
#     filter(COUNTRYB %in% c(EU_countries, 'NAT'))
#   
#   #set up df with only non-EU immigrants and natives
#   noneu_native_df <- country_df %>%
#     filter(COUNTRYB %in% c(non_EU_countries, 'NAT'))
#   
#   #loop over dependent variables
#   for(dv in dependent_vars){
#     print(dv)
#     #set up output fp and dir
#     output_fp_dv <- paste0(output_fp_country, dv, '/')
#     dir.create(output_fp_dv, showWarnings = F)
#     
#     ### BASIC ###
#     #Immigrants only
#     #set up formula
#     formula_eu08_basic <- paste0(dv, ' ~ ', 'treat_EU08 * treat_EU08_post')
#     #run regression
#     fit_eu08_basic_immigr <- try(lm(formula = formula_eu08_basic, data = immigr_only_df))
#     if(is(fit_eu08_basic_immigr, 'try-error')) next
#     #transform fit to df
#     fit_eu08_basic_immigr_df <- fit_eu08_basic_immigr %>% 
#       tidy(conf.int = T, conf.level = 0.95) %>%
#       as.data.frame()
#     #save coefficients
#     write.csv(fit_eu08_basic_immigr_df, paste0(output_fp_dv, 'eu08_basic_immigr.csv'))
#     #clean memory
#     remove(fit_eu08_basic_immigr_df)
#     gc()
#     
#     #EU-native comparison
#     #run regression
#     fit_eu08_basic_eu_native <- try(lm(formula = formula_eu08_basic, data = eu_native_df))
#     if(is(fit_eu08_basic_eu_native, 'try-error')) next
#     #transform fit to df
#     fit_eu08_basic_eu_native_df <- fit_eu08_basic_eu_native %>% 
#       tidy(conf.int = T, conf.level = 0.95) %>%
#       as.data.frame()
#     #save coefficients
#     write.csv(fit_eu08_basic_eu_native_df, paste0(output_fp_dv, 'eu08_basic_eu_native.csv'))
#     remove(fit_eu08_basic_eu_native_df)
#     gc()
#     
#     #Immigrants only
#     #set up formula
#     #run regression
#     formula_eu13_basic <- paste0(dv, ' ~ ', 'treat_EU13 * treat_EU13_post')
#     fit_eu13_basic_immigr <- try(lm(formula = formula_eu13_basic, data = immigr_only_df))
#     if(is(fit_eu13_basic_immigr, 'try-error')) next
#     #transform fit to df
#     fit_eu13_basic_immigr_df <- fit_eu13_basic_immigr %>% 
#       tidy(conf.int = T, conf.level = 0.95) %>%
#       as.data.frame()
#     #save coefficients
#     write.csv(fit_eu13_basic_immigr_df, paste0(output_fp_dv, 'eu13_basic_immigr.csv'))
#     remove(fit_eu13_basic_immigr)
#     gc()
#     
#     #EU-native comparison
#     #run regression
#     fit_eu13_basic_eu_native <- try(lm(formula = formula_eu13_basic, data = eu_native_df))
#     if(is(fit_eu13_basic_eu_native, 'try-error')) next
#     #transform fit to df
#     fit_eu13_basic_eu_native_df <- fit_eu13_basic_eu_native %>% 
#       tidy(conf.int = T, conf.level = 0.95) %>%
#       as.data.frame()
#     #save coefficients
#     write.csv(fit_eu13_basic_eu_native_df, paste0(output_fp_dv, 'eu13_basic_eu_native.csv'))
#     remove(fit_eu13_basic_eu_native)
#     gc()
#     
#     ### CONTROLS
#     #Immigrants only
#     #set up formula
#     formula_eu08_controls <- paste(c(formula_eu08_basic, personal_controls_basic), collapse = ' + ')
#     #run regression
#     fit_eu08_controls_immigr <- try(lm(formula = formula_eu08_controls, data = immigr_only_df))
#     if(is(fit_eu08_controls_immigr, 'try-error')) next
#     #transform fit to df
#     fit_eu08_controls_immigr_df <- fit_eu08_controls_immigr %>% 
#       tidy(conf.int = T, conf.level = 0.95) %>%
#       as.data.frame()
#     #save coefficients
#     write.csv(fit_eu08_controls_immigr_df, paste0(output_fp_dv, 'eu08_controls_immigr.csv'))
#     remove(fit_eu08_controls_immigr_df)
#     gc()
#     
#     #EU-native comparison
#     #run regression
#     fit_eu08_controls_eu_native <- try(lm(formula = formula_eu08_controls, data = eu_native_df))
#     if(is(fit_eu08_controls_eu_native, 'try-error')) next
#     #transform fit to df
#     fit_eu08_controls_eu_native_df <- fit_eu08_controls_eu_native %>% 
#       tidy(conf.int = T, conf.level = 0.95) %>%
#       as.data.frame()
#     #save coefficients
#     write.csv(fit_eu08_controls_eu_native_df, paste0(output_fp_dv, 'eu08_controls_eu_native.csv'))
#     remove(fit_eu08_controls_eu_native_df)
#     gc()
#     
#     #Immigrants only
#     #set up formula
#     formula_eu13_controls <- paste(c(formula_eu13_basic, personal_controls_basic), collapse = ' + ')
#     #run regression
#     fit_eu13_controls_immigr <- try(lm(formula = formula_eu13_controls, data = immigr_only_df))
#     if(is(fit_eu13_controls_immigr, 'try-error')) next
#     #transform fit to df
#     fit_eu13_controls_immigr_df <- fit_eu13_controls_immigr %>% 
#       tidy(conf.int = T, conf.level = 0.95) %>%
#       as.data.frame()
#     #save coefficients
#     write.csv(fit_eu13_controls_immigr_df, paste0(output_fp_dv, 'eu13_controls_immigr.csv'))
#     remove(fit_eu13_controls_immigr)
#     gc()
#     
#     #EU-native comparison
#     #run regression
#     fit_eu13_controls_eu_native <- try(lm(formula = formula_eu13_controls, data = eu_native_df))
#     if(is(fit_eu13_controls_eu_native, 'try-error')) next
#     #transform fit to df
#     fit_eu13_controls_eu_native_df <- fit_eu13_controls_eu_native %>% 
#       tidy(conf.int = T, conf.level = 0.95) %>%
#       as.data.frame()
#     #save coefficients
#     write.csv(fit_eu13_controls_eu_native_df, paste0(output_fp_dv, 'eu13_controls_eu_native.csv'))
#     remove(fit_eu13_controls_eu_native)
#     gc()
#     
#     ### region_fe
#     #Immigrants only
#     #set up formula
#     formula_eu08_region_fe <- paste(c(formula_eu08_controls, region_fe), collapse = ' + ')
#     #run regression
#     fit_eu08_region_fe_immigr <- try(lm(formula = formula_eu08_region_fe, data = immigr_only_df))
#     if(is(fit_eu08_region_fe_immigr, 'try-error')) next
#     #transform fit to df
#     fit_eu08_region_fe_immigr_df <- fit_eu08_region_fe_immigr %>% 
#       tidy(conf.int = T, conf.level = 0.95) %>%
#       as.data.frame()
#     #save coefficients
#     write.csv(fit_eu08_region_fe_immigr_df, paste0(output_fp_dv, 'eu08_region_fe_immigr.csv'))
#     remove(fit_eu08_region_fe_immigr_df)
#     gc()
#     
#     #EU-native comparison
#     #run regression
#     fit_eu08_region_fe_eu_native <- try(lm(formula = formula_eu08_region_fe, data = eu_native_df))
#     if(is(fit_eu08_region_fe_eu_native, 'try-error')) next
#     #transform fit to df
#     fit_eu08_region_fe_eu_native_df <- fit_eu08_region_fe_eu_native %>% 
#       tidy(conf.int = T, conf.level = 0.95) %>%
#       as.data.frame()
#     #save coefficients
#     write.csv(fit_eu08_region_fe_eu_native_df, paste0(output_fp_dv, 'eu08_region_fe_eu_native.csv'))
#     remove(fit_eu08_region_fe_eu_native_df)
#     gc()
#     
#     #Immigrants only
#     #set up formula
#     formula_eu13_region_fe <- paste(c(formula_eu13_controls, region_fe), collapse = ' + ')
#     fit_eu13_region_fe_immigr <- try(lm(formula = formula_eu13_region_fe, data = immigr_only_df))
#     if(is(fit_eu13_region_fe_immigr, 'try-error')) next
#     #transform fit to df
#     fit_eu13_region_fe_immigr_df <- fit_eu13_region_fe_immigr %>% 
#       tidy(conf.int = T, conf.level = 0.95) %>%
#       as.data.frame()
#     #save coefficients
#     write.csv(fit_eu13_region_fe_immigr_df, paste0(output_fp_dv, 'eu13_region_fe_immigr.csv'))
#     remove(fit_eu13_region_fe_immigr)
#     gc()
#     
#     #EU-native comparison
#     #run regression
#     fit_eu13_region_fe_eu_native <- try(lm(formula = formula_eu13_region_fe, data = eu_native_df))
#     if(is(fit_eu13_region_fe_eu_native, 'try-error')) next
#     #transform fit to df
#     fit_eu13_region_fe_eu_native_df <- fit_eu13_region_fe_eu_native %>% 
#       tidy(conf.int = T, conf.level = 0.95) %>%
#       as.data.frame()
#     #save coefficients
#     write.csv(fit_eu13_region_fe_eu_native_df, paste0(output_fp_dv, 'eu13_region_fe_eu_native.csv'))
#     remove(fit_eu13_region_fe_eu_native)
#     gc()
#     
#     #Run only if country specific treatment variable is available
#     if(sum(country_df$treat_country, na.rm = T) > 0){
#       ###BASIC###
#       #set up formula
#       formula_treatcountry_basic <- paste0(dv, ' ~ ', 'treat_country * treat_country_post')
#       #run regression
#       fit_treatcountry_basic_eu_native <- try(lm(formula = formula_treatcountry_basic, data = eu_native_df))
#       if(is(fit_treatcountry_basic_eu_native, 'try-error')) next
#       #transform fit to df
#       fit_treatcountry_basic_eu_native_df <- fit_treatcountry_basic_eu_native %>%
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_treatcountry_basic_eu_native_df, paste0(output_fp_dv, 'treatcountry_basic_eu_native.csv'))
#       remove(fit_treatcountry_basic_eu_native)
#       gc()
#       
#       #Non-EU native comparison
#       #run regression
#       fit_treatcountry_basic_noneu_native <- try(lm(formula = formula_treatcountry_basic, data = noneu_native_df))
#       if(is(fit_treatcountry_basic_noneu_native, 'try-error')) next
#       #transform fit to df
#       fit_treatcountry_basic_noneu_native_df <- fit_treatcountry_basic_noneu_native %>%
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_treatcountry_basic_noneu_native_df, paste0(output_fp_dv, 'treatcountry_basic_noneu_native.csv'))
#       remove(fit_treatcountry_basic_noneu_native)
#       gc()
#       
#       ###controls###
#       #set up formula
#       formula_treatcountry_controls <- paste(c(formula_treatcountry_basic, personal_controls_basic), collapse = ' + ')
#       #run regression
#       fit_treatcountry_controls_eu_native <- try(lm(formula = formula_treatcountry_controls, data = eu_native_df))
#       if(is(fit_treatcountry_controls_eu_native, 'try-error')) next
#       #transform fit to df
#       fit_treatcountry_controls_eu_native_df <- fit_treatcountry_controls_eu_native %>%
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_treatcountry_controls_eu_native_df, paste0(output_fp_dv, 'treatcountry_controls_eu_native.csv'))
#       remove(fit_treatcountry_controls_eu_native)
#       gc()
#       
#       #Non-EU native comparison
#       #run regression
#       fit_treatcountry_controls_noneu_native <- try(lm(formula = formula_treatcountry_controls, data = noneu_native_df))
#       if(is(fit_treatcountry_controls_noneu_native, 'try-error')) next
#       #transform fit to df
#       fit_treatcountry_controls_noneu_native_df <- fit_treatcountry_controls_noneu_native %>%
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_treatcountry_controls_noneu_native_df, paste0(output_fp_dv, 'treatcountry_controls_noneu_native.csv'))
#       remove(fit_treatcountry_controls_noneu_native)
#       gc()
#       
#       ###REGION FEs###
#       #set up formula
#       formula_treatcountry_region_fe <- paste(c(formula_treatcountry_controls, region_fe), collapse = ' + ')
#       #run regression
#       fit_treatcountry_region_fe_eu_native <- try(lm(formula = formula_treatcountry_region_fe, data = eu_native_df))
#       if(is(fit_treatcountry_region_fe_eu_native, 'try-error')) next
#       #transform fit to df
#       fit_treatcountry_region_fe_eu_native_df <- fit_treatcountry_region_fe_eu_native %>%
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_treatcountry_region_fe_eu_native_df, paste0(output_fp_dv, 'treatcountry_region_fe_eu_native.csv'))
#       remove(fit_treatcountry_region_fe_eu_native)
#       gc()
#       
#       #Non-EU native comparison
#       #run regression
#       fit_treatcountry_region_fe_noneu_native <- try(lm(formula = formula_treatcountry_region_fe, data = noneu_native_df))
#       if(is(fit_treatcountry_region_fe_noneu_native, 'try-error')) next
#       #transform fit to df
#       fit_treatcountry_region_fe_noneu_native_df <- fit_treatcountry_region_fe_noneu_native %>%
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_treatcountry_region_fe_noneu_native_df, paste0(output_fp_dv, 'treatcountry_region_fe_noneu_native.csv'))
#       remove(fit_treatcountry_region_fe_noneu_native)
#       gc()
#     }
#     
#     #Interaction effects
#     for(interact_var in interact_vars){
#       print(paste0('Interact: ', interact_var))
#       output_fp_interact <- paste0(output_fp_dv, interact_var, '/')
#       dir.create(output_fp_interact, showWarnings = F)
#       
#       ### BASIC ###
#       #Immigrants only
#       #set up formula
#       formula_eu08_basic <- paste0(dv, ' ~ ', 'treat_EU08 * treat_EU08_post', ' * ', interact_var)
#       #run regression
#       fit_eu08_basic_immigr <- try(lm(formula = formula_eu08_basic, data = immigr_only_df))
#       if(is(fit_eu08_basic_immigr, 'try-error')) next
#       #transform fit to df
#       fit_eu08_basic_immigr_df <- fit_eu08_basic_immigr %>% 
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_eu08_basic_immigr_df, paste0(output_fp_interact, 'eu08_basic_immigr.csv'))
#       remove(fit_eu08_basic_immigr_df)
#       gc()
#       
#       #EU-native comparison
#       #run regression
#       fit_eu08_basic_eu_native <- try(lm(formula = formula_eu08_basic, data = eu_native_df))
#       if(is(fit_eu08_basic_eu_native, 'try-error')) next
#       #transform fit to df
#       fit_eu08_basic_eu_native_df <- fit_eu08_basic_eu_native %>% 
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_eu08_basic_eu_native_df, paste0(output_fp_interact, 'eu08_basic_eu_native.csv'))
#       remove(fit_eu08_basic_eu_native_df)
#       gc()
#       
#       #Immigrants only
#       #set up formula
#       formula_eu13_basic <- paste0(dv, ' ~ ', 'treat_EU13 * treat_EU13_post', ' * ', interact_var)
#       #run regression
#       fit_eu13_basic_immigr <- try(lm(formula = formula_eu13_basic, data = immigr_only_df))
#       if(is(fit_eu13_basic_immigr, 'try-error')) next
#       #transform fit to df
#       fit_eu13_basic_immigr_df <- fit_eu13_basic_immigr %>% 
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_eu13_basic_immigr_df, paste0(output_fp_interact, 'eu13_basic_immigr.csv'))
#       remove(fit_eu13_basic_immigr)
#       gc()
#       
#       #EU-native comparison
#       #run regression
#       fit_eu13_basic_eu_native <- try(lm(formula = formula_eu13_basic, data = eu_native_df))
#       if(is(fit_eu13_basic_eu_native, 'try-error')) next
#       #transform fit to df
#       fit_eu13_basic_eu_native_df <- fit_eu13_basic_eu_native %>% 
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_eu13_basic_eu_native_df, paste0(output_fp_interact, 'eu13_basic_eu_native.csv'))
#       remove(fit_eu13_basic_eu_native)
#       gc()
#       
#       ### CONTROLS
#       #Immigrants only
#       #set up formula
#       formula_eu08_controls <- paste(c(formula_eu08_basic, personal_controls_basic), collapse = ' + ')
#       #run regression
#       fit_eu08_controls_immigr <- try(lm(formula = formula_eu08_controls, data = immigr_only_df))
#       if(is(fit_eu08_controls_immigr, 'try-error')) next
#       #transform fit to df
#       fit_eu08_controls_immigr_df <- fit_eu08_controls_immigr %>% 
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_eu08_controls_immigr_df, paste0(output_fp_interact, 'eu08_controls_immigr.csv'))
#       remove(fit_eu08_controls_immigr_df)
#       gc()
#       
#       #EU-native comparison
#       #run regression
#       fit_eu08_controls_eu_native <- try(lm(formula = formula_eu08_controls, data = eu_native_df))
#       if(is(fit_eu08_controls_eu_native, 'try-error')) next
#       #transform fit to df
#       fit_eu08_controls_eu_native_df <- fit_eu08_controls_eu_native %>% 
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_eu08_controls_eu_native_df, paste0(output_fp_interact, 'eu08_controls_eu_native.csv'))
#       remove(fit_eu08_controls_eu_native_df)
#       gc()
#       
#       #Immigrants only
#       #set up formula
#       formula_eu13_controls <- paste(c(formula_eu13_basic, personal_controls_basic), collapse = ' + ')
#       #run regression
#       fit_eu13_controls_immigr <- try(lm(formula = formula_eu13_controls, data = immigr_only_df))
#       if(is(fit_eu13_controls_immigr, 'try-error')) next
#       #transform fit to df
#       fit_eu13_controls_immigr_df <- fit_eu13_controls_immigr %>% 
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_eu13_controls_immigr_df, paste0(output_fp_interact, 'eu13_controls_immigr.csv'))
#       remove(fit_eu13_controls_immigr)
#       gc()
#       
#       #EU-native comparison
#       #run regression
#       fit_eu13_controls_eu_native <- try(lm(formula = formula_eu13_controls, data = eu_native_df))
#       if(is(fit_eu13_controls_eu_native, 'try-error')) next
#       #transform fit to df
#       fit_eu13_controls_eu_native_df <- fit_eu13_controls_eu_native %>% 
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_eu13_controls_eu_native_df, paste0(output_fp_interact, 'eu13_controls_eu_native.csv'))
#       remove(fit_eu13_controls_eu_native)
#       gc()
#       
#       ### region_fe
#       #Immigrants only
#       #set up formula
#       formula_eu08_region_fe <- paste(c(formula_eu08_controls, region_fe), collapse = ' + ')
#       #run regression
#       fit_eu08_region_fe_immigr <- try(lm(formula = formula_eu08_region_fe, data = immigr_only_df))
#       if(is(fit_eu08_region_fe_immigr, 'try-error')) next
#       #transform fit to df
#       fit_eu08_region_fe_immigr_df <- fit_eu08_region_fe_immigr %>% 
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_eu08_region_fe_immigr_df, paste0(output_fp_interact, 'eu08_region_fe_immigr.csv'))
#       remove(fit_eu08_region_fe_immigr_df)
#       gc()
#       
#       #EU-native comparison
#       #run regression
#       fit_eu08_region_fe_eu_native <- try(lm(formula = formula_eu08_region_fe, data = eu_native_df))
#       if(is(fit_eu08_region_fe_eu_native, 'try-error')) next
#       #transform fit to df
#       fit_eu08_region_fe_eu_native_df <- fit_eu08_region_fe_eu_native %>% 
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_eu08_region_fe_eu_native_df, paste0(output_fp_interact, 'eu08_region_fe_eu_native.csv'))
#       remove(fit_eu08_region_fe_eu_native_df)
#       gc()
#       
#       #Immigrants only
#       #set up formula
#       formula_eu13_region_fe <- paste(c(formula_eu13_controls, region_fe), collapse = ' + ')
#       #run regression
#       fit_eu13_region_fe_immigr <- try(lm(formula = formula_eu13_region_fe, data = immigr_only_df))
#       if(is(fit_eu13_region_fe_immigr, 'try-error')) next
#       #transform fit to df
#       fit_eu13_region_fe_immigr_df <- fit_eu13_region_fe_immigr %>% 
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_eu13_region_fe_immigr_df, paste0(output_fp_interact, 'eu13_region_fe_immigr.csv'))
#       remove(fit_eu13_region_fe_immigr)
#       gc()
#       
#       #EU-native comparison
#       #run regression
#       fit_eu13_region_fe_eu_native <- try(lm(formula = formula_eu13_region_fe, data = eu_native_df))
#       if(is(fit_eu13_region_fe_eu_native, 'try-error')) next
#       #transform fit to df
#       fit_eu13_region_fe_eu_native_df <- fit_eu13_region_fe_eu_native %>% 
#         tidy(conf.int = T, conf.level = 0.95) %>%
#         as.data.frame()
#       #save coefficients
#       write.csv(fit_eu13_region_fe_eu_native_df, paste0(output_fp_interact, 'eu13_region_fe_eu_native.csv'))
#       remove(fit_eu13_region_fe_eu_native)
#       gc()
#       
#       #Run only if country specific treatment variable is available
#       if(sum(country_df$treat_country, na.rm = T) > 0){
#         ###BASIC###
#         #EU-native comparison
#         #set up formula
#         formula_treatcountry_basic <- paste0(dv, ' ~ ', 'treat_country * treat_country_post', ' * ', interact_var)
#         #run regression
#         fit_treatcountry_basic_eu_native <- try(lm(formula = formula_treatcountry_basic, data = eu_native_df))
#         if(is(fit_treatcountry_basic_eu_native, 'try-error')) next
#         #transform fit to df
#         fit_treatcountry_basic_eu_native_df <- fit_treatcountry_basic_eu_native %>%
#           tidy(conf.int = T, conf.level = 0.95) %>%
#           as.data.frame()
#         #save coefficients
#         write.csv(fit_treatcountry_basic_eu_native_df, paste0(output_fp_interact, 'treatcountry_basic_eu_native.csv'))
#         remove(fit_treatcountry_basic_eu_native)
#         gc()
#         
#         #Non-EU native comparison
#         #run regression
#         fit_treatcountry_basic_noneu_native <- try(lm(formula = formula_treatcountry_basic, data = noneu_native_df))
#         if(is(fit_treatcountry_basic_noneu_native, 'try-error')) next
#         #transform fit to df
#         fit_treatcountry_basic_noneu_native_df <- fit_treatcountry_basic_noneu_native %>%
#           tidy(conf.int = T, conf.level = 0.95) %>%
#           as.data.frame()
#         #save coefficients
#         write.csv(fit_treatcountry_basic_noneu_native_df, paste0(output_fp_interact, 'treatcountry_basic_noneu_native.csv'))
#         remove(fit_treatcountry_basic_noneu_native)
#         gc()
#         
#         ###controls###
#         #EU-native comparison
#         #set up formula
#         formula_treatcountry_controls <- paste(c(formula_treatcountry_basic, personal_controls_basic), collapse = ' + ')
#         #run regression
#         fit_treatcountry_controls_eu_native <- try(lm(formula = formula_treatcountry_controls, data = eu_native_df))
#         if(is(fit_treatcountry_controls_eu_native, 'try-error')) next
#         #transform fit to df
#         fit_treatcountry_controls_eu_native_df <- fit_treatcountry_controls_eu_native %>%
#           tidy(conf.int = T, conf.level = 0.95) %>%
#           as.data.frame()
#         #save coefficients
#         write.csv(fit_treatcountry_controls_eu_native_df, paste0(output_fp_interact, 'treatcountry_controls_eu_native.csv'))
#         remove(fit_treatcountry_controls_eu_native)
#         gc()
#         
#         #Non-EU native comparison
#         #run regression
#         fit_treatcountry_controls_noneu_native <- try(lm(formula = formula_treatcountry_controls, data = noneu_native_df))
#         if(is(fit_treatcountry_controls_noneu_native, 'try-error')) next
#         #transform fit to df
#         fit_treatcountry_controls_noneu_native_df <- fit_treatcountry_controls_noneu_native %>%
#           tidy(conf.int = T, conf.level = 0.95) %>%
#           as.data.frame()
#         #save coefficients
#         write.csv(fit_treatcountry_controls_noneu_native_df, paste0(output_fp_interact, 'treatcountry_controls_noneu_native.csv'))
#         remove(fit_treatcountry_controls_noneu_native)
#         gc()
#         
#         ###REGION FEs###
#         #EU-native comparison
#         #set up formula
#         formula_treatcountry_region_fe <- paste(c(formula_treatcountry_controls, region_fe), collapse = ' + ')
#         #run regression
#         fit_treatcountry_region_fe_eu_native <- try(lm(formula = formula_treatcountry_region_fe, data = eu_native_df))
#         if(is(fit_treatcountry_region_fe_eu_native, 'try-error')) next
#         #transform fit to df
#         fit_treatcountry_region_fe_eu_native_df <- fit_treatcountry_region_fe_eu_native %>%
#           tidy(conf.int = T, conf.level = 0.95) %>%
#           as.data.frame()
#         #save coefficients
#         write.csv(fit_treatcountry_region_fe_eu_native_df, paste0(output_fp_interact, 'treatcountry_region_fe_eu_native.csv'))
#         remove(fit_treatcountry_region_fe_eu_native)
#         gc()
#         
#         #Non-EU native comparison
#         #run regression
#         fit_treatcountry_region_fe_noneu_native <- try(lm(formula = formula_treatcountry_region_fe, data = noneu_native_df))
#         if(is(fit_treatcountry_region_fe_noneu_native, 'try-error')) next
#         #transform fit to df
#         fit_treatcountry_region_fe_noneu_native_df <- fit_treatcountry_region_fe_noneu_native %>%
#           tidy(conf.int = T, conf.level = 0.95) %>%
#           as.data.frame()
#         #save coefficients
#         write.csv(fit_treatcountry_region_fe_noneu_native_df, paste0(output_fp_interact, 'treatcountry_region_fe_noneu_native.csv'))
#         remove(fit_treatcountry_region_fe_noneu_native)
#         gc()
#       }
#     }
#   }
# }
