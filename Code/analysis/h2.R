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
output_fp <- 'Results/h2/'
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

#get dependent variables
dependent_vars <- extract_vars('dependent_vars', var_path)
#get basic personal control vars
personal_controls_basic <- extract_vars('personal_controls_basic', var_path)
#get region FEs
region_fe <- extract_vars('region_fe', var_path)
#get interaction variables
interact_vars <- extract_vars('interact_vars', var_path)

#EU countries
EU_countries <- c('EUR_NEU28_NEFTA', 'EU15', 'NMS10', 'EUR_NEU27_2020_NEFTA', 'EU27_2020')
#non EU countries
non_EU_countries <- c('EFTA', 'AME_C_CRB', 'AME_S', 'AME_N', 'ASI_SSE', 'ASI_E',
                      'ASI_NME', 'OCE')

#loop over countries
for(country in countries_to_analyze){
  print(country)
  #load country df
  country_df <- arrow::read_feather(paste0(input_fp, country, '.feather'))
  
  #create output fp and dir
  output_fp_country <- paste0(output_fp, country, '/')
  dir.create(output_fp_country, showWarnings = F)
  
  ### Set up dfs for various comparison groups ###
  #set up immigrant only df
  immigr_only_df <- country_df %>%
    filter(is_immigrant == 1)
  
  #set up df with only EU immigrants and natives
  eu_native_df <- country_df %>%
    filter(COUNTRYB %in% c(EU_countries, 'NAT'))
  
  #set up df with only non-EU immigrants and natives
  noneu_native_df <- country_df %>%
    filter(COUNTRYB %in% c(non_EU_countries, 'NAT'))
  
  #loop over dependent variables
  for(dv in dependent_vars){
    print(dv)
    #set up output fp and dir
    output_fp_dv <- paste0(output_fp_country, dv, '/')
    dir.create(output_fp_dv, showWarnings = F)
    
    ### BASIC ###
    #Immigrants only
    #set up formula
    formula_eu08_basic <- paste0(dv, ' ~ ', 'treat_EU08 * treat_EU08_post')
    #run regression
    fit_eu08_basic_immigr <- try(lm(formula = formula_eu08_basic, data = immigr_only_df))
    if(is(fit_eu08_basic_immigr, 'try-error')) next
    #transform fit to df
    fit_eu08_basic_immigr_df <- fit_eu08_basic_immigr %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame()
    #save coefficients
    write.csv(fit_eu08_basic_immigr_df, paste0(output_fp_dv, 'eu08_basic_immigr.csv'))
    #clean memory
    remove(fit_eu08_basic_immigr_df)
    gc()
    
    #EU-native comparison
    #run regression
    fit_eu08_basic_eu_native <- try(lm(formula = formula_eu08_basic, data = eu_native_df))
    if(is(fit_eu08_basic_eu_native, 'try-error')) next
    #transform fit to df
    fit_eu08_basic_eu_native_df <- fit_eu08_basic_eu_native %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame()
    #save coefficients
    write.csv(fit_eu08_basic_eu_native_df, paste0(output_fp_dv, 'eu08_basic_eu_native.csv'))
    remove(fit_eu08_basic_eu_native_df)
    gc()
    
    #Immigrants only
    #set up formula
    #run regression
    formula_eu13_basic <- paste0(dv, ' ~ ', 'treat_EU13 * treat_EU13_post')
    fit_eu13_basic_immigr <- try(lm(formula = formula_eu13_basic, data = immigr_only_df))
    if(is(fit_eu13_basic_immigr, 'try-error')) next
    #transform fit to df
    fit_eu13_basic_immigr_df <- fit_eu13_basic_immigr %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame()
    #save coefficients
    write.csv(fit_eu13_basic_immigr_df, paste0(output_fp_dv, 'eu13_basic_immigr.csv'))
    remove(fit_eu13_basic_immigr)
    gc()
    
    #EU-native comparison
    #run regression
    fit_eu13_basic_eu_native <- try(lm(formula = formula_eu13_basic, data = eu_native_df))
    if(is(fit_eu13_basic_eu_native, 'try-error')) next
    #transform fit to df
    fit_eu13_basic_eu_native_df <- fit_eu13_basic_eu_native %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame()
    #save coefficients
    write.csv(fit_eu13_basic_eu_native_df, paste0(output_fp_dv, 'eu13_basic_eu_native.csv'))
    remove(fit_eu13_basic_eu_native)
    gc()
    
    ### CONTROLS
    #Immigrants only
    #set up formula
    formula_eu08_controls <- paste(c(formula_eu08_basic, personal_controls_basic), collapse = ' + ')
    #run regression
    fit_eu08_controls_immigr <- try(lm(formula = formula_eu08_controls, data = immigr_only_df))
    if(is(fit_eu08_controls_immigr, 'try-error')) next
    #transform fit to df
    fit_eu08_controls_immigr_df <- fit_eu08_controls_immigr %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame()
    #save coefficients
    write.csv(fit_eu08_controls_immigr_df, paste0(output_fp_dv, 'eu08_controls_immigr.csv'))
    remove(fit_eu08_controls_immigr_df)
    gc()
    
    #EU-native comparison
    #run regression
    fit_eu08_controls_eu_native <- try(lm(formula = formula_eu08_controls, data = eu_native_df))
    if(is(fit_eu08_controls_eu_native, 'try-error')) next
    #transform fit to df
    fit_eu08_controls_eu_native_df <- fit_eu08_controls_eu_native %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame()
    #save coefficients
    write.csv(fit_eu08_controls_eu_native_df, paste0(output_fp_dv, 'eu08_controls_eu_native.csv'))
    remove(fit_eu08_controls_eu_native_df)
    gc()
    
    #Immigrants only
    #set up formula
    formula_eu13_controls <- paste(c(formula_eu13_basic, personal_controls_basic), collapse = ' + ')
    #run regression
    fit_eu13_controls_immigr <- try(lm(formula = formula_eu13_controls, data = immigr_only_df))
    if(is(fit_eu13_controls_immigr, 'try-error')) next
    #transform fit to df
    fit_eu13_controls_immigr_df <- fit_eu13_controls_immigr %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame()
    #save coefficients
    write.csv(fit_eu13_controls_immigr_df, paste0(output_fp_dv, 'eu13_controls_immigr.csv'))
    remove(fit_eu13_controls_immigr)
    gc()
    
    #EU-native comparison
    #run regression
    fit_eu13_controls_eu_native <- try(lm(formula = formula_eu13_controls, data = eu_native_df))
    if(is(fit_eu13_controls_eu_native, 'try-error')) next
    #transform fit to df
    fit_eu13_controls_eu_native_df <- fit_eu13_controls_eu_native %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame()
    #save coefficients
    write.csv(fit_eu13_controls_eu_native_df, paste0(output_fp_dv, 'eu13_controls_eu_native.csv'))
    remove(fit_eu13_controls_eu_native)
    gc()
    
    ### region_fe
    #Immigrants only
    #set up formula
    formula_eu08_region_fe <- paste(c(formula_eu08_controls, region_fe), collapse = ' + ')
    #run regression
    fit_eu08_region_fe_immigr <- try(lm(formula = formula_eu08_region_fe, data = immigr_only_df))
    if(is(fit_eu08_region_fe_immigr, 'try-error')) next
    #transform fit to df
    fit_eu08_region_fe_immigr_df <- fit_eu08_region_fe_immigr %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame()
    #save coefficients
    write.csv(fit_eu08_region_fe_immigr_df, paste0(output_fp_dv, 'eu08_region_fe_immigr.csv'))
    remove(fit_eu08_region_fe_immigr_df)
    gc()
    
    #EU-native comparison
    #run regression
    fit_eu08_region_fe_eu_native <- try(lm(formula = formula_eu08_region_fe, data = eu_native_df))
    if(is(fit_eu08_region_fe_eu_native, 'try-error')) next
    #transform fit to df
    fit_eu08_region_fe_eu_native_df <- fit_eu08_region_fe_eu_native %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame()
    #save coefficients
    write.csv(fit_eu08_region_fe_eu_native_df, paste0(output_fp_dv, 'eu08_region_fe_eu_native.csv'))
    remove(fit_eu08_region_fe_eu_native_df)
    gc()
    
    #Immigrants only
    #set up formula
    formula_eu13_region_fe <- paste(c(formula_eu13_controls, region_fe), collapse = ' + ')
    fit_eu13_region_fe_immigr <- try(lm(formula = formula_eu13_region_fe, data = immigr_only_df))
    if(is(fit_eu13_region_fe_immigr, 'try-error')) next
    #transform fit to df
    fit_eu13_region_fe_immigr_df <- fit_eu13_region_fe_immigr %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame()
    #save coefficients
    write.csv(fit_eu13_region_fe_immigr_df, paste0(output_fp_dv, 'eu13_region_fe_immigr.csv'))
    remove(fit_eu13_region_fe_immigr)
    gc()
    
    #EU-native comparison
    #run regression
    fit_eu13_region_fe_eu_native <- try(lm(formula = formula_eu13_region_fe, data = eu_native_df))
    if(is(fit_eu13_region_fe_eu_native, 'try-error')) next
    #transform fit to df
    fit_eu13_region_fe_eu_native_df <- fit_eu13_region_fe_eu_native %>% 
      tidy(conf.int = T, conf.level = 0.95) %>%
      as.data.frame()
    #save coefficients
    write.csv(fit_eu13_region_fe_eu_native_df, paste0(output_fp_dv, 'eu13_region_fe_eu_native.csv'))
    remove(fit_eu13_region_fe_eu_native)
    gc()
    
    #Run only if country specific treatment variable is available
    if(sum(country_df$treat_country, na.rm = T) > 0){
      ###BASIC###
      #set up formula
      formula_treatcountry_basic <- paste0(dv, ' ~ ', 'treat_country * treat_country_post')
      #run regression
      fit_treatcountry_basic_eu_native <- try(lm(formula = formula_treatcountry_basic, data = eu_native_df))
      if(is(fit_treatcountry_basic_eu_native, 'try-error')) next
      #transform fit to df
      fit_treatcountry_basic_eu_native_df <- fit_treatcountry_basic_eu_native %>%
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_treatcountry_basic_eu_native_df, paste0(output_fp_dv, 'treatcountry_basic_eu_native.csv'))
      remove(fit_treatcountry_basic_eu_native)
      gc()
      
      #Non-EU native comparison
      #run regression
      fit_treatcountry_basic_noneu_native <- try(lm(formula = formula_treatcountry_basic, data = noneu_native_df))
      if(is(fit_treatcountry_basic_noneu_native, 'try-error')) next
      #transform fit to df
      fit_treatcountry_basic_noneu_native_df <- fit_treatcountry_basic_noneu_native %>%
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_treatcountry_basic_noneu_native_df, paste0(output_fp_dv, 'treatcountry_basic_noneu_native.csv'))
      remove(fit_treatcountry_basic_noneu_native)
      gc()
      
      ###controls###
      #set up formula
      formula_treatcountry_controls <- paste(c(formula_treatcountry_basic, personal_controls_basic), collapse = ' + ')
      #run regression
      fit_treatcountry_controls_eu_native <- try(lm(formula = formula_treatcountry_controls, data = eu_native_df))
      if(is(fit_treatcountry_controls_eu_native, 'try-error')) next
      #transform fit to df
      fit_treatcountry_controls_eu_native_df <- fit_treatcountry_controls_eu_native %>%
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_treatcountry_controls_eu_native_df, paste0(output_fp_dv, 'treatcountry_controls_eu_native.csv'))
      remove(fit_treatcountry_controls_eu_native)
      gc()
      
      #Non-EU native comparison
      #run regression
      fit_treatcountry_controls_noneu_native <- try(lm(formula = formula_treatcountry_controls, data = noneu_native_df))
      if(is(fit_treatcountry_controls_noneu_native, 'try-error')) next
      #transform fit to df
      fit_treatcountry_controls_noneu_native_df <- fit_treatcountry_controls_noneu_native %>%
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_treatcountry_controls_noneu_native_df, paste0(output_fp_dv, 'treatcountry_controls_noneu_native.csv'))
      remove(fit_treatcountry_controls_noneu_native)
      gc()
      
      ###REGION FEs###
      #set up formula
      formula_treatcountry_region_fe <- paste(c(formula_treatcountry_controls, region_fe), collapse = ' + ')
      #run regression
      fit_treatcountry_region_fe_eu_native <- try(lm(formula = formula_treatcountry_region_fe, data = eu_native_df))
      if(is(fit_treatcountry_region_fe_eu_native, 'try-error')) next
      #transform fit to df
      fit_treatcountry_region_fe_eu_native_df <- fit_treatcountry_region_fe_eu_native %>%
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_treatcountry_region_fe_eu_native_df, paste0(output_fp_dv, 'treatcountry_region_fe_eu_native.csv'))
      remove(fit_treatcountry_region_fe_eu_native)
      gc()
      
      #Non-EU native comparison
      #run regression
      fit_treatcountry_region_fe_noneu_native <- try(lm(formula = formula_treatcountry_region_fe, data = noneu_native_df))
      if(is(fit_treatcountry_region_fe_noneu_native, 'try-error')) next
      #transform fit to df
      fit_treatcountry_region_fe_noneu_native_df <- fit_treatcountry_region_fe_noneu_native %>%
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_treatcountry_region_fe_noneu_native_df, paste0(output_fp_dv, 'treatcountry_region_fe_noneu_native.csv'))
      remove(fit_treatcountry_region_fe_noneu_native)
      gc()
    }
    
    #Interaction effects
    for(interact_var in interact_vars){
      print(paste0('Interact: ', interact_var))
      output_fp_interact <- paste0(output_fp_dv, interact_var, '/')
      dir.create(output_fp_interact, showWarnings = F)
      
      ### BASIC ###
      #Immigrants only
      #set up formula
      formula_eu08_basic <- paste0(dv, ' ~ ', 'treat_EU08 * treat_EU08_post', ' * ', interact_var)
      #run regression
      fit_eu08_basic_immigr <- try(lm(formula = formula_eu08_basic, data = immigr_only_df))
      if(is(fit_eu08_basic_immigr, 'try-error')) next
      #transform fit to df
      fit_eu08_basic_immigr_df <- fit_eu08_basic_immigr %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_eu08_basic_immigr_df, paste0(output_fp_interact, 'eu08_basic_immigr.csv'))
      remove(fit_eu08_basic_immigr_df)
      gc()
      
      #EU-native comparison
      #run regression
      fit_eu08_basic_eu_native <- try(lm(formula = formula_eu08_basic, data = eu_native_df))
      if(is(fit_eu08_basic_eu_native, 'try-error')) next
      #transform fit to df
      fit_eu08_basic_eu_native_df <- fit_eu08_basic_eu_native %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_eu08_basic_eu_native_df, paste0(output_fp_interact, 'eu08_basic_eu_native.csv'))
      remove(fit_eu08_basic_eu_native_df)
      gc()
      
      #Immigrants only
      #set up formula
      formula_eu13_basic <- paste0(dv, ' ~ ', 'treat_EU13 * treat_EU13_post', ' * ', interact_var)
      #run regression
      fit_eu13_basic_immigr <- try(lm(formula = formula_eu13_basic, data = immigr_only_df))
      if(is(fit_eu13_basic_immigr, 'try-error')) next
      #transform fit to df
      fit_eu13_basic_immigr_df <- fit_eu13_basic_immigr %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_eu13_basic_immigr_df, paste0(output_fp_interact, 'eu13_basic_immigr.csv'))
      remove(fit_eu13_basic_immigr)
      gc()
      
      #EU-native comparison
      #run regression
      fit_eu13_basic_eu_native <- try(lm(formula = formula_eu13_basic, data = eu_native_df))
      if(is(fit_eu13_basic_eu_native, 'try-error')) next
      #transform fit to df
      fit_eu13_basic_eu_native_df <- fit_eu13_basic_eu_native %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_eu13_basic_eu_native_df, paste0(output_fp_interact, 'eu13_basic_eu_native.csv'))
      remove(fit_eu13_basic_eu_native)
      gc()
      
      ### CONTROLS
      #Immigrants only
      #set up formula
      formula_eu08_controls <- paste(c(formula_eu08_basic, personal_controls_basic), collapse = ' + ')
      #run regression
      fit_eu08_controls_immigr <- try(lm(formula = formula_eu08_controls, data = immigr_only_df))
      if(is(fit_eu08_controls_immigr, 'try-error')) next
      #transform fit to df
      fit_eu08_controls_immigr_df <- fit_eu08_controls_immigr %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_eu08_controls_immigr_df, paste0(output_fp_interact, 'eu08_controls_immigr.csv'))
      remove(fit_eu08_controls_immigr_df)
      gc()
      
      #EU-native comparison
      #run regression
      fit_eu08_controls_eu_native <- try(lm(formula = formula_eu08_controls, data = eu_native_df))
      if(is(fit_eu08_controls_eu_native, 'try-error')) next
      #transform fit to df
      fit_eu08_controls_eu_native_df <- fit_eu08_controls_eu_native %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_eu08_controls_eu_native_df, paste0(output_fp_interact, 'eu08_controls_eu_native.csv'))
      remove(fit_eu08_controls_eu_native_df)
      gc()
      
      #Immigrants only
      #set up formula
      formula_eu13_controls <- paste(c(formula_eu13_basic, personal_controls_basic), collapse = ' + ')
      #run regression
      fit_eu13_controls_immigr <- try(lm(formula = formula_eu13_controls, data = immigr_only_df))
      if(is(fit_eu13_controls_immigr, 'try-error')) next
      #transform fit to df
      fit_eu13_controls_immigr_df <- fit_eu13_controls_immigr %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_eu13_controls_immigr_df, paste0(output_fp_interact, 'eu13_controls_immigr.csv'))
      remove(fit_eu13_controls_immigr)
      gc()
      
      #EU-native comparison
      #run regression
      fit_eu13_controls_eu_native <- try(lm(formula = formula_eu13_controls, data = eu_native_df))
      if(is(fit_eu13_controls_eu_native, 'try-error')) next
      #transform fit to df
      fit_eu13_controls_eu_native_df <- fit_eu13_controls_eu_native %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_eu13_controls_eu_native_df, paste0(output_fp_interact, 'eu13_controls_eu_native.csv'))
      remove(fit_eu13_controls_eu_native)
      gc()
      
      ### region_fe
      #Immigrants only
      #set up formula
      formula_eu08_region_fe <- paste(c(formula_eu08_controls, region_fe), collapse = ' + ')
      #run regression
      fit_eu08_region_fe_immigr <- try(lm(formula = formula_eu08_region_fe, data = immigr_only_df))
      if(is(fit_eu08_region_fe_immigr, 'try-error')) next
      #transform fit to df
      fit_eu08_region_fe_immigr_df <- fit_eu08_region_fe_immigr %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_eu08_region_fe_immigr_df, paste0(output_fp_interact, 'eu08_region_fe_immigr.csv'))
      remove(fit_eu08_region_fe_immigr_df)
      gc()
      
      #EU-native comparison
      #run regression
      fit_eu08_region_fe_eu_native <- try(lm(formula = formula_eu08_region_fe, data = eu_native_df))
      if(is(fit_eu08_region_fe_eu_native, 'try-error')) next
      #transform fit to df
      fit_eu08_region_fe_eu_native_df <- fit_eu08_region_fe_eu_native %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_eu08_region_fe_eu_native_df, paste0(output_fp_interact, 'eu08_region_fe_eu_native.csv'))
      remove(fit_eu08_region_fe_eu_native_df)
      gc()
      
      #Immigrants only
      #set up formula
      formula_eu13_region_fe <- paste(c(formula_eu13_controls, region_fe), collapse = ' + ')
      #run regression
      fit_eu13_region_fe_immigr <- try(lm(formula = formula_eu13_region_fe, data = immigr_only_df))
      if(is(fit_eu13_region_fe_immigr, 'try-error')) next
      #transform fit to df
      fit_eu13_region_fe_immigr_df <- fit_eu13_region_fe_immigr %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_eu13_region_fe_immigr_df, paste0(output_fp_interact, 'eu13_region_fe_immigr.csv'))
      remove(fit_eu13_region_fe_immigr)
      gc()
      
      #EU-native comparison
      #run regression
      fit_eu13_region_fe_eu_native <- try(lm(formula = formula_eu13_region_fe, data = eu_native_df))
      if(is(fit_eu13_region_fe_eu_native, 'try-error')) next
      #transform fit to df
      fit_eu13_region_fe_eu_native_df <- fit_eu13_region_fe_eu_native %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      #save coefficients
      write.csv(fit_eu13_region_fe_eu_native_df, paste0(output_fp_interact, 'eu13_region_fe_eu_native.csv'))
      remove(fit_eu13_region_fe_eu_native)
      gc()
      
      #Run only if country specific treatment variable is available
      if(sum(country_df$treat_country, na.rm = T) > 0){
        ###BASIC###
        #EU-native comparison
        #set up formula
        formula_treatcountry_basic <- paste0(dv, ' ~ ', 'treat_country * treat_country_post', ' * ', interact_var)
        #run regression
        fit_treatcountry_basic_eu_native <- try(lm(formula = formula_treatcountry_basic, data = eu_native_df))
        if(is(fit_treatcountry_basic_eu_native, 'try-error')) next
        #transform fit to df
        fit_treatcountry_basic_eu_native_df <- fit_treatcountry_basic_eu_native %>%
          tidy(conf.int = T, conf.level = 0.95) %>%
          as.data.frame()
        #save coefficients
        write.csv(fit_treatcountry_basic_eu_native_df, paste0(output_fp_interact, 'treatcountry_basic_eu_native.csv'))
        remove(fit_treatcountry_basic_eu_native)
        gc()
        
        #Non-EU native comparison
        #run regression
        fit_treatcountry_basic_noneu_native <- try(lm(formula = formula_treatcountry_basic, data = noneu_native_df))
        if(is(fit_treatcountry_basic_noneu_native, 'try-error')) next
        #transform fit to df
        fit_treatcountry_basic_noneu_native_df <- fit_treatcountry_basic_noneu_native %>%
          tidy(conf.int = T, conf.level = 0.95) %>%
          as.data.frame()
        #save coefficients
        write.csv(fit_treatcountry_basic_noneu_native_df, paste0(output_fp_interact, 'treatcountry_basic_noneu_native.csv'))
        remove(fit_treatcountry_basic_noneu_native)
        gc()
        
        ###controls###
        #EU-native comparison
        #set up formula
        formula_treatcountry_controls <- paste(c(formula_treatcountry_basic, personal_controls_basic), collapse = ' + ')
        #run regression
        fit_treatcountry_controls_eu_native <- try(lm(formula = formula_treatcountry_controls, data = eu_native_df))
        if(is(fit_treatcountry_controls_eu_native, 'try-error')) next
        #transform fit to df
        fit_treatcountry_controls_eu_native_df <- fit_treatcountry_controls_eu_native %>%
          tidy(conf.int = T, conf.level = 0.95) %>%
          as.data.frame()
        #save coefficients
        write.csv(fit_treatcountry_controls_eu_native_df, paste0(output_fp_interact, 'treatcountry_controls_eu_native.csv'))
        remove(fit_treatcountry_controls_eu_native)
        gc()
        
        #Non-EU native comparison
        #run regression
        fit_treatcountry_controls_noneu_native <- try(lm(formula = formula_treatcountry_controls, data = noneu_native_df))
        if(is(fit_treatcountry_controls_noneu_native, 'try-error')) next
        #transform fit to df
        fit_treatcountry_controls_noneu_native_df <- fit_treatcountry_controls_noneu_native %>%
          tidy(conf.int = T, conf.level = 0.95) %>%
          as.data.frame()
        #save coefficients
        write.csv(fit_treatcountry_controls_noneu_native_df, paste0(output_fp_interact, 'treatcountry_controls_noneu_native.csv'))
        remove(fit_treatcountry_controls_noneu_native)
        gc()
        
        ###REGION FEs###
        #EU-native comparison
        #set up formula
        formula_treatcountry_region_fe <- paste(c(formula_treatcountry_controls, region_fe), collapse = ' + ')
        #run regression
        fit_treatcountry_region_fe_eu_native <- try(lm(formula = formula_treatcountry_region_fe, data = eu_native_df))
        if(is(fit_treatcountry_region_fe_eu_native, 'try-error')) next
        #transform fit to df
        fit_treatcountry_region_fe_eu_native_df <- fit_treatcountry_region_fe_eu_native %>%
          tidy(conf.int = T, conf.level = 0.95) %>%
          as.data.frame()
        #save coefficients
        write.csv(fit_treatcountry_region_fe_eu_native_df, paste0(output_fp_interact, 'treatcountry_region_fe_eu_native.csv'))
        remove(fit_treatcountry_region_fe_eu_native)
        gc()
        
        #Non-EU native comparison
        #run regression
        fit_treatcountry_region_fe_noneu_native <- try(lm(formula = formula_treatcountry_region_fe, data = noneu_native_df))
        if(is(fit_treatcountry_region_fe_noneu_native, 'try-error')) next
        #transform fit to df
        fit_treatcountry_region_fe_noneu_native_df <- fit_treatcountry_region_fe_noneu_native %>%
          tidy(conf.int = T, conf.level = 0.95) %>%
          as.data.frame()
        #save coefficients
        write.csv(fit_treatcountry_region_fe_noneu_native_df, paste0(output_fp_interact, 'treatcountry_region_fe_noneu_native.csv'))
        remove(fit_treatcountry_region_fe_noneu_native)
        gc()
      }
    }
  }
}
