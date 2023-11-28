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

#set up input and output fps
input_fp <- 'Input Data/ELF_Merged/final/merged_country_final_2006_onwards_'
output_fp <- 'Results/h1/'
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


#loop over countries
for(country in countries_to_analyze){
  print(country)
  #set up country output fp and dir
  output_fp_country <- paste0(output_fp, country, '/')
  dir.create(output_fp_country, showWarnings = F)
  
  #load country data
  country_df <- arrow::read_feather(paste0(input_fp, country, '.feather'))
  
  for(dv in dependent_vars){
    print(paste0('DV: ', dv))
    #set up dv output fp and dir
    output_fp_dv <- paste0(output_fp_country, dv, '/')
    dir.create(output_fp_dv, showWarnings = F)
    
    #loop over independent vars
    for(iv in independent_vars){
      print(paste0('iv: ', iv))
      #set up independent output fp and dir
      output_fp_dv_iv <- paste0(output_fp_dv, iv, '/')
      dir.create(output_fp_dv_iv, showWarnings = F)
      
      #restrict country df to only include immigrants if the independent variable is NA for all non-immigrants
      cur_df <- country_df
      if(iv %in% immigr_vars){
        cur_df <- country_df %>%
          filter(is_immigrant == 1)
        output_fp_dv_iv <- paste0(output_fp_dv_iv, 'IMIGR_ONLY_')
      }
      
      #basic regression analysis: DV ~ IV
      formula_basic <- paste0(dv, ' ~ ', iv)
      fit_basic <- try(speedlm(data = cur_df, formula = formula_basic))
      if(is(fit_basic, 'try-error')) next
      
      #transform fit object to df
      fit_basic_df <- fit_basic %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      
      #save coefficients
      write.csv(fit_basic_df, paste0(output_fp_dv_iv, 'basic.csv'))
      # #clean memory
      # remove(fit_basic)
      # gc()
      
      #regression analysis including personal controls: DV ~ IV + personal_controls_basic
      formula_cotrols_basic <- paste(c(formula_basic, personal_controls_basic), collapse = ' + ')
      fit_controls_basic <- try(speedlm(data = cur_df, formula = formula_cotrols_basic))
      if(is(fit_controls_basic, 'try-error')) next
      
      #transform fit object to df
      fit_controls_basic_df <- fit_controls_basic %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      
      #save coefficients
      write.csv(fit_controls_basic_df, paste0(output_fp_dv_iv, 'controls_basic.csv'))
      # #clean memory
      # remove(fit_controls_basic_df)
      # gc()
      
      #regression analysis including extensive personal controls: DV ~ IV + personal_controls_basic + personal_controls_extensive
      formula_cotrols_extensive <- paste(c(formula_cotrols_basic, personal_countrols_extensive), collapse = ' + ')
      fit_controls_extensive <- try(speedlm(data = cur_df, formula = formula_cotrols_extensive))
      if(is(fit_controls_extensive, 'try-error')) next
      
      #transform fit object to df
      fit_controls_extensive_df <- fit_controls_extensive %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      
      #save coefficients
      write.csv(fit_controls_extensive_df, paste0(output_fp_dv_iv, 'controls_extensive.csv'))
      # #clean memory
      # remove(fit_controls_extensive)
      # gc()
      
      #time fixed effects can only be included if the independent variable is available in multiple years
      if(iv %in% single_year_vars){
        
        #a bit hacky, but allows for the code further down to run smoothly
        formula_year_fe <- formula_cotrols_extensive
      } else {
        #regression analysis including time fixed effects: 
        #DV ~ IV + personal_controls_basic + personal_controls_extensive + time_FEs
        formula_year_fe <- paste(c(formula_cotrols_extensive, time_fe), collapse = ' + ')
        fit_year_fe <- try(speedlm(data = cur_df, formula = formula_year_fe))
        if(is(fit_year_fe, 'try-error')) next
        
        #transform fit object to df
        fit_year_fe_df <- fit_year_fe %>% 
          tidy(conf.int = T, conf.level = 0.95) %>%
          as.data.frame()
        
        #save coefficients
        write.csv(fit_year_fe_df, paste0(output_fp_dv_iv, 'year_fe.csv'))
        # #clean memory
        # remove(fit_year_fe)
        # gc()
      }
      
      
      #Regression analysis with region FEs
      #DV ~ IV + personal_controls_basic + personal_controls_extensive + time_FEs + region_FEs
      formula_year_region_fe <- paste(c(formula_year_fe, region_fe), collapse = ' + ')
      fit_year_region_fe <- try(speedlm(data = cur_df, formula = formula_year_region_fe))
      if(is(fit_year_region_fe, 'try-error')) next
      
      #transform fit object to df
      fit_year_region_fe_df <- fit_year_region_fe %>% 
        tidy(conf.int = T, conf.level = 0.95) %>%
        as.data.frame()
      
      #save coefficients
      write.csv(fit_year_region_fe_df, paste0(output_fp_dv_iv, 'year_region_fe_df.csv'))
      
      # #clean memory
      # remove(fit_year_region_fe)
      # gc()
      
      #loop over interact variables
      for(interact in interact_vars){
        print(paste0('Interact: ', interact))
        #Don't interact independent variable with REFYEAR, if independent variable is available in single year only
        if(iv %in% single_year_vars & interact == 'as.factor(REFYEAR)') next
        
        #Basic interaction regression analysis: DV ~ IV * Interact
        formula_basic_interact <- paste0(dv, ' ~ ', iv, ' * ', interact)
        fit_basic_interact <- try(speedlm(data = cur_df, formula = formula_basic_interact))
        if(is(fit_basic_interact, 'try-error')) next
        
        #transform fit object to df
        fit_basic_interact_df <- fit_basic_interact %>% 
          tidy(conf.int = T, conf.level = 0.95) %>%
          as.data.frame()
        
        #save coefficients
        write.csv(fit_basic_interact_df, paste0(output_fp_dv_iv, 'basic_interact_', interact, '.csv'))
        
        # #clean memory
        # remove(fit_basic_interact)
        # gc()
        
        full_controls <- c()
        #make sure we aren't controlling for time when IV is available in a single year only
        if (iv %in% single_year_vars | interact %in% single_year_vars){
          full_controls <- c(personal_controls_basic, personal_countrols_extensive, region_fe)
        } else {
          full_controls <- c(personal_controls_basic, personal_countrols_extensive, time_fe, region_fe)
        }
        
        #get rid of interacts from controls
        if (interact %in% full_controls){
          full_controls <- full_controls[!full_controls == interact]
        }
        
        #Regression analysis with interaction effects and all controls: DV ~ IV * Interact + Controls + FEs
        formula_full_controls_interact <- paste0(c(formula_basic_interact, full_controls), collapse = ' + ')
        fit_full_control_interact <- try(speedlm(data = cur_df, formula = formula_full_controls_interact))
        if(is(fit_full_control_interact, 'try-error')) next
        
        #transform fit object to df
        fit_full_control_interact_df <- fit_full_control_interact %>% 
          tidy(conf.int = T, conf.level = 0.95) %>%
          as.data.frame()
        
        #save coefficients
        write.csv(fit_full_control_interact_df, paste0(output_fp_dv_iv, 'full_control_interact_', interact, '.csv'))
        
        # #clean memory
        # remove(fit_full_control_interact)
        # gc()
      }
    }
  }
}


