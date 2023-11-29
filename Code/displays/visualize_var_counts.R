library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(data.table)
library(feather)
library(tidyverse)
#set up output fp
output_fp <- 'Results/visualize/var_counts/'
dir.create(output_fp, showWarnings = F)

#set up input fp
input_fp <- 'Results/Raw_Variable_Counts/'
files <- list.files(input_fp, pattern = 'csv', full.names = T)

#set up master_df
master_df <- data.frame(REFYEAR = numeric(),
                        COUNTRY = character(),
                        n = numeric(),
                        share = numeric(),
                        var = character(),
                        var_value = character())

#loop over all files
for(f in files){
  #read file and merge with master df
  master_df <- read.csv(f) %>%
    mutate(REFYEAR = as.numeric(REFYEAR),
           COUNTRY = as.character(COUNTRY),
           n = as.numeric (n),
           var = as.character(var),
           var_value = as.character(var_value)) %>%
    bind_rows(master_df)
}

#loop over all variables
unique_vars <- unique(master_df$var)

for(cur_var in unique_vars){
  #subset current variable from master_df
  master_df_cur_var <- master_df %>%
    dplyr::filter(var == cur_var) %>%
    filter(!is.na(var_value))
  
  #don't generate plot if variable has more than 20 unique values
  if(length(unique(master_df_cur_var$var_value)) > 20 | nrow(master_df_cur_var) == 0) {
    print(paste0(cur_var, ': skip'))
    next
  }
  
  #plot
  p <- ggplot(master_df_cur_var, aes(x = REFYEAR, y = share, fill = var_value))+
    geom_bar(stat = 'identity', position = 'dodge')+
    facet_wrap(. ~ COUNTRY)+
    labs(x = 'year', y = 'share', fill = cur_var,
         title = paste0('Shares of ', cur_var, ' by country and year'))
  #save
  ggsave(paste0(output_fp, cur_var, '.png'), plot = last_plot(), width = 20, height = 20)
}
