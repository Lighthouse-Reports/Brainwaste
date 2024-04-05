setwd("/Users/justin-casimirbraun/Brainwaste")

#countries to analyze
countries_to_analyze <- c('AT', 'BE', 'CH','CY', 'DK', 
                          'EL', 'ES', 'FI', 'FR', 'IE', 
                          'IS', 'IT', 'LU', 'MT', 'NL', 
                          'NO', 'PT', 'SE', 'UK',
                          #'EE', 'LT', 'LV', 'PL', 'CZ', 'BG',  'HR', 'HU', 'RO','SI', 'SK'
                          'Baltics', 'eu07', 'Visegrad', 'yugo'
                          )

#set date
cur_date <- 'college_230314_'

### preprocessing ###
source('Code/preprocessing/setup_country.R', local = T)

source('Code/preprocessing/operationalize_variables.R', local = T)

source('Code/preprocessing/merge_with_external_data.R', local = T)

# merge Eastern European countries
source('Code/preprocessing/merge_east.R')

source('Code/preprocessing/restrict_rows.R', local = T)

### analysis ###
source('Code/preprocessing/understand_variables.R', local = T)

source('Code/analysis/descriptives.R', local = T)

source('Code/analysis/h1.R', local = T)

source('Code/analysis/h2.R', local = T)

source('Code/analysis/h3_1.R', local = T)

source('Code/analysis/h3_2.R', local = T)

source('Code/analysis/h4.R', local = T)

### visualize descriptives ###
source('Code/displays/visualize_var_counts.R', local = T)

source('Code/displays/visualize_descriptives.R', local = T)

source('Code/displays/visualize_h1.R', local = T)

source('Code/displays/visualize_h2.R', local = T)


## scratchpad ###
source('Code/scratchpad/240108_justin_rq01_01.R')
source('Code/scratchpad/240108_justin_rq01_02.R')
source('Code/scratchpad/231212_justin_rq01_04.R')
#source('Code/scratchpad/231212_justin_rq01_07.R')
source('Code/scratchpad/231220_justin_rq03_01.R')
source('Code/scratchpad/230224_justin_rq03_01_SEX.R')
source('Code/scratchpad/231212_justin_sample_size_country.R')

source('Code/scratchpad/240305_maud_questions.R')


source('Code/scratchpad/summary_tables.R')
source('Code/scratchpad/summary_tables_gender.R')
