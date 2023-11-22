setwd("/Users/justin-casimirbraun/Brainwaste")

#countries to analyze
countries_to_analyze <- c('UK', 'ES')

### preprocessing ###
source('Code/preprocessing/setup_country.R', local = T)

source('Code/preprocessing/understand_variables.R', local = T)

source('Code/preprocessing/operationalize_variables.R', local = T)

source('Code/preprocessing/merge_with_external_data.R', local = T)

source('Code/preprocessing/restrict_rows.R', local = T)

### analysis ###
source('Code/analysis/descriptives.R', local = T)

source('Code/analysis/h1.R', local = T)

source('Code/analysis/h2.R', local = T)

### visualize descriptives ###
source('Code/displays/visualize_descriptives.R', local = T)

source('Code/displays/visualize_h1.R')

source('Code/displays/visualize_h2.R')
