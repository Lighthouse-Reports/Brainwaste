setwd("/Users/justin-casimirbraun/Documents/Brainwaste")

library(dplyr)
library(ggplot2)
library(tidyr)


input_data_path = 'Input Data/231106_ELF_Raw/'

#load data
at95q1 <- read.csv(paste0(input_data_path, 'QUAR_1995_2021/AT_QUAR/AT1995Q1.csv'))
at21q1 <- read.csv(paste0(input_data_path, 'QUAR_1995_2021/AT_QUAR/AT2021Q1.csv'))
at21q2 <- read.csv(paste0(input_data_path, 'QUAR_1995_2021/AT_QUAR/AT2021Q2.csv'))
at95y <- read.csv(paste0(input_data_path, 'YEAR_1983_2005/AT_YEAR_1983_2005/AT1995_y.csv'))
at06y <-  read.csv(paste0(input_data_path, 'YEAR_2006_2021/AT_YEAR_2006_onwards/AT2006_y.csv'))
at21y <-  read.csv(paste0(input_data_path, 'YEAR_2006_2021/AT_YEAR_2006_onwards/AT2021_y.csv'))
at20y <-  read.csv(paste0(input_data_path, 'YEAR_2006_2021/AT_YEAR_2006_onwards/AT2020_y.csv'))
at19y <-  read.csv(paste0(input_data_path, 'YEAR_2006_2021/AT_YEAR_2006_onwards/AT2019_y.csv'))

#extract column lists for quarterly, yearly old, and yearly new datasets
cols_at95q1 <- names(at95q1)
cols_at95y <- names(at95y)
cols_at06y <- names(at06y)

#check overlap between datasets
quarter_in_year_old <- cols_at95q1[cols_at95q1 %in% cols_at95q1]
quarter_not_in_year_old <- cols_at95q1[!(cols_at95q1 %in% cols_at95q1)]

year_old_not_in_quarter <- cols_at95y[!(cols_at95y %in% cols_at95q1)]

year_old_not_in_year_new <- cols_at95y[!(cols_at95y %in% cols_at06y)]
year_new_not_in_year_old <- cols_at06y[!(cols_at06y %in% cols_at95y)]


#check individual overlap between quarter and year





