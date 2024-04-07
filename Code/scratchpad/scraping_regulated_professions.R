setwd("/Users/justin-casimirbraun/Documents/Lighthouse Reports/Brain Waste/Analysis")

#load packages
require(magrittr)
require(rvest)
library(RSelenium)
library(dplyr)
library(readr)

#get links to occupation websites
job_list_url <- 'https://ec.europa.eu/growth/tools-databases/regprof/professions/generic'
rD <- rsDriver(browser="firefox", port=4444L, chromever = NULL)
remDr <- rD[["client"]]
remDr$navigate(job_list_url)
show_all_button <- '#btn_showall_type'

show_all_button_elem <- remDr$findElement(using = 'css selector', show_all_button)
# click button
show_all_button_elem$clickElement()

job_list_css_selector <- '#ecl-main-content > div > ng-component > ng-component > eui-page > eui-page-content > div.ecl-container > div > div.col-8_75.ecl-offset-_25 > app-professions-generic-results > div:nth-child(2) > table > tbody'
job_list_table <- remDr$getPageSource()[[1]] %>% 
  read_html() %>%
  html_element(css = job_list_css_selector)
  
jobs <- job_list_table %>%
  html_elements('a') %>%
  html_text()
links <- job_list_table %>%
  html_elements('a') %>%
  html_attr("href")
jobs_df <- data.frame(job = jobs,
                      link_short = links)
jobs_df$full_link <- paste0('https://ec.europa.eu', jobs_df$link_short)


###scrape individual job profiles
job_country_df <- data.frame(name_country = character(),
                             country = character(),
                             region = character(),
                             recognition_procedure = character(),
                             generic_name = character(),
                             index = numeric())
for(i in 445:nrow(jobs_df)){
  job_name <- jobs_df$job[i]
  job_link <- jobs_df$full_link[i]
  remDr$navigate(job_link)
  remDr$maxWindowSize()
  Sys.sleep(2)
  num_jobs_elem <-NULL
  while(is.null(num_jobs_elem)){
    num_jobs_elem <- tryCatch({remDr$findElement(using = 'css selector', value = '#navCountries > app-regprofs > div.ecl-u-type-heading-3.ecl-u-pa-none > span')},
                              error = function(e){NULL})
  }
  num_jobs <- num_jobs_elem$getElementText()[[1]] %>%
    parse_number()
  if(num_jobs > 15){
    res_shown <- 0
    while(res_shown != num_jobs) {
      show_all_button_elem <- NULL
      show_all_button <- '#btn_showall_type > span'
      while(is.null(show_all_button_elem)){
        show_all_button_elem <- tryCatch({remDr$findElement(using = 'css selector', show_all_button)},
                                         error = function(e){NULL})
      }
      webElem <- remDr$findElement(using = 'css selector', 'body')
      webElem$sendKeysToElement(list(key = "page_down"))
      Sys.sleep(1)
      show_all_button_elem$clickElement()
      Sys.sleep(1)
      res_shown_elem <- NULL
      while(is.null(res_shown_elem)){
        res_shown_elem <- tryCatch({remDr$findElement(using = 'css selector', '#navCountries > app-regprofs > div:nth-child(3) > div > div.ecl-u-type-heading-4.col-md-12.ecl-u-mv-s.ecl-u-pa-none > span')},
                                   error = function(e){NULL})
      }
      res_shown <- res_shown_elem$getElementText()[[1]] %>%
        parse_number()
      print(res_shown)
    }
  }
  page <- remDr$getPageSource()[[1]]
  temp <- page %>% 
    read_html() %>%
    rvest::html_element('#navCountries > app-regprofs > div:nth-child(3) > table') %>%
    html_table()
  temp$generic_name <- job_name
  temp$index <- i
  names(temp) <- c('name_country', 'country', 'region', 'recognition_procedure', 'generic_name', 'index')
  job_country_df <- bind_rows(job_country_df, temp)
}

###save and close
remDr$close()
rD$server$stop()

write.csv(job_country_df, 'Data/Regulates_Jobs/regulated_professions_database.csv')
