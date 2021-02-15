library(knitr)
library(rmarkdown)
library(distill)
library(tidyverse)
library(scales)
library(googlesheets4)
library(googledrive)
library(DT)

gs4_auth(email = 'bryanpblanc@gmail.com')
raw_sessions = as_id('https://docs.google.com/spreadsheets/d/1FlUvEhTikxxnQYV5CP1T4M5wgudnZJ88f4T44C9RSpc/edit?usp=sharing') %>%
  read_sheet('Webinar Sessions') 

ex_slugs = list.files('_posts/')
new_slugs = raw_sessions %>%
  filter(!is.na(Slug)) %>%
  filter(!(Slug %in% ex_slugs)) %>%
  pull(Slug)

for(i in 1:length(new_slugs)){
  
  slug = new_slugs[i]
  
  post_date = raw_sessions %>%
    filter(Slug == slug) %>%
    pull(Date)
  
  post_title = raw_sessions %>%
    filter(Slug == slug) %>%
    pull(Topic)
  
  distill::create_post(title = post_title,
                       collection = 'posts',
                       slug = slug,
                       date = post_date,
                       draft = TRUE,
                       edit = FALSE,
                       date_prefix = NULL)
  
  print(i)
}
