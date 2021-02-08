library(knitr)
library(rmarkdown)
library(distill)
library(tidyverse)
library(scales)

knit_all_posts = function(post_folder){
  folders = list.files(post_folder)
  
  for(i in 1:length(folders)){
    folder = folders[i]
    
    rmd_file = paste0(post_folder,'/',folder,'/',str_sub(folder,12,-1),'.Rmd')
    
    rmarkdown::render(rmd_file)
    
    print(paste0(percent(i/length(folders)),' done'))
  }
}

#Posts
knit_all_posts('_posts')

#User Resources
knit_all_posts('_user-resources')

#Example Analyses
knit_all_posts('_example-analyses')
