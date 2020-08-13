require(knitr)
require(rmarkdown)

#Home page
rmarkdown::render('markdown/index_gen.Rmd',output_format = 'all','../index.html')

#Course topics
rmarkdown::render('topics_setup/02_shiny_1/shiny_1.Rmd',output_format = 'all','../../topics_output/shiny_1.html')
rmarkdown::render('topics_setup/04_shiny_2/shiny_2.Rmd',output_format = 'all','../../topics_output/shiny_2.html')
rmarkdown::render('topics_setup/06_shiny_3/shiny_3.Rmd',output_format = 'all','../../topics_output/shiny_3.html')
rmarkdown::render('topics_setup/08_shiny_4/shiny_4.Rmd',output_format = 'all','../../topics_output/shiny_4.html')
rmarkdown::render('topics_setup/01_tidyverse/tidyverse.Rmd',output_format = 'all','../../topics_output/tidyverse.html')
rmarkdown::render('topics_setup/03_geospatial/geospatial.Rmd',output_format = 'all','../../topics_output/geospatial.html')
rmarkdown::render('topics_setup/05_census/census.Rmd',output_format = 'all','../../topics_output/census.html')
rmarkdown::render('topics_setup/07_gtfs/gtfs_data.Rmd',output_format = 'all','../../topics_output/gtfs_data.html')
rmarkdown::render('topics_setup/09_markdown/rmarkdown.Rmd',output_format = 'all','../../topics_output/rmarkdown.html')
rmarkdown::render('topics_setup/10_misc_questions/misc_questions.Rmd',output_format = 'all','../../topics_output/misc_questions.html')

#Other topics
rmarkdown::render('markdown/r_setup_nn.Rmd',output_format = 'all','../other_topics/r_setup_nn.html')
