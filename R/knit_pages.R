require(knitr)
require(rmarkdown)

#Home page
rmarkdown::render('markdown/index_gen.Rmd',output_format = 'all','../index.html')

#Course topics
rmarkdown::render('markdown/shiny_1.Rmd',output_format = 'all','../topics/shiny_1.html')
rmarkdown::render('markdown/shiny_2.Rmd',output_format = 'all','../topics/shiny_2.html')
rmarkdown::render('markdown/shiny_3.Rmd',output_format = 'all','../topics/shiny_3.html')
rmarkdown::render('markdown/shiny_4.Rmd',output_format = 'all','../topics/shiny_4.html')
rmarkdown::render('markdown/tidyverse.Rmd',output_format = 'all','../topics/tidyverse.html')
rmarkdown::render('markdown/geospatial.Rmd',output_format = 'all','../topics/geospatial.html')
rmarkdown::render('markdown/census.Rmd',output_format = 'all','../topics/census.html')
rmarkdown::render('markdown/gtfs_data.Rmd',output_format = 'all','../topics/gtfs_data.html')
rmarkdown::render('markdown/rmarkdown.Rmd',output_format = 'all','../topics/rmarkdown.html')
rmarkdown::render('markdown/misc_questions.Rmd',output_format = 'all','../topics/misc_questions.html')

#Other topics
rmarkdown::render('markdown/r_setup_nn.Rmd',output_format = 'all','../other_topics/r_setup_nn.html')
