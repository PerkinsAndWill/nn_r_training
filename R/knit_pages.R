require(knitr)
require(rmarkdown)

rmarkdown::render('markdown/read_me_gen.Rmd',output_format = 'all','../README.md')
