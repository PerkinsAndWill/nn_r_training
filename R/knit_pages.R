require(knitr)
require(rmarkdown)

rmarkdown::render('markdown/index_gen.Rmd',output_format = 'all','../index.html')
