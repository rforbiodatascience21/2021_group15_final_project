# Run all scripts ---------------------------------------------------------
source(file = "R/01_load.R")
source(file = "R/02_clean.R")
source(file = "R/03_augment.R")
source(file = "R/04_EDA.R")
source(file = "R/05_logistic_reg.R")
source(file = "R/06_clustering.R")


# Knit the Presentation slides --------------------------------------------
rmarkdown::render('doc/Presentation.Rmd',output_file='Presentation.html')
rmarkdown::render('doc/Presentation.Rmd',output_file='Presentation.html.gz')