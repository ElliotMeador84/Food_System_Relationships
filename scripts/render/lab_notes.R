Sys.setenv(RSTUDIO_PANDOC=Sys.getenv("RSTUDIO_PANDOC"))

rmarkdown::render('~/Documents/R/Food_System_Relationships/lab_notes/Starter_stakeholders.Rmd')


file.copy('lab_notes/Starter_stakeholders.html', 
          '~/Google Drive/T-GRAINS/Twitter data/Twitter-results.html', overwrite = T)

