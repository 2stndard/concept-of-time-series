wd <- getwd()
rmarkdown::render("01-data_import.Rmd")
rmarkdown::render("02-visualization.Rmd")
rmarkdown::render("03-data_handling.Rmd", output_dir = './doc')
rmarkdown::render("04-forecasting_I.Rmd", output_dir = './doc')
rmarkdown::render("05-forecasting-II.Rmd")
rmarkdown::render("06-forecasting-III.Rmd", output_dir = './doc')
rmarkdown::render("07-references.Rmd", output_dir = './doc')
rmarkdown::render("index.Rmd", output_dir = paste0(wd, './docs'), 
                  output_options=list(html_document = 
                                        list(self_contained = FALSE, 
                                             lib_dir = file.path(wd, 'docs', "libs"),
                                             css = paste(wd, "custom.css",
                                                         sep="/")
                                             )
                                      )
                  )


