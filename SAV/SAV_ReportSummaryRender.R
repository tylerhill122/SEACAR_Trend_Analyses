#This script is created to automate the production of Rmd documents for SAV.


## WHEN RUNNING IN RSTUDIO:
## Set working directory to "Source File Location" in "Session" menu

file_out <-  paste0("SAV_ReportSummary")

rmarkdown::render(input = "SAV_ReportSummary.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(file_out, ".html"),
                  output_dir = "reports",
                  clean=TRUE)
rmarkdown::render(input = paste0("reports/", file_out, ".md"), 
                  output_format = "word_document",
                  output_file = paste0(file_out, ".docx"),
                  output_dir = "reports",
                  clean=TRUE)