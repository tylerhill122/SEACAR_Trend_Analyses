# The purpose of this script is to automate the rendering of Rmd documents for SAV.


## WHEN RUNNING IN RSTUDIO:
## Set working directory to "Source File Location" in "Session" menu
 out_dir <- "output"

#Renders SEACAR_SAV_ReportSummary.Rmd and writes the
#report to a pdf and Word document stored in output directory
file_out <-  "SEACAR_SAV_ReportSummary"


rmarkdown::render(input = "SEACAR_SAV_ReportSummary.Rmd", 
                  output_format = "pdf_document",
                  output_file = paste0(file_out, ".pdf"),
                  output_dir = out_dir,
                  clean=TRUE)
rmarkdown::render(input = paste0(out_dir, "/", file_out, ".md"),
                  output_format = "word_document",
                  output_file = paste0(file_out, ".docx"),
                  output_dir = out_dir,
                  clean=TRUE)

#Removes unwanted files created in the rendering process
unlink(paste0(out_dir, "/", file_out, ".md"))
unlink(paste0(out_dir, "/", file_out, "_files"), recursive=TRUE)