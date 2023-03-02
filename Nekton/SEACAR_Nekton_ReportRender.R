#This script is created to automate the production of Rmd documents Nekton.
# Created by J.E. Panzik (jepanzik@usf.edu) for SEACAR


## WHEN RUNNING IN RSTUDIO:
## Set working directory to "Source File Location" in "Session" menu


#Load libraries
library(knitr)
library(readr)
library(dplyr)
library(data.table)

#Sets whether to run documents with plots or not (APP_Plots==TRUE to include plots)
APP_Plots <- TRUE

#Set output directory
out_dir <- "output/by_parameter"

#Sets the list of parameter names to cycle through. This can be edited to limit the number of parameters.
#Easiest way to edit is to comment out undesired parameters.
#If only one parameter is desired, comment out all other parameters and delete comma after remaining parameter
all_params <- c(
      # "Count",
      # "Fork Length",
      "Presence"
      # "Standard Length",
      # "Total Length"
)

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("../ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

#Starts for loop that cycles through each parameter
for (param_name in all_params){
      #Gets the files with the file names containing the desired parameter
      file_in <- list.files("data", pattern=param_name, full=TRUE)
      
      #Renders SEACAR_Nekton.Rmd for each parameter combination and writes the
      #report to an html and Word document stored in reports/by_parameter directory
      file_out <-  paste0("SEACAR_Nekton_", param_name)
      
      rmarkdown::render(input = "SEACAR_Nekton.Rmd", 
                        output_format = "pdf_document",
                        output_file = paste0(file_out, ".pdf"),
                        output_dir = "reports/by_parameter",
                        clean=TRUE)
      rmarkdown::render(input = paste0("reports/by_parameter/", file_out, ".md"),
                        output_format = "word_document",
                        output_file = paste0(file_out, ".docx"),
                        output_dir = "reports/by_parameter",
                        clean=TRUE)
      # rmarkdown::render(input = paste0("reports/by_parameter/", file_out, ".md"), 
      #                   output_format = "word_document",
      #                   output_file = paste0(file_out, ".docx"),
      #                   output_dir = "reports/by_parameter",
      #                   clean=TRUE)
      unlink(paste0("reports/by_parameter/", file_out, ".md"))
      unlink(paste0("reports/by_parameter/", file_out, "_files"), recursive=TRUE)
}
