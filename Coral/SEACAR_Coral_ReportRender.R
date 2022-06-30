#This script is created to automate the production of Rmd documents for Coral.


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
out_dir <- "output"

#Sets the list of parameter names to cycle through. This can be edited to limit the number of parameters.
#Easiest way to edit is to comment out undesired parameters.
#If only one parameter is desired, comment out all other parameters and delete comma after remaining parameter


#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("../ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

#Renders SEACAR_CoastalWetlands.Rmd for each parameter combination and writes the
#report to an html and Word document stored in reports/by_parameter directory
file_out <- "SEACAR_Coral"

rmarkdown::render(input = "SEACAR_Coral.Rmd", 
                  output_format = "html_document",
                  output_file = paste0(file_out, ".html"),
                  output_dir = "reports",
                  clean=TRUE)
rmarkdown::render(input = paste0("reports/", file_out, ".md"), 
                  output_format = "word_document",
                  output_file = paste0(file_out, ".docx"),
                  output_dir = "reports/",
                  clean=TRUE)
