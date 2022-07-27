#This script is created to automate the production of Rmd documents for oyster analysis.
#Created for SEACAR by J.E. Panzik

## WHEN RUNNING IN RSTUDIO:
## Set working directory to "Source File Location" in "Session" menu

## To ensure this script runs smoothly, please run in a fresh session of R
## Some other libraries cause this script not to work properly

## THIS SCRIPT WILL ONLY RUN WITH brms version 2.16.3 or lower. DOES NOT WORK WITH brms 2.17.0

#Load libraries
library(knitr)
library(readr)
library(dplyr)
library(data.table)

#Sets whether to run documents with plots or not (APP_Plots==TRUE to include plots)
Run_An <- TRUE

#Set output directory
out_dir <- "output"

#Sets the list of parameter names to cycle through. This can be edited to limit the number of parameters.
#Easiest way to edit is to comment out undesired parameters.
#If only one parameter is desired, comment out all other parameters and delete comma after remaining parameter
all_params <- c(
      "All_Oyster_Parameters"
      # "Hectares"
)

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("../ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

#Starts for loop that cycles through each parameter
for (param_name in all_params){
      #Gets the files with the file names containing the desired parameter
      file_in <- list.files("data", pattern=param_name, full=TRUE)
      
      if(param_name=="Hectares" & length(grep("All Parameters", file_in))>0){
            file_in <- file_in[-grep("All Parameters", file_in)]
      }
      #Renders SEACAR_CoastalWetlands.Rmd for each parameter combination and writes the
      #report to an html and Word document stored in reports/by_parameter directory
      file_out <-  paste0("SEACAR_", param_name)
      
      rmarkdown::render(input = "Oyster_Report.Rmd", 
                        output_format = "html_document",
                        output_file = paste0(file_out, ".html"),
                        output_dir = "reports",
                        clean=TRUE)
      rmarkdown::render(input = paste0("reports/", file_out, ".md"), 
                        output_format = "word_document",
                        output_file = paste0(file_out, ".docx"),
                        output_dir = "reports",
                        clean=TRUE)
}