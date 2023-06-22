# The purpose of this script is to automate the production of Rmd documents for oyster analysis.
# Created by J.E. Panzik (jepanzik@usf.edu) for SEACAR

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
param_name <- "All_Oyster_Parameters"

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")


#Gets the files with the file names containing the desired parameter
file_in <- list.files("data", pattern=param_name, full=TRUE)

if(param_name=="Hectares" & length(grep("All Parameters", file_in))>0){
   file_in <- file_in[-grep("All Parameters", file_in)]
}
#Gets the specific file used and removes the directory names
file_short <- sub("data/", "", file_in)

#Renders SEACAR_Oyster.Rmd for each parameter combination and writes the
#report to an html and Word document stored in output directory
file_out <-  paste0("SEACAR_", param_name)


rmarkdown::render(input = "SEACAR_Oyster.Rmd", 
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

# Section to create zip files of all plots created
# Gets list of all directories in output
param_dirs <- list.dirs(out_dir, full.name=FALSE)
# Only keeps Figure directories
param_dirs <- param_dirs[ grepl("Figures", param_dirs) ]
# Loops through each figure directory
wd <- getwd()
for(i in 1:length(param_dirs)){
   # Makes full directory name
   param <- paste0("output/",param_dirs[i])
   
   # Creates shorter version of parameter directory to be used as zip file name
   param_short <- gsub("_", "", param_dirs[i])
   param_short <- gsub("/", "", param_short)
   # Gets list of png plots
   fig_list <- list.files(param, pattern=".png", full=FALSE)
   
   setwd(paste0(wd, "/",param))
   # Sets name to be used for zip file
   zip_name <- paste0("Oyster", param_short)
   # Zips png figures to a zip file with name created above
   zip(zip_name, files=fig_list)
   setwd(wd)
}
