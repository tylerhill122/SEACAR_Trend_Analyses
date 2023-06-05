#This script is created to automate the production of Rmd documents for Coral.


## WHEN RUNNING IN RSTUDIO:
## Set working directory to "Source File Location" in "Session" menu


#Load libraries
library(knitr)
library(readr)
library(dplyr)
library(data.table)
library(utils)

#Sets whether to run documents with plots or not (APP_Plots==TRUE to include plots)
APP_Plots <- TRUE

#Set output directory
out_dir <- "output/PercentCover"

# Sets coastal wetland file to only care about "All Parameters" file
param_name <- "PercentCover"

#Sets abbreviation or label to be used in file names
param_file <- "PercentCover"

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

#Gets the files with the file names containing the desired parameter
file_in <- list.files("data", pattern="All_CORAL", full=TRUE)

#Gets the specific file used and removes the directory names
file_short <- sub("data/", "", file_in)

#Renders SEACAR_Coral_PercentCover.Rmd and writes the report to a pdf and 
#Word document stored in output directory
file_out <-  paste0("SEACAR_Coral_", param_file)


rmarkdown::render(input = "SEACAR_Coral_PercentCover.Rmd", 
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


#Gets list of all image files in output/by_parameter/Figures
fig_list <- list.files(paste0(out_dir, "/Figures"), pattern=".png", full=TRUE)
zip(paste0(out_dir, "/Figures/CoralPercentCoverFigures"), files=fig_list)