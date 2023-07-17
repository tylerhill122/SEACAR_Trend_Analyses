# The purpose of this script is to automate the production of Rmd documents for each relevant combination of
# parameter, relative depth, and activity type for continuous WC data.
# Created by J.E. Panzik (jepanzik@usf.edu) for SEACAR

## WHEN RUNNING IN RSTUDIO:
## Set working directory to "Source File Location" in "Session" menu
start_time <- Sys.time()

#Load libraries
library(knitr)
library(readr)
library(dplyr)
library(data.table)
library(rstudioapi)

# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

#Sets whether to run documents with plots or not (APP_Plots==TRUE to include plots)
APP_Plots <- TRUE

#Set output directory
out_dir <- "output"

#Set number of unique years a location must have to be considered for analysis
suff_years <- 5

#Sets the list of parameter names to cycle through. This can be edited to limit the number of parameters.
#Easiest way to edit is to comment out undesired parameters.
#If only one parameter is desired, comment out all other parameters and delete comma after remaining parameter
all_params <- c(
   "Dissolved_Oxygen",
   "Dissolved_Oxygen_Saturation",
   "pH",
   "Salinity",
   "Turbidity",
   "Water_Temperature"
)

#Sets the list of parameter abbreviation names to cycle through. This can be edited to limit the number of parameters.
#Easiest way to edit is to comment out undesired parameters.
#If only one parameter is desired, comment out all other parameters and delete comma after remaining parameter
all_params_short <- c(
   "DO",
   "DOS",
   "pH",
   "Sal",
   "Turb",
   "TempW"
)

#Sets the list of regions to cycle through. This can be edited to limit the number of regions.
#If only one region is desired, comment out the other regions and delete comma after remaining region
all_regions <- c(
   "NE",
   "NW",
   "SE",
   "SW"
)

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

#Starts for loop that cycles through each parameter
for (j in 1:length(all_params)){
   param_name <- all_params[j]
   param_abrev <- all_params_short[j]
   #Starts for loop that cycles through each depth
   for (i in 1:length(all_regions)){
      #Gets the files with the file names containing the desired parameter
      file_list <- list.files("data", pattern=param_name, full=TRUE)
      
      #Since Dissolved_Oxygen will return both Dissolved_Oxygen and Dissolved_Oxygen_Saturation,
      #the if statement removes the entries for Dissolved_Oxygen_Saturation when trying to get Dissolved_Oxygen
      if(param_name=="Dissolved_Oxygen" & length(grep("Saturation", file_list))>0){
         file_list <- file_list[-grep("Saturation", file_list)]
      }
      
      #Filters list of file names for the desired region
      region <- all_regions[i]
      # Uses _[Region Abbreviation]- so that is does not return any coincidental combinations
      file_in <- file_list[grep(paste0("_", region, "-"), file_list)]
      #Gets the specific file used and removes the directory names
      file_short <- sub("data/", "", file_in)
      out_dir_param <- paste0(out_dir, "/", param_name)
      
      #Renders WC_Continuous.Rmd for each parameter and region combination and writes the report to a pdf
      #Stored in output directory
      file_out <-  paste0("WC_Continuous_", param_abrev, "_",
                          region, "_Report")
      rmarkdown::render(input = "WC_Continuous.Rmd", 
                        output_format = "pdf_document",
                        output_file = paste0(file_out, ".pdf"),
                        output_dir = out_dir_param,
                        clean=TRUE)
      rmarkdown::render(input = paste0(out_dir_param, "/", file_out, ".md"),
                        output_format = "word_document",
                        output_file = paste0(file_out, ".docx"),
                        output_dir = out_dir_param,
                        clean=TRUE)
      #Removes unwanted files created in the rendering process
      unlink(paste0(out_dir_param, "/", file_out, ".md"))
      unlink(paste0(out_dir_param, "/", file_out, "_files"), recursive=TRUE)
   }
}
end_time <- Sys.time()

print(start_time)
print(end_time)