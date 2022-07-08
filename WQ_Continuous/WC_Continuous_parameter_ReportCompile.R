# This script is created to automate the production of Rmd documents for each relevant combination of
# parameter, relative depth, and activity type for continuous WC data.
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
out_dir <- "output/by_parameter/"

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
for (param_name in all_params){
   
   #Starts for loop that cycles through each depth
   for (i in 1:length(all_regions)){
      #Gets the files with the file names containing the desired parameter
      file_list <- list.files("data", pattern=param_name, full=TRUE)
      
      #Since Dissolved_Oxygen will return both Dissolved_Oxygen and Dissolved_Oxygen_Saturation,
      #the if statement removes the entry for Dissolved_Oxygen_Saturation when trying to get Dissolved_Oxygen
      if(param_name=="Dissolved_Oxygen" & length(grep("Saturation", file_list))>0){
         file_list <- file_list[-grep("Saturation", file_list)]
      }
      
      #Filters list of file names for the desired region
      region <- all_regions[i]
      file_in <- file_list[grep(paste0("_", region, "-"), file_list)]
      
      #Renders WC_discrete_parameter.Rmd for each parameter combination and writes the report to a pdf
      #Stored in reports/by_parameter directory
      file_out <-  paste0("WC_Continuous_", param_name, "_",
                          region)
      rmarkdown::render(input = "WC_Continuous_parameter.Rmd", 
                        output_format = "html_document",
                        output_file = paste0(file_out, ".html"),
                        output_dir = "reports/by_parameter",
                        clean=TRUE)
      rmarkdown::render(input = paste0("reports/by_parameter/", file_out, ".md"), 
                        output_format = "word_document",
                        output_file = paste0(file_out, ".docx"),
                        output_dir = "reports/by_parameter",
                        clean=TRUE)
   }
}