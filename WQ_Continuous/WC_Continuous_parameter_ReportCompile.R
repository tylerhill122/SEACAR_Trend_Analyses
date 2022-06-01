#This script is created to automate the production of Rmd documents for each relevant combination of
#parameter, relative depth, and activity type for continuous WC data.

#Load libraries
library(knitr)
library(readr)
library(dplyr)

#Sets whether to run documents with plots or not (APP_Plots==TRUE to include plots)
APP_Plots <- TRUE
#Set output directory
out_dir <- "output/by_parameter/"

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
      file_in <- list.files("data", pattern=param_name, full=TRUE)
      
      #Since Dissolved_Oxygen will return both Dissolved_Oxygen and Dissolved_Oxygen_Saturation,
      #the if statement removes the entry for Dissolved_Oxygen_Saturation when trying to get Dissolved_Oxygen
      if(param_name=="Dissolved_Oxygen" & length(grep("Saturation", file_in))>0){
         file_in <- file_in[-grep("Saturation", file_in)]
      }
      
      #Filters list of file names for the desired region
      file_in <- file_in[i]
      region <- all_regions[i]
         #Renders WC_discrete_parameter.Rmd for each parameter combination and writes the report to a pdf
         #Stored in reports/by_parameter directory
         rmarkdown::render(input = "WC_Continuous_parameter.Rmd", 
                           output_format = "pdf_document",
                           output_file = paste0("WC_Continuous_", param_name, "_",
                                                region, ".pdf"),
                           output_dir = "reports/by_parameter",
                           clean=TRUE)
   }
}
