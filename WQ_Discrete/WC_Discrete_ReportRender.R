# The purpose of this script is to automate the production of Rmd documents for each relevant combination of
# parameter, relative depth, and activity type for discrete WC data.
# Created by J.E. Panzik (jepanzik@usf.edu) for SEACAR


## WHEN RUNNING IN RSTUDIO:
## Set working directory to "Source File Location" in "Session" menu
Start_time <- Sys.time()

#Load libraries
library(data.table)
library(knitr)
library(readr)
library(dplyr)
library(tictoc)

# Gets directory of this script and sets it as the working directory
wd <- dirname(getActiveDocumentContext()$path)
setwd(wd)

tic()
#Sets whether to run documents with plots or not (APP_Plots==TRUE to include plots)
APP_Plots <- TRUE

#Set output directory
out_dir <- "output"

#Set number of unique years a location must have to be considered for analysis
suff_years <- 10

#Sets the list of parameter names to cycle through. This can be edited to limit the number of parameters.
#Easiest way to edit is to comment out undesired parameters.
#If only one parameter is desired, comment out all other parameters and delete comma after remaining parameter
all_params <- c(
      "Chlorophyll_a_corrected_for_pheophytin",
      "Chlorophyll_a_uncorrected_for_pheophytin",
      "Colored_dissolved_organic_matter_CDOM",
      "Dissolved_Oxygen",
      "Dissolved_Oxygen_Saturation",
      "pH",
      "Salinity",
      "Secchi_Depth",
      "Total_Nitrogen",
      "Total_Phosphorus",
      "Total_Suspended_Solids_TSS",
      "Turbidity",
      "Water_Temperature"
)

#Sets the list of parameter abbreviation names to cycle through. This can be edited to limit the number of parameters.
#Easiest way to edit is to comment out undesired parameters.
#If only one parameter is desired, comment out all other parameters and delete comma after remaining parameter
all_params_short <- c(
      "ChlaC",
      "Chla",
      "CDOM",
      "DO",
      "DOS",
      "pH",
      "Sal",
      "Secchi",
      "TN",
      "TP",
      "TSS",
      "Turb",
      "TempW"
)

#Sets the list of relative depths to cycle through. This can be edited to limit the number of depths.
#If only one depth is desired, comment out the other depth and delete comma after remaining depth
all_depths <- c(
      "Surface",
      "Bottom",
      "All"
)

#Sets the list of activity types to cycle through. This can be edited to limit the number of types.
#If only one type is desired, comment out the other type and delete comma after remaining type
all_activity <- c(
      "Field",
      "Lab",
      "All"
)

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

#Starts for loop that cycles through each parameter
for (j in 1:length(all_params)){
      param_name <- all_params[j]
      param_abrev <- all_params_short[j]
      #Gets the file with the filename containing the desired parameter
      file_in <- list.files("data", pattern=param_name, full=TRUE)
      
      #Since Dissolved_Oxygen will return both Dissolved_Oxygen and Dissolved_Oxygen_Saturation,
      #the if statement removes the entry for Dissolved_Oxygen_Saturation when trying to get Dissolved_Oxygen
      if(param_name=="Dissolved_Oxygen" & length(grep("Saturation", file_in))>0){
            file_in <- file_in[-grep("Saturation", file_in)]
      }
      
      file_short <- sub("data/", "", file_in)
      out_dir_param <- paste0(out_dir, "/", param_name)
      #Starts for loop that cycles through each depth
      for (depth in all_depths){
            #Because secchi depth is does not have a bottom measurement, this statement skips
            #Secchi depth for bottom
            if (param_name=="Secchi_Depth" & (depth=="Bottom" | depth=="All")){
                  next
            }
            
            #Starts for loop that cycles through activity types.
            for (activity in all_activity){
                  #Skips Field loops for parameters that only have Lab measurements
                  if ((param_name=="Chlorophyll_a_corrected_for_pheophytin" | 
                       param_name=="Chlorophyll_a_uncorrected_for_pheophytin" |
                       param_name=="Colored_dissolved_organic_matter_CDOM" |
                       param_name=="Total_Nitrogen" |
                       param_name=="Total_Phosphorus" |
                       param_name=="Total_Suspended_Solids_TSS") & activity=="Field") {
                        next
                        #Skips Lab loops for parameters that only have Field measurements
                  } else if ((param_name=="Dissolved_Oxygen" |
                              param_name=="Dissolved_Oxygen_Saturation" |
                              param_name=="pH" |
                              param_name=="Secchi_Depth" |
                              param_name=="Water_Temperature") & activity=="Lab") {
                        next
                  } else if ((param_name=="Chlorophyll_a_corrected_for_pheophytin" |
                              param_name=="Colored_dissolved_organic_matter_CDOM" |
                              param_name=="Dissolved_Oxygen" |
                              param_name=="Dissolved_Oxygen_Saturation" |
                              param_name=="pH" |
                              param_name=="Secchi_Depth" |
                              param_name=="Total_Nitrogen" |
                              param_name=="Total_Phosphorus" |
                              param_name=="Water_Temperature") & activity=="All") {
                        next
                  }
                  
                  
                  
                  #Renders SEACAR_WC_Discrete.Rmd for each parameter combination and writes the report to a pdf
                  #Stored in output directory
                  file_out <- paste0("WC_Discrete_", param_abrev, "_", activity, "_",
                                     depth, "_Report")
                  rmarkdown::render(input = "WC_Discrete.Rmd", 
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
}
toc()
End_time <- Sys.time()

print(Start_time)
print(End_time)