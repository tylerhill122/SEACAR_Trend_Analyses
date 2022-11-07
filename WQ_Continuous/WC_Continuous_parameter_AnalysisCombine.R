# This script is designed to read the file names for the Kendall Tau results,
# import each one, combine into one data table, arrange by managed area, and write to file
# Created by J.E. Panzik (jepanzik@usf.edu) for SEACAR

## WHEN RUNNING IN RSTUDIO:
## Set working directory to "Source File Location" in "Session" menu


library(data.table)
library(dplyr)

out_dir <- "output/by_parameter/Combined"

#List all of the files in the "tables" directory that are KendallTau results
files <- list.files("output/by_parameter", pattern="KendallTau", full.names=TRUE)
website <- fread("data/WebsiteParameters.csv",  sep=",", header=TRUE,
                 stringsAsFactors=FALSE, na.strings="")

for(i in 1:length(files)){
      if(i==1){
            data <- fread(files[i], sep="|", header=TRUE, stringsAsFactors=FALSE,
                         na.strings="")
      } else{
            data <- bind_rows(data,
                              fread(files[i], sep="|", header=TRUE,
                                    stringsAsFactors=FALSE, na.strings=""))
      }
}
data <- merge.data.frame(data, website, by=c("ParameterName"), all=TRUE)
data$Website[is.na(data$Website)] <- 0
data <- data %>%
      select(AreaID, ManagedAreaName, ProgramID, ProgramName, ProgramLocationID,
             everything())
data <- as.data.table(data[order(data$ManagedAreaName, data$ProgramID,
                                 data$ProgramName, data$ProgramLocationID,
                                 data$ParameterName), ])

fwrite(data, paste0(out_dir,"/", "WQ_Continuous_All_KendallTau_Stats.txt"), sep="|")
fwrite(data, paste0(out_dir,"/", "WQ_Continuous_All_KendallTau_Stats.csv"), sep=",")
