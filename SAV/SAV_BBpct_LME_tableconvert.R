#This script is designed to read the file names for the LME results of the BBpct analysis,
#import each one, extract the intercept, slope, and p values, then write them to a pipe-delimited file
library(data.table)
library(dplyr)

#List all of the files in the "tables" directory that are LME results
files <- list.files("SAV/output/tables", pattern="lmeresults", full.names=TRUE)

#Include only those that are BBpct
files <- files[grep("BBpct", files)]

#For loop cycles through each file name
for (i in 1:length(files)) {
   #Get filename from list
   filename <- files[i]
   
   #Read in file
   table <- readRDS(filename)
   
   #Keep only rows that are values with "fixed" in the effect column
   table <- table[table$effect=="fixed" & !is.na(table$effect),]
   
   #For each managed area and species, get the LME intercept, slope, and p values
   table <- table %>%
      group_by(managed_area, species) %>%
      summarise(LME_Intercept = estimate[term == "(Intercept)"],
                LME_Slope = estimate[term == "relyear"],
                p = p.value[term == "relyear"])
   
   #If this is the first file, the table from above is stored as the output table
   #If not the first file, the table is added to the end of the output table
   if(i==1) {
      output <- table
   } else {
      output <- bind_rows(output, table)
   }
}

#Change column names to better match other outputs
setnames(output, c("managed_area", "species"), c("ManagedAreaName", "Species"))

#Write output table to a pipe-delimited txt file
fwrite(output, "SAV/output/website/SAV_BBpct_LMEresults_All.txt", sep="|")
