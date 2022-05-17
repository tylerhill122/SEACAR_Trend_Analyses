#This script is designed to read the file names for the trendplots and bar plots
#Plot files are either copied to the website directory or the rds file is import
#and converted to a png
library(data.table)
library(dplyr)
library(ggplot2)

#List all of the trendplot jpg files
files <- list.files("SAV/output/Figures/BB/img", pattern="trendplot", full.names=TRUE)

#Include only those that are BBpct
files <- files[grep("BBpct", files)]

#Copy BBpct trendplots to website file directory
file.copy(from = files, to = "SAV/output/website/images/trendplots",
          overwrite = TRUE)


###Bar Plot
#List all of the trendplot jpg files
files <- list.files("SAV/output/Figures/BB", pattern="barplot")

#Set the directory with the input rds files and directory to save to
in_dir <- "SAV/output/Figures/BB/"
out_dir <- "SAV/output/website/images/barplots/"

#For loop the cycles through each barplot file
for(i in 1:length(files)){
      #Imports the barplot object
      barplot <- readRDS(paste0(in_dir, files[i]))
      
      #Changes output name from ending in rds to png
      outname <- substr(files[i], 1, nchar(files[i])-3)
      outname <- paste0(outname, "png")
      
      #Creates and saves png file of plot
      png(paste0(out_dir,outname))
      print(barplot)
      dev.off()
}



