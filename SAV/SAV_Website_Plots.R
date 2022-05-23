#This script is designed to read the file names for the trendplots and bar plots
#Plot files are either copied to the website directory or the rds file is import
#and converted to a png
library(data.table)
library(dplyr)
library(ggplot2)

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
      barplot <- barplot + theme(plot.title = element_text(face="bold", hjust = 0.5))
      #Changes output name from ending in rds to png
      outname <- substr(files[i], 1, nchar(files[i])-3)
      outname <- paste0(outname, "png")
      
      #Creates and saves png file of plot
      png(paste0(out_dir,outname),
          width = 8,
          height = 4,
          units = "in",
          res = 100)
      
      print(barplot)
      dev.off()
}



