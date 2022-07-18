#This script is designed to read the file names for the LME results of the BBpct analysis,
#import each one, extract the intercept, slope, and p values, then write them to a pipe-delimited file
library(data.table)
library(dplyr)

#List all of the files in the "tables" directory that are LME results
file_list <- list.files("OA_plots", pattern="oysterresults", full.names=TRUE)

#Include only those that are BBpct
file_in <- file_list[grep("txt", file_list)]


#Read in file
data <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE, na.strings = "")

#Keep only rows that are values with "fixed" in the effect column
data <- data[data$effect=="fixed" & !is.na(data$effect),]

#For each managed area and species, get the LME intercept, slope, and p values
table <- data %>%
      group_by(managed_area, indicator, live_date_qual, size_class, habitat_class) %>%
      summarise(Programs=unique(programs),
                Intercept = estimate[term == "(Intercept)"],
                Slope = estimate[term == "RelYear" |
                                       term == "meRelYearSampleAge_StdevgrEQQuadIdentifier"],
                StandardError = std.error[term == "RelYear" |
                                                term == "meRelYearSampleAge_StdevgrEQQuadIdentifier"],
                LowerConfidence = conf.low[term == "RelYear" |
                                                  term == "meRelYearSampleAge_StdevgrEQQuadIdentifier"],
                UpperConfidence = conf.high[term == "RelYear" |
                                                 term == "meRelYearSampleAge_StdevgrEQQuadIdentifier"])

#Change column names to better match other outputs
setnames(table, c("managed_area", "indicator", "size_class", "live_date_qual", "habitat_class", "Programs"),
         c("ManagedAreaName", "ParameterName", "SizeClass", "ShellType", "HabitatType", "ProgramNames"))

table$ManagedAreaName[table$ManagedAreaName=="Fort Pickens Aquatic Preserve"] <-
      "Fort Pickens State Park Aquatic Preserve"

table$ManagedAreaName[table$ManagedAreaName=="St. Andrews Aquatic Preserve"] <-
      "St. Andrews State Park Aquatic Preserve"

table$ShellType[table$ShellType=="Exact"] <- "Live Oysters"
table$ShellType[table$ShellType=="Estimate"] <- "Dead Shells"


#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("../ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

# stats <- fread("SAV/output/data/SAV_BBpct_Stats.txt", sep = "|", header = TRUE, stringsAsFactors = FALSE,
#                na.strings = "")
# setnames(stats, c("ManagedAreaName", "analysisunit"), c("ShortName","Species"))
# 
table <- merge.data.frame(MA_All[,c("AreaID", "ManagedAreaName")],
                          table, by="ManagedAreaName", all=TRUE)

# stats$ShortName <- NULL
# stats$AreaID <- NULL

# stats <-  merge.data.frame(stats, output,
#                            by=c("ManagedAreaName", "Species"), all=TRUE)
# 
# 
# stats <- merge.data.frame(MA_All[,c("AreaID", "ManagedAreaName")],
#                           stats, by=c("ManagedAreaName"), all=TRUE)

table <- as.data.table(table[order(table$ManagedAreaName, table$ParameterName,
                                   table$ShellType, table$SizeClass,
                                   table$HabitatType), ])
table <- table %>% select(AreaID, everything())

#Write output table to a pipe-delimited txt file
fwrite(table, "OA_plots/Oyster_results_All.txt", sep="|")
