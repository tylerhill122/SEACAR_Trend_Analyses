#This script is designed to read the file names for the LME results of the BBpct analysis,
#import each one, extract the intercept, slope, and p values, then write them to a pipe-delimited file
library(data.table)
library(dplyr)

#List all of the files in the "tables" directory that are LME results
file_list <- list.files("output/Tables", pattern="ModelResults", full.names=TRUE)

#Include only those that are txt
file_in <- file_list[grep("csv", file_list)]


#Read in file
data <- fread(file_in, sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "")

############
# #Include only those that are BBpct
# file_in <- file_list[grep("csv", file_list)]
# 
# 
# #Read in file
# data <- fread(file_in, sep = ",", header = TRUE, stringsAsFactors = FALSE, na.strings = "")
# 
# data <- data[data$Converged==1,]
##############

#Keep only rows that are values with "fixed" in the effect column
data <- data[data$effect=="fixed" & !is.na(data$effect),]

#For each managed area and species, get the LME intercept, slope, and p values
table <- data %>%
      group_by(managed_area, indicator, live_date_qual, size_class, habitat_class) %>%
      dplyr::summarise(Programs=unique(programs),
                Intercept = estimate[term == "(Intercept)"],
                ModelEstimate = estimate[term == "RelYear" |
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

table$ShellType[table$ShellType=="Exact"] <- "Live Oyster Shells"
table$ShellType[table$ShellType=="Estimate"] <- "Dead Oyster Shells"

table$ParameterName[table$ParameterName=="Size class"] <- "Shell Height"
table$ParameterName[table$ParameterName=="Percent live"] <- "Percent Live"

#Load data summary
data_summ <- fread("output/data/OysterSummary.txt", sep = "|", header = TRUE, stringsAsFactors = FALSE,
                   na.strings = "")

data_summ$ParameterName[data_summ$ParameterName=="ShellHeight_mm"] <- "Shell Height"
data_summ$ParameterName[data_summ$ParameterName=="Density_m2"] <- "Density"
data_summ$ParameterName[data_summ$ParameterName=="PercentLive_pct"] <- "Percent Live"


table <- merge.data.frame(data_summ, table, by=c("ManagedAreaName", "ParameterName",
                                                 "ShellType", "SizeClass", "HabitatType"),
                             all=TRUE)
#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
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

#Write output table to a csv and pipe-delimited txt file
fwrite(table, "output/Tables/Oyster_results_All.txt", sep="|")
fwrite(table, "output/Tables/Oyster_results_All.csv", sep=",")

###### Compile data used for plots

#List all of the files in the "Tables" directory that are Shell Heights
file_list <- list.files("data/GLMMs/AllDates/Data", pattern="sho25", full.names=TRUE)

for(i in 1:length(file_list)){
  if(i==1){
    data <- readRDS(file_list[i])
    data$ProgramID <- as.character(data$ProgramID)
  } else{
    temp_data <- readRDS(file_list[i])
    temp_data$ProgramID <- as.character(temp_data$ProgramID)
    data <- bind_rows(data, temp_data)
  }
}

#Write output table to a csv and pipe-delimited txt file
fwrite(table, "output/Tables/Oyster_data_ShellHeight.txt", sep="|")
fwrite(table, "output/Tables/Oyster_data_ShellHeight.csv", sep=",")


#List all of the files in the "tables" directory that are Density
file_list <- list.files("data/GLMMs/AllDates/Data", pattern="_n_", full.names=TRUE)

for(i in 1:length(file_list)){
  if(i==1){
    data <- readRDS(file_list[i])
    data$ProgramID <- as.character(data$ProgramID)
  } else{
    temp_data <- readRDS(file_list[i])
    temp_data$ProgramID <- as.character(temp_data$ProgramID)
    data <- bind_rows(data, temp_data)
  }
}

#Write output table to a csv and pipe-delimited txt file
fwrite(table, "output/Tables/Oyster_data_Density.txt", sep="|")
fwrite(table, "output/Tables/Oyster_data_Density.csv", sep=",")


#List all of the files in the "tables" directory that are Density
file_list <- list.files("data/GLMMs/AllDates/Data", pattern="_p_", full.names=TRUE)

for(i in 1:length(file_list)){
  if(i==1){
    data <- readRDS(file_list[i])
    data$ProgramID <- as.character(data$ProgramID)
  } else{
    temp_data <- readRDS(file_list[i])
    temp_data$ProgramID <- as.character(temp_data$ProgramID)
    data <- bind_rows(data, temp_data)
  }
}

#Write output table to a csv and pipe-delimited txt file
fwrite(table, "output/Tables/Oyster_data_PctLive.txt", sep="|")
fwrite(table, "output/Tables/Oyster_data_PctLive.csv", sep=",")