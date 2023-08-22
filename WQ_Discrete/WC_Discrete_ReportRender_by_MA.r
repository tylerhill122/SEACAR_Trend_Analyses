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
library(rstudioapi)
library(tictoc)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(scales)
library(EnvStats)
library(tidyr)
library(kableExtra)

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
  # "Chlorophyll_a_corrected_for_pheophytin",
  # "Chlorophyll_a_uncorrected_for_pheophytin",
  # "Colored_dissolved_organic_matter_CDOM",
  # "Dissolved_Oxygen",
  # "Dissolved_Oxygen_Saturation",
  # "pH",
  # "Salinity",
  # "Secchi_Depth",
  # "Total_Nitrogen",
  # "Total_Phosphorus",
  # "Total_Suspended_Solids_TSS",
  # "Turbidity",
  "Water_Temperature"
)

#Sets the list of parameter abbreviation names to cycle through. This can be edited to limit the number of parameters.
#Easiest way to edit is to comment out undesired parameters.
#If only one parameter is desired, comment out all other parameters and delete comma after remaining parameter
all_params_short <- c(
  # "ChlaC",
  # "Chla",
  # "CDOM",
  # "DO",
  # "DOS",
  # "pH",
  # "Sal",
  # "Secchi",
  # "TN",
  # "TP",
  # "TSS",
  # "Turb",
  "TempW"
)

#Sets the list of relative depths to cycle through. This can be edited to limit the number of depths.
#If only one depth is desired, comment out the other depth and delete comma after remaining depth
all_depths <- c(
  #   "Surface",
  #   "Bottom",
  "All"
)

#Sets the list of activity types to cycle through. This can be edited to limit the number of types.
#If only one type is desired, comment out the other type and delete comma after remaining type
all_activity <- c(
  # "Field",
  # "Lab",
  "All"
)

#Loads data file with list on managed area names and corresponding area IDs and short names
MA_All <- fread("data/ManagedArea.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE,
                na.strings = "")

###############################
#### DATA READ IN FROM RMD ####
###############################

data <- fread(file_in, sep="|", header=TRUE, stringsAsFactors=FALSE,
              select=c("ManagedAreaName", "ProgramID", "ProgramName",
                       "ProgramLocationID", "SampleDate", "Year", "Month",
                       "RelativeDepth", "ActivityType", "ParameterName",
                       "ResultValue", "ParameterUnits", "ValueQualifier",
                       "SEACAR_QAQCFlagCode", "Include"),
              na.strings=c("NULL","","NA"))

parameter <- unique(data$ParameterName)
unit <- unique(data$ParameterUnits)
cat(paste("The data file(s) used:", file_short, sep="\n"))

# Removes data rows with missing ResultValue
data <- data[!is.na(data$ResultValue),]
# Changes "Sample" to "Lab" for ActivityType
data$ActivityType <- gsub("Sample", "Lab", data$ActivityType)

# Gets data for the specific activity type if it is not All
if(activity!="All"){
  data <- data[grep(activity, data$ActivityType),]
}

# Changes RelativeDepth to Bottom for the QAQC flag 12Q that indicates
# measurements are both surface and bottom if the relative depth is bottom
if(depth=="Bottom"){
  data$RelativeDepth[grep("12Q", data$SEACAR_QAQCFlagCode[
    data$RelativeDepth=="Surface"])] <- "Bottom"
}
# Removes missing RelativeDepth data and data for RelativeDepth not of interest
# from all parameters except Secchi_Depth
if(param_name!="Secchi_Depth" & depth!="All"){
  data <- data[!is.na(data$RelativeDepth),]
  data <- data[data$RelativeDepth==depth,]
}

# Removes data rows that have "Blank" as an ActivityType
if(length(grep("Blank", data$ActivityType))>0){
  data <- data[-grep("Blank", data$ActivityType),]
}

# Removes data rows with ResultValue below 0, or -2 for Water_Temperature
if(param_name=="Water_Temperature"){
  data <- data[data$ResultValue>=-2,]
} else{
  data <- data[data$ResultValue>=0,]
}
# Changes Include to be either TRUE or FALSE
data$Include <- as.logical(data$Include)
# Changes Include to be TRUE for ProgramID 476 if it had the H value qualifier
data$Include[grep("H", data$ValueQualifier[data$ProgramID==476])] <- TRUE
# Change Include to be FALSE for Secchi_Depth with U value qualifier
if(param_name=="Secchi_Depth"){
  data$Include[grep("U", data$ValueQualifier)] <- FALSE
}
# Gets AreaID for data by merging data with the managed area list
data <- merge.data.frame(MA_All[,c("AreaID", "ManagedAreaName")],
                         data, by="ManagedAreaName", all=TRUE)
# Creates function to checks managed area for at least 2 years of
# continuous consecutive data
DiscreteConsecutiveCheck <- function(con_data){
  # Gets AreaIDs
  IDs <- unique(con_data$AreaID[con_data$Include==TRUE &
                                  !is.na(con_data$Include)])
  # Loops through each AreaID
  for(i in 1:length(IDs)) {
    # Gets list of Years for AreaID
    Years <- unique(con_data$Year[con_data$AreaID==IDs[i] &
                                    con_data$Include==TRUE &
                                    !is.na(con_data$Include)])
    # Puts Years in order
    Years <- Years[order(Years)]
    # If there are fewer than 2 years, skip to next AreaID
    if(length(Years)<2) {
      next
    }
    # Starts loop to make sure there are at least 2 consecutive years
    # with consecutive months of data
    for(j in 2:length(Years)) {
      # If adjacent year entries are not 1 year apart, skip to the
      # next set of year entries
      if(Years[j]-Years[j-1]!=1) {
        next
      }
      # Gets the list of months from the first year
      Months1 <- unique(con_data$Month[
        con_data$AreaID==IDs[i] &
          con_data$Year==Years[j-1] &
          con_data$Include==TRUE &
          !is.na(con_data$Include)])
      # Gets list of months for the second year
      Months2 <- unique(con_data$Month[
        con_data$AreaID==IDs[i] &
          con_data$Year==Years[j] &
          con_data$Include==TRUE &
          !is.na(con_data$Include)])
      # If there are more than 2 months shared between the two
      # years, the AreaID passes the check and is stored
      if(length(intersect(Months1, Months2))>=2) {
        # Creates variable for stored AreaID if it
        # doesn't exist
        if(exists("consecutive")==FALSE){
          consecutive <- IDs[i]
          break
          # Adds to variable for storing AreaID if does exist
        } else{
          consecutive <- append(consecutive, IDs[i])
          break
        }
      }
    }
  }
  # After going through all AreaID, return variable with list of all
  # that pass
  return(consecutive)
}
# Stores the AreaID that pass the consecutive year check
consMonthIDs <- DiscreteConsecutiveCheck(data)

# Creates data frame with summary for each managed area
MA_Summ <- data %>%
  group_by(AreaID, ManagedAreaName) %>%
  summarize(ParameterName=parameter,
            RelativeDepth=depth,
            ActivityType=activity,
            N_Data=length(ResultValue[Include==TRUE & !is.na(ResultValue)]),
            N_Years=length(unique(Year[Include==TRUE & !is.na(Year)])),
            EarliestYear=min(Year[Include==TRUE & N_Data!=0]),
            LatestYear=max(Year[Include==TRUE & N_Data!=0]),
            EarliestSampleDate=min(SampleDate[Include==TRUE]),
            LastSampleDate=max(SampleDate[Include==TRUE]),
            ConsecutiveMonths=ifelse(unique(AreaID) %in%
                                       consMonthIDs==TRUE, TRUE, FALSE),
            # Determines if monitoring location is sufficient for analysis
            # based on having more than 0 data entries, more than the
            # sufficient number of year, and the consecutive month criteria
            SufficientData=ifelse(N_Data>0 & N_Years>=suff_years &
                                    ConsecutiveMonths==TRUE, TRUE, FALSE),
            Median=median(ResultValue[Include==TRUE & N_Data!=0], na.rm=TRUE))

MA_Summ$ConsecutiveMonths <- NULL
# Creates column in data that determines how many years from the start for each
# managed area
data <- data %>%
  group_by(AreaID, ManagedAreaName) %>%
  mutate(YearFromStart=Year-min(Year))
# Adds SufficientData column to data table based on managed area
data <- merge.data.frame(data, MA_Summ[,c("ManagedAreaName", "SufficientData")],
                         by="ManagedAreaName")
# Creates Use_In_Analysis column for data that is determined if the row has
# Include value of TRUE and SufficientData value of TRUE
data$Use_In_Analysis <- ifelse(data$Include==TRUE & data$SufficientData==TRUE,
                               TRUE, FALSE)
# Rearranges the summary data frame columns to be AreaID, ManagedAreaName,
# ParameterName RelativeDepth, ActivityType, SufficientData, everything else
MA_Summ <- MA_Summ %>%
  select(AreaID, ManagedAreaName, ParameterName, RelativeDepth, ActivityType,
         SufficientData, everything())
# Puts summary data in order based on managed area
MA_Summ <- as.data.frame(MA_Summ[order(MA_Summ$ManagedAreaName), ])
# Put SampleDate as date object
data$SampleDate <- as.Date(data$SampleDate)
# Creates character object for Month and Year
data$YearMonth <- paste0(data$Month, "-", data$Year)
# Creates variable that puts year and month into a decimal year format
data$YearMonthDec <- data$Year + ((data$Month-0.5) / 12)
# Converts ampleDate to a decimal date
data$DecDate <- decimal_date(data$SampleDate)

# Get list of and number of managed areas that are to be used in analysis
MA_Include <- MA_Summ$ManagedAreaName[MA_Summ$SufficientData==TRUE]

#################################
MA_Include <- MA_Include[c(1,2)]
#################################

n <- length(MA_Include)
# Get list of and number of managed areas that are excluded from analysis
MA_Exclude <- MA_Summ[MA_Summ$N_Years<10 & MA_Summ$N_Years>0,]
MA_Exclude <- MA_Exclude[,c("ManagedAreaName", "N_Years")]
z <- nrow(MA_Exclude)

###############################
####### END DATA READ IN ######
###############################


# Find out how much total data exists and how much passed the initial filters
total <- length(data$Include)
pass_filter <- length(data$Include[data$Include==TRUE])
# Get the number and percentage of data entries impacted by value qualifier H
count_H <- length(grep("H", data$ValueQualifier[data$ProgramID==476]))
perc_H <- 100*count_H/length(data$ValueQualifier)
# Get the number and percentage of data entries impacted by value qualifier I
count_I <- length(grep("I", data$ValueQualifier))
perc_I <- 100*count_I/length(data$ValueQualifier)
# Get the number and percentage of data entries impacted by value qualifier Q
count_Q <- length(grep("Q", data$ValueQualifier))
perc_Q <- 100*count_Q/length(data$ValueQualifier)
# Get the number and percentage of data entries impacted by value qualifier S
count_S <- length(grep("S", data$ValueQualifier))
perc_S <- 100*count_S/length(data$ValueQualifier)
# Get the number and percentage of data entries impacted by value qualifier U
count_U <- length(grep("U", data$ValueQualifier))
perc_U <- 100*count_U/length(data$ValueQualifier)
# Copy ValueQualifier to a new VQ_Plot to create codes for plots
data$VQ_Plot <- data$ValueQualifier
# Determine if data with value qualifier H should be included for plots based
# on the parameter being observed
inc_H <- ifelse(param_name=="pH" | param_name=="Dissolved_Oxygen" |
                  param_name=="Dissolved_Oxygen_Saturation", TRUE, FALSE)
# Loops through conditions to determine what indicators to include in plots.
# If H should be included
if (inc_H==TRUE){
  # Remove any Value qualifiers that aren't H or U
  data$VQ_Plot <- gsub("[^HU]+", "", data$VQ_Plot)
  # Standardize order of qualifiers. Puts UH as HU
  data$VQ_Plot <- gsub("UH", "HU", data$VQ_Plot)
  # Remove anything from ValueQualifier that isn't U from programs and that
  # aren't ProgramID 476
  data$VQ_Plot[na.omit(data$ProgramID!=476)] <-
    gsub("[^U]+", "", data$VQ_Plot[na.omit(data$ProgramID!=476)])
  # Changes blank character strings to NA
  data$VQ_Plot[data$VQ_Plot==""] <- NA
  # Prints the number and percentage of H, I, Q, U value qualifiers
  cat(paste0("Number of Measurements: ", total,
             ", Number Passed Filter: ", pass_filter, "\n",
             "Program 476 H Codes: ", count_H, " (", round(perc_H, 6), "%)\n",
             "I Codes: ", count_I, " (", round(perc_I, 6), "%)\n",
             "Q Codes: ", count_Q, " (", round(perc_Q, 6), "%)\n",
             "U Codes: ", count_U, " (", round(perc_U, 6), "%)"))
  # If Parameter is Secchi_Depth
} else if (param_name=="Secchi_Depth") {
  # Count the number of S ValueQualifier
  count_S <- length(grep("S", data$ValueQualifier))
  # Get percentage of S ValueQualifier
  perc_S <- 100*count_S/length(data$ValueQualifier)
  # Remove anything from ValueQualifier that isn't S or U
  data$VQ_Plot <- gsub("[^SU]+", "", data$VQ_Plot)
  # Change all ValueQualifier that are US to be US, standardizes codes
  data$VQ_Plot <- gsub("US", "SU", data$VQ_Plot)
  # Sets any blank character ValueQualifier to be NA
  data$VQ_Plot[data$VQ_Plot==""] <- NA
  # Prints the number and percentage of I, Q, S, U
  cat(paste0("Number of Measurements: ", total,
             ", Number Passed Filter: ", pass_filter, "\n",
             "I Codes: ", count_I, " (", round(perc_I, 6), "%)\n",
             "Q Codes: ", count_Q, " (", round(perc_Q, 6), "%)\n",
             "S Codes: ", count_S, " (", round(perc_S, 6), "%)\n",
             "U Codes: ", count_U, " (", round(perc_U, 6), "%)"))
  # For all other scenarios
} else{
  # Remove all ValueQualifier except U
  data$VQ_Plot <- gsub("[^U]+", "", data$VQ_Plot)
  # Sets any blank character ValueQualifier to be NA
  data$VQ_Plot[data$VQ_Plot==""] <- NA
  # Prints the number and percentage of I, Q, U
  cat(paste0("Number of Measurements: ", total,
             ", Number Passed Filter: ", pass_filter, "\n",
             "I Codes: ", count_I, " (", round(perc_I, 6), "%)\n",
             "Q Codes: ", count_Q, " (", round(perc_Q, 6), "%)\n",
             "U Codes: ", count_U, " (", round(perc_U, 6), "%)"))
}
# Creates a data table that summarizes the number and percentage of
# ValueQualifier H, I, Q, S, and U for each managed area each year
data_summ <- data %>%
  group_by(AreaID, ManagedAreaName, Year) %>%
  summarize(ParameterName=parameter,
            RelativeDepth=depth,
            ActivityType=activity,
            N_Total=length(ResultValue),
            N_AnalysisUse=length(ResultValue[Use_In_Analysis==TRUE]),
            N_H=length(grep("H", ValueQualifier[ProgramID==476])),
            perc_H=100*N_H/length(ValueQualifier),
            N_I=length(grep("I", ValueQualifier)),
            perc_I=100*N_I/length(ValueQualifier),
            N_Q=length(grep("Q", ValueQualifier)),
            perc_Q=100*N_Q/length(ValueQualifier),
            N_S=length(grep("S", ValueQualifier)),
            perc_S=100*N_S/length(ValueQualifier),
            N_U=length(grep("U", ValueQualifier)),
            perc_U=100*N_U/length(ValueQualifier))
# Orders the data table rows based on managed area name
data_summ <- as.data.table(data_summ[order(data_summ$ManagedAreaName,
                                           data_summ$Year), ])

#### SKT ANALYSIS ####

# List for column names
c_names <- c("AreaID", "ManagedAreaName", "Independent", "tau", "p",
             "SennSlope", "SennIntercept", "ChiSquared", "pChiSquared", "Trend")

skt_stats <- data.frame(matrix(ncol = length(c_names), nrow = n))

colnames(skt_stats) <- c_names
# Determines if there are any managed areas to analyze
if(n==0){
  print("There are no managed areas that qualify.")
} else{
  # Starts cycling through managed areas to determine seasonal Kendall Tau
  for (i in 1:n) {
    # Gets the number of rows of data for the managed area
    data_SKT <- MA_YM_Stats[MA_YM_Stats$ManagedAreaName==MA_Include[i], ]
    x <- nrow(data_SKT)
    # Perform analysis if there is more than 1 row
    if (x>0) {
      # Store the managed area summary statistics to be used in
      # trend analysis
      SKT.med <- MA_Summ$Median[MA_Summ$ManagedAreaName==MA_Include[i]]
      SKT.minYr <- MA_Summ$EarliestYear[MA_Summ$ManagedAreaName==
                                          MA_Include[i]]
      SKT.maxYr <- MA_Summ$LatestYear[MA_Summ$ManagedAreaName==MA_Include[i]]
      SKT.ind <- TRUE
      SKT <- kendallSeasonalTrendTest(y=data_SKT$Mean,
                                      season=data_SKT$Month,
                                      year=data_SKT$YearFromStart,
                                      independent.obs=SKT.ind)
      if(is.na(SKT$estimate[1])==TRUE){
        SKT.ind <- FALSE
        SKT <- kendallSeasonalTrendTest(y=data_SKT$Mean,
                                        season=data_SKT$Month,
                                        year=data_SKT$YearFromStart,
                                        independent.obs=SKT.ind)
      }
      skt_stats$AreaID[i] <-
        MA_Summ$AreaID[MA_Summ$ManagedAreaName==MA_Include[i]]
      skt_stats$ManagedAreaName[i] <-
        MA_Summ$ManagedAreaName[MA_Summ$ManagedAreaName==MA_Include[i]]
      skt_stats$Independent[i] <- SKT.ind
      skt_stats$tau[i] <- SKT$estimate[1]
      skt_stats$p[i] <- SKT$p.value[2]
      skt_stats$SennSlope[i] <- SKT$estimate[2]
      skt_stats$SennIntercept[i] <- SKT$estimate[3]
      skt_stats$ChiSquared[i] <- SKT$statistic[1]
      skt_stats$pChiSquared[i] <- SKT$p.value[1]
      # If the p value is less than 5% and the slope is greater than 10% of the
      # median value, the trend is large (2).
      if (skt_stats$p[i] < .05 & abs(skt_stats$SennSlope[i]) >
          abs(SKT.med) / 10.) {
        skt_stats$Trend[i] <- 2
        
        # If the p value is less than 5% and the slope is less than 10% of the
        # median value, there is a trend (1).
      }else if (skt_stats$p[i] < .05 & abs(skt_stats$SennSlope[i]) <
                abs(SKT.med) / 10.) {
        skt_stats$Trend[i] <- 1
        
        # Otherwise, there is no trend (0)
      }else {
        skt_stats$Trend[i] <- 0
      }
      # Sets the sign of the trend based on Senn Slope direction
      if (skt_stats$SennSlope[i] <= 0) {
        skt_stats$Trend[i] <- -skt_stats$Trend[i]
      }
    }
  }
  
  # Stores as data frame
  skt_stats <- as.data.frame(skt_stats)
  
}
# Clears unused variables
rm(SKT, data_SKT, x, SKT.med, SKT.minYr, SKT.maxYr, SKT.ind)
# Combines the skt_stats with MA_Summ
skt_stats <-  merge.data.frame(MA_Summ, skt_stats,
                               by=c("AreaID","ManagedAreaName"), all=TRUE)

skt_stats <- as.data.table(skt_stats[order(skt_stats$ManagedAreaName), ])

# Sets variables to proper format and rounds values if necessary
skt_stats$tau <- round(as.numeric(skt_stats$tau), digits=4)
skt_stats$p <- format(round(as.numeric(skt_stats$p), digits=4),
                      scientific=FALSE)
skt_stats$SennSlope <- as.numeric(skt_stats$SennSlope)
skt_stats$SennIntercept <- as.numeric(skt_stats$SennIntercept)
skt_stats$ChiSquared <- round(as.numeric(skt_stats$ChiSquared), digits=4)
skt_stats$pChiSquared <- round(as.numeric(skt_stats$pChiSquared), digits=4)
skt_stats$Trend <- as.integer(skt_stats$Trend)

# Writes combined statistics to file
fwrite(select(skt_stats, -c(EarliestSampleDate)),
       paste0(out_dir_param,"/WC_Discrete_", param_abrev, "_",
              activity, "_", depth, "_KendallTau_Stats.txt"),
       sep="|")

# Removes data rows with no ResultValue (created by merging with MA_All)
data <- data[!is.na(data$ResultValue),]

# Gets x and y values for starting point for trendline
KT.Plot <- skt_stats %>%
  group_by(ManagedAreaName) %>%
  summarize(x=decimal_date(EarliestSampleDate),
            y=(x-EarliestYear)*SennSlope+SennIntercept)
# Gets x and y values for ending point for trendline
KT.Plot2 <- skt_stats %>%
  group_by(ManagedAreaName) %>%
  summarize(x=decimal_date(LastSampleDate),
            y=(x-EarliestYear)*SennSlope+SennIntercept)
# Combines the starting and endpoints for plotting the trendline
KT.Plot <- bind_rows(KT.Plot, KT.Plot2)
rm(KT.Plot2)
KT.Plot <- as.data.table(KT.Plot[order(KT.Plot$ManagedAreaName), ])
KT.Plot <- KT.Plot[!is.na(KT.Plot$y),]

#### SETTING PLOT THEME ####
plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="Arial"),
        plot.title=element_text(hjust=0.5, size=12, color="#314963"),
        plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
        legend.title=element_text(size=10),
        legend.text.align = 0,
        axis.title.x = element_text(size=10, margin = margin(t = 5, r = 0,
                                                             b = 10, l = 0)),
        axis.title.y = element_text(size=10, margin = margin(t = 0, r = 10,
                                                             b = 0, l = 0)),
        axis.text=element_text(size=10),
        axis.text.x=element_text(angle = 60, hjust = 1))

###############################
######## SCRIPT START #########
###############################

# Determines if there are any managed areas to analyze
if(n==0){
  print("There are no managed areas that qualify.") 
} else {
  # Looping through included Managed Areas
  for (ma in MA_Include) {
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
      
      # to display file-name in document (without path)
      file_short <- sub("data/", "", file_in)
      # ma_short to create abbreviated folder names for each managed area
      ma_short <- gsub("[^::A-Z::]","", ma)
      out_dir_param <- paste0(out_dir, "/", ma_short, "/", param_name)

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
          rmarkdown::render(input = "WC_Discrete_by_MA.Rmd",
                            output_format = "pdf_document",
                            output_file = paste0(file_out, ".pdf"),
                            output_dir = out_dir_param,
                            clean=TRUE,
                            params = list(managedarea = ma))
          rmarkdown::render(input = paste0(out_dir_param, "/", file_out, ".md"),
                            output_format = "word_document",
                            output_file = paste0(file_out, ".docx"),
                            output_dir = out_dir_param,
                            clean=TRUE,
                            params = list(managedarea = ma))
          #Removes unwanted files created in the rendering process
          unlink(paste0(out_dir_param, "/", file_out, ".md"))
          unlink(paste0(out_dir_param, "/", file_out, "_files"), recursive=TRUE)
        }
      }
    }
  }
}

toc()
End_time <- Sys.time()

print(Start_time)
print(End_time)