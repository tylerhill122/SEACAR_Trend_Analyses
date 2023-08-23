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
library(lubridate)
library(rstudioapi)
library(tictoc)
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
      "Turbidity",
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

# Creates folders for outputs
folder_paths <- c("output/tables")
for (path in folder_paths) {if(!dir.exists(path)){dir.create(path)}}

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
      out_dir_tables <- paste0(out_dir, "/tables")
      out_dir_param <- paste0(out_dir, "/", param_abrev)
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
                          param_name=="Chlorophyll_a_uncorrected_for_pheophytin" |
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
              
              ###########################
              ### BEGIN DATA CREATION ###
              ###########################
              
              ###################
              ### FILE IMPORT ###
              ###################
              
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
              
              #################
              ### FILTERING ###
              #################
              
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
              n <- length(MA_Include)
              # Get list of and number of managed areas that are excluded from analysis
              MA_Exclude <- MA_Summ[MA_Summ$N_Years<10 & MA_Summ$N_Years>0,]
              MA_Exclude <- MA_Exclude[,c("ManagedAreaName", "N_Years")]
              z <- nrow(MA_Exclude)
              
              ################################
              ### DETERMING ValueQualifers ###
              ################################
              
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
              # Writes the ValueQualifier summary to a RDS file
              
              saveRDS(data_summ, file = paste0(out_dir_tables,"/WC_Discrete_", param_abrev, "_", activity, "_", depth, "_VQSummary.rds"))
              
              rm(data_summ)
              
              ###############################
              ### MANAGED AREA STATISTICS ###
              ###############################
              
              # Create summary statistics for each managed area based on Year and Month
              # intervals.
              MA_YM_Stats <- data[data$Use_In_Analysis==TRUE, ] %>%
                group_by(AreaID, ManagedAreaName, Year, Month) %>%
                summarize(ParameterName=parameter,
                          RelativeDepth=depth,
                          ActivityType=activity,
                          N_Data=length(ResultValue),
                          Min=min(ResultValue),
                          Max=max(ResultValue),
                          Median=median(ResultValue),
                          Mean=mean(ResultValue),
                          StandardDeviation=sd(ResultValue),
                          Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                                         collapse=', '),
                          ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                                           collapse=', '))
              # Puts the data in order based on ManagedAreaName, Year, then Month
              MA_YM_Stats <- as.data.table(MA_YM_Stats[order(MA_YM_Stats$ManagedAreaName,
                                                             MA_YM_Stats$Year,
                                                             MA_YM_Stats$Month), ])
              # Writes summary statistics to file
              
              saveRDS(MA_YM_Stats, file = paste0(out_dir_tables,"/WC_Discrete_", param_abrev, "_", activity, "_", depth, "_MA_MMYY_Stats.rds"))
              
              # Get year from start for each managed area to be used in SKT analysis
              MA_YM_Stats <- MA_YM_Stats %>%
                group_by(AreaID, ManagedAreaName) %>%
                mutate(YearFromStart=Year-min(Year))
              # Create decimal value of year and month values
              MA_YM_Stats$YearMonthDec <- MA_YM_Stats$Year + ((MA_YM_Stats$Month-0.5) / 12)
              # Create summary statistics for each managed area based on Year intervals.
              MA_Y_Stats <- data[data$Use_In_Analysis==TRUE, ] %>%
                group_by(AreaID, ManagedAreaName, Year) %>%
                summarize(ParameterName=parameter,
                          RelativeDepth=depth,
                          ActivityType=activity,
                          N_Data=length(ResultValue),
                          Min=min(ResultValue),
                          Max=max(ResultValue),
                          Median=median(ResultValue),
                          Mean=mean(ResultValue),
                          StandardDeviation=sd(ResultValue),
                          Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                                         collapse=', '),
                          ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                                           collapse=', '))
              # Puts the data in order based on ManagedAreaName then Year
              MA_Y_Stats <- as.data.table(MA_Y_Stats[order(MA_Y_Stats$ManagedAreaName,
                                                           MA_Y_Stats$Year), ])
              # Writes summary statistics to file
              
              saveRDS(MA_Y_Stats, file = paste0(out_dir_tables,"/WC_Discrete_", param_abrev, "_", activity, "_", depth, "_MA_Yr_Stats.rds"))
              rm(MA_Y_Stats)
              
              # Create summary statistics for each managed area based on Month intervals.
              MA_M_Stats <- data[data$Use_In_Analysis==TRUE, ] %>%
                group_by(AreaID, ManagedAreaName, Month) %>%
                summarize(ParameterName=parameter,
                          RelativeDepth=depth,
                          ActivityType=activity,
                          N_Data=length(ResultValue),
                          Min=min(ResultValue),
                          Max=max(ResultValue),
                          Median=median(ResultValue),
                          Mean=mean(ResultValue),
                          StandardDeviation=sd(ResultValue),
                          Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                                         collapse=', '),
                          ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                                           collapse=', '))
              # Puts the data in order based on ManagedAreaName then Month
              MA_M_Stats <- as.data.table(MA_M_Stats[order(MA_M_Stats$ManagedAreaName,
                                                           MA_M_Stats$Month), ])
              # Writes summary statistics to file
              
              saveRDS(MA_M_Stats, file = paste0(out_dir_tables,"/WC_Discrete_", param_abrev, "_", activity, "_", depth, "_MA_Mo_Stats.rds"))
              rm(MA_M_Stats)
              
              #################################
              ### MONITORING LOCATION STATS ###
              #################################
              
              # Gets summary statistics for monitoring locations, which are defined as unique
              # combinations of ManagedAreaName, ProgramID, And ProgramLocationID
              Mon_Stats <- data[data$Use_In_Analysis==TRUE, ] %>%
                group_by(AreaID, ManagedAreaName, ProgramID, ProgramName,
                         ProgramLocationID) %>%
                summarize(ParameterName=parameter,
                          RelativeDepth=depth,
                          ActivityType=activity,
                          EarliestSampleDate=min(SampleDate),
                          LastSampleDate=max(SampleDate),
                          N_Data=length(ResultValue),
                          Min=min(ResultValue),
                          Max=max(ResultValue),
                          Median=median(ResultValue),
                          Mean=mean(ResultValue),
                          StandardDeviation=sd(ResultValue))
              # Order data rows by ManagedAreaName, ProgramName, ProgramID, then
              # ProgramLocationID
              Mon_Stats <- as.data.table(Mon_Stats[order(Mon_Stats$ManagedAreaName,
                                                         Mon_Stats$ProgramName,
                                                         Mon_Stats$ProgramID, 
                                                         Mon_Stats$ProgramLocationID), ])
              # Write summary statistics to file
              saveRDS(Mon_Stats, file = paste0(out_dir_tables,"/WC_Discrete_", param_abrev, "_", activity, "_", depth, "_MonLoc_Stats.rds"))
              rm(Mon_Stats)
              
              ####################
              ### SKT ANALYSIS ###
              ####################
              
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
              saveRDS(select(skt_stats, -c(EarliestSampleDate)), file = paste0(out_dir_tables,"/WC_Discrete_", param_abrev, "_", activity, "_", depth, "_KendallTau_Stats.rds"))
              
              
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
              
              # ######################################
              # ### SCATTER PLOT OF ENTIRE DATASET ###
              # ######################################
              # 
              # # Defines standard plot theme: black and white, no major or minor grid lines,
              # # Arial font. Title is centered, size 12, and blue (hex coded). Subtitle is
              # # centered, size 10, and blue (hex coded). Legend title is size 10 and the
              # # legend is left-justified. X-axis title is size 10 and the margins are padded
              # # at the top and bottom to give more space for angled axis labels. Y-axis title
              # # is size 10 and margins are padded on the right side to give more space for
              # # axis labels. Axis labels are size 10 and the x-axis labels are rotated -45
              # # degrees with a horizontal justification that aligns them with the tick mark
              # plot_theme <- theme_bw() +
              #   theme(panel.grid.major = element_blank(),
              #         panel.grid.minor = element_blank(),
              #         text=element_text(family="Arial"),
              #         plot.title=element_text(hjust=0.5, size=12, color="#314963"),
              #         plot.subtitle=element_text(hjust=0.5, size=10, color="#314963"),
              #         legend.title=element_text(size=10),
              #         legend.text.align = 0,
              #         axis.title.x = element_text(size=10, margin = margin(t = 5, r = 0,
              #                                                              b = 10, l = 0)),
              #         axis.title.y = element_text(size=10, margin = margin(t = 0, r = 10,
              #                                                              b = 0, l = 0)),
              #         axis.text=element_text(size=10),
              #         axis.text.x=element_text(angle = 60, hjust = 1))
              # # Gets first and most recent years from data set
              # year_lower <- min(data$Year)
              # year_upper <- max(data$Year)
              # # Gets minimum, mean, and standard deviation of ResultValue for setting y-axis
              # # scale
              # min_RV <- min(data$ResultValue)
              # mn_RV <- mean(data$ResultValue[data$ResultValue <
              #                                  quantile(data$ResultValue, 0.98)])
              # sd_RV <- sd(data$ResultValue[data$ResultValue <
              #                                quantile(data$ResultValue, 0.98)])
              # x_scale <- ifelse(year_upper - year_lower > 30, 10, 5)
              # y_scale <- mn_RV + 4 * sd_RV
              # 
              # # Create plot object for auto-scaled y-axis plot
              # p1 <- ggplot(data=data[data$Include==TRUE,],
              #              aes(x=SampleDate, y=ResultValue, fill=VQ_Plot)) +
              #   geom_point(shape=21, size=3, color="#333333", alpha=0.75) +
              #   labs(subtitle="Autoscale",
              #        x="Year", y=paste0("Values (", unit, ")"),
              #        fill="Value Qualifier") +
              #   plot_theme +
              #   theme(legend.position="top", legend.box="horizontal",
              #         legend.justification="right") +
              #   scale_x_date(labels=date_format("%Y")) +
              #   {if(inc_H==TRUE){
              #     scale_fill_manual(values=c("H"= "#F8766D", "U"= "#00BFC4",
              #                                "HU"="#7CAE00"), na.value="#cccccc")
              #   } else if(param_name=="Secchi_Depth"){
              #     scale_fill_manual(values=c("S"= "#F8766D", "U"= "#00BFC4",
              #                                "SU"="#7CAE00"), na.value="#cccccc")
              #   } else {
              #     scale_fill_manual(values=c("U"= "#00BFC4"), na.value="#cccccc")
              #   }}
              # # Create plot object for y-axis scaled plot
              # p2 <- ggplot(data=data[data$Include==TRUE,],
              #              aes(x=SampleDate, y=ResultValue, fill=VQ_Plot)) +
              #   geom_point(shape=21, size=3, color="#333333", alpha=0.75) +
              #   ylim(min_RV, y_scale) +
              #   labs(subtitle="Scaled to 4x Standard Deviation",
              #        x="Year", y=paste0("Values (", unit, ")")) +
              #   plot_theme +
              #   theme(legend.position="none") +
              #   scale_x_date(labels=date_format("%Y")) +
              #   {if(inc_H==TRUE){
              #     scale_fill_manual(values=c("H"= "#F8766D", "U"= "#00BFC4",
              #                                "HU"="#7CAE00"), na.value="#cccccc")
              #   } else if(param_name=="Secchi_Depth"){
              #     scale_fill_manual(values=c("S"= "#F8766D", "U"= "#00BFC4",
              #                                "SU"="#7CAE00"), na.value="#cccccc")
              #   } else {
              #     scale_fill_manual(values=c("U"= "#00BFC4"), na.value="#cccccc")
              #   }}
              # # Create legend object
              # leg <- get_legend(p1)
              # # Arrange plots and legend
              # pset <- ggarrange(leg, p1 + theme(legend.position="none"), p2,
              #                   ncol=1, heights=c(0.1, 1, 1))
              # # Create title object
              # p0 <- ggplot() + labs(title="Scatter Plot for Entire Dataset") +
              #   plot_theme + theme(panel.border=element_blank(),
              #                      panel.grid.major=element_blank(),
              #                      panel.grid.minor=element_blank(),
              #                      axis.line=element_blank())
              # # Arrange and print title with plots
              # ggarrange(p0, pset, ncol=1, heights=c(0.1, 1))
              # 
              # ################################
              # ### DATASET SUMMARY BOXPLOTS ###
              # ################################
              # ####### GROUPED BY YEAR ########
              # 
              # # Get minimum, mean, and standard deviation of the data
              # min_RV <- min(data$ResultValue[data$Include==TRUE])
              # mn_RV <- mean(data$ResultValue[data$Include==TRUE &
              #                                  data$ResultValue <
              #                                  quantile(data$ResultValue, 0.98)])
              # sd_RV <- sd(data$ResultValue[data$Include==TRUE &
              #                                data$ResultValue <
              #                                quantile(data$ResultValue, 0.98)])
              # # Sets y scale based on data
              # y_scale <- mn_RV + 4 * sd_RV
              # # Create plot object for auto-scaled y-axis plot
              # p1 <- ggplot(data=data[data$Include==TRUE, ],
              #              aes(x=Year, y=ResultValue, group=Year)) +
              #   geom_boxplot(color="#333333", fill="#cccccc", outlier.shape=21,
              #                outlier.size=3, outlier.color="#333333",
              #                outlier.fill="#cccccc", outlier.alpha=0.75) +
              #   labs(subtitle="Autoscale", x="Year",
              #        y=paste0("Values (", unit, ")")) +
              #   plot_theme
              # # Create plot object for y-axis scaled plot
              # p2 <- ggplot(data=data[data$Include==TRUE, ],
              #              aes(x=Year, y=ResultValue, group=Year)) +
              #   geom_boxplot(color="#333333", fill="#cccccc", outlier.shape=21,
              #                outlier.size=3, outlier.color="#333333",
              #                outlier.fill="#cccccc", outlier.alpha=0.75) +
              #   labs(subtitle="Scaled to 4x Standard Deviation", x="Year",
              #        y=paste0("Values (", unit, ")")) +
              #   ylim(min_RV, y_scale) +
              #   plot_theme
              # # Create plot object for y-axis scaled plot for past 10 years
              # p3 <- ggplot(data=data[data$Include==TRUE, ],
              #              aes(x=as.integer(Year), y=ResultValue, group=Year)) +
              #   geom_boxplot(color="#333333", fill="#cccccc", outlier.shape=21,
              #                outlier.size=3, outlier.color="#333333",
              #                outlier.fill="#cccccc", outlier.alpha=0.75) +
              #   labs(subtitle="Scaled to 4x Standard Deviation, Last 10 Years",
              #        x="Year", y=paste0("Values (", unit, ")")) +
              #   ylim(min_RV, y_scale) +
              #   scale_x_continuous(limits=c(max(data$Year) - 10.5, max(data$Year)+1),
              #                      breaks=seq(max(data$Year) - 10, max(data$Year), 2)) +
              #   plot_theme
              # # Arrange plot objects
              # set <- ggarrange(p1, p2, p3, ncol=1)
              # # Create title object for plots
              # p0 <- ggplot() + labs(title="Summary Box Plots for Entire Data",
              #                       subtitle="By Year") + plot_theme +
              #   theme(panel.border=element_blank(), panel.grid.major=element_blank(),
              #         panel.grid.minor=element_blank(), axis.line=element_blank())
              # # Arrange title on plots
              # Yset <- ggarrange(p0, set, ncol=1, heights=c(0.07, 1))
              # 
              # ####### GROUPED BY YEAR/MONTH ########
              # 
              # # Create plot object for auto-scaled y-axis plot
              # p1 <- ggplot(data=data[data$Include==TRUE, ],
              #              aes(x=YearMonthDec, y=ResultValue,
              #                  group=YearMonth, color=as.factor(Month))) +
              #   geom_boxplot(fill="#cccccc", outlier.size=1.5, outlier.alpha=0.75) +
              #   labs(subtitle="Autoscale", x="Year",
              #        y=paste0("Values (", unit, ")"), color="Month") +
              #   plot_theme +
              #   theme(legend.position="top", legend.box="horizontal") +
              #   guides(color=guide_legend(nrow=1))
              # # Create plot object for y-axis scaled plot
              # p2 <- ggplot(data=data[data$Include==TRUE, ],
              #              aes(x=YearMonthDec, y=ResultValue,
              #                  group=YearMonth, color=as.factor(Month))) +
              #   geom_boxplot(fill="#cccccc", outlier.size=1.5, outlier.alpha=0.75) +
              #   labs(subtitle="Scaled to 4x Standard Deviation",
              #        x="Year", y=paste0("Values (", unit, ")")) +
              #   ylim(min_RV, y_scale) +
              #   plot_theme +
              #   theme(legend.position="none", axis.text.x=element_text(face="bold"),
              #         axis.text.y=element_text(face="bold"))
              # # Create plot object for y-axis scaled plot for past 10 years
              # p3 <- ggplot(data=data[data$Include==TRUE, ],
              #              aes(x=YearMonthDec, y=ResultValue,
              #                  group=YearMonth, color=as.factor(Month))) +
              #   geom_boxplot(fill="#cccccc", outlier.size=1.5, outlier.alpha=0.75) +
              #   labs(subtitle="Scaled to 4x Standard Deviation, Last 10 Years",
              #        x="Year", y=paste0("Values (", unit, ")")) +
              #   ylim(min_RV, y_scale) +
              #   scale_x_continuous(limits=c(max(data$Year) - 10.5, max(data$Year)+1),
              #                      breaks=seq(max(data$Year) - 10, max(data$Year), 2)) +
              #   plot_theme +
              #   theme(legend.position="none")
              # # Create legend item
              # leg <- get_legend(p1)
              # # Arrange plots and legend
              # set <- ggarrange(leg, p1 + theme(legend.position="none"), p2, p3, ncol=1,
              #                  heights=c(0.1, 1, 1, 1))
              # # Create plot title object
              # p0 <- ggplot() + labs(title="Summary Box Plots for Entire Data",
              #                       subtitle="By Year & Month") + plot_theme +
              #   theme(panel.border=element_blank(), panel.grid.major=element_blank(),
              #         panel.grid.minor=element_blank(), axis.line=element_blank())
              # # Arrange plots and title
              # YMset <- ggarrange(p0, set, ncol=1, heights=c(0.07, 1))
              # 
              # ##### GROUPED BY MONTH #####
              # # Create plot object for auto-scaled y-axis plot
              # p1 <- ggplot(data=data[data$Include==TRUE, ],
              #              aes(x=Month, y=ResultValue,
              #                  group=Month, fill=as.factor(Month))) +
              #   geom_boxplot(color="#333333", outlier.shape=21, outlier.size=3,
              #                outlier.color="#333333", outlier.alpha=0.75) +
              #   labs(subtitle="Autoscale", x="Month",
              #        y=paste0("Values (", unit, ")"), fill="Month") +
              #   scale_x_continuous(limits=c(0, 13), breaks=seq(3, 12, 3)) +
              #   plot_theme +
              #   theme(legend.position="top", legend.box="horizontal") +
              #   guides(fill=guide_legend(nrow=1))
              # # Create plot object for y-axis scaled plot
              # p2 <- ggplot(data=data[data$Include==TRUE, ],
              #              aes(x=Month, y=ResultValue,
              #                  group=Month, fill=as.factor(Month))) +
              #   geom_boxplot(color="#333333", outlier.shape=21, outlier.size=3,
              #                outlier.color="#333333", outlier.alpha=0.75) +
              #   labs(subtitle="Scaled to 4x Standard Deviation",
              #        x="Month", y=paste0("Values (", unit, ")")) +
              #   ylim(min_RV, y_scale) +
              #   scale_x_continuous(limits=c(0, 13), breaks=seq(3, 12, 3)) +
              #   plot_theme +
              #   theme(legend.position="none")
              # # Create plot object for y-axis scaled plot for past 10 years
              # p3 <- ggplot(data=data[data$Include==TRUE &
              #                          data$Year >= max(data$Year) - 10, ],
              #              aes(x=Month, y=ResultValue,
              #                  group=Month, fill=as.factor(Month))) +
              #   geom_boxplot(color="#333333", outlier.shape=21, outlier.size=3,
              #                outlier.color="#333333", outlier.alpha=0.75) +
              #   labs(subtitle="Scaled to 4x Standard Deviation, Last 10 Years",
              #        x="Month", y=paste0("Values (", unit, ")")) +
              #   ylim(min_RV, y_scale) +
              #   scale_x_continuous(limits=c(0, 13), breaks=seq(3, 12, 3)) +
              #   plot_theme +
              #   theme(legend.position="none")
              # # Create legend object
              # leg <- get_legend(p1)
              # # Arrange plots and legend
              # set <- ggarrange(leg, p1 + theme(legend.position="none"), p2, p3, ncol=1,
              #                  heights=c(0.1, 1, 1, 1))
              # # Create title object for plots
              # p0 <- ggplot() + labs(title="Summary Box Plots for Entire Data",
              #                       subtitle="By Month") + plot_theme +
              #   theme(panel.border=element_blank(), panel.grid.major=element_blank(),
              #         panel.grid.minor=element_blank(), axis.line=element_blank())
              # # Arrange plots and title
              # Mset <- ggarrange(p0, set, ncol=1, heights=c(0.07, 1))
              # 
              # #######################
              # ### PRINT ALL PLOTS ###
              # #######################
              # 
              # if(nrow(data[data$Include==TRUE,])==0){
              #   print("No data passed filtering criteria.")
              # } else {
              #   print(Yset)
              #   print(YMset)
              #   print(Mset)
              # }
              # rm(Yset, YMset, Mset)
              
            }
      }
}
toc()
End_time <- Sys.time()

print(Start_time)
print(End_time)