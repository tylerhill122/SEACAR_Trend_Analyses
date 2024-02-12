## Coastal Wetlands

out_dir_cw <- "output/Data/CoastalWetlands"

param_file <- "SpeciesRichness"

data <- fread(cw_file_in, sep="|", header=TRUE, stringsAsFactors=FALSE,
              na.strings=c("NULL","","NA"))

# Only interested in Percent Cover measurements
data <- data[data$ParameterName=="Percent Cover - Species Composition"]
# Make species group name uniform
data$SpeciesGroup1[data$SpeciesGroup1=="Marsh Succulents"] <- "Marsh succulents"
# Only keep data rows that are Marsh, Marsh succulents, and Mangroves and assoc.
data <- data[SpeciesGroup1=="Marsh"|
               SpeciesGroup1=="Marsh succulents"|
               SpeciesGroup1=="Mangroves and associate", ]
setnames(data, "SpeciesGroup1", "SpeciesGroup")
# Create ParameterName Column
data$ParameterName <- "Species Richness"
parameter <- "Species Richness"

# Sets units for species richness
unit <- "# of species"
data$ParameterUnits <- unit

# Replace instances where NA values imported as blank character string or as "NA"
# data <- replace(data, data=="", NA)
# data <- replace(data, data=="NA", NA)

# Remove rows with missing ManagedAreaName
data <- data[!is.na(data$ManagedAreaName),]
data <- data[data$ManagedAreaName!="NA",]
# Remove rows with missing GenusName
data <- data[!is.na(data$GenusName),]
# Remove rows with missing SpeciesName
data <- data[!is.na(data$SpeciesName),]
# Remove rows with missing Months
data <- data[!is.na(data$Month),]
# Remove rows with missing Years
data <- data[!is.na(data$Year),]
# Set ResultValue to be a number value
data$ResultValue <- as.numeric(data$ResultValue)
# Remove rows where ResultValue is 0
data <- data[data$ResultValue!=0,]
# Remove duplicate rows
data <- data[data$MADup==1,]
# Create variable that combines the genus and species name
data$gensp <- paste(data$GenusName, data$SpeciesName, sep=" ")
# Corrects Managed Area names to be consistent with official names
data$ManagedAreaName[data$ManagedAreaName=="Apalachicola Bay"] <-
  "Apalachicola Bay Aquatic Preserve"
data$ManagedAreaName[data$ManagedAreaName=="Big Bend Seagrasses"] <-
  "Big Bend Seagrasses Aquatic Preserve"
data$ManagedAreaName[data$ManagedAreaName=="Cockroach Bay"] <-
  "Cockroach Bay Aquatic Preserve"
data$ManagedAreaName[data$ManagedAreaName=="Guana River Marsh"] <-
  "Guana River Marsh Aquatic Preserve"
data$ManagedAreaName[data$ManagedAreaName=="Guana Tolomato Matanzas NERR"] <-
  "Guana Tolomato Matanzas National Estuarine Research Reserve"

# Create Species Richness values for groups of unique combinations of
# ManagedAreaName, ProgramID, ProgramName, ProgramLocationID, and SampleDate.
data <- data %>%
  group_by(ManagedAreaName, ProgramID, ProgramName, ProgramLocationID,
           SampleDate, SpeciesGroup) %>%
  summarise(ParameterName=parameter,
            Year=unique(Year), Month=unique(Month),
            SpeciesRichness=length(unique(gensp)))

# Adds AreaID for each managed area by combining the MA_All datatable to the
# data based on ManagedAreaName
data <- merge.data.frame(MA_All[,c("AreaID", "ManagedAreaName")],
                         data, by="ManagedAreaName", all=TRUE)

# Writes this data that is used by the rest of the script to a text file
fwrite(data, paste0(out_dir_cw,"/CoastalWetlands_", param_file, "_UsedData.txt"),
       sep="|")

# Makes sure SampleDate is being stored as a Date object
data$SampleDate <- as.Date(data$SampleDate)

# Creates a variable with the names of all the managed areas that contain
# species observations
MA_Include <- unique(data$ManagedAreaName[!is.na(data$SpeciesRichness)])

# Puts the managed areas in alphabetical order
MA_Include <- MA_Include[order(MA_Include)]

#create global CoastalWetland MA_Include for use in ReportRender.R
cw_managed_areas <- MA_Include

# Determines the number of managed areas used
n <- length(MA_Include)

################
### MA STATS ###
################

# Create summary statistics for each managed area based on Year and Month
# intervals.
MA_YM_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, Year, Month, SpeciesGroup) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(SpeciesRichness)),
            Min=min(SpeciesRichness),
            Max=max(SpeciesRichness),
            Median=median(SpeciesRichness),
            Mean=mean(SpeciesRichness),
            StandardDeviation=sd(SpeciesRichness),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '))
# Puts the data in order based on ManagedAreaName, Year, then Month
MA_YM_Stats <- as.data.table(MA_YM_Stats[order(MA_YM_Stats$ManagedAreaName,
                                               MA_YM_Stats$Year,
                                               MA_YM_Stats$Month), ])
# Writes summary statistics to file
fwrite(MA_YM_Stats, paste0(out_dir_cw,"/CoastalWetlands_", param_file,
                           "_MA_MMYY_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_YM_Stats)

# Create summary statistics for each managed area based on Year intervals
MA_Y_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, Year, SpeciesGroup) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(SpeciesRichness)),
            Min=min(SpeciesRichness),
            Max=max(SpeciesRichness),
            Median=median(SpeciesRichness),
            Mean=mean(SpeciesRichness),
            StandardDeviation=sd(SpeciesRichness),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '))
# Puts the data in order based on ManagedAreaName then Year
MA_Y_Stats <- as.data.table(MA_Y_Stats[order(MA_Y_Stats$ManagedAreaName,
                                             MA_Y_Stats$Year), ])
# Writes summary statistics to file
fwrite(MA_Y_Stats, paste0(out_dir_cw,"/CoastalWetlands_", param_file,
                          "_MA_Yr_Stats.txt"), sep="|")

MA_Y_Stats_cw <- MA_Y_Stats

# Create summary statistics for each managed area based on Month intervals.
MA_M_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, Month, SpeciesGroup) %>%
  summarize(ParameterName=parameter,
            N_Data=length(na.omit(SpeciesRichness)),
            Min=min(SpeciesRichness),
            Max=max(SpeciesRichness),
            Median=median(SpeciesRichness),
            Mean=mean(SpeciesRichness),
            StandardDeviation=sd(SpeciesRichness),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '))
# Puts the data in order based on ManagedAreaName then Month
MA_M_Stats <- as.data.table(MA_M_Stats[order(MA_M_Stats$ManagedAreaName,
                                             MA_M_Stats$Month), ])
# Writes summary statistics to file
fwrite(MA_M_Stats, paste0(out_dir_cw,"/CoastalWetlands_", param_file,
                          "_MA_Mo_Stats.txt"), sep="|")
# Removes variable storing data to improve computer memory
rm(MA_M_Stats)

# Create summary overall statistics for each managed area.
MA_Ov_Stats <- data %>%
  group_by(AreaID, ManagedAreaName, SpeciesGroup) %>%
  summarize(ParameterName=parameter,
            N_Years=length(unique(na.omit(Year))),
            EarliestYear=min(Year),
            LatestYear=max(Year),
            N_Data=length(na.omit(SpeciesRichness)),
            Min=min(SpeciesRichness),
            Max=max(SpeciesRichness),
            Median=median(SpeciesRichness),
            Mean=mean(SpeciesRichness),
            StandardDeviation=sd(SpeciesRichness),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '))
# Puts the data in order based on ManagedAreaName
MA_Ov_Stats <- as.data.table(MA_Ov_Stats[order(MA_Ov_Stats$ManagedAreaName), ])
# Creates Year_MinRichness and Year_MaxRichness columns
MA_Ov_Stats$Year_MinRichness <- NA
MA_Ov_Stats$Year_MaxRichness <- NA

# Loops through each ManagedAreaName.
# Determines what year the minimum and maximum species richness occurred
for(m in 1:nrow(MA_Ov_Stats)){
  # Stores ManagedAreaName for this row
  man <- MA_Ov_Stats$ManagedAreaName[m]
  
  # Skips to next row if there are no data for this combination
  if(MA_Ov_Stats$N_Data[m]==0){
    next
  }
  # Gets subset of data from MA_Y_Stats (yearly summary stats) with this
  # ManagedAreaName
  ds <- MA_Y_Stats[MA_Y_Stats$ManagedAreaName==man,]
  # Gets the minimum and maximum Mean (yearly averages)
  min <- min(ds$Mean)
  max <- max(ds$Mean)
  #Determines what years those minimum and maximum values occured
  year_min <- ds$Year[ds$Mean==min]
  year_max <- ds$Year[ds$Mean==max]
  # Stores the occurrence years of the minimum and maximum into the overall
  # stats for this row
  MA_Ov_Stats$Year_MinRichness[m] <- year_min
  MA_Ov_Stats$Year_MaxRichness[m] <- year_max
}
# Replaces blank ProgramIDs with NA (missing values)
MA_Ov_Stats$ProgramIDs <- replace(MA_Ov_Stats$ProgramIDs,
                                  MA_Ov_Stats$ProgramIDs=="", NA)
MA_Ov_Stats$Programs <- replace(MA_Ov_Stats$Programs,
                                MA_Ov_Stats$Programs=="", NA)
# Write overall statistics to file
fwrite(MA_Ov_Stats, paste0(out_dir_cw,"/CoastalWetlands_", param_file,
                           "_MA_Overall_Stats.txt"), sep="|")
# Removes entries from the overall statistics that do not have data.
# Based on presence or absence of EarliestYear
MA_Ov_Stats <- MA_Ov_Stats[!is.na(MA_Ov_Stats$EarliestYear), ]

MA_Ov_Stats_cw <- MA_Ov_Stats

# Defines standard plot theme: black and white, no major or minor grid lines,
# Arial font. Title is centered, size 12, and blue (hex coded). Subtitle is
# centered, size 10, and blue (hex coded). Legend title is size 10 and the
# legend is left-justified. X-axis title is size 10 and the margins are padded
# at the top and bottom to give more space for angled axis labels. Y-axis title
# is size 10 and margins are padded on the right side to give more space for
# axis labels. Axis labels are size 10 and the x-axis labels are rotated -45
# degrees with a horizontal justification that aligns them with the tick mark
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
        axis.text.x=element_text(angle = -45, hjust = 0))

# Color palette for SEACAR
color_palette <- c("#005396", "#0088B1", "#00ADAE", "#65CCB3", "#AEE4C1",
                            "#FDEBA8", "#F8CD6D", "#F5A800", "#F17B00")
                            
# Defines and sets variable with standardized group colors for plots
group_colors <- c("Marsh"=color_palette[1],
                  "Marsh succulents"=color_palette[2],
                  "Mangroves and associate"=color_palette[3])

# Defines and sets variable with standardized group shapes for plots
group_shapes <- c("Marsh"=21,
                  "Marsh succulents"=22,
                  "Mangroves and associate"=24)

## Plotting function for use in ReportTemplate.Rmd ##

plot_cw <- function(ma, MA_Ov_Stats = "MA_Ov_Stats_cw", MA_Y_Stats = "MA_Y_Stats_cw"){
  
  # Gets data for target managed area
  plot_data <- MA_Y_Stats[MA_Y_Stats$ManagedAreaName==ma]
  # Determines most recent year with available data for managed area
  t_max <- max(MA_Ov_Stats$LatestYear[MA_Ov_Stats$ManagedAreaName==
                                        ma])
  # Determines earliest recent year with available data for managed area
  t_min <- min(MA_Ov_Stats$EarliestYear[MA_Ov_Stats$ManagedAreaName==
                                          ma])
  # Determines how many years of data are present
  t <- t_max-t_min
  
  # Creates break intervals for plots based on number of years of data
  if(t>=30){
    # Set breaks to every 10 years if more than 30 years of data
    brk <- -10
  }else if(t<30 & t>=10){
    # Set breaks to every 5 years if between 30 and 10 years of data
    brk <- -5
  }else if(t<10 & t>=4){
    # Set breaks to every 2 years if between 10 and 4 years of data
    brk <- -2
  }else if(t<4 & t>=2){
    # Set breaks to every year if between 4 and 2 years of data
    brk <- -1
  }else if(t<2){
    # Set breaks to every year if less than 2 years of data
    brk <- -1
    # Sets t_max to be 1 year greater and t_min to be 1 year lower
    # Forces graph to have at least 3 tick marks
    t_max <- t_max+1
    t_min <- t_min-1
  }
  # Determine range of data values for the managed area
  y_range <- max(plot_data$Mean) - min(plot_data$Mean)
  
  # Determines lower bound of y-axis based on data range. Set based on
  # relation of data range to minimum value. Designed to set lower boundary
  # to be 10% of the data range below the minimum value
  y_min <- if(min(plot_data$Mean)-(0.1*y_range)<0){
    # If 10% of the data range below the minimum value is less than 0,
    # set as 0
    y_min <- 0
  } else {
    # Otherwise set minimum bound as 10% data range below minimum value
    y_min <- min(plot_data$Mean)-(0.1*y_range)
  }
  
  # Sets upper bound of y-axis to be 10% of the data range above the
  # maximum value.
  y_max <- max(plot_data$Mean)+(0.1*y_range)
  
  # Determines what combination of groups are present for managed area
  # and subsets color and shape scheme to be used by plots.
  # Used so only group combinations present for managed area appear in
  # the legend.
  group_colors_plot <- group_colors[unique(plot_data$SpeciesGroup)]
  group_shapes_plot <- group_shapes[unique(plot_data$SpeciesGroup)]
  
  # Creates plot object using plot_data.
  # Data is plotted as symbols with connected lines.
  p1 <- ggplot(data=plot_data, group=as.factor(SpeciesGroup)) +
    geom_line(aes(x=Year, y=Mean, color=as.factor(SpeciesGroup)),
              size=0.75, alpha=1) +
    geom_point(aes(x=Year, y=Mean, fill=as.factor(SpeciesGroup),
                   shape=as.factor(SpeciesGroup)), size=2,
               color="#333333", alpha=1) +
    labs(title="Coastal Wetlands Species Richness",
         subtitle=ma,
         x="Year", y="Richness (# of species)",
         fill="Species group", color="Species group",
         shape="Species group") +
    scale_x_continuous(limits=c(t_min-0.25, t_max+0.25),
                       breaks=seq(t_max, t_min, brk)) +
    scale_y_continuous(limits=c(y_min, y_max),
                       breaks=pretty_breaks(n=5)) +
    scale_fill_manual(values=group_colors_plot) +
    scale_color_manual(values=group_colors_plot) +
    scale_shape_manual(values=group_shapes_plot) +
    plot_theme
  # Sets file name of plot created
  outname <- paste0("CoastalWetlands_", param_file, "_",
                    gsub(" ", "", ma), ".png")
  # Saves plot as a png image
  # png(paste0(out_dir_cw, "/Figures/", outname),
  #     width = 8,
  #     height = 4,
  #     units = "in",
  #     res = 200)
  # print(p1)
  # dev.off()
  
  # Creates a data table object to be shown underneath plots in report
  ResultTable <-
    MA_Ov_Stats[MA_Ov_Stats$ManagedAreaName==ma,]
  # Removes location, species group, and parameter information because it is
  # in plot labels
  ResultTable <- ResultTable[,-c("AreaID", "ManagedAreaName",
                                 "ProgramIDs", "Programs", "ParameterName")]
  # Renames StandardDeviation to StDev to save horizontal space
  ResultTable <- ResultTable %>%
    rename("StDev"="StandardDeviation")
  # Converts all non-integer values to 2 decimal places for space
  ResultTable$Min <- round(ResultTable$Min, digits=2)
  ResultTable$Max <- round(ResultTable$Max, digits=2)
  ResultTable$Median <- round(ResultTable$Median, digits=2)
  ResultTable$Mean <- round(ResultTable$Mean, digits=2)
  ResultTable$StDev <- round(ResultTable$StDev, digits=2)
  # Stores as plot table object
  t1 <- ggtexttable(ResultTable, rows = NULL,
                    theme=ttheme(base_size=7))
  # Combines plot and table into one figure
  print(ggarrange(p1, t1, ncol=1, heights=c(0.85, 0.15)))
  cat("  \n")
}