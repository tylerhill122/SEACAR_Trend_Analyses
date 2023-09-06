library(stringr)
library(dplyr)
library(data.table)
library(ggplot2)

all_depths <- c("Surface","Bottom","All")
all_activities <- c("Field","Lab","All")
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

# function of parameter, activity type, depth, with specified filetype
# retrieves RDS filepath to be loaded
get_files <- function(p, a, d, filetype) {
  
  # Declaring RDS file list of respective tables
  files <- list.files(here::here("output/tables"),pattern = "\\.rds$")
  
  # "data" contains overall data for each param, regardless of depth/activity
  if (filetype == "data") {
    pattern <- paste0(p,"_",filetype)
    
  } else {
    pattern <- paste0(p,"_",a,"_",d,"_",filetype)
  }
  # subset directory files for given pattern
  file_return <- str_subset(files, pattern)
  return(file_return)
}

#function to check the number of managed areas for each p,a,d combination
n_managedareas <- function(p, a, d) {
  # Declaring n value as count of managed areas
  # return 0 if unable to load file (activity/depth combo not available for that param)
  n <- tryCatch(
    {
      ma_file <- get_files(p, a, d, "MA_Include")
      ma_inclusion <- readRDS(paste0("output/tables/", ma_file))
      n <- length(ma_inclusion)
      rm(ma_inclusion)
      n
    },
    error = function(e) {
      0
    },
    warning = function(w) {
      0
    }
  )
  return(n)
}

#function to make a list of managed area names
get_managed_area_names <- function(p, a, d) {
  ma_list <- with(
    readRDS(paste0("output/tables/",get_files(p, a, d, "MA_MMYY"))),
    {
      unique(ManagedAreaName)
    }
  )
  return(list(ma_list))
}

#results list to record managed areas for each combination
results_list <- list()

# for (parameter in all_params_short) {
#   for (depth in all_depths) {
#     for (activity in all_activities) {
#       # n <- length(get_managed_area_names(parameter, activity, depth))
#       n <- n_managedareas(parameter, activity, depth)
#       if (n > 0) {
#         print(n)
#         managed_area_names <- get_managed_area_names(parameter, activity, depth)
# 
#         # Concatenate the managed area names into a single character vector
#         concatenated_names <- unlist(managed_area_names)
# 
#         # Create a data frame for the current combination
#         result_df <- data.frame(Parameter = parameter,
#                                 Depth = depth,
#                                 Activity = activity,
#                                 ManagedAreaName = paste(concatenated_names))
# 
#         # Append the result data frame to the list
#         results_list <- c(results_list, list(result_df))
#         rm(result_df, concatenated_names, managed_area_names, n)
# 
#       } else {
#         print(0)
#       }
#     }
#   }
# }

for (param in all_params_short) {
  if (param == "Secchi"){
    depth <- "Surface"
  } else {
    depth <- "All"
  }
  
  # Choosing which analyses to plot, when to combine 
  if (param == "ChlaC" |
      param == "Chla" |
      param == "CDOM" |
      param == "TN" |
      param == "TP") {activity = "Lab"} else if (
        param == "DO" |
        param == "DOS" |
        param == "pH" |
        param == "Secchi" |
        param == "TempW") {activity = "Field"} else if (
          param == "Sal" |
          param == "TSS" |
          param == "Turb") {activity = "All"}
  
  n <- n_managedareas(param, activity, depth)
  
  if (n > 0) {
    print(n)
    managed_area_names <- get_managed_area_names(param, activity, depth)

    # Concatenate the managed area names into a single character vector
    concatenated_names <- unlist(managed_area_names)

    # Create a data frame for the current combination
    result_df <- data.frame(Parameter = param,
                            Depth = depth,
                            Activity = activity,
                            ManagedAreaName = paste(concatenated_names))

    # Append the result data frame to the list
    results_list <- c(results_list, list(result_df))
    rm(result_df, concatenated_names, managed_area_names, n)

  } else {
    print(0)
  }
}

# Bind the list of data frames using bind_rows()
managed_area_df <- bind_rows(results_list)

## Setting plot theme for plots
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

# Get list of managed areas to create reports for
all_managed_areas <- unique(managed_area_df$ManagedAreaName)
all_managed_areas <- all_managed_areas[2]

# Loop through list of managed areas
for (ma in all_managed_areas) {
  print(ma)
  # determine which analyses to run for each MA
  # variables will be input into RMD file
  ma_df <- managed_area_df %>% filter(ManagedAreaName == ma)
  p_inc <- unique(ma_df$Parameter)
  d_inc <- unique(ma_df$Depth)
  a_inc <- unique(ma_df$Activity)
  
  # Shortened names for managed areas
  ma_short <- gsub("[^::A-Z::]","", ma)
  
  # output path for managed areas, creating folders
  output_path <- paste0("output/managedareas/",ma_short)
  for (path in output_path) {if(!dir.exists(path)){dir.create(path, recursive = TRUE)}}
  
  ### RENDERING ###
  file_out <- paste0(ma_short,"_WC_Discrete_Report")
  rmarkdown::render(input = "WC_Discrete2.Rmd", 
                    output_format = "pdf_document",
                    output_file = paste0(file_out, ".pdf"),
                    output_dir = output_path,
                    clean=TRUE)

  unlink(paste0(output_path, "/", file_out, ".md"))
  # unlink(paste0(output_path, "/", file_out, "_files"), recursive=TRUE)
}