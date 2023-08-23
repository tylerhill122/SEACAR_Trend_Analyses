library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(stringr)
library(tictoc)

all_params <- c(
  "Chlorophyll_a_corrected_for_pheophytin",
  "Chlorophyll_a_uncorrected_for_pheophytin",
  "Colored_dissolved_organic_matter_CDOM",
  "Dissolved_Oxygen",
  "Dissolved_Oxygen_Saturation",
  "pH",
  "Salinity",
  "Secchi_Depth",
  "Total_Nitrogen",
  "Total_Phosphorus",
  "Total_Suspended_Solids_TSS",
  "Turbidity",
  "Water_Temperature"
)

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

table_types <- c("MA_MMYY_Stats","MA_Mo_Stats","MA_Yr_Stats","MonLoc_Stats","VQSummary")

#### Function to create unified data tables ####

create_data_table <- function(table) {
  
  # get list of files
  files <- list.files(here::here("output/tables"),pattern = "\\.rds$")
  
  # select for given table (KT, MonLoc, etc)
  table_file <- paste0("output/tables/",str_subset(files, table))
  
  # create data frame by importing RDS files
  df <- lapply(table_file, readRDS)
  
  # bind each table together
  output <- do.call(rbind, df)
  
  # drop na
  output <- na.omit(output)
  
  # write to excel file
  openxlsx::write.xlsx(output, here::here(paste0("output/tables/",table,".xlsx")), colNames = c(TRUE, TRUE), colWidths = c("auto", "auto"), firstRow = c(TRUE, TRUE))
  
  print(paste0("Wrote ", table, ".xlsx to file"))
}

## Start script
tic()
Start_time <- Sys.time()

print("#### Starting Export ####")

for (t in table_types) {
  print(t)
  create_data_table(t)
}

print("#### Data Table Exports Complete ####")

toc()
End_time <- Sys.time()

print(Start_time)
print(End_time)