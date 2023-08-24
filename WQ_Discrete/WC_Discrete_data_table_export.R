library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(stringr)
library(tictoc)

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
  #output <- na.omit(output)
  
  # VQSummary conditions for qualifying VQ values
  vq_condition <- output$N_H !=0 | output$N_I != 0 | output$N_Q != 0 | output$N_S != 0 | output$N_U != 0
  
  # Execute condition and filter dataframe, export separate excel file
  if (table == 'VQSummary'){
    filtered_output <- output[vq_condition, ]
    openxlsx::write.xlsx(filtered_output, here::here(paste0("output/tables/",table,"_Qualified.xlsx")), colNames = c(TRUE, TRUE), colWidths = c("auto", "auto"), firstRow = c(TRUE, TRUE))
  }
  
  # Get list of managed areas for each output file
  ma_list <- unique(output$ManagedAreaName)
  
  # Looping through managed areas
  for (ma in ma_list) {
    
    # abbreviated ma names for folder paths
    ma_short <- gsub("[^::A-Z::]","", ma)
    
    # creating folder paths to avoid errors
    output_path <- paste0("output/managedareas/",ma_short)
    for (path in output_path) {if(!dir.exists(path)){dir.create(path, recursive = TRUE)}}
    
    # filter each data file for selected managed area
    ma_output <- output %>% filter(ManagedAreaName == ma)
    
    # write each output to excel for each managed area
    openxlsx::write.xlsx(ma_output, here::here(paste0(output_path,"/",table,".xlsx")), colNames = c(TRUE, TRUE), colWidths = c("auto", "auto"), firstRow = c(TRUE, TRUE))
    
    print(paste0("Wrote ", table, ".xlsx to file for ", ma))
    
    # Export separate VQ summary file where VQ values > 0 for each managed area
    if (table == 'VQSummary'){
      ma_filtered_output <- filtered_output %>% filter(ManagedAreaName == ma)
      openxlsx::write.xlsx(ma_filtered_output, here::here(paste0(output_path,"/",table,"_Qualified.xlsx")), colNames = c(TRUE, TRUE), colWidths = c("auto", "auto"), firstRow = c(TRUE, TRUE))
    }
  }
  
  # write overall output for all managed areas to excel file for internal use
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