#title: SEACAR Oyster Analyses
#author: Stephen R. Durham (stephen.durham@floridadep.gov)
#date: 11/05/2021"
#description:   This script models change in oyster shell height, live density and percent live over time for FDEP RCP managed areas
#               with a minimum of 5 years of data for each indicator. Currently it is hard-coded for each managed area with 
#               sufficient data, but future updates should incorporate the filtering of managed areas into the modeling to allow for
#               automatically including new managed areas that meet the minimum data requirements in the future.

# NOTE: The "Generate UniversalReefIDs" section requires updating that I did not have time for prior to final code review, 
# so probably will not run on the most up-to-date oyster combined table. 

library(Rmisc)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(here)
library(sf)
library(mapview)
library(rcompanion)
library(data.table)
library(brms)
library(modelr)
library(tidybayes)
library(doFuture)
library(tictoc)
library(doRNG)
library(piecewiseSEM)

session <- sessionInfo()
saveRDS(session, here::here(paste0("SessionInfo_", Sys.Date())))

#load data
oysterraw <- fread(here::here("All_Parameters_but_Hectares-2021-Sep-20.csv"))
#oysterraw <- fread("C:/Users/steph/Downloads/All_Parameters_but_Hectares-2021-Sep-20.csv")


# Generate UniversalReefIDs -----------------------------------------------

#Load spatial data files for RCP managed areas, SEACAR sample locations and FWC statewide oyster reef layer
fwcoymap <- st_read(here::here("OysterGIS_files/Oyster_Beds_in_Florida_2021update/Oyster_Beds_in_Florida.shp"))
aps <- st_read(here::here("OysterGIS_files/APs/Florida_Aquatic_Preserves.shp"))
nerrs <- st_read(here::here("OysterGIS_files/NERRs/Florida_National_Estuarine_Resarch_Reserves__NERR__Boundaries.shp"))
GTMnew <- st_read(here::here("OysterGIS_files/GTM_RB_2016_Merge.kml"))
GTMnew2 <- st_union(GTMnew)
GTMnew <- subset(nerrs, nerrs$SITE_NAME == "Guana Tolomato Matanzas National Estuarine Research Reserve")
GTMnew$geometry <- GTMnew2
othernerrs <- subset(nerrs, nerrs$SITE_NAME != "Guana Tolomato Matanzas National Estuarine Research Reserve")
oysamplelocs <- st_read(here::here("OysterGIS_files/SEACAR_SampleLocationMatching/SampleLocations_12jan21/seacar_dbo_SampleLocation_Point.shp"))
oysterprogs <- unique(oysterraw$ProgramID)
oysamplelocs <- subset(oysamplelocs, oysamplelocs$ProgramID %in% oysterprogs)

#Make sure spatial data are in the same projection
aps_m <- st_transform(aps, 32119)
GTMnew_m <- st_transform(GTMnew, 32119)
othernerrs_m <- st_transform(nerrs, 32119)
fwcoymap_m <- st_transform(fwcoymap, 32119)
oysamplelocs_m <- st_transform(oysamplelocs, 32119)

#Create oyster map file for RCP managed areas
fwcoymap_m_aps <- fwcoymap_m[aps_m, , op = st_intersects]
fwcoymap_m_othernerrs <- fwcoymap_m[othernerrs_m, , op = st_intersects]
fwcoymap_m_GTMnew <- fwcoymap_m[GTMnew_m, , op = st_intersects]
fwcoymap_m_rcp <- unique(rbind(fwcoymap_m_aps, fwcoymap_m_othernerrs))
fwcoymap_m_rcp <- unique(rbind(fwcoymap_m_rcp, fwcoymap_m_GTMnew))

#Create dataframe of oyster sample locations within RCP managed areas that will be used to crosswalk reefIDs from different programIDs
reefcrosswalk_aps <- st_join(oysamplelocs_m, aps_m["LONG_NAME"], join = st_intersects)
setnames(reefcrosswalk_aps, "LONG_NAME", "SITE_NAME")
reefcrosswalk_aps <- subset(reefcrosswalk_aps, !is.na(reefcrosswalk_aps$SITE_NAME))
reefcrosswalk_othernerrs <- st_join(oysamplelocs_m, othernerrs_m["SITE_NAME"], join = st_intersects)
reefcrosswalk_othernerrs <- subset(reefcrosswalk_othernerrs, !is.na(reefcrosswalk_othernerrs$SITE_NAME))
reefcrosswalk_GTMnew <- st_join(oysamplelocs_m, GTMnew_m["SITE_NAME"], join = st_intersects)
reefcrosswalk_GTMnew <- subset(reefcrosswalk_GTMnew, !is.na(reefcrosswalk_GTMnew$SITE_NAME))
reefcrosswalk_rcp <- unique(rbind(reefcrosswalk_aps, reefcrosswalk_othernerrs))
reefcrosswalk_rcp <- unique(rbind(reefcrosswalk_rcp, reefcrosswalk_GTMnew))

#Need to make sure that samples outside of MA boundaries but taken from reefs that are partially within the MA boundaries are included. 
reefcrosswalk_oymap <- st_join(oysamplelocs_m, fwcoymap_m_rcp["OBJECTID"], join = st_intersects)
st_geometry(reefcrosswalk_rcp) <- NULL
reefcrosswalk_rcp <- dplyr::left_join(reefcrosswalk_oymap, reefcrosswalk_rcp)

#Create column to record the closest reef to each sample
reefcrosswalk_rcp$closest <- c(1:nrow(reefcrosswalk_rcp))
for(i in seq_len(nrow(reefcrosswalk_rcp))){
  reefcrosswalk_rcp$closest[i] <- fwcoymap_m_rcp[which.min(st_distance(fwcoymap_m_rcp, reefcrosswalk_rcp[i,])),]$OBJECTID
}

#Create column to record the closest reef to each sample - Parallel version of previous (NOT TESTED YET!)
registerDoFuture()
no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)

tic()
reefcrosswalk_rcp$closest[i] <- foreach(i = seq_len(nrow(reefcrosswalk_rcp)), .packages = c('sf', 'data.table')) %dorng% {
  fwcoymap_m_rcp[which.min(st_distance(fwcoymap_m_rcp, reefcrosswalk_rcp[i,])),]$OBJECTID
}
toc()

#Create match category column to record reef match (or no match) for each sample
reefcrosswalk_rcp$match_cat <- c(1:nrow(reefcrosswalk_rcp))
for(i in seq_len(nrow(reefcrosswalk_rcp))){
  obj_id <- subset(fwcoymap_m_rcp, fwcoymap_m_rcp$OBJECTID == reefcrosswalk_rcp$closest[i])
  reefcrosswalk_rcp$match_cat[i] <- ifelse(st_is_within_distance(reefcrosswalk_rcp[i,], obj_id, dist = 20, sparse = FALSE), reefcrosswalk_rcp$closest[i], "no match")
}

#Create a match index column that will provide unique values for each sample location (so sampleloc metadata will show correctly on the map)
reefcrosswalk_rcp$match_ind <- rep("1", times = nrow(reefcrosswalk_rcp))
for(i in unique(reefcrosswalk_rcp$match_cat)){
  match <- subset(reefcrosswalk_rcp, reefcrosswalk_rcp$match_cat == i)
  match$match_ind <- NULL
  
  #need a reference table for match indexes because some samples appear in overlapping managed areas 
  match_u <- match[, c(1:6, 8:10)]
  match_u$geometry <- NULL
  match_u <- unique(match_u)
  
  match_u$match_ind <- rep("1", times = length(match_u$match_cat))
  
  #create index
  for(j in seq_len(nrow(match_u))){
    match_u$match_ind[j] <- paste0(match_u$match_cat[j], "_", j)
  }
  
  #use reference index table to add indexes to the full data subset for the match category
  match <- left_join(match, match_u)
  #  setcolorder(match, c('LocationID', 'ProgramID', 'ProgramLoc', 'Latitude_D', 'Longitude_', 'SITE_NAME', 'geometry', 'closest', 
  #                       'match_cat', 'match_ind'))
  
  #replace match category data in reef crosswalk table with data updated with match indexes
  everythingelse <- subset(reefcrosswalk_rcp, reefcrosswalk_rcp$match_cat != i)
  reefcrosswalk_rcp <- rbind(everythingelse, match)
}


#Add match category to the FWC oyster map for RCP managed areas
reefcrosswalk_rcp_sum <- reefcrosswalk_rcp %>% dplyr::count(match_cat)
matches <- as.integer(subset(reefcrosswalk_rcp, reefcrosswalk_rcp$match_cat != "no match")$match_cat)
fwcoymap_m_rcp$match <- ifelse(fwcoymap_m_rcp$OBJECTID %in% matches, fwcoymap_m_rcp$OBJECTID, "no match")

#Create crosswalk reef ID column
for(i in seq_len(nrow(reefcrosswalk_rcp))){
  reefcrosswalk_rcp$crosswalk[i] <- ifelse(reefcrosswalk_rcp$match_cat[i] != "no match", reefcrosswalk_rcp$match_cat[i], reefcrosswalk_rcp$LocationID[i])
}

#manually adjust crosswalk values for some reefs in Estero Bay where the polygons appear to have plotted inaccurately
#samples_to_correct <- c(101957, 101956, 918388, 101955, 918389, 918387, 101945, 918335)
#correct_reef_matches <- c(136121, 136117, 136120, 136119, 136119, 136119, 136064, 136064)
samples_to_correct <- c(918390, 101956, 918388, 101955, 918389, 918387, 918337, 918335)
correct_reef_matches <- c(171071, 171067, 171069, 171069, 171069, 171069, 171014, 171014)

for(i in 1:length(samples_to_correct)){
  sample_to_correct <- subset(reefcrosswalk_rcp, reefcrosswalk_rcp$LocationID == samples_to_correct[i])
  sample_to_correct$crosswalk <- correct_reef_matches[i]
  allothersamples <- subset(reefcrosswalk_rcp, reefcrosswalk_rcp$LocationID != samples_to_correct[i])
  reefcrosswalk_rcp <- rbind(allothersamples, sample_to_correct)
}

#Remove samples that were not either within a managed area or matched to a reef that is at least partially within a managed area.
reefcrosswalk_rcp_MA <- subset(reefcrosswalk_rcp, !is.na(reefcrosswalk_rcp$SITE_NAME))
reefcrosswalk_rcp_nMA <- subset(reefcrosswalk_rcp, is.na(reefcrosswalk_rcp$SITE_NAME))
reefcrosswalk_rcp_nMA <- subset(reefcrosswalk_rcp_nMA, !is.na(reefcrosswalk_rcp_nMA$OBJECTID))
reefcrosswalk_rcp <- rbind(reefcrosswalk_rcp_MA, reefcrosswalk_rcp_nMA)

#Fix the special cases where a sample should have been included, but was outside both MA and reef boundaries.
samples_to_add <- c(864711, 864856, 918365, 945699, 945698, 78218, 918364, 864592)
matches_for_samples <- c("192956", "171697", "168801", "Unknown reef", "Unknown reef", "175231", "168801", "192956")
samples_to_add <- data.frame(samples_to_add, matches_for_samples)
samples_to_add <- rename(samples_to_add, "LocationID" = "samples_to_add", "crosswalk" = "matches_for_samples")
samples_to_add$SITE_NAME <- c("Loxahatchee River-Lake Worth Creek Aquatic Preserve", "Loxahatchee River-Lake Worth Creek Aquatic Preserve", "Lemon Bay Aquatic Preserve", "St. Martins Marsh Aquatic Preserve", "St. Martins Marsh Aquatic Preserve", "Guana Tolomato Matanzas National Estuarine Research Reserve", "Lemon Bay Aquatic Preserve", "Loxahatchee River-Lake Worth Creek Aquatic Preserve")
missedsamps <- subset(reefcrosswalk_oymap, reefcrosswalk_oymap$LocationID %in% samples_to_add$LocationID)
missedsamps$closest <- NA
missedsamps$match_cat <- NA
missedsamps$match_ind <- NA
missedsamps <- dplyr::left_join(missedsamps, samples_to_add, by = "LocationID")
reefcrosswalk_rcp <- rbind(reefcrosswalk_rcp, missedsamps)


#Not sure why, but in the end SA22 ends up with SITE_NAME blank, so I correct it manually here.
reefcrosswalk_rcp$SITE_NAME[reefcrosswalk_rcp$ProgramLoc == "SA22"] <- "Guana Tolomato Matanzas National Estuarine Research Reserve"


#Use crosswalk reef IDs to populate new reef ID column in oysterraw data
#for(i in seq_len(nrow(oysterraw))){
#  oysterraw$UniversalReefID[i] <- ifelse(oysterraw$ProgramLocationID[i] %in% reefcrosswalk_rcp$ProgramLoc, 
#                                            subset(reefcrosswalk_rcp, reefcrosswalk_rcp$ProgramLoc == oysterraw$ProgramLocationID[i])$crosswalk, 'no #match')
#}

#Parallel version of previous <- THIS FUNCTION WORKS NOW.
registerDoFuture()
no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)

tic()
oysterraw$UniversalReefID <- foreach(i = seq_len(nrow(oysterraw)), .packages = c('data.table')) %dorng% {
  ifelse(oysterraw$ProgramLocationID[i] %in% reefcrosswalk_rcp$ProgramLoc, 
         subset(reefcrosswalk_rcp, reefcrosswalk_rcp$ProgramLoc == oysterraw$ProgramLocationID[i])$crosswalk, 'no match')
}
toc()

oysterraw[, UniversalReefID := as.character(UniversalReefID)]


#Create a list of all ProgramLocationIDs in the full oysterraw data table (before "no matches" are removed)
shLocations <- unique(oysterraw$ProgramLocationID)

#Verify visually that all "no matches" in oysterraw_sh are from outside RCP managed area boundaries. If they are not, then go back to line 454 and correct the special cases.
nomatches_u <- unique(subset(oysterraw, oysterraw$UniversalReefID == "no match")$ProgramLocationID)
nomatches <- subset(oysamplelocs_m, oysamplelocs_m$ProgramLoc %in% nomatches_u)

mapview(nerrs, color = "yellow", alpha.regions = 0) +
  mapview(aps, color = "yellow", alpha.regions = 0) +
  #mapview(fwcoymap_m) +
  mapview(fwcoymap_m_rcp) +
  mapview(nomatches)

#Remove "no matches" from the oyster program data
oysterraw <- subset(oysterraw, oysterraw$UniversalReefID != "no match")

#Save a .RDS copies to avoid having to run this time consuming code every time I need the oysterraw file or crosswalk map
saveRDS(oysterraw, here::here(paste0("oysterraw_", Sys.Date(), ".rds")))
saveRDS(othernerrs, here::here('OysterGIS_files/reefIDcrosswalk_map_files/othernerrs.rds'))
saveRDS(GTMnew, here::here('OysterGIS_files/reefIDcrosswalk_map_files/GTMnew.rds'))
saveRDS(aps, here::here('OysterGIS_files/reefIDcrosswalk_map_files/aps.rds'))
saveRDS(oysamplelocs_m, here::here('OysterGIS_files/reefIDcrosswalk_map_files/oysamplelocs_m.rds'))
saveRDS(fwcoymap_m_rcp, here::here('OysterGIS_files/reefIDcrosswalk_map_files/fwcoymap_m_rcp.rds'))
saveRDS(reefcrosswalk_rcp, here::here('OysterGIS_files/reefIDcrosswalk_map_files/reefcrosswalk_rcp.rds'))

# #Create an interactive map of UniversalReefIDs
# othernerrs <- readRDS(here::here('OysterGIS_files/reefIDcrosswalk_map_files/othernerrs.rds'))
# GTMnew <- readRDS(here::here('OysterGIS_files/reefIDcrosswalk_map_files/GTMnew.rds'))
# aps <- readRDS(here::here('OysterGIS_files/reefIDcrosswalk_map_files/aps.rds'))
# oysamplelocs_m <- readRDS(here::here('OysterGIS_files/reefIDcrosswalk_map_files/oysamplelocs_m.rds'))
# fwcoymap_m_rcp <- readRDS(here::here('OysterGIS_files/reefIDcrosswalk_map_files/fwcoymap_m_rcp.rds'))
# reefcrosswalk_rcp <- readRDS(here::here('OysterGIS_files/reefIDcrosswalk_map_files/reefcrosswalk_rcp.rds'))
# 
# mapview(othernerrs, color = "yellow", alpha.regions = 0) +
#   mapview(GTMnew, color = "yellow", alpha.regions = 0) +
#   mapview(aps, color = "blue", alpha.regions = 0) +
#   mapview(oysamplelocs_m, col.regions = "grey") +
#   mapview(fwcoymap_m_rcp, zcol = 'match', burst = TRUE) +
#   mapview(reefcrosswalk_rcp, zcol = 'match_ind', burst = TRUE)



# Assign GTM regions to GTM data ------------------------------------------

#Set up data components to look at GTM reef regions
gtmn <- oysterraw[ManagedAreaName == "Guana Tolomato Matanzas NERR", ]
#gtmn_regions <- fread(here::here("GTMregions.csv"))

#temporarily load reefcrosswalk_rcp file until UniversalReefID generation code is fixed
reefcrosswalk_rcp <- readRDS(here::here("OysterGIS_files/reefIDcrosswalk_map_files/reefcrosswalk_rcp.rds"))

#Used gtmn_regions file to create shape files for the regions within GTMNERR boundaries in Google Earth
gtm_oyregions <- st_read(here::here("OysterGIS_files/GTMNERR_Regions.kml"))
gtm_oyregions_m <- st_transform(gtm_oyregions, 32119)

#Assign regions to GTM samples using region polygons
reefcrosswalk_rcp_gtm <- subset(reefcrosswalk_rcp, reefcrosswalk_rcp$SITE_NAME == "Guana Tolomato Matanzas National Estuarine Research Reserve")
reefcrosswalk_rcp_gtm_m <- st_transform(reefcrosswalk_rcp_gtm, 32119)
reefcrosswalk_rcp_gtm_m <- st_join(reefcrosswalk_rcp_gtm_m, gtm_oyregions_m["Name"], join = st_intersects)
setDT(reefcrosswalk_rcp_gtm_m)

rc_reg <- reefcrosswalk_rcp_gtm_m[, c("crosswalk", "Name")]
setnames(rc_reg, c("crosswalk", "Name"), c("UniversalReefID", "Region"))

#Add region names to the raw data file for the oyster analyses
oysterraw2 <- dplyr::left_join(oysterraw, rc_reg, by = 'UniversalReefID')
oysterraw2 <- unique(oysterraw2)

#Test that all GTM NERR data rows have a region name (i.e., no "NA"s)
gtmtest <- oysterraw2[ManagedAreaName == "Guana Tolomato Matanzas NERR", ]
unique(gtmtest$Region.y)

#Update oysterraw object and delete oysterraw2
oysterraw <- oysterraw2
rm(oysterraw2)


# Finish initial data file setup ------------------------------------------

#oysterraw <- readRDS(here::here("oysterraw_2021-02-09_wRegions.rds"))

oysterraw[oysterraw == "NA"] <- NA
oysterraw[, SampleDate := parse_date_time(SampleDate, "bdY IMp", tz = "America/New_York")]

#Make sure column formats are correct - I am still getting an "NAs introduced by coercion" warning on the LiveDate calculation, 
#but I'm not sure what is going on because when I spot-check the output, it does not look like it is introducing NAs...
oysterraw[, `:=` (RowID = as.integer(RowID),
                  ProgramID = as.integer(ProgramID),
                  LocationID = as.integer(LocationID),
                  ProgramName = as.character(ProgramName),
                  ProgramLocationID = as.character(ProgramLocationID),
                  QuadIdentifier = as.character(QuadIdentifier),
                  ReefIdentifier = as.character(ReefIdentifier),
                  UniversalReefID = as.factor(UniversalReefID),
                  LiveDate = as.integer(ifelse(!is.na(LiveDate_Qualifier) & str_detect(LiveDate, "....-..-.."), 
                                               paste0(str_sub(LiveDate, 1, 4)), 
                                               round(as.numeric(LiveDate)))),
                  LiveDate_Qualifier = as.character(LiveDate_Qualifier),
                  LiveDate_MinEstDate = as.numeric(LiveDate_MinEstDate),
                  LiveDate_MaxEstDate = as.numeric(LiveDate_MaxEstDate),
                  SampleAge_Stdev = as.numeric(SampleAge_Stdev),
                  GISUniqueID = as.logical(GISUniqueID),
                  Year = as.integer(Year),
                  Month = as.integer(Month),
                  ManagedAreaName = as.character(ManagedAreaName),
                  Region = as.character(Region),
                  SurveyMethod = as.character(SurveyMethod),
                  PercentLiveMethod = as.character(PercentLiveMethod),
                  HabitatClassification = as.character(HabitatClassification),
                  MinimumSizeMeasured_mm = as.character(MinimumSizeMeasured_mm),
                  NumberMeasured_n = as.character(NumberMeasured_n),
                  QuadSize_m2 = as.factor(QuadSize_m2),
                  MADup = as.integer(MADup),
                  DataFileName = as.character(DataFileName),
                  Density_m2 = as.numeric(Density_m2),
                  PercentLive_pct = as.numeric(PercentLive_pct),
                  ShellHeight_mm = as.numeric(ShellHeight_mm),
                  Number_of_Oysters_Counted_Total_Count = as.integer(Number_of_Oysters_Counted_Total_Count),
                  Number_of_Oysters_Counted_Live_Count = as.integer(Number_of_Oysters_Counted_Live_Count),
                  Number_of_Oysters_Counted_Dead_Count = as.integer(Number_of_Oysters_Counted_Dead_Count),
                  ObsIndex = as.integer(ObsIndex))]

#Temporary fixes for data file issues I have found
  #Some LiveDate_Qualifiers are "Estimate" that shouldn't be
  oysterraw[ProgramID == 5035 & str_detect(QuadIdentifier, "L$"), LiveDate_Qualifier := "Exact"]
  
  #Some LiveDates are NA that shouldn't be
  oysterraw[ProgramID == 5035 & LiveDate_Qualifier == "Exact", LiveDate := as.integer(paste0(year(SampleDate)))]
  
  #Add in NumberMeasured_n value for ID5070 and ID5072; correct UniversalReefID for ID5072
  oysterraw[ProgramID %in% c(5070, 5072), NumberMeasured_n := "ALL"]
  oysterraw[ProgramID == 5072, UniversalReefID := "Unknown reef"]
  
  #Fix QuadID and ReefID columns for 2003 data in program 4014 ***this will not work because the Number_of_Oysters_Counted_Live_Count 
  #column is no longer populated for this program in the newest combined table. I put in a ticket with Claude to fix it.
  oysterraw[ProgramID == 4014 & Year == 2003, `:=` (QuadIdentifier = ProgramLocationID,
                                                     ReefIdentifier = fcase(ProgramLocationID == "14", "13",
                                                                            ProgramLocationID == "13", "12",
                                                                            ProgramLocationID == "12", "11",
                                                                            as.numeric(ProgramLocationID) < 12, ProgramLocationID),
                                                     Density_m2 = Number_of_Oysters_Counted_Live_Count/as.numeric(QuadSize_m2))]
  
  #Calculate Density_m2 values for ProgramID == 4016 & 4042
  oysterraw[ProgramID == 4016, Density_m2 := Number_of_Oysters_Counted_Live_Count/as.numeric(QuadSize_m2)]
  oysterraw[ProgramID == 4042 & !is.na(Number_of_Oysters_Counted_Live_Count), Density_m2 := Number_of_Oysters_Counted_Live_Count/as.numeric(QuadSize_m2)]
  
  #Remove "25" values from total counts column, make all "PercentLiveMethod" values the same, and calculate estimated live Density for ProgramID == 5074 and 
  oysterraw <- oysterraw[RowID %in% setdiff(oysterraw[, RowID], oysterraw[ProgramID == 5074 & Number_of_Oysters_Counted_Total_Count == 25, RowID]), ]
  oysterraw[ProgramID == 5074, PercentLiveMethod := "Estimated percent"]
  oysterraw[ProgramID == 5074, SampleDate := unique(oysterraw[ProgramID == 5074 & !is.na(Number_of_Oysters_Counted_Total_Count), SampleDate])[1]]
  
  #Some PercentLiveMethod values for ID4042 are NA
  oysterraw[ProgramID == 4042 | ProgramID == 4016, PercentLiveMethod := "Point-intercept"]
  
  #Fix multiple spellings of PercentLiveMethod categories
  oysterraw[, PercentLiveMethod := fcase(PercentLiveMethod == "Point-Intercept", "Point-intercept",
                                         PercentLiveMethod == "percent", "Percent")]
  
  
  
#make sure quadrat identifiers are unique
oysterraw[, QuadIdentifier_old := QuadIdentifier]
oysterraw[, QuadIdentifier := paste(UniversalReefID, LocationID, Year, Month, QuadIdentifier_old, sep = "_")] #Note that these QuadIdentifier values DO NOT end up being unique for ReefHeight_mm

oysterraw[, MA_plotlab := paste0(ManagedAreaName, "_", HabitatClassification)]
subtidal <- c(4044, 5007, 5071, 5073)
oysterraw[, Subtidal := ifelse(ProgramID %in% subtidal, 1, 0)][, Subtidal := as.logical(Subtidal)]

#Create variables for relative year and size class category for data that should be included in analyses and counts of live oysters measured
for(i in unique(oysterraw$ManagedAreaName)){
  oysterraw[ManagedAreaName == i & !is.na(LiveDate), `:=` (RelYear = LiveDate - min(LiveDate),
                                                           SizeClass = fcase(ShellHeight_mm >= 25 & ShellHeight_mm < 75, "25to75mm",
                                                                             ShellHeight_mm >= 75, "o75mm", 
                                                                             default = NA))]
  
  oysterraw[ManagedAreaName == i & !is.na(LiveDate), counts := length(ShellHeight_mm), by = c("QuadIdentifier")]
}

#Remove unrealistically high shell heights from ID_5017
oysterraw <- setdiff(oysterraw, oysterraw[ProgramID == 5017 & ShellHeight_mm >= 165, ])

#Create data table to save model results
oysterresults <- data.table(indicator = character(),
                            managed_area = character(),
                            habitat_class = character(),
                            size_class = character(),
                            live_date_qual = character(),
                            n_programs = integer(),
                            programs = list(),
                            filename = character(),
                            effect = character(),
                            component = character(),
                            group = character(),
                            term = character(),
                            estimate = numeric(),
                            std.error = numeric(),
                            conf.low = numeric(),
                            conf.high = numeric())


# Oyster Shell Height -----------------------------------------------------

# #This code will make a collapsed version of the oysterraw table for shell height, but I'm not sure it is needed.
# oysterraw_sh <- oysterraw[!is.na(ShellHeight_mm), c("ProgramID", "ProgramName", "ProgramLocationID", "QuadIdentifier", "ReefIdentifier", "LiveDate", "LiveDate_Qualifier", "LiveDate_MinEstDate", "LiveDate_MaxEstDate", "GISUniqueID", "SampleDate", "Year", "Month", "ManagedAreaName", "Region.x", "SurveyMethod", "HabitatClassification", "MinimumSizeMeasured_mm", "NumberMeasured_n", "QuadSize_m2", "MADup", "DataFileName", "ShellHeight_mm", "UniversalReefID", "Region.y", "MA_plotlab", "Subtidal", "ObsIndex")] %>%
  # dplyr::group_by(ProgramID, ProgramName, ProgramLocationID, QuadIdentifier, ReefIdentifier, 
  #                 LiveDate, LiveDate_Qualifier, LiveDate_MinEstDate, LiveDate_MaxEstDate, 
  #                 GISUniqueID, SampleDate, Year, Month, ManagedAreaName, Region.x, SurveyMethod, 
  #                 HabitatClassification, MinimumSizeMeasured_mm, NumberMeasured_n, QuadSize_m2, 
  #                 MADup, DataFileName, UniversalReefID, Region.y, MA_plotlab, Subtidal) %>%
  # tidyr::fill(ShellHeight_mm, SHIndex) %>%
  # tidyr::fill(ShellHeight_mm, SHIndex, .direction = 'up') %>%
  # dplyr::distinct()

#summarize shell height data
sh_all_sum <- summarySE(oysterraw[!is.na(ShellHeight_mm), ], measurevar = 'ShellHeight_mm', groupvars = c('ManagedAreaName', 
                                                                                   'LiveDate_Qualifier', 'LiveDate'))

## Apalachicola Bay_Natural ----------------------------------------

#Exclude the five samples that don't have counts less than the "NumberMeasured" value for the corresponding program (see variable exploration graphs in the 25to75mm section for the rationale and graphs for this step.)
numValves <- unique(oysterraw[, c("ProgramID", "RelYear", "counts", "QuadIdentifier", "Subtidal", "QuadSize_m2", "LiveDate_Qualifier", "NumberMeasured_n")])
exclude_samps <- subset(numValves, numValves$NumberMeasured_n == "20" & numValves$counts > 19)$QuadIdentifier
ab_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                        ShellHeight_mm >= 25 &
                        MA_plotlab == "Apalachicola Bay_Natural" & 
                        QuadIdentifier %in% setdiff(oysterraw[!is.na(ShellHeight_mm) & ManagedAreaName == "Apalachicola Bay", QuadIdentifier], exclude_samps), ]

saveRDS(ab_sho25, here::here(paste0('GLMMs/AllDates/Data/ab_sho25_', Sys.Date(), '.rds')))


### ABAP - 25 to 75mm -------------------------------------------------------

ab_sh25to75 <- ab_sho25[ShellHeight_mm < 75, ]

saveRDS(ab_sh25to75, here::here(paste0('GLMMs/AllDates/Data/ab_sh25to75_', Sys.Date(), '.rds')))

#ab_sh25to75_glmm <- brm(formula = ShellHeight_mm | trunc(lb = 25, ub = 75) ~ RelYear + QuadSize_m2 + (1 | UniversalReefID), data = subset(ab_sh25to75, ab_sh25to75$LiveDate_Qualifier != "Estimate"), family = gaussian, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/ab_sh25to75_glmm4b.rds")
ab_sh25to75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 25, ub = 75) ~ me(RelYear, Sample_age_stdev, gr = QuadIdentifier) + (1 | UniversalReefID), data = subset(ab_sh25to75, ab_sh25to75$LiveDate_Qualifier == "Estimate"), family = gaussian, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/ab_sh25to75_glmm_hist2.rds")

# Model results table
oyres_i <- setDT(broom.mixed::tidy(ab_sh25to75_glmm_hist)) #tidy() does not like that parameter values have underscores for some reason, so the resulting table is incomplete
missingrow <- data.table(effect = "fixed",
                         component = "cond", #not sure what "cond" means in the tidy summary.
                         group = NA,
                         term = "meRelYear2Sample_age_stdevgrEQQuadIdentifier",
                         estimate = summary(ab_sh25to75_glmm_hist)$fixed$Estimate[2],
                         std.error = summary(ab_sh25to75_glmm_hist)$fixed$Est.Error[2],
                         conf.low = summary(ab_sh25to75_glmm_hist)$fixed$`l-95% CI`[2],
                         conf.high = summary(ab_sh25to75_glmm_hist)$fixed$`u-95% CI`[2])
oyres_i <- rbind(oyres_i, missingrow)
oyres_i[, `:=` (indicator = "Size class",
                managed_area = unique(ab_sh25to75$ManagedAreaName),
                habitat_class = unique(ab_sh25to75$HabitatClassification),
                size_class = unique(ab_sh25to75$SizeClass),
                live_date_qual = ifelse(str_detect(ab_sh25to75_glmm_hist$file, "_hist"), "Estimate", "Exact"),
                n_programs = length(unique(ab_sh25to75[LiveDate_Qualifier == ifelse(str_detect(ab_sh25to75_glmm_hist$file, "_hist"), "Estimate", "Exact"), ProgramID])),
                programs = as.list(unique(ab_sh25to75[LiveDate_Qualifier == ifelse(str_detect(ab_sh25to75_glmm_hist$file, "_hist"), "Estimate", "Exact"), ProgramID])),
                filename = ab_sh25to75_glmm_hist$file)]
oysterresults <- rbind(oysterresults, oyres_i)

# Posterior distributions and Markov chains:
SH_AllDates_GLMM_AB_PDistandMChains_25to75_hist <- plot(ab_sh25to75_glmm_hist)

len <- length(SH_AllDates_GLMM_AB_PDistandMChains_25to75_hist)
for(i in 1:len){
  jpeg(filename = here::here(paste0("OA_plots/SH_AllDates_GLMM_AB_PDistandMChains_25to75_hist_", i, "of", len, "_", Sys.Date(), ".jpeg")), width = 4, height = 6, units = "in", quality = 100, res = 300)
  print(SH_AllDates_GLMM_AB_PDistandMChains_25to75_hist[i])
  dev.off()
}


# Posterior predictive check plot:
SH_AllDates_GLMM_AB_PPcheck_25to75_hist <- tryCatch(pp_check(ab_sh25to75_glmm_hist),
                                                    error = function(e) NA)
while(is.na(SH_AllDates_GLMM_AB_PPcheck_25to75_hist) == TRUE){
  SH_AllDates_GLMM_AB_PPcheck_25to75_hist <- tryCatch(pp_check(ab_sh25to75_glmm_hist), 
                                                      error = function(e) NA)}

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_AB_PPcheck_25to75_hist_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_AB_PPcheck_25to75_hist,
       width = 4,
       height = 3,
       units = "in",
       dpi = 300)


#Marginal effects plot including random effects
labs1 <- c(1704, 1813, 1899, 1933, 1997, 2017)
labs2 <- c(1987, 2013)

set.seed(987)
#ab_sh25to75_1 <- plot(conditional_effects(ab_sh25to75_glmm, re_formula = NULL), plot = FALSE)[[1]]
ab_sh25to75_hist_1 <- plot(conditional_effects(ab_sh25to75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]

SH_AllDates_GLMM_AB_MEPrand_25to75_allYr <- ggplot() +#ab_sh25to75_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__)) +
  geom_jitter(data = ab_sh25to75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  #geom_line(aes(y = estimate__), color = "blue", lwd = 1) +
  geom_ribbon(data = ab_sh25to75_hist_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = ab_sh25to75_hist_1$data, aes(x = RelYear2, y = estimate__), color = "red", lwd = 1, inherit.aes = FALSE) +
  geom_text(data = subset(ab_sh25to75, ab_sh25to75$Sample_mean_age %in% labs1), aes(y = 22.75, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  geom_text(data = subset(ab_sh25to75, ab_sh25to75$Sample_mean_age %in% labs2), aes(y = 21, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw()  + theme(axis.title = element_text(size = 13), 
                      axis.text = element_text(size = 12), 
                      legend.text = element_text(size = 12), 
                      legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID") +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(20, 80))

SH_AllDates_GLMM_AB_MEPrand_25to75_post1994 <- SH_AllDates_GLMM_AB_MEPrand_25to75_allYr +
  geom_boxplot(data = subset(ab_sh25to75, ab_sh25to75$Sample_mean_age_qualifier == "Exact"), aes(x = RelYear2, y = ShellHeight_mm), color = "blue", alpha = 0.5, lwd = 1, inherit.aes = FALSE) +
  coord_cartesian(xlim = c(290, 315)) +
  theme(legend.position = "right")

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_AB_MEPrand_25to75_allYr_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_AB_MEPrand_25to75_allYr,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_AB_MEPrand_25to75_post1994_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_AB_MEPrand_25to75_post1994,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


### ABAP - >75mm ------------------------------------------------------------

ab_sho75 <- ab_sho25[ShellHeight_mm >= 75, ]

saveRDS(ab_sho75, here::here(paste0('GLMMs/AllDates/Data/ab_sho75_', Sys.Date(), '.rds')))

ab_sho75_glmm <- brm(formula = ShellHeight_mm | trunc(lb = 75, ub = 250) ~ RelYear2 + Subtidal + (1 | UniversalReefID), data = subset(ab_sho75, ab_sho75$LiveDate_Qualifier != "Estimate"), family = gaussian, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/ab_sho75_glmm4c.rds")
ab_sho75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 75, ub = 250) ~ me(RelYear2, Sample_age_stdev, gr = QuadIdentifier) + (1 | UniversalReefID), data = subset(ab_sho75, ab_sho75$LiveDate_Qualifier == "Estimate"), family = gaussian, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/ab_sho75_glmm_hist2.rds")

# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- c(1704, 1813, 1899, 1933, 1997, 2016)
labs2 <- c(1987, 2013)

set.seed(987)
#ab_sho75_1 <- plot(conditional_effects(ab_sho75_glmm, re_formula = NULL), plot = FALSE)[[1]]
ab_sho75_hist_1 <- plot(conditional_effects(ab_sho75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]

SH_AllDates_GLMM_AB_MEPrand_o75_allYr <- ggplot() +#ab_sho75_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__)) +
  geom_jitter(data = ab_sho75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_boxplot(data = subset(ab_sho75, ab_sho75$Sample_mean_age_qualifier == "Exact"), aes(x = RelYear2, y = ShellHeight_mm), color = "blue", alpha = 0.5, lwd = 1, inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  #geom_line(aes(y = estimate__), color = "blue", lwd = 1) +
  geom_ribbon(data = ab_sho75_hist_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = ab_sho75_hist_1$data, aes(x = RelYear2, y = estimate__), color = "red", lwd = 1, inherit.aes = FALSE) +
  geom_text(data = subset(ab_sho75, ab_sho75$Sample_mean_age %in% labs1), aes(y = 73, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  geom_text(data = subset(ab_sho75, ab_sho75$Sample_mean_age %in% labs2), aes(y = 69, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw()  + theme(axis.title = element_text(size = 13), 
                      axis.text = element_text(size = 12), 
                      legend.text = element_text(size = 12), 
                      legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID") +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(68, 150))

SH_AllDates_GLMM_AB_MEPrand_o75_post1994 <- SH_AllDates_GLMM_AB_MEPrand_o75_allYr +
  geom_boxplot(data = subset(ab_sho75, ab_sho75$Sample_mean_age_qualifier == "Exact"), aes(x = RelYear2, y = ShellHeight_mm), color = "blue", alpha = 0.5, lwd = 1, inherit.aes = FALSE) +
  coord_cartesian(xlim = c(285, 315)) +
  theme(legend.position = "right")

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_AB_MEPrand_o75_allYr_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_AB_MEPrand_o75_allYr,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_AB_MEPrand_o75_post1994_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_AB_MEPrand_o75_post1994,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)




## Apalachicola NERR_Natural ---------------------------------------

an_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                        ShellHeight_mm >= 25 &
                        MA_plotlab == "Apalachicola NERR_Natural" & 
                        QuadIdentifier %in% setdiff(oysterraw[!is.na(ShellHeight_mm) & ManagedAreaName == "Apalachicola NERR", QuadIdentifier], exclude_samps), ]

saveRDS(an_sho25, here::here(paste0('GLMMs/AllDates/Data/an_sho25_', Sys.Date(), '.rds')))


### ANERR - 25 to 75mm -------------------------------------------------------

an_sh25to75 <- subset(an_sho25, an_sho25$ShellHeight_mm < 75)

saveRDS(an_sh25to75, here::here(paste0('GLMMs/AllDates/Data/an_sh25to75_', Sys.Date(), '.rds')))

an_sh25to75_glmm <- brm(formula = ShellHeight_mm | trunc(lb = 25, ub = 75) ~ RelYear2 + QuadSize_m2 + (1 | UniversalReefID), data = subset(an_sh25to75, an_sh25to75$LiveDate_Qualifier != "Estimate"), family = gaussian, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/an_sh25to75_glmm4b.rds")
an_sh25to75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 25, ub = 75) ~ me(RelYear2, Sample_age_stdev, gr = QuadIdentifier) + (1 | UniversalReefID), data = subset(an_sh25to75, an_sh25to75$LiveDate_Qualifier == "Estimate"), family = gaussian, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/an_sh25to75_glmm_hist3.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- c(1580, 1704, 1757, 1899, 1972, 2013)
labs2 <- c(1688, 1813, 1933, 1995, 2019)

set.seed(987)
an_sh25to75_1 <- plot(conditional_effects(an_sh25to75_glmm, re_formula = NULL), plot = FALSE)[[1]]
an_sh25to75_hist_1 <- plot(conditional_effects(an_sh25to75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]

SH_AllDates_GLMM_ANERR_MEPrand_25to75_allYr <- ggplot(an_sh25to75_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__)) +
  geom_jitter(data = an_sh25to75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  geom_line(aes(y = estimate__), color = "blue", lwd = 1) +
  geom_ribbon(data = an_sh25to75_hist_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = an_sh25to75_hist_1$data, aes(x = RelYear2, y = estimate__), color = "red", lwd = 1, inherit.aes = FALSE) +
  geom_text(data = subset(an_sh25to75, an_sh25to75$Sample_mean_age %in% labs1), aes(y = 22, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  geom_text(data = subset(an_sh25to75, an_sh25to75$Sample_mean_age %in% labs2), aes(y = 20, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw()  + theme(axis.title = element_text(size = 13), 
                      axis.text = element_text(size = 12), 
                      legend.text = element_text(size = 12), 
                      legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID") +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(20, 80))

SH_AllDates_GLMM_ANERR_MEPrand_25to75_post1994 <- SH_AllDates_GLMM_ANERR_MEPrand_25to75_allYr +
  coord_cartesian(xlim = c(414, 440)) +
  theme(legend.position = "right")

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_ANERR_MEPrand_25to75_allYr_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_ANERR_MEPrand_25to75_allYr,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_ANERR_MEPrand_25to75_post1994_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_ANERR_MEPrand_25to75_post1994,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


### ANERR - >75mm -------------------------------------------------------

an_sho75 <- an_sho25[ShellHeight_mm >= 75, ]

saveRDS(an_sho75, here::here(paste0('GLMMs/AllDates/Data/an_sho75_', Sys.Date(), '.rds')))

an_sho75_glmm <- brm(formula = ShellHeight_mm | trunc(lb = 75, ub = 250) ~ RelYear2 + (1 | UniversalReefID), data = subset(an_sho75, an_sho75$LiveDate_Qualifier != "Estimate"), family = gaussian, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/an_sho75_glmm4b.rds")
an_sho75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 75, ub = 250) ~ me(RelYear2, Sample_age_stdev, gr = QuadIdentifier) + (1 | UniversalReefID), data = subset(an_sho75, an_sho75$LiveDate_Qualifier == "Estimate"), family = gaussian, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/an_sho75_glmm_hist3.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- c(1580, 1704, 1757, 1899, 1972, 2013)
labs2 <- c(1688, 1813, 1933, 1992, 2017)

set.seed(987)
an_sho75_1 <- plot(conditional_effects(an_sho75_glmm, re_formula = NULL), plot = FALSE)[[1]]
an_sho75_hist_1 <- plot(conditional_effects(an_sho75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]

SH_AllDates_GLMM_ANERR_MEPrand_o75_allYr <- ggplot(an_sho75_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__)) +
  geom_jitter(data = an_sho75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  geom_line(aes(y = estimate__), color = "blue", lwd = 1) +
  geom_ribbon(data = an_sho75_hist_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = an_sho75_hist_1$data, aes(x = RelYear2, y = estimate__), color = "red", lwd = 1, inherit.aes = FALSE) +
  geom_text(data = subset(an_sho75, an_sho75$Sample_mean_age %in% labs1), aes(y = 71, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  geom_text(data = subset(an_sho75, an_sho75$Sample_mean_age %in% labs2), aes(y = 68, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() + theme(axis.title = element_text(size = 13), 
                     axis.text = element_text(size = 12), 
                     legend.text = element_text(size = 12), 
                     legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID") +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(70, 250))

SH_AllDates_GLMM_ANERR_MEPrand_o75_post1994 <- SH_AllDates_GLMM_ANERR_MEPrand_o75_allYr +
  coord_cartesian(xlim = c(414, 440)) +
  theme(legend.position = "right")

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_ANERR_MEPrand_o75_allYr_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_ANERR_MEPrand_o75_allYr,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_ANERR_MEPrand_o75_post1994_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_ANERR_MEPrand_o75_post1994,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)



## Big Bend Seagrasses_Natural ---------------------------------------

bbs_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                        ShellHeight_mm >= 25 &
                        MA_plotlab == "Big Bend Seagrasses_Natural" & 
                        QuadIdentifier %in% setdiff(oysterraw[!is.na(ShellHeight_mm) & ManagedAreaName == "Big Bend Seagrasses_Natural", QuadIdentifier], exclude_samps), ]

saveRDS(bbs_sho25, here::here(paste0('GLMMs/AllDates/Data/bbs_sho25_', Sys.Date(), '.rds')))



### BBSAP - 25 to 75mm -------------------------------------------------------

bbs_sh25to75 <- subset(bbs_sho25, bbs_sho25$ShellHeight_mm < 75)

saveRDS(bbs_sh25to75, here::here(paste0('GLMMs/AllDates/Data/bbs_sh25to75_', Sys.Date(), '.rds')))

bbs_sh25to75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 25, ub = 75) ~ me(RelYear2, Sample_age_stdev, gr = QuadIdentifier) + (1 | UniversalReefID), data = bbs_sh25to75, family = gaussian, prior = c(set_prior("normal(125.4, 80)", class = "meanme"), set_prior("normal(49.59, 100)", class = "sdme"), set_prior("cauchy(0,2)", class = "sd")), cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/bbs_sh25to75_glmm_hist3.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- sort(unique(bbs_sh25to75$Sample_mean_age))[c(TRUE, FALSE)]
labs1 <- labs1[seq(1, length(labs1), 2)]
labs2 <- sort(unique(bbs_sh25to75$Sample_mean_age))[c(FALSE, TRUE)]
labs2 <- labs2[seq(1, length(labs2), 2)]

set.seed(987)
#bbs_sh25to75_1 <- plot(conditional_effects(bbs_sh25to75_glmm, re_formula = NULL), plot = FALSE)[[1]]
bbs_sh25to75_hist_1 <- plot(conditional_effects(bbs_sh25to75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]

SH_AllDates_GLMM_BBS_MEPrand_25to75 <- ggplot() + #bbs_sh25to75_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__)) +
  geom_jitter(data = bbs_sh25to75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  #geom_ribbon(fill = "grey", alpha = 0.4) +
  #geom_line(aes(y = estimate__), color = "blue", lwd = 1) +
  geom_ribbon(data = bbs_sh25to75_hist_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = bbs_sh25to75_hist_1$data, aes(x = RelYear2, y = estimate__), color = "red", lwd = 1, inherit.aes = FALSE) +
  geom_text(data = subset(bbs_sh25to75, bbs_sh25to75$Sample_mean_age %in% labs1), aes(y = 22, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  geom_text(data = subset(bbs_sh25to75, bbs_sh25to75$Sample_mean_age %in% labs2), aes(y = 20, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() + theme(axis.title = element_text(size = 13), 
                     axis.text = element_text(size = 12), 
                     legend.text = element_text(size = 12), 
                     legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID") +
  #theme(legend.position = "none") +
  coord_cartesian(ylim = c(19, 80))

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_BBS_MEPrand_25to75_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_BBS_MEPrand_25to75,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)



### BBSAP - >75mm -------------------------------------------------------

bbs_sho75 <- bbs_sho25[ShellHeight_mm >= 75, ]

saveRDS(bbs_sho75, here::here(paste0('GLMMs/AllDates/Data/bbs_sho75_', Sys.Date(), '.rds')))

bbs_sho75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 75, ub = 250) ~ me(RelYear2, Sample_age_stdev, gr = QuadIdentifier) + (1 | UniversalReefID), data = subset(bbs_sho75, bbs_sho75$LiveDate_Qualifier == "Estimate"), family = gaussian, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/bbs_sho75_glmm_hist3.rds")

#Marginal effects plot including random effects
labs1 <- sort(unique(bbs_sho75$Sample_mean_age))[c(TRUE, FALSE)]
labs1 <- labs1[seq(1, length(labs1), 2)]
labs2 <- sort(unique(bbs_sho75$Sample_mean_age))[c(FALSE, TRUE)]
labs2 <- labs2[seq(1, length(labs2), 2)]

set.seed(987)
#bbs_sho75_1 <- plot(conditional_effects(bbs_sho75_glmm, re_formula = NULL), plot = FALSE)[[1]]
bbs_sho75_hist_1 <- plot(conditional_effects(bbs_sho75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]

SH_AllDates_GLMM_BBS_MEPrand_o75 <- ggplot() + #bbs_sho75_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__)) +
  geom_jitter(data = bbs_sho75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  #geom_ribbon(fill = "grey", alpha = 0.4) +
  #geom_line(aes(y = estimate__), color = "blue", lwd = 1) +
  geom_ribbon(data = bbs_sho75_hist_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = bbs_sho75_hist_1$data, aes(x = RelYear2, y = estimate__), color = "red", lwd = 1, inherit.aes = FALSE) +
  geom_text(data = subset(bbs_sho75, bbs_sho75$Sample_mean_age %in% labs1), aes(y = 71, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  geom_text(data = subset(bbs_sho75, bbs_sho75$Sample_mean_age %in% labs2), aes(y = 68, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() + theme(axis.title = element_text(size = 13), 
                     axis.text = element_text(size = 12), 
                     legend.text = element_text(size = 12), 
                     legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID") +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(70, 250))

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_BBS_MEPrand_o75_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_BBS_MEPrand_o75,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)



## Estero Bay_Natural ---------------------------------------

eb_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                         ShellHeight_mm >= 25 &
                         MA_plotlab == "Estero Bay_Natural" & 
                         QuadIdentifier %in% setdiff(oysterraw[!is.na(ShellHeight_mm) & ManagedAreaName == "Estero Bay_Natural", QuadIdentifier], exclude_samps), ]

saveRDS(eb_sho25, here::here(paste0('GLMMs/AllDates/Data/eb_sho25_', Sys.Date(), '.rds')))



### EBAP - 25 to 75mm -------------------------------------------------------

eb_sh25to75 <- subset(eb_sho25, eb_sho25$ShellHeight_mm < 75)

saveRDS(eb_sh25to75, here::here(paste0('GLMMs/AllDates/Data/eb_sh25to75_', Sys.Date(), '.rds')))

eb_sh25to75_glmm <- brm(formula = ShellHeight_mm ~ RelYear2 + QuadSize_m2 + (0 + RelYear2 | UniversalReefID), data = subset(eb_sh25to75, eb_sh25to75$Sample_mean_age_qualifier == "Exact"), family = gaussian, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/eb_sh25to75_glmm5.rds")
eb_sh25to75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 25, ub = 75) ~ me(RelYear2, Sample_age_stdev, gr = QuadIdentifier) + (1 | UniversalReefID), data = subset(eb_sh25to75, eb_sh25to75$Sample_mean_age_qualifier == "Estimate"), family = gaussian, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 20), iter = 3000, warmup = 1000, chains = 4, thin = 3, inits = 30, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/eb_sh25to75_glmm_hist3.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- sort(unique(eb_sh25to75$Sample_mean_age))[c(TRUE, FALSE)]
labs1 <- c(labs1[seq(1, length(labs1), 4)], 2018)
labs2 <- sort(unique(eb_sh25to75$Sample_mean_age))[c(FALSE, TRUE)]
labs2 <- labs2[seq(1, length(labs2), 4)]

set.seed(987)
eb_sh25to75_1 <- plot(conditional_effects(eb_sh25to75_glmm, re_formula = NULL), plot = FALSE)[[1]]
eb_sh25to75_hist_1 <- plot(conditional_effects(eb_sh25to75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]

SH_AllDates_GLMM_EB_MEPrand_25to75 <- ggplot(eb_sh25to75_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__)) +
  geom_jitter(data = eb_sh25to75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  geom_line(aes(y = estimate__), color = "blue", lwd = 1) +
  geom_ribbon(data = eb_sh25to75_hist_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = eb_sh25to75_hist_1$data, aes(x = RelYear2, y = estimate__), color = "red", lwd = 1, inherit.aes = FALSE) +
  geom_text(data = subset(eb_sh25to75, eb_sh25to75$Sample_mean_age %in% labs1), aes(y = 23, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  geom_text(data = subset(eb_sh25to75, eb_sh25to75$Sample_mean_age %in% labs2), aes(y = 20, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw()  + theme(axis.title = element_text(size = 13), 
                      axis.text = element_text(size = 12), 
                      legend.text = element_text(size = 12), 
                      legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID") +
  #theme(legend.position = "none") +
  coord_cartesian(ylim = c(20, 80))

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_EB_MEPrand_25to75_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_EB_MEPrand_25to75,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


### EBAP - >75mm -------------------------------------------------------

eb_sho75 <- eb_sho25[ShellHeight_mm >= 75, ]

saveRDS(eb_sho75, here::here(paste0('GLMMs/AllDates/Data/eb_sho75_', Sys.Date(), '.rds')))

eb_sho75_glmm <- brm(formula = ShellHeight_mm ~ RelYear2 + (1 | UniversalReefID), data = subset(eb_sho75, eb_sho75$Sample_mean_age_qualifier == "Exact"), family = gaussian, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/eb_sho75_glmm4.rds")
eb_sho75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 75, ub = 250) ~ me(RelYear2, Sample_age_stdev, gr = QuadIdentifier) + (1 | UniversalReefID), data = subset(eb_sho75, eb_sho75$Sample_mean_age_qualifier == "Estimate"), family = gaussian, cores = 3, control= list(adapt_delta = 0.99, max_treedepth = 20), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/eb_sho75_glmm_hist3.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- sort(unique(eb_sho75$Sample_mean_age))[c(TRUE, FALSE)]
labs1 <- labs1[seq(1, length(labs1), 4)]
labs2 <- sort(unique(eb_sho75$Sample_mean_age))[c(FALSE, TRUE)]
labs2 <- labs2[seq(1, length(labs2), 4)]

set.seed(987)
eb_sho75_1 <- plot(conditional_effects(eb_sho75_glmm, re_formula = NULL), plot = FALSE)[[1]]
eb_sho75_hist_1 <- plot(conditional_effects(eb_sho75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]

SH_AllDates_GLMM_EB_MEPrand_o75 <- ggplot(eb_sho75_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__)) +
  geom_jitter(data = eb_sho75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  geom_line(aes(y = estimate__), color = "blue", lwd = 1) +
  geom_ribbon(data = eb_sho75_hist_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = eb_sho75_hist_1$data, aes(x = RelYear2, y = estimate__), color = "red", lwd = 1, inherit.aes = FALSE) +
  geom_text(data = subset(eb_sho75, eb_sho75$Sample_mean_age %in% labs1), aes(y = 73, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  geom_text(data = subset(eb_sho75, eb_sho75$Sample_mean_age %in% labs2), aes(y = 70, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw()  + theme(axis.title = element_text(size = 13), 
                      axis.text = element_text(size = 12), 
                      legend.text = element_text(size = 12), 
                      legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID") +
  #theme(legend.position = "none") +
  coord_cartesian(ylim = c(70, 150))

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_EB_MEPrand_o75_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_EB_MEPrand_o75,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)



## Guana River Marsh_Natural ---------------------------------------

grm_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                        ShellHeight_mm >= 25 &
                        MA_plotlab == "Guana River Marsh_Natural" & 
                        QuadIdentifier %in% setdiff(oysterraw[!is.na(ShellHeight_mm) & ManagedAreaName == "Guana River Marsh_Natural", QuadIdentifier], exclude_samps), ]

saveRDS(grm_sho25, here::here(paste0('GLMMs/AllDates/Data/grm_sho25_', Sys.Date(), '.rds')))


### GRMAP - 25 to 75mm -------------------------------------------------------

grm_sh25to75 <- subset(grm_sho25, grm_sho25$ShellHeight_mm < 75)

saveRDS(grm_sh25to75, here::here(paste0('GLMMs/AllDates/Data/grm_sh25to75_', Sys.Date(), '.rds')))

grm_sh25to75_glmm <- brm(formula = ShellHeight_mm | trunc(lb = 25, ub = 75) ~ RelYear2 + NumberMeasured_n + (1 | UniversalReefID), data = subset(grm_sh25to75, grm_sh25to75$Sample_mean_age_qualifier == "Exact"), family = gaussian, cores = 4, control= list(adapt_delta = 0.8, max_treedepth = 10), iter = 3000, warmup = 1000, chains = 4, inits = 30, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/grm_sh25to75_glmm4.rds")
grm_sh25to75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 25, ub = 75) ~ me(RelYear2, Sample_age_stdev, gr = QuadIdentifier) + (1 | UniversalReefID), data = subset(grm_sh25to75, grm_sh25to75$Sample_mean_age_qualifier == "Estimate"), family = gaussian, prior = c(set_prior("normal(6.25, 7)", class = "meanme", coef = "meRelYear2"), set_prior("normal(15.27, 5)", class = "sdme", coef = "meRelYear2"), set_prior("cauchy(0,2)", class = "sd")), cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/grm_sh25to75_glmm_hist3c.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- sort(unique(grm_sh25to75$Sample_mean_age))[c(TRUE, FALSE)]
labs2 <- sort(unique(grm_sh25to75$Sample_mean_age))[c(FALSE, TRUE)]

set.seed(987)
grm_sh25to75_1 <- plot(conditional_effects(grm_sh25to75_glmm, re_formula = NULL), plot = FALSE)[[1]]
grm_sh25to75_hist_1 <- plot(conditional_effects(grm_sh25to75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]

SH_AllDates_GLMM_GRM_MEPrand_25to75 <- ggplot(grm_sh25to75_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__)) +
  geom_jitter(data = grm_sh25to75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  geom_line(aes(y = estimate__), color = "blue", lwd = 1) +
  geom_ribbon(data = grm_sh25to75_hist_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = grm_sh25to75_hist_1$data, aes(x = RelYear2, y = estimate__), color = "red", lwd = 1, inherit.aes = FALSE) +
  geom_text(data = subset(grm_sh25to75, grm_sh25to75$Sample_mean_age %in% labs1), aes(y = 23, label = Sample_mean_age, x = RelYear2), size = 3.5, col = "grey30", inherit.aes = FALSE) +
  geom_text(data = subset(grm_sh25to75, grm_sh25to75$Sample_mean_age %in% labs2), aes(y = 21, label = Sample_mean_age, x = RelYear2), size = 3.5, col = "grey30", inherit.aes = FALSE) +
  theme_bw() + theme(axis.title = element_text(size = 13), 
                     axis.text = element_text(size = 12), 
                     legend.text = element_text(size = 12), 
                     legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID") +
  #theme(legend.position = "none") +
  coord_cartesian(ylim = c(20, 80))

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_GRM_MEPrand_25to75_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_GRM_MEPrand_25to75,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


### GRMAP - >75mm -------------------------------------------------------

grm_sho75 <- grm_sho25[ShellHeight_mm >= 75, ]

saveRDS(grm_sho75, here::here(paste0('GLMMs/AllDates/Data/grm_sho75_', Sys.Date(), '.rds')))

grm_sho75_glmm <- brm(formula = ShellHeight_mm | trunc(lb = 75, ub = 250) ~ RelYear2 + NumberMeasured_n + (1 | UniversalReefID), data = subset(grm_sho75, grm_sho75$Sample_mean_age_qualifier == "Exact"), family = gaussian, cores = 4, control= list(adapt_delta = 0.8, max_treedepth = 10), iter = 3000, warmup = 1000, chains = 4, inits = 30, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/grm_sho75_glmm4.rds")
grm_sho75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 75, ub = 250) ~ me(RelYear2, Sample_age_stdev, gr = QuadIdentifier) + (0 + me(RelYear2, Sample_age_stdev, gr = QuadIdentifier) | UniversalReefID), data = subset(grm_sho75, grm_sho75$Sample_mean_age_qualifier == "Estimate"), family = gaussian, prior = c(set_prior("normal(7.36, 6)", class = "meanme"), set_prior("normal(15.54, 4)", class = "sdme"), set_prior("cauchy(0,2)", class = "sd")), cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/grm_sho75_glmm_hist4.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- sort(unique(grm_sho75$Sample_mean_age))[c(TRUE, FALSE)]
labs2 <- sort(unique(grm_sho75$Sample_mean_age))[c(FALSE, TRUE)]

set.seed(987)
grm_sho75_1 <- plot(conditional_effects(grm_sho75_glmm, re_formula = NULL), plot = FALSE)[[1]]
grm_sho75_hist_1 <- plot(conditional_effects(grm_sho75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]

SH_AllDates_GLMM_GRM_MEPrand_o75 <- ggplot(grm_sho75_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__)) +
  geom_jitter(data = grm_sho75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  geom_line(aes(y = estimate__), color = "blue", lwd = 1) +
  geom_ribbon(data = grm_sho75_hist_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = grm_sho75_hist_1$data, aes(x = RelYear2, y = estimate__), color = "red", lwd = 1, inherit.aes = FALSE) +
  geom_text(data = subset(grm_sho75, grm_sho75$Sample_mean_age %in% labs1), aes(y = 72, label = Sample_mean_age, x = RelYear2), size = 3.5, col = "grey30", inherit.aes = FALSE) +
  geom_text(data = subset(grm_sho75, grm_sho75$Sample_mean_age %in% labs2), aes(y = 68, label = Sample_mean_age, x = RelYear2), size = 3.5, col = "grey30", inherit.aes = FALSE) +
  theme_bw() + theme(axis.title = element_text(size = 13), 
                     axis.text = element_text(size = 12), 
                     legend.text = element_text(size = 12), 
                     legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID") +
  #theme(legend.position = "none") +
  coord_cartesian(ylim = c(70, 250))

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_GRM_MEPrand_o75_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_GRM_MEPrand_o75,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)





## Guana Tolomato Matanzas NERR_Natural ---------------------------------------

gtmn_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                         ShellHeight_mm >= 25 &
                         MA_plotlab == "Guana Tolomato Matanzas NERR_Natural" & 
                         QuadIdentifier %in% setdiff(oysterraw[!is.na(ShellHeight_mm) & ManagedAreaName == "Guana Tolomato Matanzas NERR_Natural", QuadIdentifier], exclude_samps), ]

saveRDS(gtmn_sho25, here::here(paste0('GLMMs/AllDates/Data/gtmn_sho25_', Sys.Date(), '.rds')))


### GTMNERR - 25 to 75mm -------------------------------------------------------

gtmn_sh25to75 <- subset(gtmn_sho25, gtmn_sho25$ShellHeight_mm < 75)

saveRDS(gtmn_sh25to75, here::here(paste0('GLMMs/AllDates/Data/gtmn_sh25to75_', Sys.Date(), '.rds')))

gtmn_sh25to75_glmm <- brm(formula = ShellHeight_mm | trunc(lb = 25, ub = 75) ~ RelYear2 + NumberMeasured_n + Region.y + (1 | UniversalReefID), data = subset(gtmn_sh25to75, gtmn_sh25to75$LiveDate_Qualifier != "Estimate"), family = gaussian, cores = 4, control = list(adapt_delta = 0.8, max_treedepth = 10), iter = 3000, warmup = 1000, chains = 4, inits = 30, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/gtmn_sh25to75_glmm5.rds")
gtmn_sh25to75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 25, ub = 75) ~ me(RelYear2, Sample_age_stdev, gr = QuadIdentifier) + Region.y + (1 + RelYear2 | UniversalReefID), data = subset(gtmn_sh25to75, gtmn_sh25to75$LiveDate_Qualifier == "Estimate"), family = gaussian, prior = c(set_prior("normal(146,25)", class = "b"), set_prior("cauchy(0,2)", class = "sd")), cores = 4, control= list(adapt_delta = 0.9, max_treedepth = 10), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/gtmn_sh25to75_glmm_hist5.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- sort(unique(gtmn_sh25to75$Sample_mean_age))#[c(TRUE, FALSE)]
labs1 <- labs1[seq(1, length(labs1), 4)]
labs1 <- append(labs1, c(1954))
labs1 <- labs1[c(-3, -4, -6)]
#labs2 <- sort(unique(gtmn_sh25to75$Sample_mean_age))[c(FALSE, TRUE)]

set.seed(987)
gtmn_sh25to75_1 <- plot(conditional_effects(gtmn_sh25to75_glmm, re_formula = NULL), plot = FALSE)[[1]]
gtmn_sh25to75_hist <- plot(conditional_effects(gtmn_sh25to75_glmm_hist, re_formula = NULL), plot = FALSE)[[2]]

SH_AllDates_GLMM_GTMNERR_MEPrand_25to75 <- ggplot(gtmn_sh25to75_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__)) +
  geom_jitter(data = gtmn_sh25to75, aes(x = RelYear2, y = ShellHeight_mm, fill = Region.y), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  geom_line(aes(y = estimate__), color = "blue", lwd = 1) +
  geom_ribbon(data = gtmn_sh25to75_hist$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = gtmn_sh25to75_hist$data, aes(x = RelYear2, y = estimate__), color = "red", lwd = 1, inherit.aes = FALSE) +
  geom_text(data = subset(gtmn_sh25to75, gtmn_sh25to75$Sample_mean_age %in% labs1), aes(y = 22, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
  #geom_text(data = subset(gtmn_sh25to75, gtmn_sh25to75$Sample_mean_age %in% labs2), aes(y = 18, label = Sample_mean_age, x = RelYear2), size = 3, col = "grey30", inherit.aes = FALSE) +
  theme_bw()  + theme(axis.title = element_text(size = 13), 
                      axis.text = element_text(size = 12), 
                      legend.text = element_text(size = 12), 
                      legend.title = element_text(size = 13)) +
  labs(fill = "Region") +
  #theme(legend.position = "none") +
  coord_cartesian(ylim = c(20, 80))

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_GTMNERR_MEPrand_25to75_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_GTMNERR_MEPrand_25to75,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

#Plot of modeled mean shell heights
meanSH_test_hist <- plot(conditional_effects(gtmn_sh25to75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]$data
meanSH_test <- plot(conditional_effects(gtmn_sh25to75_glmm, re_formula = NULL), plot = FALSE)[[3]]$data
meanSH_test_hist$data <- "Historical data"
meanSH_test$data <- "Real-time data"
meanSH <- rbind(meanSH_test[, c("effect1__", "estimate__", "se__", "lower__", "upper__", "data")],
                meanSH_test_hist[, c("effect1__", "estimate__", "se__", "lower__", "upper__", "data")])
setnames(meanSH, c("effect1__"), c("Region"))

SH_AllDates_GLMM_GTMNERR_MEPrand_25to75_MeanRes <- ggplot(meanSH, aes(x = Region, y = estimate__, ymin = lower__, ymax = upper__, fill = data)) +
  geom_pointinterval(position = position_jitter(width = 0.35, height = 0), size = 3, fatten_point = 4, shape = 21, color = "black") +
  ylab("ShellHeight_mm | trunc(lb = 25, ub = 75)") +
  theme_bw()  + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13), 
        axis.text.x = element_text(angle = 90)) +
  labs(fill = NULL)

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_GTMNERR_MEPrand_25to75_MeanRes_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_GTMNERR_MEPrand_25to75_MeanRes,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)



### GTMNERR - >75mm -------------------------------------------------------

gtmn_sho75 <- gtmn_sho25[ShellHeight_mm >= 75, ]

saveRDS(gtmn_sho75, here::here(paste0('GLMMs/AllDates/Data/gtmn_sho75_', Sys.Date(), '.rds')))

gtmn_sho75_glmm <- brm(formula = ShellHeight_mm | trunc(lb = 75) ~ RelYear2 + NumberMeasured_n + Region.y + (0 + RelYear2 | UniversalReefID), data = subset(gtmn_sho75, gtmn_sho75$LiveDate_Qualifier != "Estimate"), family = gaussian, prior = c(set_prior("normal(171,10)", class = "b", coef = "RelYear2"), set_prior("cauchy(0,2)")), cores = 4, control = list(adapt_delta = 0.99, max_treedepth = 10), iter = 3000, warmup = 1000, chains = 4, inits = 30, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/gtmn_sho75_glmm6.rds")
gtmn_sho75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 75, ub = 250) ~ me(RelYear2, Sample_age_stdev, gr = QuadIdentifier) + Region.y + (1 + RelYear2 | UniversalReefID), data = subset(gtmn_sho75, gtmn_sho75$LiveDate_Qualifier == "Estimate"), family = gaussian, prior = c(set_prior("normal(146,25)", class = "b", coef = "meRelYear2Sample_age_stdevgrEQQuadIdentifier")), cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 4000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/gtmn_sho75_glmm_hist22.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- sort(unique(gtmn_sho75$Sample_mean_age))#[c(TRUE, FALSE)]
labs1 <- labs1[seq(1, length(labs1), 4)]
labs1 <- append(labs1, c(1954))
#labs2 <- sort(unique(gtmn_sho75$Sample_mean_age))[c(FALSE, TRUE)]

set.seed(987)
gtmn_sho75_1 <- plot(conditional_effects(gtmn_sho75_glmm, re_formula = NULL), plot = FALSE)[[1]]
gtmn_sho75_hist <- plot(conditional_effects(gtmn_sho75_glmm_hist, re_formula = NULL), plot = FALSE)[[2]]

SH_AllDates_GLMM_GTMNERR_MEPrand_o75 <- ggplot(gtmn_sho75_1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__)) +
  geom_jitter(data = gtmn_sho75, aes(x = RelYear2, y = ShellHeight_mm, fill = Region.y), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  geom_line(aes(y = estimate__), color = "blue", lwd = 1) +
  geom_ribbon(data = gtmn_sho75_hist$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = gtmn_sho75_hist$data, aes(x = RelYear2, y = estimate__), color = "red", lwd = 1, inherit.aes = FALSE) +
  geom_text(data = subset(gtmn_sho75, gtmn_sho75$Sample_mean_age %in% labs1), aes(y = 70, label = Sample_mean_age, x = RelYear2), size = 4, col = "grey30", inherit.aes = FALSE) +
    #geom_text(data = subset(gtmn_sho75, gtmn_sho75$Sample_mean_age %in% labs2), aes(y = 18, label = Sample_mean_age, x = RelYear2), size = 3, col = "grey30", inherit.aes = FALSE) +
  theme_bw() + theme(axis.title = element_text(size = 13), 
    axis.text = element_text(size = 12), 
    legend.text = element_text(size = 12), 
    legend.title = element_text(size = 13)) +
  labs(fill = "Region") +
  #theme(legend.position = "none") +
  coord_cartesian(ylim = c(70, 250))

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_GTMNERR_MEPrand_o75_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_GTMNERR_MEPrand_o75,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

#Plot of modeled mean shell heights
meanSH_test_hist2 <- plot(conditional_effects(gtmn_sho75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]$data
meanSH_test2 <- plot(conditional_effects(gtmn_sho75_glmm, re_formula = NULL), plot = FALSE)[[3]]$data
meanSH_test_hist2$data <- "Historical data"
meanSH_test2$data <- "Real-time data"
meanSH2 <- rbind(meanSH_test2[, c("effect1__", "estimate__", "se__", "lower__", "upper__", "data")],
                meanSH_test_hist2[, c("effect1__", "estimate__", "se__", "lower__", "upper__", "data")])
setnames(meanSH2, c("effect1__"), c("Region"))

SH_AllDates_GLMM_GTMNERR_MEPrand_o75_MeanRes <- ggplot(meanSH2, aes(x = Region, y = estimate__, ymin = lower__, ymax = upper__, fill = data)) +
  geom_pointinterval(position = position_jitter(width = 0.35, height = 0), size = 3, fatten_point = 4, shape = 21, color = "black") +
  ylab("ShellHeight_mm | trunc(lb = 75, ub = 250)") +
  theme_bw()  + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13),
        axis.text.x = element_text(angle = 90))

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_GTMNERR_MEPrand_o75_MeanRes_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_GTMNERR_MEPrand_o75_MeanRes,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)



## Indian River-Vero Beach to Ft. Pierce_Natural ---------------------------------------

irvbfp_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                          ShellHeight_mm >= 25 &
                          MA_plotlab == "Indian River-Vero Beach to Ft. Pierce_Natural" & 
                          QuadIdentifier %in% setdiff(oysterraw[!is.na(ShellHeight_mm) & ManagedAreaName == "Indian River-Vero Beach to Ft. Pierce_Natural", QuadIdentifier], exclude_samps), ]

saveRDS(irvbfp_sho25, here::here(paste0('GLMMs/AllDates/Data/irvbfp_sho25_', Sys.Date(), '.rds')))


### IRVBFPAP - 25 to 75mm -------------------------------------------------------

irvbfp_sh25to75 <- subset(irvbfp_sho25, irvbfp_sho25$ShellHeight_mm < 75)

saveRDS(irvbfp_sh25to75, here::here(paste0('GLMMs/AllDates/Data/irvbfp_sh25to75_', Sys.Date(), '.rds')))

irvbfp_sh25to75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 25, ub = 75) ~ me(RelYear2, Sample_age_stdev, gr = QuadIdentifier) + (1 | UniversalReefID), data = subset(irvbfp_sh25to75, irvbfp_sh25to75$Sample_mean_age_qualifier == "Estimate"), family = gaussian, prior = set_prior("normal(113, 20)", class = "b", coef = "meRelYear2Sample_age_stdevgrEQQuadIdentifier"), cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/irvbfp_sh25to75_glmm_hist3.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- c(1840, 1943, 1993, 2001, 2019)

set.seed(987)
#irvbfp_sh25to75_1 <- plot(conditional_effects(irvbfp_sh25to75_glmm, re_formula = NULL), plot = FALSE)[[1]]
irvbfp_sh25to75_hist1 <- plot(conditional_effects(irvbfp_sh25to75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]

SH_AllDates_GLMM_IRVBFP_MEPrand_25to75 <- ggplot(irvbfp_sh25to75_hist1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__)) +
  geom_jitter(data = irvbfp_sh25to75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  geom_line(aes(y = estimate__), color = "red", lwd = 1) +
  geom_boxplot(data = subset(irvbfp_sh25to75, irvbfp_sh25to75$LiveDate_Qualifier == "Exact"), aes(x = RelYear2, y = ShellHeight_mm), color = "blue", fill = "light blue", lwd = 1, alpha = 0.5, inherit.aes = FALSE) +
  geom_boxplot(data = subset(irvbfp_sh25to75, irvbfp_sh25to75$LiveDate_Qualifier == "Exact" & irvbfp_sh25to75$UniversalReefID == 198340), aes(x = 186, y = ShellHeight_mm), color = "salmon", fill = "salmon", lwd = 1, alpha = 0.5, width = 1, inherit.aes = FALSE) +
  geom_boxplot(data = subset(irvbfp_sh25to75, irvbfp_sh25to75$LiveDate_Qualifier == "Exact" & irvbfp_sh25to75$UniversalReefID == 198341), aes(x = 188, y = ShellHeight_mm), color = "gold", fill = "gold", lwd = 1, alpha = 0.5, width = 1, inherit.aes = FALSE) +
  geom_boxplot(data = subset(irvbfp_sh25to75, irvbfp_sh25to75$LiveDate_Qualifier == "Exact" & irvbfp_sh25to75$UniversalReefID == 198352), aes(x = 192, y = ShellHeight_mm), color = "turquoise", fill = "turquoise", lwd = 1, alpha = 0.5, width = 1, inherit.aes = FALSE) +
  geom_boxplot(data = subset(irvbfp_sh25to75, irvbfp_sh25to75$LiveDate_Qualifier == "Exact" & irvbfp_sh25to75$UniversalReefID == 198360), aes(x = 194, y = ShellHeight_mm), color = "dodgerblue", fill = "dodgerblue", lwd = 1, alpha = 0.5, width = 1, inherit.aes = FALSE) +
  geom_boxplot(data = subset(irvbfp_sh25to75, irvbfp_sh25to75$LiveDate_Qualifier == "Exact" & irvbfp_sh25to75$UniversalReefID == 198362), aes(x = 196, y = ShellHeight_mm), color = "purple", fill = "purple", lwd = 1, alpha = 0.5, width = 1, inherit.aes = FALSE) +
  #geom_ribbon(data = irvbfp_sh25to75_hist$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.4, inherit.aes = FALSE) +
  #geom_line(data = irvbfp_sh25to75_hist$data, aes(x = RelYear2, y = estimate__), color = "red", lwd = 1, inherit.aes = FALSE) +
  geom_text(data = subset(irvbfp_sh25to75, irvbfp_sh25to75$Sample_mean_age %in% labs1), aes(y = 22, label = Sample_mean_age, x = RelYear2), size = 3.5, col = "grey30", inherit.aes = FALSE) +
  geom_text(aes(y = 23.5, label = "Real-time data \n by reef", x = 192), size = 2.6, col = "grey50", inherit.aes = FALSE) +
  #geom_text(data = subset(irvbfp_sh25to75, irvbfp_sh25to75$Sample_mean_age %in% labs2), aes(y = 18, label = Sample_mean_age, x = RelYear2), size = 3, col = "grey30", inherit.aes = FALSE) +
  theme_bw()  + theme(axis.title = element_text(size = 13), 
                      axis.text = element_text(size = 12), 
                      legend.text = element_text(size = 12), 
                      legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID") +
  #theme(legend.position = "none") +
  coord_cartesian(ylim = c(20, 100))

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_IRVBFP_MEPrand_25to75_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_IRVBFP_MEPrand_25to75,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)



### IRVBFPAP - >75mm -------------------------------------------------------

irvbfp_sho75 <- irvbfp_sho25[ShellHeight_mm >= 75, ]

saveRDS(irvbfp_sho75, here::here(paste0('GLMMs/AllDates/Data/irvbfp_sho75_', Sys.Date(), '.rds')))

irvbfp_sho75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 75, ub = 250) ~ RelYear2 + (1 | UniversalReefID), data = subset(irvbfp_sho75, irvbfp_sho75$Sample_mean_age_qualifier == "Estimate"), family = gaussian, cores = 4, control= list(adapt_delta = 0.999, max_treedepth = 15), iter = 5000, warmup = 1000, chains = 4, inits = 75, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/irvbfp_sho75_glmm_hist6.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- c(1840, 1943, 2001, 2019)

set.seed(986)
#irvbfp_sho75_1 <- plot(conditional_effects(irvbfp_sho75_glmm, re_formula = NULL), plot = FALSE)[[1]]
irvbfp_sho75_hist1 <- plot(conditional_effects(irvbfp_sho75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]

SH_AllDates_GLMM_IRVBFP_MEPrand_o75 <- ggplot(irvbfp_sho75_hist1$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__)) +
  geom_jitter(data = irvbfp_sho75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  geom_line(aes(y = estimate__), color = "red", lwd = 1) +
  geom_vline(xintercept = 185, lwd = 0.5) +
  geom_vline(xintercept = 206, lwd = 0.5, lty = 2, color = "grey50") +
  geom_boxplot(data = subset(irvbfp_sho75, irvbfp_sho75$LiveDate_Qualifier == "Exact"), aes(x = RelYear2, y = ShellHeight_mm), color = "blue", fill = "light blue", lwd = 1, alpha = 0.5, inherit.aes = FALSE) +
  #geom_boxplot(data = subset(irvbfp_sho75, irvbfp_sho75$LiveDate_Qualifier == "Estimate" & irvbfp_sho75$UniversalReefID == 198340), aes(x = 190, y = ShellHeight_mm), color = "salmon", fill = "salmon", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  geom_boxplot(data = subset(irvbfp_sho75, irvbfp_sho75$LiveDate_Qualifier == "Estimate" & irvbfp_sho75$UniversalReefID == 198341), aes(x = 193, y = ShellHeight_mm), color = "gold", fill = "gold", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  geom_boxplot(data = subset(irvbfp_sho75, irvbfp_sho75$LiveDate_Qualifier == "Estimate" & irvbfp_sho75$UniversalReefID == 198345), aes(x = 196, y = ShellHeight_mm), color = "turquoise", fill = "turquoise", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  #geom_boxplot(data = subset(irvbfp_sho75, irvbfp_sho75$LiveDate_Qualifier == "Estimate" & irvbfp_sho75$UniversalReefID == 198360), aes(x = 199, y = ShellHeight_mm), color = "dodgerblue", fill = "dodgerblue", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  #geom_boxplot(data = subset(irvbfp_sho75, irvbfp_sho75$LiveDate_Qualifier == "Estimate" & irvbfp_sho75$UniversalReefID == 198362), aes(x = 202, y = ShellHeight_mm), color = "purple", fill = "purple", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  geom_boxplot(data = subset(irvbfp_sho75, irvbfp_sho75$LiveDate_Qualifier == "Exact" & irvbfp_sho75$UniversalReefID == 198340), aes(x = 209, y = ShellHeight_mm), color = "salmon", fill = "salmon", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  #geom_boxplot(data = subset(irvbfp_sho75, irvbfp_sho75$LiveDate_Qualifier == "Exact" & irvbfp_sho75$UniversalReefID == 198341), aes(x = 212, y = ShellHeight_mm), color = "gold", fill = "gold", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  #geom_boxplot(data = subset(irvbfp_sho75, irvbfp_sho75$LiveDate_Qualifier == "Exact" & irvbfp_sho75$UniversalReefID == 198345), aes(x = 215, y = ShellHeight_mm), color = "turquoise", fill = "turquoise", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  geom_boxplot(data = subset(irvbfp_sho75, irvbfp_sho75$LiveDate_Qualifier == "Exact" & irvbfp_sho75$UniversalReefID == 198360), aes(x = 218, y = ShellHeight_mm), color = "dodgerblue", fill = "dodgerblue", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  geom_boxplot(data = subset(irvbfp_sho75, irvbfp_sho75$LiveDate_Qualifier == "Exact" & irvbfp_sho75$UniversalReefID == 198362), aes(x = 221, y = ShellHeight_mm), color = "purple", fill = "purple", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  #geom_ribbon(data = irvbfp_sho75_hist$data, aes(x = RelYear2, y = ShellHeight_mm, ymin = lower__, ymax = upper__), fill = "grey", alpha = 0.4, inherit.aes = FALSE) +
  #geom_line(data = irvbfp_sho75_hist$data, aes(x = RelYear2, y = estimate__), color = "red", lwd = 1, inherit.aes = FALSE) +
  geom_text(data = subset(irvbfp_sho75, irvbfp_sho75$Sample_mean_age %in% labs1), aes(y = 65, label = Sample_mean_age, x = RelYear2), size = 3.5, col = "grey30", inherit.aes = FALSE) +
  geom_text(aes(y = 72, label = "Historical \n data by \n reef", x = 197), size = 2.6, col = "grey50", inherit.aes = FALSE) +
  geom_text(aes(y = 72, label = "Real-time \n data by \n reef", x = 219), size = 2.6, col = "grey50", inherit.aes = FALSE) +
  geom_text(aes(y = 107, label = "Note that time-averaging is not accounted for in this \n model fit (model makes the unrealistic assumption \n that the estimated sample ages are exactly correct).", x = 0), size = 3, col = "grey50", hjust = 0, inherit.aes = FALSE) +
  #geom_text(data = subset(irvbfp_sho75, irvbfp_sho75$Sample_mean_age %in% labs2), aes(y = 18, label = Sample_mean_age, x = RelYear2), size = 3, col = "grey30", inherit.aes = FALSE) +
  theme_bw() + theme(axis.title = element_text(size = 13), 
                     axis.text = element_text(size = 12), 
                     legend.text = element_text(size = 12), 
                     legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID") #+
#theme(legend.position = "none") +
#coord_cartesian(xlim = c(150, 180))

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_IRVBFP_MEPrand_o75_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_IRVBFP_MEPrand_o75,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)



## Lemon Bay_Natural ---------------------------------------

lb_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                            ShellHeight_mm >= 25 &
                            MA_plotlab == "Lemon Bay_Natural" & 
                            QuadIdentifier %in% setdiff(oysterraw[!is.na(ShellHeight_mm) & ManagedAreaName == "Lemon Bay_Natural", QuadIdentifier], exclude_samps), ]

saveRDS(lb_sho25, here::here(paste0('GLMMs/AllDates/Data/lb_sho25_', Sys.Date(), '.rds')))


### LBAP - 25 to 75mm -------------------------------------------------------

lb_sh25to75 <- subset(lb_sho25, lb_sho25$ShellHeight_mm < 75)

saveRDS(lb_sh25to75, here::here(paste0('GLMMs/AllDates/Data/lb_sh25to75_', Sys.Date(), '.rds')))

lb_sh25to75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 25, ub = 75) ~ me(RelYear2, Sample_age_stdev, gr = QuadIdentifier) + (1 | UniversalReefID), data = lb_sh25to75, family = gaussian, prior = set_prior("normal(53, 20)", class = "b", coef = "meRelYear2Sample_age_stdevgrEQQuadIdentifier"), cores = 4, control= list(adapt_delta = 0.999, max_treedepth = 15), iter = 4000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/lb_sh25to75_glmm_hist4.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- c(1929, 1954, 1972, 1979, 1983, 1995, 2004, 2018)

set.seed(987)
lb_sh25to75_hist1 <- plot(conditional_effects(lb_sh25to75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]
SH_AllDates_GLMM_LB_MEPrand_25to75_hist <- lb_sh25to75_hist1 +
  geom_jitter(data = lb_sh25to75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  geom_line(aes(y = estimate__), color = "red", lwd = 1) +
  #geom_boxplot(data = subset(lb_sh25to75, lb_sh25to75$LiveDate_Qualifier == "Exact"), aes(x = RelYear2, y = ShellHeight_mm), color = "blue", fill = "light blue", lwd = 1, alpha = 0.5, inherit.aes = FALSE) +
  #geom_boxplot(data = subset(lb_sh25to75, lb_sh25to75$UniversalReefID == 168801), aes(x = 80, y = ShellHeight_mm), color = "salmon", fill = "salmon", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  #geom_boxplot(data = subset(lb_sh25to75, lb_sh25to75$UniversalReefID == 168802), aes(x = 83, y = ShellHeight_mm), color = "green", fill = "green", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  #geom_boxplot(data = subset(lb_sh25to75, lb_sh25to75$UniversalReefID == 169320), aes(x = 86, y = ShellHeight_mm), color = "blue", fill = "blue", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  geom_jitter(data = LBLiveSH_5035, aes(RelYear2, ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, color = "black", inherit.aes = FALSE) +
  geom_boxplot(data = LBLiveSH_5035, aes(RelYear2, ShellHeight_mm), color = "blue", fill = "white", alpha = 0.5, lwd = 1, width = 3, inherit.aes = FALSE) +
  geom_text(data = subset(lb_sh25to75, lb_sh25to75$Sample_mean_age %in% labs1), aes(y = 23, label = Sample_mean_age, x = RelYear2), size = 3.5, col = "grey30", inherit.aes = FALSE) +
  geom_text(data = LBLiveSH_5035, aes(y = 22.4, label = "2018 \n(Live)", x = RelYear2), size = 3.5, col = "grey30", inherit.aes = FALSE) +
  #geom_text(aes(y = 23.5, label = "Real-time data \n by reef", x = 192), size = 2.6, col = "grey50", inherit.aes = FALSE) +
  #geom_text(data = subset(lb_sh25to75, lb_sh25to75$Sample_mean_age %in% labs2), aes(y = 18, label = Sample_mean_age, x = RelYear2), size = 3, col = "grey30", inherit.aes = FALSE) +
  theme_bw()  + theme(axis.title = element_text(size = 13), 
                      axis.text = element_text(size = 12), 
                      legend.text = element_text(size = 12), 
                      legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID") #+
#theme(legend.position = "none") +
#coord_cartesian(ylim = c(20, 80))

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_IRVBFP_MEPrand_25to75_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_IRVBFP_MEPrand_25to75,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)



### LBAP - >75mm -------------------------------------------------------

lb_sho75 <- lb_sho25[ShellHeight_mm >= 75, ]

saveRDS(lb_sho75, here::here(paste0('GLMMs/AllDates/Data/lb_sho75_', Sys.Date(), '.rds')))

lb_sho75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 75, ub = 250) ~ RelYear2 + (1 | UniversalReefID), data = lb_sho75, family = gaussian, cores = 4, control= list(adapt_delta = 0.999, max_treedepth = 20), iter = 5000, warmup = 1000, chains = 4, inits = 75, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/lb_sho75_glmm_hist14.rds")

#Important: note that time-averaging is not accounted for in the model fit for the data on shell height >75mm. The measurement error approach I was taking did not result in any models that converged, possibly because the combination of the data and degree of measurement error leads to multiple possible solutions. This means the model reported in this section makes the unrealistic assumption that the estimated sample ages are exactly correct.

# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- c(1929, 1954, 1972, 1979, 1983, 1995, 2004)

set.seed(987)
lb_sho75_hist1 <- plot(conditional_effects(lb_sho75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]
SH_AllDates_GLMM_LB_MEPrand_o75_hist <- lb_sho75_hist1 +
  geom_jitter(data = lb_sho75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  geom_line(aes(y = estimate__), color = "red", lwd = 1) +
  geom_vline(xintercept = 78, lwd = 0.5) +
  #geom_boxplot(data = subset(lb_sho75, lb_sho75$LiveDate_Qualifier == "Exact"), aes(x = RelYear2, y = ShellHeight_mm), color = "blue", fill = "light blue", lwd = 1, alpha = 0.5, inherit.aes = FALSE) +
  geom_boxplot(data = subset(lb_sho75, lb_sho75$UniversalReefID == 168801), aes(x = 81.5, y = ShellHeight_mm), color = "salmon", fill = "salmon", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  geom_boxplot(data = subset(lb_sho75, lb_sho75$UniversalReefID == 168802), aes(x = 84.5, y = ShellHeight_mm), color = "green", fill = "green", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  geom_boxplot(data = subset(lb_sho75, lb_sho75$UniversalReefID == 169320), aes(x = 87.5, y = ShellHeight_mm), color = "blue", fill = "blue", lwd = 1, alpha = 0.5, width = 2, inherit.aes = FALSE) +
  geom_text(data = subset(lb_sho75, lb_sho75$Sample_mean_age %in% labs1), aes(y = 73, label = Sample_mean_age, x = RelYear2), size = 3.5, col = "grey30", inherit.aes = FALSE) +
  geom_text(aes(y = 73.5, label = "All samples \n by reef", x = 84.5), size = 2.6, col = "grey50", inherit.aes = FALSE) +
  geom_text(aes(y = 105, label = "Note that time-averaging is not accounted for in this \n model fit (model makes the unrealistic assumption \n that the estimated sample ages are exactly correct).", x = 0), size = 3, col = "grey50", hjust = 0, inherit.aes = FALSE) +
  #geom_text(data = subset(lb_sho75, lb_sho75$Sample_mean_age %in% labs2), aes(y = 18, label = Sample_mean_age, x = RelYear2), size = 3, col = "grey30", inherit.aes = FALSE) +
  theme_bw()  + theme(axis.title = element_text(size = 13), 
                      axis.text = element_text(size = 12), 
                      legend.text = element_text(size = 12), 
                      legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID") +
  #theme(legend.position = "none") +
  coord_cartesian(xlim = c(0.5, 86.5))

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_LB_MEPrand_o75_hist_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_LB_MEPrand_o75_hist,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)




## St. Martins Marsh_Natural ---------------------------------------

smm_sho25 <- oysterraw[!is.na(ShellHeight_mm) & 
                        ShellHeight_mm >= 25 &
                        MA_plotlab == "St. Martins Marsh_Natural" & 
                        QuadIdentifier %in% setdiff(oysterraw[!is.na(ShellHeight_mm) & ManagedAreaName == "St. Martins Marsh_Natural", QuadIdentifier], exclude_samps), ]

saveRDS(smm_sho25, here::here(paste0('GLMMs/AllDates/Data/smm_sho25_', Sys.Date(), '.rds')))


### SMMAP - 35 to 75mm -------------------------------------------------------

smm_sh35to75 <- subset(smm_sho25, smm_sho25$ShellHeight_mm > 35 & smm_sho25$ShellHeight_mm < 75)

saveRDS(smm_sh35to75, here::here(paste0('GLMMs/AllDates/Data/smm_sh35to75_', Sys.Date(), '.rds')))

smm_sh35to75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 35, ub = 75) ~ RelYear2, data = subset(smm_sh35to75, smm_sh35to75$Sample_mean_age_qualifier == "Estimate"), family = gaussian, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/smm_sh35to75_glmm_hist2.rds")

#Important: note that this analysis is focused on oyster specimens from surveys of archaeological middens and it assumes that the estimated sample ages are correct and that all specimens in a sample lived at the estimated age of the sample (i.e., time-averaging is not accounted for in the model), which is almost certainly not the case. Archaeological samples have an additional type of error that is not present for historical size class estimates from death assemblages (e.g., those from Program ID 5035), which is that they were gathered by humans, so they didn't die in-place; this means a single midden sample is possibly a mix of shells from oysters from multiple different reefs.

# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- c(167, 382, 644, 892, 2018)

set.seed(987)
smm_sh35to75_hist1 <- plot(conditional_effects(smm_sh35to75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]
SH_AllDates_GLMM_SMM_MEPrand_35to75_allYr <- smm_sh35to75_hist1 +
  geom_jitter(data = smm_sh35to75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.1, color = "black", inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  geom_line(aes(y = estimate__), color = "red", lwd = 1) +
  geom_boxplot(data = subset(smm_sh35to75, smm_sh35to75$LiveDate_Qualifier == "Exact"), aes(x = 1850, y = ShellHeight_mm), color = "blue", lwd = 1, alpha = 0.5, width = 30, inherit.aes = FALSE) +
  geom_text(data = subset(smm_sh35to75, smm_sh35to75$Sample_mean_age %in% labs1), aes(y = 33, label = Sample_mean_age, x = RelYear2), size = 3.5, col = "grey30", inherit.aes = FALSE) +
  theme_bw()  + theme(axis.title = element_text(size = 13), 
                      axis.text = element_text(size = 12), 
                      legend.text = element_text(size = 12), 
                      legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID")

SH_AllDates_GLMM_SMM_MEPrand_35to75_rtonly <- SH_AllDates_GLMM_SMM_MEPrand_35to75_allYr
SH_AllDates_GLMM_SMM_MEPrand_35to75_rtonly$layers[[5]] <- NULL
SH_AllDates_GLMM_SMM_MEPrand_35to75_rtonly <- SH_AllDates_GLMM_SMM_MEPrand_35to75_rtonly +
  geom_boxplot(data = subset(smm_sh35to75, smm_sh35to75$Sample_mean_age_qualifier == "Exact"), aes(x = 1850, y = ShellHeight_mm), color = "blue", alpha = 0.15, lwd = 1, width = 4, inherit.aes = FALSE) +
  coord_cartesian(xlim = c(1820, 1852)) +
  theme(legend.position = "right")


ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_SMM_MEPrand_35to75_allYr_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_SMM_MEPrand_35to75_allYr,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_SMM_MEPrand_35to75_rtonly_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_SMM_MEPrand_35to75_rtonly,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)



### SMMAP - >75mm -------------------------------------------------------

smm_sho75 <- smm_sho25[ShellHeight_mm >= 75, ]

saveRDS(smm_sho75, here::here(paste0('GLMMs/AllDates/Data/smm_sho75_', Sys.Date(), '.rds')))

smm_sho75_glmm_hist <- brm(formula = ShellHeight_mm | trunc(lb = 75, ub = 250) ~ RelYear2, data = subset(smm_sho75, smm_sho75$Sample_mean_age_qualifier == "Estimate"), family = gaussian, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/smm_sho75_glmm_hist2.rds")

#Important: note that this analysis is focused on oyster specimens from surveys of archaeological middens and it assumes that the estimated sample ages are correct and that all specimens in a sample lived at the estimated age of the sample (i.e., time-averaging is not accounted for in the model), which is almost certainly not the case. Archaeological samples have an additional type of error that is not present for historical size class estimates from death assemblages (e.g., those from Program ID 5035), which is that they were gathered by humans, so they didn't die in-place; this means a single midden sample is possibly a mix of shells from oysters from multiple different reefs.

# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
labs1 <- c(167, 382, 644, 892, 2018)

set.seed(987)
smm_sho75_hist1 <- plot(conditional_effects(smm_sho75_glmm_hist, re_formula = NULL), plot = FALSE)[[1]]
SH_AllDates_GLMM_SMM_MEPrand_o75_allYr <- smm_sho75_hist1 +
  geom_jitter(data = smm_sho75, aes(x = RelYear2, y = ShellHeight_mm, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.1, color = "black", inherit.aes = FALSE) +
  geom_ribbon(fill = "grey", alpha = 0.4) +
  geom_line(aes(y = estimate__), color = "red", lwd = 1) +
  geom_boxplot(data = subset(smm_sho75, smm_sho75$LiveDate_Qualifier == "Exact"), aes(x = 1850, y = ShellHeight_mm), color = "blue", lwd = 1, alpha = 0.5, width = 30, inherit.aes = FALSE) +
  geom_text(data = subset(smm_sho75, smm_sho75$Sample_mean_age %in% labs1), aes(y = 72, label = Sample_mean_age, x = RelYear2), size = 3.5, col = "grey30", inherit.aes = FALSE) +
  theme_bw()  + theme(axis.title = element_text(size = 13), 
                      axis.text = element_text(size = 12), 
                      legend.text = element_text(size = 12), 
                      legend.title = element_text(size = 13)) +
  labs(fill = "Reef ID")

SH_AllDates_GLMM_SMM_MEPrand_o75_rtonly <- SH_AllDates_GLMM_SMM_MEPrand_o75_allYr
SH_AllDates_GLMM_SMM_MEPrand_o75_rtonly$layers[[5]] <- NULL
SH_AllDates_GLMM_SMM_MEPrand_o75_rtonly <- SH_AllDates_GLMM_SMM_MEPrand_o75_rtonly +
  geom_boxplot(data = subset(smm_sho75, smm_sho75$Sample_mean_age_qualifier == "Exact"), aes(x = 1850, y = ShellHeight_mm), color = "blue", alpha = 0.15, lwd = 1, width = 4, inherit.aes = FALSE) +
  coord_cartesian(xlim = c(1820, 1852)) +
  theme(legend.position = "right")


ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_SMM_MEPrand_o75_allYr_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_SMM_MEPrand_o75_allYr,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

ggsave(here::here(paste0("OA_plots/SH_AllDates_GLMM_SMM_MEPrand_o75_rtonly_", Sys.Date(), ".jpeg")),
       SH_AllDates_GLMM_SMM_MEPrand_o75_rtonly,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)





# Oyster Live Density -----------------------------------------------------

# #Make a collapsed version of the oysterraw table for density
oysterraw_den <- oysterraw[, c("ProgramID", "ProgramName", "ProgramLocationID", "QuadIdentifier", "ReefIdentifier", "GISUniqueID", "SampleDate", "Year", "Month", "ManagedAreaName", "Region.x", "SurveyMethod", "HabitatClassification", "QuadSize_m2", "MADup", "Density_m2", "Number_of_Oysters_Counted_Total_Count", "Number_of_Oysters_Counted_Live_Count", "Number_of_Oysters_Counted_Dead_Count", "ObsIndex", "UniversalReefID", "Region.y", "MA_plotlab", "Subtidal", "RelYear")]
oysterraw_den[!is.na(Density_m2), DensIndex := ObsIndex]
oysterraw_den[!is.na(Number_of_Oysters_Counted_Total_Count), NTotIndex := ObsIndex]
oysterraw_den[!is.na(Number_of_Oysters_Counted_Live_Count), NLiveIndex := ObsIndex]
oysterraw_den[!is.na(Number_of_Oysters_Counted_Dead_Count), NDeadIndex := ObsIndex]
oysterraw_den[, ObsIndex := NULL]

oysterraw_den <- unique(oysterraw_den)
oysterraw_den <- oysterraw_den %>%
  dplyr::group_by(ProgramID, ProgramName, ProgramLocationID, QuadIdentifier,
                  ReefIdentifier, GISUniqueID, SampleDate, Year, Month, ManagedAreaName,
                  Region.x, SurveyMethod, HabitatClassification, QuadSize_m2, MADup, UniversalReefID,
                  Region.y, MA_plotlab, Subtidal) %>%
  tidyr::fill(Density_m2, Number_of_Oysters_Counted_Total_Count,
              Number_of_Oysters_Counted_Live_Count, Number_of_Oysters_Counted_Dead_Count,
              DensIndex, NTotIndex, NLiveIndex, NDeadIndex) %>%
  tidyr::fill(Density_m2, Number_of_Oysters_Counted_Total_Count,
              Number_of_Oysters_Counted_Live_Count, Number_of_Oysters_Counted_Dead_Count,
              DensIndex, NTotIndex, NLiveIndex, NDeadIndex, .direction = 'up') %>%
  dplyr::distinct()

oysterraw_den <- subset(oysterraw_den, !is.na(oysterraw_den$Density_m2) |
                          !is.na(oysterraw_den$Number_of_Oysters_Counted_Total_Count) |
                          !is.na(oysterraw_den$Number_of_Oysters_Counted_Live_Count) |
                          !is.na(oysterraw_den$Number_of_Oysters_Counted_Dead_Count) |
                          !is.na(oysterraw_den$DensIndex) | !is.na(oysterraw_den$NTotIndex) |
                          !is.na(oysterraw_den$NLiveIndex) | !is.na(oysterraw_den$NDeadIndex))
setDT(oysterraw_den)

#Calculate estimated Density_m2 values for ProgramID == 5074. This line can be deleted after Claude recalculates 
#in the combined table. I couldn't include it at the beginning of the script because I need to use the counts columns
#rather than the QuadSize_m2 column which is filled for the whole combined table.
oysterraw_den[ProgramID == 5074, Density_m2 := (Number_of_Oysters_Counted_Total_Count/as.numeric(paste0(QuadSize_m2))) * (Number_of_Oysters_Counted_Live_Count/(Number_of_Oysters_Counted_Live_Count + Number_of_Oysters_Counted_Dead_Count))]


## Generate estimated densities by size class ------------------------------

# Randomly select subsamples of n=20 from each sample with n>20, then subset each random sample to only include specimens 25mm <= sh < 75mm.
oysterraw_sh_r_seed_b20 = oysterraw[0]
oysterraw_sh_o20 = oysterraw[0]
oysterraw_sh_r_seed_o20 = oysterraw[0]
oysterraw_den_r_seed = data.table(ProgramID = character(), 
                                  ProgramName = character(), 
                                  ProgramLocationID = integer(), 
                                  QuadIdentifier = character(), 
                                  ReefIdentifier = character(), 
                                  LiveDate = character(), 
                                  LiveDate_Qualifier = character(), 
                                  LiveDate_MinEstDate = numeric(), 
                                  LiveDate_MaxEstDate = numeric(), 
                                  GISUniqueID = logical(), 
                                  SampleDate = POSIXct(), 
                                  Year = integer(), 
                                  Month = integer(), 
                                  ManagedAreaName = character(), 
                                  Region.x = character(), 
                                  SurveyMethod = character(), 
                                  HabitatClassification = character(), 
                                  MinimumSizeMeasured_mm = integer(), 
                                  NumberMeasured_n = character(), 
                                  QuadSize_m2 = character(), 
                                  MADup = integer(), 
                                  DataFileName = character(), 
                                  UniveralReefID = character(), 
                                  MA_plotlab = character(), 
                                  count = integer(), 
                                  meanSH = numeric(), 
                                  Region.y = character(), 
                                  Subtidal = logical(),
                                  RelYear = integer())
quads <- unique(oysterraw[!is.na(ShellHeight_mm) & LiveDate_Qualifier == "Exact", QuadIdentifier])

# Separate samples with <20 specimens from those with >20 specimens, while subsetting the small samples by size class.
for(i in quads){
  qdat <- oysterraw[!is.na(ShellHeight_mm) & LiveDate_Qualifier == "Exact" & QuadIdentifier == i, ]
  if(length(qdat$ShellHeight_mm) < 20){
    qdat_sub <- qdat[ShellHeight_mm >= 25 & ShellHeight_mm < 75, ]
    oysterraw_sh_r_seed_b20 <- rbind(oysterraw_sh_r_seed_b20, qdat_sub)
  } else {
    oysterraw_sh_o20 <- rbind(oysterraw_sh_o20, qdat)
  }
}

saveRDS(oysterraw_sh_o20, here::here('oysterraw_sh_o20.rds'))

# Take random samples of 20 specimens from each sample larger than 20 specimens and bootstrap 1000x to get an average density of the size class
quads_o20 <- unique(oysterraw_sh_o20$QuadIdentifier)
bycols <- c('ProgramID', 'ProgramName', 'ProgramLocationID', 'QuadIdentifier', 'ReefIdentifier', 'LiveDate', 
            'LiveDate_Qualifier', 'LiveDate_MinEstDate', 'LiveDate_MaxEstDate', 'GISUniqueID', 'SampleDate', 
            'Year', 'Month', 'ManagedAreaName', 'Region.x', 'SurveyMethod', 'HabitatClassification', 
            'MinimumSizeMeasured_mm', 'NumberMeasured_n', 'QuadSize_m2', 'MADup', 'DataFileName', 'UniversalReefID', 
            'MA_plotlab', 'Region.y', 'Subtidal', 'RelYear')
registerDoFuture()
no_cores <- availableCores() - 1
plan(multisession, workers = no_cores)
#set.seed(5678)

tic()
den_r <- foreach(n = 1:1000, .combine = rbind, .packages = c('data.table')) %dorng% {
  foreach(i = quads_o20, .combine = rbind, .packages = c('data.table')) %do% {
    oysterraw_sh_o20[QuadIdentifier == i, ][sample(.N, 20, replace = FALSE)][ShellHeight_mm >= 25 & ShellHeight_mm < 75, ][, .(count = .N, meanSH = mean(ShellHeight_mm)), by = bycols]
  }
}
toc()

for(i in quads_o20) {
  idat <- den_r[QuadIdentifier == i]
  if(length(idat$count) > 0){
    if(max(idat$MADup) == 2){
      idat1 <- idat[MADup == 1, ]
      idat2 <- idat[MADup == 2, ]
      if(length(idat1$count) == 1000 & length(idat2$count) == 1000) next
      zeros1 <- 1000 - length(idat1$count)
      print(paste0('QuadIdentifier: ', i, ', zeros1: ', zeros1))
      zeros2 <- 1000 - length(idat2$count)
      print(paste0('QuadIdentifier: ', i, ', zeros2: ', zeros2))
      if(zeros1 > 0) {
        zerodat1 <- idat1[c(rep(1, times = zeros1)), ][, c('count', 'meanSH') := .(0, NA)]
        den_r <- rbind(den_r, zerodat1)
      }
      
      if(zeros2 > 0) {
        zerodat2 <- idat2[c(rep(1, times = zeros2)), ][, c('count', 'meanSH') := .(0, NA)]
        den_r <- rbind(den_r, zerodat2)
      }
    }
    else{
      if(length(idat$count) == 1000) next
      zeros <- 1000 - length(idat$count)
      print(paste0('QuadIdentifier: ', i, ', zeros: ', zeros))
      
      if(zeros > 0) {
        zerodat <- idat[c(rep(1, times = zeros)), ][, c('count', 'meanSH') := .(0, NA)]
        den_r <- rbind(den_r, zerodat)
      }
    }
  }  
  else{
    idat <- oysterraw_sh_o20[QuadIdentifier == i, c("ProgramID", "ProgramName", "ProgramLocationID", "QuadIdentifier", "ReefIdentifier", "LiveDate", 
                                                  "LiveDate_Qualifier", "LiveDate_MinEstDate", "LiveDate_MaxEstDate", "GISUniqueID", "SampleDate", 
                                                  "Year", "Month", "ManagedAreaName", "Region.x", "SurveyMethod", "HabitatClassification", 
                                                  "MinimumSizeMeasured_mm", "NumberMeasured_n", "QuadSize_m2", "MADup", "DataFileName", 
                                                  "UniversalReefID", "MA_plotlab", "Region.y", "Subtidal", "RelYear")]
    if(max(idat$MADup) == 2){
      idat1 <- idat[MADup == 1, ][1]
      idat2 <- idat[MADup == 2, ][1]
      zeros1 <- 1000
      print(paste0('QuadIdentifier: ', i, ', zeros1: ', zeros1))
      zeros2 <- 1000
      print(paste0('QuadIdentifier: ', i, ', zeros2: ', zeros2))
      zerodat1 <- idat1[c(rep(1, times = zeros1)), ][, c('count', 'meanSH') := .(0, NA)]
      den_r <- rbind(den_r, zerodat1)
      zerodat2 <- idat2[c(rep(1, times = zeros2)), ][, c('count', 'meanSH') := .(0, NA)]
      den_r <- rbind(den_r, zerodat2)
    }
    else{
      zeros <- 1000
      print(paste0('QuadIdentifier: ', i, ', zeros: ', zeros))
      zerodat <- idat[c(rep(1, times = zeros)), ][, c('count', 'meanSH') := .(0, NA)]
      den_r <- rbind(den_r, zerodat)
    }
  }
}

saveRDS(den_r, here::here("den_r.rds"))

oysterraw_den_r_seed <- den_r[, .(nboot = .N, meanCount = mean(count), sdCount = sd(count), meanSH = mean(meanSH), sdSH = sd(meanSH)), by = bycols][, QuadSize_m2 := as.numeric(QuadSize_m2)][, meanDen := meanCount/QuadSize_m2, by = bycols]
saveRDS(oysterraw_den_r_seed, here::here('oysterraw_den_r_seed.rds'))


# Randomly select subsamples of n=20 from each sample with n>20, then subset each random sample to only include specimens >= 75mm sh.
#oysterraw_sh_r_market_o20 = oysterraw_sh[0]
oysterraw_den_r_market <- oysterraw_den_r_seed[0]

# Take random samples of 20 specimens from each sample larger than 20 specimens and bootstrap 1000x to get an average density of the size class RUN THIS CODE BLOCK OUTSIDE OF RSTUDIO!
tic()
den_r_o75 <- foreach(n = 1:1000, .combine = rbind, .packages = c('data.table')) %dorng% {
  foreach(i = quads_o20, .combine = rbind, .packages = c('data.table')) %do% {
    oysterraw_sh_o20[QuadIdentifier == i, ][sample(.N, 20, replace = FALSE)][ShellHeight_mm >= 75, ][, .(count = .N, meanSH = mean(ShellHeight_mm)), by = bycols]
  }
}
toc()


for(i in quads_o20) {
  idat <- den_r_o75[QuadIdentifier == i]
  if(length(idat$count) > 0){
    if(max(idat$MADup) == 2){
      idat1 <- idat[MADup == 1, ]
      idat2 <- idat[MADup == 2, ]
      if(length(idat1$count) == 1000 & length(idat2$count) == 1000) next
      zeros1 <- 1000 - length(idat1$count)
      print(paste0('QuadIdentifier: ', i, ', zeros1: ', zeros1))
      zeros2 <- 1000 - length(idat2$count)
      print(paste0('QuadIdentifier: ', i, ', zeros2: ', zeros2))
      if(zeros1 > 0) {
        zerodat1 <- idat1[c(rep(1, times = zeros1)), ][, c('count', 'meanSH') := .(0, NA)]
        den_r_o75 <- rbind(den_r_o75, zerodat1)
      }
      
      if(zeros2 > 0) {
        zerodat2 <- idat2[c(rep(1, times = zeros2)), ][, c('count', 'meanSH') := .(0, NA)]
        den_r_o75 <- rbind(den_r_o75, zerodat2)
      }
    }
    else{
      if(length(idat$count) == 1000) next
      zeros <- 1000 - length(idat$count)
      print(paste0('QuadIdentifier: ', i, ', zeros: ', zeros))
      
      if(zeros > 0) {
        zerodat <- idat[c(rep(1, times = zeros)), ][, c('count', 'meanSH') := .(0, NA)]
        den_r_o75 <- rbind(den_r_o75, zerodat)
      }
    }
  }  
  else{
    #idat <- oysterraw_sh_o20[QuadIdentifier == i, c(1:16, 18:23, 25:29, 33)]
    idat <- oysterraw_sh_o20[QuadIdentifier == i, c("ProgramID", "ProgramName", "ProgramLocationID", "QuadIdentifier", "ReefIdentifier", "LiveDate", 
                                                  "LiveDate_Qualifier", "LiveDate_MinEstDate", "LiveDate_MaxEstDate", "GISUniqueID", "SampleDate", 
                                                  "Year", "Month", "ManagedAreaName", "Region.x", "SurveyMethod", "HabitatClassification", 
                                                  "MinimumSizeMeasured_mm", "NumberMeasured_n", "QuadSize_m2", "MADup", "DataFileName", 
                                                  "UniversalReefID", "MA_plotlab", "QuadIdentifier", "Region.y", "Subtidal", "RelYear")]
    if(max(idat$MADup) == 2){
      idat1 <- idat[MADup == 1, ][1]
      idat2 <- idat[MADup == 2, ][1]
      zeros1 <- 1000
      print(paste0('QuadIdentifier: ', i, ', zeros1: ', zeros1))
      zeros2 <- 1000
      print(paste0('QuadIdentifier: ', i, ', zeros2: ', zeros2))
      zerodat1 <- idat1[c(rep(1, times = zeros1)), ][, c('count', 'meanSH') := .(0, NA)]
      den_r_o75 <- rbind(den_r_o75, zerodat1)
      zerodat2 <- idat2[c(rep(1, times = zeros2)), ][, c('count', 'meanSH') := .(0, NA)]
      den_r_o75 <- rbind(den_r_o75, zerodat2)
    }
    else{
      zeros <- 1000
      print(paste0('QuadIdentifier: ', i, ', zeros: ', zeros))
      zerodat <- idat[c(rep(1, times = zeros)), ][, c('count', 'meanSH') := .(0, NA)]
      den_r_o75 <- rbind(den_r_o75, zerodat)
    }
  }
}

saveRDS(den_r_o75, here::here('den_r_o75.rds'))

oysterraw_den_r_market <- den_r_o75[, .(nboot = .N, meanCount = mean(count), sdCount = sd(count), meanSH = mean(meanSH), sdSH = sd(meanSH)), by = bycols][, QuadSize_m2 := as.numeric(QuadSize_m2)][, meanDen := meanCount/QuadSize_m2, by = bycols]
saveRDS(oysterraw_den_r_market, here::here('oysterraw_den_r_market.rds'))


#Summarize density data by managed area
den_all_sum <- summarySE(oysterraw_den, measurevar = 'Density_m2', groupvars = c('ManagedAreaName', 'Year'))

#Summarize density data by managed area (mean density 25mm <= x < 75)
#oysterraw_den_r_seed <- readRDS(here::here('oysterraw_den_r_seed.rds'))
oysterraw_den_r_seed[, meanDen_int := round(meanDen, digits = 0)]
den_25to75_sum <- summarySE(oysterraw_den_r_seed, measurevar = 'meanDen', groupvars = c('ManagedAreaName', 'LiveDate'))
colnames(den_25to75_sum)[2] <- "Year"

#Summarize density data by managed area (mean density >= 75)
#oysterraw_den_r_market <- readRDS(here::here('oysterraw_den_r_market.rds'))
oysterraw_den_r_market[, meanDen_int := round(meanDen, digits = 0)]
den_o75_sum <- summarySE(oysterraw_den_r_market, measurevar = 'meanDen', groupvars = c('ManagedAreaName', 'LiveDate'))
colnames(den_o75_sum)[2] <- "Year"




## Raw density results -----------------------------------------------------


### Apalachicola Bay_Natural ----------------------------------------

ab_n <- subset(oysterraw_den, oysterraw_den$MA_plotlab == "Apalachicola Bay_Natural")
ab_n[, Density_m2 := as.integer(round(Density_m2))]
saveRDS(ab_n, here::here(paste0('GLMMs/AllDates/Data/ab_n_', Sys.Date(), '.rds')))

ab_den_glmm <- brm(formula = Density_m2 ~ RelYear + (0 + RelYear | UniversalReefID), data = ab_n, family = negbinomial, cores = 2, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/ab_den_glmm9.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
ab1 <- plot(conditional_effects(ab_den_glmm, re_formula = NULL), plot = FALSE)[[1]]
Den_AllDates_GLMM_AB_MEPrand_raw <- ab1 +
  geom_point(data = ab_n, aes(x = RelYear, y = Density_m2, fill = UniversalReefID), alpha = 0.5, shape = 21, color = "black", inherit.aes = FALSE) +
  geom_text(data = ab_n, aes(y = -25, label = Year, x = RelYear), size = 3, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  labs(fill = "Reef ID")

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_AB_MEPrand_raw_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_AB_MEPrand_raw,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)



### Apalachicola NERR_Natural ----------------------------------------

an_n <- subset(oysterraw_den, oysterraw_den$MA_plotlab == "Apalachicola NERR_Natural")
an_n[, Density_m2 := as.integer(round(Density_m2))]
saveRDS(an_n, here::here(paste0('GLMMs/AllDates/Data/an_n_', Sys.Date(), '.rds')))

an_den_glmm <- brm(formula = Density_m2 ~ RelYear + Subtidal + (0 + RelYear | UniversalReefID), data = an_n, family = zero_inflated_negbinomial, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/an_den_glmm11.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
set.seed(987)
an1 <- plot(conditional_effects(an_den_glmm, re_formula = NULL), plot = FALSE)[[1]]
Den_AllDates_GLMM_AN_MEPrand_raw <- an1 +
  geom_jitter(data = an_n, aes(x = RelYear, y = Density_m2, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_text(data = an_n, aes(y = -900, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  labs(fill = "Reef ID") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13),
        legend.position = "none") +
  labs(colour = NULL)

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_AN_MEPrand_raw_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_AN_MEPrand_raw,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)



### Estero Bay_Natural ----------------------------------------

eb_n <- subset(oysterraw_den, oysterraw_den$MA_plotlab == "Estero Bay_Natural")
eb_n[, Density_m2 := as.integer(round(Density_m2))]
saveRDS(eb_n, here::here(paste0('GLMMs/AllDates/Data/eb_n_', Sys.Date(), '.rds')))

eb_den_glmm <- brm(formula = Density_m2 ~ RelYear + (1 | UniversalReefID), data = eb_n, family = zero_inflated_negbinomial, cores = 2, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/eb_den_glmm10.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
set.seed(987)
eb1 <- plot(conditional_effects(eb_den_glmm, re_formula = NULL), plot = FALSE)[[1]]
Den_AllDates_GLMM_EB_MEPrand_raw <- eb1 +
  geom_jitter(data = eb_n, aes(x = RelYear, y = Density_m2, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_text(data = eb_n, aes(y = -300, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  labs(fill = "Reef ID") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13),
        legend.position = "none") +
  labs(colour = NULL)

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_EB_MEPrand_raw_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_EB_MEPrand_raw,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


### Guana River Marsh_Natural ----------------------------------------

grm_n <- subset(oysterraw_den, oysterraw_den$MA_plotlab == "Guana River Marsh_Natural")
grm_n[, Density_m2 := as.integer(round(Density_m2))]
saveRDS(grm_n, here::here(paste0('GLMMs/AllDates/Data/grm_n_', Sys.Date(), '.rds')))

grm_den_glmm <- brm(formula = Density_m2 ~ RelYear + (1 | UniversalReefID), data = grm_n, family = zero_inflated_negbinomial, cores = 2, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/grm_den_glmm6.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
grm1 <- plot(conditional_effects(grm_den_glmm, re_formula = NULL), plot = FALSE)[[1]]
Den_AllDates_GLMM_GRM_MEPrand_raw <- grm1 +
  geom_jitter(data = grm_n, aes(x = RelYear, y = Density_m2, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_text(data = grm_n, aes(y = -300, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  labs(fill = "Reef ID") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13),
        legend.position = "none") +
  labs(colour = NULL)

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_GRM_MEPrand_raw_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_GRM_MEPrand_raw,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


### Guana Tolomato Matanzas NERR_Natural ----------------------------------------

gtmn_n <- subset(oysterraw_den, oysterraw_den$MA_plotlab == "Guana Tolomato Matanzas NERR_Natural")
gtmn_n[, Density_m2 := as.integer(round(Density_m2))]
saveRDS(gtmn_n, here::here(paste0('GLMMs/AllDates/Data/gtmn_n_', Sys.Date(), '.rds')))

gtmn_den_glmm <- brm(formula = Density_m2 ~ RelYear + Region.y + RelYear:Region.y + (1 | UniversalReefID), data = gtmn_n, family = zero_inflated_negbinomial, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/gtmn_den_glmm18.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
gtmn1 <- plot(conditional_effects(gtmn_den_glmm, re_formula = NULL), plot = FALSE)[[1]]
Den_AllDates_GLMM_GTMNERR_MEPrand_raw <- gtmn1 +
  geom_jitter(data = gtmn_n, aes(x = RelYear, y = Density_m2, fill = Region.y), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_text(data = gtmn_n, aes(y = -500, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  labs(fill = "Region") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13)) +
  labs(colour = NULL)

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_GTMNERR_MEPrand_raw_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_GTMNERR_MEPrand_raw,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


#Plot of modeled mean densities
meanDen_raw <- plot(conditional_effects(gtmn_den_glmm, re_formula = NULL), plot = FALSE)[[2]]$data
setnames(meanDen_raw, "effect1__", "Region")

Den_AllDates_GLMM_GTMNERR_MEPrand_raw_MeanRes <- ggplot(meanDen_raw, aes(x = Region, y = estimate__, ymin = lower__, ymax = upper__)) +
  geom_pointinterval(fill = "black", size = 3, fatten_point = 4, shape = 21, color = "black") +
  ylab("Density_m2") +
  theme_bw()  + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13)) +
  labs(fill = NULL)

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_GTMNERR_MEPrand_raw_MeanRes_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_GTMNERR_MEPrand_raw_MeanRes,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


#Plot of RelYear * Region.y interaction
RelYrbyRegion <- plot(conditional_effects(gtmn_den_glmm, re_formula = NULL), plot = FALSE)[[3]]
#setnames(meanDen_raw, "effect1__", "Region")

Den_AllDates_GLMM_GTMNERR_MEPrand_raw_RelYrbyRegion <- RelYrbyRegion +
  geom_point(data = gtmn_n, aes(x = RelYear, y = Density_m2, fill = Region.y), alpha = 0.5, shape = 21, size = 3, color = "black", inherit.aes = FALSE) +
  geom_text(data = gtmn_n, aes(y = 0, label = "2020", x = 6), size = 4, col = "grey30", inherit.aes = FALSE) +
  scale_x_continuous(limits = c(0, max(gtmn_n$RelYear) + 1)) +
  #ylab("Density_m2") +
  theme_bw()  + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13), 
        legend.position = "none") +
  facet_wrap(~ Region.y, ncol = 3, scales = "free")

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_GTMNERR_MEPrand_raw_RelYrbyRegion_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_GTMNERR_MEPrand_raw_RelYrbyRegion,
       width = 10,
       height = 10,
       units = "in",
       dpi = 300)


### Lemon Bay_Natural ----------------------------------------

lb_n <- subset(oysterraw_den, oysterraw_den$MA_plotlab == "Lemon Bay_Natural")
lb_n[, Density_m2 := as.integer(round(Density_m2))]
saveRDS(lb_n, here::here(paste0('GLMMs/AllDates/Data/lb_n_', Sys.Date(), '.rds')))

lb_den_glmm <- brm(formula = Density_m2 ~ RelYear + (1 | ReefIdentifier), data = lb_n, family = zero_inflated_negbinomial, cores = 2, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/lb_den_glmm6.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
lb1 <- plot(conditional_effects(lb_den_glmm, re_formula = NULL), plot = FALSE)[[1]]
Den_AllDates_GLMM_LB_MEPrand_raw <- lb1 +
  geom_jitter(data = lb_n, aes(x = RelYear, y = Density_m2, fill = ReefIdentifier), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_text(data = lb_n, aes(y = -100, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  labs(fill = "Reef ID") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13)) +
  labs(colour = NULL)

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_LB_MEPrand_raw_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_LB_MEPrand_raw,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


### Pine Island Sound_Natural ----------------------------------------

pis_n <- subset(oysterraw_den, oysterraw_den$MA_plotlab == "Pine Island Sound_Natural")
pis_n[, `:=` (Density_m2 = as.integer(round(Density_m2)),
              Treatment = ifelse(UniversalReefID == 170711, "Reference", "Control"))]
saveRDS(pis_n, here::here(paste0('GLMMs/AllDates/Data/pis_n_', Sys.Date(), '.rds')))

pis_den_glmm <- brm(formula = Density_m2 ~ RelYear + (0 + RelYear | UniversalReefID), data = pis_n, family = zero_inflated_negbinomial, cores = 2, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/pis_den_glmm9.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
set.seed(987)
pis1 <- plot(conditional_effects(pis_den_glmm, re_formula = NULL), plot = FALSE)[[1]]
Den_AllDates_GLMM_PIS_MEPrand_raw <- pis1 +
  geom_jitter(data = pis_n, aes(x = RelYear, y = Density_m2, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_text(data = pis_n, aes(y = -250, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  #ylim(0, 3000) +
  labs(fill = "Reef ID") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13)) +
  labs(colour = NULL)

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_PIS_MEPrand_raw_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_PIS_MEPrand_raw,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


### Pine Island Sound_Restored ----------------------------------------

pisr_n <- subset(oysterraw_den, oysterraw_den$MA_plotlab == "Pine Island Sound_Restored")
pisr_n[, `:=` (Density_m2 = as.integer(round(Density_m2)),
              Treatment = ifelse(UniversalReefID == 170711, "Reference", "Control"))]
saveRDS(pisr_n, here::here(paste0('GLMMs/AllDates/Data/pisr_n_', Sys.Date(), '.rds')))

pisr_den_glmm <- brm(formula = Density_m2 ~ RelYear + QuadSize_m2, data = pisr_n, family = zero_inflated_negbinomial, prior = set_prior("uniform(0,5)", class = "b", lb = 0, ub = 5), cores = 2, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/pisr_den_glmm12.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot; note there is no grouping variable in the model - the data are from a single restored reef.
pisr1 <- plot(conditional_effects(pisr_den_glmm), plot = FALSE)[[1]]
Den_AllDates_GLMM_PISR_MEPrand_raw <- pisr1 +
  geom_jitter(data = pisr_n, aes(x = RelYear, y = Density_m2, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_text(data = pisr_n, aes(y = -250, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  #ylim(0, 3000) +
  labs(fill = "Reef ID") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13)) +
  labs(colour = NULL)

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_PISR_MEPrand_raw_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_PISR_MEPrand_raw,
       width = 5,
       height = 4,
       units = "in",
       dpi = 300)



## Est. density by 25-75mm size class ----------------------------------------


### Apalachicola NERR_Natural ----------------------------------------

an_n_r_seed <- subset(oysterraw_den_r_seed, oysterraw_den_r_seed$MA_plotlab == "Apalachicola NERR_Natural")

saveRDS(an_n_r_seed, here::here(paste0('GLMMs/AllDates/Data/an_n_r_seed_', Sys.Date(), '.rds')))

an_den_r_seed_glmm <- brm(formula = meanDen_int ~ RelYear + QuadSize_m2 + Subtidal + (1 | UniversalReefID), data = an_n_r_seed, family = zero_inflated_negbinomial, cores = 2, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/an_den_r_seed_glmm10b.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
set.seed(987)
an1_r_seed <- plot(conditional_effects(an_den_r_seed_glmm, re_formula = NULL), plot = FALSE)[[1]]
Den_AllDates_GLMM_AN_MEPrand_r_seed <- an1_r_seed +
  geom_jitter(data = an_n_r_seed, aes(x = RelYear, y = meanDen_int, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_text(data = an_n_r_seed, aes(y = -10, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  labs(fill = "Reef ID") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13),
        legend.position = "none") +
  labs(colour = NULL)

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_AN_MEPrand_r_seed_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_AN_MEPrand_r_seed,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


### Estero Bay_Natural ----------------------------------------

eb_n_r_seed <- subset(oysterraw_den_r_seed, oysterraw_den_r_seed$MA_plotlab == "Estero Bay_Natural")

saveRDS(eb_n_r_seed, here::here(paste0('GLMMs/AllDates/Data/eb_n_r_seed_', Sys.Date(), '.rds')))

eb_den_r_seed_glmm <- brm(formula = meanDen_int ~ RelYear + QuadSize_m2, data = eb_n_r_seed, family = negbinomial, cores = 2, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/eb_den_r_seed_glmm7.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
set.seed(987)
eb1_r_seed <- plot(conditional_effects(eb_den_r_seed_glmm, re_formula = NULL), plot = FALSE)[[1]]
Den_AllDates_GLMM_EB_MEP_r_seed <- eb1_r_seed +
  geom_jitter(data = eb_n_r_seed, aes(x = RelYear, y = meanDen_int, color = UniversalReefID, shape = QuadSize_m2), alpha = 0.5, size = 3, width = 0.05, inherit.aes = FALSE) +
  geom_text(data = eb_n_r_seed, aes(y = -10, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  labs(color = "Reef ID") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13)) #,
#legend.position = "none")

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_EB_MEP_r_seed_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_EB_MEP_r_seed,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


#Plot of modeled mean densities
meanDen_r_seed <- plot(conditional_effects(eb_den_r_seed_glmm, re_formula = NULL), plot = FALSE)[[2]]$data
#setnames(meanDen_r_seed, "effect1__", "QuadSize_m2")

Den_AllDates_GLMM_EB_MEP_r_seed_MeanRes <- ggplot(meanDen_r_seed, aes(x = QuadSize_m2, y = estimate__, ymin = lower__, ymax = upper__)) +
  geom_pointinterval(fill = "black", size = 3, fatten_point = 4, shape = 21, color = "black") +
  ylab("Density_m2 (shell height 25-75mm") +
  theme_bw()  + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13))+
  labs(fill = NULL)

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_EB_MEP_r_seed_MeanRes_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_EB_MEP_r_seed_MeanRes,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


### Guana River Marsh_Natural ----------------------------------------

grm_n_r_seed <- subset(oysterraw_den_r_seed, oysterraw_den_r_seed$MA_plotlab == "Guana River Marsh_Natural")

saveRDS(grm_n_r_seed, here::here(paste0('GLMMs/AllDates/Data/grm_n_r_seed_', Sys.Date(), '.rds')))

grm_den_r_seed_glmm <- brm(formula = meanDen_int ~ RelYear + (0 + RelYear | UniversalReefID), data = grm_n_r_seed, family = negbinomial, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/grm_den_r_seed_glmm7.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
set.seed(987)
grm1_r_seed <- plot(conditional_effects(grm_den_r_seed_glmm, re_formula = NULL), plot = FALSE)[[1]]
Den_AllDates_GLMM_GRM_MEPrand_r_seed <- grm1_r_seed +
  geom_jitter(data = grm_n_r_seed, aes(x = RelYear, y = meanDen_int, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_text(data = grm_n_r_seed, aes(y = -10, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  labs(fill = "Reef ID") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13),
        legend.position = "none") +
  labs(colour = NULL)

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_GRM_MEPrand_r_seed_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_GRM_MEPrand_r_seed,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


### Guana Tolomato Matanzas NERR_Natural ----------------------------------------

gtmn_n_r_seed <- subset(oysterraw_den_r_seed, oysterraw_den_r_seed$MA_plotlab == "Guana Tolomato Matanzas NERR_Natural")

saveRDS(gtmn_n_r_seed, here::here(paste0('GLMMs/AllDates/Data/gtmn_n_r_seed_', Sys.Date(), '.rds')))

gtmn_den_r_seed_glmm <- brm(formula = meanDen_int ~ RelYear + Region.y + (1 | UniversalReefID), data = gtmn_n_r_seed, family = negbinomial, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/gtmn_den_r_seed_glmm5c.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
gtmn1_r_seed <- plot(conditional_effects(gtmn_den_r_seed_glmm, re_formula = NULL), plot = FALSE)[[1]]
Den_AllDates_GLMM_GTMNERR_MEPrand_r_seed <- gtmn1_r_seed +
  geom_jitter(data = gtmn_n_r_seed, aes(x = RelYear, y = meanDen_int, fill = Region.y), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_text(data = gtmn_n_r_seed, aes(y = 10, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  coord_cartesian(xlim = c(-0.5,6.5)) +
  labs(fill = "Region") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13)) +
  labs(colour = NULL)

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_GTMNERR_MEPrand_r_seed_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_GTMNERR_MEPrand_r_seed,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

#Plot of modeled mean densities
meanDen_r_seed <- plot(conditional_effects(gtmn_den_r_seed_glmm, re_formula = NULL), plot = FALSE)[[2]]$data
setnames(meanDen_r_seed, "effect1__", "Region")

Den_AllDates_GLMM_GTMNERR_MEPrand_r_seed_MeanRes <- ggplot(meanDen_r_seed, aes(x = Region, y = estimate__, ymin = lower__, ymax = upper__)) +
  geom_pointinterval(fill = "black", size = 3, fatten_point = 4, shape = 21, color = "black") +
  ylab("Density_m2 (shell height 25-75mm") +
  theme_bw()  + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13)) +
  labs(fill = NULL)

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_GTMNERR_MEPrand_r_seed_MeanRes_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_GTMNERR_MEPrand_r_seed_MeanRes,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)



## Est. density by >75mm size class ----------------------------------------

### Estero Bay_Natural ----------------------------------------

eb_n_r_market <- subset(oysterraw_den_r_market_den, oysterraw_den_r_market_den$MA_plotlab == "Estero Bay_Natural")

saveRDS(eb_n_r_market, here::here(paste0('GLMMs/AllDates/Data/eb_n_r_market_', Sys.Date(), '.rds')))

eb_den_r_market_glmm <- brm(formula = meanDen_int ~ RelYear + QuadSize_m2 + (1 | UniversalReefID), data = eb_n_r_market, family = zero_inflated_negbinomial, cores = 2, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/eb_den_r_market_glmm8.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
set.seed(987)
eb1_r_market <- plot(conditional_effects(eb_den_r_market_glmm, re_formula = NULL), plot = FALSE)[[1]]
Den_AllDates_GLMM_EB_MEPrand_r_market <- eb1_r_market +
  geom_jitter(data = eb_n_r_market, aes(x = RelYear, y = meanDen_int, color = UniversalReefID, shape = QuadSize_m2), alpha = 0.5, size = 3, width = 0.05, inherit.aes = FALSE) +
  geom_text(data = eb_n_r_market, aes(y = -10, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  labs(color = "Reef ID") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13)) #,
#legend.position = "none")

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_EB_MEPrand_r_market_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_EB_MEPrand_r_market,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)



### Guana River Marsh_Natural ----------------------------------------

grm_n_r_market <- subset(oysterraw_den_r_market_den, oysterraw_den_r_market_den$MA_plotlab == "Guana River Marsh_Natural")

saveRDS(grm_n_r_market, here::here(paste0('GLMMs/AllDates/Data/grm_n_r_market_', Sys.Date(), '.rds')))

grm_den_r_market_glmm <- brm(formula = meanDen_int ~ RelYear + (1 | UniversalReefID), data = grm_n_r_market, family = zero_inflated_negbinomial, cores = 2, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/grm_den_r_market_glmm6.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
set.seed(987)
grm1_r_market <- plot(conditional_effects(grm_den_r_market_glmm, re_formula = NULL), plot = FALSE)[[1]]
Den_AllDates_GLMM_GRM_MEPrand_r_market <- grm1_r_market +
  geom_jitter(data = grm_n_r_market, aes(x = RelYear, y = meanDen_int, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_text(data = grm_n_r_market, aes(y = -5, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  labs(color = "Reef ID") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13),
        legend.position = "none")

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_GRM_MEPrand_r_market_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_GRM_MEPrand_r_market,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


### Guana Tolomato Matanzas NERR_Natural ----------------------------------------

gtmn_n_r_market <- subset(oysterraw_den_r_market_den, oysterraw_den_r_market_den$MA_plotlab == "Guana Tolomato Matanzas NERR_Natural")

saveRDS(gtmn_n_r_market, here::here(paste0('GLMMs/AllDates/Data/gtmn_n_r_market_', Sys.Date(), '.rds')))

gtmn_den_r_market_glmm <- brm(formula = meanDen_int ~ RelYear + Region.y + (1 | UniversalReefID), data = gtmn_n_r_market, family = zero_inflated_negbinomial, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/gtmn_den_r_market_glmm5.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
gtmn1_r_market <- plot(conditional_effects(gtmn_den_r_market_glmm, re_formula = NULL), plot = FALSE)[[1]]
Den_AllDates_GLMM_GTMNERR_MEPrand_r_market <- gtmn1_r_market +
  geom_jitter(data = gtmn_n_r_market, aes(x = RelYear, y = meanDen_int, fill = Region.y), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_text(data = gtmn_n_r_market, aes(y = -10, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  coord_cartesian(xlim = c(-0.5,6.5)) +
  labs(fill = "Region") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13)) +
  labs(colour = NULL)

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_GTMNERR_MEPrand_r_market_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_GTMNERR_MEPrand_r_market,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

#Plot of modeled mean densities
meanDen_r_market <- plot(conditional_effects(gtmn_den_r_market_glmm, re_formula = NULL), plot = FALSE)[[2]]$data
setnames(meanDen_r_market, "effect1__", "Region")

Den_AllDates_GLMM_GTMNERR_MEPrand_r_market_MeanRes <- ggplot(meanDen_r_market, aes(x = Region, y = estimate__, ymin = lower__, ymax = upper__)) +
  geom_pointinterval(fill = "black", size = 3, fatten_point = 4, shape = 21, color = "black") +
  ylab("Density_m2 (shell height >75mm)") +
  theme_bw()  + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13))+
  labs(fill = NULL)

ggsave(here::here(paste0("OA_plots/Den_AllDates_GLMM_GTMNERR_MEPrand_r_market_MeanRes_", Sys.Date(), ".jpeg")),
       Den_AllDates_GLMM_GTMNERR_MEPrand_r_market_MeanRes,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)




# Oyster Percent Live -----------------------------------------------------

#Make a collapsed version of the oysterraw table for density
oysterraw_pct <- oysterraw[, c("ProgramID", "ProgramName", "ProgramLocationID", "QuadIdentifier", "ReefIdentifier", "GISUniqueID", "SampleDate", "Year", "Month", "ManagedAreaName", "Region.x", "SurveyMethod", "PercentLiveMethod", "HabitatClassification", "QuadSize_m2", "MADup", "PercentLive_pct", "Number_of_Oysters_Counted_Total_Count", "Number_of_Oysters_Counted_Live_Count", "Number_of_Oysters_Counted_Dead_Count", "ObsIndex", "UniversalReefID", "Region.y", "MA_plotlab", "Subtidal", "RelYear")]
oysterraw_pct[!is.na(PercentLive_pct), PctIndex := ObsIndex]
oysterraw_pct[!is.na(Number_of_Oysters_Counted_Total_Count), NTotIndex := ObsIndex]
oysterraw_pct[!is.na(Number_of_Oysters_Counted_Live_Count), NLiveIndex := ObsIndex]
oysterraw_pct[!is.na(Number_of_Oysters_Counted_Dead_Count), NDeadIndex := ObsIndex]
oysterraw_pct[, ObsIndex := NULL]

oysterraw_pct <- unique(oysterraw_pct)
oysterraw_pct <- oysterraw_pct %>%
  dplyr::group_by(ProgramID, ProgramName, ProgramLocationID, QuadIdentifier,
                  ReefIdentifier, GISUniqueID, SampleDate, Year, Month, ManagedAreaName,
                  Region.x, SurveyMethod, PercentLiveMethod, HabitatClassification, QuadSize_m2, MADup,
                  UniversalReefID, Region.y, MA_plotlab, Subtidal, RelYear) %>%
  tidyr::fill(PercentLive_pct, Number_of_Oysters_Counted_Total_Count,
              Number_of_Oysters_Counted_Live_Count, Number_of_Oysters_Counted_Dead_Count,
              PctIndex, NTotIndex, NLiveIndex, NDeadIndex) %>%
  tidyr::fill(PercentLive_pct, Number_of_Oysters_Counted_Total_Count,
              Number_of_Oysters_Counted_Live_Count, Number_of_Oysters_Counted_Dead_Count,
              PctIndex, NTotIndex, NLiveIndex, NDeadIndex, .direction = 'up') %>%
  dplyr::distinct()

oysterraw_pct <- subset(oysterraw_pct, !is.na(oysterraw_pct$PercentLive_pct) |
                          !is.na(oysterraw_pct$Number_of_Oysters_Counted_Total_Count) |
                          !is.na(oysterraw_pct$Number_of_Oysters_Counted_Live_Count) |
                          !is.na(oysterraw_pct$Number_of_Oysters_Counted_Dead_Count) |
                          !is.na(oysterraw_pct$PctIndex) | !is.na(oysterraw_pct$NTotIndex) |
                          !is.na(oysterraw_pct$NLiveIndex) | !is.na(oysterraw_pct$NDeadIndex))


#Calculate PercentLive_pct values for some ProgramIDs where it is missing. This line can be deleted after Claude recalculates 
#in the combined table. I couldn't include it at the beginning of the script because I need to use the counts columns
#rather than the QuadSize_m2 column which is filled for the whole combined table.
oysterraw_pct[ProgramID == 972 | ProgramID == 4014 | ProgramID == 4044, PercentLive_pct := (Number_of_Oysters_Counted_Live_Count/(Number_of_Oysters_Counted_Live_Count + Number_of_Oysters_Counted_Dead_Count) * 100)]

#Filter NAs for PercentLive_pct (these are related to 1) programs that do counts to measure density, but do not estimate percent live and
#2) Programs that are listed as measuring percent live by a Point-intercept method, which cannot be calculated from counts.
oysterraw_pct <- oysterraw_pct[!is.na(PercentLive_pct), ]

#Summarize percent live values
pct_all_sum <- summarySE(oysterraw_pct, measurevar = 'PercentLive_pct', groupvars = c('ManagedAreaName', 'Year', 'PercentLiveMethod'))



## Guana River Marsh_Natural ----------------------------------------

grm_p <- subset(oysterraw_pct, oysterraw_pct$MA_plotlab == "Guana River Marsh_Natural")
saveRDS(grm_p, here::here(paste0('GLMMs/AllDates/Data/grm_p_', Sys.Date(), '.rds')))

grm_p_binom <- data.table(ProgramID = character(), ProgramLocationID = character(), QuadIdentifier = character(), Year = integer(), ManagedAreaName = character(), PercentLiveMethod = character(), UniversalReefID = factor(), Region.y = character(), MA_plotlab = character(), RelYear = integer(), PercentLive_pct = numeric(), LiveObs = logical())
for(i in 1:nrow(grm_p)){
  dat_i <- grm_p[i, c("ProgramID", "ProgramLocationID", "QuadIdentifier", "Year", "ManagedAreaName", "PercentLiveMethod", "UniversalReefID", "Region.y", "MA_plotlab", "RelYear", "PercentLive_pct")]
  dat_l <- purrr::map_dfr(seq_len(round(dat_i$PercentLive_pct[1], digits = 0)), ~dat_i[, LiveObs := 1])
  dat_nl <- purrr::map_dfr(seq_len((100 - round(dat_i$PercentLive_pct[1], digits = 0))), ~dat_i[, LiveObs := 0])
  dat <- rbind(dat_l, dat_nl)
  grm_p_binom <- rbind(grm_p_binom, dat)
}
saveRDS(grm_p_binom, here::here(paste0('GLMMs/AllDates/Data/grm_p_binom_', Sys.Date(), '.rds')))

grm_pct_glmm <- brm(formula = LiveObs ~ RelYear + (1 | UniversalReefID), data = grm_p_binom, family = bernoulli, cores = 4, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/grm_pct_glmm3.rds")


# Model results table


# Posterior distributions and Markov chains:



# Posterior predictive check plot:



#Marginal effects plot including random effects
set.seed(987)
grm_p1 <- plot(conditional_effects(grm_pct_glmm, re_formula = NULL), plot = FALSE)[[1]]
Pct_AllDates_GLMM_GRM_MEPrand_raw <- grm_p1 +
  geom_jitter(data = grm_p, aes(x = RelYear, y = PercentLive_dec, fill = UniversalReefID), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_text(data = grm_p, aes(y = -0.05, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  coord_cartesian(xlim = c(-0.5,6.5), ylim = c(-0.05, 1)) +
  labs(fill = "Reef ID") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13), 
        legend.position = "none") +
  labs(colour = NULL)

ggsave(here::here(paste0("OA_plots/Pct_AllDates_GLMM_GRM_MEPrand_raw_", Sys.Date(), ".jpeg")),
       Pct_AllDates_GLMM_GRM_MEPrand_raw,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


## Guana Tolomato Matanzas NERR_Natural ----------------------------------------

gtmn_p <- subset(oysterraw_pct, oysterraw_pct$MA_plotlab == "Guana Tolomato Matanzas NERR_Natural")
saveRDS(gtmn_p, here::here(paste0('GLMMs/AllDates/Data/gtmn_p_', Sys.Date(), '.rds')))

gtmn_p_binom <- data.table(ProgramID = character(), ProgramLocationID = character(), QuadIdentifier = character(), Year = integer(), ManagedAreaName = character(), PercentLiveMethod = character(), UniversalReefID = factor(), Region.y = character(), MA_plotlab = character(), RelYear = integer(), PercentLive_pct = numeric(), LiveObs = logical())
for(i in 1:nrow(gtmn_p)){
  dat_i <- gtmn_p[i, c("ProgramID", "ProgramLocationID", "QuadIdentifier", "Year", "ManagedAreaName", "PercentLiveMethod", "UniversalReefID", "Region.y", "MA_plotlab", "RelYear", "PercentLive_pct")]
  dat_l <- purrr::map_dfr(seq_len(round(dat_i$PercentLive_pct[1], digits = 0)), ~dat_i[, LiveObs := 1])
  dat_nl <- purrr::map_dfr(seq_len((100 - round(dat_i$PercentLive_pct[1], digits = 0))), ~dat_i[, LiveObs := 0])
  dat <- rbind(dat_l, dat_nl)
  gtmn_p_binom <- rbind(gtmn_p_binom, dat)
}
saveRDS(gtmn_p_binom, here::here(paste0('GLMMs/AllDates/Data/gtmn_p_binom_', Sys.Date(), '.rds')))

gtmn_pct_glmm <- brm(formula = LiveObs ~ RelYear + Region.y + RelYear:Region.y + (1 | UniversalReefID), data = gtmn_p_binom, family = bernoulli, cores = 4, control = list(adapt_delta = 0.8, max_treedepth = 10), iter = 3000, warmup = 1000, chains = 4, thin = 3, backend = "cmdstanr", threads = threading(3), file = "GLMMs/AllDates/gtmn_pct_glmm6.rds")


# Posterior distributions and Markov chains:
Pct_AllDates_GLMM_GTMNERR_PDistandMChains_raw <- plot(gtmn_pct_glmm)

len <- length(Pct_AllDates_GLMM_GTMNERR_PDistandMChains_raw)
for(i in 1:len){
  jpeg(filename = here::here(paste0("OA_plots/Pct_AllDates_GLMM_GTMNERR_PDistandMChains_raw_", i, "of", len, "_", Sys.Date(), ".jpeg")), width = 4, height = 6, units = "in", quality = 100, res = 300)
  print(Pct_AllDates_GLMM_GTMNERR_PDistandMChains_raw[i])
  dev.off()
}

test test test

# Posterior predictive check plot:
Pct_AllDates_GLMM_GTMNERR_PPcheck_raw <- try(pp_check(gtmn_pct_glmm), silent = TRUE)

if(class(Pct_AllDates_GLMM_GTMNERR_PPcheck_raw) != "try-error"){
  ggsave(here::here(paste0("OA_plots/Pct_AllDates_GLMM_GTMNERR_PPcheck_raw_", Sys.Date(), ".jpeg")),
         Pct_AllDates_GLMM_GTMNERR_PPcheck_raw,
         width = 4,
         height = 3,
         units = "in",
         dpi = 300)
}

#Marginal effects plot including random effects
set.seed(987)
gtmn_p1 <- plot(conditional_effects(gtmn_pct_glmm, re_formula = NULL), plot = FALSE)[[1]]
Pct_AllDates_GLMM_GTMNERR_MEPrand_raw <- gtmn_p1 +
  geom_jitter(data = gtmn_p, aes(x = RelYear, y = PercentLive_dec, fill = Region.y), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_text(data = gtmn_p, aes(y = -0.05, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  theme_bw() +
  coord_cartesian(xlim = c(-0.5,6.5), ylim = c(-0.05, 1)) +
  labs(fill = "Region") + theme(axis.title = element_text(size = 13), 
                                axis.text = element_text(size = 12), 
                                legend.text = element_text(size = 12), 
                                legend.title = element_text(size = 13)) +
  labs(colour = NULL)

ggsave(here::here(paste0("OA_plots/Pct_AllDates_GLMM_GTMNERR_MEPrand_raw_", Sys.Date(), ".jpeg")),
       Pct_AllDates_GLMM_GTMNERR_MEPrand_raw,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

#Plot of modeled mean densities
meanPct <- plot(conditional_effects(gtmn_pct_glmm, re_formula = NULL), plot = FALSE)[[2]]$data
setnames(meanPct, "effect1__", "Region")

Pct_AllDates_GLMM_GTMNERR_MEPrand_raw_MeanRes <- ggplot(meanPct, aes(x = Region, y = estimate__, ymin = lower__, ymax = upper__)) +
  geom_pointinterval(fill = "black", size = 3, fatten_point = 4, shape = 21, color = "black") +
  ylab("PercentLive_dec") +
  theme_bw()  + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13)) +
  labs(fill = NULL)

ggsave(here::here(paste0("OA_plots/Pct_AllDates_GLMM_GTMNERR_MEPrand_raw_MeanRes_", Sys.Date(), ".jpeg")),
       Pct_AllDates_GLMM_GTMNERR_MEPrand_raw_MeanRes,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


#Plot of RelYear * Region.y interaction
RelYrbyRegion <- plot(conditional_effects(gtmn_pct_glmm, re_formula = NULL), plot = FALSE)[[3]]

Pct_AllDates_GLMM_GTMNERR_MEPrand_raw_RelYrbyRegion <- RelYrbyRegion +
  geom_point(data = gtmn_p, aes(x = RelYear, y = PercentLive_dec, fill = Region.y), alpha = 0.5, shape = 21, size = 3, color = "black", inherit.aes = FALSE) +
  geom_text(data = gtmn_p, aes(y = 0, label = "2020", x = 6), size = 4, col = "grey30", inherit.aes = FALSE) +
  scale_x_continuous(limits = c(0, max(gtmn_p$RelYear) + 1)) +
  #ylab("Density_m2") +
  theme_bw()  + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13), 
        legend.position = "none") +
  facet_wrap(~ Region.y, ncol = 3, scales = "free")

ggsave(here::here(paste0("OA_plots/Pct_AllDates_GLMM_GTMNERR_MEPrand_raw_RelYrbyRegion_", Sys.Date(), ".jpeg")),
       Pct_AllDates_GLMM_GTMNERR_MEPrand_raw_RelYrbyRegion,
       width = 10,
       height = 10,
       units = "in",
       dpi = 300)


## Lemon Bay_Natural ----------------------------------------

lb_p <- subset(oysterraw_pct, oysterraw_pct$MA_plotlab == "Lemon Bay_Natural")
lb_p[, PercentLive_dec := PercentLive_pct/100] #PercentLiveMethod == "Percent" for Lemon Bay program(s) with sufficient data, so cannot be modeled as binomial
saveRDS(lb_p, here::here(paste0('GLMMs/AllDates/Data/lb_p_', Sys.Date(), '.rds')))

lb_pct_glmm <- brm(formula = PercentLive_dec ~ RelYear + (0 + RelYear | ReefIdentifier), data = subset(lb_p, lb_p$PercentLive_dec > 0), family = Beta, cores = 2, control= list(adapt_delta = 0.99, max_treedepth = 15), iter = 3000, warmup = 1000, chains = 4, inits = 0, thin = 3, backend = "cmdstanr", threads = threading(2), file = "GLMMs/AllDates/lb_pct_glmm6.rds")

# Posterior distributions and Markov chains:
Pct_AllDates_GLMM_LB_PDistandMChains_raw <- plot(lb_pct_glmm)

len <- length(Pct_AllDates_GLMM_LB_PDistandMChains_raw)
for(i in 1:len){
  jpeg(filename = here::here(paste0("OA_plots/Pct_AllDates_GLMM_LB_PDistandMChains_raw_", i, "of", len, "_", Sys.Date(), ".jpeg")), width = 4, height = 6, units = "in", quality = 100, res = 300)
  print(Pct_AllDates_GLMM_LB_PDistandMChains_raw[i])
  dev.off()
}

# Posterior predictive check plot:
Pct_AllDates_GLMM_LB_PPcheck_raw <- try(pp_check(lb_pct_glmm), silent = TRUE)

if(class(Pct_AllDates_GLMM_LB_PPcheck_raw) != "try-error"){
  ggsave(here::here(paste0("OA_plots/Pct_AllDates_GLMM_LB_PPcheck_raw_", Sys.Date(), ".jpeg")),
         Pct_AllDates_GLMM_LB_PPcheck_raw,
         width = 4,
         height = 3,
         units = "in",
         dpi = 300)
}

#Marginal effects plot including random effects
set.seed(987)
lb_p1 <- plot(conditional_effects(lb_pct_glmm, re_formula = NULL), plot = FALSE)[[1]]
Pct_AllDates_GLMM_LB_MEPrand_raw <- lb_p1 +
  geom_jitter(data = lb_p, aes(x = RelYear, y = PercentLive_dec, fill = ReefIdentifier), alpha = 0.5, shape = 21, size = 3, width = 0.05, color = "black", inherit.aes = FALSE) +
  geom_text(data = lb_p, aes(y = -0.05, label = Year, x = RelYear), size = 4, col = "grey30", inherit.aes = FALSE) +
  geom_text(aes(y = 0.03, label = "Note: zero-value data points were \n excluded from the model fit", x = 3), size = 3, col = "grey60", inherit.aes = FALSE) +
  theme_bw() +
  coord_cartesian(xlim = c(-0.5,10.5), ylim = c(-0.05, 1)) +
  labs(fill = "Reef ID") + 
  theme(axis.title = element_text(size = 13), 
        axis.text = element_text(size = 12), 
        legend.text = element_text(size = 12), 
        legend.title = element_text(size = 13)) +
  labs(colour = NULL)

ggsave(here::here(paste0("OA_plots/Pct_AllDates_GLMM_LB_MEPrand_raw_", Sys.Date(), ".jpeg")),
       Pct_AllDates_GLMM_LB_MEPrand_raw,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

