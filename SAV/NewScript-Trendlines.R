# data directory and loading SAV4 output from previous script
data_dir <- here::here("output")
file_in <- list.files(data_dir, pattern="SAV_DataUsed.txt", full=TRUE)

SAV4 <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
              na.strings=c("NULL","","NA"))

addfits <- function(models, plot_i, param) {
  # aucol determines whether analysisunit or analysisunit_halid is used
  aucol <- as.name(names(plot_i$data)[1])
  # empty data frame to fill with regression data
  regression_data <- data.frame()
  
  for (i in seq_along(models)) {
    # finding model name, calling previously created model variable
    model <- get(models[[i]])
    
    # declaring species & managed area of each model
    species <- unique(model$data[[aucol]])
    managed_area <- unique(model$data$ManagedAreaName)
    
    #extract p-value
    p_val <- summary(model)$tTab[2,5]
    
    # only plot models with p-values <= 0.05
    if(p_val <= 0.05) {
      
      # filter dataframe for managed_area & species
      species_data <- SAV4 %>%
        filter(ManagedAreaName == managed_area,
               !is.na({{p}}),
               {{ aucol }} == species)
      
      # create predicted values variable for each model
      predicted_values <- predict(model, level = 0, newdata = species_data)
      
      # Add predicted values to the regression_data dataframe, with species & relyear
      regression_data <- rbind(regression_data, data.frame(
        relyear = species_data$relyear,
        fit = predicted_values,
        species = unique(species_data[[aucol]])))
      
      # Plot all the regression lines on the same plot
      plot_i <- plot_i +
        geom_line(data = regression_data,
                  aes(x = relyear, y = fit, color = species),
                  size = 1.2, alpha = 0.9, inherit.aes = FALSE)
    }
  }
  return(plot_i)
}

# Specify what to produce --------------
EDA <- "no" #Create and export Exploratory Data Analysis plots ("maps and plots" = create all EDA output, 
#                                                   "maps" = create geographic scope maps only,
#                                                   "plots" = create data exploration plots only,
#                                                   "no" (or anything else) = skip all EDA output)

Analyses <- c("BB_pct", "PC") #Which analyses to run? c("BB_all," "BB_pct", "PC", "PO", and/or "PA") or c("none") for just EDA plotting

#Empty data.table to house names of any failed models generated below.
failedmods <- data.table(model = character(),
                         error = character())

#Create a table of the proportion of present SAV types by managed area and year
props_halid <- SAV4 %>% filter(str_detect(analysisunit_halid, "Total|Drift|spp\\.", negate = TRUE), !is.na(PA)) %>% group_by(ManagedAreaName, analysisunit_halid, relyear) %>% summarize(n_P = sum(PA), ntot_PA = n(), prop_P = n_P/ntot_PA)
# props_halcom <- SAV4 %>% filter(str_detect(analysisunit_halcom, "Total", negate = TRUE)) %>% group_by(ManagedAreaName, analysisunit_halcom, relyear) %>% summarize(n_P_halcom = sum(PA), ntot_PA_halcom = n(), prop_P_halcom = n_P_halcom/ntot_PA_halcom)
props <- SAV4 %>% filter(str_detect(analysisunit, "Total|Drift|decipiens|engelmannii|johnsonii|Unidentified", negate = TRUE), !is.na(PA)) %>% group_by(ManagedAreaName, analysisunit, relyear) %>% summarize(n_P = sum(PA), ntot_PA = n(), prop_P = n_P/ntot_PA)
setDT(props_halid)
setDT(props)
for(m in unique(props_halid$ManagedAreaName)){
  props_halid[ManagedAreaName == m, `:=` (n_allsp_P = sum(n_P), sp_prop = n_P/sum(n_P), sp_pct = (n_P/sum(n_P)) * 100), by = c("relyear")]
}
for(m in unique(props$ManagedAreaName)){
  # props_halcom[ManagedAreaName == m, `:=` (n_allsp_P_halcom = sum(n_P_halcom), sp_prop_halcom = n_P_halcom/sum(n_P_halcom), sp_pct_halcom = (n_P_halcom/sum(n_P_halcom)) * 100), by = c("relyear")]
  props[ManagedAreaName == m, `:=` (n_allsp_P = sum(n_P), sp_prop = n_P/sum(n_P), sp_pct = (n_P/sum(n_P)) * 100), by = c("relyear")]
}
setnames(props_halid, "analysisunit_halid", "analysisunit")
props2 <- distinct(rbind(props_halid, props))
setorder(props2, ManagedAreaName, relyear, analysisunit)
props <- props2

spcollist <- c("#005396",
                        "#0088B1",
                        "#00ADAE",
                        "#65CCB3",
                        "#AEE4C1",
                        "#FDEBA8",
                        "#F8CD6D",
                        "#F5A800",
                        "#F17B00")
                        
# spcollist_a <- sequential_hcl(8, palette = "YlOrRd")
# spcollist_b <- sequential_hcl(8, palette = "YlGnBu", rev = TRUE)
# spcollist <- append(spcollist_a[4:7], spcollist_b[3:7])
# spcollist <- rev(spcollist)

# spcollist <- hcl.colors(n = 8, palette = "Blues 3")
spp <- c("Halodule wrightii", "Halophila decipiens", "Halophila engelmannii", "Halophila johnsonii", 
         "Halophila spp.", "Ruppia maritima", "Syringodium filiforme", "Thalassia testudinum", "Attached algae")

spp_common <- c("Halophila spp.", "Johnson's seagrass", "Manatee grass", "Paddle grass", 
                "Shoal grass", "Star grass", "Turtle grass", "Widgeon grass", "Attached algae")

usenames <- "common" #alternative is "scientific"
if(usenames == "common"){
  spcols <- setNames(spcollist, spp_common)
} else{
  spcols <- setNames(spcollist, spp)
}

spindet_nm <- c("Unidentified Halophila")
spindet_cl <- spcols["Halophila spp."][[1]]
spindet <- setNames(spindet_cl, spindet_nm)
spcols <- append(spcols, spindet, after = which(spcols == spcols["Halophila spp."][[1]]))

if(usenames == "common"){
  SAV4[, `:=` (analysisunit_halid = fcase(analysisunit_halid == "Thalassia testudinum", "Turtle grass",
                                          analysisunit_halid == "Syringodium filiforme", "Manatee grass",
                                          analysisunit_halid == "Halodule wrightii", "Shoal grass",
                                          analysisunit_halid == "Ruppia maritima", "Widgeon grass",
                                          analysisunit_halid == "Halophila decipiens", "Paddle grass",
                                          analysisunit_halid == "Halophila engelmannii", "Star grass",
                                          analysisunit_halid == "Halophila johnsonii", "Johnson's seagrass",
                                          analysisunit_halid == "Unidentified Halophila", "Unidentified Halophila",
                                          analysisunit_halid == "Halophila spp.", "Halophila spp.",
                                          analysisunit_halid == "Total seagrass", "Total seagrass",
                                          analysisunit_halid == "Attached algae", "Attached algae",
                                          analysisunit_halid == "Drift algae", "Drift algae",
                                          analysisunit_halid == "Total SAV", "Total SAV"),
               analysisunit = fcase(analysisunit == "Thalassia testudinum", "Turtle grass",
                                    analysisunit == "Syringodium filiforme", "Manatee grass",
                                    analysisunit == "Halodule wrightii", "Shoal grass",
                                    analysisunit == "Ruppia maritima", "Widgeon grass",
                                    analysisunit == "Halophila decipiens", "Paddle grass",
                                    analysisunit == "Halophila engelmannii", "Star grass",
                                    analysisunit == "Halophila johnsonii", "Johnson's seagrass",
                                    analysisunit == "Unidentified Halophila", "Unidentified Halophila",
                                    analysisunit == "Halophila spp.", "Halophila spp.",
                                    analysisunit == "Total seagrass", "Total seagrass",
                                    analysisunit == "Attached algae", "Attached algae",
                                    analysisunit == "Drift algae", "Drift algae",
                                    analysisunit == "Total SAV", "Total SAV"))]
  
  props[, analysisunit := fcase(analysisunit == "Thalassia testudinum", "Turtle grass",
                                analysisunit == "Syringodium filiforme", "Manatee grass",
                                analysisunit == "Halodule wrightii", "Shoal grass",
                                analysisunit == "Ruppia maritima", "Widgeon grass",
                                analysisunit == "Halophila decipiens", "Paddle grass",
                                analysisunit == "Halophila engelmannii", "Star grass",
                                analysisunit == "Halophila johnsonii", "Johnson's seagrass",
                                analysisunit == "Unidentified Halophila", "Unidentified Halophila",
                                analysisunit == "Halophila spp.", "Halophila spp.",
                                analysisunit == "Attached algae", "Attached algae")]
  
  props <- props[, analysisunit := factor(analysisunit, levels = c("Unidentified Halophila",
                                                                   "Halophila spp.",
                                                                   "Johnson's seagrass",
                                                                   "Manatee grass",
                                                                   "Paddle grass",
                                                                   "Shoal grass",
                                                                   "Star grass",
                                                                   "Turtle grass",
                                                                   "Widgeon grass",
                                                                   "Attached algae"))]
}

if(usenames != "common"){
  props <- props[, analysisunit := factor(analysisunit, levels = c("Halodule wrightii",
                                                                   "Halophila decipiens",
                                                                   "Halophila engelmannii",
                                                                   "Halophila johnsonii",
                                                                   "Unidentified Halophila",
                                                                   "Halophila spp.",
                                                                   "Ruppia maritima",
                                                                   "Syringodium filiforme",
                                                                   "Thalassia testudinum",
                                                                   "Attached algae"))]
}

# prcollist <- hcl.colors(n = length(unique(SAV4$ProgramID)), palette = "viridis")
prcollist_a <- sequential_hcl(length(unique(SAV4$ProgramName)), palette = "YlOrRd")
prcollist_b <- sequential_hcl(length(unique(SAV4$ProgramName)), palette = "YlGnBu", rev = TRUE)
prcollist <- append(prcollist_a[which(seq(1, length(prcollist_a)) %% 2 == 0)], 
                    prcollist_b[which(seq(1, length(prcollist_b)) %% 2 != 0)])
prcollist <- rev(prcollist)
set.seed(4691)
progs <- sample(sort(unique(SAV4$ProgramName)))
prcols <- setNames(prcollist, progs)

parameters <- data.table(column = c(as.name("BB_all"), as.name("BB_pct"), as.name("PC"), as.name("PO"), as.name("PA")),
                         name = c("Braun Blanquet score", "Median percent cover", "Visual percent cover", "Percent occurrence", "Frequency of occurrence"),
                         type = c("BBall", "BBpct", "PC", "PO", "PA"))

plot_theme <- theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Arial"),
        # title = element_text(face="bold"),
        plot.title = element_text(hjust = 0.5, size = 12, color = "#314963"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#314963"),
        legend.title = element_text(size = 10),
        legend.text.align = 0,
        axis.title.x = element_text(size = 10, margin = margin(t = 5, r = 0,
                                                               b = 10, l = 0)),
        axis.title.y = element_text(size = 10, margin = margin(t = 0, r = 10,
                                                               b = 0, l = 0)),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = -45, hjust = 0))

#Managed areas that should have Halophila species combined:
ma_halspp <- c("Banana River", "Indian River-Malabar to Vero Beach", "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet",
               "Loxahatchee River-Lake Worth Creek", "Mosquito Lagoon", "Biscayne Bay", "Florida Keys NMS")

#save summary stats file
stats_pct <- SAV4[ManagedAreaName %in% ma_halspp, ] %>%
  group_by(ManagedAreaName, analysisunit) %>%
  summarize(ParameterName="Median percent cover (from BB scores)",
            N_Programs=length(unique(ProgramID)),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            N_Data=length(BB_pct[!is.na(BB_pct)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(BB_pct)])),
            EarliestYear=min(Year[!is.na(BB_pct)]),
            LatestYear=max(Year[!is.na(BB_pct)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
stats_pct2 <- SAV4[ManagedAreaName %in% setdiff(unique(SAV4$ManagedAreaName), ma_halspp), ] %>%
  group_by(ManagedAreaName, analysisunit_halid) %>%
  summarize(ParameterName="Median percent cover (from BB scores)",
            N_Programs=length(unique(ProgramID)),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            N_Data=length(BB_pct[!is.na(BB_pct)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(BB_pct)])),
            EarliestYear=min(Year[!is.na(BB_pct)]),
            LatestYear=max(Year[!is.na(BB_pct)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
setDT(stats_pct2)
setnames(stats_pct2, "analysisunit_halid", "analysisunit")
stats_pct <- distinct(rbind(stats_pct, stats_pct2))
setcolorder(stats_pct, c("ManagedAreaName", "analysisunit"))
setDT(stats_pct)
stats_pct[N_Years == 0, `:=` (EarliestYear = NA, LatestYear = NA)]
stats_pct$ProgramIDs
data.table::fwrite(stats_pct, "output/SAV_BBpct_Stats.txt", sep = "|")

stats_pa <- SAV4[ManagedAreaName %in% ma_halspp, ] %>%
  group_by(ManagedAreaName, analysisunit) %>%
  summarize(ParameterName="Frequency of occurrence",
            N_Programs=length(unique(ProgramID)),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            N_Data=length(PA[!is.na(PA)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(PA)])),
            EarliestYear=min(Year[!is.na(PA)]),
            LatestYear=max(Year[!is.na(PA)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
stats_pa2 <- SAV4[ManagedAreaName %in% setdiff(unique(SAV4$ManagedAreaName), ma_halspp), ] %>%
  group_by(ManagedAreaName, analysisunit_halid) %>%
  summarize(ParameterName="Frequency of occurrence",
            N_Programs=length(unique(ProgramID)),
            Programs=paste(sort(unique(ProgramName), decreasing=FALSE),
                           collapse=', '),
            ProgramIDs=paste(sort(unique(ProgramID), decreasing=FALSE),
                             collapse=', '),
            N_Data=length(PA[!is.na(PA)]),
            N_Years=length(unique(Year[!is.na(Year) & !is.na(PA)])),
            EarliestYear=min(Year[!is.na(PA)]),
            LatestYear=max(Year[!is.na(PA)]),
            SufficientData=ifelse(N_Data>0 & N_Years>=5, TRUE, FALSE))
setDT(stats_pa2)
setnames(stats_pa2, "analysisunit_halid", "analysisunit")
stats_pa <- distinct(rbind(stats_pa, stats_pa2))
setcolorder(stats_pa, c("ManagedAreaName", "analysisunit"))
setDT(stats_pa)
stats_pa[N_Years == 0, `:=` (EarliestYear = NA, LatestYear = NA)]
# fwrite(stats_pa, here::here("output/data/SAV_PA_Stats.txt"), sep = "|", sep2 = ",")

# stats2 <- rbind(stats_pct, stats_pa)
# fwrite(stats2, here::here(paste0("output/data/SAV_BBpct_PA_Stats", Sys.Date(), ".txt")), sep = "|")
statpardat <- list("BB_pct" = stats_pct, "PA" = stats_pa)
openxlsx::write.xlsx(statpardat, here::here("output/SAV_BBpct_PA_Stats.xlsx"), colNames = c(TRUE, TRUE), colWidths = c("auto", "auto"), firstRow = c(TRUE, TRUE))
openxlsx::write.xlsx(statpardat, here::here(paste0("output/SAV_BBpct_PA_Stats_", Sys.Date(), ".xlsx")), colNames = c(TRUE, TRUE), colWidths = c("auto", "auto"), firstRow = c(TRUE, TRUE))

# #subset to run only part of the script------------------------------------------------------
# parameters <- parameters[column == "PA", ]

#Save session info-----------------------------------------------------
session <- sessionInfo()
saveRDS(session, here::here(paste0("output/SessionInfo_", Sys.Date())))

#start script----------------------------------------------------------------------
tic()
n <- 0
seed <- 352
set.seed(seed)
for(p in parameters$column){
  
  cat(paste0("\nStarting indicator: ", p, "\n"))
  
  #List managed areas with at least 5 years of data
  nyears <- SAV4[!is.na(eval(p)) & !is.na(analysisunit), ] %>% group_by(ManagedAreaName, analysisunit) %>% summarize(type = paste0(p), nyr = length(unique(Year)))
  nyears2 <- SAV4[!is.na(eval(p)) & !is.na(analysisunit_halid), ] %>% group_by(ManagedAreaName, analysisunit_halid) %>% summarize(type = paste0(p), nyr = length(unique(Year)))
  setDT(nyears2)
  setnames(nyears2, "analysisunit_halid", "analysisunit")
  nyears <- distinct(rbind(nyears, nyears2))
  ma_include <- unique(subset(nyears, nyears$nyr >= 5)$ManagedAreaName)
  
  #Subset ma_include to first 5 entries
  # ma_include <- "Big Bend Seagrasses"
  ma_include <- ma_include[c(1,2,3,4,5)]
  
  #For each managed area, make sure there are multiple levels of BB scores per species; remove ones that don't from further consideration.
  for(i in ma_include){
    
    cat(paste0("\nStarting MA: ", i, "\n"))
    
    if("none" %in% Analyses){
      if(p == parameters$column[length(parameters$column)] & i == ma_include[length(ma_include)]){
        toc()
      }
      next
    } 
    
    if(i %in% ma_halspp){
      species <- subset(nyears, nyears$ManagedAreaName == i & nyears$nyr >= 5 & analysisunit %in% c("Attached algae", "Drift algae", "Halophila spp.", "Manatee grass", 
                                                                                                    "Shoal grass", "Total seagrass", "Total SAV", "Turtle grass", 
                                                                                                    "Widgeon grass"))$analysisunit
    } else{
      species <- subset(nyears, nyears$ManagedAreaName == i & nyears$nyr >= 5 & analysisunit %in% c("Attached algae", "Drift algae", "Unidentified Halophila", 
                                                                                                    "Johnson's seagrass", "Manatee grass", "Paddle grass", 
                                                                                                    "Shoal grass", "Star grass", "Total seagrass", "Total SAV", 
                                                                                                    "Turtle grass", "Widgeon grass"))$analysisunit
    }
    
    models <- c()
    
    #Create data.tables to hold model results for managed area i----------------------------------------------------
    lmemodresults <- data.table(managed_area = character(),
                                species = character(),
                                filename = character(),
                                effect = character(),
                                group = character(),
                                term = character(),
                                estimate = numeric(),
                                std.error = numeric(),
                                df = numeric(),
                                statistic = numeric(),
                                p.value = numeric())
    
    
    #In case model doesn't converge on the first try, attempt each model up to 5 times before moving on
    for(j in species){
      
      cat(paste0("\n  Starting species: ", j, "\n"))
      
      if(paste0(p) %in% c("BB_pct", "PC") & ("BB_pct" %in% Analyses | "PC" %in% Analyses)){
        
        formula_j <- as.formula(paste0(p, " ~ relyear"))
        
        set.seed(seed + n)
        if(j %in% setdiff(unique(SAV4$analysisunit_halid), unique(SAV4$analysisunit))){
          model_j <- try(lme(formula_j,
                             random = list(SiteIdentifier = ~relyear),
                             control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                             na.action = na.omit, 
                             data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit_halid == j, ]), 
                         silent = TRUE)
          n <- n + 1
          x <- 0
          
          while(class(model_j) == "try-error" & x < 5){
            if(x %% 25 == 0) print(paste0("    Model failed, starting attempt ", x, " of 5"))
            
            set.seed(seed + n)
            model_j <- try(lme(formula_j,
                               random = list(SiteIdentifier = ~relyear),
                               control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                               na.action = na.omit, 
                               data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit_halid == j, ]),
                           silent = TRUE)
            n <- n + 1
            x <- x + 1
          }
        } else{
          model_j <- try(lme(formula_j,
                             random = list(SiteIdentifier = ~relyear),
                             control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                             na.action = na.omit, 
                             data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == j, ]), 
                         silent = TRUE)
          n <- n + 1
          x <- 0
          
          while(class(model_j) == "try-error" & x < 5){
            if(x %% 25 == 0) print(paste0("    Model failed, starting attempt ", x, " of 5"))
            
            set.seed(seed + n)
            model_j <- try(lme(formula_j,
                               random = list(SiteIdentifier = ~relyear),
                               control = list(msMaxIter = 1000, msMaxEval = 1000, sing.tol=1e-20),
                               na.action = na.omit, 
                               data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & analysisunit == j, ]),
                           silent = TRUE)
            n <- n + 1
            x <- x + 1
          }
        }
        
        
        #Individual model objects are needed for plotting all species together
        eval(call("<-", as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.', '\\U\\1', i, perl = TRUE), 
                                       "_", 
                                       gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE))), 
                  model_j))
        
        #Save the model object as .rds
        saveRDS(model_j, here::here(paste0("output/models/SAV_", parameters[column == p, type], "_", 
                                           gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                           ifelse(stringr::str_detect(i, "NERR"), "ERR_lme_", 
                                                  ifelse(stringr::str_detect(i, "NMS"), "MS_lme_", "AP_lme_")), 
                                           gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE), 
                                           ".rds")))
        
        print(paste0("  Model object saved: ", 
                     gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                     "_", 
                     gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE)))
        
        #record lme model results------------------------------------------------------
        if(class(try(eval(as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_", gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE)))), silent = TRUE)) != "try-error"){
          models <- append(models, as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_", gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE))))
          modj_i <- setDT(broom.mixed::tidy(eval(as.name(paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "_", gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE))))))
          modj_i[, `:=` (managed_area = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                               ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
                         species = j,
                         filename = paste0("SAV_", parameters[column == p, type], "_", gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                           ifelse(stringr::str_detect(i, "NERR"), "ERR_lme_", 
                                                  ifelse(stringr::str_detect(i, "NMS"), "MS_lme_", "AP_lme_")), 
                                           gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE), ".rds"))]
          lmemodresults <- rbind(lmemodresults, modj_i)
          
        } else{
          failedmod <- data.table(model = paste0("SAV_", parameters[column == p, type], "_",
                                                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE),
                                                 ifelse(stringr::str_detect(i, "NERR"), "ERR_lme_", 
                                                        ifelse(stringr::str_detect(i, "NMS"), "MS_lme_", "AP_lme_")), 
                                                 gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE), ".rds"),
                                  error = model_j[1])
          
          failedmods <- rbind(failedmods, failedmod)
          
          modj_i <- data.table(managed_area = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                                     ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
                               species = j,
                               filename = paste0("SAV_", parameters[column == p, type], "_", gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                 ifelse(stringr::str_detect(i, "NERR"), "ERR_lme_", 
                                                        ifelse(stringr::str_detect(i, "NMS"), "MS_lme_", "AP_lme_")), 
                                                 gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE), ".rds"),
                               effect = NA,
                               group = NA,
                               term = NA,
                               estimate = NA,
                               std.error = NA,
                               df = NA,
                               statistic = NA,
                               p.value = NA)
          lmemodresults <- rbind(lmemodresults, modj_i)
        }
      }

    }
    
    #Final results tables and plots--------------------------------------------------------------------
    if(paste0(p) %in% c("BB_pct", "PC") & ("BB_pct" %in% Analyses | "PC" %in% Analyses)){
      #Summarize # points per category
      # if(TRUE %in% str_detect(models, "_HaSpIn|_JoSe|_PaGr|_StGr")){
      if(i %in% ma_halspp){
        plotdat <- SAV4[ManagedAreaName == i & !is.na(eval(p)), ] %>% group_by(analysisunit, Year, relyear, eval(p)) %>% summarise(npt = n())
      } else{
        plotdat <- SAV4[ManagedAreaName == i & !is.na(eval(p)), ] %>% group_by(analysisunit_halid, Year, relyear, eval(p)) %>% summarise(npt = n())
      }
      setDT(plotdat)
      setnames(plotdat, "eval(p)", "data")
      
      #split modeled vs unmodeled data
      modeledsp <- c()
      for(u in seq_along(models)){
        if(usenames == "common"){
          name_u <- fcase(str_detect(paste0(models[[u]]), "_ShGr"), "Shoal grass",
                          str_detect(paste0(models[[u]]), "_TuGr"), "Turtle grass",
                          str_detect(paste0(models[[u]]), "_MaGr"), "Manatee grass",
                          str_detect(paste0(models[[u]]), "_WiGr"), "Widgeon grass",
                          str_detect(paste0(models[[u]]), "_PaGr"), "Paddle grass",
                          str_detect(paste0(models[[u]]), "_StGr"), "Star grass",
                          str_detect(paste0(models[[u]]), "_JoSe"), "Johnson's seagrass",
                          str_detect(paste0(models[[u]]), "_UnHa"), "Unidentified Halophila",
                          str_detect(paste0(models[[u]]), "_HaSp"), "Halophila spp.",
                          str_detect(paste0(models[[u]]), "_ToSe"), "Total seagrass",
                          str_detect(paste0(models[[u]]), "_AtAl"), "Attached algae",
                          str_detect(paste0(models[[u]]), "_DrAl"), "Drift algae",
                          str_detect(paste0(models[[u]]), "_To"), "Total SAV")
          modeledsp <- append(modeledsp, name_u)
          
        } else{
          name_u <- fcase(str_detect(models[[u]], "_ThTe"), "Thalassia testudinum",
                          str_detect(models[[u]], "_SyFi"), "Syringodium filiforme",
                          str_detect(models[[u]], "_HaWr"), "Halodule wrightii",
                          str_detect(models[[u]], "_RuMa"), "Ruppia maritima",
                          str_detect(models[[u]], "_HaDe"), "Halophila decipiens",
                          str_detect(models[[u]], "_HaEn"), "Halophila engelmannii",
                          str_detect(models[[u]], "_HaJo"), "Halophila johnsonii",
                          str_detect(models[[u]], "_UnHa"), "Unidentified Halophila",
                          str_detect(models[[u]], "_HaSp"), "Halophila spp.",
                          str_detect(models[[u]], "_ToSe"), "Total seagrass",
                          str_detect(models[[u]], "_AtAl"), "Attached algae",
                          str_detect(models[[u]], "_DrAl"), "Drift algae",
                          str_detect(models[[u]], "_To"), "Total SAV")
          modeledsp <- append(modeledsp, name_u)
          
        }
      }
      
      miny <- c()
      for(v in seq_along(models)){
        miny_v <- try(predict(eval(models[[v]]), level = 0), silent = TRUE)
        if(class(miny_v) == "try-error") next
        miny <- append(miny, min(miny_v))
      }
      miny <- ifelse(floor(min(miny)) < 0, floor(min(miny)), 0)
      
      # Scale x-axis data
      breaks_seq <- seq(from = min(plotdat$relyear),
                        to = max(plotdat$relyear),
                        by = 3)
      labels_seq <- seq(from = min(plotdat$Year),
                        to = max(plotdat$Year),
                        by = 3)
      
      #create base plot of seagrass percent cover data over time for managed area i
      plot_i <- ggplot(data = droplevels(plotdat),
                       aes(x = relyear, y = data)) +
        # geom_point(shape = 21, alpha=0.9, color="grey50") +
        labs(title = parameters[column == p, name], subtitle = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"),
                                                                      ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
             x = "Year",
             y = parameters[column == p, name]) +
        plot_theme +
        ylim(miny, 100) +
        scale_fill_continuous_sequential(palette = "YlGnBu") +
        scale_x_continuous(breaks = breaks_seq, labels = labels_seq) +
        scale_colour_manual(values=spcols)
      
      if(length(models) > 0){
        #make sure that no failed models slipped through
        classes <- lapply(models, function(x) class(eval(x)))
        models <- models[classes != "try-error"]
        
        plot_i <- addfits(models, plot_i, p)
      }
      
      #Save the plot object as .rds
      saveRDS(plot_i, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                        ifelse(stringr::str_detect(i, "NERR"), 
                                               paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NERR_trendplot.rds"),
                                               ifelse(stringr::str_detect(i, "NMS"), 
                                                      paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NMS_trendplot.rds"),
                                                      paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "AP_trendplot.rds"))))))
      
      #Save the results table objects as .rds
      saveRDS(lmemodresults, here::here(paste0("output/tables/SAV_", parameters[column == p, type], "_", 
                                               ifelse(stringr::str_detect(i, "NERR"), 
                                                      paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NERR_lmeresults.rds"), 
                                                      ifelse(stringr::str_detect(i, "NMS"),
                                                             paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NMS_lmeresults.rds"),
                                                             paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "AP_lmeresults.rds"))))))
    }
    
    if(paste0(p) == "PA" & "PA" %in% Analyses){
      #Bar chart of proportions by analysisunit
      breaks <- c(seq(min(SAV4[ManagedAreaName == i & !is.na(PA), relyear]),
                      max(SAV4[ManagedAreaName == i & !is.na(PA), relyear]),
                      by = 2))
      yrlist <- sort(unique(SAV4$Year))
      
      labels <- c()
      for(b in breaks){
        labels <- append(labels, yrlist[b + 1])
      }
      
      if(i %in% ma_halspp){
        bpdat <- props[ManagedAreaName == i & !is.na(analysisunit) & str_detect(analysisunit, "decipiens|engelmannii|johnsonii|Unidentified|Star|Paddle|Johnson", negate = TRUE), ]
        
        barplot_sp <- ggplot(data = bpdat, aes(x = relyear, y = sp_pct, fill = analysisunit)) +
          geom_col(color = "grey20") +
          scale_x_continuous(breaks = breaks, labels = labels) +
          plot_theme +
          labs(title = parameters[column == p, name], subtitle = paste0(ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                                                               ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve")))), 
               fill = "Species", 
               x = "Year", 
               y = "Occurrence frequency (%)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(bpdat$analysisunit)),
                             labels = subset(names(spcols) ,names(spcols) %in% unique(bpdat$analysisunit)), 
                             aesthetics = c("color", "fill"))
        
      } else{
        bpdat <- props[ManagedAreaName == i & !is.na(analysisunit) & analysisunit != "Halophila spp.", ]
        
        barplot_sp <- ggplot(data = bpdat, aes(x = relyear, y = sp_pct, fill = analysisunit)) +
          geom_col(color = "grey20") +
          scale_x_continuous(breaks = breaks, labels = labels) +
          plot_theme +
          labs(title = parameters[column == p, name], subtitle = paste0(ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                                                                               ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve")))), 
               fill = "Species", 
               x = "Year", 
               y = "Occurrence frequency (%)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(bpdat$analysisunit)),
                             labels = str_replace(subset(names(spcols) ,names(spcols) %in% unique(bpdat$analysisunit)), "Unidentified Halophila", "Halophila, unk."),
                             aesthetics = c("color", "fill"))
        
      }
      
      saveRDS(barplot_sp, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                            gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                            ifelse(stringr::str_detect(i, "NERR"), "ERR_barplot_sp", 
                                                   ifelse(stringr::str_detect(i, "NMS"), "MS_barplot_sp", "AP_barplot_sp")), 
                                            ".rds")))
      
      # saveRDS(belrmodresults, here::here(paste0("output/tables/SAV_", parameters[column == p, type], "_", 
      #                                           ifelse(stringr::str_detect(i, "NERR"), 
      #                                                  paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NERR_belrresults.rds"),
      #                                                  ifelse(stringr::str_detect(i, "NMS"),
      #                                                         paste0(str_sub(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 1, -2), "NMS_belrresults.rds"),
      #                                                         paste0(gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), "AP_belrresults.rds"))))))
    }
    
    print(paste0("  Plot objects and results tables saved: ", 
                 gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                 "_", 
                 gsub('\\b(\\p{Lu}\\p{Ll})|.','\\1', str_to_title(j), perl = TRUE)))
  }
}

#Save failedmodslist-----------------------------------------------------
saveRDS(failedmods, here::here("output/models/failedmodslist.rds"))

#Get rid of eval(p)'s from plot file mappings---------------------------------------
files <- list.files(here::here("output/Figures/BB/")) #get file list
files <- str_subset(files, ".rds") #exclude non-.RDS files

filesupdated <- list()
for(f in seq_along(files)){
  file_f <- readRDS(here::here(paste0("output/Figures/BB/", files[f])))
  if(paste0(as_label(file_f$mapping$y)) == "eval(p)"){
    file_f$mapping$y <- parameters[name %in% file_f$labels$y, column][[1]]
    saveRDS(file_f, here::here(paste0("output/Figures/BB/", files[f])))
    rm(file_f)
    filesupdated <- append(filesupdated, files[f])
  } else {
    rm(file_f)
  }
  if(round((f/length(files))*100, 1) %% 10 == 0){
    print(paste0(round((f/length(files))*100), "% done!"))
  }
}


#Save .png versions of specified "trendplot" .rds files --------------------------------------------------
files <- list.files(here::here("output/Figures/BB/")) #get file list
plots <- stringr::str_subset(files, "_trendplot") #identify map file
mods <- list.files(here::here("output/models/"))
#models2 <- str_subset(mods, paste0(str_sub(plots[1], 1, str_locate_all(plots[1], "_")[[1]][2])))
models2 <- str_subset(mods, "_lme")

malist <- c()
for(pl in plots){
  ma_p <- str_sub(pl, str_locate_all(pl, "_")[[1]][2], str_locate_all(pl, "_")[[1]][3])
  malist <- append(malist, ma_p)
}

failedmodslist <- readRDS(here::here("output/models/failedmodslist.rds"))

for(m in malist){
  mods_m <- str_subset(models2, m)
  mods_m <- setdiff(mods_m, failedmodslist$model)
  mods_m <- setdiff(mods_m, subset(mods_m, str_detect(mods_m, "_PC_")))
  params <- unique(str_sub(mods_m, 1, str_locate_all(mods_m, "_")[[1]][2] - 1))
  
  for(param in params){
    mods_m_p <- str_subset(mods_m, param)
    plot_m <- try(str_subset(plots, unique(str_sub(mods_m_p, 1, str_locate_all(mods_m_p, "_")[[1]][3] - 1))), silent = TRUE)
    
    if(class(plot_m) != "try-error"){
      for(w in mods_m_p){
        aucol <- ifelse(str_sub(w, str_locate_all(w, "_")[[1]][2],
                                ifelse(str_detect(w, "AP_"), str_locate(w, "AP_")[2],
                                       ifelse(str_detect(w, "NERR_"), 
                                              str_locate(w, "NERR_")[2], 
                                              str_locate(w, "NMS_")[2]))) %in% c("_BRAP_", "_IRMVBAP_", "_IRVBPAP_", "_JBJIAP_", "_LRLWCAP_", 
                                                                                 "_MLAP_", "_BBAP_", "_FKNMS_"), as.name("analysisunit"), as.name("analysisunit_halid"))
        
        eval(call("<-", as.name(paste0(str_sub(w, str_locate_all(w, "_")[[1]][2] + 1, 
                                               ifelse(str_detect(w, "AP_"), str_locate(w, "AP_") - 1, 
                                                      ifelse(str_detect(w, "NERR_"), str_locate(w, "NERR_"), str_locate(w, "NMS_")))), 
                                       "_", 
                                       str_sub(w, -8, -5))), 
                  readRDS(here::here(paste0("output/models/", w)))))
      }
      
      plot_m2 <- readRDS(here::here(paste0("output/Figures/BB/", plot_m)))
      plot_m2 <- plot_m2 +
        plot_theme
      
      wid <- ifelse(length(unique(plot_m2$data$analysisunit)) == 1, 4,
                    ifelse(length(unique(plot_m2$data$analysisunit)) == 2, 6, 8))
      hei <- ifelse(length(unique(plot_m2$data$analysisunit)) <= 3, 3,
                    ifelse(length(unique(plot_m2$data$analysisunit)) <= 6, 6, 9))
      
      png(here::here(paste0("output/website/images/trendplots/", str_sub(plot_m, 1, -5), ".png")),
          width = wid,
          height = hei,
          units = "in",
          res = 300)
      # jpeg(here::here(paste0("output/Figures/BB/img/", str_sub(plot_m, 1, -5), ".jpg")),
      #      width = 10,
      #      height = 6,
      #      units = "in",
      #      res = 300)
      print(plot_m2)
      dev.off()
      
    } else{
      next
    }
  }
}

toc()