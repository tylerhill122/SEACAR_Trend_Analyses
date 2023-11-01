library(mgcv)
library(tidymv)

##############################
### SAV PLOTTING FUNCTIONS ###
##############################

#Managed areas that should have Halophila species combined:
ma_halspp <- c("Banana River Aquatic Preserve", "Indian River-Malabar to Vero Beach Aquatic Preserve", 
               "Indian River-Vero Beach to Ft. Pierce Aquatic Preserve", "Jensen Beach to Jupiter Inlet Aquatic Preserve",
               "Loxahatchee River-Lake Worth Creek Aquatic Preserve", "Mosquito Lagoon Aquatic Preserve", 
               "Biscayne Bay Aquatic Preserve", "Florida Keys National Marine Sanctuary")

files <- list.files(here::here("output/Figures/BB/")) #get file list
trendplots <- stringr::str_subset(files, "_trendplot") #identify map file
trendplots <- stringr::str_subset(trendplots, "_BBpct_")

mods <- list.files(here::here("output/models/"))
models2 <- str_subset(mods, paste0(str_sub(trendplots[1], 1, str_locate_all(trendplots[1], "_")[[1]][2])))

malist <- c()
for(pl in trendplots){
  ma_p <- str_split(pl, "_")[[1]][3]
  malist <- append(malist, ma_p)
}

failedmodslist <- readRDS(here::here("output/models/failedmodslist.rds"))

find_exact_matches <- function(pattern, filenames) {
  regex <- paste0("(_|^)", pattern, "(_|$)")
  matched_files <- str_subset(filenames, regex)
  return(matched_files)
}

plot_sav_trendplot <- function(ma_abrev){
  if(ma_abrev %in% malist){
    plot_file <- lapply(ma_abrev, find_exact_matches, filenames = trendplots)
    plot <- readRDS(here::here(paste0("output/Figures/BB/", plot_file)))
    print(plot)
  }
}

barplots <- stringr::str_subset(files, "_barplot") #identify map file

malist2 <- c()
for(pl in barplots){
  ma_p <- str_split(pl, "_")[[1]][3]
  malist2 <- append(malist2, ma_p)
}


plot_sav_barplot <- function(ma_abrev){
  if(ma_abrev %in% malist2){
    plot_file <- lapply(ma_abrev, find_exact_matches, filenames = barplots)
    plot <- readRDS(here::here(paste0("output/Figures/BB/", plot_file)))
    print(plot)
  }
}

sav_managed_areas <- unique(c(malist, malist2))

sp_to_skip <- c("Drift algae", "Total seagrass", "Attached algae", "Total SAV")

ggplot_gam <- function(ma, hal = "all") {
  
  data <- SAV4 %>% filter(ManagedAreaName==ma_short)
  
  if (hal == "combined"){
    species <- unique(data$analysisunit)
    au_col <- "analysisunit"
  } else if(hal == "only"){
    species <- str_subset(unique(data$analysisunit_halid), "Halophila")
    au_col <- "analysisunit_halid"
  } else if(hal == "none"){
    species <- str_subset(unique(data$analysisunit_halid), "Halophila", negate = TRUE)
    au_col <- "analysisunit_halid"
  } else {
    if(ma %in% ma_halspp){
      species <- unique(data$analysisunit)
      au_col <- "analysisunit"
    } else {
      species <- unique(data$analysisunit_halid)
      au_col <- "analysisunit_halid"
    }
  }
  
  min_years <- data %>% 
    group_by(!!sym(au_col)) %>% 
    summarise(n = n_distinct(Year)) %>% pull(n) %>% min()
  
  # k_value <- ifelse(min_years > 2, min_years - 1, 2)
  # print(k_value)
  k_value <- 3
  
  model_list <- list()
  
  for (i in 1:length(species)){
    s <- species[i]
    
    if (s %in% sp_to_skip){
      next
    } else {
      species_data <- data %>% filter(!!sym(au_col) == s, !is.na(BB_pct))
      # at least 10 years of data per species
      if (length(unique(species_data$Year)) >= 10){
        model_list[[s]] <- gam(BB_pct ~ s(relyear, k=k_value, fx = TRUE), data = species_data)
      }
    }
  }
  
  new_data <- expand.grid(relyear = seq(min(data$relyear), max(data$relyear), by = 1),
                          species = species)
  # model predict function
  get_predictions <- function(models, newdata) {
    preds <- lapply(names(models), function(sp) {
      pred_data <- newdata %>% filter(species == sp)
      pred <- predict(models[[sp]], newdata=pred_data, type="link", se.fit=TRUE)
      data.frame(relyear=pred_data$relyear, species=sp, fit=pred$fit, lwr=pred$fit-1.96*pred$se.fit, upr=pred$fit+1.96*pred$se.fit)
    })
    
    bind_rows(preds)
  }
  
  predictions <- get_predictions(model_list, new_data)
  
  color_palette <- scale_color_manual(values = rainbow(length(unique(predictions$species))))
  
  # Scale x-axis data
  year_list <- data %>%
    filter(relyear %in% unique(predictions$relyear)) %>%
    group_by(relyear) %>%
    summarise(Year = list(unique(Year))) %>%
    unnest(Year)
  
  breaks_seq <- seq(from = min(year_list$relyear),
                    to = max(year_list$relyear),
                    by = 3)
  labels_seq <- seq(from = min(year_list$Year),
                    to = max(year_list$Year),
                    by = 3)
  
  plot <- ggplot(predictions, aes(x = relyear, y = fit, color = species)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = species), alpha = 0.2) + 
    # geom_line() +
    labs(title = paste0("BB_pct over Years for Seagrass Species in ", ma),
         y = "Braun-Blanquet Percentage",
         x = "Year") +
    color_palette +
    scale_fill_manual(values = rainbow(length(unique(predictions$species)))) +
    scale_x_continuous(breaks = breaks_seq, labels = labels_seq) +
    plot_theme
  
  print(plot)
}