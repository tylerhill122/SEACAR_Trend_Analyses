library(mgcv)

##############################
### SAV PLOTTING FUNCTIONS ###
##############################

#Managed areas that should have Halophila species combined:
ma_halspp <- c("Banana River", "Indian River-Malabar to Vero Beach", "Indian River-Vero Beach to Ft. Pierce", "Jensen Beach to Jupiter Inlet",
               "Loxahatchee River-Lake Worth Creek", "Mosquito Lagoon", "Biscayne Bay", "Florida Keys NMS")

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

# GAM prediction function
get_gam_preds <- function(model, new_data){
  pred <- predict(model, newdata=new_data, type="link", se.fit=TRUE)
  upper_bound <- pred$fit + (1.96 * pred$se.fit)
  lower_bound <- pred$fit - (1.96 * pred$se.fit)
  return(list(fit=pred$fit, upper=upper_bound, lower=lower_bound))
}

# Plot GAM function
plot_sav_gam <- function(ma_short){
  
  ma_data <- SAV4 %>% filter(ManagedAreaName==ma_short)
  
  if(i %in% ma_halspp){
    species <- unique(ma_data$analysisunit)
    au_col <- "analysisunit"
    } else {
    species <- unique(ma_data$analysisunit_halid)
    au_col <- "analysisunit_halid"
  }
  
  gam_models <- list()
  
  for (s in species){
    ma_data_species <- ma_data %>% filter(!!sym(au_col) == s)
    gam_model <- gam(BB_pct ~ s(relyear), data = ma_data_species, family = gaussian())
    gam_models[[s]] <- gam_model
  }
  
  xlims <- range(ma_data$relyear, na.rm = TRUE)
  ylims <- range(ma_data$BB_pct, na.rm = TRUE)
  
  plot(NULL, xlim=xlims, ylim=ylims, xlab = "Year", ylab = "BB_pct", type = "n")
  colors <- rainbow(length(species))
  
  sp_to_skip <- c("Drift algae", "Total seagrass", "Attached algae")
  inc_sp <- list()
  
  for (i in 1:length(species)){
    sp <- species[i]
    time_range <- seq(min(ma_data$relyear, na.rm = TRUE), max(ma_data$relyear, na.rm = TRUE), length.out = 100)
    
    if(sp %in% sp_to_skip) {
      next
    } else {
      preds <- get_gam_preds(gam_models[[sp]], data.frame(relyear = time_range))
      
      lines(time_range, preds$fit, col=colors[i], lty=i)
      
      polygon(c(time_range, rev(time_range)), c(preds$lower, rev(preds$upper)), col=alpha(colors[i], 0.2), border=NA)
      
      inc_sp <- c(inc_sp, sp)
    }
  }
  
  legend("topright", legend=inc_sp, col=colors, lty=1:length(inc_sp))
}
