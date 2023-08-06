#Starting over converting all percent cover metrics to BB
library(tidyverse)
library(data.table)
library(scales)
library(brms)
library(broom.mixed)
library(tidybayes)
library(bayesplot)
library(sf)
library(gtable)
library(grid)
library(gridExtra)
library(tictoc)
library(here)
library(colorspace)
library(nlme)

# data directory and loading SAV4 output from previous script
data_dir <- here::here("output")
file_in <- list.files(data_dir, pattern="SAV_DataUsed.txt", full=TRUE)

SAV4 <- fread(file_in, sep = "|", header = TRUE, stringsAsFactors = FALSE,
             na.strings=c("NULL","","NA"))

#Create a table of the proportion of present SAV types by managed area and year

props_halid <- SAV4 %>% filter(str_detect(analysisunit_halid, "Total|Drift|spp\\.", negate = TRUE), !is.na(PA)) %>% group_by(ManagedAreaName, analysisunit_halid, relyear) %>% summarize(n_P = sum(PA), ntot_PA = n(), prop_P = n_P/ntot_PA)
props <- SAV4 %>% filter(str_detect(analysisunit, "Total|Drift|decipiens|engelmannii|johnsonii|Unidentified", negate = TRUE), !is.na(PA)) %>% group_by(ManagedAreaName, analysisunit, relyear) %>% summarize(n_P = sum(PA), ntot_PA = n(), prop_P = n_P/ntot_PA)
setDT(props_halid)
setDT(props)

for(m in unique(props_halid$ManagedAreaName)){
  props_halid[ManagedAreaName == m, `:=` (n_allsp_P = sum(n_P), sp_prop = n_P/sum(n_P), sp_pct = (n_P/sum(n_P)) * 100), by = c("relyear")]
}
for(m in unique(props$ManagedAreaName)){
  props[ManagedAreaName == m, `:=` (n_allsp_P = sum(n_P), sp_prop = n_P/sum(n_P), sp_pct = (n_P/sum(n_P)) * 100), by = c("relyear")]
}

setnames(props_halid, "analysisunit_halid", "analysisunit")
props2 <- distinct(rbind(props_halid, props))
setorder(props2, ManagedAreaName, relyear, analysisunit)
props <- props2

# species color palette
spcollist <- c("#005396",
               "#0088B1",
               "#00ADAE",
               "#65CCB3",
               "#AEE4C1",
               "#FDEBA8",
               "#F8CD6D",
               "#F5A800",
               "#F17B00")

EDA <- "plots"

Analyses <- c("BB_pct", "PA")

#Empty data.table to house names of any failed models generated below.
failedmods <- data.table(model = character(),
                         error = character())

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

# Use common names for now
usenames <- "common"
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
}

prcollist_a <- sequential_hcl(length(unique(SAV4$ProgramName)), palette = "YlOrRd")
prcollist_b <- sequential_hcl(length(unique(SAV4$ProgramName)), palette = "YlGnBu", rev = TRUE)
prcollist <- append(prcollist_a[which(seq(1, length(prcollist_a)) %% 2 == 0)], 
                    prcollist_b[which(seq(1, length(prcollist_b)) %% 2 != 0)])
prcollist <- rev(prcollist)
set.seed(4691)
progs <- sample(sort(unique(SAV4$ProgramName)))
prcols <- setNames(prcollist, progs)

#### addfits function
addfits_blacktrendlines <- function(models, plot_i, param){
  aucol <- as.name(names(plot_i$data)[1])
  ifelse(length(models) == 1,
         return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                   aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
         ifelse(length(models) == 2,
                return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                          aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                         geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                   aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                ifelse(length(models) == 3,
                       return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                 aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                          aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                          aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                       ifelse(length(models) == 4,
                              return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                        aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                       geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                 aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                       geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                 aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                       geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                 aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                              ifelse(length(models) == 5,
                                     return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                               aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                        aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                        aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                        aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                              geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                        aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                                     ifelse(length(models) == 6,
                                            return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                                      aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                               aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                               aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                               aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                               aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                     geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[6]], "$data$", aucol, ")"))), ],
                                                               aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                                            ifelse(length(models) == 7,
                                                   return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                                      aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                                      aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                                      aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                                      aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[6]], "$data$", aucol, ")"))), ],
                                                                      aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                            geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[7]], "$data$", aucol, ")"))), ],
                                                                      aes(x = relyear, y = predict(eval(models[[7]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                                                   ifelse(length(models) == 8,
                                                          return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[6]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[7]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[7]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                   geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[8]], "$data$", aucol, ")"))), ],
                                                                             aes(x = relyear, y = predict(eval(models[[8]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                                                          ifelse(length(models) == 9,
                                                                 return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[6]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[7]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[7]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[8]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[8]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                          geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[9]], "$data$", aucol, ")"))), ],
                                                                                    aes(x = relyear, y = predict(eval(models[[9]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                                                                 ifelse(length(models) == 10,
                                                                        return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[6]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[7]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[7]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[8]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[8]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[9]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[9]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                 geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[10]], "$data$", aucol, ")"))), ],
                                                                                           aes(x = relyear, y = predict(eval(models[[10]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                                                                        ifelse(length(models) == 11,
                                                                               return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                                                                         aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[6]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[7]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[7]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[8]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[8]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[9]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[9]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[10]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[10]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[11]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[11]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)),
                                                                               
                                                                               return(plot_i + geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[1]], "$data$", aucol, ")"))), ],
                                                                                                         aes(x = relyear, y = predict(eval(models[[1]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[2]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[2]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[3]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[3]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[4]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[4]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[5]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[5]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[6]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[6]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[7]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[7]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[8]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[8]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[9]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[9]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[10]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[10]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[11]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[11]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE) +
                                                                                        geom_line(data = SAV4[ManagedAreaName == i & !is.na(eval(p)) & eval(aucol) == eval(parse(text = paste0("unique(", models[[12]], "$data$", aucol, ")"))), ],
                                                                                                  aes(x = relyear, y = predict(eval(models[[12]]), level = 0)), color="#000099", size=0.75, alpha=0.7, inherit.aes = FALSE)))))))))))))
}


# Parameters column for iterating through each parameter
# parameters <- data.table(column = c(as.name("BB_all"), as.name("BB_pct"), as.name("PC"), as.name("PO"), as.name("PA")),
#                          name = c("Braun Blanquet score", "Median percent cover", "Visual percent cover", "Percent occurrence", "Frequency of occurrence"),
#                          type = c("BBall", "BBpct", "PC", "PO", "PA"))

### SMALLER PARAMETERS TABLE ###
################################
parameters <- data.table(
  column=c(as.name("BB_pct"), as.name("PA")),
  name = c("Median percent cover", "Frequency of occurrence"),
  type = c("BBpct","PA"))

# Setting plot theme
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

#Save session info-----------------------------------------------------
session <- sessionInfo()
saveRDS(session, here::here(paste0("output/SessionInfo_", Sys.Date())))

#### START SCRIPT ####
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
  
  #subsetting ma_include to only include first 5 locations
  # ma_include <- ma_include[c(1,2,3,4,5)]
  
  for(i in ma_include){
    
    cat(paste0("\nStarting MA: ", i, "\n"))
    
    if(str_detect(EDA, "plots")){
      parvYear_bysp <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ],
                              aes(x = Year, y = eval(p), color = analysisunit)) +
        geom_jitter() +
        theme_bw() +
        labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                            ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
             y = parameters[column == p, name],
             color = "Species") +
        scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                           aesthetics = c("color", "fill"))
      
      saveRDS(parvYear_bysp, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                               gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                               ifelse(stringr::str_detect(i, "NERR"), paste0("ERR_EDA001_", "vYear_bysp.rds"), 
                                                      ifelse(stringr::str_detect(i, "NMS"), paste0("MS_EDA001_", "vYear_bysp.rds"), paste0("AP_EDA001_", "vYear_bysp.rds"))))))
      
      parvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = eval(p), color = as.factor(ProgramID))) +
        geom_jitter() +
        theme_bw() +
        labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                            ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
             y = parameters[column == p, name],
             color = "Program ID") +
        scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                           aesthetics = c("color", "fill"))
      
      saveRDS(parvYear_bypr, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                               gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                               ifelse(stringr::str_detect(i, "NERR"), paste0("ERR_EDA002_", "vYear_bypr.rds"), 
                                                      ifelse(stringr::str_detect(i, "NMS"), paste0("MS_EDA002_", "vYear_bypr.rds"), paste0("AP_EDA002_", "vYear_bypr.rds"))))))
      
      spvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = analysisunit, color = as.factor(ProgramID))) +
        geom_jitter() +
        theme_bw() +
        labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                            ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
             y = "Species",
             color = "Program ID") +
        scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                           aesthetics = c("color", "fill"))
      
      saveRDS(spvYear_bypr, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                              gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                              ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA003_spvYear_bypr.rds", 
                                                     ifelse(stringr::str_detect(i, "NMS"), "MS_EDA003_spvYear_bypr.rds", "AP_EDA003_spvYear_bypr.rds")))))
      
      qsvYear_bysp <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = QuadSize_m2, color = analysisunit)) +
        geom_jitter() +
        theme_bw() +
        labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                            ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
             y = "Quadrat size (m^2)",
             color = "Species") +
        scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                           aesthetics = c("color", "fill"))
      
      saveRDS(qsvYear_bysp, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                              gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                              ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA004_qsvYear_bysp.rds", 
                                                     ifelse(stringr::str_detect(i, "NMS"), "MS_EDA004_qsvYear_bysp.rds", "AP_EDA004_qsvYear_bysp.rds")))))
      
      qsvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = QuadSize_m2, color = as.factor(ProgramID))) +
        geom_jitter() +
        theme_bw() +
        labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                            ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
             y = "Quadrat size (m^2)",
             color = "Program ID") +
        scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                           aesthetics = c("color", "fill"))
      
      saveRDS(qsvYear_bypr, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                              gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                              ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA005_qsvYear_bypr.rds", 
                                                     ifelse(stringr::str_detect(i, "NMS"), "MS_EDA005_qsvYear_bypr.rds", "AP_EDA005_qsvYear_bypr.rds")))))
      
      metvYear_bysp <- ggplot(data = SAV4[ManagedAreaName == i, ], aes(x = Year, y = method, color = analysisunit)) +
        geom_jitter() +
        theme_bw() +
        labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                            ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
             y = "Method",
             color = "Species") +
        scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                           aesthetics = c("color", "fill"))
      
      saveRDS(metvYear_bysp, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                               gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                               ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA006_metvYear_bysp.rds", 
                                                      ifelse(stringr::str_detect(i, "NMS"), "MS_EDA006_metvYear_bysp.rds", "AP_EDA006_metvYear_bysp.rds")))))
      
      metvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i, ], aes(x = Year, y = method, color = as.factor(ProgramID))) +
        geom_jitter() +
        theme_bw() +
        labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                            ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
             y = "Method",
             color = "Program ID") +
        scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                           aesthetics = c("color", "fill"))
      
      saveRDS(metvYear_bypr, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                               gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                               ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA007_metvYear_bypr.rds", 
                                                      ifelse(stringr::str_detect(i, "NMS"), "MS_EDA007_metvYear_bypr.rds", "AP_EDA007_metvYear_bypr.rds")))))
      
      metvqs_bysp <- ggplot(data = SAV4[ManagedAreaName == i, ], aes(x = QuadSize_m2, y = method, color = analysisunit)) +
        geom_jitter() +
        theme_bw() +
        labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                            ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
             x = "Quadrat size (m^2)",
             y = "Method",
             color = "Species") +
        scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                           aesthetics = c("color", "fill"))
      
      saveRDS(metvqs_bysp, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                             gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                             ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA008_metvqs_bysp.rds", 
                                                    ifelse(stringr::str_detect(i, "NMS"), "MS_EDA008_metvqs_bysp.rds", "AP_EDA008_metvqs_bysp.rds")))))
      
      metvqs_bypr <- ggplot(data = SAV4[ManagedAreaName == i, ], aes(x = QuadSize_m2, y = method, color = as.factor(ProgramID))) +
        geom_jitter() +
        theme_bw() +
        labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                            ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
             x = "Quadrat size (m^2)",
             y = "Method",
             color = "Program ID") +
        scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                           aesthetics = c("color", "fill"))
      
      saveRDS(metvqs_bypr, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                             gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                             ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA009_metvqs_bypr.rds", 
                                                    ifelse(stringr::str_detect(i, "NMS"), "MS_EDA009_metvqs_bypr.rds", "AP_EDA009_metvqs_bypr.rds")))))
      
      if(length(SAV4[ManagedAreaName == i & !is.na(eval(p)) & !is.na(Grid), Grid]) > 0){              
        grvYear_bysp <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = Grid, color = analysisunit)) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = "Grid number",
               color = "Species") +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(grvYear_bysp, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA010_grvYear_bysp.rds", 
                                                       ifelse(stringr::str_detect(i, "NMS"), "MS_EDA010_grvYear_bysp.rds", "AP_EDA010_grvYear_bysp.rds")))))
        
        grvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = Grid, color = as.factor(ProgramID))) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = "Grid number",
               color = "Program ID") +
          scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(grvYear_bypr, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA011_grvYear_bypr.rds", 
                                                       ifelse(stringr::str_detect(i, "NMS"), "MS__EDA011_grvYear_bypr.rds", "AP_EDA011_grvYear_bypr.rds")))))
      }
      
      if(length(SAV4[ManagedAreaName == i & !is.na(eval(p)) & !is.na(Depth_M), Depth_M]) > 0){                
        dpvYear_bysp <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = Depth_M, color = analysisunit)) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), "National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = "Depth (m)",
               color = "Species") +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(dpvYear_bysp, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA012_dpvYear_bysp.rds", 
                                                       ifelse(stringr::str_detect(i, "NMS"), "MS_EDA012_dpvYear_bysp.rds", "AP_EDA012_dpvYear_bysp.rds")))))
        
        dpvYear_bypr <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = Depth_M, color = as.factor(ProgramID))) +
          geom_jitter() +
          theme_bw() +
          labs(title = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"), 
                              ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
               y = "Depth (m)",
               color = "Program ID") +
          scale_color_manual(values = subset(prcols, names(prcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), as.factor(ProgramID)])), 
                             aesthetics = c("color", "fill"))
        
        saveRDS(dpvYear_bypr, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                                gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                                ifelse(stringr::str_detect(i, "NERR"), "ERR_EDA013_dpvYear_bypr.rds", 
                                                       ifelse(stringr::str_detect(i, "NMS"), "MS_EDA013_dpvYear_bypr.rds", "AP_EDA013_dpvYear_bypr.rds")))))
      }
      
      
      #Generate the legend
      plotall <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, fill = analysisunit)) +
        geom_bar()  +
        scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                           aesthetics = c("color", "fill")) +
        labs(y="Frequency of data", x="Year") +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title = element_text(size = 7),
              axis.text = element_text(size = 7),
              legend.text = element_text(size = 7))
      
      legend = gtable::gtable_filter(ggplotGrob(plotall), "guide-box")
      
      saveRDS(legend, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                        gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                        ifelse(stringr::str_detect(i, "NERR"), "ERR_hist_specieslegend.rds", 
                                               ifelse(stringr::str_detect(i, "NMS"), "MS_hist_specieslegend.rds", "AP_hist_specieslegend.rds")))))
      
      #Create and save the hist objects---------------------------------------------------
      for(a in setdiff(unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit]), c("Total seagrass", "Attached algae", "Drift algae"))){
        dat <- filter(SAV4[ManagedAreaName == i & !is.na(eval(p)), ], analysisunit == a)
        
        plot <- ggplot(data = dat, aes(x = Year, fill = analysisunit)) +
          geom_bar() +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill")) +
          scale_x_continuous(limits = c(min(dat$Year - 1), max(dat$Year + 1))) +
          #scale_y_continuous(limits = c(0, 2600)) +
          labs(y="Frequency of data", x="Year") +
          theme(#legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            #axis.title = element_text(size = 7),
            axis.title = element_blank(),
            axis.text = element_text(size = 7),
            #legend.text = element_text(size = 7),
            legend.position = "none")
        
        saveRDS(plot, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                        gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                        ifelse(stringr::str_detect(i, "NERR"), "ERR_hist_", 
                                               ifelse(stringr::str_detect(i, "NMS"), "MS_hist_", "AP_hist_")), 
                                        gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', a, perl = TRUE), ".rds")))
      }
      
      dat <- filter(SAV4[ManagedAreaName == i & !is.na(eval(p)), ], analysisunit %in% c("Total seagrass", "Attached algae", "Drift algae"))
      
      plot <- ggplot(data = dat, aes(x = Year, fill = analysisunit)) +
        geom_bar() +
        scale_color_manual(values = subset(spcols, names(spcols) %in% unique(dat$analysisunit)), 
                           aesthetics = c("color", "fill")) +
        scale_x_continuous(limits = c(min(dat$Year - 1), max(dat$Year + 1))) +
        #scale_y_continuous(limits = c(0, 2600)) +
        #paste0(as_label(BBAP_BB_EDAplots[[1]]$mapping$y))
        labs(y="Frequency of data", x="Year") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              #axis.title = element_text(size = 7),
              #axis.title = element_blank(),
              axis.text = element_text(size = 7),
              #legend.text = element_text(size = 7),
              legend.position = "none",
              legend.title = element_blank())
      
      saveRDS(plot, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                      gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                      ifelse(stringr::str_detect(i, "NERR"), "ERR_hist_SGvMA.rds", 
                                             ifelse(stringr::str_detect(i, "NMS"), "MS_hist_SGvMA.rds", "AP_hist_SGvMA.rds")))))  
      
      #Generate the legend
      plotall <- ggplot(data = SAV4[ManagedAreaName == i & !is.na(eval(p)), ], aes(x = Year, y = eval(p), color = analysisunit)) +
        geom_boxplot() +
        scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                           aesthetics = c("color", "fill")) +
        labs(y = parameters[column == p, name], x = "Year") +
        theme(legend.title = element_blank(),
              axis.text.x = element_text(angle = 45, hjust = 1),
              axis.title = element_text(size = 7),
              axis.text = element_text(size = 7),
              legend.text = element_text(size = 7))
      
      legend = gtable::gtable_filter(ggplotGrob(plotall), "guide-box")
      
      saveRDS(legend, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                        gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                        ifelse(stringr::str_detect(i, "NERR"), "ERR_boxplot_specieslegend.rds", 
                                               ifelse(stringr::str_detect(i, "NMS"), "MS_boxplot_specieslegend.rds", "AP_boxplot_specieslegend.rds")))))
      
      #Create and save the boxplot objects--------------------------------------------------
      for(b in setdiff(unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit]), c("Total seagrass", "Attached algae", "Drift algae"))){
        dat <- filter(SAV4[ManagedAreaName == i & !is.na(eval(p)), ], analysisunit == b)
        
        plot <- ggplot(data = dat, aes(group=Year, x = Year, y = eval(p), color = analysisunit)) +
          geom_boxplot() +
          scale_color_manual(values = subset(spcols, names(spcols) %in% unique(SAV4[ManagedAreaName == i & !is.na(eval(p)), analysisunit])), 
                             aesthetics = c("color", "fill")) +
          scale_x_continuous(limits = c(min(dat$Year - 1), max(dat$Year + 1))) +
          #scale_y_continuous(limits = c(0, 100)) +
          labs(y = parameters[column == p, name], x = "Year") +
          theme(#legend.title = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            #axis.title = element_text(size = 7),
            axis.title = element_blank(),
            axis.text = element_text(size = 7),
            #legend.text = element_text(size = 7),
            legend.position = "none")
        
        saveRDS(plot, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                        gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                        ifelse(stringr::str_detect(i, "NERR"), "ERR_boxplot_", 
                                               ifelse(stringr::str_detect(i, "NMS"), "MS_boxplot_", "AP_boxplot_")), 
                                        gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', b, perl = TRUE), ".rds")))
      }
      
      dat <- filter(SAV4[ManagedAreaName == i & !is.na(eval(p)), ], analysisunit %in% c("Total seagrass", "Attached algae", "Drift algae"))
      
      plot <- ggplot(data = dat, aes(x = as.factor(Year), y = eval(p), color = analysisunit)) +
        geom_boxplot() +
        scale_color_manual(values = subset(spcols, names(spcols) %in% unique(dat$analysisunit)), 
                           aesthetics = c("color", "fill")) +
        #scale_x_continuous(limits = c(min(dat$Year - 1), max(dat$Year + 1))) +
        #scale_y_continuous(limits = c(0, 100)) +
        labs(y = parameters[column == p, name], x = "Year") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              #axis.title = element_text(size = 7),
              #axis.title = element_blank(),
              axis.text = element_text(size = 7),
              #legend.text = element_text(size = 7),
              legend.position = "none",
              legend.title = element_blank())
      
      saveRDS(plot, here::here(paste0("output/Figures/BB/SAV_", parameters[column == p, type], "_", 
                                      gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1', i, perl = TRUE), 
                                      ifelse(stringr::str_detect(i, "NERR"), "ERR_boxplot_SGvMA.rds", 
                                             ifelse(stringr::str_detect(i, "NMS"), "MS_boxplot_SGvMA.rds", "AP_boxplot_SGvMA.rds")))))  
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
    ### Loop through Species ###
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
  }
}

###################
# splitting up for-loop to create plots after model creation to save time
###################

for(p in parameters$column){
  for(i in ma_include){
    #Final results tables and plots--------------------------------------------------------------------
    if(paste0(p) %in% c("BB_pct", "PC") & ("BB_pct" %in% Analyses | "PC" %in% Analyses)){
      #Summarize # points per category
      if(i %in% ma_halspp){
        plotdat <- SAV4[ManagedAreaName == i & !is.na(eval(p)), ] %>% group_by(analysisunit, Year, relyear, eval(p)) %>% summarise(npt = n())
      }else{
        plotdat <- SAV4[ManagedAreaName == i & !is.na(eval(p)), ] %>% group_by(analysisunit_halid, Year, relyear, eval(p)) %>% summarise(npt = n())
      }
      setDT(plotdat)
      setnames(plotdat, "eval(p)", "data")
      
      #split modeled vs unmodeled data
      modeledsp <- c()
      # model labels
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
      
      # set min y for plots
      miny <- c()
      for(v in seq_along(models)){
        miny_v <- try(predict(eval(models[[v]]), level = 0), silent = TRUE)
        if(class(miny_v) == "try-error") next
        miny <- append(miny, min(miny_v))
      }
      miny <- ifelse(floor(min(miny)) < 0, floor(min(miny)), 0)
      
      #create base plot of seagrass percent cover data over time for managed area i
      plot_i <- ggplot(data = droplevels(plotdat),
                       aes(x = relyear, y = data, fill = npt)) + #SAV4[ManagedAreaName == i & !is.na(eval(p)), ]    size = npt   fill = analysisunit,   y = eval(p)
        # geom_hline(yintercept = 0, color = "grey20") +
        geom_point(shape = 21, #fill = "grey90", #stroke = 2,
                   alpha = 0.9, color = "grey50") + #fill = analysisunit  shape = 21, color = "#333333", , color = analysisunit   size = 0.5, width = 0.001, height = 0.3
        geom_hline(yintercept = 0, color = "grey10", lwd = 0.5) +
        labs(title = parameters[column == p, name], subtitle = ifelse(stringr::str_detect(i, "NERR"), paste0(str_sub(i, 1, -6), " National Estuarine Research Reserve"),
                                                                      ifelse(stringr::str_detect(i, "NMS"), paste0(str_sub(i, 1, -5), " National Marine Sanctuary"), paste0(i, " Aquatic Preserve"))),
             x = "Year",
             y = parameters[column == p, name],
             #color = "Species",
             #fill = "Species",
             fill = "Number of\nobservations") +
        plot_theme +
        ylim(miny, 100) +
        # scale_size_area(limits = c(1, max(plotdat$npt))) +
        # scale_color_manual(values = subset(spcols, names(spcols) %in% unique(plotdat[, analysisunit])),
        #                    aesthetics = c("color", "fill")) +
        scale_fill_continuous_sequential(palette = "YlGnBu") +
        scale_x_continuous(breaks = c(seq(from = min(SAV4[ManagedAreaName == i & !is.na(eval(p)), relyear]),
                                          to = max(SAV4[ManagedAreaName == i & !is.na(eval(p)), relyear]),
                                          by = 3)),
                           labels = c(seq(from = min(SAV4[ManagedAreaName == i & !is.na(eval(p)), Year]),
                                          to = max(SAV4[ManagedAreaName == i & !is.na(eval(p)), Year]),
                                          by = 3)))
      
      if(length(models) > 0){
        #make sure that no failed models slipped through
        classes <- lapply(models, function(x) class(eval(x)))
        models <- models[classes != "try-error"]
        
        aucol <- as.name(names(plot_i$data)[1])
        
        plot_i <- addfits_blacktrendlines(models, plot_i, p) + 
          {if(usenames == "common"){
            facet_wrap(~factor(eval(aucol), levels = c("Total SAV",
                                                       "Total seagrass",
                                                       "Unidentified Halophila",
                                                       "Halophila spp.",
                                                       "Johnson's seagrass",
                                                       "Manatee grass",
                                                       "Paddle grass",
                                                       "Shoal grass",
                                                       "Star grass",
                                                       "Turtle grass",
                                                       "Widgeon grass",
                                                       "Attached algae",
                                                       "Drift algae")),
                       ncol = 3, strip.position = "top")
          } else{
            facet_wrap(~factor(eval(aucol), levels = c("Total SAV",
                                                       "Total seagrass",
                                                       "Halodule wrightii",
                                                       "Halophila decipiens",
                                                       "Halophila engelmannii",
                                                       "Halophila johnsonii",
                                                       "Unidentified Halophila",
                                                       "Halophila spp.",
                                                       "Ruppia maritima",
                                                       "Syringodium filiforme",
                                                       "Thalassia testudinum",
                                                       "Attached algae",
                                                       "Drift algae")),
                       labeller = c("Total SAV",
                                    "Total seagrass",
                                    "Halodule wrightii",
                                    "Halophila decipiens",
                                    "Halophila engelmannii",
                                    "Halophila johnsonii",
                                    "Halophila spp.",
                                    "Halophila spp.",
                                    "Ruppia maritima",
                                    "Syringodium filiforme",
                                    "Thalassia testudinum",
                                    "Attached algae",
                                    "Drift algae"), 
                       ncol = 3, strip.position = "top")
          }}
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

# subsetting models to only run first five available models
# malist <- malist[c(1,2,3,4,5)]

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

#Save .png versions of "barplot" .rds files --------------------------------------------------
plots2 <- stringr::str_subset(files, "_barplot") #identify map file

for(pl2 in plots2){
  plot_pl2 <- readRDS(here::here(paste0("output/Figures/BB/", pl2)))
  plot_pl2 <- plot_pl2 +
    plot_theme
  
  png(here::here(paste0("output/website/images/barplots/", str_sub(pl2, 1, -5), ".png")),
      width = 8,
      height = 4,
      units = "in",
      res = 200)
  
  print(plot_pl2)
  dev.off()
}
