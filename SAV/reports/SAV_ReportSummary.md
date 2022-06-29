---
title: "SEACAR Coastal Wetlands Analysis"
date: "Last compiled on 29 June, 2022"
output:
  html_document:
    toc: TRUE
    toc_float: TRUE
    toc_depth: 2
    dev: png
    keep_md: yes
  pdf_document:
    toc: TRUE
    toc_depth: 2
    dev: png
    extra_dependencies: ["float"]
---

# Important Notes
All scripts and outputs can be found on the SEACAR GitHub repository:

https://github.com/FloridaSEACAR/SEACAR_Panzik

This script is based off of code originally wrtten by Stephen Durham with comments by Marcus W. Beck.


# Summary of SEACAR_SAV_BB_script_update.R

+ Objective: Import and format SAV data, create summary plots and maps of changes over time, model changes over time using Bayesian and mixed-effects models. Results are separate for each managed area, species, and parameter (e.g., Braun Blanquet, percent cover, etc.).
+ Packages: bayesplot, brms, broom.mixed, data.table, grid, gridExtra, gtable, nlme, scales, sf, tictoc, tidybayes, tidyverse
+ File inputs: Combined_SAV_column_All-2021-Sep-20.csv, seacar_dbo_SampleLocation_Point.shp, seacar_dbo_SampleLocation_Line.shp, ORCP_Managed_Areas.shp, Counties_-_Detailed_Shoreline.shp, MApolygons_corners.csv
+ Steps by line number:
    + 1 - 17: load libraries and import SAV file
    + 18 - 104: format SAV data including renaming columns, removing NA values, and reformatting abundance/cover values. For the latter, this included removal of NA values and those out of range, and ensuring appropriate values for Braun Blanquet, modified Braun Blaunqet, percent occurrence, and percent cover.
    + 105 - 243: commented (not used) code for creating a percent occurrence table and functions for plotting model results
    + 244 - 293: function for plotting model predictions
    + 294 - 342: function for rotating a spatial object, used for plotting to create a presentation of stacked 2-d layers.
    + 343 - 366: import spatial data objects, project all to WGS 1984 datum. Spatial data objects include sample locations and relevant boundaries (counties, management area boundaries)
    + 367 - 400: setup parameter list and objects for looping through parameters to create models and summary output, parameters include Braun Blanquet, median percent cover, visual percent cover, percent occurrence, frequency of occurrence
    + 401 - 1712: loop through parameters to create models and summary output, the following is an outline of steps in this loop.
        + 416 -1710: loop through managed areas using parameter from outside loop
        + 435 - 449: create and save plot of parameter score for managed area over time by species
        + 450 - 464: create and save plot of parameter score for managed area over time by program ID
        + 465 - 479: create and save plot of parameter score for managed area over time grouped by species, by program ID
        + 480 - 494: create and save plot of quadrat sizes for managed area over time by species
        + 495 - 509: create and save plot of quadrat sizes for managed area over time by program ID
        + 510 - 524: create and save plot of method for managed area over time by species
        + 525 - 539: create and save plot of method for managed area over time by program ID
        + 540 - 555: create and save plot of method for managed area by quadrat size and species
        + 556 - 571: create and save plot of method for managed area by quadrat size and program ID
        + 572 - 603: create and save plots of grid values over time by species and program ID if data available
        + 604 - 636: create and save plots of depth values over time by species and program ID if data available
        + 637 - 655: create and save a plot legend of species
        + 675 - 681: loop through species to create and save a plot of parameter score over time
        + 681 - 704: create and save a plot of totals for the species of parameter score over time
        + 705 - 723: create and save a plot legend of species
        + 724 - 749: loop through species to create and save a plot of parameter score as boxplots over time
        + 750 - 772: create and save a plot of boxplots for the species of parameter score over time
        + 773 - 833: create and save stacked maps of parameter values for the managed area by year
        + 834 - 886: setup empty objects for model results
        + 888 - 1603: Loop through species to fit models, with separate exception statements for different parameters. The modeling workflow is similar for each parameter, with minor exceptions. The general goal of each is to assess trends in a parameter over time for a particular species and managed area. Each workflow includes error handling if models did not converge, produces summary tables of model fit, and summary plots showing model result. The models vary in the Gaussian distribution family for the response variable depending on parameter. Random effects (e.g., for LocationID) are used for all models.
        + 1604 - 1648: create and save barplots of parameter results for managed area over time by species, only for Braun Blanquet and percent cover, save model results from prior loops
        + 1649 - 1666: save model results from prior loops for frequency occurrence and percent occurrence
        + 1667 - 1704: create and save barplots of parameter results for managed area over time by species, only for frequency occurrence, save model results from prior loops
        + 1705 - 1709: print statements for the console
    + 1713 - 1716: save list of failed models and R session info
+ File outputs: Multiple binary RDS files.

# Summary of SEACAR_SAV_BB_script_website.R

This script is similar to SEACAR_SAV_BB_script_update.R, but only focuses on creating plots and tables needed for website display.

# Summary of SAV_BBpct_LME_tableconvert.R

The SAV_BBpct_LME_tableconvert.R script is designed to take the results of all LME model runs that are stored in RDS files, collect them, and write them to a new file to be used for the website.

# Summary of SAV_Website_Plots.R

The SAV_Website_Plots.R script is designed to take the RDS files that store plot information for barplots, load them, and save them as png images.
