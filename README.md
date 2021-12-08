# wfg_2021
Determinants of handpump functionality in the Central African Republic - code used in analysis

Contact: elfi2553@colorado.edu

Date: 12/07/2021

## maint_data.R
first consolidates separate maintenance records from 2012-2018, 2019, and 2020
outputs this full data frame as file 1220maint.csv which is used in well_data.R file

then cleans maintenance data
 - pump fixed to binary
 - pump working to binary
 - last known functionality to binary
 - calculates history of breakdown (sum and percent)
 - payment to binary
 - pump type
 - hybrid motorcycle and rapid response teams

outputs cleaned data as visits_records.csv and visits.Rdata which are loaded into stats_dataframe.R


## well_data.R
this code takes the data consolidated in maint_data.R file  
outputs a list of all unique well IDs visited  
includes latitude, longitude, first visit date, last visit date, and decomission date (if applicable)

outputs from this file are used as the lat/long for all later spatial calculations

## pop_data.R
this code takes gps data from water for good and high density population data from facebook
to estimate the populatin in 1 and 3 km radii of each water point
population data downloaded from here: https://data.humdata.org/dataset/highresolutionpopulationdensitymaps-caf

outputs files population.Rdata that are loaded into stats_dataframe.R

## spatial.R

takes well data from well_data.R file (wells.Rdata)
calculates spatial variables
 - average distance to nearest 5 water for good wells
 - distance to water for good office
 - distance to nearest urban center

saves files as nearest nearestneighbor.Rdata and distances.Rdata which are loaded into stats_dataframe.R

## stats_dataframe.R
combines all data into one frame to be used in modeling  
population data from pop_data.R  
distances and nearest neighbor from spatial.R  
visits from maint_data.R  
wells from well_data.R  
removes missing data (18938 to 16399)  
outputs model.Rdata which is used for modeling  

## model_testing.R
tests model assumptions / validity
takes data consolidated in stats_dataframe.R
screens for outliers using mahalanobis distance for continuous variables
checks for colinearity using VIF scores (this done in multiple iterations as well as variable combos change)
looks at ratios of dependent variables
compares multilevel logistic regression (including accounting for repeated measure clusters) to regualr glm
code was manipulated to test AIC values, model accuracy, and probability of model accuracy better than no-information rate for combinations of control variables

## final_models.R
final models reported in paper
confusion matrix, accuracy, and probability of accuracy > no information rate
confidence intervals

## summary stats.R
this file calculates summary statistics for supplementaty information
loads final data frame from stats_dataframe.R 

## effects_plot_manuscript.R
creates all marginal effects plots included in the manuscript
uses sjPlot package
