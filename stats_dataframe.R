
# combines all data into one data frame
# combines all data into one frame to be used in modeling
# population data from pop_data.R
# distances and nearest neighbor from spatial.R
# visits from maint_data.R
# wells from well_data.R
# removes missing data (18938 to 16399)

library(tidyverse)
library(lubridate)

setwd("C:/Users/Eliza Fink/Desktop/WfG 2021")

load("population.Rdata")
load("distances.Rdata")
load("nearestneighbor.Rdata")
load("visits.Rdata")
load("wells.Rdata")

pop_tot$Year <- as.numeric(pop_tot$Year)

full_data <- left_join(visits, distances)
full_data <- left_join(full_data, well.spat)
full_data <- left_join(full_data, pop_tot)


full_data$month <- factor(month(full_data$activity_date, label = TRUE, abbr = TRUE), ordered = FALSE)

####add factor for conflict
full_data$conflict <- full_data$Year %in% c(2012, 2013, 2014)
####add flipped payment outcome
####change nearest neighbor units to kilometers
full_data$average_nn <- full_data$average_nn / 1000
# now all distance units are in km

write.csv(full_data, file = "full_data.csv")
save(visits, file = "visits.Rdata")

model <- full_data[,c("well_ID","pump_fixed","pump_working", "payment_YN","activity_date",
                      "Year", "month", "time_between", "last_funct", "pump_type", "sum_past_broken", 
                      "percent_past_broken", "follow_up", "eep_moto", "rapid", "dist_office", 
                      "urban_center", "average_nn", "Population_1k", "Population_3k", "conflict")]

model <- model[which(!apply(model, MARGIN = 1, function(x) sum(is.na(x))>0)),]   #18938 to 16399



write.csv(model, file = "model.csv")
save(model, file = "model.Rdata")