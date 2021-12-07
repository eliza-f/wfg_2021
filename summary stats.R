# this file calculates summary statistics for supplementaty information
# loads final data frame from stats_dataframe.R

# -------------------------------------------------------------------------

# load model data

setwd("C:/Users/Eliza Fink/Desktop/WfG 2021")
load("model.Rdata")

# outcome variables -------------------------------------------------------

# pump fixed
table(model$pump_fixed)

# pump worked
table(model$pump_working)

# payment
table(model$payment_YN)

# Continuous --------------------------------------------------------------

# Date
summary(model$activity_date)
sd(model$activity_date)

# Time Since Last Maintenance Visit (days)
summary(model$time_between)
sd(model$time_between)

# Number of Times the Pump was Reported Broken since 2012
summary(model$sum_past_broken)
sd(model$sum_past_broken)

# Percent of Past Visits in Which the Pump was Broken since 2012
summary(model$percent_past_broken)
sd(model$percent_past_broken)

# Distance to Nearest Urban Center
summary(model$urban_center)
sd(model$urban_center)

# Number of People in 1km Radius of Well
summary(model$Population_1k)
sd(model$Population_1k)

# Average Distance to Nearest Water for Good Wells
summary(model$average_nn)
sd(model$average_nn)

# Categorical -------------------------------------------------------------

# Year 
table(as.factor(model$Year))

# Month
table(as.factor(model$month))

# Fixed at Previous Visit
table(model$last_funct)

# Pump Type
table(model$pump_type)

# Hybrid Motorcycle Program
table(model$eep_moto)

# Rapid Response Program
table(model$rapid)

# Follow Up Visit
table(model$follow_up)
