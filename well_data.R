
# this code takes the data consolidated in maint_data.R file
# full maintenance records from 2012 - 2020

# outputs a list of all unique well IDs visited
# includes latitude, longitude, first visit date, last visit date, and decomission date (if applicable)

# outputs from this file are used as the lat/long for all later spatial calculations

library(tidyverse)
library(lubridate)

setwd("C:/Users/Eliza Fink/Desktop/WfG 2021")

visits <- read.csv("1220maint.csv")

visits$decom <- ifelse(visits$pump_fixed == "decommissioned" |
                         visits$pump_fixed == "Désactiver cette point d'eau", visits$activity_date, "")

#get rid of NA values for lat/long

visits <- visits[which(!is.na(visits$Latitude) &
                         visits$Longitude > 0),]

wells <- visits %>% 
  group_by(well_ID) %>%
  summarise(
    total_visits = n(),
    first_visit = min(as_date(mdy(activity_date))),
    decom_date = min(as_date(mdy(decom))),
    last_date = max (as_date(mdy(activity_date))),
    Latitude = median(Latitude),
    Longitude = median(Longitude)
    )

write.csv(wells, file = "well_data.csv")
save(wells, file = "wells.Rdata")  