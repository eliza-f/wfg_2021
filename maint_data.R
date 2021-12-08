# this code consolidates and cleans maintenance data from water for records

# date: 01/28/2021

# set up ------------------------------------------------------------------

library(lubridate)
library(tidyverse)

setwd("C:/Users/Eliza Fink/Desktop/WfG 2021")

# load data

# maintenance records 2012 - 2018
visits12 <- read.csv("2012_full_data.csv")
# maintenance records 2019
visits19 <- read.csv("2019_full_data.csv")
# maintenance records 2020
visits20 <- read.csv("2020_full_data.csv")


# combine all data into one data frmae
visits <- bind_rows(visits12, visits19, visits20)

# remove observations without missing date
visits <- visits[which(visits$activity_date != "" &
                         is.na(visits$activity_date) == FALSE),]

rm("visits12", "visits19", "visits20")

# write csv --- this is in well_data.R
write.csv(visits, file = "1220maint.csv") 

# added last known functionality and time between in excel before reloading

#################################################################################

visits <- read.csv("1220maint.csv")

# make functionality TRUE/FALSE

# problem coded as functional

visits$pump_fixed <- visits$pump_fixed %in% c("oui", "Oui", 
                                                "problem", "Oui, mais pas correctement")
visits$pump_working <- visits$pump_working %in% c("oui", "Oui", 
                                                  "problem", "Oui, mais pas correctement")
visits$last_funct <- ifelse(is.na(visits$time_between) == FALSE, 
                            visits$last_funct %in% c("oui", "Oui", 
                                                     "problem", "Oui, mais pas correctement"), {NA})

# make activity date into date format

visits <- visits %>%
  mutate(
    activity_date = as_date(mdy(activity_date)),
    Year = year(activity_date)
  )


# calculate the sum and percentage of past breakdowns for each pump

sum_past_broken <- c()
percent_past_broken <- c()

for(i in 1:(dim(visits)[1])){
  this_id <- visits[i, "well_ID"]
  this_data <- visits[which(visits$well_ID == this_id),]
  past_data <- this_data[which(this_data$activity_date < visits[i, "activity_date"]),]
  sum_past_broken <- append(sum_past_broken, 
                            dim(past_data)[1] - sum(past_data$pump_working))
  percent_past_broken<- append(percent_past_broken, sum_past_broken[i] / dim(past_data)[1])
}


visits$sum_past_broken<- sum_past_broken
visits$percent_past_broken<- percent_past_broken
visits$percent_past_broken[is.na(visits$percent_past_broken)] <- 0

rm(list=c("sum_past_broken", "percent_past_broken", "this_data", "past_data", "i", "this_id"))


# payment

# if there was any payment mark true

visits$payment_YN <- if_else(visits$payment > 0, TRUE, FALSE, FALSE)

# pump types

visits$pump_type <- as_factor(tolower(visits$pump_type))

visits <- visits %>%
  mutate(
    pump_type = recode (
      pump_type, 
      'hpv_60_2000' = 'hpv_60',
      'india_mk_ii' = 'india_mark_2',
      'decommissioned' = 'other',
      'vergnet_hpv_60_meter' = 'hpv_60',
      'vergnet hpv 60 2000' = 'hpv_60',
      'vergnet hpv 60' = 'hpv_60',
      'decommissioned' = 'other',
      'autres' = 'other',
      'vergnet hpv 100' = 'hpv_100',
      'vergnet hydro india' = 'hydro_india',
      'electrique' = 'electric',
      'vergnet hpv 60/2000 compteur' = 'hpv_60',
      'pompe électrique solaire avec tour' = 'other',
      'india mark 2' = 'india_mark_2',
      'lifepump' = 'other',
    )
  )

visits$pump_type <- as_factor(if_else(visits$pump_type == "", "other", as.character(visits$pump_type)))

# follow up
# is the visit a follow up? 2020 only

visits$follow_up <- visits$service_visit_type == "Visite de suivi"

# eep moto
# was the vist conducted by the eep moto teams (hybrid motorcycle)

visits$eep_moto <- visits$team_number %in% c("EEP Moto 1", "eep_moto_1", 
                                             "eep_moto_2", "EEP Moto 2", "Animateur Moto 1")

# rapid response
# was the pump in the rapid response program?


# list of pumps enrolled in the program
rapid_pumps <- unique(visits[which(visits$service_visit_type == "rapid_response" |
                                     visits$service_visit_type == "Réponse rapide" ),2])

# code all visits as rapid response if they are in the list of rapid response pumps between first date of program and end of 2020

visits$rapid <- visits$well_ID %in% rapid_pumps &
  visits$activity_date >= min(visits[which(visits$service_visit_type == "rapid_response"),"activity_date"])

# month factor

visits$month <- month(visits$activity_date, label = TRUE)

visits <- visits[,-c(8:10)]


# save and write csv

write.csv(visits, file = "visits_records.csv")
save(visits, file = "visits.Rdata")
