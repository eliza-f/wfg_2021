
# calculates spatial values used in analysis

# date: 01/30/2021


# -------------------------------------------------------------------------

setwd("C:/Users/Eliza Fink/Desktop/WfG 2021")

library(maptools)
library(spatstat)
library(lubridate)
library(tidyverse)


# unique wells with lat/long from maintenance records
# calculated in well_data.R file
load("wells.Rdata")

# coordinates for urban centers in CAR plus locations of headquarters
cities <- read.csv("CAR_cities.csv")

# spatial calculations ----------------------------------------------------
# make well data frame spatial

wells.st <- sf::st_as_sf(wells, coords = c("Longitude", "Latitude"), crs = 4326)
wells.proj <- sf::st_transform(wells.st, crs = 32633)


# nearest neighbor
# well.spat (spatial) data frame will contain all spatial variables of wells
well.spat <- data.frame(
  well_ID = character(),
  Year = numeric(),
  average_nn = numeric()
)

#calculate nearest neighbor
for(i in 2012:2020){
  
  #make separate data frame for that year
  #get rid of wells not yet visited
  wellsyear <- wells.proj[which(year(wells.proj$first_visit) <= i),]
  #get rid of decommissioned wells
  wellsyear <- wellsyear[which(year(wellsyear$decom_date) >= i |
                                 is.na(wellsyear$decom_date) == TRUE),]
  
  wellsyear$Year <- i
  
  #change to spatial data frame object
  wellsyear.ppp <- as(wellsyear, "Spatial")
  #to ppp format, for use in spat stat
  wellsyear.ppp <- as(wellsyear.ppp, "ppp")
  
  #distance in meters to nearest neighbor
  wellsyear$dist1 <- nndist(wellsyear.ppp)
  #distance in meters to second nearest neighbor
  wellsyear$dist2 <- nndist(wellsyear.ppp, k = 2)
  #distance to third nearest neighbor
  wellsyear$dist3 <- nndist(wellsyear.ppp, k = 3)
  #calculate nearest neighbor estimates
  wellsyear$dist4 <- nndist(wellsyear.ppp, k = 4)
  #calculate nearest neighbor estimates
  wellsyear$dist5 <- nndist(wellsyear.ppp, k = 5)
  #calculate nearest neighbor estimates
  
  wellsyear <- wellsyear %>% mutate(
    average_nn = (dist1 + dist2 + dist3 + dist4 + dist5) / 5
  )
  
  wellsyear$geometry <- NULL
  
  
  wellsyear <- wellsyear[,c(1,6,12)]

  well.spat <- rbind(well.spat, wellsyear)
}

# distance to headquarters AND distance to nearest urban center
##distance to office calculation
BanguiHQ <- cities[14,c(2:3)]
BerberatiHQ <- cities[15,c(2:3)]

BanguiHQ <- sf::st_as_sf(BanguiHQ, coords = c("Long", "Lat"), crs = 4326)
BerberatiHQ <- sf::st_as_sf(BerberatiHQ, coords = c("Long", "Lat"), crs = 4326)


# calculate distance to headquarters in meters
wells.st$dist_ban <- sf::st_distance(wells.st$geometry,BanguiHQ$geometry[1])
wells.st$dist_ber <- sf::st_distance(wells.st$geometry,BerberatiHQ$geometry[1])

wells.st$dist_office <- ifelse(wells.st$dist_ber < wells.st$dist_ban, 
                               wells.st$dist_ber / 1000, 
                               wells.st$dist_ban / 1000)

#distance to city calculation
cities.sf <- sf::st_as_sf(cities, coords = c("Long", "Lat"), crs = 4326)

bangui <- cities.sf[1,4]
bimbo <- cities.sf[2,4]
berberati <- cities.sf[3,4]
carnot <- cities.sf[4,4]
bambari <- cities.sf[5,4]
bouar <- cities.sf[6,4]
bassan <- cities.sf[7,4]
bria <- cities.sf[8,4]
nola <- cities.sf[9,4]
kaga <- cities.sf[10,4]
sibut <- cities.sf[11,4]
mbaiki <- cities.sf[12,4]
bozoum <- cities.sf[13,4]


wells.st <- wells.st %>%
  mutate(
    bangui = sf::st_distance(wells.st$geometry,bangui$geometry[1]),
    bimbo = sf::st_distance(wells.st$geometry,bimbo$geometry[1]),
    berberati = sf::st_distance(wells.st$geometry,berberati$geometry[1]),
    carnot = sf::st_distance(wells.st$geometry,carnot$geometry[1]),
    bambari = sf::st_distance(wells.st$geometry,bambari$geometry[1]),
    bouar = sf::st_distance(wells.st$geometry,bouar$geometry[1]),
    bassan = sf::st_distance(wells.st$geometry,bassan$geometry[1]),
    bria = sf::st_distance(wells.st$geometry,bria$geometry[1]),
    nola = sf::st_distance(wells.st$geometry,nola$geometry[1]),
    kaga = sf::st_distance(wells.st$geometry,kaga$geometry[1]),
    sibut = sf::st_distance(wells.st$geometry,sibut$geometry[1]),
    mbaiki = sf::st_distance(wells.st$geometry,mbaiki$geometry[1]),
    bozoum = sf::st_distance(wells.st$geometry,bozoum$geometry[1])
  )

wells.st <- wells.st %>%
  rowwise() %>%
  mutate(
    urban_center = min(bangui, bimbo, berberati, carnot, bambari, bouar, bassan,
                       bria, nola, kaga, sibut, mbaiki, bozoum)
  )

wells.st$urban_center <- wells.st$urban_center / 1000
wells.st$urban_center <- as.numeric(wells.st$urban_center)

distances <- wells.st[,c(1,9,23)]


# save files

save(well.spat, file = 'nearestneighbor.Rdata')
save(distances, file = 'distances.Rdata')

write.csv(well.spat, file = 'nearestneighbor.csv')
write.csv(distances, file = 'distances.csv')
