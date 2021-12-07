# this code takes gps data from water for good and high density population data from facebook 
# to estimate the populatin in 1 and 3 km radii of each water point
# population data downloaded from here: https://data.humdata.org/dataset/highresolutionpopulationdensitymaps-caf

# outputs files population.Rdata and well_population.csv

# date: 01/29/21

# set up ------------------------------------------------------------------

library(lubridate)
library(rgeos)
library(sp)
library(rgdal)
library(tidyverse)
library(units)
library(raster)

setwd("C:/Users/Eliza Fink/Desktop/WfG 2021")


# write shapefiles --------------------------------------------------------

# shapefiles are of 1 and 3 km buffers around each water point

wells <- read.csv("well_data.csv")
wells <- wells[which(wells$Latitude > 0), c(2,7:8)]

wells <- sf::st_as_sf(wells, coords = c("Longitude", "Latitude"), crs = 4236)
wells <- sf::st_transform(wells, 32633)

threekm <- 3000
onekm <- 1000
units(threekm) <- as_units("m")
units(onekm) <- as_units("m")

wells3 <- wells

wells3$buffer3 <- sf::st_buffer(wells3$geometry, threekm) #3km radius around each well
wells$buffer1 <- sf::st_buffer(wells$geometry, onekm)   #1km radius around each well
wells$geometry <- NULL
wells3$geometry <- NULL

wells <- sf::st_as_sf(wells, crs = 32633)
wells_buffer1 <- as(wells, 'Spatial')

wells3 <- sf::st_as_sf(wells3, crs = 32633)
wells_buffer3 <- as(wells3, 'Spatial')

shapefile(wells_buffer3, 'popbuffer3.shp')
shapefile(wells_buffer1, 'popbuffer1.shp')

# clear global environment

# population calculation --------------------------------------------------

# read shapefiles back into R in the right form (OGR)
buffer1 <- readOGR("popbuffer1.shp")
buffer3 <- readOGR("popbuffer3.shp")

pop <- read.csv("pop_caf_fb.csv") #population in 2018
pop_2018 <- read.csv("pop_caf_fb.csv") #population in 2018

#projection
projGPS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
projCAR <- "+proj=utm +zone=33 +datum=WGS84 +datum=WGS84 +units=m +no_defs"

# make population into spatial points data frame
coordinates(pop) <- ~ Lon + Lat
# set the projection to EPSG 4326
proj4string(pop) <- CRS(projGPS)
# transform projection of the SpatialPointsDataFrame to UTM 33 N
# this allows us to workin meters instead of lat/long
pop <- spTransform(pop, projCAR)

# check that the extents of both match (did projection transformations work?)
extent(pop)
extent(buffer1)

# read in well data
# well_data.csv is outputed from the well_data.R file

wells <- read.csv("well_data.csv")

# make a list of well_ids
well_ids <- unique(wells[which(is.na(wells$well_ID) == "FALSE" &
                                 wells$well_ID != ""), "well_ID"])

well_pop1 <- data.frame(
  well_ID = character(),
  pop18 = numeric()
)

well_pop3 <- data.frame(
  well_ID = character(),
  pop18 = numeric()
)


# for loop
# for each unique well id, identify which grid points from the facebook data set fall within the 1 and 3 k buffer shapefiles
# then sum up the population for each point within the buffer
# this estimates population in 2018

for (i in well_ids) {
  
  #only the buffer of the important well
  circle1 <- buffer1[which(buffer1$well_ID == i),]
  circle3 <- buffer3[which(buffer3$well_ID == i),]
  #what points are of pop within the buffer zone? 
  #pull those from the population number data
  well_radius1 <- pop_2018[!is.na(over(pop,circle1)),]
  well_radius3 <- pop_2018[!is.na(over(pop,circle3)),]
  #match population data points to those in the circle
  #sum up population
  well_sums1 <- well_radius1 %>% 
    summarise(
      well_ID = i,
      pop18 = sum(Population),
    )
  
  well_sums3 <- well_radius3 %>% 
    summarise(
      well_ID = i,
      pop18 = sum(Population),
    )
  #add to master data frame
  well_pop1 <- bind_rows(well_pop1, well_sums1)
  well_pop3 <- bind_rows(well_pop3, well_sums3)
}

pop_tot <- left_join

write.csv(well_pop1, file = "well_pop1.csv")
write.csv(well_pop3, file = "well_pop3.csv")


well_pop3 <- well_pop3 %>%
  rename(
    pop18_3 = pop18
  )


# population growth adjustment
# based on World Bank growth information here: https://data.worldbank.org/indicator/SP.POP.GROW?locations=CF

year <- c("year", 2012:2019)
percent_growth <- c("percent_growth", 0.401556668,	0.259489943,
                    0.364290952,	0.647403919,	0.985894486,	1.277504136,
                    1.519051537,	1.674745212)

grow12 = 0.401556668
grow13 = 0.259489943
grow14 = 0.364290952
grow15 = 0.647403919
grow16 = 0.985894486
grow17 = 1.277504136
grow18 = 1.519051537
grow19 = 1.674745212

growth <- data.frame(year, percent_growth)
growth <- growth[-1,]

well_pop1_full <- well_pop1 %>%
  mutate(
    pop19 = pop18 * grow18,
    pop20 = pop18 * grow18 * grow19,
    pop17 = pop18 / grow17,
    pop16 = pop18 / grow17 / grow16,
    pop15 = pop18 / grow17 / grow16 / grow15,
    pop14 = pop18 / grow17 / grow16 / grow15 / grow14,
    pop13 = pop18 / grow17 / grow16 / grow15 / grow14 / grow13,
    pop12 = pop18 / grow17 / grow16 / grow15 / grow14 / grow13 / grow12
  )


well_pop3_full <- well_pop3 %>%
  mutate(
    pop19_3 = pop18_3 * grow18,
    pop20_3 = pop18_3 * grow18 * grow19,
    pop17_3 = pop18_3 / grow17,
    pop16_3 = pop18_3 / grow17 / grow16,
    pop15_3 = pop18_3 / grow17 / grow16 / grow15,
    pop14_3 = pop18_3 / grow17 / grow16 / grow15 / grow14,
    pop13_3 = pop18_3 / grow17 / grow16 / grow15 / grow14 / grow13,
    pop12_3 = pop18_3 / grow17 / grow16 / grow15 / grow14 / grow13 / grow12,
  )


# pivot data frames

well_pop1_full <- well_pop1_full %>%
  rename(
    '2012' = 'pop12',
    '2013' = 'pop13',
    '2014' = 'pop14',
    '2015' = 'pop15',
    '2016' = 'pop16',
    '2017' = 'pop17',
    '2018' = 'pop18',
    '2019' = 'pop19',
    '2020' = 'pop20'
  )


well_pop1_full <- well_pop1_full %>%
  pivot_longer(c('2012','2013','2014','2015','2016','2017','2018','2019', '2020'), 
               names_to = "Year", values_to = "Population_1k")

well_pop3_full <- well_pop3_full %>%
  rename(
    '2012' = 'pop12_3',
    '2013' = 'pop13_3',
    '2014' = 'pop14_3',
    '2015' = 'pop15_3',
    '2016' = 'pop16_3',
    '2017' = 'pop17_3',
    '2018' = 'pop18_3',
    '2019' = 'pop19_3',
    '2020' = 'pop20_3'
  )

well_pop3_full <- well_pop3_full %>%
  pivot_longer(c('2012','2013','2014','2015','2016','2017','2018','2019','2020'), 
               names_to = "Year", values_to = "Population_3k")


pop_tot <- left_join(well_pop1_full, well_pop3_full)

save(pop_tot, file = "population.Rdata")
write.csv(pop_tot, file = 'well_population.csv')

