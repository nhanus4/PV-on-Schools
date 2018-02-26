
# Install packages & libraries
install.packages("stringr")
install.packages("xml2")
install.packages("XML")

library(stringr)
library(maps)
library(mapproj)
library(ggplot2)
library(lubridate)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)
library(rgdal)
library(ggmap)
library(readxl)
library(dplyr)
library(chron)
library(FinCal)
library(xlsx)
library(grid)
library(ggplot2)
library(plyr)
library(scales)
library(rworldmap)
library(data.table)
library(httr)
library(XML)
library(xml2)
library(reshape2)
library(scales)
library(stats)
# install.packages("car", repos = "http://lib.stat.cmu.edu/R/CRAN/")
library(car)

###################################################
# Upload building lists
# Append population data to addresses
# Geocode the necessary datasets (i.e. K-12 Public)
###################################################


## Comparing old (7.11.17) with new (10.15.17)
# Upload the LIDAR output from NREL
#LIDAR.old <- read.csv("C:/Users/hanusnil/Box Sync/NREL deliverables/7.11.17/deliverables/join_output.csv", header=TRUE)
#LIDAR.old$JoinID <- paste(LIDAR.old$school_id_type, LIDAR.old$school_id)
#nrow(LIDAR.old[!duplicated(LIDAR.old[,"JoinID"]),]) # 38,022 points; matches summary provided by NREL

LIDAR.new <- read.csv("C:/Users/hanusnil/Box Sync/NREL deliverables/11.16.17/join_output_11_16_17.csv", header=TRUE)
LIDAR.new$JoinID <- paste(LIDAR.new$school_id_type, LIDAR.new$school_id)
nrow(LIDAR.new[!duplicated(LIDAR.new[,"JoinID"]),]) # 38,761 points matches what was said in the NREL email
nrow(LIDAR.new[!duplicated(LIDAR.new[,"JoinID"]) & !is.na(LIDAR.new[,"pct_school_pop"]),]) # 37,262 points matches what was said in the NREL email

# In LIDAR.new, check the number of times match_type == LIDAR and devp_sum_slopearea_sqft is blank
nrow(LIDAR.new[which(LIDAR.new$match_type == "LiDAR" & is.na(LIDAR.new$devp_sum_slopearea_sqft)),]) # 1,571 or 4% of the data; we will just keep these as NA

# In LIDAR.new, check the number of times pct_school_pop is blank
nrow(LIDAR.new[which(is.na(LIDAR.new$pct_school_pop)),]) # 3,526 or 9% of the data

# In LIDAR.new, check the number of times pct_school_pop is blank and the devp_sum_slopearea_sqft is not
nrow(LIDAR.new[which(is.na(LIDAR.new$pct_school_pop) & !is.na(LIDAR.new$devp_sum_slopearea_sqft) & !duplicated(LIDAR.new[,"JoinID"])),]) # 1,431 or 4% of the data 

# Check the types of schools are listed with the pct_school_pop being blank
count(LIDAR.new[which(is.na(LIDAR.new$pct_school_pop)),], 'school_id_type') # 126 higher education and 3,400 K-12

# Check the types of set_types are listed with the pct_school_pop being blank
count(LIDAR.new[which(is.na(LIDAR.new$pct_school_pop)),], 'set_type') # 673 buildings and 2853 campus

# Make a column that should be the roof area for each school
LIDAR.new$EstimatedArea       <- LIDAR.new$devp_sum_slopearea_sqft*LIDAR.new$pct_school_pop
LIDAR.new$EstimatedCapacity   <- LIDAR.new$devp_sum_capacity_kw*LIDAR.new$pct_school_pop
LIDAR.new$EstimatedGeneration <- LIDAR.new$devp_sum_generation_kwh*LIDAR.new$pct_school_pop

# Look at overall LIDAR school counts
nrow(LIDAR.new[!duplicated(LIDAR.new[,"JoinID"]),]) # 38,761
nrow(LIDAR.new) # 128,632 entries
LIDAR.unique <- LIDAR.new[!duplicated(LIDAR.new[,"JoinID"]),]
nrow(LIDAR.unique) # 38,761 points
count(LIDAR.unique, 'match_type') # LIDAR 22,321; OSM 16,440
count(LIDAR.unique, 'source_dataset') # Public 26,140; Private 9,586; HE 3,035
# Check to see if one of the schools in the duplicate list is entered once in the unique list
#View(LIDAR.unique[which(LIDAR.unique$set_id == 90561), ])

# look at the duplicates
LIDAR.duplicate <- LIDAR.new[duplicated(LIDAR.new[,"JoinID"]),]
# sort by the JoinID
LIDAR.duplicate <- LIDAR.duplicate[order(LIDAR.duplicate$JoinID),]
# see the top entries for the LIDAR.duplicate to find pattern
#View(head(LIDAR.duplicate))
nrow(LIDAR.duplicate) # 89,871 entries
count(LIDAR.duplicate, 'source_dataset') # HE 26,308; PR 6,279; PU 57,284
count(LIDAR.duplicate, 'match_type') # all OSM

# See which states have LIDAR
count(LIDAR.unique, "lidar_state") # Doesn't include Alaska (AK), Hawaii (HI), South Dakota (SD), Tennessee (TN) (4 missing out of 51 territories, including DC)

# Do some sanity checks to compare with Technical Potential NREL report
sum(LIDAR.unique$EstimatedCapacity, na.rm = TRUE)/(10^6) # 13.74 GW (same as in NREL email) -> compared to 1,118 GW for total US potential (NREL)
sum(LIDAR.unique$EstimatedGeneration, na.rm = TRUE)/(10^9) # 19.9 Twh/yr (same as in NREL email) -> compared to 1,432 TWh/yr for total US potential (NREL)
(sum(LIDAR.unique$EstimatedArea, na.rm = TRUE)*(.3048^2))/(10^9) # 0.09 Billion m2 -> compared to 8.12 billion m2 for total US potential (NREL)

# Look at summary stats and histograms across school types and data types
# K12-Public
summary(LIDAR.unique[which(LIDAR.unique$source_dataset == "public"),]$EstimatedArea) # mean = 24,990; median = 13,110
nrow(LIDAR.unique) # 38,761
sum(is.na(LIDAR.unique$EstimatedArea) & LIDAR.unique$source_dataset == "public") # 2583
nrow(LIDAR.unique[which(LIDAR.unique$EstimatedArea == 0 & LIDAR.unique$source_dataset == "public"),]) # 57

# Histograms
hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "public"),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,400000),
     xlab = "Roof Space (SF)", main = "K-12 Public: Overall NREL Estimate")

hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "public" & LIDAR.unique$EstimatedArea < 2000),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,2000),
     xlab = "Roof Space (SF)", main = "K-12 Public: Overall NREL Estimate, < 2,000SF")


# K12-Private
summary(LIDAR.unique[which(LIDAR.unique$source_dataset == "private"),]$EstimatedArea) # mean = 11,710; median = 4,888
hist2 <- hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "private"),]$EstimatedArea, 
              col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,400000),
              xlab = "Roof Space (SF)", main = "K-12 Private: Overall NREL Estimate")
sum(is.na(LIDAR.unique$EstimatedArea) & LIDAR.unique$source_dataset == "private") # 345
nrow(LIDAR.unique[which(LIDAR.unique$EstimatedArea == 0 & LIDAR.unique$source_dataset == "private"),]) # 11

hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "private"),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,400000),
     xlab = "Roof Space (SF)", main = "K-12 Private: Overall NREL Estimate")

hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "private" & LIDAR.unique$EstimatedArea < 2000),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,2000),
     xlab = "Roof Space (SF)", main = "K-12 Private: Overall NREL Estimate, < 2,000SF")


# Higher Education
summary(LIDAR.unique[which(LIDAR.unique$source_dataset == "higher education"),]$EstimatedArea) # mean = 77,680; median = 14,720
hist3 <- hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "higher education"),]$EstimatedArea, 
              col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,400000),
              xlab = "Roof Space (SF)", main = "Higher Education: Overall NREL Estimate")
sum(is.na(LIDAR.unique$EstimatedArea) & LIDAR.unique$source_dataset == "higher education") # 100
nrow(LIDAR.unique[which(LIDAR.unique$EstimatedArea == 0 & LIDAR.unique$source_dataset == "higher education"),]) # 32

hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "higher education"),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,400000),
     xlab = "Roof Space (SF)", main = "Higher Education: Overall NREL Estimate")

hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "higher education" & LIDAR.unique$EstimatedArea < 20000),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,20000),
     xlab = "Roof Space (SF)", main = "Higher Education: Overall NREL Estimate, < 20,000SF")


# Summary stats and histograms of LIDAR vs. OSM linked
# LIDAR
# K12-Public
summary(LIDAR.unique[which(LIDAR.unique$source_dataset == "public" & LIDAR.unique$match_type == "LiDAR"),]$EstimatedArea) # mean = 8,000; median = 852

hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "public" & LIDAR.unique$match_type == "LiDAR"),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,400000),
     xlab = "Roof Space (SF)", main = "K-12 Public: LiDAR match")
hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "public" & LIDAR.unique$match_type == "LiDAR" & LIDAR.unique$EstimatedArea < 2000),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,2000),
     xlab = "Roof Space (SF)", main = "K-12 Public: LiDAR match, < 2,000SF")

# K12-Private
summary(LIDAR.unique[which(LIDAR.unique$source_dataset == "private" & LIDAR.unique$match_type == "LiDAR"),]$EstimatedArea) # mean = 7,300; median = 3,000
hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "private" & LIDAR.unique$match_type == "LiDAR"),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,400000),
     xlab = "Roof Space (SF)", main = "K-12 Private: LiDAR match")
hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "private" & LIDAR.unique$match_type == "LiDAR" & LIDAR.unique$EstimatedArea < 2000),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,2000),
     xlab = "Roof Space (SF)", main = "K-12 Private: LiDAR match, < 2,000SF")


# Higher education
summary(LIDAR.unique[which(LIDAR.unique$source_dataset == "higher education" & LIDAR.unique$match_type == "LiDAR"),]$EstimatedArea) # mean = 27,520; median = 11,000
hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "higher education" & LIDAR.unique$match_type == "LiDAR"),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,400000),
     xlab = "Roof Space (SF)", main = "Higher Education: LiDAR match")

hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "higher education" & LIDAR.unique$match_type == "LiDAR" & LIDAR.unique$EstimatedArea < 20000),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,20000),
     xlab = "Roof Space (SF)", main = "Higher Education: LiDAR match, < 20,000SF")


# OSMs
# K12-Public
summary(LIDAR.unique[which(LIDAR.unique$source_dataset == "public" & LIDAR.unique$match_type == "OSM"),]$EstimatedArea) # mean = 39,920; median = 29,410
hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "public" & LIDAR.unique$match_type == "OSM"),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,400000),
     xlab = "Roof Space (SF)", main = "K-12 Public: OSM match")

hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "public" & LIDAR.unique$match_type == "OSM" & LIDAR.unique$EstimatedArea < 2000),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,2000),
     xlab = "Roof Space (SF)", main = "K-12 Public: OSM match, < 2,000SF")


# K12-Private
summary(LIDAR.unique[which(LIDAR.unique$source_dataset == "private" & LIDAR.unique$match_type == "OSM"),]$EstimatedArea) # mean = 22,400; median = 13,700
hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "private" & LIDAR.unique$match_type == "OSM"),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,400000),
     xlab = "Roof Space (SF)", main = "K-12 Private: OSM match")
hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "private" & LIDAR.unique$match_type == "OSM" & LIDAR.unique$EstimatedArea < 2000),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,2000),
     xlab = "Roof Space (SF)", main = "K-12 Private: OSM match, < 2,000SF")


# Higher Education
summary(LIDAR.unique[which(LIDAR.unique$source_dataset == "higher education" & LIDAR.unique$match_type == "OSM"),]$EstimatedArea) # mean = 225,100; median = 119,900
hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "higher education" & LIDAR.unique$match_type == "OSM"),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,400000),
     xlab = "Roof Space (SF)", main = "Higher Education: OSM match")

hist(LIDAR.unique[which(LIDAR.unique$source_dataset == "higher education" & LIDAR.unique$match_type == "OSM" & LIDAR.unique$EstimatedArea < 20000),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,20000),
     xlab = "Roof Space (SF)", main = "Higher Education: OSM match, < 20,000SF")



# Total counts of OSM & LIDAR
nrow(LIDAR.unique[which(LIDAR.unique$match_type == "OSM"),]) # 16,440
nrow(LIDAR.unique[which(LIDAR.unique$match_type == "LiDAR"),]) # 22,321
nrow(LIDAR.unique) # 38761

nrow(LIDAR.unique[which(LIDAR.unique$source_dataset == "public"),]) # 26140
nrow(LIDAR.unique[which(LIDAR.unique$source_dataset == "private"),]) # 9586
nrow(LIDAR.unique[which(LIDAR.unique$source_dataset == "higher education"),]) # 3035

nrow(LIDAR.unique[which(LIDAR.unique$match_type == "OSM" & LIDAR.unique$source_dataset == "public"),]) # 12,994
nrow(LIDAR.unique[which(LIDAR.unique$match_type == "OSM" & LIDAR.unique$source_dataset == "private"),]) # 2,695
nrow(LIDAR.unique[which(LIDAR.unique$match_type == "OSM" & LIDAR.unique$source_dataset == "higher education"),]) # 751

nrow(LIDAR.unique[which(LIDAR.unique$match_type == "LiDAR" & LIDAR.unique$source_dataset == "public"),]) # 13,146
nrow(LIDAR.unique[which(LIDAR.unique$match_type == "LiDAR" & LIDAR.unique$source_dataset == "private"),]) # 6,891
nrow(LIDAR.unique[which(LIDAR.unique$match_type == "LiDAR" & LIDAR.unique$source_dataset == "higher education"),]) # 2284

nrow(LIDAR.unique[which(LIDAR.unique$source_dataset == "higher education"),]) # 3035

count(LIDAR.unique$lidar_city) # 120 cities
count(LIDAR.unique[which(LIDAR.unique$match_type == "OSM"),]$lidar_city) # 120 cities

#########################################################
#########################################################
#### Upload school geocode data and add to roof data ####
#########################################################
#########################################################

#####################
#### K-12 PUBLIC ####
#####################

# Read in public school data with the geocodes
# read in the data the NREL used to link the LIDAR data
PU.NREL <- read.csv("C:/Users/hanusnil/Box Sync/NREL deliverables/7.11.17/deliverables/k_12_public_database.csv", header=TRUE)
PU.NREL$school_id_type <- 'K-12'
PU.NREL$JoinID <- paste(PU.NREL$school_id_type, PU.NREL$id)
nrow(PU.NREL[!duplicated(PU.NREL[,"JoinID"]),]) #101,294... we sent them a spreadsheet with 101331 schools, missing 37 schools
PU.NREL.LIDAR <- merge(PU.NREL, LIDAR.unique, by = "JoinID", all.x = TRUE)

# Now clean the merged dataset
PU.NREL.LIDAR$school_name.y <- NULL
PU.NREL.LIDAR$address.y <- NULL
PU.NREL.LIDAR$source_dataset.y <- NULL
PU.NREL.LIDAR$school_id_type.y <- NULL

write.table(PU.NREL.LIDAR, file = "C:/Users/hanusnil/Box Sync/Model Outputs/School Counts and GeoCodes/K-12PublicLidar no pct fix 11.7.17.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")


######################
#### K-12 PRIVATE ####
######################

# Read in private school data with the geocodes
# read in the data the NREL used to link the LIDAR data
PR.NREL <- read.csv("C:/Users/hanusnil/Box Sync/NREL deliverables/7.11.17/deliverables/k_12_private_database.csv", header=TRUE)
PR.NREL$school_id_type <- 'K-12'
PR.NREL$JoinID <- paste(PR.NREL$school_id_type, PR.NREL$id)
nrow(PR.NREL[!duplicated(PR.NREL[,"JoinID"]),]) # 26980
PR.NREL.LIDAR <- merge(PR.NREL, LIDAR.unique, by = "JoinID", all.x = TRUE)

# Now clean the merged dataset
PR.NREL.LIDAR$school_name.y <- NULL
PR.NREL.LIDAR$address.y <- NULL
PR.NREL.LIDAR$source_dataset.y <- NULL
PR.NREL.LIDAR$school_id_type.y <- NULL

write.table(PR.NREL.LIDAR, file = "C:/Users/hanusnil/Box Sync/Model Outputs/School Counts and GeoCodes/K-12PrivateLidar no pct fix 11.7.17.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")

##########################
#### HIGHER EDUCATION ####
##########################

# Read in higher education data with the geocodes
# read in the data the NREL used to link the LIDAR data
HE.NREL <- read.csv("C:/Users/hanusnil/Box Sync/NREL deliverables/7.11.17/deliverables/higher_education_database.csv", header=TRUE)
HE.NREL$school_id_type <- 'Higher Education'
# need to convert "id" to numeric
HE.NREL$id <- as.numeric(gsub(",","",HE.NREL$id))
HE.NREL$JoinID <- paste(HE.NREL$school_id_type, HE.NREL$id)
HE.NREL.LIDAR <- merge(HE.NREL, LIDAR.unique, by = "JoinID", all.x = TRUE)
nrow(HE.NREL.LIDAR) #7764
nrow(HE.NREL.LIDAR[which(HE.NREL.LIDAR$set_id != "NA"),]) #3035

# Now clean the merged dataset
HE.NREL.LIDAR$school_name.y <- NULL
HE.NREL.LIDAR$address.y <- NULL
HE.NREL.LIDAR$source_dataset.y <- NULL
HE.NREL.LIDAR$school_id_type.y <- NULL
names(HE.NREL.LIDAR)[10] <- "teachers"
HE.NREL.LIDAR <- HE.NREL.LIDAR[, c(1:6,8,7,9:11,27,12:26,28:33)]
names(HE.NREL.LIDAR)[12] <- "source_dataset.x"
HE.NREL.LIDAR$source_dataset.x <- "higher education"

write.table(HE.NREL.LIDAR, file = "C:/Users/hanusnil/Box Sync/Model Outputs/School Counts and GeoCodes/HigherEducationLidar no pct fix 11.7.17.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")

###################################################
###################################################
# Combine LIDAR datasets & Import states for maps
###################################################
###################################################

AllSchools.LIDAR <- rbind(PU.NREL.LIDAR, PR.NREL.LIDAR, HE.NREL.LIDAR)

# Import state data for plotting
states <- readOGR(dsn = "C:/Users/hanusnil/Box Sync/states_21basic", layer = "states")
# get states into dataframe
states@data$id <- rownames(states@data)
states.df <- fortify(states, region="id")
statesDF <- merge(states.df, states@data, by = "id")

# Remove Alaska and Hawaii
remove1 <- c("Hawaii","Alaska")
nrow(statesDF[which(statesDF$STATE_NAME == "Alaska" | statesDF$STATE_NAME == "Hawaii"),]) # 2213
nrow(statesDF) #13694
statesDF <- statesDF[!statesDF$STATE_NAME %in% remove1, ]
nrow(statesDF) # 11481

# Also remove schools reporting lat/lon values outside the US main borders
Lat.Max <- max(statesDF$lat)
Lat.Min <- min(statesDF$lat)
Lon.Max <- max(statesDF$long)
Lon.Min <- min(statesDF$long)

# Remove alaska, hawaii, and virgin islands from AllSchools.LIDAR
remove2 <- c("AK","HI", "VI")
nrow(AllSchools.LIDAR[which((AllSchools.LIDAR$state == "AK" | AllSchools.LIDAR$state == "HI") & AllSchools.LIDAR$source_dataset.x == "higher education"),]) # 40 out of 136,038
nrow(AllSchools.LIDAR[which(AllSchools.LIDAR$state == "AK" | AllSchools.LIDAR$state == "HI"),]) # 984 out of 136,038
AllSchools.LIDAR.reduced <- AllSchools.LIDAR[!AllSchools.LIDAR$state %in% remove2, ] 
nrow(AllSchools.LIDAR.reduced) #135,054
count(AllSchools.LIDAR.reduced, 'source_dataset.x') # HE 7724

AllSchools.LIDAR.reduced <- AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$latitude < Lat.Max &
                                                             AllSchools.LIDAR.reduced$latitude > Lat.Min &
                                                             AllSchools.LIDAR.reduced$longitude < Lon.Max &
                                                             AllSchools.LIDAR.reduced$longitude > Lon.Min),] 
nrow(AllSchools.LIDAR.reduced) # 134,135
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$set_id != "NA"),]) # 38,761
count(AllSchools.LIDAR.reduced, 'source_dataset.x') # HE 7556; PU 99,772; PR 26,807

# Include a variable for whether or not we have LIDAR data
AllSchools.LIDAR.reduced$LIDAR <- 0
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$set_id != "NA"),]$LIDAR <- "HaveLIDAR"
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == 0),]$LIDAR <- "NoLIDAR"

# Check the class of every variable
class(AllSchools.LIDAR.reduced$JoinID) # character
class(AllSchools.LIDAR.reduced$school_name.x) # factor
class(AllSchools.LIDAR.reduced$address.x) # factor
class(AllSchools.LIDAR.reduced$city) # factor
class(AllSchools.LIDAR.reduced$state) # factor
class(AllSchools.LIDAR.reduced$zip) # character
class(AllSchools.LIDAR.reduced$latitude) # numeric
class(AllSchools.LIDAR.reduced$longitude) # numeric
class(AllSchools.LIDAR.reduced$students) # character - need to change to numeric
class(AllSchools.LIDAR.reduced$teachers) # character - need to change to numeric
class(AllSchools.LIDAR.reduced$population) # character - need to change to numeric
class(AllSchools.LIDAR.reduced$source_dataset.x) # factor
class(AllSchools.LIDAR.reduced$id) # numeric
class(AllSchools.LIDAR.reduced$school_id_type.x) # character
class(AllSchools.LIDAR.reduced$set_id) # integer
class(AllSchools.LIDAR.reduced$set_type) # factor
class(AllSchools.LIDAR.reduced$school_id) # integer
class(AllSchools.LIDAR.reduced$lidar_building_id) # integer
class(AllSchools.LIDAR.reduced$osm_id) # integer
class(AllSchools.LIDAR.reduced$distance) # numeric
class(AllSchools.LIDAR.reduced$match_type) # factor
class(AllSchools.LIDAR.reduced$lidar_city) # factor
class(AllSchools.LIDAR.reduced$lidar_state) # factor
class(AllSchools.LIDAR.reduced$lidar_year) # integer
class(AllSchools.LIDAR.reduced$devp_sum_capacity_kw) # numeric
class(AllSchools.LIDAR.reduced$devp_sum_generation_kwh) # numeric
class(AllSchools.LIDAR.reduced$devp_sum_slopearea_sqft) # numeric
class(AllSchools.LIDAR.reduced$sum_pop) # numeric
class(AllSchools.LIDAR.reduced$pct_school_pop) # numeric
class(AllSchools.LIDAR.reduced$EstimatedArea) # numeric
class(AllSchools.LIDAR.reduced$Flag) # numeric
class(AllSchools.LIDAR.reduced$LIDAR) # character

# need to convert students, teachers, and population to numeric
AllSchools.LIDAR.reduced$students <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$students))
AllSchools.LIDAR.reduced$teachers <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$teachers))
AllSchools.LIDAR.reduced$population <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$population))

nrow(AllSchools.LIDAR.reduced) #134,135
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),])/nrow(AllSchools.LIDAR.reduced) # 29%
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced$source_dataset.x == "public"),])/nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public"),]) # 26%
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced$source_dataset.x == "private"),])/nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "private"),]) # 36%
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced$source_dataset.x == "higher education"),])/nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "higher education"),]) # 40%


AllSchools.LIDAR.reduced$SchoolType <- 0
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public"),"SchoolType"] <- "K12Public"
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "private"),"SchoolType"] <- "K12Private"
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "higher education"),"SchoolType"] <- "HigherEducation"
nrow(AllSchools.LIDAR.reduced) # 134,135
count(AllSchools.LIDAR.reduced, 'source_dataset.x') # HE 7556; PU 99,772; PR 26,807


# Save combined dataset
write.table(AllSchools.LIDAR.reduced, file = "C:/Users/hanusnil/Box Sync/Model Outputs/School Counts and GeoCodes/AllSchoolsLidar no pct fix 11.7.17.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")

###################################################
###################################################
# Read in the cleaned up AllSchool Lidar data
###################################################
###################################################

#AllSchools.LIDAR.reduced <- read.csv("C:/Users/hanusnil/Box Sync/Model Outputs/School Counts and GeoCodes/AllSchoolsLidar no pct fix 11.7.17.csv", header=TRUE)
#nrow(AllSchools.LIDAR.reduced) # 134,137

################################################################
################################################################
### Add variables from other Census datasets for regressions ###
################################################################
################################################################

# need to remove the "-xxxx" in some of the zip codes for the LIDAR databse
# then convert to numeric
AllSchools.LIDAR.reduced$zip.clean <- as.numeric(gsub("([0-9]+)-.*", "\\1", AllSchools.LIDAR.reduced$zip))

# Read in FIPS code data
# FIPS code is the 5-digit Federal Information Processing Standard (FIPS) - uniquely identifies counties and county equivalents
FIPS.county <- read.csv(file = "C:/Users/hanusnil/Box Sync/ZIP_COUNTY_062016.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
names(FIPS.county)[1] <- "zip.clean"
nrow(AllSchools.LIDAR.reduced[!duplicated(AllSchools.LIDAR.reduced[,"zip.clean"]),]) #24,008
nrow(FIPS.county[!duplicated(FIPS.county[,"zip.clean"]),]) # 39,487
FIPS.county.clean <- FIPS.county[!duplicated(FIPS.county[,"zip.clean"]),]
nrow(FIPS.county.clean) #39,487

# Combine the LIDAR data with the county FIPS code data
AllSchools.LIDAR.reduced <- merge(AllSchools.LIDAR.reduced, FIPS.county.clean, by = "zip.clean", all.x = TRUE)

# sanity checks
nrow(AllSchools.LIDAR.reduced) # 134,135
count(AllSchools.LIDAR.reduced, 'source_dataset.x') # HE 7556; PU 99,772; PR 26,807 

# Read in the US Census Bureau urban and rural distinction lookup
RuralUrbanZip <- read.csv("C:/Users/hanusnil/Box Sync/RuralUrbanZips.csv", header=TRUE)
names(RuralUrbanZip)[1] <- "COUNTY"
class(RuralUrbanZip$COUNTY) # integer
class(AllSchools.LIDAR.reduced$COUNTY) # integer

# Combine the LIDAR data with the urban distinction
AllSchools.LIDAR.reduced <- merge(AllSchools.LIDAR.reduced, RuralUrbanZip, by = "COUNTY", all.x = TRUE)
# change name to "percent rural"
names(AllSchools.LIDAR.reduced)[42] <- "PercentRural"

# sanity checks
nrow(AllSchools.LIDAR.reduced) #134,135
count(AllSchools.LIDAR.reduced, 'source_dataset.x') # HE 7556; PU 99,772; PR 26,807
nrow(count(AllSchools.LIDAR.reduced, "COUNTY")) #3046
head(count(AllSchools.LIDAR.reduced, "COUNTY"))
tail(count(AllSchools.LIDAR.reduced, "COUNTY"))
nrow(AllSchools.LIDAR.reduced[!duplicated(AllSchools.LIDAR.reduced[,"COUNTY"]),]) #3046

# Read census median income and poverty data
Census.income <- read.csv(file = "C:/Users/hanusnil/Box Sync/est10ALL - reduced.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
Census.income$County.FIPS.clean <- sprintf("%03d",Census.income$County.FIPS)
Census.income$COUNTY <- paste(Census.income$State.FIPS, Census.income$County.FIPS.clean, sep = "")
class(Census.income$COUNTY) # character, need to convert to numeric
Census.income$COUNTY <- as.integer(gsub(",","", Census.income$COUNTY))
class(Census.income$COUNTY)
nrow(count(Census.income, "COUNTY")) #3196
head(count(Census.income, "COUNTY"))
tail(count(Census.income, "COUNTY"))
nrow(Census.income[!duplicated(Census.income[,"COUNTY"]),]) #3196
Census.income.clean <- Census.income[!duplicated(Census.income[,"COUNTY"]),]
nrow(Census.income.clean) #3196

# Combine the LIDAR data with the census income data
AllSchools.LIDAR.reduced <- merge(AllSchools.LIDAR.reduced, Census.income.clean, by = "COUNTY", all.x = TRUE)

# sanity checks
nrow(AllSchools.LIDAR.reduced) # 134,137
count(AllSchools.LIDAR.reduced, 'source_dataset.x') # HE 7556; PU 99,772; PR 26,807 

# Read in census population density
Census.population <- read.csv(file = "C:/Users/hanusnil/Box Sync/Variables to include/Density.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
class(Census.population$COUNTY) # integer
class(Census.population$PopDensity) # numeric
class(Census.population$HouseDensity) # numeric

# Combine the LIDAR data with the census poulation data
length(intersect(Census.population$COUNTY, AllSchools.LIDAR.reduced$COUNTY)) # 3045
AllSchools.LIDAR.reduced <- merge(AllSchools.LIDAR.reduced, Census.population, by = "COUNTY", all.x = TRUE)

# sanity checks
nrow(AllSchools.LIDAR.reduced) # 134,135
count(AllSchools.LIDAR.reduced, 'source_dataset.x') # HE 7556; PU 99,772; PR 26,807 

# Read in other various census data: Below18, 18andOver, GovHealth, Physicians, VehiclesHouse, GovHigh, CountyArea, WaterArea, Farmland, Cropland, GovRev, Employment
Census.misc <- read.csv(file = "C:/Users/hanusnil/Box Sync/Variables to include/CensusData.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
class(Census.misc$COUNTY) # integer
class(Census.misc$Below18) # integer
class(Census.misc$X18andOver) # integer
class(Census.misc$GovHealth) # integer
class(Census.misc$Physicians) #integer
class(Census.misc$VehiclesHouse) # numeric
class(Census.misc$GovHigh) #integer
class(Census.misc$CountyArea) # numeric
class(Census.misc$WaterArea) # numeric
class(Census.misc$Farmland) # integer
class(Census.misc$Cropland) # integer
class(Census.misc$GovRev) # integer
class(Census.misc$Employment) # integer

# Combine the LIDAR data with the census misc data
length(intersect(Census.misc$COUNTY, AllSchools.LIDAR.reduced$COUNTY)) # 3044
AllSchools.LIDAR.reduced <- merge(AllSchools.LIDAR.reduced, Census.misc, by = "COUNTY", all.x = TRUE)

# sanity checks
nrow(AllSchools.LIDAR.reduced) # 134,135
count(AllSchools.LIDAR.reduced, 'source_dataset.x') # HE 7556; PU 99,772; PR 26,807 

############################################################
############################################################
#### Upload additional school data and add to roof data ####
############################################################
############################################################

# K12-Public

# Introduce some public school data
# load in 4 public school datasets
PU1.misc <- read.csv(file = "C:/Users/hanusnil/Box Sync/Variables to include/PUmisc1.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
PU2.misc <- read.csv(file = "C:/Users/hanusnil/Box Sync/Variables to include/PUmisc2.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
PU3.misc <- read.csv(file = "C:/Users/hanusnil/Box Sync/Variables to include/PUmisc3.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
PU4.misc <- read.csv(file = "C:/Users/hanusnil/Box Sync/Variables to include/PUmisc4.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)

PU1.misc$joinID <- paste(PU1.misc$SCHID, PU1.misc$NCESSCH, PU1.misc$SCH_NAME)
nrow(PU1.misc) #102799
nrow(PU1.misc[!duplicated(PU1.misc[,"joinID"]),]) #102799

PU2.misc$joinID <- paste(PU2.misc$SCHID, PU2.misc$NCESSCH, PU2.misc$SCH_NAME)
nrow(PU2.misc) #100187
nrow(PU2.misc[!duplicated(PU2.misc[,"joinID"]),]) #100187

PU3.misc$joinID <- paste(PU3.misc$SCHID, PU3.misc$NCESSCH, PU3.misc$SCH_NAME)
nrow(PU3.misc) #100184
nrow(PU3.misc[!duplicated(PU3.misc[,"joinID"]),]) #100184

PU4.misc$joinID <- paste(PU4.misc$SCHID, PU4.misc$NCESSCH, PU4.misc$SCH_NAME)
nrow(PU4.misc) #100187
nrow(PU4.misc[!duplicated(PU4.misc[,"joinID"]),]) #100187

PU.misc <- merge(PU1.misc, PU2.misc, by = "joinID", all.x = TRUE)
PU.misc <- merge(PU.misc, PU3.misc, by = "joinID", all.x = TRUE)
PU.misc <- merge(PU.misc, PU4.misc, by = "joinID", all.x = TRUE)
PU.misc[,c(12:14, 16:18,20:22)] <- NULL

PU.misc$PUJoinID <- paste(PU.misc$SCH_NAME.x, PU.misc$LSTREET1, PU.misc$LCITY, PU.misc$LSTATE)
nrow(PU.misc[!duplicated(PU.misc[,"PUJoinID"]),]) # 102,662
nrow(PU.misc) # 102799
# remove duplicates
PU.misc <- PU.misc[!duplicated(PU.misc[,"PUJoinID"]),]
nrow(PU.misc) # 102,662
PU.misc[,1:8] <- NULL

# create PUJoin id for the AllSchools data
AllSchools.LIDAR.reduced$PUJoinID <- paste(AllSchools.LIDAR.reduced$school_name.x, AllSchools.LIDAR.reduced$address.x,
                                           AllSchools.LIDAR.reduced$city, AllSchools.LIDAR.reduced$state)
nrow(AllSchools.LIDAR.reduced[(!duplicated(AllSchools.LIDAR.reduced[,"PUJoinID"]) & AllSchools.LIDAR.reduced$source_dataset.x == "public"),]) # 99,666
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public"),]) #99,774

# Combine the LIDAR data with the Public misc data
length(intersect(PU.misc$PUJoinID, AllSchools.LIDAR.reduced$PUJoinID)) # 99217
AllSchools.LIDAR.reduced <- merge(AllSchools.LIDAR.reduced, PU.misc, by = "PUJoinID", all.x = TRUE)

# sanity checks
nrow(AllSchools.LIDAR.reduced) # 134,135
count(AllSchools.LIDAR.reduced, 'source_dataset.x') # HE 7556; PU 99,772; PR 26,807 



# K12-Private

# Introduce some private school data
Private.misc <- read.csv(file = "C:/Users/hanusnil/Box Sync/Variables to include/pss1112_pu_misc.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
nrow(Private.misc) # 26983
Private.misc$PRJoinID <- paste(Private.misc$pinst, Private.misc$paddrs, Private.misc$pcity, Private.misc$pstabb, 
                               Private.misc$pzip, Private.misc$Population)
nrow(Private.misc[!duplicated(Private.misc[,"PRJoinID"]),]) # 26,981
# remove duplicates
Private.misc <- Private.misc[!duplicated(Private.misc[,"PRJoinID"]),]
nrow(Private.misc) # 26,981
Private.misc[,1:10] <- NULL

# create PRJoin id for the AllSchools data
AllSchools.LIDAR.reduced$PRJoinID <- paste(AllSchools.LIDAR.reduced$school_name.x, AllSchools.LIDAR.reduced$address.x,
                                           AllSchools.LIDAR.reduced$city, AllSchools.LIDAR.reduced$state, AllSchools.LIDAR.reduced$zip.clean,
                                           AllSchools.LIDAR.reduced$population)
nrow(AllSchools.LIDAR.reduced[(!duplicated(AllSchools.LIDAR.reduced[,"PRJoinID"]) & AllSchools.LIDAR.reduced$source_dataset.x == "private"),]) # 26,807
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "private"),]) #26,807

# Combine the LIDAR data with the Private misc data
length(intersect(Private.misc$PRJoinID, AllSchools.LIDAR.reduced$PRJoinID)) # 26,804
AllSchools.LIDAR.reduced <- merge(AllSchools.LIDAR.reduced, Private.misc, by = "PRJoinID", all.x = TRUE)

# sanity checks
nrow(AllSchools.LIDAR.reduced) # 134,137
count(AllSchools.LIDAR.reduced, 'source_dataset.x') # HE 7556; PU 99,772; PR 26,807 


# Higher Education

# Introduce some higher education school data
Higher.misc <- read.csv(file = "C:/Users/hanusnil/Box Sync/Variables to include/HE Misc.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
nrow(Higher.misc) # 7764
Higher.misc$HEJoinID <- paste(Higher.misc$INSTNM, Higher.misc$ADDR, Higher.misc$CITY, Higher.misc$STABBR)
nrow(Higher.misc[!duplicated(Higher.misc[,"HEJoinID"]),]) # 7755

# remove duplicates
Higher.misc <- Higher.misc[!duplicated(Higher.misc[,"HEJoinID"]),]
nrow(Higher.misc) # 7755
Higher.misc[,1:6] <- NULL


# create HEJoin id for the AllSchools data
AllSchools.LIDAR.reduced$HEJoinID <- paste(AllSchools.LIDAR.reduced$school_name.x, AllSchools.LIDAR.reduced$address.x,
                                           AllSchools.LIDAR.reduced$city, AllSchools.LIDAR.reduced$state)
nrow(AllSchools.LIDAR.reduced[(!duplicated(AllSchools.LIDAR.reduced[,"HEJoinID"]) & AllSchools.LIDAR.reduced$source_dataset.x == "higher education"),]) # 7547
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "higher education"),]) # 7556


# Combine the LIDAR data with the Higher Education misc data
length(intersect(Higher.misc$HEJoinID, AllSchools.LIDAR.reduced$HEJoinID)) # 6882
AllSchools.LIDAR.reduced <- merge(AllSchools.LIDAR.reduced, Higher.misc, by = "HEJoinID", all.x = TRUE)

# sanity checks
nrow(AllSchools.LIDAR.reduced) # 134,137
count(AllSchools.LIDAR.reduced, 'source_dataset.x') # HE 7556; PU 99,772; PR 26,807 


# Save combined dataset
write.table(AllSchools.LIDAR.reduced, file = "C:/Users/hanusnil/Box Sync/Model Outputs/School Counts and GeoCodes/AllSchoolsLidar no pct fix with reg data 11.7.17.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")

######################################################
######################################################
# check the class of each variable in the regression #
######################################################
######################################################

class(AllSchools.LIDAR.reduced$EstimatedArea) # numeric
class(AllSchools.LIDAR.reduced$population) # numeric
class(AllSchools.LIDAR.reduced$students) # numeric
class(AllSchools.LIDAR.reduced$teachers) # numeric
class(AllSchools.LIDAR.reduced$state) # factor
class(AllSchools.LIDAR.reduced$Poverty.Percent.All.Ages) # character - change to numeric
class(AllSchools.LIDAR.reduced$Median.Household.Income) # character - change to numeric
class(AllSchools.LIDAR.reduced$PercentRural) # integer - change to factor
class(AllSchools.LIDAR.reduced$SchoolType) # character - change to factor
class(AllSchools.LIDAR.reduced$PopDensity) # numeric
class(AllSchools.LIDAR.reduced$HouseDensity) # numeric
class(AllSchools.LIDAR.reduced$Below18) # integer - change to numeric
class(AllSchools.LIDAR.reduced$X18andOver) # integer - change to numeric
class(AllSchools.LIDAR.reduced$GovHealth) # integer - change to numeric
class(AllSchools.LIDAR.reduced$Physicians) # integer - change to numeric
class(AllSchools.LIDAR.reduced$VehiclesHouse) # numeric
class(AllSchools.LIDAR.reduced$GovHigh) # integer - change to numeric
class(AllSchools.LIDAR.reduced$CountyArea) # numeric 
class(AllSchools.LIDAR.reduced$WaterArea) # numeric
class(AllSchools.LIDAR.reduced$Farmland) # integer - change to numeric
class(AllSchools.LIDAR.reduced$Cropland) # integer - change to numeric
class(AllSchools.LIDAR.reduced$GovRev) # integer - change to numeric
class(AllSchools.LIDAR.reduced$Employment) # integer - change to numeric
class(AllSchools.LIDAR.reduced$PopDensity) # numeric
class(AllSchools.LIDAR.reduced$HouseDensity) # numeric
class(AllSchools.LIDAR.reduced$LANDGRNT) # integer - change to factor
class(AllSchools.LIDAR.reduced$PTC_EF) # integer- change to factor
class(AllSchools.LIDAR.reduced$PGRNT_P) # character - change to numeric
class(AllSchools.LIDAR.reduced$RELAFFIL) # character - change to factor
class(AllSchools.LIDAR.reduced$LEVEL1) # character - change to factor
class(AllSchools.LIDAR.reduced$LEVEL2) # character - change to factor
class(AllSchools.LIDAR.reduced$LEVEL3) # character - change to factor
class(AllSchools.LIDAR.reduced$LEVEL4) # character - change to factor
class(AllSchools.LIDAR.reduced$LEVEL5) # character - change to factor
class(AllSchools.LIDAR.reduced$LEVEL6) # character - change to factor
class(AllSchools.LIDAR.reduced$LEVEL7) # character - change to factor
class(AllSchools.LIDAR.reduced$LEVEL8) # character - change to factor
class(AllSchools.LIDAR.reduced$ASSOC1) # character - change to factor
class(AllSchools.LIDAR.reduced$APPLCN) # character - change to numeric
class(AllSchools.LIDAR.reduced$TUITION2) # character - change to numeric
class(AllSchools.LIDAR.reduced$SCH_TYPE_TEXT) # character - change to factor
class(AllSchools.LIDAR.reduced$LEVEL) # character - change to factor
class(AllSchools.LIDAR.reduced$CHARTER_TEXT) # character - change to factor
class(AllSchools.LIDAR.reduced$TOTFRL) # integer - change to numeric
class(AllSchools.LIDAR.reduced$level) # change name to PRLevel, integer - change to factor
class(AllSchools.LIDAR.reduced$males) # integer - change to numeric
class(AllSchools.LIDAR.reduced$orient) # integer - change to factor
class(AllSchools.LIDAR.reduced$p_asian) # numeric
class(AllSchools.LIDAR.reduced$p_indian) # numeric
class(AllSchools.LIDAR.reduced$p_pacific) # numeric
class(AllSchools.LIDAR.reduced$p_hisp) # numeric
class(AllSchools.LIDAR.reduced$p_black) # numeric
class(AllSchools.LIDAR.reduced$p_white) # numeric
class(AllSchools.LIDAR.reduced$relig) # integer - change to factor
class(AllSchools.LIDAR.reduced$s_kg) # integer - change to numeric
class(AllSchools.LIDAR.reduced$tothrs) # numeric
class(AllSchools.LIDAR.reduced$ucommtyp) # integer - change to factor


### Make classification changes, if necessary
# Change the characters and integers to numerics
AllSchools.LIDAR.reduced$PercentRural <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$PercentRural))
AllSchools.LIDAR.reduced$Poverty.Percent.All.Ages   <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$Poverty.Percent.All.Ages))
AllSchools.LIDAR.reduced$Median.Household.Income    <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$Median.Household.Income))
AllSchools.LIDAR.reduced$Below18     <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$Below18))
AllSchools.LIDAR.reduced$X18andOver  <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$X18andOver))
AllSchools.LIDAR.reduced$GovHealth   <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$GovHealth))
AllSchools.LIDAR.reduced$Physicians  <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$Physicians))
AllSchools.LIDAR.reduced$GovHigh     <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$GovHigh))
AllSchools.LIDAR.reduced$Farmland    <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$Farmland))
AllSchools.LIDAR.reduced$Cropland    <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$Cropland))
AllSchools.LIDAR.reduced$GovRev      <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$GovRev))
AllSchools.LIDAR.reduced$Employment  <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$Employment))
AllSchools.LIDAR.reduced$APPLCN      <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$APPLCN))
AllSchools.LIDAR.reduced$TUITION2    <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$TUITION2))
AllSchools.LIDAR.reduced$TOTFRL      <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$TOTFRL))
AllSchools.LIDAR.reduced$males       <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$males))
AllSchools.LIDAR.reduced$s_kg        <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$s_kg))
AllSchools.LIDAR.reduced$PGRNT_P      <- as.numeric(gsub(",","", AllSchools.LIDAR.reduced$PGRNT_P))

# Check class changes
class(AllSchools.LIDAR.reduced$PercentRural) 
class(AllSchools.LIDAR.reduced$Poverty.Percent.All.Ages) 
class(AllSchools.LIDAR.reduced$Median.Household.Income) 
class(AllSchools.LIDAR.reduced$Below18)
class(AllSchools.LIDAR.reduced$X18andOver)
class(AllSchools.LIDAR.reduced$GovHealth)
class(AllSchools.LIDAR.reduced$Physicians)
class(AllSchools.LIDAR.reduced$GovHigh)
class(AllSchools.LIDAR.reduced$Farmland)
class(AllSchools.LIDAR.reduced$Cropland)
class(AllSchools.LIDAR.reduced$GovRev)
class(AllSchools.LIDAR.reduced$Employment) 
class(AllSchools.LIDAR.reduced$APPLCN)
class(AllSchools.LIDAR.reduced$TUITION2)
class(AllSchools.LIDAR.reduced$TOTFRL)
class(AllSchools.LIDAR.reduced$males)
class(AllSchools.LIDAR.reduced$s_kg) 
class(AllSchools.LIDAR.reduced$PGRNT_P)

# Change characters to factors
AllSchools.LIDAR.reduced$SchoolType    <- as.factor(AllSchools.LIDAR.reduced$SchoolType)
AllSchools.LIDAR.reduced$LANDGRNT      <- as.factor(AllSchools.LIDAR.reduced$LANDGRNT)
AllSchools.LIDAR.reduced$PTC_EF        <- as.factor(AllSchools.LIDAR.reduced$PTC_EF)
AllSchools.LIDAR.reduced$RELAFFIL      <- as.factor(AllSchools.LIDAR.reduced$RELAFFIL)
AllSchools.LIDAR.reduced$LEVEL1        <- as.factor(AllSchools.LIDAR.reduced$LEVEL1)
AllSchools.LIDAR.reduced$LEVEL2        <- as.factor(AllSchools.LIDAR.reduced$LEVEL2)
AllSchools.LIDAR.reduced$LEVEL3        <- as.factor(AllSchools.LIDAR.reduced$LEVEL3)
AllSchools.LIDAR.reduced$LEVEL4        <- as.factor(AllSchools.LIDAR.reduced$LEVEL4)
AllSchools.LIDAR.reduced$LEVEL5        <- as.factor(AllSchools.LIDAR.reduced$LEVEL5)
AllSchools.LIDAR.reduced$LEVEL6        <- as.factor(AllSchools.LIDAR.reduced$LEVEL6)
AllSchools.LIDAR.reduced$LEVEL7        <- as.factor(AllSchools.LIDAR.reduced$LEVEL7)
AllSchools.LIDAR.reduced$LEVEL8        <- as.factor(AllSchools.LIDAR.reduced$LEVEL8)
AllSchools.LIDAR.reduced$ASSOC1        <- as.factor(AllSchools.LIDAR.reduced$ASSOC1)
AllSchools.LIDAR.reduced$SCH_TYPE_TEXT <- as.factor(AllSchools.LIDAR.reduced$SCH_TYPE_TEXT)
AllSchools.LIDAR.reduced$LEVEL         <- as.factor(AllSchools.LIDAR.reduced$LEVEL)
AllSchools.LIDAR.reduced$CHARTER_TEXT  <- as.factor(AllSchools.LIDAR.reduced$CHARTER_TEXT)
AllSchools.LIDAR.reduced$level         <- as.factor(AllSchools.LIDAR.reduced$level)
AllSchools.LIDAR.reduced$orient        <- as.factor(AllSchools.LIDAR.reduced$orient)
AllSchools.LIDAR.reduced$relig         <- as.factor(AllSchools.LIDAR.reduced$relig)
AllSchools.LIDAR.reduced$ucommtyp      <- as.factor(AllSchools.LIDAR.reduced$ucommtyp)

# Check class changes
class(AllSchools.LIDAR.reduced$SchoolType)
class(AllSchools.LIDAR.reduced$LANDGRNT) 
class(AllSchools.LIDAR.reduced$PTC_EF) 
class(AllSchools.LIDAR.reduced$RELAFFIL)
class(AllSchools.LIDAR.reduced$LEVEL1)
class(AllSchools.LIDAR.reduced$LEVEL2)
class(AllSchools.LIDAR.reduced$LEVEL3)
class(AllSchools.LIDAR.reduced$LEVEL4)
class(AllSchools.LIDAR.reduced$LEVEL5)
class(AllSchools.LIDAR.reduced$LEVEL6)
class(AllSchools.LIDAR.reduced$LEVEL7) 
class(AllSchools.LIDAR.reduced$LEVEL8)
class(AllSchools.LIDAR.reduced$ASSOC1)
class(AllSchools.LIDAR.reduced$SCH_TYPE_TEXT)
class(AllSchools.LIDAR.reduced$LEVEL)
class(AllSchools.LIDAR.reduced$CHARTER_TEXT)
class(AllSchools.LIDAR.reduced$level)
class(AllSchools.LIDAR.reduced$orient)
class(AllSchools.LIDAR.reduced$relig)
class(AllSchools.LIDAR.reduced$ucommtyp) 


###################################################
###################################################
# Summary statistics
###################################################
###################################################

# Check summary statistics for all IVs and DV - here, we find variables to remove if there isn't enough entries
# First, count number of schools in each category for which we have LIDAR
count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),], "SchoolType") # HE 3035, PR 9586, PU 26140
count(AllSchools.LIDAR.reduced, "SchoolType") # HE 7556, PR 26807, PU 99772
nrow(AllSchools.LIDAR.reduced) # 134,135

###############################################
### Summary statistics for continuous variables
###############################################

# School level data - for all schools
summary(AllSchools.LIDAR.reduced$EstimatedArea) 
head(count(AllSchools.LIDAR.reduced$EstimatedArea))
tail(count(AllSchools.LIDAR.reduced$EstimatedArea))
sum(!is.na(AllSchools.LIDAR.reduced$EstimatedArea)) # 35,733

summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "higher education"),]$EstimatedArea)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "higher education"),]$EstimatedArea)) # 2935

summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public"),]$EstimatedArea)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public"),]$EstimatedArea)) # 23,557

summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "private"),]$EstimatedArea)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "private"),]$EstimatedArea)) # 9,241



summary(AllSchools.LIDAR.reduced$population) 
head(count(AllSchools.LIDAR.reduced$population))
tail(count(AllSchools.LIDAR.reduced$population))
sum(!is.na(AllSchools.LIDAR.reduced$population)) # 129,183
# check total population for all schools
sum(AllSchools.LIDAR.reduced$population, na.rm = TRUE) # 74,953,758
sum(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),]$population, na.rm = TRUE) # 24,672,756
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$population == 0),]) # 210 schools have a population of 0
# Check to see if it's equal to the sum_pop
summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),]$sum_pop) # this multiplied by the pct_pop should arrive at a close approximation of population for each school
summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),]$population)

summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "higher education"),]$population)
summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public"),]$population)
summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "private"),]$population)


summary(AllSchools.LIDAR.reduced$teachers) 
head(count(AllSchools.LIDAR.reduced$teachers))
tail(count(AllSchools.LIDAR.reduced$teachers))
sum(!is.na(AllSchools.LIDAR.reduced$teachers)) # 119,881

summary(AllSchools.LIDAR.reduced$students) 
head(count(AllSchools.LIDAR.reduced$students))
tail(count(AllSchools.LIDAR.reduced$students))
sum(!is.na(AllSchools.LIDAR.reduced$students)) # 129,740

# County level data - for all schools
summary(AllSchools.LIDAR.reduced$Poverty.Percent.All.Ages) 
head(count(AllSchools.LIDAR.reduced$Poverty.Percent.All.Ages))
tail(count(AllSchools.LIDAR.reduced$Poverty.Percent.All.Ages))
sum(!is.na(AllSchools.LIDAR.reduced$Poverty.Percent.All.Ages)) # 133,769

summary(AllSchools.LIDAR.reduced$Median.Household.Income) 
head(count(AllSchools.LIDAR.reduced$Median.Household.Income))
tail(count(AllSchools.LIDAR.reduced$Median.Household.Income))
sum(!is.na(AllSchools.LIDAR.reduced$Median.Household.Income)) # 133,769

summary(AllSchools.LIDAR.reduced$PercentRural) 
head(count(AllSchools.LIDAR.reduced$PercentRural))
tail(count(AllSchools.LIDAR.reduced$PercentRural))
sum(!is.na(AllSchools.LIDAR.reduced$PercentRural)) # 133,784

summary(AllSchools.LIDAR.reduced$PopDensity) 
head(count(AllSchools.LIDAR.reduced$PopDensity))
tail(count(AllSchools.LIDAR.reduced$PopDensity))
sum(!is.na(AllSchools.LIDAR.reduced$PopDensity)) # 134,120

summary(AllSchools.LIDAR.reduced$HouseDensity) 
head(count(AllSchools.LIDAR.reduced$HouseDensity))
tail(count(AllSchools.LIDAR.reduced$HouseDensity))
sum(!is.na(AllSchools.LIDAR.reduced$HouseDensity)) # 134,120

summary(AllSchools.LIDAR.reduced$Below18) 
head(count(AllSchools.LIDAR.reduced$Below18))
tail(count(AllSchools.LIDAR.reduced$Below18))
sum(!is.na(AllSchools.LIDAR.reduced$Below18)) # 133,769

summary(AllSchools.LIDAR.reduced$X18andOver) 
head(count(AllSchools.LIDAR.reduced$X18andOver))
tail(count(AllSchools.LIDAR.reduced$X18andOver))
sum(!is.na(AllSchools.LIDAR.reduced$X18andOver)) # 133,769

summary(AllSchools.LIDAR.reduced$GovHealth) 
head(count(AllSchools.LIDAR.reduced$GovHealth))
tail(count(AllSchools.LIDAR.reduced$GovHealth))
sum(!is.na(AllSchools.LIDAR.reduced$GovHealth)) # 133,769

summary(AllSchools.LIDAR.reduced$Physicians) 
head(count(AllSchools.LIDAR.reduced$Physicians))
tail(count(AllSchools.LIDAR.reduced$Physicians))
sum(!is.na(AllSchools.LIDAR.reduced$Physicians)) # 133,769

summary(AllSchools.LIDAR.reduced$VehiclesHouse) 
head(count(AllSchools.LIDAR.reduced$VehiclesHouse))
tail(count(AllSchools.LIDAR.reduced$VehiclesHouse))
sum(!is.na(AllSchools.LIDAR.reduced$VehiclesHouse)) # 133,769

summary(AllSchools.LIDAR.reduced$GovHigh) 
head(count(AllSchools.LIDAR.reduced$GovHigh))
tail(count(AllSchools.LIDAR.reduced$GovHigh))
sum(!is.na(AllSchools.LIDAR.reduced$GovHigh)) # 133,769

summary(AllSchools.LIDAR.reduced$CountyArea) 
head(count(AllSchools.LIDAR.reduced$CountyArea))
tail(count(AllSchools.LIDAR.reduced$CountyArea))
sum(!is.na(AllSchools.LIDAR.reduced$CountyArea)) # 133,769

summary(AllSchools.LIDAR.reduced$WaterArea) 
head(count(AllSchools.LIDAR.reduced$WaterArea))
tail(count(AllSchools.LIDAR.reduced$WaterArea))
sum(!is.na(AllSchools.LIDAR.reduced$WaterArea)) # 133,769

summary(AllSchools.LIDAR.reduced$Farmland) 
head(count(AllSchools.LIDAR.reduced$Farmland))
tail(count(AllSchools.LIDAR.reduced$Farmland))
sum(!is.na(AllSchools.LIDAR.reduced$Farmland)) # 133,769

summary(AllSchools.LIDAR.reduced$Cropland) 
head(count(AllSchools.LIDAR.reduced$Cropland))
tail(count(AllSchools.LIDAR.reduced$Cropland))
sum(!is.na(AllSchools.LIDAR.reduced$Cropland)) # 133,769

summary(AllSchools.LIDAR.reduced$GovRev) 
head(count(AllSchools.LIDAR.reduced$GovRev))
tail(count(AllSchools.LIDAR.reduced$GovRev))
sum(!is.na(AllSchools.LIDAR.reduced$GovRev)) # 133,769

summary(AllSchools.LIDAR.reduced$Employment) 
head(count(AllSchools.LIDAR.reduced$Employment))
tail(count(AllSchools.LIDAR.reduced$Employment))
sum(!is.na(AllSchools.LIDAR.reduced$Employment)) # 133,769



# Higher education; we have LIDAR data for 3035; total 7556
summary(AllSchools.LIDAR.reduced$APPLCN) 
head(count(AllSchools.LIDAR.reduced$APPLCN))
tail(count(AllSchools.LIDAR.reduced$APPLCN))
sum(!is.na(AllSchools.LIDAR.reduced$APPLCN)) # 6,791
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$APPLCN)),]) # 2,635

summary(AllSchools.LIDAR.reduced$TUITION2) # cut this since it doesn't cover 75% of the LIDAR HE data
head(count(AllSchools.LIDAR.reduced$TUITION2))
tail(count(AllSchools.LIDAR.reduced$TUITION2))
sum(!is.na(AllSchools.LIDAR.reduced$TUITION2)) # 4320
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$TUITION2)),]) # 1626

summary(AllSchools.LIDAR.reduced$PGRNT_P) # cut this since it doesn't cover 75% of the LIDAR HE data
head(count(AllSchools.LIDAR.reduced$PGRNT_P))
tail(count(AllSchools.LIDAR.reduced$PGRNT_P))
sum(!is.na(AllSchools.LIDAR.reduced$PGRNT_P)) # 6384
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$PGRNT_P)),]) # 2424



# K12 Public; we have LIDAR data for 26140; total 99772
summary(AllSchools.LIDAR.reduced$TOTFRL) # cut this since it doesn't cover 75% of the LIDAR PU data
head(count(AllSchools.LIDAR.reduced$TOTFRL))
tail(count(AllSchools.LIDAR.reduced$TOTFRL))
# Need to change -1 to NA value (checked with companion file)
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$TOTFRL == -1),]$TOTFRL <- NA
summary(AllSchools.LIDAR.reduced$TOTFRL) 
head(count(AllSchools.LIDAR.reduced$TOTFRL))
tail(count(AllSchools.LIDAR.reduced$TOTFRL))
sum(!is.na(AllSchools.LIDAR.reduced$TOTFRL)) # 17,053
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$TOTFRL)),]) # 6222



# K12 Private; we have LIDAR data for 9586; total 26807
summary(AllSchools.LIDAR.reduced$males) 
head(count(AllSchools.LIDAR.reduced$males))
tail(count(AllSchools.LIDAR.reduced$males))
sum(!is.na(AllSchools.LIDAR.reduced$males)) # 26804
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$males)),]) # 9585

summary(AllSchools.LIDAR.reduced$p_asian) # Drop variables because they are highly correlated with others
head(count(AllSchools.LIDAR.reduced$p_asian))
tail(count(AllSchools.LIDAR.reduced$p_asian))
sum(!is.na(AllSchools.LIDAR.reduced$p_asian)) # 26804
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$p_asian)),]) # 9585

summary(AllSchools.LIDAR.reduced$p_indian) # Drop variables because they are highly correlated with others
head(count(AllSchools.LIDAR.reduced$p_indian))
tail(count(AllSchools.LIDAR.reduced$p_indian))
sum(!is.na(AllSchools.LIDAR.reduced$p_indian)) # 26804
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$p_indian)),]) # 9585

summary(AllSchools.LIDAR.reduced$p_pacific) # Drop variables because they are highly correlated with others
head(count(AllSchools.LIDAR.reduced$p_pacific))
tail(count(AllSchools.LIDAR.reduced$p_pacific))
sum(!is.na(AllSchools.LIDAR.reduced$p_pacific)) # 26804
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$p_pacific)),]) # 9585

summary(AllSchools.LIDAR.reduced$p_hisp) # Drop variables because they are highly correlated with others
head(count(AllSchools.LIDAR.reduced$p_hisp))
tail(count(AllSchools.LIDAR.reduced$p_hisp))
sum(!is.na(AllSchools.LIDAR.reduced$p_hisp)) # 26804
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$p_hisp)),]) # 9585

summary(AllSchools.LIDAR.reduced$p_black) # Drop variables because they are highly correlated with others
head(count(AllSchools.LIDAR.reduced$p_black))
tail(count(AllSchools.LIDAR.reduced$p_black))
sum(!is.na(AllSchools.LIDAR.reduced$p_black)) # 26804   
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$p_black)),]) # 9585

summary(AllSchools.LIDAR.reduced$p_white) 
head(count(AllSchools.LIDAR.reduced$p_white))
tail(count(AllSchools.LIDAR.reduced$p_white))
sum(!is.na(AllSchools.LIDAR.reduced$p_white)) # 26804  
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$p_white)),]) # 9585

summary(AllSchools.LIDAR.reduced$s_kg) 
head(count(AllSchools.LIDAR.reduced$s_kg))
tail(count(AllSchools.LIDAR.reduced$s_kg))
sum(!is.na(AllSchools.LIDAR.reduced$s_kg)) # 26804
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$s_kg)),]) # 9585

summary(AllSchools.LIDAR.reduced$tothrs) 
head(count(AllSchools.LIDAR.reduced$tothrs))
tail(count(AllSchools.LIDAR.reduced$tothrs))
sum(!is.na(AllSchools.LIDAR.reduced$tothrs)) # 26804
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$tothrs)),]) # 9585


###########################################
### Summary statistics for factor variables
###########################################

# School level data - for all schools
count(AllSchools.LIDAR.reduced$state) # 49 states (missing AK and HI - adding DC)
sum(!is.na(AllSchools.LIDAR.reduced$state)) # 134,137

count(AllSchools.LIDAR.reduced$SchoolType)
sum(!is.na(AllSchools.LIDAR.reduced$SchoolType)) # 134,135



# Higher education; we have LIDAR data for 3055; total 7556
count(AllSchools.LIDAR.reduced$LANDGRNT)
sum(!is.na(AllSchools.LIDAR.reduced$LANDGRNT)) # 6900
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$LANDGRNT)),]) # 2682

count(AllSchools.LIDAR.reduced$PTC_EF)
# checked these changes with the IPEDS201314TablesDoc
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$PTC_EF == "-9"),]$PTC_EF <- NA
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$PTC_EF == "-2"),]$PTC_EF <- NA
sum(!is.na(AllSchools.LIDAR.reduced$PTC_EF)) # 6192
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$PTC_EF)),]) # 2322

count(AllSchools.LIDAR.reduced$RELAFFIL) # cut this since it doesn't cover 75% of the LIDAR HE data
# Need to change -2 and #N/A to NA values; didn't specify in table whether these are NA but referred to previous table entries
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$RELAFFIL == "-2"),]$RELAFFIL <- NA
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$RELAFFIL == "#N/A"),]$RELAFFIL <- NA
sum(!is.na(AllSchools.LIDAR.reduced$RELAFFIL)) # 906
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$RELAFFIL)),]) # 353

count(AllSchools.LIDAR.reduced$LEVEL1)
# Need to change #N/A to NA values
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LEVEL1 == "#N/A"),]$LEVEL1 <- NA
sum(!is.na(AllSchools.LIDAR.reduced$LEVEL1)) # 6791
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$LEVEL1)),]) # 2635

count(AllSchools.LIDAR.reduced$LEVEL2)
# Need to change #N/A to NA values
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LEVEL2 == "#N/A"),]$LEVEL2 <- NA
sum(!is.na(AllSchools.LIDAR.reduced$LEVEL2)) # 6791
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$LEVEL2)),]) # 2635

count(AllSchools.LIDAR.reduced$LEVEL3)
# Need to change #N/A to NA values
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LEVEL3 == "#N/A"),]$LEVEL3 <- NA
sum(!is.na(AllSchools.LIDAR.reduced$LEVEL3)) # 6791
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$LEVEL3)),]) # 2635

count(AllSchools.LIDAR.reduced$LEVEL4)
# Need to change #N/A to NA values
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LEVEL4 == "#N/A"),]$LEVEL4 <- NA
sum(!is.na(AllSchools.LIDAR.reduced$LEVEL4)) # 6791
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$LEVEL4)),]) # 2635

count(AllSchools.LIDAR.reduced$LEVEL5)
# Need to change #N/A to NA values; didn't specify in table whether these are NA but referred to previous table entries
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LEVEL5 == "#N/A"),]$LEVEL5 <- NA
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LEVEL5 == "-2"),]$LEVEL5 <- NA
sum(!is.na(AllSchools.LIDAR.reduced$LEVEL5)) # 5058
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$LEVEL5)),]) # 1941

count(AllSchools.LIDAR.reduced$LEVEL6)
# Need to change #N/A to NA values; didn't specify in table whether these are NA but referred to previous table entries
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LEVEL6 == "#N/A"),]$LEVEL6 <- NA
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LEVEL6 == "-2"),]$LEVEL6 <- NA
sum(!is.na(AllSchools.LIDAR.reduced$LEVEL6)) # 5058
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$LEVEL6)),]) # 1941

count(AllSchools.LIDAR.reduced$LEVEL7)
# Need to change #N/A to NA values; didn't specify in table whether these are NA but referred to previous table entries
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LEVEL7 == "#N/A"),]$LEVEL7 <- NA
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LEVEL7 == "-2"),]$LEVEL7 <- NA
sum(!is.na(AllSchools.LIDAR.reduced$LEVEL7)) # 5058
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$LEVEL7)),]) # 1941

count(AllSchools.LIDAR.reduced$LEVEL8)
# Need to change #N/A to NA values; didn't specify in table whether these are NA but referred to previous table entries
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LEVEL8 == "#N/A"),]$LEVEL8 <- NA
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LEVEL8 == "-2"),]$LEVEL8 <- NA
sum(!is.na(AllSchools.LIDAR.reduced$LEVEL8)) # 5058
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$LEVEL8)),]) # 1941

count(AllSchools.LIDAR.reduced$ASSOC1) # cut this since it doesn't cover 75% of the LIDAR HE data
# Need to change #N/A to NA values; didn't specify in table whether these are NA but referred to previous table entries
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$ASSOC1 == "#N/A"),]$ASSOC1 <- NA
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$ASSOC1 == "-2"),]$ASSOC1 <- NA
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$ASSOC1 == "-1"),]$ASSOC1 <- NA
sum(!is.na(AllSchools.LIDAR.reduced$ASSOC1)) # 4404
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$ASSOC1)),]) # 1681



# K12 Public; we have LIDAR data for 25378; total 99774
count(AllSchools.LIDAR.reduced$SCH_TYPE_TEXT)
sum(!is.na(AllSchools.LIDAR.reduced$SCH_TYPE_TEXT)) # 99,334
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$SCH_TYPE_TEXT)),]) # 25,953

count(AllSchools.LIDAR.reduced$LEVEL)
sum(!is.na(AllSchools.LIDAR.reduced$LEVEL)) # 96,031
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$LEVEL)),]) # 24,913

count(AllSchools.LIDAR.reduced$CHARTER_TEXT)
sum(!is.na(AllSchools.LIDAR.reduced$CHARTER_TEXT)) # 92,388
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$CHARTER_TEXT)),]) # 24,954


# K12 Private; we have LIDAR data for 9372; total 26807
count(AllSchools.LIDAR.reduced$level)
sum(!is.na(AllSchools.LIDAR.reduced$level)) # 26,804
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$level)),]) # 9585

count(AllSchools.LIDAR.reduced$orient) # Drop variables because they are highly correlated with others
sum(!is.na(AllSchools.LIDAR.reduced$orient)) # 26,804
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$orient)),]) # 9585

count(AllSchools.LIDAR.reduced$relig)
sum(!is.na(AllSchools.LIDAR.reduced$relig)) # 26,804
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$relig)),]) # 9585

count(AllSchools.LIDAR.reduced$ucommtyp) # Drop variables because they are highly correlated with others
sum(!is.na(AllSchools.LIDAR.reduced$ucommtyp)) # 26,804
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$ucommtyp)),]) # 9585


## Drop variables because they are less that 75% of the LIDAR data
AllSchools.LIDAR.reduced$TUITION2   <- NULL
AllSchools.LIDAR.reduced$TOTFRL     <- NULL
AllSchools.LIDAR.reduced$RELAFFIL   <- NULL
AllSchools.LIDAR.reduced$ASSOC1     <- NULL

## Drop variables because they are highly correlated with others
AllSchools.LIDAR.reduced$p_asian    <- NULL
AllSchools.LIDAR.reduced$p_indian   <- NULL
AllSchools.LIDAR.reduced$p_hisp     <- NULL
AllSchools.LIDAR.reduced$p_pacific  <- NULL
AllSchools.LIDAR.reduced$p_black    <- NULL
AllSchools.LIDAR.reduced$orient     <- NULL
AllSchools.LIDAR.reduced$ucommtyp   <- NULL

## Create a new HE level variable
# 1 is <= Assoc. degree
# Assoc. degree < 2 <= bach degree
# Bach degree < 3 <= postbach
AllSchools.LIDAR.reduced$HELevel <- NA
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LEVEL1 == 1 | AllSchools.LIDAR.reduced$LEVEL2 == 1 | AllSchools.LIDAR.reduced$LEVEL3 == 1 | AllSchools.LIDAR.reduced$LEVEL4 == 1),]$HELevel <- 1
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LEVEL5 == 1),]$HELevel <- 2
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LEVEL6 == 1 | AllSchools.LIDAR.reduced$LEVEL7 == 1 | AllSchools.LIDAR.reduced$LEVEL8 == 1),]$HELevel <- 3
count(AllSchools.LIDAR.reduced$HELevel)
sum(!is.na(AllSchools.LIDAR.reduced$HELevel)) # 6756
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$HELevel)),]) # 2618
class(AllSchools.LIDAR.reduced$HELevel) # numeric -> change to factor
AllSchools.LIDAR.reduced$HELevel <- as.factor(AllSchools.LIDAR.reduced$HELevel)

## Treat population of 0 as "NA" for all of the schools
nrow(AllSchools.LIDAR.reduced[is.na(AllSchools.LIDAR.reduced$population),]) #4,953
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$population == 0),]) # 210 schools in total have a population of "0"
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$population == 0 & AllSchools.LIDAR.reduced$source_dataset.x == "higher education"),]) # 210
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$population == 0 & AllSchools.LIDAR.reduced$source_dataset.x == "public"),]) # 0
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$population == 0 & AllSchools.LIDAR.reduced$source_dataset.x == "private"),]) # 0

AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$population == 0),]$population <- NA

nrow(AllSchools.LIDAR.reduced[is.na(AllSchools.LIDAR.reduced$population),]) # 5162; we added 210 more NA values to the previous total of 4,953
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "higher education" & is.na(AllSchools.LIDAR.reduced$population)),]) # 210
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public" & is.na(AllSchools.LIDAR.reduced$population)),]) # 4952
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "private" & is.na(AllSchools.LIDAR.reduced$population)),]) # 0

# since public school population has values that are less than 1 but greater than 0, covert all values less than 1 to 1
summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public"),]$population)
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public" & AllSchools.LIDAR.reduced$population < 1),]) # 75
AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public" & AllSchools.LIDAR.reduced$population < 1),]$population <- 1
summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),]$population)

# should also check the break down of populations for LIDAR versus OSM linked schools
summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),]$population)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),]$population))
summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "private"),]$population)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "private" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),]$population))
summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "higher education"),]$population)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "higher education" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),]$population))

summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public" & AllSchools.LIDAR.reduced$match_type == "LiDAR"),]$population)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public" & AllSchools.LIDAR.reduced$match_type == "LiDAR"),]$population))
summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "private" & AllSchools.LIDAR.reduced$match_type == "LiDAR"),]$population)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "private" & AllSchools.LIDAR.reduced$match_type == "LiDAR"),]$population))
summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "higher education" & AllSchools.LIDAR.reduced$match_type == "LiDAR"),]$population)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "higher education" & AllSchools.LIDAR.reduced$match_type == "LiDAR"),]$population))

summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public" & AllSchools.LIDAR.reduced$match_type == "OSM"),]$population)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public" & AllSchools.LIDAR.reduced$match_type == "OSM"),]$population))
summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "private" & AllSchools.LIDAR.reduced$match_type == "OSM"),]$population)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "private" & AllSchools.LIDAR.reduced$match_type == "OSM"),]$population))
summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "higher education" & AllSchools.LIDAR.reduced$match_type == "OSM"),]$population)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "higher education" & AllSchools.LIDAR.reduced$match_type == "OSM"),]$population))




hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset == "public" & AllSchools.LIDAR.reduced$match_type == "LiDAR"),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Population", main = "K-12 Public: LiDAR match")
hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset == "public" & AllSchools.LIDAR.reduced$match_type == "OSM"),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Population", main = "K-12 Public: OSM match")
hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset == "public" & AllSchools.LIDAR.reduced$match_type == "LiDAR" & AllSchools.LIDAR.reduced$population < 4000),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,4000), ylim = c(0,1000),
     xlab = "Population", main = "K-12 Public: LiDAR match, < 4000")
hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset == "public" & AllSchools.LIDAR.reduced$match_type == "OSM" & AllSchools.LIDAR.reduced$population < 4000),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,4000), ylim = c(0,1000),
     xlab = "Population", main = "K-12 Public: OSM match, < 4000")


hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset == "private" & AllSchools.LIDAR.reduced$match_type == "LiDAR"),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Population", main = "K-12 Private: LiDAR match")
hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset == "private" & AllSchools.LIDAR.reduced$match_type == "OSM"),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Population", main = "K-12 Private: OSM match")
hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset == "private" & AllSchools.LIDAR.reduced$match_type == "LiDAR" & AllSchools.LIDAR.reduced$population < 2000),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,2000), ylim = c(0,1500),
     xlab = "Population", main = "K-12 Private: LiDAR match, < 2000")
hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset == "private" & AllSchools.LIDAR.reduced$match_type == "OSM" & AllSchools.LIDAR.reduced$population < 2000),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,2000), ylim = c(0,1500),
     xlab = "Population", main = "K-12 Private: OSM match, < 2000")


hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset == "higher education" & AllSchools.LIDAR.reduced$match_type == "LiDAR"),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Population", main = "Higher Education: LiDAR match")
hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset == "higher education" & AllSchools.LIDAR.reduced$match_type == "OSM"),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Population", main = "Higher Education: OSM match")
hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset == "higher education" & AllSchools.LIDAR.reduced$match_type == "LiDAR" & AllSchools.LIDAR.reduced$population < 30000),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,30000), ylim = c(0,300),
     xlab = "Population", main = "Higher Education: LiDAR match, < 30000")
hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset == "higher education" & AllSchools.LIDAR.reduced$match_type == "OSM" & AllSchools.LIDAR.reduced$population < 30000),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,30000), ylim = c(0,300),
     xlab = "Population", main = "Higher Education: OSM match, < 30000")


###############################################
### Redo summary statistics after changes #####
###############################################

#############################################
# Summary statistics for continuous variables
#############################################

# School level data - for all schools
summary(AllSchools.LIDAR.reduced$EstimatedArea) 
head(count(AllSchools.LIDAR.reduced$EstimatedArea))
tail(count(AllSchools.LIDAR.reduced$EstimatedArea))
sum(!is.na(AllSchools.LIDAR.reduced$EstimatedArea)) # 35,733

summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "higher education"),]$EstimatedArea)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "higher education"),]$EstimatedArea)) # 2935

summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public"),]$EstimatedArea)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "public"),]$EstimatedArea)) # 23,557

summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "private"),]$EstimatedArea)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$source_dataset.x == "private"),]$EstimatedArea)) # 9,241


# School level data - for all schools
count(AllSchools.LIDAR.reduced$state) # 49 states (missing AK and HI - adding DC)
count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),]$state) # 47 states (4 out of 51 territories missing: AK, HI, SD, TN)
sum(!is.na(AllSchools.LIDAR.reduced$state)) # 134,137

# County level data - for all schools
summary(AllSchools.LIDAR.reduced$PercentRural) 
head(count(AllSchools.LIDAR.reduced$PercentRural))
tail(count(AllSchools.LIDAR.reduced$PercentRural))
sum(!is.na(AllSchools.LIDAR.reduced$PercentRural)) # 133,784

summary(AllSchools.LIDAR.reduced$Median.Household.Income) 
head(count(AllSchools.LIDAR.reduced$Median.Household.Income))
tail(count(AllSchools.LIDAR.reduced$Median.Household.Income))
sum(!is.na(AllSchools.LIDAR.reduced$Median.Household.Income)) # 133,769

summary(AllSchools.LIDAR.reduced$HouseDensity) 
head(count(AllSchools.LIDAR.reduced$HouseDensity))
tail(count(AllSchools.LIDAR.reduced$HouseDensity))
sum(!is.na(AllSchools.LIDAR.reduced$HouseDensity)) # 134,120

summary(AllSchools.LIDAR.reduced$Poverty.Percent.All.Ages) 
head(count(AllSchools.LIDAR.reduced$Poverty.Percent.All.Ages))
tail(count(AllSchools.LIDAR.reduced$Poverty.Percent.All.Ages))
sum(!is.na(AllSchools.LIDAR.reduced$Poverty.Percent.All.Ages)) # 133,769

summary(AllSchools.LIDAR.reduced$PopDensity) 
head(count(AllSchools.LIDAR.reduced$PopDensity))
tail(count(AllSchools.LIDAR.reduced$PopDensity))
sum(!is.na(AllSchools.LIDAR.reduced$PopDensity)) # 134,120

summary(AllSchools.LIDAR.reduced$Below18) 
head(count(AllSchools.LIDAR.reduced$Below18))
tail(count(AllSchools.LIDAR.reduced$Below18))
sum(!is.na(AllSchools.LIDAR.reduced$Below18)) # 133,769

summary(AllSchools.LIDAR.reduced$X18andOver) 
head(count(AllSchools.LIDAR.reduced$X18andOver))
tail(count(AllSchools.LIDAR.reduced$X18andOver))
sum(!is.na(AllSchools.LIDAR.reduced$X18andOver)) # 133,769

summary(AllSchools.LIDAR.reduced$GovHealth) 
head(count(AllSchools.LIDAR.reduced$GovHealth))
tail(count(AllSchools.LIDAR.reduced$GovHealth))
sum(!is.na(AllSchools.LIDAR.reduced$GovHealth)) # 133,769

summary(AllSchools.LIDAR.reduced$Physicians) 
head(count(AllSchools.LIDAR.reduced$Physicians))
tail(count(AllSchools.LIDAR.reduced$Physicians))
sum(!is.na(AllSchools.LIDAR.reduced$Physicians)) # 133,769

summary(AllSchools.LIDAR.reduced$VehiclesHouse) 
head(count(AllSchools.LIDAR.reduced$VehiclesHouse))
tail(count(AllSchools.LIDAR.reduced$VehiclesHouse))
sum(!is.na(AllSchools.LIDAR.reduced$VehiclesHouse)) # 133,769

summary(AllSchools.LIDAR.reduced$GovHigh) 
head(count(AllSchools.LIDAR.reduced$GovHigh))
tail(count(AllSchools.LIDAR.reduced$GovHigh))
sum(!is.na(AllSchools.LIDAR.reduced$GovHigh)) # 133,769

summary(AllSchools.LIDAR.reduced$CountyArea) 
head(count(AllSchools.LIDAR.reduced$CountyArea))
tail(count(AllSchools.LIDAR.reduced$CountyArea))
sum(!is.na(AllSchools.LIDAR.reduced$CountyArea)) # 133,769

summary(AllSchools.LIDAR.reduced$WaterArea) 
head(count(AllSchools.LIDAR.reduced$WaterArea))
tail(count(AllSchools.LIDAR.reduced$WaterArea))
sum(!is.na(AllSchools.LIDAR.reduced$WaterArea)) # 133,769

summary(AllSchools.LIDAR.reduced$Farmland) 
head(count(AllSchools.LIDAR.reduced$Farmland))
tail(count(AllSchools.LIDAR.reduced$Farmland))
sum(!is.na(AllSchools.LIDAR.reduced$Farmland)) # 133,769

summary(AllSchools.LIDAR.reduced$GovRev) 
head(count(AllSchools.LIDAR.reduced$GovRev))
tail(count(AllSchools.LIDAR.reduced$GovRev))
sum(!is.na(AllSchools.LIDAR.reduced$GovRev)) # 133,769

summary(AllSchools.LIDAR.reduced$Employment) 
head(count(AllSchools.LIDAR.reduced$Employment))
tail(count(AllSchools.LIDAR.reduced$Employment))
sum(!is.na(AllSchools.LIDAR.reduced$Employment)) # 133,769

summary(AllSchools.LIDAR.reduced$population) 
head(count(AllSchools.LIDAR.reduced$population))
tail(count(AllSchools.LIDAR.reduced$population))
sum(!is.na(AllSchools.LIDAR.reduced$population)) # 128,973



# K12 Public
count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),]$SCH_TYPE_TEXT)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),]$SCH_TYPE_TEXT)) # 99,318

count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),]$LEVEL)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),]$LEVEL)) # 99,318
count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),]$LEVEL)
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$LEVEL)),]) # 24,913

count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),]$CHARTER_TEXT)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),]$CHARTER_TEXT)) # 99,318
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$CHARTER_TEXT)),]) # 24,954


# K12 Private
count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]$level)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]$level)) # 26,804

count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]$relig)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]$relig)) # 26,804
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & !is.na(AllSchools.LIDAR.reduced$relig)),]) # 9585

summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]$males) 
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]$males)) # 26804

summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]$tothrs) 
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]$tothrs)) # 26804

summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]$p_white) 
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]$p_white)) # 26804  


# Higher education
count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),]$HELevel)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),]$HELevel)) # 6746

count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),]$LANDGRNT)
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),]$LANDGRNT)) # 6900

summary(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),]$APPLCN) 
sum(!is.na(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),]$APPLCN)) # 6,781

########################################################################
## make quick histograms and plots for an idea of what to convert to log
########################################################################

hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,400000),
     xlab = "Roof Space (SF)", main = "Higher Education: Estimated space from LIDAR")

hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,400000),
     xlab = "Roof Space (SF)", main = "K12 Public: Estimated space from LIDAR")

hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), xlim = c(0,400000),
     xlab = "Roof Space (SF)", main = "K12 Private: Estimated space from LIDAR")

hist(AllSchools.LIDAR.reduced$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "population", main = "Population")

hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$population < 5000),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "population", main = "Population, < 5000")

# take log
hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "population", main = "Higher Education: population")

hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "population", main = "K12 Public: population")

hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),]$population, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "population", main = "K12 Private: population")

# take log
hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),]$teachers, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "teachers", main = "Higher Education: teachers")

hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]$teachers, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "teachers", main = "K12 Public: teachers")

hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),]$teachers, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "teachers", main = "K12 Private: teachers")

# take log
hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),]$students, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "students", main = "Higher Education: students")

hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]$students, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "students", main = "K12 Public: students")

hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),]$students, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "students", main = "K12 Private: students")

# might log this
hist(AllSchools.LIDAR.reduced$PercentRural, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "PercentRural", main = "PercentRural")

hist(AllSchools.LIDAR.reduced$Poverty.Percent.All.Ages, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "Poverty.Percent.All.Ages", main = "Poverty.Percent.All.Ages")

# might log
hist(AllSchools.LIDAR.reduced$Median.Household.Income, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "Median.Household.Income", main = "Median.Household.Income")

# log this
hist(AllSchools.LIDAR.reduced$HouseDensity, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "HouseDensity", main = "HouseDensity")


# log this
hist(AllSchools.LIDAR.reduced$PopDensity, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "PopDensity", main = "PopDensity")


# log this
hist(AllSchools.LIDAR.reduced$Below18, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "Below18", main = "Below18")

# log this
hist(AllSchools.LIDAR.reduced$X18andOver, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "X18andOver", main = "X18andOver")

# log this
hist(AllSchools.LIDAR.reduced$GovHealth, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "GovHealth", main = "GovHealth")

# log this
hist(AllSchools.LIDAR.reduced$Physicians, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "Physicians", main = "Physicians")

hist(AllSchools.LIDAR.reduced$VehiclesHouse, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "VehiclesHouse", main = "VehiclesHouse")

# log this
hist(AllSchools.LIDAR.reduced$GovHigh, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "GovHigh", main = "GovHigh")

# log this
hist(AllSchools.LIDAR.reduced$CountyArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "CountyArea", main = "CountyArea")

# log this
hist(AllSchools.LIDAR.reduced$WaterArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "WaterArea", main = "WaterArea")

# log this
hist(AllSchools.LIDAR.reduced$Farmland, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "Farmland", main = "Farmland")

# log this
hist(AllSchools.LIDAR.reduced$Cropland, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "Cropland", main = "Cropland")

hist(AllSchools.LIDAR.reduced$GovRev, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "GovRev", main = "GovRev")

# log this
hist(AllSchools.LIDAR.reduced$Employment, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "Employment", main = "Employment")

hist(AllSchools.LIDAR.reduced$PGRNT_P, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "PGRNT_P", main = "PGRNT_P")


op <- par(mar=c(12,4,4,2), cex = 0.8)
plot(AllSchools.LIDAR.reduced$SCH_TYPE_TEXT, las = 2, main = "K12 Public - School Type") 
dev.off()


plot(AllSchools.LIDAR.reduced$LEVEL, las = 2, main = "K12 Public - School Level") 
plot(AllSchools.LIDAR.reduced$CHARTER_TEXT, main = "K12 Public - Charter") 
plot(AllSchools.LIDAR.reduced$level, main = "K12 Private - School Level") 
plot(AllSchools.LIDAR.reduced$relig, main = "K12 Private - Religious Affiliation") 

# log this
hist(AllSchools.LIDAR.reduced$males, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "males", main = "K12 Private - No. of males")

hist(AllSchools.LIDAR.reduced$s_kg, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "s_kg", main = "K12 Private - No. of Kingergarteners")

hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$s_kg < 100),]$s_kg, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "s_kg", main = "K12 Private - No. of Kingergarteners, < 100")

hist(AllSchools.LIDAR.reduced$tothrs, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "tothrs", main = "K12 Private - Total Hours")

hist(AllSchools.LIDAR.reduced$p_white, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5), 
     xlab = "p_white", main = "K12 Private - Percent Caucasian")

plot(AllSchools.LIDAR.reduced$HELevel, main = "Higher Education - School Level") 

plot(AllSchools.LIDAR.reduced$LANDGRNT, main = "Higher Education - Land Grant") 

# log this
hist(AllSchools.LIDAR.reduced$APPLCN, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),  
     xlab = "APPLCN", xlim = c(0,500), main = "Higher Education - Applications")
dev.off()

bins <- seq(0,500,25)
hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$APPLCN < 500),]$APPLCN, 
     col=rgb(1,0,0,0.5), breaks = bins, border =rgb(1,0,0,0.5),  
     xlab = "APPLCN", main = "Higher Education - Applications, < 500")

plot(AllSchools.LIDAR.reduced$state) 
plot(AllSchools.LIDAR.reduced$SchoolType) 
plot(AllSchools.LIDAR.reduced$PTC_EF) 


#################################
# make log-normal transformations
#################################

AllSchools.LIDAR.reduced$lnMedian.Household.Income <- log(AllSchools.LIDAR.reduced$Median.Household.Income + 1)
AllSchools.LIDAR.reduced$lnpopulation              <- log(AllSchools.LIDAR.reduced$population + 1)
AllSchools.LIDAR.reduced$lnEstimatedArea           <- log(AllSchools.LIDAR.reduced$EstimatedArea + 1)
AllSchools.LIDAR.reduced$lnPopDensity              <- log(AllSchools.LIDAR.reduced$PopDensity + 1)
AllSchools.LIDAR.reduced$lnHouseDensity            <- log(AllSchools.LIDAR.reduced$HouseDensity + 1)
AllSchools.LIDAR.reduced$lnBelow18                 <- log(AllSchools.LIDAR.reduced$Below18 + 1)
AllSchools.LIDAR.reduced$lnX18andOver              <- log(AllSchools.LIDAR.reduced$X18andOver + 1)
AllSchools.LIDAR.reduced$lnGovHealth               <- log(AllSchools.LIDAR.reduced$GovHealth  + 1)
AllSchools.LIDAR.reduced$lnPhysicians              <- log(AllSchools.LIDAR.reduced$Physicians + 1)
AllSchools.LIDAR.reduced$lnGovHigh                 <- log(AllSchools.LIDAR.reduced$GovHigh + 1)
AllSchools.LIDAR.reduced$lnCountyArea              <- log(AllSchools.LIDAR.reduced$CountyArea + 1)
AllSchools.LIDAR.reduced$lnWaterArea               <- log(AllSchools.LIDAR.reduced$WaterArea  + 1)
AllSchools.LIDAR.reduced$lnFarmland                <- log(AllSchools.LIDAR.reduced$Farmland  + 1)
AllSchools.LIDAR.reduced$lnCropland                <- log(AllSchools.LIDAR.reduced$Cropland+ 1)
AllSchools.LIDAR.reduced$lnEmployment              <- log(AllSchools.LIDAR.reduced$Employment + 1)
AllSchools.LIDAR.reduced$lnAPPLCN                  <- log(AllSchools.LIDAR.reduced$APPLCN  + 1)
AllSchools.LIDAR.reduced$lnteachers                <- log(AllSchools.LIDAR.reduced$teachers + 1)
AllSchools.LIDAR.reduced$lnstudents                <- log(AllSchools.LIDAR.reduced$students  + 1)
AllSchools.LIDAR.reduced$lnPercentRural            <- log(AllSchools.LIDAR.reduced$PercentRural + 1)
AllSchools.LIDAR.reduced$lnmales                   <- log(AllSchools.LIDAR.reduced$males  + 1)


# Check the number of times pct_school_pop is blank and devp_sum_slopearea_sqft is not NA
nrow(LIDAR.new[which(is.na(AllSchools.LIDAR.reduced$pct_school_pop) & !is.na(AllSchools.LIDAR.reduced$devp_sum_slopearea_sqft) & !duplicated(AllSchools.LIDAR.reduced[,"JoinID"])),]) # 1,431 or 4% of the data 
nrow(LIDAR.new[which(is.na(AllSchools.LIDAR.reduced$pct_school_pop) & !is.na(AllSchools.LIDAR.reduced$devp_sum_slopearea_sqft)),]) # 1,431 or 4% of the data 
nrow(LIDAR.new[which(is.na(AllSchools.LIDAR.reduced$EstimatedArea) & !is.na(AllSchools.LIDAR.reduced$devp_sum_slopearea_sqft)),]) # 1,431 or 4% of the data 

## Reality checks
sum(AllSchools.LIDAR.reduced$EstimatedCapacity, na.rm = TRUE)/(10^6) # 13.74 GW -> compared to 1,118 GW for total US potential (NREL)
sum(AllSchools.LIDAR.reduced$EstimatedGeneration, na.rm = TRUE)/(10^9) # 19.91 Twh/yr -> compared to 1,432 TWh/yr for total US potential (NREL)
sum(AllSchools.LIDAR.reduced$EstimatedArea, na.rm = TRUE)*(.3048^2)/(10^9) # 0.09 Billion m2 -> compared to 8.12 billion m2 for total US potential (NREL)


# Determine means for plotting purposes
AllSchools.LIDAR.reduced$EstimatedArea.mu <- mean(AllSchools.LIDAR.reduced$EstimatedArea, na.rm = TRUE)


# Make scatter plots of continuous variables - now that we have made some transformations
plot(AllSchools.LIDAR.reduced$PercentRural, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "Percent Rural (%)", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$Median.Household.Income, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "Median Household Income ($)", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$HouseDensity, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "Household Density (#/SQM)", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$Poverty.Percent.All.Ages, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "Percent Poverty (%)", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$Below18, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "Below 18 (#)", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$X18andOver, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "18 & Above (#)", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$GovHealth, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "Gov. Health Expenditures (TH$)", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$Physicians, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "No. Physicians (#)", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$VehiclesHouse, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "Vehicles per Household", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$GovHigh, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "Gov. Highway Expenditures (TH$)", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$CountyArea, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "County Area (SQM)", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$WaterArea, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "Water Area (SQM)", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$Farmland, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "Farm Land (SQM)", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$GovRev, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "Gov. Revenue ($)", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$population, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "Population (students + teachers)", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$males, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "K12 Private - No. of Males", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$s_kg, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "K12 Private - No. of Kindergarteners", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$tothrs, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "K12 Private - Total Hours", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$p_white, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "K12 Private - Percent Caucasian", ylab = "Estimated Area (SF)")

plot(AllSchools.LIDAR.reduced$APPLCN, AllSchools.LIDAR.reduced$EstimatedArea,
     xlab = "Higher Education - Applications", ylab = "Estimated Area (SF)")

# Make a correlation table of continuous variables
install.packages("Hmisc")
library(Hmisc)


################################################
################################################
### Check correlation of possible regressors ###
################################################
################################################

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

correlation <- rcorr(as.matrix(AllSchools.LIDAR.reduced[,c("EstimatedArea",
                                                           "PercentRural",
                                                           "Median.Household.Income",
                                                           "HouseDensity",
                                                           "Poverty.Percent.All.Ages",
                                                           "PopDensity",
                                                           "Below18",
                                                           "X18andOver",
                                                           "GovHealth",
                                                           "Physicians",
                                                           "VehiclesHouse",
                                                           "GovHigh",
                                                           "CountyArea",
                                                           "WaterArea",
                                                           "Farmland",
                                                           "GovRev",
                                                           "Employment",
                                                           "population",
                                                           "males",
                                                           "s_kg",
                                                           "tothrs",
                                                           "p_white",
                                                           "APPLCN")]), type = "pearson")
flattenCorrMatrix(correlation$r, correlation$P)


cor(as.matrix(AllSchools.LIDAR.reduced[,c("EstimatedArea",
                                          "PercentRural",
                                          "Median.Household.Income",
                                          "HouseDensity",
                                          "Poverty.Percent.All.Ages",
                                          "PopDensity",
                                          "Below18",
                                          "X18andOver",
                                          "GovHealth",
                                          "Physicians",
                                          "VehiclesHouse",
                                          "GovHigh",
                                          "CountyArea",
                                          "WaterArea",
                                          "Farmland",
                                          "GovRev",
                                          "Employment",
                                          "population",
                                          "males",
                                          "s_kg",
                                          "tothrs",
                                          "p_white",
                                          "APPLCN")]))


###################################################
###################################################
# Begin EDA report plots and summaries
###################################################
###################################################

# Plot school populations against the measured roof space
# First, look at the K12Public Schools
m <- ggplot(data = AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),], aes(x = population, y = devp_sum_slopearea_sqft))
m + geom_point(colour = "forestgreen") +
  labs(title= "K12 Public LIDAR Measurements", x = "Population", y = "LIDAR Roof Space (SF)") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme(title= element_text(hjust = 0.5, vjust = 1, face= c("bold"))) 


# Next, look at the K12Private Schools
m <- ggplot(data = AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),], aes(x = population, y = devp_sum_slopearea_sqft))
m + geom_point(colour = "blue") +
  labs(title= "K12 Private LIDAR Measurements", x = "Population", y = "LIDAR Roof Space (SF)") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme(title= element_text(hjust = 0.5, vjust = 1, face= c("bold"))) 


# Next, look at the Higher Education
m <- ggplot(data = AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),], aes(x = population, y = devp_sum_slopearea_sqft))
m + geom_point(colour = "firebrick") +
  labs(title= "Higher Education LIDAR Measurements", x = "Population", y = "LIDAR Roof Space (SF)") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme(title= element_text(hjust = 0.5, vjust = 1, face= c("bold"))) 


# Frequency plot of total roof size for each school type
m <- ggplot(data = AllSchools.LIDAR.reduced, aes(EstimatedArea, colour = SchoolType))
m + geom_freqpoly(size = 1.5) +
  scale_colour_manual(values = c("firebrick","blue","forestgreen")) +
  labs(title= "LIDAR Available Roof Space (SF)", x = "Available Roof Space (SF)") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme(title= element_text(hjust = 0.5, vjust = 1, face= c("bold"))) 

# Frequency plot of total capacity for each school type
m <- ggplot(data = AllSchools.LIDAR.reduced, aes(EstimatedCapacity, colour = SchoolType))
m + geom_freqpoly(size = 1.5) +
  scale_colour_manual(values = c("firebrick","blue","forestgreen")) +
  labs(title= "NREL Estimated Capacity", x = "Solar Capacity (kW)") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme(title= element_text(hjust = 0.5, vjust = 1, face= c("bold")))

# Frequency plot of total annual energy output for each school type
m <- ggplot(data = AllSchools.LIDAR.reduced, aes(EstimatedGeneration, colour = SchoolType))
m + geom_freqpoly(size = 1.5) +
  scale_colour_manual(values = c("firebrick","blue","forestgreen")) +
  labs(title= "NREL Estimated Annual Generation", x = "Annual Electricity Generation (kWh)") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme(title= element_text(hjust = 0.5, vjust = 1, face= c("bold"))) 


###################################################
###################################################
# Map schools
# Red dots indicate no LIDAR data
# Blude dots indicate LIDAR data
###################################################
###################################################

# Figure 2
# Plot all schools and organize by type - THIS PLOT IN PAPER AND PRESENTATION
m <- ggplot(data=statesDF, aes(x=long, y=lat)) + geom_path(color = "gray10", aes(group = group)) 

# Plot all schools and organize by school type
AllSchools.LIDAR.reduced$SchoolType <- factor(AllSchools.LIDAR.reduced$SchoolType, levels = c("K12Public","K12Private","HigherEducation"))

m +  geom_point(data = AllSchools.LIDAR.reduced, aes(x = longitude, y = latitude, color=SchoolType, size=SchoolType)) +
  scale_color_manual(values=c('forestgreen','blue', 'firebrick'))+
  scale_size_manual(values=c(.8,.8,1))+
  geom_point(data=AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),], aes(x=longitude, y=latitude), size = 0.5, shape = 18, colour = "black") +
  theme(panel.background = element_rect(fill="white")) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position="none")


# No legend: Plot all schools and organize by school type
m + geom_point(data=AllSchools.LIDAR.reduced, aes(x=longitude, y=latitude, colour = SchoolType), size = .2) +
  geom_point(data=AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),], aes(x=longitude, y=latitude), size = 0.5, shape = 18) +
  scale_colour_manual(values = c("firebrick","blue","forestgreen","black")) +
  theme(title= element_text(hjust = 0.5, vjust = 1, face= c("bold"))) + 
  theme(panel.background = element_rect(fill="white")) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = "none")


# Plot all schools and organize by if we have LIDAR data
# First, plot K12Public
m + geom_point(data=AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),], aes(x=longitude, y=latitude, colour = LIDAR), size = 1) +
  scale_colour_manual(values = c("firebrick","dimgray")) +
  labs(title= "K12 Public Schools") +
  theme(title= element_text(hjust = 0.5, vjust = 1, face= c("bold"))) + 
  theme(panel.background = element_rect(fill="white")) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.key = element_rect(colour = NA, fill = NA))

# Next, plot K12Private
m + geom_point(data=AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),], aes(x=longitude, y=latitude, colour = LIDAR), size = 1) +
  scale_colour_manual(values = c("firebrick","dimgray")) +
  labs(title= "K12 Private Schools") +
  theme(title= element_text(hjust = 0.5, vjust = 1, face= c("bold"))) + 
  theme(panel.background = element_rect(fill="white")) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.key = element_rect(colour = NA, fill = NA))

# Next, plot Higher Education
m + geom_point(data=AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),], aes(x=longitude, y=latitude, colour = LIDAR), size = 1) +
  scale_colour_manual(values = c("firebrick","dimgray")) +
  labs(title= "Higher Education") +
  theme(title= element_text(hjust = 0.5, vjust = 1, face= c("bold"))) + 
  theme(panel.background = element_rect(fill="white")) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.key = element_rect(colour = NA, fill = NA))


# Histogram of total available space estimated by LIDAR - all schools
m <- ggplot(data = AllSchools.LIDAR.reduced, aes(EstimatedArea))
m + geom_area(stat = "bin", color = "firebrick", fill = "firebrick1") +
  labs(title= "Total Available Space Estimated by LIDAR", x = "Estimated Roof Space (SF)") +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  theme(axis.text=element_text(size = 12), axis.title=element_text(size = 14, face="bold")) +
  theme(title= element_text(size = 14, hjust = 0.5, vjust = 1, face= c("bold"))) +
  geom_vline(data=AllSchools.LIDAR.reduced, aes(xintercept=EstimatedArea.mu),
             linetype="dashed")

# Histogram of total available space estimated by LIDAR - all schools < 200,000 SF
options(scipen=999)
hist(AllSchools.LIDAR.reduced$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Roof Space (SF)", main = "All Schools: Estimated space from LIDAR")
histinfo <- hist(AllSchools.LIDAR.reduced$EstimatedArea, 
                 col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
                 xlab = "Roof Space (SF)", main = "All Schools: Estimated space from LIDAR")
hist(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$EstimatedArea < 200000),]$EstimatedArea, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Roof Space (SF)", main = "All Schools: Estimated space from LIDAR (< 200,000 SF)")

######################################################################
####### Do some counts
######################################################################

nrow(AllSchools.LIDAR.reduced) # 134,135
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),]) # 38,761
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),])/nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),]) # 40%
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced$SchoolType == "K12Public"),])/nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),]) # 26%
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced$SchoolType == "K12Private"),])/nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]) # 36%

nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$match_type == "OSM"),]) # 16,440
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$match_type == "OSM" & AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),])/nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),]) # 10%
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$match_type == "OSM" & AllSchools.LIDAR.reduced$SchoolType == "K12Public"),])/nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),]) # 13%
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$match_type == "OSM" & AllSchools.LIDAR.reduced$SchoolType == "K12Private"),])/nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]) # 10%

######################################################################
####### Break datasets up - SD and TN go in one datset for predictions
######################################################################

count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),],"state") # Missing TN and SD
count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$match_type == "OSM"),],"state") # Missing TN and SD

count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),],"state") # Missing TN and SD
count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced$SchoolType == "K12Public"),],"state") # Missing TN and SD
count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced$SchoolType == "K12Private"),],"state") # Missing TN and SD

# check that we have TN and SD in non-lidar schools
count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),],"state") # doesn't include AK or HI, but does have DC
count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),],"state") # doesn't include AK or HI, but does have DC
count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),],"state") # # doesn't include AK or HI, but does have DC

AllSchools.LIDAR.reduced.TNSD   <- AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$state == "TN" | AllSchools.LIDAR.reduced$state == "SD"),]
AllSchools.LIDAR.reduced.others <- AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$state != "TN" & AllSchools.LIDAR.reduced$state != "SD"),]

count(AllSchools.LIDAR.reduced,"state")
count(AllSchools.LIDAR.reduced.TNSD,"state")
count(AllSchools.LIDAR.reduced.others,"state")

######################################################################
####### Break datasets up - OSM vs LIDAR data
######################################################################

# check that we have TN and SD in non-lidar schools
count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$match_type == "LiDAR"),],"SchoolType") # HE = 2284, K12 Private = 6891, K12 Public = 13,146
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$match_type == "LiDAR"),]) # 22,321
count(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$match_type == "OSM"),],"SchoolType") # HE = 751, K12 Private = 2695, K12 Public = 12,994
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$match_type == "OSM"),]) # 16,440

AllSchools.LIDAR.LIDAR.TNSD   <- AllSchools.LIDAR.reduced.TNSD[which(AllSchools.LIDAR.reduced.TNSD$match_type == "LiDAR"),]
AllSchools.LIDAR.LIDAR.others <- AllSchools.LIDAR.reduced.others[which(AllSchools.LIDAR.reduced.others$match_type == "LiDAR"),]
AllSchools.LIDAR.OSM.TNSD     <- AllSchools.LIDAR.reduced.TNSD[which(AllSchools.LIDAR.reduced.TNSD$match_type == "OSM"),]
AllSchools.LIDAR.OSM.others   <- AllSchools.LIDAR.reduced.others[which(AllSchools.LIDAR.reduced.others$match_type == "OSM"),]

nrow(AllSchools.LIDAR.LIDAR.TNSD)
nrow(AllSchools.LIDAR.LIDAR.others) # 22321
nrow(AllSchools.LIDAR.OSM.TNSD)
nrow(AllSchools.LIDAR.OSM.others) # 16440

######################################
####### Split data for model selection
######################################

# First, count how many OSM data points we have
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$match_type == "OSM"),]) # 16,440 or 12% of all school data
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$match_type == "LiDAR"),]) # 22,321 or 17% of all school data
nrow(AllSchools.LIDAR.reduced) # 134,135

# Next, separate the OSM from LIDAR data
AllSchools.LIDAR.reduced.LiDAR <- AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$match_type == "LiDAR"),]
AllSchools.LIDAR.reduced.OSM <- AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$match_type == "OSM"),]
nrow(AllSchools.LIDAR.reduced.LiDAR) # 22,321
nrow(AllSchools.LIDAR.reduced.OSM) # 16,440

fold.labels.LiDAR <- sample(rep(1:5, length.out=nrow(AllSchools.LIDAR.reduced.LiDAR)))
AllSchools.LIDAR.reduced.LiDAR$fold <- fold.labels.LiDAR
fold.labels.OSM <- sample(rep(1:5, length.out=nrow(AllSchools.LIDAR.reduced.OSM)))
AllSchools.LIDAR.reduced.OSM$fold <- fold.labels.OSM
fold.labels.ALL <- sample(rep(1:5, length.out=nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),])))
AllSchools.LIDAR.reduced.AllLIDAR <- AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),]
AllSchools.LIDAR.reduced.AllLIDAR$fold <- fold.labels.ALL

LiDAR.first.20percent <- AllSchools.LIDAR.reduced.LiDAR[which(AllSchools.LIDAR.reduced.LiDAR$fold == 1),]
LiDAR.60percent       <- AllSchools.LIDAR.reduced.LiDAR[which(AllSchools.LIDAR.reduced.LiDAR$fold == 2 | AllSchools.LIDAR.reduced.LiDAR$fold == 3 | AllSchools.LIDAR.reduced.LiDAR$fold == 4),]
LiDAR.last.20percent  <- AllSchools.LIDAR.reduced.LiDAR[which(AllSchools.LIDAR.reduced.LiDAR$fold == 5),]

OSM.first.20percent <- AllSchools.LIDAR.reduced.OSM[which(AllSchools.LIDAR.reduced.OSM$fold == 1),]
OSM.60percent       <- AllSchools.LIDAR.reduced.OSM[which(AllSchools.LIDAR.reduced.OSM$fold == 2 | AllSchools.LIDAR.reduced.OSM$fold == 3 | AllSchools.LIDAR.reduced.OSM$fold == 4),]
OSM.last.20percent  <- AllSchools.LIDAR.reduced.OSM[which(AllSchools.LIDAR.reduced.OSM$fold == 5),]

All.first.20percent <- AllSchools.LIDAR.reduced.AllLIDAR[which(AllSchools.LIDAR.reduced.AllLIDAR$fold == 1),]
All.60percent       <- AllSchools.LIDAR.reduced.AllLIDAR[which(AllSchools.LIDAR.reduced.AllLIDAR$fold == 2 | AllSchools.LIDAR.reduced.AllLIDAR$fold == 3 | AllSchools.LIDAR.reduced.AllLIDAR$fold == 4),]
All.last.20percent  <- AllSchools.LIDAR.reduced.AllLIDAR[which(AllSchools.LIDAR.reduced.AllLIDAR$fold == 5),]


######################################
####### Try linear mixed model
######################################

# Use lmer to fit a varying intercept model with no predictors
install.packages("arm")
library(arm)

########################################################
####### Mixed Linear - for comparison report - First 20%
########################################################

# All data
# No pooling - just population predictor
no.pooling.all <- lm(EstimatedArea ~ lnpopulation + factor(state) - 1, data = All.first.20percent[which(All.first.20percent$SchoolType == "HigherEducation"),])
summary(no.pooling.all) # R2 = 0.32
AIC(no.pooling.all) # 16,188
BIC(no.pooling.all) # 16,378
predictions.no.pooling.all <- predict(no.pooling.all, All.first.20percent[which(All.first.20percent$SchoolType == "HigherEducation"),])

plot(predictions.no.pooling.all, All.first.20percent[which(All.first.20percent$SchoolType == "HigherEducation"),]$EstimatedArea,
     xlim = c(-250000, 1750000),
     ylim = c(-250000, 1750000),
     xlab = "Predicted Area (SF)",
     ylab = "Observed Area (SF)",
     main = "All LIDAR: No Pooling - Population (R2 = 0.32; AIC = 16.2K)",
     cex.axis = 0.8,
     cex.main = 0.8)
abline(0,1)

# No pooling - long model
no.pooling.all.long <- lm(EstimatedArea ~ PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                          + lnFarmland + lnWaterArea + lnpopulation + factor(state) - 1, data = All.first.20percent[which(All.first.20percent$SchoolType == "HigherEducation"),])
summary(no.pooling.all.long) # R2 = 0.31
AIC(no.pooling.all.long) # 16,146 -> lower, good
BIC(no.pooling.all.long) # 16,361 -> lower, good
predictions.no.pooling.all.long <- predict(no.pooling.all.long, All.first.20percent[which(All.first.20percent$SchoolType == "HigherEducation"),])

plot(predictions.no.pooling.all.long, All.first.20percent[which(All.first.20percent$SchoolType == "HigherEducation"),]$EstimatedArea,
     xlim = c(-250000, 1750000),
     ylim = c(-250000, 1750000),
     xlab = "Predicted Area (SF)",
     ylab = "Observed Area (SF)",
     main = "All LIDAR: No Pooling - Long Model (R2 = 0.31; AIC = 16.1K)",
     cex.axis = 0.8,
     cex.main = 0.8)
abline(0,1)


# Only OSM data
# No pooling - just population predictor
no.pooling.OSM <- lm(EstimatedArea ~ lnpopulation + factor(state) - 1, data = OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),])
summary(no.pooling.OSM) # R2 = .56
AIC(no.pooling.OSM) # 4,049
BIC(no.pooling.OSM) # 4,153
predictions.no.pooling.OSM <- predict(no.pooling.OSM, OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),])

plot(predictions.no.pooling.OSM, OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),]$EstimatedArea,
     xlim = c(-250000, 1750000),
     ylim = c(-250000, 1750000),
     xlab = "Predicted Area (SF)",
     ylab = "Observed Area (SF)",
     main = "OSM: No Pooling - Population (R2 = 0.56; AIC = 4K)",
     cex.axis = 0.8,
     cex.main = 0.8)
abline(0,1)

# No pooling - long model
no.pooling.OSM.long <- lm(EstimatedArea ~ PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                          + lnFarmland + lnWaterArea + lnpopulation + factor(state) - 1, data = OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),])
summary(no.pooling.OSM.long) # R2 = .54
AIC(no.pooling.OSM.long) # 4,057
BIC(no.pooling.OSM.long) # 4,179 
predictions.no.pooling.OSM.long <- predict(no.pooling.OSM.long, OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),])

plot(predictions.no.pooling.OSM.long, OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),]$EstimatedArea,
     xlim = c(-250000, 1750000),
     ylim = c(-250000, 1750000),
     xlab = "Predicted Area (SF)",
     ylab = "Observed Area (SF)",
     main = "OSM: No Pooling - Long Model (R2 = 0.54; AIC = 4.1K)",
     cex.axis = 0.8,
     cex.main = 0.8)
abline(0,1)

# Only OSM data
# Partial pooling - varying int - just population predictor
varying.int.OSM <- lmer(EstimatedArea ~ lnpopulation + (1 | state), data = OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),])
summary(varying.int.OSM)
display(varying.int.OSM) # AIC = 3,973; DIC = 4,049
BIC(varying.int.OSM) # 3,985
predictions.varying.int.OSM  <- predict(varying.int.OSM, OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),])

plot(predictions.varying.int.OSM, OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),]$EstimatedArea,
     xlim = c(-250000, 1750000),
     ylim = c(-250000, 1750000),
     xlab = "Predicted Area (SF)",
     ylab = "Observed Area (SF)",
     main = "OSM: Partial Pooling - Varying Int. - Pop. (AIC = 4K; DIC = 4K)",
     cex.axis = 0.8,
     cex.main = 0.8)
abline(0,1)

# Partial pooling - varying int - long model
varying.int.OSM.long <- lmer(EstimatedArea ~ PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                             + lnFarmland + lnWaterArea + lnpopulation + (1 | state), data = OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),])
summary(varying.int.OSM.long)
display(varying.int.OSM.long) # AIC = 3,872; DIC = 4,155
BIC(varying.int.OSM.long) # 3,901
predictions.varying.int.OSM.long  <- predict(varying.int.OSM.long, OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),])

plot(predictions.varying.int.OSM.long, OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),]$EstimatedArea,
     xlim = c(-250000, 1750000),
     ylim = c(-250000, 1750000),
     xlab = "Predicted Area (SF)",
     ylab = "Observed Area (SF)",
     main = "OSM: Partial Pooling - Varying Int. - Long Model (AIC = 3.9K; DIC = 4.2K)",
     cex.axis = 0.8,
     cex.main = 0.8)
abline(0,1)


# Partial pooling - varying int and slope - just population predictor
varying.both.OSM <- lmer(EstimatedArea ~ lnpopulation + (1 + lnpopulation | state), data = OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),])
summary(varying.both.OSM)
display(varying.both.OSM) # AIC = 3,975; DIC = 4,047
BIC(varying.both.OSM) # 3,992
predictions.varying.both.OSM  <- predict(varying.both.OSM, OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),])

plot(predictions.varying.both.OSM, OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),]$EstimatedArea,
     xlim = c(-250000, 1750000),
     ylim = c(-250000, 1750000),
     xlab = "Predicted Area (SF)",
     ylab = "Observed Area (SF)",
     main = "OSM: Partial Pooling - Varying Int. & Slope - Pop. (AIC = 4K; DIC = 4K)",
     cex.axis = 0.8,
     cex.main = 0.8)
abline(0,1)

# Partial pooling - varying int and slope - long model
varying.both.OSM.long <- lmer(EstimatedArea ~ PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                              + lnFarmland + lnWaterArea + lnpopulation + (1 + lnpopulation | state), data = OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),])
summary(varying.both.OSM.long)
display(varying.both.OSM.long) # AIC = 3,873; DIC = 4,153
BIC(varying.both.OSM.long) # 3,909
predictions.varying.both.OSM.long  <- predict(varying.both.OSM.long, OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),])

plot(predictions.varying.both.OSM.long, OSM.first.20percent[which(OSM.first.20percent$SchoolType == "HigherEducation"),]$EstimatedArea,
     xlim = c(-250000, 1750000),
     ylim = c(-250000, 1750000),
     xlab = "Predicted Area (SF)",
     ylab = "Observed Area (SF)",
     main = "OSM: Partial Pooling - Varying Int. & Slope - Long Model (AIC = 3.9K; DIC = 4.2K)",
     cex.axis = 0.8,
     cex.main = 0.8)
abline(0,1)


#############################################
####### LINEAR REGRESSIONS - 1.11.2018 
#############################################

OSM.data <- AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$match_type == "OSM"),]
count(OSM.data,"state") # missing SD, TN, AK, and HI
count(OSM.data[which(OSM.data$SchoolType == "HigherEducation"),],"state") # missing ME, VT, WY, SD, TN, AK, HI
count(OSM.data[which(OSM.data$SchoolType == "K12Public"),],"state") # missing SD, TN, AK, HI
count(OSM.data[which(OSM.data$SchoolType == "K12Private"),],"state") # missing DE, MT, SD, TN, AK, HI


OSM.NoPooling.HE <- lm(EstimatedArea ~ PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                       + lnFarmland + lnWaterArea + lnpopulation + LANDGRNT + lnAPPLCN + HELevel + factor(state) -1, data = OSM.data[which(OSM.data$SchoolType == "HigherEducation"),])
summary(OSM.NoPooling.HE) # R2 = 0.54
nobs(OSM.NoPooling.HE) # 703

OSM.NoPooling.PU <- lm(EstimatedArea ~ PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                       + lnFarmland + lnWaterArea + lnpopulation +
                         SCH_TYPE_TEXT + LEVEL + CHARTER_TEXT + factor(state) - 1, data = OSM.data[which(OSM.data$SchoolType == "K12Public"),])
summary(OSM.NoPooling.PU) # R2 = 0.76
nobs(OSM.NoPooling.PU) # 12532

OSM.NoPooling.PR <- lm(EstimatedArea ~ PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                       + lnFarmland + lnWaterArea + lnpopulation + p_white + s_kg + 
                         tothrs + level + relig + factor(state) - 1, data = OSM.data[which(OSM.data$SchoolType == "K12Private"),])
summary(OSM.NoPooling.PR) # R2 = 0.51
nobs(OSM.NoPooling.PR) # 2679



OSM.avg.HE <- lm(EstimatedArea ~ PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                 + lnFarmland + lnWaterArea + lnpopulation + LANDGRNT + lnAPPLCN + HELevel, data = OSM.data[which(OSM.data$SchoolType == "HigherEducation"),])
summary(OSM.avg.HE) # R2 = 0.31
nobs(OSM.avg.HE) # 703

OSM.avg.PU <- lm(EstimatedArea ~ PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                 + lnFarmland + lnWaterArea + lnpopulation +
                   SCH_TYPE_TEXT + LEVEL + CHARTER_TEXT, data = OSM.data[which(OSM.data$SchoolType == "K12Public"),])
summary(OSM.avg.PU) # R2 = 0.44
nobs(OSM.avg.PU) # 12532

OSM.avg.PR <- lm(EstimatedArea ~ PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                 + lnFarmland + lnWaterArea + lnpopulation + p_white + s_kg + 
                   tothrs + level + relig, data = OSM.data[which(OSM.data$SchoolType == "K12Private"),])
summary(OSM.avg.PR) # R2 = 0.16
nobs(OSM.avg.PR) # 2679

# Set column for the predictions
AllSchools.LIDAR.reduced$predictions <- NA

# See how much of our dataset will be predicted with the average models
# First, count HE we need to make predictions for which we have LIDAR data (states)
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "NoLIDAR" & AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$state != "ME" & AllSchools.LIDAR.reduced$state != "VT" & AllSchools.LIDAR.reduced$state != "WY" & AllSchools.LIDAR.reduced$state != "SD" & AllSchools.LIDAR.reduced$state != "TN"),]) # 4227
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "NoLIDAR" & AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),]) # 4521 (94% of the HE schools we need to predict roofspace can use the no pooling model)

nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "NoLIDAR" & AllSchools.LIDAR.reduced$SchoolType == "K12Public" & AllSchools.LIDAR.reduced$state != "SD" & AllSchools.LIDAR.reduced$state != "TN"),]) # 71,036
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "NoLIDAR" & AllSchools.LIDAR.reduced$SchoolType == "K12Public"),]) # 73,632 (96% of the PU schools we need to predict roofspace can use the no pooling model)

nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "NoLIDAR" & AllSchools.LIDAR.reduced$SchoolType == "K12Private" & AllSchools.LIDAR.reduced$state != "DE" & AllSchools.LIDAR.reduced$state != "MT" & AllSchools.LIDAR.reduced$state != "SD" & AllSchools.LIDAR.reduced$state != "TN"),]) # 16,484
nrow(AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "NoLIDAR" & AllSchools.LIDAR.reduced$SchoolType == "K12Private"),]) # 17,221 (96% of the PR schools we need to predict roofspace can use the no pooling model)

# Make the predictions (Need to rerun this for 1.11.18 regressions)
# No Pooling Model predictions - all states except the following for each school type
HE.avg <- c("ME", "VT", "WY", "SD", "TN")
PU.avg <- c("SD", "TN")
PR.avg <- c("DE", "MT", "SD", "TN")


AllSchools.LIDAR.reduced.HE <- AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),] # 7556
AllSchools.LIDAR.reduced.PU <- AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Public"),] # 99772
AllSchools.LIDAR.reduced.PR <- AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "K12Private"),] # 26807

AllSchools.LIDAR.reduced.HE.tn <- AllSchools.LIDAR.reduced.HE[which(AllSchools.LIDAR.reduced.HE$state == "TN"),] # 189
AllSchools.LIDAR.reduced.HE.sd <- AllSchools.LIDAR.reduced.HE[which(AllSchools.LIDAR.reduced.HE$state == "SD"),] # 31
AllSchools.LIDAR.reduced.HE.me <- AllSchools.LIDAR.reduced.HE[which(AllSchools.LIDAR.reduced.HE$state == "ME"),] # 43
AllSchools.LIDAR.reduced.HE.vt <- AllSchools.LIDAR.reduced.HE[which(AllSchools.LIDAR.reduced.HE$state == "VT"),] # 28
AllSchools.LIDAR.reduced.HE.wy <- AllSchools.LIDAR.reduced.HE[which(AllSchools.LIDAR.reduced.HE$state == "WY"),] # 12
AllSchools.LIDAR.reduced.HE.others <- AllSchools.LIDAR.reduced.HE[!AllSchools.LIDAR.reduced.HE$state %in% HE.avg,] # 7253

AllSchools.LIDAR.reduced.PU.tn <- AllSchools.LIDAR.reduced.PU[which(AllSchools.LIDAR.reduced.PU$state == "TN"),] # 1865
AllSchools.LIDAR.reduced.PU.sd <- AllSchools.LIDAR.reduced.PU[which(AllSchools.LIDAR.reduced.PU$state == "SD"),] # 731
AllSchools.LIDAR.reduced.PU.others <- AllSchools.LIDAR.reduced.PU[!AllSchools.LIDAR.reduced.PU$state %in% PU.avg,] # 97176

AllSchools.LIDAR.reduced.PR.tn <- AllSchools.LIDAR.reduced.PR[which(AllSchools.LIDAR.reduced.PR$state == "TN"),] # 471
AllSchools.LIDAR.reduced.PR.sd <- AllSchools.LIDAR.reduced.PR[which(AllSchools.LIDAR.reduced.PR$state == "SD"),] # 68
AllSchools.LIDAR.reduced.PR.de <- AllSchools.LIDAR.reduced.PR[which(AllSchools.LIDAR.reduced.PR$state == "DE"),] # 112
AllSchools.LIDAR.reduced.PR.mt <- AllSchools.LIDAR.reduced.PR[which(AllSchools.LIDAR.reduced.PR$state == "MT"),] # 101
AllSchools.LIDAR.reduced.PR.others <- AllSchools.LIDAR.reduced.PR[!AllSchools.LIDAR.reduced.PR$state %in% PR.avg,] # 26,055



########################
########################
# Make predictions
########################
########################

# First, consider the state-average models for HE
AllSchools.LIDAR.reduced.HE.tn[,"predictions"] <- predict(OSM.avg.HE, AllSchools.LIDAR.reduced.HE.tn)
AllSchools.LIDAR.reduced.HE.sd[,"predictions"] <- predict(OSM.avg.HE, AllSchools.LIDAR.reduced.HE.sd)
AllSchools.LIDAR.reduced.HE.me[,"predictions"] <- predict(OSM.avg.HE, AllSchools.LIDAR.reduced.HE.me)
AllSchools.LIDAR.reduced.HE.vt[,"predictions"] <- predict(OSM.avg.HE, AllSchools.LIDAR.reduced.HE.vt)
AllSchools.LIDAR.reduced.HE.wy[,"predictions"] <- predict(OSM.avg.HE, AllSchools.LIDAR.reduced.HE.wy)

AllSchools.LIDAR.reduced.PU.tn[,"predictions"] <- predict(OSM.avg.PU, AllSchools.LIDAR.reduced.PU.tn)
AllSchools.LIDAR.reduced.PU.sd[,"predictions"] <- predict(OSM.avg.PU, AllSchools.LIDAR.reduced.PU.sd)

AllSchools.LIDAR.reduced.PR.tn[,"predictions"] <- predict(OSM.avg.PR, AllSchools.LIDAR.reduced.PR.tn)
AllSchools.LIDAR.reduced.PR.sd[,"predictions"] <- predict(OSM.avg.PR, AllSchools.LIDAR.reduced.PR.sd)
AllSchools.LIDAR.reduced.PR.de[,"predictions"] <- predict(OSM.avg.PR, AllSchools.LIDAR.reduced.PR.de)
AllSchools.LIDAR.reduced.PR.mt[,"predictions"] <- predict(OSM.avg.PR, AllSchools.LIDAR.reduced.PR.mt)

# Next, consider the no.pooling model
AllSchools.LIDAR.reduced.HE.others[,"predictions"] <- predict(OSM.NoPooling.HE, AllSchools.LIDAR.reduced.HE.others)
AllSchools.LIDAR.reduced.PU.others[,"predictions"] <- predict(OSM.NoPooling.PU, AllSchools.LIDAR.reduced.PU.others)
AllSchools.LIDAR.reduced.PR.others[,"predictions"] <- predict(OSM.NoPooling.PR, AllSchools.LIDAR.reduced.PR.others)

nrow(AllSchools.LIDAR.reduced.HE.tn) # 189
nrow(AllSchools.LIDAR.reduced.HE.sd) # 31
nrow(AllSchools.LIDAR.reduced.HE.me) # 43
nrow(AllSchools.LIDAR.reduced.HE.vt) # 28
nrow(AllSchools.LIDAR.reduced.HE.wy) # 12
nrow(AllSchools.LIDAR.reduced.PU.tn) # 1865
nrow(AllSchools.LIDAR.reduced.PU.sd) # 731
nrow(AllSchools.LIDAR.reduced.PR.tn) # 471
nrow(AllSchools.LIDAR.reduced.PR.sd) # 68
nrow(AllSchools.LIDAR.reduced.PR.de) # 112
nrow(AllSchools.LIDAR.reduced.PR.mt) # 101
nrow(AllSchools.LIDAR.reduced.HE.others) # 7253
nrow(AllSchools.LIDAR.reduced.PU.others) # 97176
nrow(AllSchools.LIDAR.reduced.PR.others) # 26055


AllSchools.LIDAR.reduced.recombined <- rbind(AllSchools.LIDAR.reduced.HE.tn, AllSchools.LIDAR.reduced.HE.sd, AllSchools.LIDAR.reduced.HE.me, 
                                             AllSchools.LIDAR.reduced.HE.vt, AllSchools.LIDAR.reduced.HE.wy, AllSchools.LIDAR.reduced.PU.tn,
                                             AllSchools.LIDAR.reduced.PU.sd, AllSchools.LIDAR.reduced.PR.tn, AllSchools.LIDAR.reduced.PR.sd, 
                                             AllSchools.LIDAR.reduced.PR.de, AllSchools.LIDAR.reduced.PR.mt, AllSchools.LIDAR.reduced.HE.others, 
                                             AllSchools.LIDAR.reduced.PU.others, AllSchools.LIDAR.reduced.PR.others)
# sanity checks
nrow(AllSchools.LIDAR.reduced.recombined) # 134135
count(AllSchools.LIDAR.reduced.recombined, 'source_dataset.x') # HE 7556, PR 26807, PU 99772

# look at predictions
summary(AllSchools.LIDAR.reduced.recombined$predictions) # min = -569k, max = 1M
hist(AllSchools.LIDAR.reduced.recombined$predictions)
sum(AllSchools.LIDAR.reduced.recombined$predictions < 0, na.rm=TRUE) # 10607 ; 8% of this df
hist(AllSchools.LIDAR.reduced.recombined[which(AllSchools.LIDAR.reduced.recombined$predictions < 0),]$predictions)
range(AllSchools.LIDAR.reduced.recombined$predictions, na.rm = TRUE) # min = -569k, max = 1M
range(AllSchools.LIDAR.reduced.recombined$EstimatedArea, na.rm = TRUE) # 0 to 3.3 M


# for now, coerce the predictions less than 0 to 0
AllSchools.LIDAR.reduced.recombined$FinalArea <- NA
AllSchools.LIDAR.reduced.recombined[which(AllSchools.LIDAR.reduced.recombined$LIDAR == "HaveLIDAR"),]$FinalArea <- AllSchools.LIDAR.reduced.recombined[which(AllSchools.LIDAR.reduced.recombined$LIDAR == "HaveLIDAR"),]$EstimatedArea
AllSchools.LIDAR.reduced.recombined[which(AllSchools.LIDAR.reduced.recombined$LIDAR == "NoLIDAR"),]$FinalArea <- AllSchools.LIDAR.reduced.recombined[which(AllSchools.LIDAR.reduced.recombined$LIDAR == "NoLIDAR"),]$predictions
AllSchools.LIDAR.reduced.recombined[which(AllSchools.LIDAR.reduced.recombined$LIDAR == "NoLIDAR" & AllSchools.LIDAR.reduced.recombined$predictions < 0),]$FinalArea <- 0
hist(AllSchools.LIDAR.reduced.recombined$FinalArea)
summary(AllSchools.LIDAR.reduced.recombined$FinalArea) # 0 to 3.3M; 6895 NAs

# plot predicted vs observed
plot(AllSchools.LIDAR.reduced.recombined$predictions, AllSchools.LIDAR.reduced.recombined$EstimatedArea)

# Write a reduced version to the csv
View(AllSchools.LIDAR.reduced.recombined)
AllSchools.LIDAR.reduced.recombined <- AllSchools.LIDAR.reduced.recombined[,-c(1:3, 11, 14:34, 41:100)]
AllSchools.LIDAR.reduced.recombined <- AllSchools.LIDAR.reduced.recombined[,-c(16:29)]
nrow(AllSchools.LIDAR.reduced.recombined)
write.table(AllSchools.LIDAR.reduced.recombined, file = "C:/Users/hanusnil/Box Sync/AllSchools.LIDAR.reduced.recombined.12.15.17.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")



##############################
####### LINEAR REGRESSIONS
##############################

###################
## HIGHER EDUCATION
###################
LIDAR.reg.HE.state <- lm(EstimatedArea ~ state + lnpopulation + PercentRural + lnMedian.Household.Income + Poverty.Percent.All.Ages +
                           lnBelow18 + lnX18andOver + lnGovHealth + lnPhysicians + VehiclesHouse + lnGovHigh + lnCountyArea + lnWaterArea + 
                           lnFarmland + GovRev + lnEmployment + lnPopDensity + lnHouseDensity +
                           LANDGRNT + lnAPPLCN + HELevel,
                         data = AllSchools.LIDAR.reduced.others[which(AllSchools.LIDAR.reduced.others$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced.others$SchoolType == "HigherEducation"),])
summary(LIDAR.reg.HE.state) #R2 0.= 29
nobs(LIDAR.reg.HE.state) # 2,563

LIDAR.reg.HE.state.simple <- lm(EstimatedArea ~ state + lnpopulation + PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                                + lnFarmland + lnWaterArea,
                                data = AllSchools.LIDAR.reduced.others[which(AllSchools.LIDAR.reduced.others$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced.others$SchoolType == "HigherEducation"),])
summary(LIDAR.reg.HE.state.simple) # R2 = 0.26
nobs(LIDAR.reg.HE.state.simple) # 2,893

LIDAR.reg.HE.state.school <- lm(EstimatedArea ~ state + LANDGRNT + lnAPPLCN + HELevel,
                                data = AllSchools.LIDAR.reduced.others[which(AllSchools.LIDAR.reduced.others$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced.others$SchoolType == "HigherEducation"),])
summary(LIDAR.reg.HE.state.school) # R2 = 0.21
nobs(LIDAR.reg.HE.state.school) # 2,577


###################
# now for SD and TN
###################
LIDAR.reg.HE.SDTN <- lm(EstimatedArea ~ lnpopulation + PercentRural + lnMedian.Household.Income + Poverty.Percent.All.Ages +
                          lnBelow18 + lnX18andOver + lnGovHealth + lnPhysicians + VehiclesHouse + lnGovHigh + lnCountyArea + lnWaterArea + 
                          lnFarmland + GovRev + lnEmployment + lnPopDensity + lnHouseDensity +
                          LANDGRNT + lnAPPLCN + HELevel,
                        data = AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),])
summary(LIDAR.reg.HE.SDTN) # R2 = 0.29
nobs(LIDAR.reg.HE.SDTN) # 2,563

LIDAR.reg.HE.SDTN.simple <- lm(EstimatedArea ~ state + lnpopulation + PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                               + lnFarmland + lnWaterArea,
                               data = AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),])
summary(LIDAR.reg.HE.SDTN.simple) # R2 = 0.22
nobs(LIDAR.reg.HE.SDTN.simple) # 2,893

LIDAR.reg.HE.SDTN.school <- lm(EstimatedArea ~ LANDGRNT + lnAPPLCN + HELevel,
                               data = AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced$SchoolType == "HigherEducation"),])
summary(LIDAR.reg.HE.SDTN.school) # R2 = 0.21
nobs(LIDAR.reg.HE.SDTN.school) # 2,577


###################
# now LIDAR vs OSM
###################
LIDAR.reg.HE.state.LIDAR <- lm(EstimatedArea ~ state + lnpopulation + PercentRural + lnMedian.Household.Income + Poverty.Percent.All.Ages +
                                 lnBelow18 + lnX18andOver + lnGovHealth + lnPhysicians + VehiclesHouse + lnGovHigh + lnCountyArea + lnWaterArea + 
                                 lnFarmland + GovRev + lnEmployment + lnPopDensity + lnHouseDensity +
                                 LANDGRNT + lnAPPLCN + HELevel,
                               data = AllSchools.LIDAR.LIDAR.others[which(AllSchools.LIDAR.LIDAR.others$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.LIDAR.others$SchoolType == "HigherEducation"),])
summary(LIDAR.reg.HE.state.LIDAR) # R2 = 0.03
nobs(LIDAR.reg.HE.state.LIDAR) # 1,860 


LIDAR.reg.HE.state.OSM <- lm(EstimatedArea ~ state + lnpopulation + PercentRural + lnMedian.Household.Income + Poverty.Percent.All.Ages +
                               lnBelow18 + lnX18andOver + lnGovHealth + lnPhysicians + VehiclesHouse + lnGovHigh + lnCountyArea + lnWaterArea + 
                               lnFarmland + GovRev + lnEmployment + lnPopDensity + lnHouseDensity +
                               LANDGRNT + lnAPPLCN + HELevel,
                             data = AllSchools.LIDAR.OSM.others[which(AllSchools.LIDAR.OSM.others$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.OSM.others$SchoolType == "HigherEducation"),])
summary(LIDAR.reg.HE.state.OSM) # R2 = 0.33
nobs(LIDAR.reg.HE.state.OSM) # 703 



###################
## K12 Public
###################
LIDAR.reg.PU.state <- lm(EstimatedArea ~ state + lnpopulation + PercentRural + lnMedian.Household.Income + Poverty.Percent.All.Ages +
                           lnBelow18 + lnX18andOver + lnGovHealth + lnPhysicians + VehiclesHouse + lnGovHigh + lnCountyArea + lnWaterArea + 
                           lnFarmland + GovRev + lnEmployment + lnPopDensity + lnHouseDensity +
                           SCH_TYPE_TEXT + LEVEL + CHARTER_TEXT,
                         data = AllSchools.LIDAR.reduced.others[which(AllSchools.LIDAR.reduced.others$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.reduced.others$SchoolType == "K12Public"),])
summary(LIDAR.reg.PU.state) # R2 = 0.22
nobs(LIDAR.reg.PU.state) # 23,466


LIDAR.reg.PU.state.simple <- lm(EstimatedArea ~ state + lnpopulation + PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                                + lnFarmland + lnWaterArea,
                                data = AllSchools.LIDAR.reduced.others[which(AllSchools.LIDAR.reduced.others$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.reduced.others$SchoolType == "K12Public"),])
summary(LIDAR.reg.PU.state.simple) # R2 = 0.16
nobs(LIDAR.reg.PU.state.simple) # 23,533


LIDAR.reg.PU.state.school <- lm(EstimatedArea ~ state + SCH_TYPE_TEXT + LEVEL + CHARTER_TEXT,
                                data = AllSchools.LIDAR.reduced.others[which(AllSchools.LIDAR.reduced.others$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.reduced.others$SchoolType == "K12Public"),])
summary(LIDAR.reg.PU.state.school) # R2 = 0.16
nobs(LIDAR.reg.PU.state.school) # 23,490


###################
# now for SD and TN
###################
LIDAR.reg.PU.SDTN <- lm(EstimatedArea ~ lnpopulation + PercentRural + lnMedian.Household.Income + Poverty.Percent.All.Ages +
                          lnBelow18 + lnX18andOver + lnGovHealth + lnPhysicians + VehiclesHouse + lnGovHigh + lnCountyArea + lnWaterArea + 
                          lnFarmland + GovRev + lnEmployment + lnPopDensity + lnHouseDensity +
                          SCH_TYPE_TEXT + LEVEL + CHARTER_TEXT,
                        data = AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.reduced$SchoolType == "K12Public"),])
summary(LIDAR.reg.PU.SDTN) # R2 = 0.21
nobs(LIDAR.reg.PU.SDTN) # 23,466


LIDAR.reg.PU.SDTN.simple <- lm(EstimatedArea ~ state + lnpopulation + PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                               + lnFarmland + lnWaterArea,
                               data = AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.reduced$SchoolType == "K12Public"),])
summary(LIDAR.reg.PU.SDTN.simple) # R2 = 0.14
nobs(LIDAR.reg.PU.SDTN.simple) # 23,533


LIDAR.reg.PU.SDTN.school <- lm(EstimatedArea ~ SCH_TYPE_TEXT + LEVEL + CHARTER_TEXT,
                               data = AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.reduced$SchoolType == "K12Public"),])
summary(LIDAR.reg.PU.SDTN.school) # R2 = 0.12
nobs(LIDAR.reg.PU.SDTN.school) # 23,490


###################
# now LIDAR vs OSM
###################
LIDAR.reg.PU.state.LIDAR <- lm(EstimatedArea ~ state + lnpopulation + PercentRural + lnMedian.Household.Income + Poverty.Percent.All.Ages +
                                 lnBelow18 + lnX18andOver + lnGovHealth + lnPhysicians + VehiclesHouse + lnGovHigh + lnCountyArea + lnWaterArea + 
                                 lnFarmland + GovRev + lnEmployment + lnPopDensity + lnHouseDensity +
                                 SCH_TYPE_TEXT + LEVEL + CHARTER_TEXT,
                               data = AllSchools.LIDAR.LIDAR.others[which(AllSchools.LIDAR.LIDAR.others$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.LIDAR.others$SchoolType == "K12Public"),])
summary(LIDAR.reg.PU.state.LIDAR) # R2 = 0.02
nobs(LIDAR.reg.PU.state.LIDAR) # 10,934

LIDAR.reg.PU.state.OSM <- lm(EstimatedArea ~ state + lnpopulation + PercentRural + lnMedian.Household.Income + Poverty.Percent.All.Ages +
                               lnBelow18 + lnX18andOver + lnGovHealth + lnPhysicians + VehiclesHouse + lnGovHigh + lnCountyArea + lnWaterArea + 
                               lnFarmland + GovRev + lnEmployment + lnPopDensity + lnHouseDensity +
                               SCH_TYPE_TEXT + LEVEL + CHARTER_TEXT,
                             data = AllSchools.LIDAR.OSM.others[which(AllSchools.LIDAR.OSM.others$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.OSM.others$SchoolType == "K12Public"),])
summary(LIDAR.reg.PU.state.OSM) # R2 = 0.49
nobs(LIDAR.reg.PU.state.OSM) # 12,532


###################
## K12 Private
###################
LIDAR.reg.PR.state <- lm(EstimatedArea ~ state + lnpopulation + PercentRural + lnMedian.Household.Income + Poverty.Percent.All.Ages +
                           lnBelow18 + lnX18andOver + lnGovHealth + lnPhysicians + VehiclesHouse + lnGovHigh + lnCountyArea + lnWaterArea + 
                           lnFarmland + GovRev + lnEmployment + lnPopDensity + lnHouseDensity +
                           lnmales + p_white + s_kg + tothrs + level + relig,
                         data = AllSchools.LIDAR.reduced.others[which(AllSchools.LIDAR.reduced.others$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.reduced.others$SchoolType == "K12Private"),])
summary(LIDAR.reg.PR.state) # R2 = 0.09
nobs(LIDAR.reg.PR.state) # 9,237


LIDAR.reg.PR.state.simple <- lm(EstimatedArea ~ state + lnpopulation + PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                                + lnFarmland + lnWaterArea,
                                data = AllSchools.LIDAR.reduced.others[which(AllSchools.LIDAR.reduced.others$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.reduced.others$SchoolType == "K12Private"),])
summary(LIDAR.reg.PR.state.simple) # R2 = 0.06
nobs(LIDAR.reg.PR.state.simple) # 9,238


LIDAR.reg.PR.state.school <- lm(EstimatedArea ~ state + lnmales + p_white + s_kg + tothrs + level + relig,
                                data = AllSchools.LIDAR.reduced.others[which(AllSchools.LIDAR.reduced.others$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.reduced.others$SchoolType == "K12Private"),])
summary(LIDAR.reg.PR.state.school) # r2 = 0.08
nobs(LIDAR.reg.PR.state.school) # 9,240


###################
# now for SD and TN
###################
LIDAR.reg.PR.SDTN <- lm(EstimatedArea ~ lnpopulation + PercentRural + lnMedian.Household.Income + Poverty.Percent.All.Ages +
                          lnBelow18 + lnX18andOver + lnGovHealth + lnPhysicians + VehiclesHouse + lnGovHigh + lnCountyArea + lnWaterArea + 
                          lnFarmland + GovRev + lnEmployment + lnPopDensity + lnHouseDensity +
                          lnmales + p_white + s_kg + tothrs + level + relig,
                        data = AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.reduced$SchoolType == "K12Private"),])
summary(LIDAR.reg.PR.SDTN) # R2 = 0.08
nobs(LIDAR.reg.PR.SDTN) # 9,237


LIDAR.reg.PR.SDTN.simple <- lm(EstimatedArea ~ state + lnpopulation + PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
                               + lnFarmland + lnWaterArea,
                               data = AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.reduced$SchoolType == "K12Private"),])
summary(LIDAR.reg.PR.SDTN.simple) # R2 = 0.06
nobs(LIDAR.reg.PR.SDTN.simple) # 9,238


LIDAR.reg.PR.SDTN.school <- lm(EstimatedArea ~ lnmales + p_white + s_kg + tothrs + level + relig,
                               data = AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.reduced$SchoolType == "K12Private"),])
summary(LIDAR.reg.PR.SDTN.school) # R2 = 0.06
nobs(LIDAR.reg.PR.SDTN.school) # 9,240


###################
# now LIDAR vs OSM
###################
LIDAR.reg.PR.state.LIDAR <- lm(EstimatedArea ~ state + lnpopulation + PercentRural + lnMedian.Household.Income + Poverty.Percent.All.Ages +
                                 lnBelow18 + lnX18andOver + lnGovHealth + lnPhysicians + VehiclesHouse + lnGovHigh + lnCountyArea + lnWaterArea + 
                                 lnFarmland + GovRev + lnEmployment + lnPopDensity + lnHouseDensity +
                                 lnmales + p_white + s_kg + tothrs + level + relig,
                               data = AllSchools.LIDAR.LIDAR.others[which(AllSchools.LIDAR.LIDAR.others$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.LIDAR.others$SchoolType == "K12Private"),])
summary(LIDAR.reg.PR.state.LIDAR) # R2 = 0.03
nobs(LIDAR.reg.PR.state.LIDAR) # 6,558

LIDAR.reg.PR.state.OSM <- lm(EstimatedArea ~ state + lnpopulation + PercentRural + lnMedian.Household.Income + Poverty.Percent.All.Ages +
                               lnBelow18 + lnX18andOver + lnGovHealth + lnPhysicians + VehiclesHouse + lnGovHigh + lnCountyArea + lnWaterArea + 
                               lnFarmland + GovRev + lnEmployment + lnPopDensity + lnHouseDensity +
                               lnmales + p_white + s_kg + tothrs + level + relig,
                             data = AllSchools.LIDAR.OSM.others[which(AllSchools.LIDAR.OSM.others$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.OSM.others$SchoolType == "K12Private"),])
summary(LIDAR.reg.PR.state.OSM) # R2 = 0.19
nobs(LIDAR.reg.PR.state.OSM) # 2679


##############################
####### NON-PARAMETRIC
##############################

library(mgcv) 

LIDAR.reg.HE.state.np <- gam(EstimatedArea ~ state + s(population) + PercentRural + s(Median.Household.Income) + Poverty.Percent.All.Ages +
                               s(Below18) + s(X18andOver) + s(GovHealth) + s(Physicians) + VehiclesHouse + s(GovHigh) + s(CountyArea) + s(WaterArea) + 
                               s(Farmland) + GovRev + s(Employment) + s(PopDensity) + s(HouseDensity) +
                               LANDGRNT + s(APPLCN) + HELevel,
                             data = AllSchools.LIDAR.reduced.others[which(AllSchools.LIDAR.reduced.others$LIDAR == "HaveLIDAR" & AllSchools.LIDAR.reduced.others$SchoolType == "HigherEducation"),])
summary(LIDAR.reg.HE.state.np) #R2 = 0.51
nobs(LIDAR.reg.HE.state.np) # 2,563

LIDAR.reg.PU.state.np <- gam(EstimatedArea ~ state + s(population) + PercentRural + s(Median.Household.Income) + Poverty.Percent.All.Ages +
                               s(Below18) + s(X18andOver) + s(GovHealth) + s(Physicians) + VehiclesHouse + s(GovHigh) + s(CountyArea) + s(WaterArea) + 
                               s(Farmland)+ GovRev + s(Employment) + s(PopDensity) + s(HouseDensity) +
                               SCH_TYPE_TEXT + LEVEL + CHARTER_TEXT,
                             data = AllSchools.LIDAR.reduced.others[which(AllSchools.LIDAR.reduced.others$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.reduced.others$SchoolType == "K12Public"),])
summary(LIDAR.reg.PU.state.np) # R2 = 0.29
nobs(LIDAR.reg.PU.state.np) # 23,466


LIDAR.reg.PR.state.np <- gam(EstimatedArea ~ state + s(population) + PercentRural + s(lnMedian.Household.Income) + Poverty.Percent.All.Ages +
                               s(Below18) + s(X18andOver) + s(GovHealth) + s(Physicians) + VehiclesHouse + s(GovHigh) + s(CountyArea) + s(WaterArea) + 
                               s(Farmland) + GovRev + s(Employment) + s(PopDensity) + s(HouseDensity) +
                               s(males) + p_white + s_kg + tothrs + level + relig,
                             data = AllSchools.LIDAR.reduced.others[which(AllSchools.LIDAR.reduced.others$LIDAR == "HaveLIDAR"  & AllSchools.LIDAR.reduced.others$SchoolType == "K12Private"),])
summary(LIDAR.reg.PR.state.np) # R2 = 0.13
nobs(LIDAR.reg.PR.state.np) # 9,237



##############################
####### CROSS-VALIDATION
##############################

# Install required libraries and packages
install.packages("formula.tools", repos = "http://cran.us.r-project.org")
library(formula.tools) 

#################
# Higher education
#################
# re-write the formulas - can't seem to use the factor variables
small_formula = "EstimatedArea ~ lnpopulation + PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev 
+ lnFarmland + lnWaterArea"
large_formula = "EstimatedArea ~ lnpopulation + PercentRural + lnMedian.Household.Income + Poverty.Percent.All.Ages +
lnBelow18 + lnX18andOver + lnGovHealth + lnPhysicians + VehiclesHouse + lnGovHigh + lnCountyArea + lnWaterArea + 
lnFarmland + GovRev + lnEmployment + lnPopDensity + lnHouseDensity + lnAPPLCN"

large_formula = "EstimatedArea ~ PercentRural + lnMedian.Household.Income + lnBelow18 + GovRev + lnFarmland + lnWaterArea + lnpopulation + (1 + lnpopulation | state)"

small_formula <- as.formula(small_formula)
large_formula <- as.formula(large_formula)

formulae <- c(small_formula, large_formula)



# Use response.name function from Cosma lecture notes
response.name <- function(formula) {
  var.names <- all.vars(formula)
  return(var.names[1])
}

# create a clean dataframe just for higher education for this cross-validation
AllSchools.LIDAR.reduced.HEclean <- AllSchools.LIDAR.reduced[which(AllSchools.LIDAR.reduced$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced$LIDAR == "HaveLIDAR"),]
nrow(AllSchools.LIDAR.reduced.HEclean) # 3,035
# Only keep variables used in the cross-validation - to process faster
AllSchools.LIDAR.reduced.HEclean <- AllSchools.LIDAR.reduced.HEclean[,-c(1:9,11:34,36:44,46:49,51:58,60:64,66:80,82:92)]
AllSchools.LIDAR.reduced.HEclean <- AllSchools.LIDAR.reduced.HEclean[,-c(25:28)]


## ----kfold-cv-for-linear-models------------------------------------------
# General function to do k-fold CV for a bunch of linear models
# Inputs: dataframe to fit all models on,
# list or vector of model formulae,
# number of folds of cross-validation
# Output: vector of cross-validated MSEs for the models
cv.lm <- function(data, formulae, nfolds) {
  # Strip data of NA rows
  # ATTN: Better to check whether NAs are in variables used by the models
  data <- na.omit(data)
  # Make sure the formulae have type "formula"
  formulae <- sapply(formulae, as.formula)
  # Extract the name of the response variable from each formula
  # ATTN: CV doesn't make a lot of sense unless these are all the same!
  responses <- sapply(formulae, response.name)
  names(responses) <- as.character(formulae)
  n <- nrow(data)
  # Assign each data point to a fold, at random
  # see ?sample for the effect of sample(x) on a vector x
  fold.labels <- sample(rep(1:nfolds, length.out=n))
  mses <- matrix(NA, nrow=nfolds, ncol=length(formulae))
  colnames <- as.character(formulae)
  
  for (fold in 1:nfolds) {
    test.rows <- which(fold.labels == fold)
    train <- data[-test.rows,]
    test <- data[test.rows,]
    for (form in 1:length(formulae)) {
      # Fit the model on the training data
      current.model <- lm(formula=formulae[[form]], data=train)
      # Generate predictions on the testing data
      predictions <- predict(current.model, newdata=test)
      # Get the responses on the testing data
      test.responses <- test[,responses[form]]
      # Calculate errors
      test.errors <- test.responses - predictions
      # Calculate the MSE on that fold
      mses[fold, form] <- sqrt(mean(test.errors^2))
    }
  }
  return(colMeans(mses))
}

cv.lm(data = AllSchools.LIDAR.reduced.HEclean, formulae = formulae, nfolds = 5) # small = 173,286; large = 168,344.4



## ----checkin how much is within 10%------------------------------------------
# General function to do k-fold CV for a bunch of linear models
# Inputs: dataframe to fit all models on,
# list or vector of model formulae,
# number of folds of cross-validation
# Output: vector of cross-validated MSEs for the models
band.lm <- function(data, formulae, nfolds) {
  # Strip data of NA rows
  # ATTN: Better to check whether NAs are in variables used by the models
  data <- na.omit(data)
  # Make sure the formulae have type "formula"
  formulae <- sapply(formulae, as.formula)
  # Extract the name of the response variable from each formula
  # ATTN: CV doesn't make a lot of sense unless these are all the same!
  responses <- sapply(formulae, response.name)
  names(responses) <- as.character(formulae)
  n <- nrow(data)
  # Assign each data point to a fold, at random
  # see ?sample for the effect of sample(x) on a vector x
  fold.labels <- sample(rep(1:nfolds, length.out=n))
  counts <- matrix(NA, nrow=nfolds, ncol=length(formulae))
  colnames <- as.character(formulae)
  # EXERCISE: Replace the double for() loop below by defining a new
  # function and then calling outer()
  for (fold in 1:nfolds) {
    test.rows <- which(fold.labels == fold)
    train <- data[-test.rows,]
    test <- data[test.rows,]
    for (form in 1:length(formulae)) {
      # Fit the model on the training data
      current.model <- lm(formula=formulae[[form]], data=train)
      # Generate predictions on the testing data
      predictions <- predict(current.model, newdata=test)
      # Get the responses on the testing data
      test.responses <- test[,responses[form]]
      # Calculate number of predictions within 10% of the test.responses
      diff <- abs(test.responses - predictions)/test.responses
      tol <- 1e-5
      count <- sum(diff <= (0.1+tol))
      denom <- length(predictions)
      # Assign the fraction of predictions within 10% of the test.responses for that fold
      counts[fold, form] <- count/denom
    }
  }
  return(test.responses)
}

test <- band.lm(data = AllSchools.LIDAR.reduced.PUclean, formulae = formulae, nfolds = 5)



# do cross-validation for the non-parametric method

## ----kfold-cv-for-non-linear-models------------------------------------------
# General function to do k-fold CV for 6.a. and 6.b smooth additive models
# Inputs: dataframe to fit all models on,
# list or vector of model formulae,
# number of folds of cross-validation
# Output: vector of cross-validated MSEs for the models
cv.gam <- function(data, nfolds) {
  data <- na.omit(data)
  n <- nrow(data)
  
  # Assign each data point to a fold, at random
  fold.labels <- sample(rep(1:nfolds, length.out=n))
  mses <- matrix(NA, nrow=nfolds, ncol=2)
  
  for (fold in 1:nfolds) {
    test.rows <- which(fold.labels == fold)
    train <- data[-test.rows,]
    test <- data[test.rows,]
    
    ### Large Model ###
    current.model.a <- gam(EstimatedArea ~ State + s(population) + PercentRural + s(Median.Household.Income) + Poverty.Percent.All.Ages +
                             s(Below18) + s(X18andOver) + s(GovHealth) + s(Physicians) + VehiclesHouse + s(GovHigh) + s(CountyArea) + s(WaterArea) + 
                             s(Farmland) + s(Cropland) + GovRev + s(Employment) + s(PopDensity) + s(HouseDensity) +
                             SCH_TYPE_TEXT + LEVEL + CHARTER_TEXT, data=train)
    
    # Generate predictions on the testing data
    predictions.a <- predict(current.model.a, newdata=test)
    # Get the responses on the testing data
    test.responses.a <- test[,2]
    # Calculate errors
    test.errors.a <- test.responses.a - predictions.a
    
    ### Small Model ###
    current.model.b <- gam(EstimatedArea ~ State + s(population) + PercentRural + s(Median.Household.Income) + Poverty.Percent.All.Ages, data=train)
    
    # Generate predictions on the testing data
    predictions.b <- predict(current.model.b, newdata=test)
    # Get the responses on the testing data
    test.responses.b <- test[,2]
    # Calculate errors
    test.errors.b <- test.responses.b - predictions.b
    
    # Calculate the MSE on that fold
    mses[fold, 1] <- sqrt(mean(test.errors.a^2))
    mses[fold, 2] <- sqrt(mean(test.errors.b^2))
  }
  return(colMeans(mses, na.rm = TRUE))
}


cv.gam(data = AllSchools.LIDAR.reduced.PUclean, nfolds = 5)




#########################################################
#########################################################
# Write datasets
#########################################################
#########################################################

# public k-12
write.table(PU.NREL, file = "C:/Users/hanusnil/Box Sync/Model Outputs/School Counts and GeoCodes/K12PUEstimate.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")

# private k-12
write.table(PR.NREL, file = "C:/Users/hanusnil/Box Sync/Model Outputs/School Counts and GeoCodes/K12PREstimate.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")

# higher education from the "updated" list
write.table(HE.NREL, file = "C:/Users/hanusnil/Box Sync/Model Outputs/School Counts and GeoCodes/HEEstimate.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")




###########################################################
###########################################################
# BOE calcs for rooftop space (SF) for all school buildings
###########################################################
###########################################################

# CBECs estimates 12.2 million SF for all school buildings in US
# Compare this to our BOE estimate for enrollment & space allotment /person
# First, take unweighted average of the occupant density values
# listed in ASHRAE Standard 62: Ventilation for Acceptable Indoor Air Quality
ASHRAE.PSF <- mean(c(25, 35, 65, 150, 20, 25, 20, 25, 25, 35, 100)) # Number of people/1000 ft^2

# Take weighted average of the number of floors in education buildings
# weighted average based on CBECs data
Floors <- .69*1+.17*2+.09*3+.01*4+.01*5+.01*6+.01*7+.01*8+.01*9

# Define total population across all dataframes
Total.Pop <- sum(PU.NREL$Population, na.rm = TRUE) + sum(PR.NREL$Population, na.rm = TRUE) + sum(HE.NREL$Population, na.rm = TRUE)

# Calculate total educational building square footage
# People / (People/1000SF) x 1000SF
Total.SF <- (Total.Pop / ASHRAE.PSF)*1000
Total.SF/1000000 # 1,590 million SF


############################################################################
### Change point-estimate for density from ASHRAE to CFI point estimate ####
############################################################################

# Campus Facility Inventory SF/buidling for educational buildings:
CFI.SF <- 677633100 # total SF

# CFI Headcount enrollment average:
CFI.Enrollment <- mean(c(10000, 25000))*87 # avg enrollment headcount * number of institutions in survey

# number people / SF
CFI.PSF <- CFI.Enrollment/CFI.SF
Total.SF.CFI <- (Total.Pop / CFI.PSF)
Total.SF.CFI/1000000 # 33,786 million square feet


#########################################################################################################
### Use EPP Regression approach discussed in the report, using Campus Facility Inventory data  ##########
#########################################################################################################

# First, upload the CFI data
# Assume "CFI_to_merge_MJC" is the CFI data used for their regression
CFI <- read.csv("C:/Users/hanusnil/Box Sync/School Databases/Campus Facility Inventory/CFI_to_merge_MJC.csv", header=TRUE)

# Run regression to estimate classroom density for the public/private K-12 schools
K12density <- lm(Classroom.sq.ft ~ Undergraduate.students, data = CFI)
summary(K12density)

new.public <- data.frame(Undergraduate.students = PU.NREL$Population)
new.private <- data.frame(Undergraduate.students = PR.NREL$Population)

PU.NREL$ESF <- predict(K12density, newdata = new.public)
PR.NREL$ESF <- predict(K12density, newdata = new.private)

# Run regression to estimate school density, given multiple SF spaces
CFI$TotalSF <- apply(CFI[,8:23], 1, sum, na.rm = T) # add all SF
CFI$Population <- apply(CFI[,c(2,4,6)], 1, sum, na.rm = T) # add undergrads, grads, and full-time faculty

HEdensity <- lm(TotalSF ~ Population, data = CFI)
summary(HEdensity)

new.HE <- data.frame(Population = HE.NREL$Population) # use FTE and full-time employees for the new data

HE.NREL$ESF <- predict(HEdensity, newdata = new.HE)

Total.ESF <- sum(PU.NREL$ESF, na.rm = TRUE) + sum(PR.NREL$ESF, na.rm = TRUE) + sum(HE.NREL$ESF, na.rm = TRUE)
Total.ESF/1000000 # 6,989 million square feet


#############################################################################################
### Use similar EPP Regression approach discussed in the report, using CBECs data  ##########
#############################################################################################

# First, upload the CBECs micro data
CBECS <- read.csv("C:/Users/hanusnil/Box Sync/School Databases/CBECs Data/2012_public_use_data_aug2016.csv", header=TRUE)

# Run regression to estimate square footage from employees
CBECSdensity <- lm(SQFT ~ NWKER, data = CBECS[which(CBECS$PBA == 14),])

new.public.cbecs <- data.frame(NWKER = as.numeric(PU.NREL$FTE))
new.private.cbecs <- data.frame(NWKER = as.numeric(PR.NREL$Teachers))
new.HE.cbecs <- data.frame(NWKER = as.numeric(HE.NREL$Employees)) # use all employees in the school, not just full-time

PU.NREL$ESF.CBECS <- predict(CBECSdensity, newdata = new.public.cbecs)
PR.NREL$ESF.CBECS <- predict(CBECSdensity, newdata = new.private.cbecs)
HE.NREL$ESF.CBECS <- predict(CBECSdensity, newdata = new.HE.cbecs)

Total.ESF.CBECS <- sum(PU.NREL$ESF.CBECS, na.rm = TRUE) + sum(PR.NREL$ESF.CBECS, na.rm = TRUE) + sum(HE.NREL$ESF.CBECS, na.rm = TRUE)
Total.ESF.CBECS/1000000 # 9,357 million square feet


# Run weighted regression
CBECSdensity.weighted <- lm(SQFT ~ NWKER, data = CBECS[which(CBECS$PBA == 14),], weight = CBECS[which(CBECS$PBA == 14),]$FINALWT)
summary(CBECSdensity.weighted)

PU.NREL$ESF.CBECS.weighted <- predict(CBECSdensity.weighted, newdata = new.public.cbecs)
PR.NREL$ESF.CBECS.weighted <- predict(CBECSdensity.weighted, newdata = new.private.cbecs)
HE.NREL$ESF.CBECS.weighted <- predict(CBECSdensity.weighted, newdata = new.HE.cbecs)

Total.ESF.CBECS.weighted <- sum(PU.NREL$ESF.CBECS.weighted, na.rm = TRUE) + sum(PR.NREL$ESF.CBECS.weighted, na.rm = TRUE) + sum(HE.NREL$ESF.CBECS.weighted, na.rm = TRUE)
Total.ESF.CBECS.weighted/1000000 # 4,543 million square feet


############################################################
############################################################
# BOE calcs for number of buildings for all school buildings
############################################################
############################################################

# CBECs estimates 389,000 education buildings in the US
# compare to total buildings in out three databases
Total.entries <- nrow(PU.NREL) + nrow(PR.NREL) + nrow(HE.NREL) # 136,077
# This value is 63x less than the number estimated by CBECs
# Could be due to multiple buildings on campuses?






