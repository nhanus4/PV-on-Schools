
# Install packages 
install.packages("lubridate")
install.packages("sp")
install.packages("raster")
install.packages("rasterVis")
install.packages("maptools")
install.packages("rgeos")
install.packages("rgdal")
install.packages("ggmap")
install.packages("readxl")
install.packages("dplyr")
install.packages("chron")
install.packages("FinCal")
install.packages("xlsx")
install.packages("grid")
install.packages("ggplot2")
install.packages("plyr")
install.packages("scales")
install.packages("rworldmap")
install.packages("data.table")
install.packages("httr")
install.packages("XML")
install.packages("reshape2")
install.packages("ggthemes")

# add libraries
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
library(ggthemes)
library(rworldmap)
library(data.table)
library(httr)
library(XML)
library(reshape2)

# Set working directory
setwd("C:/Users/hanusnil/Box Sync")

#################################
# Read in relevent raw data files
#################################

# Read in the whol LBNL dataset - OLD DATA
#lbnl <- read.csv(file = "C:/Users/hanusnil/Box Sync/lbnl.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)

# Read in the whol LBNL dataset - NEW DATA
lbnl.r2.p1 <- read.csv(file = "C:/Users/hanusnil/Box Sync/TTSX_LBNL_OpenPV_public_file_p1.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
lbnl.r2.p2 <- read.csv(file = "C:/Users/hanusnil/Box Sync/TTSX_LBNL_OpenPV_public_file_p2.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
lbnl <- rbind(lbnl.r2.p1,lbnl.r2.p2)



# Read the shapes of the eGrid regions
eGrid <- readOGR(dsn = "C:/Users/hanusnil/Box Sync/Regional Shapefiles", layer = "eGRIDshapes")

# EASIUR: Read marginal emissions damages by year ,hour of day, season, and eGrid region
mef.eas <- read.table("C:/Users/hanusnil/Box Sync/Damages/Generation_MAR_DAM-EASIUR_egrid_by-SeasonalTOD_2016.csv", 
                      header = TRUE, sep = ",", na.strings="NA", dec = ".", strip.white = TRUE)

# EASIUR: Read average emissions damages by year ,hour of day, season, and eGrid region
af.eas <- read.table("C:/Users/hanusnil/Box Sync/Damages/Generation_AVG_DAM-EASIUR_egrid_by-SeasonalTOD_2016.csv", 
                     header = TRUE, sep = ",", na.strings="NA", dec = ".", strip.white = TRUE)

# AP2: Read marginal emissions damages by year ,hour of day, season, and eGrid region
mef.ap2 <- read.table("C:/Users/hanusnil/Box Sync/Damages/Generation_MAR_DAM-AP2_egrid_by-SeasonalTOD_2016.csv", 
                      header = TRUE, sep = ",", na.strings="NA", dec = ".", strip.white = TRUE)

# AP2: Read average emissions damages by year ,hour of day, season, and eGrid region
af.ap2 <- read.table("C:/Users/hanusnil/Box Sync/Damages/Generation_AVG_DAM-AP2_egrid_by-SeasonalTOD_2016.csv", 
                     header = TRUE, sep = ",", na.strings="NA", dec = ".", strip.white = TRUE)

# Read in data for which season is which day of year
seasons <- read.table("C:/Users/hanusnil/Box Sync/seasons.csv", 
                      header = TRUE, sep = ",", na.strings="NA", dec = ".", strip.white = TRUE)

# Read in lmp data - 2015
lmp <- read.table("C:/Users/hanusnil/Box Sync/pc_ti_lmp.csv", 
                  header = TRUE, sep = ",", na.strings="NA", dec = ".", strip.white = TRUE)
lmp <- melt(lmp, id = "h")
names(lmp)[names(lmp)=="variable"] <- "state"
names(lmp)[names(lmp)=="value"] <- "lmp"
names(lmp)[names(lmp)=="h"] <- "hoy"
lmp$hoy <- lmp$hoy - 1

# Read in load profiles for schools
# Taken from DOE Commercial Reference Buildings
# Link: https://energy.gov/eere/buildings/commercial-reference-buildings
# Use secondary school, existing buildigs constructed in or after 1980
# Currently using the new construction, can't find post 1980 online
load1 <- list.files("C:/Users/hanusnil/Box Sync/All Ref Schools", recursive = TRUE, full.names = TRUE, pattern = "RefBldgSecondarySchoolNew")
load2 <- list.files("C:/Users/hanusnil/Box Sync/All Ref Schools", recursive = FALSE, full.names = FALSE)
load2 <- gsub("TMY3","",load2)
load2 <- gsub("[^0-9]","",load2)

insolation <- paste("C:/Users/hanusnil/Box Sync/tmy3/",load2,"TY.csv", sep = "")
x <- length(load1) 
length(insolation)

# Read in Zip lookup files
zip_lookup1 <- read.csv(file = "C:/Users/hanusnil/Box Sync/Zip Lookup/zip_lookup.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
zip_lookup2 <- read.csv(file = "C:/Users/hanusnil/Box Sync/Zip Lookup/zip_lookup2.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
zip_lookup <- rbind(zip_lookup1, zip_lookup2)

# Read in FIPS code data
# FIPS code is the 5-digit Federal Information Processing Standard (FIPS) - uniquely identifies counties and county equivalents
zip <- read.csv(file = "C:/Users/hanusnil/Box Sync/ZIP_COUNTY_062016.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)

# Read in Gross private domestic investment (implicit price deflator) https://fred.stlouisfed.org/series/A006RD3A086NBEA 
deflator <- read.csv(file = "C:/Users/hanusnil/Box Sync/deflators 2016.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)


# Read in AllSchools.LIDAR.reduced (only area data)
#AllSchools.LIDAR.reduced.area <- read.csv(file = "C:/Users/hanusnil/Box Sync/AllSchools.LIDAR.reduced - justarea.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
#nrow(AllSchools.LIDAR.reduced.area) # 134,137

# Read in AllSchools.LIDAR.recombined - includes LIDAR estimated and linear regression prediction
AllSchools.LIDAR.reduced.area <- read.csv(file = "C:/Users/hanusnil/Box Sync/AllSchools.LIDAR.reduced.recombined.12.15.17.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
nrow(AllSchools.LIDAR.reduced.area) # 134,135
class(AllSchools.LIDAR.reduced.area$FinalArea) # numeric
summary(AllSchools.LIDAR.reduced.area$FinalArea)

# prices - retail - 2015 values
prices_ret <- read.csv(file = "C:/Users/hanusnil/Box Sync/eia_historical_retail.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)

# Read in rebates by state - on 1.12.18 switched to using state rebates for schools, gov, non-profit in the LBNL TTS dataset
rebates <- read.csv(file = "C:/Users/hanusnil/Box Sync/PV rebates LBNL.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)

# Read in net metering by state
net.metering <- read.csv(file = "C:/Users/hanusnil/Box Sync/netmeteringpolicies July 2017 wo notes.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)



#############################################
# Process LBNL Data for educational buildings
#############################################

# READ IN YEAR OF SYSTEM INSTALLATION
lbnl$year <- as.numeric(substr(lbnl$Installation.Date, nchar(lbnl$Installation.Date)-2+1,nchar(lbnl$Installation.Date)))

# Adjust the numbering so that we have YYYY format
lbnl[which(lbnl$year > 16),]$year <- paste("19",lbnl[which(lbnl$year > 16),]$year, sep = "")
lbnl$year <- as.numeric(lbnl$year)
lbnl[which(lbnl$year < 10),]$year <- paste("200",lbnl[which(lbnl$year < 10),]$year, sep = "")
lbnl$year <- as.numeric(lbnl$year)
lbnl[which(lbnl$year >= 10 & lbnl$year < 1000),]$year <- paste("20",lbnl[which(lbnl$year >= 10 & lbnl$year < 1000),]$year, sep = "")
lbnl$year <- as.numeric(lbnl$year)

# Rename missing data
nrow(lbnl) # 789818
lbnl[lbnl == -9999] <- NA
nrow(lbnl) # 789818

# Examine what the dataset represents - consider only 2015 and 2016 data
nrow(lbnl) # 789,818 entries
nrow(subset(lbnl, year == 2015)) # 195,342 entries
nrow(subset(lbnl, year == 2016)) # 202,044 entries
nrow(lbnl[which(lbnl$year==2015),]) # 195,342 entries
nrow(lbnl[which(lbnl$year==2016),]) # 202,044 entries

# INFLATE THE TOTAL INSTALLED PRICE BY Gross private domestic investment (implicit price deflator) https://fred.stlouisfed.org/series/A006RD3A086NBEA #### 
lbnl <- merge(lbnl, deflator, by = "year", all.x = TRUE)

lbnl$Total.Installed.Price.unadjusted <- lbnl$Total.Installed.Price 
lbnl$Rebate.or.Grant.unadjusted <- lbnl$Rebate.or.Grant 

lbnl$Total.Installed.Price <- lbnl$Total.Installed.Price * lbnl$deflator
lbnl$Rebate.or.Grant <- lbnl$Rebate.or.Grant * lbnl$deflator

# installation costs - dollars per kw
# keep these with underscores since rest of analysis uses them that way
lbnl$price_per_kW <- lbnl$Total.Installed.Price / lbnl$System.Size

# rebates or grants per kw
lbnl$rog_per_kW <- lbnl$Rebate.or.Grant / lbnl$System.Size

# Look at installations over the years
hist(lbnl$year, main = "Installations over Time - All Customer Segments", xlab = "Year")


# need to adjust histograms
# Histogram of school, non-profit, and government project cost $/KW
#hist(lbnl[which(lbnl$Customer.Segment == "SCHOOL" & lbnl$Total.Installed.Price > 0 & lbnl$Total.Installed.Price < 1e+05 & lbnl$price.per.kW < 10e+03 & lbnl$year > 2013) ,]$price_per_kW, 
#    col="red", ylim=c(0,200), breaks = "FD", border =rgb(1,0,0,0.5),
#   xlab = "$/kW", main = "Project Cost - $/kW")
# Add non-residential
#hist(lbnl[which(lbnl$Customer_Segment == "GOV" & lbnl$Total_Installed_Price > 0 & lbnl$Total_Installed_Price < 1e+05 & lbnl$price_per_kW < 10e+03 & lbnl$year > 2013),]$price_per_kW, 
#    col="blue", add = T, breaks = "FD")
#hist(lbnl[which(lbnl$Customer_Segment == "NON-PROFIT" & lbnl$Total_Installed_Price > 0 & lbnl$Total_Installed_Price < 1e+05 & lbnl$price_per_kW < 10e+03 & lbnl$year > 2013),]$price_per_kW, 
#    col="green", add = T, breaks = "FD")
#legend('topright', c("School", "Government", "Non-Profit"), col = c("red", "blue", "green"), lty=c(1,1,1))

# Histogram of school, non-profit, and government rebate or grant $/KW
#hist(lbnl[which(lbnl$Customer_Segment == "SCHOOL" & lbnl$Total_Installed_Price > 0 & lbnl$Total_Installed_Price < 1e+05 & lbnl$price_per_kW < 10e+03 & lbnl$year > 2013) ,]$rog_per_kW, 
#    col="red", ylim=c(0,200), breaks = "FD", border =rgb(1,0,0,0.5),
#   xlab = "$/kW", main = "Rebate or Grant - $/kW")
# Add non-residential
#hist(lbnl[which(lbnl$Customer_Segment == "GOV" & lbnl$Total_Installed_Price > 0 & lbnl$Total_Installed_Price < 1e+05 & lbnl$price_per_kW < 10e+03 & lbnl$year > 2013),]$rog_per_kW, 
#    col="blue", add = T, breaks = "FD")
#hist(lbnl[which(lbnl$Customer_Segment == "NON-PROFIT" & lbnl$Total_Installed_Price > 0 & lbnl$Total_Installed_Price < 1e+05 & lbnl$price_per_kW < 10e+03 & lbnl$year > 2013),]$rog_per_kW, 
#    col="green", add = T, breaks = "FD")
#legend('topright', c("School", "Government", "Non-Profit"), col = c("red", "blue", "green"), lty=c(1,1,1))


# See how many entries we have with this criterion
nrow(subset(lbnl, year == 2015 & Customer.Segment == "SCHOOL")) # 196
nrow(subset(lbnl, year == 2015 & Customer.Segment == "NON-PROFIT")) # 208
nrow(subset(lbnl, year == 2015 & Customer.Segment == "GOV")) # 208
# total: 635
nrow(subset(lbnl, year == 2016 & Customer.Segment == "SCHOOL")) # 262
nrow(subset(lbnl, year == 2016 & Customer.Segment == "NON-PROFIT")) # 350
nrow(subset(lbnl, year == 2016 & Customer.Segment == "GOV")) # 210
# total: 822

count(lbnl[which(lbnl$year==2016),]$Customer.Segment)
count(lbnl[which(lbnl$year==2015),]$Customer.Segment)
nrow(lbnl[which(lbnl$year==2015),]) # 195342
nrow(lbnl[which(lbnl$year==2016),]) # 202044

###########################
# Start cutting LBNL data
###########################

# set a min and max bounds of $/kW
nrow(lbnl[which(lbnl$price_per_kW <= 1000 & lbnl$Customer.Segment == "SCHOOL" & lbnl$year > 2014),]) # 2
nrow(lbnl[which(lbnl$price_per_kW <= 1000 & lbnl$Customer.Segment == "NON-PROFIT" & lbnl$year > 2014),]) # 0
nrow(lbnl[which(lbnl$price_per_kW <= 1000 & lbnl$Customer.Segment == "GOV" & lbnl$year > 2014),]) # 6

nrow(lbnl[which(lbnl$price_per_kW >= 15000 & lbnl$Customer.Segment == "SCHOOL" & lbnl$year > 2014),]) # 0
nrow(lbnl[which(lbnl$price_per_kW >= 15000 & lbnl$Customer.Segment == "NON-PROFIT" & lbnl$year > 2014),]) # 0
nrow(lbnl[which(lbnl$price_per_kW >= 15000 & lbnl$Customer.Segment == "GOV" & lbnl$year > 2014),]) # 0


# ONLY KEEP SCHOOLS, GOV, AND NON-PROFIT
lbnl.keep <- c("SCHOOL", "NON-PROFIT", "GOV")
lbnl <- lbnl[lbnl$Customer.Segment %in% lbnl.keep, ]

# THROW OUT ANYTHING OLDER THAN 2015
lbnl <- lbnl[which(lbnl$year > 2014),]
nrow(lbnl) # 1,434
nrow(lbnl[which(lbnl$price_per_kW >= 15000),]) # 0
nrow(lbnl[which(lbnl$price_per_kW <= 1000),]) # 8
nrow(lbnl[which(is.na(lbnl$price_per_kW)),]) # 380


# THROW OUT ANYTHING GREATER THAN $15000 per kW and LESS THAN $1000 per KW
lbnl <- lbnl[which(lbnl$price_per_kW < 15000),]
lbnl <- lbnl[which(lbnl$price_per_kW > 1000),] # removes NAs as well
nrow(lbnl) # 1,046


# Look at the median project cost $/kw
summary(lbnl$price_per_kW) # med = 3500/kw; mean = 3837/kw
summary(lbnl$Total.Installed.Price) # med = 128800; mean = 559100

# Look at the number of projects that received a rebate or grant greater than $0/kw
nrow(lbnl[which(lbnl$rog_per_kW > 0),]) # 618
summary(lbnl$rog_per_kW) # mean is $753/kw; me = 532/kW
summary(lbnl$Rebate.or.Grant) # med = 7260; mean = 33260
# Look at states that have projects with rebates
count(lbnl[which(lbnl$rog_per_kW > 0),]$State) # CA, DE, MA, NH, NV, NY, TX, VT, WI
# make a dataframe of the average $/kW rebate observed in each state
state.rebates <- ddply(lbnl, ~State, summarize, 
                       rog_per_kW.mu = mean(rog_per_kW, na.rm = TRUE))

# count total number of states considered here
nrow(lbnl[!duplicated(lbnl[,"State"]),]) # 11
count(lbnl, "Appraised.Value.Flag") # 158

# Plot the overall project cost and rebates ($/kW), with a mean
lbnl$price_per_kW.mu <- mean(lbnl$price_per_kW, na.rm = TRUE)
lbnl$rog_per_kW.mu <- mean(lbnl$rog_per_kW, na.rm = TRUE) 

m <- ggplot(data = lbnl, aes(price_per_kW))
m + geom_area(stat = "bin", color = "firebrick", fill = "firebrick1") +
  labs(title= "Total Project Cost for Schools, Government, and Non-Profit", x = "Project Cost ($/kW)") +
  scale_x_continuous(labels = comma, breaks = round(seq(0, 14000, by = 2000),0)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text=element_text(size = 12, angle = 90, hjust = 1), axis.title=element_text(size = 14, face="bold")) +
  theme(title= element_text(size = 14, hjust = 0.5, vjust = 1, face= c("bold"))) +
  geom_vline(data=lbnl, aes(xintercept=price_per_kW.mu),
             linetype="dashed")


m <- ggplot(data = lbnl, aes(rog_per_kW))
m + geom_area(stat = "bin", color = "dodgerblue4", fill = "dodgerblue") +
  labs(title= "Rebates or Grants for Schools, Government, and Non-Profit", x = "Rebate or Grant ($/kW)") +
  scale_x_continuous(labels = comma, breaks = round(seq(0, 5500, by = 500),0)) +
  scale_y_continuous(labels = comma) +
  theme(axis.text=element_text(size = 12, angle = 90, hjust = 1), axis.title=element_text(size = 14, face="bold")) +
  theme(title= element_text(size = 14, hjust = 0.5, vjust = 1, face= c("bold"))) +
  geom_vline(data=lbnl, aes(xintercept=rog_per_kW.mu),
             linetype="dashed")


# Look at distributions of price and rebate for the following state:
# CA, NY, TX, PA compared to US average
lbnl.keep2 <- c("CA", "NY", "TX", "OH")
lbnl.trim <- lbnl[lbnl$State %in% lbnl.keep2, ]

# Make a df with the state averages of price and rebate
mu <- ddply(lbnl.trim, ~State, summarize, 
            price_per_kW.mu = mean(price_per_kW),
            rog_per_kW.mu = mean(rog_per_kW))

# not enough projects to make plots accross states
m <- ggplot(data = lbnl.trim, aes(rog_per_kW))
m + geom_area(stat = "bin") +
  facet_grid(State ~ .)




# don't need to run 325 - 880

##########################################
# Process marginal and avg emissions data
##########################################

# EASIUR

# Marginal
# Average data for different years, so that it is now only by hour of day, season, and eGrid region
mef.eas_sum <- ddply(mef.eas, c("region", "season", "hour", "pollutant", "year"), summarise, mean = mean(mf)) 

# Put the different pollutants into columns
mef.eas_sum <- dcast(mef.eas_sum, region + season + hour + year ~ pollutant, value.var = "mean")
names(mef.eas_sum)[names(mef.eas_sum)=="region"] <- "eGrid"

# Create dataframe to store emissions reductions in 
reductions_eas.mar <- data.frame(year = numeric(0), state = character(), lat = numeric(0), lon = numeric(0), power = numeric(0), net_power = numeric(0),
                                 co2_dam = numeric(0), nox_dam_eas = numeric(0), pm25_dam_eas = numeric(0), so2_dam_eas = numeric(0), lmp = numeric(0))

# Average 
# Average data for different years, so that it is now only by hour of day, season, and eGrid region
af.eas_sum <- ddply(af.eas, c("region", "season", "hour", "pollutant", "year"), summarise, mean = mean(avg)) 

# Put the different pollutants into columns
af.eas_sum <- dcast(af.eas_sum, region + season + hour + year ~ pollutant, value.var = "mean")
names(af.eas_sum)[names(af.eas_sum)=="region"] <- "eGrid"

# Create dataframe to store emissions reductions in, for average
reductions_eas.avg <- data.frame(year = numeric(0), state = character(), lat = numeric(0), lon = numeric(0), power = numeric(0), net_power = numeric(0),
                                 co2_dam = numeric(0), nox_dam_eas = numeric(0), pm25_dam_eas = numeric(0), so2_dam_eas = numeric(0), lmp = numeric(0))


# AP2

# Marginal
# Average data for different years, so that it is now only by hour of day, season, and eGrid region
mef.ap2_sum <- ddply(mef.ap2, c("region", "season", "hour", "pollutant", "year"), summarise, mean = mean(mf)) # use "mf" instead of "avg" if using marginal emission factors

# Put the different pollutants into columns
mef.ap2_sum <- dcast(mef.ap2_sum, region + season + hour + year ~ pollutant, value.var = "mean")
names(mef.ap2_sum)[names(mef.ap2_sum)=="region"] <- "eGrid"

# Create dataframe to store emissions reductions in 
reductions_ap2.mar <- data.frame(year = numeric(0), state = character(), lat = numeric(0), lon = numeric(0), power = numeric(0), net_power = numeric(0),
                                 co2_dam = numeric(0), nox_dam_ap2 = numeric(0), pm25_dam_ap2 = numeric(0), so2_dam_ap2 = numeric(0), lmp = numeric(0))

# Average 
# Average data for different years, so that it is now only by hour of day, season, and eGrid region
af.ap2_sum <- ddply(af.ap2, c("region", "season", "hour", "pollutant", "year"), summarise, mean = mean(avg)) # use "mf" instead of "avg" if using marginal emission factors

# Put the different pollutants into columns
af.ap2_sum <- dcast(af.ap2_sum, region + season + hour + year ~ pollutant, value.var = "mean")
names(af.ap2_sum)[names(af.ap2_sum)=="region"] <- "eGrid"

# Create dataframe to store emissions reductions in, for average
reductions_ap2.avg <- data.frame(year = numeric(0), state = character(), lat = numeric(0), lon = numeric(0), power = numeric(0), net_power = numeric(0),
                                 co2_dam = numeric(0), nox_dam_ap2 = numeric(0), pm25_dam_ap2 = numeric(0), so2_dam_ap2 = numeric(0), lmp = numeric(0))


###############################################
# Set global parameters for the Solar functions
###############################################

# US east time zone relative to UTC
deltaTGMT_NYT <- 5



###############################################
# Define solar functions
###############################################

########################################################################################
# Function to calculate reductions for EASIUR
# Input: r = hour/time?, load = DOE load data, ins = site-specific insolation data
# Output: Total power (MWh/yr), Net Power (MWh/yr), co2 and pollution damages ($/kW/yr), 
# weighted lmp ($MWh/yr) for each TMY location aggregated for each year from 2006 - 2014
########################################################################################

solar.eas <- function (r, load, ins,ef_sum){
  
  mef_summary <- ef_sum[which(ef_sum$year == r),] # Select rows corresponding to certain year
  
  site <- read.table(file = ins, header = FALSE, sep = ",", na.strings="NA", dec = ".", strip.white = TRUE, nrows = 1)
  
  latitude <- site[5]
  longitude <- site[6]
  deltaTGMT <- site[4]
  state <- site[1,3]
  siteno <- site[1,1]
  
  names(site)[5] <- "lat"
  names(site)[6] <- "lon"
  
  # Tell R that lon and lat are coordinates
  coordinates(site) <- c("lon", "lat")
  
  
  # Tell R that the projection used in the eGrid regions is identical to that used to derive the location
  proj4string(site) <- proj4string(eGrid)
  
  # Find out what eGrid region that combination of lon and lat corresponds to
  site$eGrid <- over(site,eGrid)$NAME
  
  # Feed in actual solar data
  data <- read.table(file = ins, header = FALSE, sep = ",", na.strings="NA", dec = ".", strip.white = TRUE, skip = 2)
  
  names(data)[1] <- "date"
  names(data)[2] <- "time"
  names(data)[8] <- "DNI"
  names(data)[11] <- "DHI"
  
  loadprofile <- read.table(file =  load, header = TRUE, sep = ",", na.strings="NA", dec = ".", strip.white = TRUE)
  
  # Combine date and time
  data$timestamp <- paste(data$date, data$time, sep = " ")
  
  # Convert timestamp to POSIXct object
  data$timestamp <- as.POSIXct(data$timestamp, format = '%m/%d/%Y %H:%M', tz = "UTC")
  loadprofile$Date.Time <- as.POSIXct(loadprofile$Date.Time, format = '%m/%d %H:%M:%S', tz = "UTC")
  
  # Convert time to time, setting the time zone to eastern time
  data$hour <- hour(data$timestamp)
  loadprofile$hour <- hour(loadprofile$Date.Time)
  
  # Define a day of year that goes from 1-366
  data$doy <- as.numeric(strftime(data$timestamp, format = "%j", tz = "UTC"))
  loadprofile$doy <- as.numeric(strftime(loadprofile$Date.Time, format = "%j", tz = "UTC"))
  
  # Calculate total load
  names(loadprofile)[2] <- "load"
  
  data <- data[c("date","timestamp","hour","doy","DNI","DHI")]
  loadprofile <- loadprofile[c("hour","doy","load")]
  # unique(data$doy)
  # check2 <- data[c("doy","hour")]
  # check2 <- ddply(check2, ~doy, summarise, length(hour))
  data <- merge(data, loadprofile, by = c("hour","doy"), match = "all") # site-specific merge
  
  
  
  
  
  # Calculate the declination in radians
  data$delta = (23.45*sin((360/365)*(284 + data$doy)*pi/180))*pi/180
  
  # Set beta = longitude...this can be changed so that beta is passed to the function
  beta = latitude[1,1] # in degrees
  beta_r = beta * pi / 180 # in radians
  
  # Set azimuth to 180...this can be changed so that azimuth is passed to the function
  psi <- 0 * pi / 180
  
  # Set latitude in radians
  phi <- latitude[1,1] * pi / 180
  
  # Calculate angle of elevation in radians
  data$alpha <- pi/2 - phi + data$delta
  
  # Calculate the hour angle
  lstm <- 15 * deltaTGMT[1,1]
  data$b <- 360/365*(data$doy - 81) * pi / 180
  data$eot <- 9.87*sin(2*data$b) - 7.53*cos(data$b) - 1.5*sin(data$b)
  data$tc <- 4 * (longitude[1,1] - lstm) + data$eot # Data TC is in minutes
  
  data$lst <- data$timestamp + data$tc * 60 # Time stam needs to be adjusted in seconds
  
  data$noon <- paste(data$date, "12:00:00", sep = " ")
  data$noon <- as.POSIXct(data$noon, format = '%m/%d/%Y %H:%M', tz = "UTC")
  
  data$hra <- 15 * as.numeric(difftime(data$lst, data$noon, units = "hours")) * pi / 180
  
  data$beam <- data$DNI * (sin(data$delta) * sin(phi) * cos(beta_r) -
                             sin(data$delta) * cos(phi) * sin(beta_r) * cos(psi) +
                             cos(data$delta) * cos(phi) * cos(beta_r) * cos(data$hra) +
                             cos(data$delta) * sin(phi) * sin(beta_r) * cos(psi) * cos(data$hra) +
                             cos(data$delta) * sin(psi) * sin(data$hra) * sin(beta_r))
  
  # Since the time correction factor used in calculating the hour angle is an approximation, the equation above produces small negative
  # Values for beam power during early mornings and evenings in deep winter. Clearly, this is "noise", so such values are set to zero
  data$beam <- pmax(0,data$beam)
  
  data$diffuse <- data$DHI * (180 - beta) / 180
  
  # This is the total power generated in MW by a 1kW panel each hour of each day
  data$power <- (data$beam + data$diffuse) / 1e6 # in MW
  data$load <- data$load / 1000 #load in MW for that hour
  
  med_sys <- max(data$load)*.8* 1000 # in kW
  
  data$net_power <- pmax(0, (data$power * med_sys - data$load) / med_sys) # This line would change so that we calculate net_powerfor each building?
  
  # To integrate with the marginal cost / emissions data, switch time stamp to the time in New York
  data$timestamp <- as.POSIXct(data$timestamp, format = '%m/%d/%Y %H:%M', tz = "UTC") + (-1*deltaTGMT[1] - deltaTGMT_NYT)[1,1]*3600
  data$hour <- hour(data$timestamp)
  data$doy <- as.numeric(strftime(data$timestamp, format = "%j", tz = "UTC"))
  
  # Add in seasonal data
  data <- merge(data, seasons, by = "doy", match = "all")
  
  # Subset those lines of the mef or marginal damages data that are in the appropriate eGrid region
  emissions <- mef_summary[which(mef_summary$eGrid == as.character(site@data$eGrid[1])),]
  
  # Add in the marginal emissions information to the data
  data <- merge(data, emissions, by = c("hour", "season"), match = "all")
  
  # ically
  data$hoy <- ((data$doy - 1)*24 + data$hour)%%8760
  state <- factor(state[1], levels = levels(lmp$state))
  lmp_state <- lmp[which(lmp$state == state[1]),]
  data <- merge(data, lmp_state, by = "hoy", all.x = FALSE, all.y = FALSE)
  
  # calculate the magnitude of damage
  data <- data[c("power", "net_power", "co2_dam", "nox_dam_eas", "pm25_dam_eas", "so2_dam_eas", "lmp")]
  
  lmp <- sum(data$net_power * data$lmp) / sum(data$net_power)
  power <- sum(data$power)
  net_power <- sum(data$net_power)
  co2_dam <- sum(data$co2_dam * data$power)
  nox_dam_eas <- sum(data$nox_dam_eas * data$power)
  pm25_dam_eas <- sum(data$pm25_dam_eas * data$power)
  so2_dam_eas <- sum(data$so2_dam_eas * data$power)
  
  red <- data.frame(year = r, state = state, lat = latitude[1,1], lon = longitude[1,1], power = power, net_power = net_power,
                    co2_dam = co2_dam, nox_dam_eas = nox_dam_eas, pm25_dam_eas = pm25_dam_eas, so2_dam_eas = so2_dam_eas, lmp = lmp)
  
  return(red)
  
}

########################################################################################
# Function to calculate reductions for AP2
# Input: r = hour/time?, load = DOE load data, ins = site-specific insolation data
# Output: Total power (MWh/yr), Net Power (MWh/yr), co2 and pollution damages ($/kW/yr), 
# weighted lmp ($MWh/yr) for each TMY location aggregated for each year from 2006 - 2014
########################################################################################

solar.ap2 <- function (r, load, ins, ef_sum){
  
  mef_summary <- ef_sum[which(ef_sum$year == r),] # Select rows corresponding to certain year
  
  site <- read.table(file = ins, header = FALSE, sep = ",", na.strings="NA", dec = ".", strip.white = TRUE, nrows = 1)
  
  latitude <- site[5]
  longitude <- site[6]
  deltaTGMT <- site[4]
  state <- site[1,3]
  siteno <- site[1,1]
  
  names(site)[5] <- "lat"
  names(site)[6] <- "lon"
  
  # Tell R that lon and lat are coordinates
  coordinates(site) <- c("lon", "lat")
  
  
  # Tell R that the projection used in the eGrid regions is identical to that used to derive the location
  proj4string(site) <- proj4string(eGrid)
  
  # Find out what eGrid region that combination of lon and lat corresponds to
  site$eGrid <- over(site,eGrid)$NAME
  
  # Feed in actual solar data
  data <- read.table(file = ins, header = FALSE, sep = ",", na.strings="NA", dec = ".", strip.white = TRUE, skip = 2)
  
  names(data)[1] <- "date"
  names(data)[2] <- "time"
  names(data)[8] <- "DNI"
  names(data)[11] <- "DHI"
  
  loadprofile <- read.table(file =  load, header = TRUE, sep = ",", na.strings="NA", dec = ".", strip.white = TRUE)
  
  # Combine date and time
  data$timestamp <- paste(data$date, data$time, sep = " ")
  
  # Convert timestamp to POSIXct object
  data$timestamp <- as.POSIXct(data$timestamp, format = '%m/%d/%Y %H:%M', tz = "UTC")
  loadprofile$Date.Time <- as.POSIXct(loadprofile$Date.Time, format = '%m/%d %H:%M:%S', tz = "UTC")
  
  # Convert time to time, setting the time zone to eastern time
  data$hour <- hour(data$timestamp)
  loadprofile$hour <- hour(loadprofile$Date.Time)
  
  # Define a day of year that goes from 1-366
  data$doy <- as.numeric(strftime(data$timestamp, format = "%j", tz = "UTC"))
  loadprofile$doy <- as.numeric(strftime(loadprofile$Date.Time, format = "%j", tz = "UTC"))
  
  # Calculate total load
  names(loadprofile)[2] <- "load"
  
  data <- data[c("date","timestamp","hour","doy","DNI","DHI")]
  loadprofile <- loadprofile[c("hour","doy","load")]
  # unique(data$doy)
  # check2 <- data[c("doy","hour")]
  # check2 <- ddply(check2, ~doy, summarise, length(hour))
  data <- merge(data, loadprofile, by = c("hour","doy"), match = "all") # site-specific merge
  
  # Calculate the declination in radians
  data$delta = (23.45*sin((360/365)*(284 + data$doy)*pi/180))*pi/180
  
  # Set beta = longitude...this can be changed so that beta is passed to the function
  beta = latitude[1,1] # in degrees
  beta_r = beta * pi / 180 # in radians
  
  # Set azimuth to 180...this can be changed so that azimuth is passed to the function
  psi <- 0 * pi / 180
  
  # Set latitude in radians
  phi <- latitude[1,1] * pi / 180
  
  # Calculate angle of elevation in radians
  data$alpha <- pi/2 - phi + data$delta
  
  # Calculate the hour angle
  lstm <- 15 * deltaTGMT[1,1]
  data$b <- 360/365*(data$doy - 81) * pi / 180
  data$eot <- 9.87*sin(2*data$b) - 7.53*cos(data$b) - 1.5*sin(data$b)
  data$tc <- 4 * (longitude[1,1] - lstm) + data$eot # Data TC is in minutes
  
  data$lst <- data$timestamp + data$tc * 60 # Time stam needs to be adjusted in seconds
  
  data$noon <- paste(data$date, "12:00:00", sep = " ")
  data$noon <- as.POSIXct(data$noon, format = '%m/%d/%Y %H:%M', tz = "UTC")
  
  data$hra <- 15 * as.numeric(difftime(data$lst, data$noon, units = "hours")) * pi / 180
  
  data$beam <- data$DNI * (sin(data$delta) * sin(phi) * cos(beta_r) -
                             sin(data$delta) * cos(phi) * sin(beta_r) * cos(psi) +
                             cos(data$delta) * cos(phi) * cos(beta_r) * cos(data$hra) +
                             cos(data$delta) * sin(phi) * sin(beta_r) * cos(psi) * cos(data$hra) +
                             cos(data$delta) * sin(psi) * sin(data$hra) * sin(beta_r))
  
  # Since the time correction factor used in calculating the hour angle is an approximation, the equation above produces small negative
  # Values for beam power during early mornings and evenings in deep winter. Clearly, this is "noise", so such values are set to zero
  data$beam <- pmax(0,data$beam)
  
  data$diffuse <- data$DHI * (180 - beta) / 180
  
  # This is the total power generated in MWh by a 1kW panel each hour of each day
  data$power <- (data$beam + data$diffuse) / 1e6
  data$load <- data$load / 1000 #load in MW in that hour
  med_sys <- max(data$load)*.8* 1000 # in kW
  data$net_power <- pmax(0, (data$power * med_sys - data$load) / med_sys) # This line would change so that we calculate net_powerfor each building?
  
  # To integrate with the marginal cost / emissions data, switch time stamp to the time in New York
  data$timestamp <- as.POSIXct(data$timestamp, format = '%m/%d/%Y %H:%M', tz = "UTC") + (-1*deltaTGMT[1] - deltaTGMT_NYT)[1,1]*3600
  data$hour <- hour(data$timestamp)
  data$doy <- as.numeric(strftime(data$timestamp, format = "%j", tz = "UTC"))
  
  # Add in seasonal data
  data <- merge(data, seasons, by = "doy", match = "all")
  
  # Subset those lines of the mef or marginal damages data that are in the appropriate eGrid region
  emissions <- mef_summary[which(mef_summary$eGrid == as.character(site@data$eGrid[1])),]
  
  # Add in the marginal emissions information to the data
  data <- merge(data, emissions, by = c("hour", "season"), match = "all")
  
  # ically
  data$hoy <- ((data$doy - 1)*24 + data$hour)%%8760
  state <- factor(state[1], levels = levels(lmp$state))
  lmp_state <- lmp[which(lmp$state == state[1]),]
  data <- merge(data, lmp_state, by = "hoy", all.x = FALSE, all.y = FALSE)
  
  # calculate the magnitude of damage
  data <- data[c("power", "net_power", "co2_dam", "nox_dam_ap2", "pm25_dam_ap2", "so2_dam_ap2", "lmp")]
  
  lmp <- sum(data$net_power * data$lmp) / sum(data$net_power)
  power <- sum(data$power)
  net_power <- sum(data$net_power)
  co2_dam <- sum(data$co2_dam * data$power)
  nox_dam_ap2 <- sum(data$nox_dam_ap2 * data$power)
  pm25_dam_ap2 <- sum(data$pm25_dam_ap2 * data$power)
  so2_dam_ap2 <- sum(data$so2_dam_ap2 * data$power)
  
  red <- data.frame(year = r, state = state, lat = latitude[1,1], lon = longitude[1,1], power = power, net_power = net_power,
                    co2_dam = co2_dam, nox_dam_ap2 = nox_dam_ap2, pm25_dam_ap2 = pm25_dam_ap2, so2_dam_ap2 = so2_dam_ap2, lmp = lmp)
  
  return(red)
  
}


###################################################################
# Run Solar function for avg/mar emissions values using both models
###################################################################

# EASIUR

# Marginal
# Calculate reductions in emissions due to a 1kW system
# for (j in min(mef.eas_sum$year):max(mef.eas_sum$year)){
#  for (i in 1:x){
#   counter <- paste(i,"of",x, sep = " ")
#  print(counter)
# reductions_eas.mar <- rbind(reductions_eas.mar, solar.eas(j, load1[i], insolation[i], ef_sum = mef.eas_sum))  
#}
#}

# Write file to .csv
# Label "_2016" since we are using the latest damages
# write.table(reductions_eas.mar, file = "C:/Users/hanusnil/Box Sync/Calculated Reductions/reductions_eas_MAR_LMP_2016 - NLH.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")


# Average
# Calculate reductions in emissions due to a 1kW system
#for (j in min(af.eas_sum$year):max(af.eas_sum$year)){
# for (i in 1:x){
#  counter <- paste(i,"of",x, sep = " ")
# print(counter)
# reductions_eas.avg <- rbind(reductions_eas.avg, solar.eas(j, load1[i], insolation[i], ef_sum = af.eas_sum))  
# }
# }

# Write file to .csv
# Label "_2016" since we are using the latest damages
# write.table(reductions_eas.avg, file = "C:/Users/hanusnil/Box Sync/Calculated Reductions/reductions_eas_AVG_LMP_2016 - NLH.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")





# AP2

# Marginal
# Calculate reductions in emissions due to a 1kW system
# for (j in min(mef.ap2_sum$year):max(mef.ap2_sum$year)){
# for (i in 1:x){
# counter <- paste(i,"of",x, sep = " ")
# print(counter)
# reductions_ap2.mar <- rbind(reductions_ap2.mar, solar.ap2(j, load1[i], insolation[i], ef_sum = mef.ap2_sum))  
# }
# }

# Write file to .csv
# Label "_2016" since we are using the latest damages
# write.table(reductions_ap2.mar, file = "C:/Users/hanusnil/Box Sync/Calculated Reductions/reductions_ap2_MAR_LMP_2016 - NLH.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")



# Average
# Calculate reductions in emissions due to a 1kW system
# for (j in min(af.ap2_sum$year):max(af.ap2_sum$year)){
#  for (i in 1:x){
#   counter <- paste(i,"of",x, sep = " ")
#  print(counter)
#  reductions_ap2.avg <- rbind(reductions_ap2.avg, solar.ap2(j, load1[i], insolation[i], ef_sum = af.ap2_sum))  
#}
#}

# Write file to .csv
# Label "_2016" since we are using the latest damages
# write.table(reductions_ap2.avg, file = "C:/Users/hanusnil/Box Sync/Calculated Reductions/reductions_ap2_AVG_LMP_2016 - NLH.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")



############################################
# Merge calculated reduction damages locations with zip locations
############################################

# Clean the zip_lookup file
zip_lookup$Zip_Code <- as.numeric(zip_lookup$Zip_Code)
zip_lookup$lat <- as.numeric(zip_lookup$lat)
zip_lookup$lon <- as.numeric(zip_lookup$lon)

# Identify the nearest zips from the emissions datasets


## EASIUR
# Marginal
locations.mar.eas <- read.csv(file = "C:/Users/hanusnil/Box Sync/Calculated Reductions/reductions_eas_MAR_LMP_2016 - NLH.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
locations_latlon.mar.eas <- locations.mar.eas[,c("lat","lon")]
locations_latlon.mar.eas <- unique(locations_latlon.mar.eas)

locations_latlon_sp.mar.eas <- SpatialPoints(locations_latlon.mar.eas)
zip_lat_lon <- zip_lookup[,c("lat","lon")]
zip_lookup_sp <- SpatialPoints(zip_lat_lon)

zip_lookup$nearest_tmy <- apply(gDistance(locations_latlon_sp.mar.eas, zip_lookup_sp, byid = TRUE), 1, which.min)
locations_latlon.mar.eas$seq <- seq.int(nrow(locations_latlon.mar.eas))

locations.mar.eas <- merge(locations.mar.eas, locations_latlon.mar.eas, by = c("lat", "lon"), match = "all")
locations.mar.eas$pol_dam <- locations.mar.eas$nox_dam_eas + locations.mar.eas$so2_dam_eas + locations.mar.eas$pm25_dam_eas
locations.mar.eas <- unique(locations.mar.eas)
zip_lookup.mar.eas <- merge(zip_lookup, locations.mar.eas, by.x = "nearest_tmy", by.y = "seq", match = "all")

# Write file to .csv
write.table(zip_lookup.mar.eas, file = "C:/Users/hanusnil/Box Sync/Zip Lookup and Damages/zip_lookup.mar.eas.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")


# Average
locations.avg.eas <- read.csv(file = "C:/Users/hanusnil/Box Sync/Calculated Reductions/reductions_eas_AVG_LMP_2016 - NLH.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
locations_latlon.avg.eas <- locations.avg.eas[,c("lat","lon")]
locations_latlon.avg.eas <- unique(locations_latlon.avg.eas)

locations_latlon_sp.avg.eas <- SpatialPoints(locations_latlon.avg.eas)
zip_lat_lon <- zip_lookup[,c("lat","lon")]
zip_lookup_sp <- SpatialPoints(zip_lat_lon)

zip_lookup$nearest_tmy <- apply(gDistance(locations_latlon_sp.avg.eas, zip_lookup_sp, byid = TRUE), 1, which.min)
locations_latlon.avg.eas$seq <- seq.int(nrow(locations_latlon.avg.eas))

locations.avg.eas <- merge(locations.avg.eas, locations_latlon.avg.eas, by = c("lat", "lon"), match = "all")
locations.avg.eas$pol_dam <- locations.avg.eas$nox_dam_eas + locations.avg.eas$so2_dam_eas + locations.avg.eas$pm25_dam_eas
locations.avg.eas <- unique(locations.avg.eas)
zip_lookup.avg.eas <- merge(zip_lookup, locations.avg.eas, by.x = "nearest_tmy", by.y = "seq", match = "all")

# Write file to .csv
write.table(zip_lookup.avg.eas, file = "C:/Users/hanusnil/Box Sync/Zip Lookup and Damages/zip_lookup.avg.eas.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")




## AP2
# Marginal
locations.mar.ap2 <- read.csv(file = "C:/Users/hanusnil/Box Sync/Calculated Reductions/reductions_ap2_MAR_LMP_2016 - NLH.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
locations_latlon.mar.ap2 <- locations.mar.ap2[,c("lat","lon")]
locations_latlon.mar.ap2 <- unique(locations_latlon.mar.ap2)

locations_latlon_sp.mar.ap2 <- SpatialPoints(locations_latlon.mar.ap2)
zip_lat_lon <- zip_lookup[,c("lat","lon")]
zip_lookup_sp <- SpatialPoints(zip_lat_lon)

zip_lookup$nearest_tmy <- apply(gDistance(locations_latlon_sp.mar.ap2, zip_lookup_sp, byid = TRUE), 1, which.min)
locations_latlon.mar.ap2$seq <- seq.int(nrow(locations_latlon.mar.ap2))

locations.mar.ap2 <- merge(locations.mar.ap2, locations_latlon.mar.ap2, by = c("lat", "lon"), match = "all")
locations.mar.ap2$pol_dam <- locations.mar.ap2$nox_dam_ap2 + locations.mar.ap2$so2_dam_ap2 + locations.mar.ap2$pm25_dam_ap2
locations.mar.ap2 <- unique(locations.mar.ap2)
zip_lookup.mar.ap2 <- merge(zip_lookup, locations.mar.ap2, by.x = "nearest_tmy", by.y = "seq", match = "all")

# Write file to .csv
write.table(zip_lookup.mar.ap2, file = "C:/Users/hanusnil/Box Sync/Zip Lookup and Damages/zip_lookup.mar.ap2.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")




# Marginal
locations.avg.ap2 <- read.csv(file = "C:/Users/hanusnil/Box Sync/Calculated Reductions/reductions_ap2_AVG_LMP_2016 - NLH.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
locations_latlon.avg.ap2 <- locations.avg.ap2[,c("lat","lon")]
locations_latlon.avg.ap2 <- unique(locations_latlon.avg.ap2)

locations_latlon_sp.avg.ap2 <- SpatialPoints(locations_latlon.avg.ap2)
zip_lat_lon <- zip_lookup[,c("lat","lon")]
zip_lookup_sp <- SpatialPoints(zip_lat_lon)

zip_lookup$nearest_tmy <- apply(gDistance(locations_latlon_sp.avg.ap2, zip_lookup_sp, byid = TRUE), 1, which.min)
locations_latlon.avg.ap2$seq <- seq.int(nrow(locations_latlon.avg.ap2))

locations.avg.ap2 <- merge(locations.avg.ap2, locations_latlon.avg.ap2, by = c("lat", "lon"), match = "all")
locations.avg.ap2$pol_dam <- locations.avg.ap2$nox_dam_ap2 + locations.avg.ap2$so2_dam_ap2 + locations.avg.ap2$pm25_dam_ap2
locations.avg.ap2 <- unique(locations.avg.ap2)
zip_lookup.avg.ap2 <- merge(zip_lookup, locations.avg.ap2, by.x = "nearest_tmy", by.y = "seq", match = "all")

# Write file to .csv
write.table(zip_lookup.avg.ap2, file = "C:/Users/hanusnil/Box Sync/Zip Lookup and Damages/zip_lookup.avg.ap2.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")

############################################
# Read in the zip_lookup files
############################################

zip_lookup.mar.eas <- read.csv(file = "C:/Users/hanusnil/Box Sync/Zip Lookup and Damages/zip_lookup.mar.eas.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
zip_lookup.avg.eas <- read.csv(file = "C:/Users/hanusnil/Box Sync/Zip Lookup and Damages/zip_lookup.avg.eas.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
zip_lookup.mar.ap2 <- read.csv(file = "C:/Users/hanusnil/Box Sync/Zip Lookup and Damages/zip_lookup.mar.ap2.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
zip_lookup.avg.ap2 <- read.csv(file = "C:/Users/hanusnil/Box Sync/Zip Lookup and Damages/zip_lookup.avg.ap2.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)

############################################
# Process the LIDAR school data
############################################

# Need to turn available roof space into PV system size
# 1 ft = 3.28084 m
View(AllSchools.LIDAR.reduced.area)
names(AllSchools.LIDAR.reduced.area)[17] <- "Total.Space"
# Only keep rows with total space - remove NA values
nrow(AllSchools.LIDAR.reduced.area[is.na(AllSchools.LIDAR.reduced.area$Total.Space),]) # 6895
nrow(AllSchools.LIDAR.reduced.area) # 134135
# Treat the NA values as 0 SF
AllSchools.LIDAR.reduced.area.LIDAR <- AllSchools.LIDAR.reduced.area
nrow(AllSchools.LIDAR.reduced.area.LIDAR) # 134135
AllSchools.LIDAR.reduced.area.LIDAR[is.na(AllSchools.LIDAR.reduced.area.LIDAR$Total.Space),]$Total.Space <- 0
nrow(AllSchools.LIDAR.reduced.area.LIDAR[is.na(AllSchools.LIDAR.reduced.area.LIDAR$Total.Space),]) # 0
View(AllSchools.LIDAR.reduced.area.LIDAR)
names(AllSchools.LIDAR.reduced.area.LIDAR)[7] <- "State"


power.density <- 0.15 # kW/m^2, taken from NREL Technical Potential of US
AllSchools.LIDAR.reduced.area.LIDAR$System_Size <- AllSchools.LIDAR.reduced.area.LIDAR$Total.Space * (1/3.28084^2) * power.density # kW
nrow(AllSchools.LIDAR.reduced.area.LIDAR[is.na(AllSchools.LIDAR.reduced.area.LIDAR$System_Size),]) # 0
View(AllSchools.LIDAR.reduced.area.LIDAR)
names(AllSchools.LIDAR.reduced.area.LIDAR)[2] <- "Zip_Code"
nrow(AllSchools.LIDAR.reduced.area.LIDAR) # 134,135
summary(AllSchools.LIDAR.reduced.area.LIDAR$System_Size)
nrow(AllSchools.LIDAR.reduced.area.LIDAR[which(AllSchools.LIDAR.reduced.area.LIDAR$Total.Space == 0),]) # 14785 (11%)
nrow(AllSchools.LIDAR.reduced.area.LIDAR[which(AllSchools.LIDAR.reduced.area.LIDAR$System_Size == 0),]) # 14785 (11%)
#View(AllSchools.LIDAR.reduced.area.LIDAR)
class(AllSchools.LIDAR.reduced.area.LIDAR$Zip_Code) # integer
# class(zip_lookup.mar.eas$Zip_Code) # integer
#View(zip_lookup.mar.eas$Zip_Code)

# Adding in the emissions, power, net power and lmp

## EASIUR
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas <- merge(AllSchools.LIDAR.reduced.area.LIDAR, zip_lookup.mar.eas, by = "Zip_Code", match = "all") 
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas) # 132,592
View(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas)
names(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas)[19] <- "seq"
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$lmp <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$lmp * 100 / 1000 # express the LMP as cents per kWh

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas <- merge(AllSchools.LIDAR.reduced.area.LIDAR, zip_lookup.avg.eas, by = "Zip_Code", match = "all") 
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas) # 132,592
View(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas)
names(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas)[19] <- "seq"
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$lmp <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$lmp * 100 / 1000 # express the LMP as cents per kWh


## AP2
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2 <- merge(AllSchools.LIDAR.reduced.area.LIDAR, zip_lookup.mar.ap2, by = "Zip_Code", match = "all") 
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2) # 132,592
View(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2)
names(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2)[19] <- "seq"
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$lmp <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$lmp * 100 / 1000 # express the LMP as cents per kWh

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2 <- merge(AllSchools.LIDAR.reduced.area.LIDAR, zip_lookup.avg.ap2, by = "Zip_Code", match = "all") 
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2) # 132,592
View(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2)
names(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2)[19] <- "seq"
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$lmp <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$lmp * 100 / 1000 # express the LMP as cents per kWh


########################
# Clean up retail values
########################
# these values are in cents/kWh

prices_ret <- melt(prices_ret, id = c("year", "State"))
names(prices_ret)[names(prices_ret) == "value"] <- "price_ret"
names(prices_ret)[names(prices_ret) == "variable"] <- "cs_ret"

# Only want commercial rates for the educational building analysis
prices_ret <- prices_ret[which(prices_ret$cs_ret == "com"),]
nrow(prices_ret) # 1352

# Only want to use the 2015 retail rate data since we don't have 2016...
prices_ret <- prices_ret[which(prices_ret$year == 2015),]
nrow(prices_ret) # 52

# INFLATE THE retail rate BY Gross private domestic investment (implicit price deflator) https://fred.stlouisfed.org/series/A006RD3A086NBEA #### 
prices_ret <- merge(prices_ret, deflator, by = "year", all.x = TRUE)

prices_ret$price_ret_unadjusted <- prices_ret$price_ret

prices_ret$price_ret <- prices_ret$price_ret * prices_ret$deflator

# adjust average retail rates based on demand charge analysis done with Naim
# Variable rate + demand savings
prices_ret$price_ret <- prices_ret$price_ret * 0.8 + prices_ret$price_ret*.20*.20

# Merge retail prices with available space data
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas <- merge(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, prices_ret, by = "State", match = "all")
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas <- merge(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas, prices_ret, by = "State", match = "all")
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2 <- merge(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2, prices_ret, by = "State", match = "all")
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2 <- merge(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2, prices_ret, by = "State", match = "all")


###########################
# Calculate inputs for BCA
###########################

# Define financial values for NPV
disc.07 <- 0.07
disc.02 <- 0.02
life <- 20

# Look at average installation costs for nonresidential systems in lbnl
# Consider $/kW
nrow(lbnl) # 1046
hist(lbnl$price_per_kW, breaks = "FD")
summary(lbnl$price_per_kW)
install.avg <- mean(lbnl$price_per_kW, na.rm = TRUE) # $3836.87 /kW


# Currently, just consider the median overall ROG for the schools
# Soon, customize for state by using Parth's spreadsheet
# Consider $/kW
hist(lbnl$rog_per_kW, breaks = "FD")
summary(lbnl$rog_per_kW)
rog.avg <- mean(lbnl$rog_per_kW, na.rm = TRUE) # $ 753 /kW

# Include maintenance and inverter replacements
# Take these values from the NREL Breakeven report
maintenance <- 15    # /kw-yr
inverter    <- 120   # /kW @ 10 yrs (would also be at 20yrs, but it is decomissioned)

#####################################################################
# Combine the schools with state incentives and net metering policies
#####################################################################

# State rebates
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas <- merge(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, rebates, by = "State", match = "all")
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas <- merge(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas, rebates, by = "State", match = "all")
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2 <- merge(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2, rebates, by = "State", match = "all")
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2 <- merge(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2, rebates, by = "State", match = "all")

# State net metering
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas <- merge(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, net.metering, by = "State", match = "all")
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas <- merge(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas, net.metering, by = "State", match = "all")
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2 <- merge(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2, net.metering, by = "State", match = "all")
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2 <- merge(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2, net.metering, by = "State", match = "all")


#####################################################################
# Calculate school costs and rebates
#####################################################################

# Calculate School Costs: Installation - rebate or grant (using the average rog)
## EASIUR
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc <- (install.avg-rog.avg)*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc <- (install.avg-rog.avg)*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size

## AP2
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc <- (install.avg-rog.avg)*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc <- (install.avg-rog.avg)*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size


# Calculate School uprfront Costs: Installation - state rebate (as of 1.12.18 this now is the mean state rebate $/kW for each state in the LBNL dataset)
## EASIUR
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$dsire.rebate <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size * AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$Rebate...kW.
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$dsire.rebate[is.na(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$dsire.rebate)] <- 0
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc.dsire <- install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$dsire.rebate

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$dsire.rebate <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size * AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$Rebate...kW.
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$dsire.rebate[is.na(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$dsire.rebate)] <- 0
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc.dsire <- install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$dsire.rebate

## AP2
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$dsire.rebate <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size * AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$Rebate...kW.
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$dsire.rebate[is.na(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$dsire.rebate)] <- 0
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc.dsire <- install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$dsire.rebate

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$dsire.rebate <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size * AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$Rebate...kW.
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$dsire.rebate[is.na(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$dsire.rebate)] <- 0
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc.dsire <- install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$dsire.rebate


# Calculate School uprfront Costs: Installation - state rebate (according to rebates on DSIRE) - ITC
## EASIUR
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc.dsire.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc.dsire - 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$dsire.rebate)
# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc.dsire.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc.dsire - 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$dsire.rebate)
## AP2
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc.dsire.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc.dsire - 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$dsire.rebate)
# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc.dsire.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc.dsire - 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$dsire.rebate)




# Calculate School Costs: annual maintenance 
## EASIUR
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$maintenance <- (maintenance)*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$maintenance <- (maintenance)*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size

## AP2
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$maintenance <- (maintenance)*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$maintenance <- (maintenance)*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size



# Calculate School Costs: inverter costs
## EASIUR
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$inverter <- (inverter)*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$inverter <- (inverter)*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size

## AP2
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$inverter <- (inverter)*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$inverter <- (inverter)*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size




# Calculate School Benefits: Elec cost savings + excess generation sales


## EASIUR
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$demand         <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$power-AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$net_power)*1000 # kWh/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$cost.savings   <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$power-AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$net_power)*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$price_ret*1000/100 # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$elec.sales.lmp <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$net_power*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$lmp # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$elec.sales.ret <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$net_power*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$price_ret*1000/100 # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.lmp    <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$cost.savings + AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$elec.sales.lmp  # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.ret    <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$cost.savings + AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$elec.sales.ret  # $/yr

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$demand         <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$power-AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$net_power)*1000 # kWh/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$cost.savings   <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$power-AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$net_power)*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$price_ret*1000/100 # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$elec.sales.lmp <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$net_power*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$lmp # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$elec.sales.ret <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$net_power*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$price_ret*1000/100 # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolb.lmp    <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$cost.savings + AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$elec.sales.lmp # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolb.ret    <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$cost.savings + AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$elec.sales.ret # $/yr


## AP2
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$demand         <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$power-AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$net_power)*1000 # kWh/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$cost.savings   <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$power-AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$net_power)*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$price_ret*1000/100 # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$elec.sales.lmp <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$net_power*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$lmp # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$elec.sales.ret <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$net_power*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$price_ret*1000/100 # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolb.lmp    <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$cost.savings + AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$elec.sales.lmp # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolb.ret    <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$cost.savings + AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$elec.sales.ret # $/yr

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$demand         <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$power-AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$net_power)*1000 # kWh/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$cost.savings   <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$power-AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$net_power)*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$price_ret*1000/100 # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$elec.sales.lmp <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$net_power*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$lmp # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$elec.sales.ret <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$net_power*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$price_ret*1000/100 # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolb.lmp    <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$cost.savings + AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$elec.sales.lmp # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolb.ret    <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$cost.savings + AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$elec.sales.ret # $/yr


# Calculate Social Costs: rebate or grant 

## EASIUR
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.ret.07        <- rog.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.ret.02        <- rog.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.lmp           <- rog.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size 
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.lmp     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$dsire.rebate
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.ret.07  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$dsire.rebate        - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.ret.02  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$dsire.rebate        - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.lmp.ITC     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$dsire.rebate + 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$dsire.rebate)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.ret.07.ITC  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$dsire.rebate + 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$dsire.rebate)   - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.ret.02.ITC  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$dsire.rebate + 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$dsire.rebate)   - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$lmp))


# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.ret.07        <- rog.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.ret.02        <- rog.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.lmp           <- rog.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size 
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.lmp     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$dsire.rebate 
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.ret.07  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$dsire.rebate        - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.ret.02  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$dsire.rebate        - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.lmp.ITC     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$dsire.rebate + 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$dsire.rebate) 
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.ret.07.ITC  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$dsire.rebate + 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$dsire.rebate)    - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.ret.02.ITC  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$dsire.rebate + 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$dsire.rebate)    - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$lmp))



## AP2
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.ret.07        <- rog.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.ret.02        <- rog.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.lmp           <- rog.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size 
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.lmp     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$dsire.rebate
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.ret.07  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$dsire.rebate        - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.ret.02  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$dsire.rebate        - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.lmp.ITC     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$dsire.rebate + 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$dsire.rebate) 
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.ret.07.ITC  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$dsire.rebate + 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$dsire.rebate) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.ret.02.ITC  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$dsire.rebate + 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$dsire.rebate) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$lmp))


# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.ret.07        <- rog.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.ret.02        <- rog.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.lmp           <- rog.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size 
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.lmp     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$dsire.rebate
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.ret.07  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$dsire.rebate        - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.ret.02  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$dsire.rebate        - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.lmp.ITC     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$dsire.rebate + 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$dsire.rebate)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.ret.07.ITC  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$dsire.rebate + 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$dsire.rebate) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$lmp))
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.ret.02.ITC  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$dsire.rebate + 0.3*(install.avg*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$dsire.rebate) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$net_power*(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$price_ret*1000/100 - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$lmp))



# Calculate Social Benefits: pollution benefits + Co2 benefits

## EASIUR
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$nox_ben  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$nox_dam_eas # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$so2_ben  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$so2_dam_eas # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$pm25_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$pm25_dam_eas # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$aq_ben   <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$pol_dam # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$co2_ben  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$co2_dam # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb       <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$aq_ben + AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$co2_ben # $/yr

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$nox_ben  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$nox_dam_eas # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$so2_ben  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$so2_dam_eas # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$pm25_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$pm25_dam_eas # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$aq_ben   <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$pol_dam # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$co2_ben  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$co2_dam # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sb       <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$aq_ben + AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$co2_ben # $/yr


## AP2
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$nox_ben  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$nox_dam_ap2 # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$so2_ben  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$so2_dam_ap2 # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$pm25_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$pm25_dam_ap2 # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$aq_ben   <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$pol_dam # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$co2_ben  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$co2_dam # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sb       <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$aq_ben + AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$co2_ben # $/yr

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$nox_ben  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$nox_dam_ap2 # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$so2_ben  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$so2_dam_ap2 # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$pm25_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$pm25_dam_ap2 # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$aq_ben   <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$pol_dam # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$co2_ben  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$System_Size*AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$co2_dam # $/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sb       <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$aq_ben + AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$co2_ben # $/yr




# Calculate net school benefits for next 20 years
# - upfront costs - 10 year inverter replacement - 20 years of maintenance + 20 years of benefits

## EASIUR
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc       + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.lmp) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.02                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc       + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.lmp) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.07                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc       + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.ret) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.02                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc       + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.ret) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc.dsire + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.lmp) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc.dsire + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.lmp) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc.dsire + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.ret) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc.dsire + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.ret) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc.dsire.ITC + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.lmp) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc.dsire.ITC + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.lmp) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc.dsire.ITC + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.ret) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc.dsire.ITC + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.ret) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$maintenance)


# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.07                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc       + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolb.lmp) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.02                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc       + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolb.lmp) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.07                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc       + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolb.ret) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.02                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc       + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolb.ret) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.dsirerebate.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc.dsire + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolb.lmp) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.dsirerebate.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc.dsire + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolb.lmp) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.dsirerebate.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc.dsire + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolb.ret) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.dsirerebate.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc.dsire + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolb.ret) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.dsirerebate.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc.dsire.ITC + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolb.lmp) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.dsirerebate.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc.dsire.ITC + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolb.lmp) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.dsirerebate.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc.dsire.ITC + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolb.ret) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.dsirerebate.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc.dsire.ITC + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolb.ret) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$maintenance)



## AP2
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.07                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc       + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolb.lmp) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.02                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc       + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolb.lmp) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.07                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc       + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolb.ret) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.02                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc       + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolb.ret) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc.dsire + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolb.lmp) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc.dsire + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolb.lmp) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc.dsire + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolb.ret) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc.dsire + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolb.ret) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc.dsire.ITC + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolb.lmp) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc.dsire.ITC + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolb.lmp) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc.dsire.ITC + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolb.ret) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc.dsire.ITC + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolb.ret) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$maintenance)


# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.07                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc       + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolb.lmp) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.02                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc       + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolb.lmp) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.07                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc       + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolb.ret) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.02                 <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc       + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolb.ret) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.dsirerebate.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc.dsire + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolb.lmp) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.dsirerebate.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc.dsire + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolb.lmp) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.dsirerebate.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc.dsire + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolb.ret) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.dsirerebate.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc.dsire + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolb.ret) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.dsirerebate.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc.dsire.ITC + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolb.lmp) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.dsirerebate.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc.dsire.ITC + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolb.lmp) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.dsirerebate.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc.dsire.ITC + pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolb.ret) + pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$maintenance)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.dsirerebate.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc.dsire.ITC + pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolb.ret) + pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$maintenance)





# Calculate net social benefits for next 20 years

## EASIUR
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.07           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.lmp          - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.02           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.lmp          - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.07           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.ret.07       - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.02           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.ret.02       - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.lmp    - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.lmp    - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.ret.07 - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.ret.02 - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.lmp.ITC    - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.lmp.ITC    - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.ret.07.ITC - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.ret.02.ITC - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb)


# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.07           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.lmp          - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.02           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.lmp          - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.07           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.ret.07       - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.02           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.ret.02       - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.dsire.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.lmp    - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.dsire.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.lmp    - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.dsire.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.ret.07 - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.dsire.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.ret.02 - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.dsire.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.lmp.ITC    - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.dsire.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.lmp.ITC    - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.dsire.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.ret.07.ITC - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.dsire.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.ret.02.ITC - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sb)



## AP2
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.07           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.lmp          - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.02           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.lmp          - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.07           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.ret.07       - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.02           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.ret.02       - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.dsire.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.lmp    - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.dsire.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.lmp    - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.dsire.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.ret.07 - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.dsire.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.ret.02 - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.dsire.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.lmp.ITC    - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.dsire.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.lmp.ITC    - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.dsire.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.ret.07.ITC - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.dsire.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.ret.02.ITC - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sb)



# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.07           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.lmp          - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.02           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.lmp          - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.07           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.ret.07       - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.02           <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.ret.02       - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.dsire.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.lmp    - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.dsire.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.lmp    - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.dsire.07     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.ret.07 - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.dsire.02     <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.ret.02 - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.dsire.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.lmp.ITC    - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.dsire.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.lmp.ITC    - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.dsire.07.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.ret.07.ITC - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.dsire.02.ITC <- -AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.ret.02.ITC - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sb)



# Calculate annuity factors and look at annualized net benefits
annuity.07 <- (1-(1+disc.07)^(-life))/disc.07
annuity.02 <- (1-(1+disc.02)^(-life))/disc.02


# SCHOOLS
## EASIUR
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07.annual                 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.02.annual                 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.07.annual                 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.02.annual                 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.07.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.02.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07.ITC/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02.ITC/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.07.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.07.ITC/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.02.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.02.ITC/annuity.02


# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.07.annual                 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.02.annual                 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.07.annual                 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.02.annual                 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.dsirerebate.07.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.dsirerebate.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.dsirerebate.02.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.dsirerebate.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.dsirerebate.07.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.dsirerebate.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.dsirerebate.02.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.dsirerebate.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.dsirerebate.07.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.dsirerebate.07.ITC/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.dsirerebate.02.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.dsirerebate.02.ITC/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.dsirerebate.07.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.dsirerebate.07.ITC/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.dsirerebate.02.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.dsirerebate.02.ITC/annuity.02



## AP2
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.07.annual                 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.02.annual                 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.07.annual                 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.02.annual                 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.07.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.02.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.07.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.02.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.07.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.07.ITC/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.02.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.02.ITC/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.07.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.07.ITC/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.02.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.02.ITC/annuity.02


# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.07.annual                  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.02.annual                  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.07.annual                  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.02.annual                  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.dsirerebate.07.annual      <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.dsirerebate.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.dsirerebate.02.annual      <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.dsirerebate.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.dsirerebate.07.annual      <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.dsirerebate.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.dsirerebate.02.annual      <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.dsirerebate.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.dsirerebate.07.annual.ITC  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.dsirerebate.07.ITC/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.dsirerebate.02.annual.ITC  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.dsirerebate.02.ITC/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.dsirerebate.07.annual.ITC  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.dsirerebate.07.ITC/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.dsirerebate.02.annual.ITC  <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.dsirerebate.02.ITC/annuity.02



# Annual Net-ben for Schools doing ITC
## EASIUR
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.lmp.dsirerebate.07 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07.annual.ITC
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.lmp.dsirerebate.02 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02.annual.ITC
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.ret.dsirerebate.07 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.07.annual.ITC
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.ret.dsirerebate.02 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.02.annual.ITC

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$annual.nb.ITC.lmp.dsirerebate.07 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.dsirerebate.07.annual.ITC
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$annual.nb.ITC.lmp.dsirerebate.02 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.lmp.dsirerebate.02.annual.ITC
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$annual.nb.ITC.ret.dsirerebate.07 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.dsirerebate.07.annual.ITC
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$annual.nb.ITC.ret.dsirerebate.02 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$school_net_ben.ret.dsirerebate.02.annual.ITC

## AP2
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$annual.nb.ITC.lmp.dsirerebate.07 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.07.annual.ITC
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$annual.nb.ITC.lmp.dsirerebate.02 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.02.annual.ITC
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$annual.nb.ITC.ret.dsirerebate.07 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.07.annual.ITC
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$annual.nb.ITC.ret.dsirerebate.02 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.02.annual.ITC

# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$annual.nb.ITC.lmp.dsirerebate.07 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.dsirerebate.07.annual.ITC
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$annual.nb.ITC.lmp.dsirerebate.02 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.lmp.dsirerebate.02.annual.ITC
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$annual.nb.ITC.ret.dsirerebate.07 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.dsirerebate.07.annual.ITC
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$annual.nb.ITC.ret.dsirerebate.02 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$cost.savings - AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$school_net_ben.ret.dsirerebate.02.annual.ITC




# SOCIAL
## EASIUR
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.lmp.annual                    <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.lmp/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.retminuslmp.annual            <- (AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.ret.07 - AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.lmp)/annuity.07

AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.07.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.02.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.07.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.02.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.07.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.02.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.ITC/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.ITC/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.07.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.07.ITC/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.02.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.02.ITC/annuity.02



# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.07.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.02.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.07.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.02.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.dsire.07.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.dsire.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.dsire.02.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.dsire.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.dsire.07.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.dsire.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.dsire.02.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.dsire.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.dsire.07.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.dsire.07.ITC /annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.dsire.02.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.lmp.dsire.02.ITC /annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.dsire.07.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.dsire.07.ITC /annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.dsire.02.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$social_net_ben.ret.dsire.02.ITC /annuity.02



## AP2
# Marginal
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.07.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.02.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.07.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.02.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.dsire.07.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.dsire.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.dsire.02.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.dsire.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.dsire.07.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.dsire.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.dsire.02.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.dsire.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.dsire.07.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.dsire.07.ITC/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.dsire.02.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.dsire.02.ITC/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.dsire.07.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.dsire.07.ITC/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.dsire.02.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.dsire.02.ITC/annuity.02


# Average
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.07.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.02.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.07.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.02.annual           <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.dsire.07.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.dsire.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.dsire.02.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.dsire.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.dsire.07.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.dsire.07/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.dsire.02.annual     <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.dsire.02/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.dsire.07.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.dsire.07.ITC/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.dsire.02.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.lmp.dsire.02.ITC/annuity.02
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.dsire.07.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.dsire.07.ITC/annuity.07
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.dsire.02.annual.ITC <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$social_net_ben.ret.dsire.02.ITC/annuity.02







####################################################################
## Look at the individual annualized per-kilowatt costs and benefits
####################################################################
# School - electricity sales at LMP
# Already captured as "elec.sales.lmp" in $/yr


# School - electricity sales at retail
# Already captured as "elec.sales.ret" in $/yr

# School - offset consumption at retail
# Already captured as "cost.savings" in $/yr


# School - costs (project + maintenance + inverter)
#AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$project.costs.annual.07 <- (AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc.dsire - pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$maintenance))/annuity.07 # this will be a positive value
#AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$project.costs.annual.07 <- (AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc.dsire - pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$maintenance))/annuity.07 # this will come out to be the total cost, as a positive value
#AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$project.costs.annual.07 <- (AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc.dsire - pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$maintenance))/annuity.07 # this will come out to be the total cost, as a positive value
#AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$project.costs.annual.07 <- (AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc.dsire - pv.simple(r = disc.07, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$inverter) - pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$maintenance))/annuity.07 # this will come out to be the total cost, as a positive value
#AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$project.costs.annual.02 <- (AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolc.dsire - pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$maintenance))/annuity.02 # this will be a positive value
#AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$project.costs.annual.02 <- (AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$schoolc.dsire - pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$maintenance))/annuity.02 # this will come out to be the total cost, as a positive value
#AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$project.costs.annual.02 <- (AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$schoolc.dsire - pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$maintenance))/annuity.02 # this will come out to be the total cost, as a positive value
#AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$project.costs.annual.02 <- (AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$schoolc.dsire - pv.simple(r = disc.02, n = 10, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$inverter) - pv.annuity(r = disc.02, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$maintenance))/annuity.02 # this will come out to be the total cost, as a positive value


# Social - air benefits
# Already quantified as "aq_ben" in $/yr

# Social - CO2 benefits
# Already quantified as "co2_ben" in $/yr


# Social - rebates or grants (from dsire) - LMP
#AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.annual.07 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire/annuity.07
#AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.annual.07 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire/annuity.07
#AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.annual.07 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire/annuity.07
#AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.annual.07 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire/annuity.07
#AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.annual.02 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire/annuity.02
#AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.annual.02 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire/annuity.02
#AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.annual.02 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire/annuity.02
#AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.annual.02 <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire/annuity.02


# Social - retail cross-subsidy
#AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.annual.07.ret <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.ret.07/annuity.07
#AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.annual.07.ret <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.ret.07/annuity.07
#AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.annual.07.ret <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.ret.07/annuity.07
#AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.annual.07.ret <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.ret.07/annuity.07
#AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.annual.02.ret <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sc.dsire.ret.02/annuity.02
#AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.annual.02.ret <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas$sc.dsire.ret.02/annuity.02
#AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.annual.02.ret <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$sc.dsire.ret.02/annuity.02
#AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.annual.02.ret <- AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2$sc.dsire.ret.02/annuity.02




####################################################################
## Summarize findings for dataset
####################################################################

# Calculate total installed capacity
# system.size is in kW
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size, na.rm = TRUE) / (10^6) # 64 GW
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$LIDAR == "HaveLIDAR"),]$System_Size, na.rm = TRUE) / (10^6) # 12 GW
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$State == "CA"),]$System_Size, na.rm = TRUE) 
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$State == "CO"),]$System_Size, na.rm = TRUE) 
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$State == "DE"),]$System_Size, na.rm = TRUE) 
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$State == "MA"),]$System_Size, na.rm = TRUE) 
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$State == "MO"),]$System_Size, na.rm = TRUE) 
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$State == "NH"),]$System_Size, na.rm = TRUE) 
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$State == "NV"),]$System_Size, na.rm = TRUE) 
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$State == "NY"),]$System_Size, na.rm = TRUE) 
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$State == "OR"),]$System_Size, na.rm = TRUE) 
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$State == "TX"),]$System_Size, na.rm = TRUE) 


# Calculate total annual electricity generated
# power is in MWh/kW
# system.size is in kW
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$power * AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size * 1000, na.rm = TRUE)/(10^9) # 100 TWh/yr
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$LIDAR == "HaveLIDAR"),]$power * AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$LIDAR == "HaveLIDAR"),]$System_Size * 1000, na.rm = TRUE)/(10^9) # 19 TWh/yr
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$power * AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size * 1000 # kWh/yr
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation, na.rm = TRUE)/(10^9)
# Look at the mean generation (or offset consumption)
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation, na.rm = TRUE) # min = 0; max = 73,600,000 kWh/yr; mean = 755,900 kWh/yr; median = 631,500 kWh/yr
# compare to mean net-generation (or excess generation)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$excessgeneration <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$net_power * AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size * 1000 # kWh/yr
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$excessgeneration, na.rm = TRUE) # min = 0; max = 16,400,000 kWh/yr; mean = 158,100 kWh/yr; median = 135,700 kWh/yr


# Histograms of generation - total and by school type
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation, .95) # 1.8M kWh
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation < 1800000),]$generation, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Total Annual Generation (kWh/yr)", main = "All Schools - 95th Percentile", xaxt="n")
axis(side=1, at=axTicks(1), 
     labels=formatC(axTicks(1), format="d", big.mark=','))

quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$generation, .95) # 0, 8.5M
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation < 8600000 & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$generation, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Total Annual Generation (kWh/yr)", main = "Higher Education - 95th Percentile", xaxt="n")
axis(side=1, at=axTicks(1), 
     labels=formatC(axTicks(1), format="d", big.mark=','))

quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$generation, .95) # 0, 1.7M
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation < 1800000 & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$generation, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Total Annual Generation (kWh/yr)", main = "K12 Public - 95th Percentile", xaxt="n")
axis(side=1, at=axTicks(1), 
     labels=formatC(axTicks(1), format="d", big.mark=','))


quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$generation, .95) # 0, 1.1M
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation < 1100000 & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$generation, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Total Annual Generation (kWh/yr)", main = "K12 Private - 95th Percentile", xaxt="n")
axis(side=1, at=axTicks(1), 
     labels=formatC(axTicks(1), format="d", big.mark=','))



# Calculate the total available space
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$Total.Space)*((1/3.28)^2)/(10^9) # 0.42 billion m^2
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$Total.Space)/(10^9)  # 4.6 billion ft2
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$LIDAR == "HaveLIDAR"),]$Total.Space)*((1/3.28)^2)/(10^9) # 0.08 billion m^2

# Histograms of available space - total and by school type
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$Total.Space, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Total Space (SF)", main = "All Schools")
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$Total.Space, c(.05,.95)) # 0, 79K
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$Total.Space < 79000),]$Total.Space, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Total Space (SF)", main = "All Schools - 95th Percentile", xaxt="n")
axis(side=1, at=axTicks(1), 
     labels=formatC(axTicks(1), format="d", big.mark=','))

quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$Total.Space, .95) # 0, 367K
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$Total.Space < 367000 & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$Total.Space, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Total Space (SF)", main = "Higher Education - 95th Percentile", xaxt="n")
axis(side=1, at=axTicks(1), 
     labels=formatC(axTicks(1), format="d", big.mark=','))

quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$Total.Space, .95) # 0, 76K
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$Total.Space < 77000 & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$Total.Space, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Total Space (SF)", main = "K12 Public - 95th Percentile", xaxt="n")
axis(side=1, at=axTicks(1), 
     labels=formatC(axTicks(1), format="d", big.mark=','))

quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$Total.Space, .95) # 0, 46K
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$Total.Space < 47000 & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$Total.Space, 
     col=rgb(1,0,0,0.5), breaks = "FD", border =rgb(1,0,0,0.5),
     xlab = "Total Space (SF)", main = "K12 Private - 95th Percentile", xaxt="n")
axis(side=1, at=axTicks(1), 
     labels=formatC(axTicks(1), format="d", big.mark=','))

# look at 95th percentiles for paper
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$LIDAR == "HaveLIDAR"),]$Total.Space, .95) # 90,316 ft2
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$LIDAR == "NoLIDAR"),]$Total.Space, .95) # 76,990 ft2


# Calculate total school net-benefits
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07, na.rm = TRUE)/(10^9) # -171 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02, na.rm = TRUE)/(10^9) # -138 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.07, na.rm = TRUE)/(10^9) # -154x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.02, na.rm = TRUE)/(10^9) # -112 x 10^9

sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.07, na.rm = TRUE)/(10^9) # -171 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.02, na.rm = TRUE)/(10^9) # -138 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.07, na.rm = TRUE)/(10^9) # -154 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.02, na.rm = TRUE)/(10^9) # -112 x 10^9

# Calculate total social net-benefits
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07, na.rm = TRUE)/(10^9) # 32 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02, na.rm = TRUE)/(10^9) # 57 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.07, na.rm = TRUE)/(10^9) # 13 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.02, na.rm = TRUE)/(10^9) # 38 x 10^9

sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.dsire.07, na.rm = TRUE)/(10^9) # 34 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.lmp.dsire.02, na.rm = TRUE)/(10^9) # 60 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.dsire.07, na.rm = TRUE)/(10^9) # 15 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$social_net_ben.ret.dsire.02, na.rm = TRUE)/(10^9) # 31 x 10^9

# Calculate annual school net-benefits
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)/(10^9) # -16 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02.annual, na.rm = TRUE)/(10^9) # -8  x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.07.annual, na.rm = TRUE)/(10^9) # -15 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.02.annual, na.rm = TRUE)/(10^9) # -7  x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.lmp.dsirerebate.07, na.rm = TRUE)/(10^9) # 16 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.lmp.dsirerebate.02, na.rm = TRUE)/(10^9) # 11 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.ret.dsirerebate.07, na.rm = TRUE)/(10^9) # 15 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.ret.dsirerebate.02, na.rm = TRUE)/(10^9) # 9  x 10^9

sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)/(10^9) # -16 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.lmp.dsirerebate.02.annual, na.rm = TRUE)/(10^9) # -8  x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.07.annual, na.rm = TRUE)/(10^9) # -15 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$school_net_ben.ret.dsirerebate.02.annual, na.rm = TRUE)/(10^9) # -7  x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$annual.nb.ITC.lmp.dsirerebate.07, na.rm = TRUE)/(10^9) # 17 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$annual.nb.ITC.lmp.dsirerebate.02, na.rm = TRUE)/(10^9) # 11 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$annual.nb.ITC.ret.dsirerebate.07, na.rm = TRUE)/(10^9) # 15 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2$annual.nb.ITC.ret.dsirerebate.02, na.rm = TRUE)/(10^9) # 9  x 10^9

# Calculate annual social net-benefits
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)/(10^9) # 3.2 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.annual, na.rm = TRUE)/(10^9) # 3.6  x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.07.annual, na.rm = TRUE)/(10^9) # 1.1 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.02.annual, na.rm = TRUE)/(10^9) # 2.3  x 10^9

sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.annual.ITC, na.rm = TRUE)/(10^9) # -3.4 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.annual.ITC, na.rm = TRUE)/(10^9) # -.7  x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.07.annual.ITC, na.rm = TRUE)/(10^9) # -5.5 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.02.annual.ITC, na.rm = TRUE)/(10^9) # -2  x 10^9


# Calculate annual net-benefits when considering LBNL rebates, 7% discount rate, Easiur Model, and excess at retail:
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.07.annual, na.rm = TRUE)/(10^9) # -11 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07.annual, na.rm = TRUE)/(10^9) # -13 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.07.annual, na.rm = TRUE)/(10^9) # -2 x 10^9          
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.07.annual, na.rm = TRUE)/(10^9) # -0.3 x 10^9  

# Calculate table values for paper Table 2
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)/(10^9) # -16 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)/(10^9) # 3.1 x 10^9

sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02.annual, na.rm = TRUE)/(10^9) # -8.5 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.annual, na.rm = TRUE)/(10^9) # 3.5 x 10^9

sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.07.annual, na.rm = TRUE)/(10^9) # -15 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.07.annual, na.rm = TRUE)/(10^9) # 1.3 x 10^9

sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.02.annual, na.rm = TRUE)/(10^9) # -6.9 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.02.annual, na.rm = TRUE)/(10^9) # 2.3 x 10^9

sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.lmp.dsirerebate.07, na.rm = TRUE)/(10^9) # 17 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.annual.ITC, na.rm = TRUE)/(10^9) # -3.6 x 10^9

sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.lmp.dsirerebate.02, na.rm = TRUE)/(10^9) # 11 x 10^9
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.annual.ITC, na.rm = TRUE)/(10^9) # -0.8 x 10^9


# just look at total annual social benefits
sum(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb, na.rm = TRUE)/(10^9) # 4B/yr

# Figure 4 in Paper
# Histogram of total generation - do 5th and 95th percentile
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation, .05) # 0
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation, .95) # 1785541 kWh
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation < 1785542),]$generation, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(0,2000000), main = NULL, xlab = NULL)

# Figure 4 in Paper
# Histogram of total generation / per kW
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation.perkW <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation/AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation.perkW)
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[is.na(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation),]) # 0
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation == 0),]) # 15944
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[is.na(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size),]) # 0
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$System_Size == 0),]) # 14613
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[is.na(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation.perkW),]) # 14613
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation.perkW == 0),]) # 1331
# remove the NAs - turn them to 0
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[is.na(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation.perkW),]$generation.perkW <- 0
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation.perkW == 0),]) # 15944
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation.perkW, .05) # 0
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation.perkW, .95) # 1952.549 kWh/kW

hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$generation.perkW < 1953),]$generation.perkW, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(0,2000), main = NULL, xlab = NULL)


# Figure 5 in paper
# First, look at 5th and 95th for each component
# All schools
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.lmp, .05, na.rm = TRUE)
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.lmp, .95, na.rm = TRUE) # 130929
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.ret, .05, na.rm = TRUE)
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.ret, .95, na.rm = TRUE) # 16050

# Higher Education
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$schoolb.lmp, .05, na.rm = TRUE)
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$schoolb.lmp, .95, na.rm = TRUE) # 606,574
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$schoolb.ret, .05, na.rm = TRUE)
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$schoolb.ret, .95, na.rm = TRUE) # 746,342

# K12 Public
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$schoolb.lmp, .05, na.rm = TRUE)
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$schoolb.lmp, .95, na.rm = TRUE) # 126,517
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$schoolb.ret, .05, na.rm = TRUE)
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$schoolb.ret, .95, na.rm = TRUE) # 154,711

# K12 Private
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$schoolb.lmp, .05, na.rm = TRUE) # 433
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$schoolb.lmp, .95, na.rm = TRUE) # 76,882
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$schoolb.ret, .05, na.rm = TRUE) # 281
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$schoolb.ret, .95, na.rm = TRUE) # 92194



# All schools: Histogram of all schools, retail and LMP values
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.ret < 124263),]$schoolb.ret,
     col=rgb(222,45,38, 255, maxColorValue=255), border = rgb(165,15,21, 255, maxColorValue=255), breaks = "FD", xlab = NULL, main = NULL, xlim = c(0,140000), ylim = c(0,30000))
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.lmp < 101545),]$schoolb.lmp, 
     col=rgb(49,130,189, 255, maxColorValue=255), border = rgb(8,81,156, 255, maxColorValue=255), add = T, breaks = "FD", xlab = NULL, main = NULL, xlim = c(0,140000), ylim = c(0,30000))
# Add retail values
legend('topright', c("LMP", "Retail"), col = c(rgb(49,130,189, 255, maxColorValue=255),rgb(222,45,38, 255, maxColorValue=255)), 
       lty=c(1,1), bty = "n", lwd = 3)

# Higher Education: Histogram of all schools, retail and LMP values
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.lmp < 606575),]$schoolb.lmp, 
     col=rgb(49,130,189, 255, maxColorValue=255), border = rgb(8,81,156, 255, maxColorValue=255), breaks = "FD", xlab = NULL, main = NULL, xlim = c(0,800000), ylim = c(0,3500))
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.ret < 746343),]$schoolb.ret,
     col=rgb(222,45,38, 255, maxColorValue=255), border = rgb(165,15,21, 255, maxColorValue=255), add = T, breaks = "FD", xlab = NULL, main = NULL, xlim = c(0,800000), ylim = c(3500))
# Add retail values
legend('topright', c("LMP", "Retail"), col = c(rgb(49,130,189, 255, maxColorValue=255),rgb(222,45,38, 255, maxColorValue=255)), 
       lty=c(1,1), bty = "n", lwd = 3)

# K12 Public: Histogram of all schools, retail and LMP values
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public" & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.ret < 154712),]$schoolb.ret,
     col=rgb(222,45,38, 255, maxColorValue=255), border = rgb(165,15,21, maxColorValue=255), breaks = "FD", xlab = NULL, main = NULL, xlim = c(0,175000), ylim = c(0,25000))
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public" & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.lmp < 126518),]$schoolb.lmp, 
     col=rgb(49,130,189, 255, maxColorValue=255), border = rgb(8,81,156, 255, maxColorValue=255), add = T, breaks = "FD", xlab = NULL, main = NULL, xlim = c(0,175000), ylim=c(0,25000))
# Add retail values
legend('topright', c("LMP", "Retail"), col = c(rgb(49,130,189, 255, maxColorValue=255),rgb(222,45,38, 255, maxColorValue=255)), 
       lty=c(1,1), bty = "n", lwd = 3)

# K12 Private: Histogram of all schools, retail and LMP values
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private" & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.ret < 92195),]$schoolb.ret,
     col=rgb(222,45,38, 255, maxColorValue=255), border = rgb(165,15,21, 255, maxColorValue=255), breaks = "FD", xlab = NULL, main = NULL, xlim = c(0,100000), ylim = c(0,5000))
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private" & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.lmp < 76882),]$schoolb.lmp, 
     col=rgb(49,130,189, 255, maxColorValue=255), border = rgb(8,81,156, 255, maxColorValue=255), add = T, breaks = "FD", xlab = NULL, main = NULL, xlim = c(0,100000), ylim = c(0,5000))
# Add retail values
legend('topright', c("LMP", "Retail"), col = c(rgb(49,130,189, 255, maxColorValue=255),rgb(222,45,38, 255, maxColorValue=255)), 
       lty=c(1,1), bty = "n", lwd = 3)


# Figure 6
# Use EASIUR results
# First, look at 5th and 95th for each component
# All schools
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb, .05, na.rm = TRUE)
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb, .95, na.rm = TRUE) # 77,719

# Higher Education
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$sb, .05, na.rm = TRUE)
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$sb, .95, na.rm = TRUE) # 346,166

# K12 Public
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$sb, .05, na.rm = TRUE)
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$sb, .95, na.rm = TRUE) # 74,823

# K12 Private
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$sb, .05, na.rm = TRUE) # 141
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$sb, .95, na.rm = TRUE) # 47,466

# Plot all schools
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb < 77720),]$sb, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(0,80000), ylim = c(0,30000), main = NULL, xlab = NULL)

# Plot higher education
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation" & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb < 346167),]$sb, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(0,350000), ylim = c(0, 3500), main = NULL, xlab = NULL)

# Plot K12 Public
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public" & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb < 74824),]$sb, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(0,80000), ylim = c(0, 25000), main = NULL, xlab = NULL)

# Plot K12 Private
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private" & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb < 47467),]$sb, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(0,50000), ylim = c(0, 5000), main = NULL, xlab = NULL)


# Figure 7
# Annualized net benefits for schools - each plot does LMP and Retail
# Two rows: 7% and 2%
# Just do all scools
# First, look at 5th and 95th for each component
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07.annual, .05, na.rm = TRUE) # -298102
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07.annual, .95, na.rm = TRUE) # 0

quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.07.annual, .05, na.rm = TRUE) # -275265
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.07.annual, .95, na.rm = TRUE) # 0

quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.lmp.dsirerebate.07, .05, na.rm = TRUE) # 0
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.lmp.dsirerebate.07, .95, na.rm = TRUE) # 293403

quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02.annual, .05, na.rm = TRUE) # -165000
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02.annual, .95, na.rm = TRUE) # 8617

quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.02.annual, .05, na.rm = TRUE) # -142240
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.02.annual, .95, na.rm = TRUE) # 24506

quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.lmp.dsirerebate.02, .05, na.rm = TRUE) # 0
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.lmp.dsirerebate.02, .95, na.rm = TRUE) # 198060

# Plot all schools - 7% - LMP
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07.annual > -298103),]$school_net_ben.lmp.dsirerebate.07.annual, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(-300000,0), ylim = c(0, 30000), main = NULL, xlab = NULL)

# Plot all schools - 7% - Retail
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.07.annual > -275266 & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.07.annual < 0),]$school_net_ben.ret.dsirerebate.07.annual, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(-300000,0),  main = NULL, xlab = NULL)

# Plot all schools - 7% - ITC
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.lmp.dsirerebate.07 < 293404),]$annual.nb.ITC.lmp.dsirerebate.07, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(0,300000),  main = NULL, xlab = NULL)

# Plot all schools - 2% - LMP
x <- seq(-175000, 25000, 25000)
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02.annual > -165001 & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02.annual < 8618),]$school_net_ben.lmp.dsirerebate.02.annual, 
     breaks = "FD", col = "#6baed6", border = "#08519c", main = NULL, xact = NULL, xlab = NULL)
axis(side = 1, at = x, labels = T)

# Plot all schools - 2% - Retail
x <- seq(-150000, 25000, 25000)
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.02.annual > -142241 & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.dsirerebate.02.annual < 24507),]$school_net_ben.ret.dsirerebate.02.annual, 
     breaks = "FD", col = "#6baed6", border = "#08519c", ylim = c(0, 30000), main = NULL, xact = NULL, xlab = NULL)
axis(side = 1, at = x, labels = T)

# Plot all schools - 2% - ITC
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$annual.nb.ITC.lmp.dsirerebate.02 < 198061),]$annual.nb.ITC.lmp.dsirerebate.02, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(0,200000), ylim = c(0,30000), main = NULL, xlab = NULL)




# Figure 8
# Annualized net benefits for society - each plot does LMP and Retail
# Two rows: 7% and 2%
# Just do all scools
# First, look at 5th and 95th for each component
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.annual, .05, na.rm = TRUE) # -31240
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.annual, .95, na.rm = TRUE) # 72651

quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.07.annual, .05, na.rm = TRUE) # -49137
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.07.annual, .95, na.rm = TRUE) # 50226

quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.annual.ITC, .05, na.rm = TRUE) # -81485
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.annual.ITC, .95, na.rm = TRUE) # 0


quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.annual, .05, na.rm = TRUE) # -14801
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.annual, .95, na.rm = TRUE) # 73179

quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.02.annual, .05, na.rm = TRUE) # -25743
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.02.annual, .95, na.rm = TRUE) # 58381

quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.annual.ITC, .05, na.rm = TRUE) # -38823
quantile(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.annual.ITC, .95, na.rm = TRUE) # 14595

# Plot all schools - 7% - LMP
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.annual > -31241 & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.annual < 72652),]$social_net_ben.lmp.dsire.07.annual, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(-40000, 80000), ylim = c(0, 20000), main = NULL, xlab = NULL)

# Plot all schools - 7% - Retail
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.07.annual > -49138 & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.07.annual < 50227),]$social_net_ben.ret.dsire.07.annual, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(-60000, 60000),  ylim = c(0, 20000), main = NULL, xlab = NULL)

# Plot all schools - 7% - ITC
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.annual.ITC > -81486 & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07.annual.ITC < 0),]$social_net_ben.lmp.dsire.07.annual.ITC, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(-100000,0), ylim = c(0,12000),  main = NULL, xlab = NULL)

# Plot all schools - 2% - LMP
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.annual > -14802 & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.annual < 73180),]$social_net_ben.lmp.dsire.02.annual, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(-20000, 80000), ylim = c(0, 20000), main = NULL, xlab = NULL)

# Plot all schools - 2% - Retail
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.02.annual > -25744 & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.dsire.02.annual < 58382),]$social_net_ben.ret.dsire.02.annual, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(-40000, 60000), ylim = c(0, 20000),  main = NULL, xlab = NULL)

# Plot all schools - 2% - ITC
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.annual.ITC > -38824 & AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02.annual.ITC < 14596),]$social_net_ben.lmp.dsire.02.annual.ITC, 
     breaks = "FD", col = "#6baed6", border = "#08519c", xlim = c(-40000, 20000), ylim = c(0, 25000), main = NULL, xlab = NULL)



# Information for results section after Figure 8
# Schools with lowest/highest private net-benefits 
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$school_net_ben.lmp.dsirerebate.07.annual) # med = -47k mean = -302k
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$school_net_ben.lmp.dsirerebate.07.annual) # med = -123k mean = -124k
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$school_net_ben.lmp.dsirerebate.07.annual) # med = -62K, mean = -71K

summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$school_net_ben.ret.dsirerebate.07.annual) # med = -36k mean = -268k
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$school_net_ben.ret.dsirerebate.07.annual) # med = -110k mean = -111k
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$school_net_ben.ret.dsirerebate.07.annual) # med = -54k mean = -64k

summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$annual.nb.ITC.lmp.dsirerebate.07) # med = 56k mean = 316k
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$annual.nb.ITC.lmp.dsirerebate.07) # med = 126k mean = 127k
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$annual.nb.ITC.lmp.dsirerebate.07) # med = 65k mean = 73k


# Schools with lowest/highest social net-benefits
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$social_net_ben.lmp.dsire.07.annual) # med = 5k mean = 53k
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$social_net_ben.lmp.dsire.07.annual) # med = 24k mean = 23k
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$social_net_ben.lmp.dsire.07.annual) # med = 123k mean = 139k

summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$social_net_ben.ret.dsire.07.annual) # med = 2k mean = 17k
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$social_net_ben.ret.dsire.07.annual) # med = 12k mean = 10k
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$social_net_ben.ret.dsire.07.annual) # med = 6k mean = 6k

summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "HigherEducation"),]$social_net_ben.lmp.dsire.07.annual.ITC) # med = -11k mean = -72k
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Public"),]$social_net_ben.lmp.dsire.07.annual.ITC) # med = -19k mean = -27k
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$SchoolType == "K12Private"),]$social_net_ben.lmp.dsire.07.annual.ITC) # med = -102k mean = -150k



####################################################################
## Make CDF plots for the presentation
####################################################################

# Plotting net benefits as CDFs
## EASIUR
# Marginal

hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02 != 0),]$school_net_ben.lmp.dsirerebate.02, breaks = "FD")
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07, breaks = "FD")
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02, breaks = "FD")
hist(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07, breaks = "FD")
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02 == 0),]) # 14,490
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07 == 0),]) # 14,490
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02 == 0),]) # 15,481
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07 == 0),]) # 15,481


# plot data
# Remove any projects where available space is 0
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$Total.Space == 0),]) # 14k
nrow(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[is.na(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$Total.Space),]) # 0

plot.data <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[-which(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$Total.Space == 0),]
nrow(plot.data)
hist(plot.data[which(plot.data$social_net_ben.lmp.dsire.07 > - 100 & plot.data$social_net_ben.lmp.dsire.07 < 100 ),]$social_net_ben.lmp.dsire.07, breaks = "FD")
hist(plot.data$social_net_ben.lmp.dsire.02, breaks = "FD")
hist(plot.data$school_net_ben.lmp.dsirerebate.07, breaks = "FD")
hist(plot.data$school_net_ben.lmp.dsirerebate.02, breaks = "FD")
nrow(plot.data[which(plot.data$school_net_ben.lmp.dsirerebate.02 == 0),])
nrow(plot.data[which(plot.data$school_net_ben.lmp.dsirerebate.07 == 0),])
nrow(plot.data[which(plot.data$social_net_ben.lmp.dsire.07 == 0),]) # 868
nrow(plot.data[which(plot.data$social_net_ben.lmp.dsire.02 == 0),]) # 868

# LMP
plot_frame <- data.frame(net_ben = numeric(), price = factor(), pvt_soc = factor(), discount = factor())

# based on mixed, using a 7% discount rate
pf_temp <- data.frame(net_ben  = numeric(length(plot.data$social_net_ben.lmp.dsire.07)),
                      price    = factor(length(plot.data$social_net_ben.lmp.dsire.07)), 
                      pvt_soc  = factor(length(plot.data$social_net_ben.lmp.dsire.07)),
                      discount = factor(length(plot.data$social_net_ben.lmp.dsire.07)))

pf_temp$net_ben  <- plot.data$social_net_ben.lmp.dsire.07.annual/1000
pf_temp$price    <- "Net generation valued at LMP"
pf_temp$pvt_soc  <- "Social"
pf_temp$discount <- "7% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)

pf_temp <- data.frame(net_ben  = numeric(length(plot.data$school_net_ben.lmp.dsirerebate.07)),
                      price    = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)), 
                      pvt_soc  = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)),
                      discount = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)))

pf_temp$net_ben  <- plot.data$school_net_ben.lmp.dsirerebate.07.annual/1000
pf_temp$price    <- "Net generation valued at LMP"
pf_temp$pvt_soc  <- "School"
pf_temp$discount <- "7% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)


pf_temp <- data.frame(net_ben  = numeric(length(plot.data$social_net_ben.lmp.dsire.07)),
                      price    = factor(length(plot.data$social_net_ben.lmp.dsire.07)), 
                      pvt_soc  = factor(length(plot.data$social_net_ben.lmp.dsire.07)),
                      discount = factor(length(plot.data$social_net_ben.lmp.dsire.07)))

pf_temp$net_ben  <- plot.data$social_net_ben.lmp.dsire.02.annual/1000
pf_temp$price    <- "Net generation valued at LMP"
pf_temp$pvt_soc  <- "Social"
pf_temp$discount <- "2% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)

pf_temp <- data.frame(net_ben  = numeric(length(plot.data$school_net_ben.lmp.dsirerebate.07)),
                      price    = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)), 
                      pvt_soc  = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)),
                      discount = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)))

pf_temp$net_ben  <- plot.data$school_net_ben.lmp.dsirerebate.02.annual/1000
pf_temp$price    <- "Net generation valued at LMP"
pf_temp$pvt_soc  <- "School"
pf_temp$discount <- "2% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)


# Valued at retail
pf_temp <- data.frame(net_ben  = numeric(length(plot.data$social_net_ben.lmp.dsire.07)),
                      price    = factor(length(plot.data$social_net_ben.lmp.dsire.07)), 
                      pvt_soc  = factor(length(plot.data$social_net_ben.lmp.dsire.07)),
                      discount = factor(length(plot.data$social_net_ben.lmp.dsire.07)))

pf_temp$net_ben  <- plot.data$social_net_ben.ret.dsire.07.annual/1000
pf_temp$price    <- "Net generation valued at Retail"
pf_temp$pvt_soc  <- "Social"
pf_temp$discount <- "7% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)

pf_temp <- data.frame(net_ben  = numeric(length(plot.data$school_net_ben.lmp.dsirerebate.07)),
                      price    = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)), 
                      pvt_soc  = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)),
                      discount = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)))

pf_temp$net_ben  <- plot.data$school_net_ben.ret.dsirerebate.07.annual/1000
pf_temp$price    <- "Net generation valued at Retail"
pf_temp$pvt_soc  <- "School"
pf_temp$discount <- "7% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)


pf_temp <- data.frame(net_ben  = numeric(length(plot.data$social_net_ben.lmp.dsire.07)),
                      price    = factor(length(plot.data$social_net_ben.lmp.dsire.07)), 
                      pvt_soc  = factor(length(plot.data$social_net_ben.lmp.dsire.07)),
                      discount = factor(length(plot.data$social_net_ben.lmp.dsire.07)))

pf_temp$net_ben  <- plot.data$social_net_ben.ret.dsire.02.annual/1000
pf_temp$price    <- "Net generation valued at Retail"
pf_temp$pvt_soc  <- "Social"
pf_temp$discount <- "2% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)

pf_temp <- data.frame(net_ben  = numeric(length(plot.data$school_net_ben.lmp.dsirerebate.07)),
                      price    = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)), 
                      pvt_soc  = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)),
                      discount = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)))

pf_temp$net_ben  <- plot.data$school_net_ben.ret.dsirerebate.02.annual/1000
pf_temp$price    <- "Net generation valued at Retail"
pf_temp$pvt_soc  <- "School"
pf_temp$discount <- "2% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)

# Look at the ITC conditions - selling excess at LMP
pf_temp <- data.frame(net_ben  = numeric(length(plot.data$social_net_ben.lmp.dsire.07)),
                      price    = factor(length(plot.data$social_net_ben.lmp.dsire.07)), 
                      pvt_soc  = factor(length(plot.data$social_net_ben.lmp.dsire.07)),
                      discount = factor(length(plot.data$social_net_ben.lmp.dsire.07)))

pf_temp$net_ben  <- plot.data$social_net_ben.lmp.dsire.07.annual.ITC/1000
pf_temp$price    <- "Third Party Ownership"
pf_temp$pvt_soc  <- "Social"
pf_temp$discount <- "7% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)

pf_temp <- data.frame(net_ben  = numeric(length(plot.data$school_net_ben.lmp.dsirerebate.07)),
                      price    = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)), 
                      pvt_soc  = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)),
                      discount = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)))

pf_temp$net_ben  <- plot.data$annual.nb.ITC.lmp.dsirerebate.07/1000
pf_temp$price    <- "Third Party Ownership"
pf_temp$pvt_soc  <- "School"
pf_temp$discount <- "7% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)

pf_temp <- data.frame(net_ben  = numeric(length(plot.data$social_net_ben.lmp.dsire.07)),
                      price    = factor(length(plot.data$social_net_ben.lmp.dsire.07)), 
                      pvt_soc  = factor(length(plot.data$social_net_ben.lmp.dsire.07)),
                      discount = factor(length(plot.data$social_net_ben.lmp.dsire.07)))

pf_temp$net_ben  <- plot.data$social_net_ben.lmp.dsire.02.annual.ITC/1000
pf_temp$price    <- "Third Party Ownership"
pf_temp$pvt_soc  <- "Social"
pf_temp$discount <- "2% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)

pf_temp <- data.frame(net_ben  = numeric(length(plot.data$school_net_ben.lmp.dsirerebate.07)),
                      price    = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)), 
                      pvt_soc  = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)),
                      discount = factor(length(plot.data$school_net_ben.lmp.dsirerebate.07)))

pf_temp$net_ben  <- plot.data$annual.nb.ITC.lmp.dsirerebate.02/1000
pf_temp$price    <- "Third Party Ownership"
pf_temp$pvt_soc  <- "School"
pf_temp$discount <- "2% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)



ggplot(data = plot_frame, aes(net_ben, colour = pvt_soc)) + stat_ecdf(size = 1.2) + 
  scale_color_manual(values=c("#a50f15", "#08519c")) +
  labs(x="Net Benefits ($TH)", y="Frequency", colour = "Perspective") +
  xlim(-400,200) + facet_grid(discount~price) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(axis.text=element_text(size = 12)) + geom_vline(xintercept = 0)



####################################################################
## Summarize results for states
####################################################################
plot.data <- ddply(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, ~State, summarize, 
                   capacity          <- sum(System_Size, na.rm = TRUE), # kW
                   generation        <- sum(power*System_Size*1000, na.rm = TRUE), # kWh/yr
                   generation.perkW  <- sum(power*System_Size*1000, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), #kWh/kW-yr
                   generation.perkWh <- sum(power*System_Size*1000, na.rm = TRUE)/sum(demand, na.rm = TRUE), # kWh/yr / kWh/yr
                   
                   nox_ben    <- sum(nox_ben, na.rm = TRUE), # $/yr
                   so2_ben    <- sum(so2_ben, na.rm = TRUE), # $/yr
                   pm25_ben   <- sum(pm25_ben, na.rm = TRUE), # $/yr
                   co2_ben    <- sum(co2_ben, na.rm = TRUE), # $/yr
                   
                   school_net_ben.lmp.dsirerebate.07  <- sum(school_net_ben.lmp.dsirerebate.07, na.rm = TRUE), # NPV $
                   school_net_ben.lmp.dsirerebate.02  <- sum(school_net_ben.lmp.dsirerebate.02, na.rm = TRUE), # NPV $
                   school_net_ben.ret.dsirerebate.07  <- sum(school_net_ben.ret.dsirerebate.07, na.rm = TRUE), # NPV $
                   school_net_ben.ret.dsirerebate.02  <- sum(school_net_ben.ret.dsirerebate.02, na.rm = TRUE), # NPV $
                   
                   social_net_ben.lmp.dsire.07        <- sum(social_net_ben.lmp.dsire.07, na.rm = TRUE), # NPV $
                   social_net_ben.lmp.dsire.02        <- sum(social_net_ben.lmp.dsire.02, na.rm = TRUE), # NPV $
                   social_net_ben.ret.dsire.07        <- sum(social_net_ben.ret.dsire.07, na.rm = TRUE), # NPV $
                   social_net_ben.ret.dsire.02        <- sum(social_net_ben.ret.dsire.02, na.rm = TRUE), # NPV $
                   
                   school_net_ben.lmp.dsirerebate.07.annual  <- sum(school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE), # $/yr
                   school_net_ben.lmp.dsirerebate.02.annual  <- sum(school_net_ben.lmp.dsirerebate.02.annual, na.rm = TRUE), # $/yr
                   school_net_ben.ret.dsirerebate.07.annual  <- sum(school_net_ben.ret.dsirerebate.07.annual, na.rm = TRUE), # $/yr
                   school_net_ben.ret.dsirerebate.02.annual  <- sum(school_net_ben.ret.dsirerebate.02.annual, na.rm = TRUE), # $/yr
                   
                   social_net_ben.lmp.dsire.07.annual        <- sum(social_net_ben.lmp.dsire.07.annual, na.rm = TRUE), # $/yr
                   social_net_ben.lmp.dsire.02.annual        <- sum(social_net_ben.lmp.dsire.02.annual, na.rm = TRUE), # $/yr
                   social_net_ben.ret.dsire.07.annual        <- sum(social_net_ben.ret.dsire.07.annual, na.rm = TRUE), # $/yr
                   social_net_ben.ret.dsire.02.annual        <- sum(social_net_ben.ret.dsire.02.annual, na.rm = TRUE), # $/yr
                   
                   school_net_ben.lmp.dsirerebate.07.annual.perkW  <- sum(school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                   school_net_ben.lmp.dsirerebate.02.annual.perkW  <- sum(school_net_ben.lmp.dsirerebate.02.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                   school_net_ben.ret.dsirerebate.07.annual.perkW  <- sum(school_net_ben.ret.dsirerebate.07.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                   school_net_ben.ret.dsirerebate.02.annual.perkW  <- sum(school_net_ben.ret.dsirerebate.02.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                   
                   social_net_ben.lmp.dsire.07.annual.perkW        <- sum(social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                   social_net_ben.lmp.dsire.02.annual.perkW        <- sum(social_net_ben.lmp.dsire.02.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                   social_net_ben.ret.dsire.07.annual.perkW        <- sum(social_net_ben.ret.dsire.07.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                   social_net_ben.ret.dsire.02.annual.perkW        <- sum(social_net_ben.ret.dsire.02.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE),
                   
                   annual.nb.ITC.lmp.dsirerebate.07.perkW          <- sum(annual.nb.ITC.lmp.dsirerebate.07, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                   annual.nb.ITC.lmp.dsirerebate.02.perkW          <- sum(annual.nb.ITC.lmp.dsirerebate.02, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                   annual.nb.ITC.ret.dsirerebate.07.perkW          <- sum(annual.nb.ITC.ret.dsirerebate.07, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                   annual.nb.ITC.ret.dsirerebate.02.perkW          <- sum(annual.nb.ITC.ret.dsirerebate.02, na.rm = TRUE)/sum(System_Size, na.rm = TRUE),
                   
                   social_net_ben.lmp.dsire.07.annual.ITC.perkW    <- sum(social_net_ben.lmp.dsire.07.annual.ITC, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                   social_net_ben.lmp.dsire.02.annual.ITC.perkW    <- sum(social_net_ben.lmp.dsire.02.annual.ITC, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                   social_net_ben.ret.dsire.07.annual.ITC.perkW    <- sum(social_net_ben.ret.dsire.07.annual.ITC, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                   social_net_ben.ret.dsire.02.annual.ITC.perkW    <- sum(social_net_ben.ret.dsire.02.annual.ITC, na.rm = TRUE)/sum(System_Size, na.rm = TRUE),
                   
                   sc.dsire.lmp.annual                             <- sum(sc.dsire.lmp.annual, na.rm = TRUE), # $/yr
                   sc.dsire.retminuslmp.annual                     <- sum(sc.dsire.retminuslmp.annual, na.rm = TRUE), # $/yr
                   sc.dsire.rebate                                 <- sum(sc.dsire.lmp, na.rm = TRUE),
                   co2.ben.total                                   <- sum(- pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$co2_ben), na.rm = TRUE), #$
                   aq.ben.total                                    <- sum(- pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$aq_ben), na.rm = TRUE)) # $


names(plot.data)[2] <- "capacity"
names(plot.data)[3] <- "generation"
names(plot.data)[4] <- "generation.perkW"
names(plot.data)[5] <- "generation.perkWh"

names(plot.data)[6] <- "nox_ben"
names(plot.data)[7] <- "so2_ben"
names(plot.data)[8] <- "pm25_ben"
names(plot.data)[9] <- "co2_ben"

names(plot.data)[10] <- "school_net_ben.lmp.dsirerebate.07"
names(plot.data)[11] <- "school_net_ben.lmp.dsirerebate.02"
names(plot.data)[12] <- "school_net_ben.ret.dsirerebate.07"
names(plot.data)[13] <- "school_net_ben.ret.dsirerebate.02"

names(plot.data)[14] <- "social_net_ben.lmp.dsire.07"
names(plot.data)[15] <- "social_net_ben.lmp.dsire.02"
names(plot.data)[16] <- "social_net_ben.ret.dsire.07"
names(plot.data)[17] <- "social_net_ben.ret.dsire.02"

names(plot.data)[18] <- "school_net_ben.lmp.dsirerebate.07.annual"
names(plot.data)[19] <- "school_net_ben.lmp.dsirerebate.02.annual"
names(plot.data)[20] <- "school_net_ben.ret.dsirerebate.07.annual"
names(plot.data)[21] <- "school_net_ben.ret.dsirerebate.02.annual"

names(plot.data)[22] <- "social_net_ben.lmp.dsire.07.annual"
names(plot.data)[23] <- "social_net_ben.lmp.dsire.02.annual"
names(plot.data)[24] <- "social_net_ben.ret.dsire.07.annual"
names(plot.data)[25] <- "social_net_ben.ret.dsire.02.annual"

names(plot.data)[26] <- "school_net_ben.lmp.dsirerebate.07.annual.perkW"
names(plot.data)[27] <- "school_net_ben.lmp.dsirerebate.02.annual.perkW"
names(plot.data)[28] <- "school_net_ben.ret.dsirerebate.07.annual.perkW"
names(plot.data)[29] <- "school_net_ben.ret.dsirerebate.02.annual.perkW"

names(plot.data)[30] <- "social_net_ben.lmp.dsire.07.annual.perkW"
names(plot.data)[31] <- "social_net_ben.lmp.dsire.02.annual.perkW"
names(plot.data)[32] <- "social_net_ben.ret.dsire.07.annual.perkW"
names(plot.data)[33] <- "social_net_ben.ret.dsire.02.annual.perkW"

names(plot.data)[34] <- "annual.nb.ITC.lmp.dsirerebate.07.perkW"
names(plot.data)[35] <- "annual.nb.ITC.lmp.dsirerebate.02.perkW"
names(plot.data)[36] <- "annual.nb.ITC.ret.dsirerebate.07.perkW"
names(plot.data)[37] <- "annual.nb.ITC.ret.dsirerebate.02.perkW"

names(plot.data)[38] <- "social_net_ben.lmp.dsire.07.annual.ITC.perkW"
names(plot.data)[39] <- "social_net_ben.lmp.dsire.02.annual.ITC.perkW"
names(plot.data)[40] <- "social_net_ben.ret.dsire.07.annual.ITC.perkW"
names(plot.data)[41] <- "social_net_ben.ret.dsire.02.annual.ITC.perkW"

names(plot.data)[42] <- "sc.dsire.lmp.annual"
names(plot.data)[43] <- "sc.dsire.retminuslmp.annual"
names(plot.data)[44] <- "sc.dsire.rebate"
names(plot.data)[45] <- "co2.ben.total"
names(plot.data)[46] <- "aq.ben.total"



###############################################
# Rosenfeld back-up calculation for California
###############################################

# Total annual carbon emissions benefits:
plot.data[which(plot.data$State == "CA"),]$co2_ben # 180,301,423 $/yr

# Total annual NOx, SO2, and PM2.5 emissions benefits:
plot.data[which(plot.data$State == "CA"),]$nox_ben + plot.data[which(plot.data$State == "CA"),]$so2_ben + plot.data[which(plot.data$State == "CA"),]$pm25_ben # 57,354,953 $/yr

# convert this to tons of carbon:
plot.data[which(plot.data$State == "CA"),]$co2_ben/40 # 4,507,536 ton Co2

# Look at the annual state rebate:
plot.data[which(plot.data$State == "CA"),]$sc.dsire.lmp.annual # 723,248,913 $/yr

# Look at the annual retail cross-subsidy:
plot.data[which(plot.data$State == "CA"),]$sc.dsire.retminuslmp.annual # 266584623 $/yr

# Total capacity for CA
plot.data[which(plot.data$State == "CA"),]$capacity # 5,548,233 kW

# Total rebate made available in CA
plot.data[which(plot.data$State == "CA"),]$sc.dsire.rebate # $7662109291

# Total offset lifetime CO2 damages
plot.data[which(plot.data$State == "CA"),]$co2.ben.total/10^6 # $ 25,660 $M

# Total offset lifetime SO2, NOX, and PM2.5 damages
plot.data[which(plot.data$State == "CA"),]$aq.ben.total/10^6 # $ 20000 $M

# Try calculating the offset lifetime CO2 by hand here:
- pv.annuity(r = disc.07, n = life, plot.data[which(plot.data$State == "CA"),]$co2_ben)/10^6 # 1910 $M

# Try calculating the offset lifetime CO2 by hand here:
- pv.annuity(r = disc.07, n = life, plot.data[which(plot.data$State == "CA"),]$nox_ben)/10^6 - pv.annuity(r = disc.07, n = life, plot.data[which(plot.data$State == "CA"),]$so2_ben)/10^6 - pv.annuity(r = disc.07, n = life, plot.data[which(plot.data$State == "CA"),]$pm25_ben)/10^6 # 607

# Create results for table in SI to compare states benefits and rebates
states <- c("NV", "CA", "VT", "DE", "NH", "NY", "WI", "TX", "MA")

state.ben <- function(data, states) {
  
  functiondata <- data
  
  output <- matrix(NA, nrow=3, ncol=length(states))
  colnames(output) <- c("NV", "CA", "VT", "DE", "NH", "NY", "WI", "TX", "MA")
  
  for (i in 1:length(states)){
    
    # total capacity (GW)
    output[1, i] <- functiondata[which(plot.data$State == states[i]),]$capacity
    
    # CO2 benefits ($/yr)
    output[2, i] <- functiondata[which(plot.data$State == states[i]),]$co2_ben
    
    # nox, so2, pm2.5 benefits ($/yr)
    output[3, i] <- functiondata[which(plot.data$State == states[i]),]$nox_ben + functiondata[which(plot.data$State == states[i]),]$so2_ben + functiondata[which(plot.data$State == states[i]),]$pm25_ben
    
  }
  
  return(output)
  
}

state.bens <- state.ben(data = plot.data, states = states) 


##################
# Make state plots
##################
library(maps)
library(mapproj)
library(ggplot2)


### Stacked bar chart of CO2 and pollution
# First, sort the bars according to overall descent
plot.data$Total.Ben <- plot.data$nox_ben + plot.data$so2_ben + plot.data$pm25_ben + plot.data$co2_ben
plot.data.sort <- plot.data[order(-plot.data$Total.Ben),]
# Just take top 10 states for plotting purposes
plot.data.sort <- plot.data.sort[1:10,]

#ggplot(data = plot.data.sort, aes (x = State, y = nox_ben)) + geom_col()
#ggplot(data = plot.data.sort, aes (x = State, y = so2_ben)) + geom_col()
#ggplot(data = plot.data.sort, aes (x = State, y = pm25_ben)) + geom_col()
#ggplot(data = plot.data.sort, aes (x = State, y = co2_ben))  + geom_col()

# rearrange data
plot.data.sort <- plot.data.sort[,c("State", "nox_ben", "so2_ben", "pm25_ben", "co2_ben","sc.dsire.retminuslmp.annual", "sc.dsire.lmp.annual","Total.Ben")]
plot.data.sort <- reshape(data = plot.data.sort, varying = list(names(plot.data.sort)[2:7]), idvar = c("State","Total.Ben"), direction = "long",
                          v.names = "value", timevar = "variable")
plot.data.sort$CostorBen <- NA
plot.data.sort[which(plot.data.sort$variable == 1),]$CostorBen <- "Benefit"
plot.data.sort[which(plot.data.sort$variable == 2),]$CostorBen <- "Benefit"
plot.data.sort[which(plot.data.sort$variable == 3),]$CostorBen <- "Benefit"
plot.data.sort[which(plot.data.sort$variable == 4),]$CostorBen <- "Benefit"
plot.data.sort[which(plot.data.sort$variable == 5),]$CostorBen <- "Cost"
plot.data.sort[which(plot.data.sort$variable == 6),]$CostorBen <- "Cost"
plot.data.sort$State <- factor(plot.data.sort$State, levels = c("TX", "CA", "IL", "OH", "IN", "FL",  "PA","MI", "NC", "NY"))

plot.data.sort$value <- plot.data.sort$value/(10^6)



# put it all together
# Figure X in Paper
my.cols <- c("#a6bddb", "#74a9cf", "#2b8cbe", "#045a8d", "#fb6a4a", "#a50f15")
ggplot(data = plot.data.sort, aes (x = CostorBen, y = value, fill = factor(variable))) + facet_grid(~State) + geom_col() + 
  scale_fill_manual(values = my.cols, guide="legend") + labs(x="Annual Benefits or Costs", y="Annual Value ($M/yr)") + theme(axis.text.x=element_text(angle=90, hjust=1))





#### Chloropleth of states and the school net benefits ####

## EASIUR
# Marginal
# use AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas
# Consider school net benefits, discount rate of 7%, selling back at LMP: school_net_ben.lmp.07



# load map of states dataframe
all_states <- map_data("state")

# Merge state names for the plotting data
states.abbrev <- read.csv(file = "C:/Users/hanusnil/Box Sync/states.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
names(states.abbrev)[1] <- "region"
names(states.abbrev)[2] <- "State"
plot.data <- merge(plot.data, states.abbrev, by="State")

# merge with actual data to plot
map.plot <- merge(all_states, plot.data, by="region")

# reorder so lines don't get messed up
map.plot  <- map.plot[order(map.plot$order),]


# Figure 3 in paper
# map of school annual generation
hist(map.plot$generation, breaks = "FD")
summary(map.plot$generation/10^9)

# Make 4 breaks
my.cols <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")

map.plot$brks <- cut(map.plot$generation/(10^9), 
                     breaks=c(0, 1, 2, 3, 4, 13), 
                     labels=c("0 - 1", "1 - 2", "2 - 3", 
                              "3 - 4", "> 4"))

ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual Generation (TWh/yr)\n", values = my.cols, guide="legend") + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  


# Figure 3 in paper
# Map generation per demand
hist(map.plot$generation.perkW/1000, breaks = "FD")
summary(map.plot$generation.perkW/1000)


my.cols <- c("#eff3ff", "#bdd7e7", "#6baed6", "#3182bd", "#08519c")

map.plot$brks <- cut(map.plot$generation.perkW/(1000), 
                     breaks=c(1.2, 1.4, 1.6, 1.8, 2, 2.2), 
                     labels=c("1.2 - 1.4", "1.4 - 1.6", "1.6 - 1.8", "1.8 - 2.0", "2.0 - 2.2"))

ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual Generation per kW (TWh/kW-yr)\n", values = my.cols, guide="legend") + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  


# Map social benefits
hist(map.plot$social_net_ben.ret.dsire.07/(10^9), breaks = "FD")
summary(map.plot$social_net_ben.ret.dsire.07/(10^9))

# Make 4 breaks
my.cols <- c("#cb181d", "#fb6a4a", "#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")

map.plot$brks <- cut(map.plot$social_net_ben.lmp.dsire.07/(10^9), 
                     breaks=c(-8, -1, -.5, 0, .5, 1, 1.5, 2, 4), 
                     labels=c("< -1", "-1 to -0.5", "-0.5 to 0", 
                              "0 to 0.5", "0.5 to 1", "1 to 1.5", "1.5 to 2", ">2"))


ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual Social Benefits ($B/yr)\n", values = my.cols, guide="legend") + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  



ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=social_net_ben.lmp.dsire.07/(10^9)), colour="black", size=0.1) + 
  scale_fill_continuous(low = "#cb181d", high = "#225ea8", guide="colorbar") + theme_bw()  + 
  labs(fill = "Total Annual Social Benefits ($B/yr)",x = "", y = "") +
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) 



# Map school benefits
hist(map.plot$school_net_ben.ret.dsirerebate.07/(10^9), breaks = "FD")
summary(map.plot$school_net_ben.ret.dsirerebate.07/(10^9))
summary(map.plot[which(map.plot$State == "TX"),]$school_net_ben.ret.dsirerebate.07/(10^9))

# Make 4 breaks
my.cols <- c("#a50f15", "#de2d26", "#fb6a4a", "#fc9272", "#fcbba1", "#fee5d9", "#c6dbef","#08519c")

map.plot$brks <- cut(map.plot$school_net_ben.ret.dsirerebate.07/(10^9), 
                     breaks=c(-21, -5,-4, -3, -2, -1, 0, 1, 2), 
                     labels=c("< -5", "-5 to -4", "-4 to -3", "-3 to -2","-2 to -1", "-1 to 0", "0 to 1", "1 to 2"))


ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual School Benefits ($B/yr)\n", values = my.cols, guide="legend") + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  



# Paper Figure 10
# Annualized school net benefits / kW 
# 7%, LMP
hist(map.plot$school_net_ben.lmp.dsirerebate.07.annual.perkW, breaks = "FD") # min = -311, max = -49
summary(map.plot$school_net_ben.lmp.dsirerebate.07.annual.perkW)

# 7%, retail
hist(map.plot$school_net_ben.ret.dsirerebate.07.annual.perkW, breaks = "FD") # min = -294, max = -4.5
summary(map.plot$school_net_ben.ret.dsirerebate.07.annual.perkW)

# 7%, itc
hist(map.plot$annual.nb.ITC.lmp.dsirerebate.07.perkW, breaks = "FD") # min = 158, max = 274
summary(map.plot$annual.nb.ITC.lmp.dsirerebate.07.perkW)

# Make 13 breaks
my.cols <- c("#99000d","#cb181d","#ef3b2c","#fb6a4a","#fc9272","#fcbba1","#fee5d9","#eff3ff","#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c")

map.plot$brks <- cut(map.plot$school_net_ben.lmp.dsirerebate.07.annual.perkW, 
                     breaks=c(-350, -300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), 
                     labels=c("-350 to -300", "-300 to -250", "-250 to -200", "-200 to -150","-150 to -100", "-100 to -50", "-50 to 0", "0 to 50", "50 to 100", "100 to 150", "150 to 200", "200 to 250", "250 to 300"))
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual School Benefits - LMP ($B/yr)\n", values = my.cols, guide=FALSE, drop = FALSE) + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  

map.plot$brks <- cut(map.plot$school_net_ben.lmp.dsirerebate.07.annual.perkW, 
                     breaks=c(-350, -300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), 
                     labels=c("-350 to -300", "-300 to -250", "-250 to -200", "-200 to -150","-150 to -100", "-100 to -50", "-50 to 0", "0 to 50", "50 to 100", "100 to 150", "150 to 200", "200 to 250", "250 to 300"))
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual School Benefits - LMP ($B/yr)\n", values = my.cols, guide="legend", drop = FALSE) + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  




map.plot$brks <- cut(map.plot$school_net_ben.ret.dsirerebate.07.annual.perkW, 
                     breaks=c(-350, -300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), 
                     labels=c("-350 to -300", "-300 to -250", "-250 to -200", "-200 to -150","-150 to -100", "-100 to -50", "-50 to 0", "0 to 50", "50 to 100", "100 to 150", "150 to 200", "200 to 250", "250 to 300"))
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual School Benefits - Ret ($B/yr)\n", values = my.cols, guide="legend", drop = FALSE) + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  

map.plot$brks <- cut(map.plot$school_net_ben.ret.dsirerebate.07.annual.perkW, 
                     breaks=c(-350, -300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), 
                     labels=c("-350 to -300", "-300 to -250", "-250 to -200", "-200 to -150","-150 to -100", "-100 to -50", "-50 to 0", "0 to 50", "50 to 100", "100 to 150", "150 to 200", "200 to 250", "250 to 300"))
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual School Benefits - Ret ($B/yr)\n", values = my.cols, guide=FALSE, drop = FALSE) + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  




map.plot$brks <- cut(map.plot$annual.nb.ITC.lmp.dsirerebate.07.perkW, 
                     breaks=c(-350, -300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), 
                     labels=c("-350 to -300", "-300 to -250", "-250 to -200", "-200 to -150","-150 to -100", "-100 to -50", "-50 to 0", "0 to 50", "50 to 100", "100 to 150", "150 to 200", "200 to 250", "250 to 300"))
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual School Benefits - ITC ($B/yr)\n", values = my.cols, guide="legend", drop = FALSE) + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  

map.plot$brks <- cut(map.plot$annual.nb.ITC.lmp.dsirerebate.07.perkW, 
                     breaks=c(-350, -300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), 
                     labels=c("-350 to -300", "-300 to -250", "-250 to -200", "-200 to -150","-150 to -100", "-100 to -50", "-50 to 0", "0 to 50", "50 to 100", "100 to 150", "150 to 200", "200 to 250", "250 to 300"))
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual School Benefits - ITC ($B/yr)\n", values = my.cols, guide= FALSE , drop = FALSE) + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  



# Continue Figure 10
# Annualized social net benefits / kW 
# 7%, LMP
hist(map.plot$social_net_ben.lmp.dsire.07.annual.perkW, breaks = "FD") # min = -107, max = 100
summary(map.plot$social_net_ben.lmp.dsire.07.annual.perkW)

# 7%, retail
hist(map.plot$social_net_ben.ret.dsire.07.annual.perkW, breaks = "FD") # min = -139, max = 68
summary(map.plot$social_net_ben.ret.dsire.07.annual.perkW)

# 7%, itc
hist(map.plot$social_net_ben.lmp.dsire.07.annual.ITC.perkW, breaks = "FD")
summary(map.plot$social_net_ben.lmp.dsire.07.annual.ITC.perkW) # min = -166, max = -8

# Make 13 breaks
my.cols <- c("#99000d","#cb181d","#ef3b2c","#fb6a4a","#fc9272","#fcbba1","#fee5d9","#eff3ff","#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c")
map.plot$brks <- cut(map.plot$social_net_ben.lmp.dsire.07.annual.perkW, 
                     breaks=c(-350, -300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), 
                     labels=c("-350 to -300", "-300 to -250", "-250 to -200", "-200 to -150","-150 to -100", "-100 to -50", "-50 to 0", "0 to 50", "50 to 100", "100 to 150", "150 to 200", "200 to 250", "250 to 300"))
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual Social Benefits - LMP ($B/yr)\n", values = my.cols, guide="legend", drop = FALSE) + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  

ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual Social Benefits - LMP ($B/yr)\n", values = my.cols, guide=FALSE, drop = FALSE) + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  




map.plot$brks <- cut(map.plot$social_net_ben.ret.dsire.07.annual.perkW, 
                     breaks=c(-350, -300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), 
                     labels=c("-350 to -300", "-300 to -250", "-250 to -200", "-200 to -150","-150 to -100", "-100 to -50", "-50 to 0", "0 to 50", "50 to 100", "100 to 150", "150 to 200", "200 to 250", "250 to 300"))
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual Social Benefits - Ret ($B/yr)\n", values = my.cols, guide="legend", drop = FALSE) + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  

ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual Social Benefits - Ret ($B/yr)\n", values = my.cols, guide= FALSE, drop = FALSE) + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  





map.plot$brks <- cut(map.plot$social_net_ben.lmp.dsire.07.annual.ITC.perkW, 
                     breaks=c(-350, -300, -250, -200, -150, -100, -50, 0, 50, 100, 150, 200, 250, 300), 
                     labels=c("-350 to -300", "-300 to -250", "-250 to -200", "-200 to -150","-150 to -100", "-100 to -50", "-50 to 0", "0 to 50", "50 to 100", "100 to 150", "150 to 200", "200 to 250", "250 to 300"))
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual Social Benefits - ITC ($B/yr)\n", values = my.cols, guide="legend", drop = FALSE) + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual Social Benefits - ITC ($B/yr)\n", values = my.cols, guide= FALSE, drop = FALSE) + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  


# print a large scale
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual Social Benefits - ITC ($B/yr)\n", values = my.cols, guide="legend", drop = FALSE) + theme_bw()  + 
  theme(legend.key.size = unit(1, "cm")) + labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  


########################
# Sensitivity Analysis #
########################

# Assume using EASIUR
# Report at LMP excess generation
# Median Value

# Project Installation Costs
# 0% to 500% of LBNL value
# 10% increments

sensitivity <- seq(0, 5, by = 0.1)

project.sensitivity <- function(data, sensitivity) {
  
  functiondata <- data
  
  output <- matrix(NA, nrow=length(sensitivity), ncol=2)
  colnames(output) <- c("project.school", "project.social")
  
  for (i in 1:length(sensitivity)){
    
    # School costs and benefits
    functiondata$schoolc.dsire  <- 3836.97*(sensitivity[i])*functiondata$System_Size - functiondata$dsire.rebate
    functiondata$demand         <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*1000 # kWh/yr
    functiondata$cost.savings   <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*functiondata$price_ret*1000/100 # $/yr
    functiondata$elec.sales.lmp <- functiondata$System_Size*functiondata$net_power*functiondata$lmp # $/yr
    functiondata$elec.sales.ret <- functiondata$System_Size*functiondata$net_power*functiondata$price_ret*1000/100 # $/yr
    functiondata$schoolb.lmp    <- functiondata$cost.savings + functiondata$elec.sales.lmp  # $/yr
    functiondata$schoolb.ret    <- functiondata$cost.savings + functiondata$elec.sales.ret  # $/yr
    
    # Social Costs
    functiondata$sc.dsire.lmp     <- functiondata$dsire.rebate
    functiondata$sc.dsire.ret.07  <- functiondata$dsire.rebate        - pv.annuity(r = .07, n = 20, functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
    functiondata$sc.dsire.ret.02  <- functiondata$dsire.rebate        - pv.annuity(r = .02, n = 20, functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
    
    # Social Benefits
    functiondata$nox_ben  <- functiondata$System_Size*functiondata$nox_dam_eas # $/yr
    functiondata$so2_ben  <- functiondata$System_Size*functiondata$so2_dam_eas # $/yr
    functiondata$pm25_ben <- functiondata$System_Size*functiondata$pm25_dam_eas # $/yr
    functiondata$aq_ben   <- functiondata$System_Size*functiondata$pol_dam # $/yr
    functiondata$co2_ben  <- functiondata$System_Size*functiondata$co2_dam # $/yr
    functiondata$sb       <- functiondata$aq_ben + functiondata$co2_ben # $/yr
    
    # Calculate net school benefits for next 20 years
    # - upfront costs - 10 year inverter replacement - 20 years of maintenance + 20 years of benefits
    functiondata$school_net_ben.lmp.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .07, n = 20, functiondata$schoolb.lmp) + pv.annuity(r = .07, n = 20, 15*functiondata$System_Size)
    functiondata$school_net_ben.lmp.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20, functiondata$schoolb.lmp) + pv.annuity(r = .02, n = 20, 15*functiondata$System_Size)
    functiondata$school_net_ben.ret.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .07, n = 20, functiondata$schoolb.ret) + pv.annuity(r = .07, n = 20, 15*functiondata$System_Size)
    functiondata$school_net_ben.ret.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20, functiondata$schoolb.ret) + pv.annuity(r = .02, n = 20, 15*functiondata$System_Size)
    
    # Calculate net social benefits for next 20 years
    functiondata$social_net_ben.lmp.dsire.07     <- -functiondata$sc.dsire.lmp    - pv.annuity(r = .07, n = 20, functiondata$sb)
    functiondata$social_net_ben.lmp.dsire.02     <- -functiondata$sc.dsire.lmp    - pv.annuity(r = .02, n = 20, functiondata$sb)
    functiondata$social_net_ben.ret.dsire.07     <- -functiondata$sc.dsire.ret.07 - pv.annuity(r = .07, n = 20, functiondata$sb)
    functiondata$social_net_ben.ret.dsire.02     <- -functiondata$sc.dsire.ret.02 - pv.annuity(r = .02, n = 20, functiondata$sb)
    
    # Calculate annuity factors and look at annualized net benefits
    annuity.07 <- (1-(1+.07)^(-20))/.07
    annuity.02 <- (1-(1+.02)^(-20))/.02
    
    # Calculate annualized net-benefits
    # Schools
    functiondata$school_net_ben.lmp.dsirerebate.07.annual     <- functiondata$school_net_ben.lmp.dsirerebate.07/annuity.07
    functiondata$school_net_ben.lmp.dsirerebate.02.annual     <- functiondata$school_net_ben.lmp.dsirerebate.02/annuity.02
    functiondata$school_net_ben.ret.dsirerebate.07.annual     <- functiondata$school_net_ben.ret.dsirerebate.07/annuity.07
    functiondata$school_net_ben.ret.dsirerebate.02.annual     <- functiondata$school_net_ben.ret.dsirerebate.02/annuity.02
    
    # Social
    functiondata$social_net_ben.lmp.dsire.07.annual     <- functiondata$social_net_ben.lmp.dsire.07/annuity.07
    functiondata$social_net_ben.lmp.dsire.02.annual     <- functiondata$social_net_ben.lmp.dsire.02/annuity.02
    functiondata$social_net_ben.ret.dsire.07.annual     <- functiondata$social_net_ben.ret.dsire.07/annuity.07
    functiondata$social_net_ben.ret.dsire.02.annual     <- functiondata$social_net_ben.ret.dsire.02/annuity.02
    
    # Output net benefit
    # School net-benefit
    output[i, 1] <- median(functiondata$school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)
    
    # Social net-benefit
    output[i, 2] <- median(functiondata$social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)
    
  }
  
  return(output)
  
}

project.sens <- project.sensitivity(data = AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, sensitivity = sensitivity) 



# Discount Rate
# 0% to 500% of 7% value
# 10% increments

sensitivity <- seq(0, 5, by = 0.1)

discount.sensitivity <- function(data, sensitivity) {
  
  functiondata <- data
  
  output <- matrix(NA, nrow=length(sensitivity), ncol=2)
  colnames(output) <- c("discount.school", "discount.social")
  
  for (i in 1:length(sensitivity)){
    
    if (i == 1){
      
      # School costs and benefits
      functiondata$schoolc.dsire  <- 3836.97*functiondata$System_Size - functiondata$dsire.rebate
      functiondata$demand         <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*1000 # kWh/yr
      functiondata$cost.savings   <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*functiondata$price_ret*1000/100 # $/yr
      functiondata$elec.sales.lmp <- functiondata$System_Size*functiondata$net_power*functiondata$lmp # $/yr
      functiondata$elec.sales.ret <- functiondata$System_Size*functiondata$net_power*functiondata$price_ret*1000/100 # $/yr
      functiondata$schoolb.lmp    <- functiondata$cost.savings + functiondata$elec.sales.lmp  # $/yr
      functiondata$schoolb.ret    <- functiondata$cost.savings + functiondata$elec.sales.ret  # $/yr
      
      # Social Costs
      functiondata$sc.dsire.lmp     <- functiondata$dsire.rebate
      functiondata$sc.dsire.ret.07  <- functiondata$dsire.rebate        + 20*functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp)
      functiondata$sc.dsire.ret.02  <- functiondata$dsire.rebate        + 20*functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp)
      
      # Social Benefits
      functiondata$nox_ben  <- functiondata$System_Size*functiondata$nox_dam_eas # $/yr
      functiondata$so2_ben  <- functiondata$System_Size*functiondata$so2_dam_eas # $/yr
      functiondata$pm25_ben <- functiondata$System_Size*functiondata$pm25_dam_eas # $/yr
      functiondata$aq_ben   <- functiondata$System_Size*functiondata$pol_dam # $/yr
      functiondata$co2_ben  <- functiondata$System_Size*functiondata$co2_dam # $/yr
      functiondata$sb       <- functiondata$aq_ben + functiondata$co2_ben # $/yr
      
      # Calculate net school benefits for next 20 years
      # - upfront costs - 10 year inverter replacement - 20 years of maintenance + 20 years of benefits
      functiondata$school_net_ben.lmp.dsirerebate.07     <- -functiondata$schoolc.dsire - 120*functiondata$System_Size + 20*functiondata$schoolb.lmp - 20*15*functiondata$System_Size
      functiondata$school_net_ben.lmp.dsirerebate.02     <- -functiondata$schoolc.dsire - 120*functiondata$System_Size + 20*functiondata$schoolb.lmp - 20*15*functiondata$System_Size
      functiondata$school_net_ben.ret.dsirerebate.07     <- -functiondata$schoolc.dsire - 120*functiondata$System_Size + 20*functiondata$schoolb.ret - 20*15*functiondata$System_Size
      functiondata$school_net_ben.ret.dsirerebate.02     <- -functiondata$schoolc.dsire - 120*functiondata$System_Size + 20*functiondata$schoolb.ret - 20*15*functiondata$System_Size
      
      # Calculate net social benefits for next 20 years
      functiondata$social_net_ben.lmp.dsire.07     <- -functiondata$sc.dsire.lmp    + 20*functiondata$sb
      functiondata$social_net_ben.lmp.dsire.02     <- -functiondata$sc.dsire.lmp    + 20*functiondata$sb
      functiondata$social_net_ben.ret.dsire.07     <- -functiondata$sc.dsire.ret.07 + 20*functiondata$sb
      functiondata$social_net_ben.ret.dsire.02     <- -functiondata$sc.dsire.ret.02 + 20*functiondata$sb
      
      
      # Calculate annualized net-benefits
      # Schools
      functiondata$school_net_ben.lmp.dsirerebate.07.annual     <- functiondata$school_net_ben.lmp.dsirerebate.07/20
      functiondata$school_net_ben.lmp.dsirerebate.02.annual     <- functiondata$school_net_ben.lmp.dsirerebate.02/20
      functiondata$school_net_ben.ret.dsirerebate.07.annual     <- functiondata$school_net_ben.ret.dsirerebate.07/20
      functiondata$school_net_ben.ret.dsirerebate.02.annual     <- functiondata$school_net_ben.ret.dsirerebate.02/20
      
      # Social
      functiondata$social_net_ben.lmp.dsire.07.annual     <- functiondata$social_net_ben.lmp.dsire.07/20
      functiondata$social_net_ben.lmp.dsire.02.annual     <- functiondata$social_net_ben.lmp.dsire.02/20
      functiondata$social_net_ben.ret.dsire.07.annual     <- functiondata$social_net_ben.ret.dsire.07/20
      functiondata$social_net_ben.ret.dsire.02.annual     <- functiondata$social_net_ben.ret.dsire.02/20
      
      # Output net benefit
      # School net-benefit
      output[i, 1] <- median(functiondata$school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)
      
      # Social net-benefit
      output[i, 2] <- median(functiondata$social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)  
      
    } else {
      
      # School costs and benefits
      functiondata$schoolc.dsire  <- 3836.97*functiondata$System_Size - functiondata$dsire.rebate
      functiondata$demand         <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*1000 # kWh/yr
      functiondata$cost.savings   <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*functiondata$price_ret*1000/100 # $/yr
      functiondata$elec.sales.lmp <- functiondata$System_Size*functiondata$net_power*functiondata$lmp # $/yr
      functiondata$elec.sales.ret <- functiondata$System_Size*functiondata$net_power*functiondata$price_ret*1000/100 # $/yr
      functiondata$schoolb.lmp    <- functiondata$cost.savings + functiondata$elec.sales.lmp  # $/yr
      functiondata$schoolb.ret    <- functiondata$cost.savings + functiondata$elec.sales.ret  # $/yr
      
      # Social Costs
      functiondata$sc.dsire.lmp     <- functiondata$dsire.rebate
      functiondata$sc.dsire.ret.07  <- functiondata$dsire.rebate        - pv.annuity(r = .07*(sensitivity[i]), n = 20, functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
      functiondata$sc.dsire.ret.02  <- functiondata$dsire.rebate        - pv.annuity(r = .02, n = 20, functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
      
      # Social Benefits
      functiondata$nox_ben  <- functiondata$System_Size*functiondata$nox_dam_eas # $/yr
      functiondata$so2_ben  <- functiondata$System_Size*functiondata$so2_dam_eas # $/yr
      functiondata$pm25_ben <- functiondata$System_Size*functiondata$pm25_dam_eas # $/yr
      functiondata$aq_ben   <- functiondata$System_Size*functiondata$pol_dam # $/yr
      functiondata$co2_ben  <- functiondata$System_Size*functiondata$co2_dam # $/yr
      functiondata$sb       <- functiondata$aq_ben + functiondata$co2_ben # $/yr
      
      # Calculate net school benefits for next 20 years
      # - upfront costs - 10 year inverter replacement - 20 years of maintenance + 20 years of benefits
      functiondata$school_net_ben.lmp.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07*(sensitivity[i]), n = 10, 120*functiondata$System_Size) - pv.annuity(r = .07*(sensitivity[i]), n = 20, functiondata$schoolb.lmp) + pv.annuity(r = .07*(sensitivity[i]), n = 20, 15*functiondata$System_Size)
      functiondata$school_net_ben.lmp.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20, functiondata$schoolb.lmp) + pv.annuity(r = .02, n = 20, 15*functiondata$System_Size)
      functiondata$school_net_ben.ret.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07*(sensitivity[i]), n = 10, 120*functiondata$System_Size) - pv.annuity(r = .07*(sensitivity[i]), n = 20, functiondata$schoolb.ret) + pv.annuity(r = .07*(sensitivity[i]), n = 20, 15*functiondata$System_Size)
      functiondata$school_net_ben.ret.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20, functiondata$schoolb.ret) + pv.annuity(r = .02, n = 20, 15*functiondata$System_Size)
      
      # Calculate net social benefits for next 20 years
      functiondata$social_net_ben.lmp.dsire.07     <- -functiondata$sc.dsire.lmp    - pv.annuity(r = .07*(sensitivity[i]), n = 20, functiondata$sb)
      functiondata$social_net_ben.lmp.dsire.02     <- -functiondata$sc.dsire.lmp    - pv.annuity(r = .02, n = 20, functiondata$sb)
      functiondata$social_net_ben.ret.dsire.07     <- -functiondata$sc.dsire.ret.07 - pv.annuity(r = .07*(sensitivity[i]), n = 20, functiondata$sb)
      functiondata$social_net_ben.ret.dsire.02     <- -functiondata$sc.dsire.ret.02 - pv.annuity(r = .02, n = 20, functiondata$sb)
      
      # Calculate annuity factors and look at annualized net benefits
      annuity.07 <- (1-(1+.07*(sensitivity[i]))^(-20))/.07*(sensitivity[i])
      annuity.02 <- (1-(1+.02)^(-20))/.02
      
      # Calculate annualized net-benefits
      # Schools
      functiondata$school_net_ben.lmp.dsirerebate.07.annual     <- functiondata$school_net_ben.lmp.dsirerebate.07/annuity.07
      functiondata$school_net_ben.lmp.dsirerebate.02.annual     <- functiondata$school_net_ben.lmp.dsirerebate.02/annuity.02
      functiondata$school_net_ben.ret.dsirerebate.07.annual     <- functiondata$school_net_ben.ret.dsirerebate.07/annuity.07
      functiondata$school_net_ben.ret.dsirerebate.02.annual     <- functiondata$school_net_ben.ret.dsirerebate.02/annuity.02
      
      # Social
      functiondata$social_net_ben.lmp.dsire.07.annual     <- functiondata$social_net_ben.lmp.dsire.07/annuity.07
      functiondata$social_net_ben.lmp.dsire.02.annual     <- functiondata$social_net_ben.lmp.dsire.02/annuity.02
      functiondata$social_net_ben.ret.dsire.07.annual     <- functiondata$social_net_ben.ret.dsire.07/annuity.07
      functiondata$social_net_ben.ret.dsire.02.annual     <- functiondata$social_net_ben.ret.dsire.02/annuity.02
      
      # Output net benefit
      # School net-benefit
      output[i, 1] <- median(functiondata$school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)
      
      # Social net-benefit
      output[i, 2] <- median(functiondata$social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)
      
    }
  }
  
  return(output)
  
}

discount.sens <- discount.sensitivity(data = AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, sensitivity = sensitivity) 


sensitivity <- seq(0.1, 5, by = 0.1)
sensitivity <- c(0.0001, sensitivity)

discount.sensitivity <- function(data, sensitivity) {
  
  functiondata <- data
  
  output <- matrix(NA, nrow=length(sensitivity), ncol=2)
  colnames(output) <- c("discount.school", "discount.social")
  
  for (i in 1:length(sensitivity)){
    
    # School costs and benefits
    functiondata$schoolc.dsire  <- 3836.97*functiondata$System_Size - functiondata$dsire.rebate
    functiondata$demand         <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*1000 # kWh/yr
    functiondata$cost.savings   <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*functiondata$price_ret*1000/100 # $/yr
    functiondata$elec.sales.lmp <- functiondata$System_Size*functiondata$net_power*functiondata$lmp # $/yr
    functiondata$elec.sales.ret <- functiondata$System_Size*functiondata$net_power*functiondata$price_ret*1000/100 # $/yr
    functiondata$schoolb.lmp    <- functiondata$cost.savings + functiondata$elec.sales.lmp  # $/yr
    functiondata$schoolb.ret    <- functiondata$cost.savings + functiondata$elec.sales.ret  # $/yr
    
    # Social Costs
    functiondata$sc.dsire.lmp     <- functiondata$dsire.rebate
    functiondata$sc.dsire.ret.07  <- functiondata$dsire.rebate        - pv.annuity(r = .07*(sensitivity[i]), n = 20, functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
    functiondata$sc.dsire.ret.02  <- functiondata$dsire.rebate        - pv.annuity(r = .02, n = 20, functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
    
    # Social Benefits
    functiondata$nox_ben  <- functiondata$System_Size*functiondata$nox_dam_eas # $/yr
    functiondata$so2_ben  <- functiondata$System_Size*functiondata$so2_dam_eas # $/yr
    functiondata$pm25_ben <- functiondata$System_Size*functiondata$pm25_dam_eas # $/yr
    functiondata$aq_ben   <- functiondata$System_Size*functiondata$pol_dam # $/yr
    functiondata$co2_ben  <- functiondata$System_Size*functiondata$co2_dam # $/yr
    functiondata$sb       <- functiondata$aq_ben + functiondata$co2_ben # $/yr
    
    # Calculate net school benefits for next 20 years
    # - upfront costs - 10 year inverter replacement - 20 years of maintenance + 20 years of benefits
    functiondata$school_net_ben.lmp.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07*(sensitivity[i]), n = 10, 120*functiondata$System_Size) - pv.annuity(r = .07*(sensitivity[i]), n = 20, functiondata$schoolb.lmp) + pv.annuity(r = .07*(sensitivity[i]), n = 20, 15*functiondata$System_Size)
    functiondata$school_net_ben.lmp.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20, functiondata$schoolb.lmp) + pv.annuity(r = .02, n = 20, 15*functiondata$System_Size)
    functiondata$school_net_ben.ret.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07*(sensitivity[i]), n = 10, 120*functiondata$System_Size) - pv.annuity(r = .07*(sensitivity[i]), n = 20, functiondata$schoolb.ret) + pv.annuity(r = .07*(sensitivity[i]), n = 20, 15*functiondata$System_Size)
    functiondata$school_net_ben.ret.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20, functiondata$schoolb.ret) + pv.annuity(r = .02, n = 20, 15*functiondata$System_Size)
    
    # Calculate net social benefits for next 20 years
    functiondata$social_net_ben.lmp.dsire.07     <- -functiondata$sc.dsire.lmp    - pv.annuity(r = .07*(sensitivity[i]), n = 20, functiondata$sb)
    functiondata$social_net_ben.lmp.dsire.02     <- -functiondata$sc.dsire.lmp    - pv.annuity(r = .02, n = 20, functiondata$sb)
    functiondata$social_net_ben.ret.dsire.07     <- -functiondata$sc.dsire.ret.07 - pv.annuity(r = .07*(sensitivity[i]), n = 20, functiondata$sb)
    functiondata$social_net_ben.ret.dsire.02     <- -functiondata$sc.dsire.ret.02 - pv.annuity(r = .02, n = 20, functiondata$sb)
    
    # Calculate annuity factors and look at annualized net benefits
    annuity.07 <- (1-(1+.07*(sensitivity[i]))^(-20))/.07*(sensitivity[i])
    annuity.02 <- (1-(1+.02)^(-20))/.02
    
    # Calculate annualized net-benefits
    # Schools
    functiondata$school_net_ben.lmp.dsirerebate.07.annual     <- functiondata$school_net_ben.lmp.dsirerebate.07/annuity.07
    functiondata$school_net_ben.lmp.dsirerebate.02.annual     <- functiondata$school_net_ben.lmp.dsirerebate.02/annuity.02
    functiondata$school_net_ben.ret.dsirerebate.07.annual     <- functiondata$school_net_ben.ret.dsirerebate.07/annuity.07
    functiondata$school_net_ben.ret.dsirerebate.02.annual     <- functiondata$school_net_ben.ret.dsirerebate.02/annuity.02
    
    # Social
    functiondata$social_net_ben.lmp.dsire.07.annual     <- functiondata$social_net_ben.lmp.dsire.07/annuity.07
    functiondata$social_net_ben.lmp.dsire.02.annual     <- functiondata$social_net_ben.lmp.dsire.02/annuity.02
    functiondata$social_net_ben.ret.dsire.07.annual     <- functiondata$social_net_ben.ret.dsire.07/annuity.07
    functiondata$social_net_ben.ret.dsire.02.annual     <- functiondata$social_net_ben.ret.dsire.02/annuity.02
    
    # Output net benefit
    # School net-benefit
    output[i, 1] <- median(functiondata$school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)
    
    # Social net-benefit
    output[i, 2] <- median(functiondata$social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)
    
  }
  
  return(output)
  
}

discount.sens <- discount.sensitivity(data = AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, sensitivity = sensitivity) 


# Available Rebates
# 0% to 500% of average LBNL state averages
# 10% increments
# First, look at LBNL State average rebates
count(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$Rebate...kW.)
summary(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$Rebate...kW.)
mean(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$Rebate...kW., na.rm = TRUE) # $784/kW

sensitivity <- seq(0, 5, by = 0.1)

rebate.sensitivity <- function(data, sensitivity) {
  
  functiondata <- data
  
  output <- matrix(NA, nrow=length(sensitivity), ncol=2)
  colnames(output) <- c("rebate.school", "rebate.social")
  
  for (i in 1:length(sensitivity)){
    
    # School costs and benefits
    functiondata$schoolc.dsire  <- 3836.97*functiondata$System_Size - 784*sensitivity[i]*functiondata$System_Size
    functiondata$demand         <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*1000 # kWh/yr
    functiondata$cost.savings   <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*functiondata$price_ret*1000/100 # $/yr
    functiondata$elec.sales.lmp <- functiondata$System_Size*functiondata$net_power*functiondata$lmp # $/yr
    functiondata$elec.sales.ret <- functiondata$System_Size*functiondata$net_power*functiondata$price_ret*1000/100 # $/yr
    functiondata$schoolb.lmp    <- functiondata$cost.savings + functiondata$elec.sales.lmp  # $/yr
    functiondata$schoolb.ret    <- functiondata$cost.savings + functiondata$elec.sales.ret  # $/yr
    
    # Social Costs
    functiondata$sc.dsire.lmp     <- 784*sensitivity[i]*functiondata$System_Size
    functiondata$sc.dsire.ret.07  <- 784*sensitivity[i]*functiondata$System_Size        - pv.annuity(r = .07, n = 20, functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
    functiondata$sc.dsire.ret.02  <- 784*sensitivity[i]*functiondata$System_Size        - pv.annuity(r = .02, n = 20, functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
    
    # Social Benefits
    functiondata$nox_ben  <- functiondata$System_Size*functiondata$nox_dam_eas # $/yr
    functiondata$so2_ben  <- functiondata$System_Size*functiondata$so2_dam_eas # $/yr
    functiondata$pm25_ben <- functiondata$System_Size*functiondata$pm25_dam_eas # $/yr
    functiondata$aq_ben   <- functiondata$System_Size*functiondata$pol_dam # $/yr
    functiondata$co2_ben  <- functiondata$System_Size*functiondata$co2_dam # $/yr
    functiondata$sb       <- functiondata$aq_ben + functiondata$co2_ben # $/yr
    
    # Calculate net school benefits for next 20 years
    # - upfront costs - 10 year inverter replacement - 20 years of maintenance + 20 years of benefits
    functiondata$school_net_ben.lmp.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .07, n = 20, functiondata$schoolb.lmp) + pv.annuity(r = .07, n = 20, 15*functiondata$System_Size)
    functiondata$school_net_ben.lmp.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20, functiondata$schoolb.lmp) + pv.annuity(r = .02, n = 20, 15*functiondata$System_Size)
    functiondata$school_net_ben.ret.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .07, n = 20, functiondata$schoolb.ret) + pv.annuity(r = .07, n = 20, 15*functiondata$System_Size)
    functiondata$school_net_ben.ret.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20, functiondata$schoolb.ret) + pv.annuity(r = .02, n = 20, 15*functiondata$System_Size)
    
    # Calculate net social benefits for next 20 years
    functiondata$social_net_ben.lmp.dsire.07     <- -functiondata$sc.dsire.lmp    - pv.annuity(r = .07, n = 20, functiondata$sb)
    functiondata$social_net_ben.lmp.dsire.02     <- -functiondata$sc.dsire.lmp    - pv.annuity(r = .02, n = 20, functiondata$sb)
    functiondata$social_net_ben.ret.dsire.07     <- -functiondata$sc.dsire.ret.07 - pv.annuity(r = .07, n = 20, functiondata$sb)
    functiondata$social_net_ben.ret.dsire.02     <- -functiondata$sc.dsire.ret.02 - pv.annuity(r = .02, n = 20, functiondata$sb)
    
    # Calculate annuity factors and look at annualized net benefits
    annuity.07 <- (1-(1+.07)^(-20))/.07
    annuity.02 <- (1-(1+.02)^(-20))/.02
    
    # Calculate annualized net-benefits
    # Schools
    functiondata$school_net_ben.lmp.dsirerebate.07.annual     <- functiondata$school_net_ben.lmp.dsirerebate.07/annuity.07
    functiondata$school_net_ben.lmp.dsirerebate.02.annual     <- functiondata$school_net_ben.lmp.dsirerebate.02/annuity.02
    functiondata$school_net_ben.ret.dsirerebate.07.annual     <- functiondata$school_net_ben.ret.dsirerebate.07/annuity.07
    functiondata$school_net_ben.ret.dsirerebate.02.annual     <- functiondata$school_net_ben.ret.dsirerebate.02/annuity.02
    
    # Social
    functiondata$social_net_ben.lmp.dsire.07.annual     <- functiondata$social_net_ben.lmp.dsire.07/annuity.07
    functiondata$social_net_ben.lmp.dsire.02.annual     <- functiondata$social_net_ben.lmp.dsire.02/annuity.02
    functiondata$social_net_ben.ret.dsire.07.annual     <- functiondata$social_net_ben.ret.dsire.07/annuity.07
    functiondata$social_net_ben.ret.dsire.02.annual     <- functiondata$social_net_ben.ret.dsire.02/annuity.02
    
    # Output net benefit
    # School net-benefit
    output[i, 1] <- median(functiondata$school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)
    
    # Social net-benefit
    output[i, 2] <- median(functiondata$social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)
    
  }
  
  return(output)
  
}

rebate.sens <- rebate.sensitivity(data = AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, sensitivity = sensitivity)


# System Size
# 0% to 500% of current system size
# 10% increments

sensitivity <- seq(0, 5, by = 0.1)

size.sensitivity <- function(data, sensitivity) {
  
  functiondata <- data
  
  output <- matrix(NA, nrow=length(sensitivity), ncol=2)
  colnames(output) <- c("size.school", "size.social")
  
  for (i in 1:length(sensitivity)){
    
    functiondata$dsire.rebate <- functiondata$System_Size*sensitivity[i] * functiondata$Rebate...kW.
    functiondata$dsire.rebate[is.na(functiondata$dsire.rebate)] <- 0
    
    # School costs and benefits
    functiondata$schoolc.dsire  <- 3836.97*functiondata$System_Size*sensitivity[i] - functiondata$dsire.rebate
    functiondata$demand         <- functiondata$System_Size*sensitivity[i]*(functiondata$power-functiondata$net_power)*1000 # kWh/yr
    functiondata$cost.savings   <- functiondata$System_Size*sensitivity[i]*(functiondata$power-functiondata$net_power)*functiondata$price_ret*1000/100 # $/yr
    functiondata$elec.sales.lmp <- functiondata$System_Size*sensitivity[i]*functiondata$net_power*functiondata$lmp # $/yr
    functiondata$elec.sales.ret <- functiondata$System_Size*sensitivity[i]*functiondata$net_power*functiondata$price_ret*1000/100 # $/yr
    functiondata$schoolb.lmp    <- functiondata$cost.savings + functiondata$elec.sales.lmp  # $/yr
    functiondata$schoolb.ret    <- functiondata$cost.savings + functiondata$elec.sales.ret  # $/yr
    
    # Social Costs
    functiondata$sc.dsire.lmp     <- functiondata$dsire.rebate
    functiondata$sc.dsire.ret.07  <- functiondata$dsire.rebate        - pv.annuity(r = .07, n = 20, functiondata$System_Size*sensitivity[i]*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
    functiondata$sc.dsire.ret.02  <- functiondata$dsire.rebate        - pv.annuity(r = .02, n = 20, functiondata$System_Size*sensitivity[i]*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
    
    # Social Benefits
    functiondata$nox_ben  <- functiondata$System_Size*sensitivity[i]*functiondata$nox_dam_eas # $/yr
    functiondata$so2_ben  <- functiondata$System_Size*sensitivity[i]*functiondata$so2_dam_eas # $/yr
    functiondata$pm25_ben <- functiondata$System_Size*sensitivity[i]*functiondata$pm25_dam_eas # $/yr
    functiondata$aq_ben   <- functiondata$System_Size*sensitivity[i]*functiondata$pol_dam # $/yr
    functiondata$co2_ben  <- functiondata$System_Size*sensitivity[i]*functiondata$co2_dam # $/yr
    functiondata$sb       <- functiondata$aq_ben + functiondata$co2_ben # $/yr
    
    # Calculate net school benefits for next 20 years
    # - upfront costs - 10 year inverter replacement - 20 years of maintenance + 20 years of benefits
    functiondata$school_net_ben.lmp.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size*sensitivity[i]) - pv.annuity(r = .07, n = 20, functiondata$schoolb.lmp) + pv.annuity(r = .07, n = 20, 15*functiondata$System_Size*sensitivity[i])
    functiondata$school_net_ben.lmp.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size*sensitivity[i]) - pv.annuity(r = .02, n = 20, functiondata$schoolb.lmp) + pv.annuity(r = .02, n = 20, 15*functiondata$System_Size*sensitivity[i])
    functiondata$school_net_ben.ret.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size*sensitivity[i]) - pv.annuity(r = .07, n = 20, functiondata$schoolb.ret) + pv.annuity(r = .07, n = 20, 15*functiondata$System_Size*sensitivity[i])
    functiondata$school_net_ben.ret.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size*sensitivity[i]) - pv.annuity(r = .02, n = 20, functiondata$schoolb.ret) + pv.annuity(r = .02, n = 20, 15*functiondata$System_Size*sensitivity[i])
    
    # Calculate net social benefits for next 20 years
    functiondata$social_net_ben.lmp.dsire.07     <- -functiondata$sc.dsire.lmp    - pv.annuity(r = .07, n = 20, functiondata$sb)
    functiondata$social_net_ben.lmp.dsire.02     <- -functiondata$sc.dsire.lmp    - pv.annuity(r = .02, n = 20, functiondata$sb)
    functiondata$social_net_ben.ret.dsire.07     <- -functiondata$sc.dsire.ret.07 - pv.annuity(r = .07, n = 20, functiondata$sb)
    functiondata$social_net_ben.ret.dsire.02     <- -functiondata$sc.dsire.ret.02 - pv.annuity(r = .02, n = 20, functiondata$sb)
    
    # Calculate annuity factors and look at annualized net benefits
    annuity.07 <- (1-(1+.07)^(-20))/.07
    annuity.02 <- (1-(1+.02)^(-20))/.02
    
    # Calculate annualized net-benefits
    # Schools
    functiondata$school_net_ben.lmp.dsirerebate.07.annual     <- functiondata$school_net_ben.lmp.dsirerebate.07/annuity.07
    functiondata$school_net_ben.lmp.dsirerebate.02.annual     <- functiondata$school_net_ben.lmp.dsirerebate.02/annuity.02
    functiondata$school_net_ben.ret.dsirerebate.07.annual     <- functiondata$school_net_ben.ret.dsirerebate.07/annuity.07
    functiondata$school_net_ben.ret.dsirerebate.02.annual     <- functiondata$school_net_ben.ret.dsirerebate.02/annuity.02
    
    # Social
    functiondata$social_net_ben.lmp.dsire.07.annual     <- functiondata$social_net_ben.lmp.dsire.07/annuity.07
    functiondata$social_net_ben.lmp.dsire.02.annual     <- functiondata$social_net_ben.lmp.dsire.02/annuity.02
    functiondata$social_net_ben.ret.dsire.07.annual     <- functiondata$social_net_ben.ret.dsire.07/annuity.07
    functiondata$social_net_ben.ret.dsire.02.annual     <- functiondata$social_net_ben.ret.dsire.02/annuity.02
    
    # Output net benefit
    # School net-benefit
    output[i, 1] <- median(functiondata$school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)
    
    # Social net-benefit
    output[i, 2] <- median(functiondata$social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)
    
  }
  
  return(output)
  
}

size.sens <- size.sensitivity(data = AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, sensitivity = sensitivity)


# Lifetime
# 0% to 500% of 20 year project lifetime
# 10% increments

sensitivity <- seq(0, 5, by = 0.1)

lifetime.sensitivity <- function(data, sensitivity) {
  
  functiondata <- data
  
  output <- matrix(NA, nrow=length(sensitivity), ncol=2)
  colnames(output) <- c("life.school", "life.social")
  
  for (i in 1:length(sensitivity)){
    
    # School costs and benefits
    functiondata$schoolc.dsire  <- 3836.97*functiondata$System_Size - functiondata$dsire.rebate
    functiondata$demand         <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*1000 # kWh/yr
    functiondata$cost.savings   <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*functiondata$price_ret*1000/100 # $/yr
    functiondata$elec.sales.lmp <- functiondata$System_Size*functiondata$net_power*functiondata$lmp # $/yr
    functiondata$elec.sales.ret <- functiondata$System_Size*functiondata$net_power*functiondata$price_ret*1000/100 # $/yr
    functiondata$schoolb.lmp    <- functiondata$cost.savings + functiondata$elec.sales.lmp  # $/yr
    functiondata$schoolb.ret    <- functiondata$cost.savings + functiondata$elec.sales.ret  # $/yr
    
    # Social Costs
    functiondata$sc.dsire.lmp     <- functiondata$dsire.rebate
    functiondata$sc.dsire.ret.07  <- functiondata$dsire.rebate        - pv.annuity(r = .07, n = 20*sensitivity[i], functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
    functiondata$sc.dsire.ret.02  <- functiondata$dsire.rebate        - pv.annuity(r = .02, n = 20*sensitivity[i], functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
    
    # Social Benefits
    functiondata$nox_ben  <- functiondata$System_Size*functiondata$nox_dam_eas # $/yr
    functiondata$so2_ben  <- functiondata$System_Size*functiondata$so2_dam_eas # $/yr
    functiondata$pm25_ben <- functiondata$System_Size*functiondata$pm25_dam_eas # $/yr
    functiondata$aq_ben   <- functiondata$System_Size*functiondata$pol_dam # $/yr
    functiondata$co2_ben  <- functiondata$System_Size*functiondata$co2_dam # $/yr
    functiondata$sb       <- functiondata$aq_ben + functiondata$co2_ben # $/yr
    
    # Calculate net school benefits for next 20 years
    # - upfront costs - 10 year inverter replacement - 20 years of maintenance + 20 years of benefits
    
    # only have inverter replacement at 20yr and 40yr project lifetimes
    
    if(i == 1 | i == 2){
      
      functiondata$school_net_ben.lmp.dsirerebate.07     <- -functiondata$schoolc.dsire  - pv.annuity(r = .07, n = 20*sensitivity[i], functiondata$schoolb.lmp) + pv.annuity(r = .07, n = 20*sensitivity[i], 15*functiondata$System_Size)
      functiondata$school_net_ben.lmp.dsirerebate.02     <- -functiondata$schoolc.dsire  - pv.annuity(r = .02, n = 20*sensitivity[i], functiondata$schoolb.lmp) + pv.annuity(r = .02, n = 20*sensitivity[i], 15*functiondata$System_Size)
      functiondata$school_net_ben.ret.dsirerebate.07     <- -functiondata$schoolc.dsire  - pv.annuity(r = .07, n = 20*sensitivity[i], functiondata$schoolb.ret) + pv.annuity(r = .07, n = 20*sensitivity[i], 15*functiondata$System_Size)
      functiondata$school_net_ben.ret.dsirerebate.02     <- -functiondata$schoolc.dsire  - pv.annuity(r = .02, n = 20*sensitivity[i], functiondata$schoolb.ret) + pv.annuity(r = .02, n = 20*sensitivity[i], 15*functiondata$System_Size)
      
    } else if (i == 3) {
      
      functiondata$school_net_ben.lmp.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .07, n = 20*sensitivity[i], functiondata$schoolb.lmp) + pv.annuity(r = .07, n = 20*sensitivity[i], 15*functiondata$System_Size)
      functiondata$school_net_ben.lmp.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20*sensitivity[i], functiondata$schoolb.lmp) + pv.annuity(r = .02, n = 20*sensitivity[i], 15*functiondata$System_Size)
      functiondata$school_net_ben.ret.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .07, n = 20*sensitivity[i], functiondata$schoolb.ret) + pv.annuity(r = .07, n = 20*sensitivity[i], 15*functiondata$System_Size)
      functiondata$school_net_ben.ret.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20*sensitivity[i], functiondata$schoolb.ret) + pv.annuity(r = .02, n = 20*sensitivity[i], 15*functiondata$System_Size)
      
    } else if (i == 4) {
      
      functiondata$school_net_ben.lmp.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size) + pv.simple(r = .07, n = 20, 120*functiondata$System_Size) - pv.annuity(r = .07, n = 20*sensitivity[i], functiondata$schoolb.lmp) + pv.annuity(r = .07, n = 20*sensitivity[i], 15*functiondata$System_Size)
      functiondata$school_net_ben.lmp.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) + pv.simple(r = .07, n = 20, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20*sensitivity[i], functiondata$schoolb.lmp) + pv.annuity(r = .02, n = 20*sensitivity[i], 15*functiondata$System_Size)
      functiondata$school_net_ben.ret.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size) + pv.simple(r = .07, n = 20, 120*functiondata$System_Size) - pv.annuity(r = .07, n = 20*sensitivity[i], functiondata$schoolb.ret) + pv.annuity(r = .07, n = 20*sensitivity[i], 15*functiondata$System_Size)
      functiondata$school_net_ben.ret.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) + pv.simple(r = .07, n = 20, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20*sensitivity[i], functiondata$schoolb.ret) + pv.annuity(r = .02, n = 20*sensitivity[i], 15*functiondata$System_Size)
      
    } else {
      
      functiondata$school_net_ben.lmp.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size) + pv.simple(r = .07, n = 20, 120*functiondata$System_Size) + pv.simple(r = .07, n = 30, 120*functiondata$System_Size) - pv.annuity(r = .07, n = 20*sensitivity[i], functiondata$schoolb.lmp) + pv.annuity(r = .07, n = 20*sensitivity[i], 15*functiondata$System_Size)
      functiondata$school_net_ben.lmp.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) + pv.simple(r = .07, n = 20, 120*functiondata$System_Size) + pv.simple(r = .07, n = 30, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20*sensitivity[i], functiondata$schoolb.lmp) + pv.annuity(r = .02, n = 20*sensitivity[i], 15*functiondata$System_Size)
      functiondata$school_net_ben.ret.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size) + pv.simple(r = .07, n = 20, 120*functiondata$System_Size) + pv.simple(r = .07, n = 30, 120*functiondata$System_Size) - pv.annuity(r = .07, n = 20*sensitivity[i], functiondata$schoolb.ret) + pv.annuity(r = .07, n = 20*sensitivity[i], 15*functiondata$System_Size)
      functiondata$school_net_ben.ret.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) + pv.simple(r = .07, n = 20, 120*functiondata$System_Size) + pv.simple(r = .07, n = 30, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20*sensitivity[i], functiondata$schoolb.ret) + pv.annuity(r = .02, n = 20*sensitivity[i], 15*functiondata$System_Size)
      
    }
    
    # Calculate net social benefits for next 20 years
    functiondata$social_net_ben.lmp.dsire.07     <- -functiondata$sc.dsire.lmp    - pv.annuity(r = .07, n = 20*sensitivity[i], functiondata$sb)
    functiondata$social_net_ben.lmp.dsire.02     <- -functiondata$sc.dsire.lmp    - pv.annuity(r = .02, n = 20*sensitivity[i], functiondata$sb)
    functiondata$social_net_ben.ret.dsire.07     <- -functiondata$sc.dsire.ret.07 - pv.annuity(r = .07, n = 20*sensitivity[i], functiondata$sb)
    functiondata$social_net_ben.ret.dsire.02     <- -functiondata$sc.dsire.ret.02 - pv.annuity(r = .02, n = 20*sensitivity[i], functiondata$sb)
    
    # Calculate annuity factors and look at annualized net benefits
    annuity.07 <- (1-(1+.07)^(-20*sensitivity[i]))/.07
    annuity.02 <- (1-(1+.02)^(-20*sensitivity[i]))/.02
    
    # Calculate annualized net-benefits
    # Schools
    functiondata$school_net_ben.lmp.dsirerebate.07.annual     <- functiondata$school_net_ben.lmp.dsirerebate.07/annuity.07
    functiondata$school_net_ben.lmp.dsirerebate.02.annual     <- functiondata$school_net_ben.lmp.dsirerebate.02/annuity.02
    functiondata$school_net_ben.ret.dsirerebate.07.annual     <- functiondata$school_net_ben.ret.dsirerebate.07/annuity.07
    functiondata$school_net_ben.ret.dsirerebate.02.annual     <- functiondata$school_net_ben.ret.dsirerebate.02/annuity.02
    
    # Social
    functiondata$social_net_ben.lmp.dsire.07.annual     <- functiondata$social_net_ben.lmp.dsire.07/annuity.07
    functiondata$social_net_ben.lmp.dsire.02.annual     <- functiondata$social_net_ben.lmp.dsire.02/annuity.02
    functiondata$social_net_ben.ret.dsire.07.annual     <- functiondata$social_net_ben.ret.dsire.07/annuity.07
    functiondata$social_net_ben.ret.dsire.02.annual     <- functiondata$social_net_ben.ret.dsire.02/annuity.02
    
    # Output net benefit
    # School net-benefit
    output[i, 1] <- median(functiondata$school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)
    
    # Social net-benefit
    output[i, 2] <- median(functiondata$social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)
    
  }
  
  return(output)
  
}

life.sens <- lifetime.sensitivity(data = AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, sensitivity = sensitivity)

# Figure 11
# Make the spider plot

# first, combine all of the sensitivity outputs
spider.data <- as.data.frame(cbind(project.sens, discount.sens, rebate.sens, size.sens, life.sens))
nrow(spider.data) # 51
ncol(spider.data) # 10
spider.data$percentage <- seq(0, 5, by = .1)

# need to make dataframe usable in ggplot
# first split the social from school
# social data
spider.social <- spider.data[,c(2,4,6,8,10,11)]
spider.school <- spider.data[,c(1,3,5,7,9,11)]

# For plotting purposes, turn discount.school and discount.social to NA
spider.school[1,2] <- NA
spider.social[1,2] <- NA

spider.school[2,2] <- NA
spider.social[2,2] <- NA

spider.school[1,5] <- NA
spider.social[1,5] <- NA

head(spider.social)
spider.social[is.infinite(spider.social$life.social),]$life.social <- 0
head(spider.social)

head(spider.school)
spider.school[is.infinite(spider.school$life.school),]$life.school <- 0
head(spider.school)

# now turn to long format
library(reshape2)
spider.social.long <- melt(spider.social, id.vars = "percentage")
spider.school.long <- melt(spider.school, id.vars = "percentage")


# Plot groups 

my.cols <- c("#a50026","#f46d43","#4d4d4d","#74add1","#313695")

ggplot(data=spider.school.long, aes(x=percentage, y=value, group=variable)) +
  geom_line(aes(colour = variable), size = 1) +
  geom_point(aes(colour = variable)) +
  scale_colour_manual(values = my.cols) + theme_bw() +
  theme(
    plot.background = element_blank(), panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black')) + expand_limits(y=c(-1200000,200)) + theme(legend.position="none")

# legend
ggplot(data=spider.school.long, aes(x=percentage, y=value, group=variable)) +
  geom_line(aes(colour = variable), size = 1) +
  geom_point(aes(colour = variable)) +
  scale_colour_manual(values = my.cols) + theme_bw() +
  theme(
    plot.background = element_blank(), panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black')) + expand_limits(y=c(-1200000,200)) +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  theme(legend.text=element_text(size=rel(1.1)))


ggplot(data=spider.social.long, aes(x=percentage, y=value, group=variable)) +
  geom_line(aes(colour = variable), size = 1) +
  geom_point(aes(colour = variable)) +
  scale_colour_manual(values = my.cols) + theme_bw() +
  theme(
    plot.background = element_blank(), panel.border = element_blank()) +
  theme(axis.line = element_line(color = 'black')) + expand_limits(y=c(-200000,600000)) + theme(legend.position="none")



# emissions levels
# -5% to 5% increase each year from baseline social benefit values
# 2.5% increments

sensitivity <- c(-0.05, -0.025, 0, 0.025, 0.05)

emissions.sensitivity <- function(data, sensitivity) {
  
  functiondata <- data
  
  output <- matrix(NA, nrow=length(sensitivity), ncol=4)
  
  for (i in 1:length(sensitivity)){
    
    # School costs and benefits
    functiondata$schoolc.dsire  <- 3836.97*functiondata$System_Size - functiondata$dsire.rebate
    functiondata$demand         <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*1000 # kWh/yr
    functiondata$cost.savings   <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*functiondata$price_ret*1000/100 # $/yr
    functiondata$elec.sales.lmp <- functiondata$System_Size*functiondata$net_power*functiondata$lmp # $/yr
    functiondata$elec.sales.ret <- functiondata$System_Size*functiondata$net_power*functiondata$price_ret*1000/100 # $/yr
    functiondata$schoolb.lmp    <- functiondata$cost.savings + functiondata$elec.sales.lmp  # $/yr
    functiondata$schoolb.ret    <- functiondata$cost.savings + functiondata$elec.sales.ret  # $/yr
    
    # Social Costs
    functiondata$sc.dsire.lmp     <- functiondata$dsire.rebate
    functiondata$sc.dsire.ret.07  <- functiondata$dsire.rebate        - pv.annuity(r = .07, n = 20, functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
    functiondata$sc.dsire.ret.02  <- functiondata$dsire.rebate        - pv.annuity(r = .02, n = 20, functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
    
    # Social Benefits
    functiondata$nox_ben  <- functiondata$System_Size*functiondata$nox_dam_eas # $/yr
    functiondata$so2_ben  <- functiondata$System_Size*functiondata$so2_dam_eas # $/yr
    functiondata$pm25_ben <- functiondata$System_Size*functiondata$pm25_dam_eas # $/yr
    functiondata$aq_ben   <- functiondata$System_Size*functiondata$pol_dam # $/yr
    functiondata$co2_ben  <- functiondata$System_Size*functiondata$co2_dam # $/yr
    functiondata$sb       <- functiondata$aq_ben + functiondata$co2_ben # $/yr
    
    # Calculate net school benefits for next 20 years
    # - upfront costs - 10 year inverter replacement - 20 years of maintenance + 20 years of benefits
    functiondata$school_net_ben.lmp.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .07, n = 20, functiondata$schoolb.lmp) + pv.annuity(r = .07, n = 20, 15*functiondata$System_Size)
    functiondata$school_net_ben.lmp.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20, functiondata$schoolb.lmp) + pv.annuity(r = .02, n = 20, 15*functiondata$System_Size)
    functiondata$school_net_ben.ret.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .07, n = 20, functiondata$schoolb.ret) + pv.annuity(r = .07, n = 20, 15*functiondata$System_Size)
    functiondata$school_net_ben.ret.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20, functiondata$schoolb.ret) + pv.annuity(r = .02, n = 20, 15*functiondata$System_Size)
    
    # Calculate net social benefits for next 20 years
    functiondata$social_net_ben.lmp.dsire.07     <- -functiondata$sc.dsire.lmp    + (functiondata$sb/(.07-sensitivity[i]))  * (1 - ((1+sensitivity[i])/(1+.07))^20)
    functiondata$social_net_ben.lmp.dsire.02     <- -functiondata$sc.dsire.lmp    + (functiondata$sb/(.07-sensitivity[i]))  * (1 - ((1+sensitivity[i])/(1+.07))^20)
    functiondata$social_net_ben.ret.dsire.07     <- -functiondata$sc.dsire.ret.07 + (functiondata$sb/(.07-sensitivity[i]))  * (1 - ((1+sensitivity[i])/(1+.07))^20)
    functiondata$social_net_ben.ret.dsire.02     <- -functiondata$sc.dsire.ret.02 + (functiondata$sb/(.07-sensitivity[i]))  * (1 - ((1+sensitivity[i])/(1+.07))^20)
    
    # Calculate annuity factors and look at annualized net benefits
    annuity.07 <- (1-(1+.07)^(-20))/.07
    annuity.02 <- (1-(1+.02)^(-20))/.02
    
    # Calculate annualized net-benefits
    # Schools
    functiondata$school_net_ben.lmp.dsirerebate.07.annual     <- functiondata$school_net_ben.lmp.dsirerebate.07/annuity.07
    functiondata$school_net_ben.lmp.dsirerebate.02.annual     <- functiondata$school_net_ben.lmp.dsirerebate.02/annuity.02
    functiondata$school_net_ben.ret.dsirerebate.07.annual     <- functiondata$school_net_ben.ret.dsirerebate.07/annuity.07
    functiondata$school_net_ben.ret.dsirerebate.02.annual     <- functiondata$school_net_ben.ret.dsirerebate.02/annuity.02
    
    # Social
    functiondata$social_net_ben.lmp.dsire.07.annual     <- functiondata$social_net_ben.lmp.dsire.07/annuity.07
    functiondata$social_net_ben.lmp.dsire.02.annual     <- functiondata$social_net_ben.lmp.dsire.02/annuity.02
    functiondata$social_net_ben.ret.dsire.07.annual     <- functiondata$social_net_ben.ret.dsire.07/annuity.07
    functiondata$social_net_ben.ret.dsire.02.annual     <- functiondata$social_net_ben.ret.dsire.02/annuity.02
    
    # Output net benefit
    
    # Social net-benefit
    output[i, 1] <- min(functiondata$social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)
    output[i, 2] <- max(functiondata$social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)
    output[i, 3] <- median(functiondata$social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)
    output[i, 4] <- mean(functiondata$social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)
  }
  
  return(output)
  
}

emissions.sensitivity(data = AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, sensitivity = sensitivity) 


# Demand Charge Savings
# Best case and worse case scenario for the demand rate and savings
# Demand charge 25th, 50th, and 75th quartiles: 0.15, 0.20, 0.41
# Demand savings 25th, 50th, and 75th quartiles: 0.06, 0.12, 0.18
# Best case: price_ret*(1-.15)+price_ret*.15*.18
# Worst case: price_ret*(1-.41)+price_ret*.41*.06

DC <- c(0.15, 0.20, 0.41)
DS <- c(0.18, 0.12, 0.06)

demand.sensitivity <- function(data, DC, DS) {
  
  functiondata <- data
  
  output <- matrix(NA, nrow=length(DC), ncol=4)
  
  # get prices_ret back to the original state values (remove demand adjustments we made)
  functiondata$price_ret <- functiondata$price_ret * (1/.84)
  
  for (i in 1:length(DC)){
    
    functiondata$price_ret <- functiondata$price_ret*(1-DC[i]) + functiondata$price_ret*DC[i]*DS[i]
    
    # School costs and benefits
    functiondata$schoolc.dsire  <- 3836.97*functiondata$System_Size - functiondata$dsire.rebate
    functiondata$demand         <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*1000 # kWh/yr
    functiondata$cost.savings   <- functiondata$System_Size*(functiondata$power-functiondata$net_power)*functiondata$price_ret*1000/100 # $/yr
    functiondata$elec.sales.lmp <- functiondata$System_Size*functiondata$net_power*functiondata$lmp # $/yr
    functiondata$elec.sales.ret <- functiondata$System_Size*functiondata$net_power*functiondata$price_ret*1000/100 # $/yr
    functiondata$schoolb.lmp    <- functiondata$cost.savings + functiondata$elec.sales.lmp  # $/yr
    functiondata$schoolb.ret    <- functiondata$cost.savings + functiondata$elec.sales.ret  # $/yr
    
    # Social Costs
    functiondata$sc.dsire.lmp     <- functiondata$dsire.rebate
    functiondata$sc.dsire.ret.07  <- functiondata$dsire.rebate        - pv.annuity(r = .07, n = 20, functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
    functiondata$sc.dsire.ret.02  <- functiondata$dsire.rebate        - pv.annuity(r = .02, n = 20, functiondata$System_Size*functiondata$net_power*(functiondata$price_ret*1000/100 - functiondata$lmp))
    
    # Social Benefits
    functiondata$nox_ben  <- functiondata$System_Size*functiondata$nox_dam_eas # $/yr
    functiondata$so2_ben  <- functiondata$System_Size*functiondata$so2_dam_eas # $/yr
    functiondata$pm25_ben <- functiondata$System_Size*functiondata$pm25_dam_eas # $/yr
    functiondata$aq_ben   <- functiondata$System_Size*functiondata$pol_dam # $/yr
    functiondata$co2_ben  <- functiondata$System_Size*functiondata$co2_dam # $/yr
    functiondata$sb       <- functiondata$aq_ben + functiondata$co2_ben # $/yr
    
    # Calculate net school benefits for next 20 years
    # - upfront costs - 10 year inverter replacement - 20 years of maintenance + 20 years of benefits
    functiondata$school_net_ben.lmp.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .07, n = 20, functiondata$schoolb.lmp) + pv.annuity(r = .07, n = 20, 15*functiondata$System_Size)
    functiondata$school_net_ben.lmp.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20, functiondata$schoolb.lmp) + pv.annuity(r = .02, n = 20, 15*functiondata$System_Size)
    functiondata$school_net_ben.ret.dsirerebate.07     <- -functiondata$schoolc.dsire + pv.simple(r = .07, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .07, n = 20, functiondata$schoolb.ret) + pv.annuity(r = .07, n = 20, 15*functiondata$System_Size)
    functiondata$school_net_ben.ret.dsirerebate.02     <- -functiondata$schoolc.dsire + pv.simple(r = .02, n = 10, 120*functiondata$System_Size) - pv.annuity(r = .02, n = 20, functiondata$schoolb.ret) + pv.annuity(r = .02, n = 20, 15*functiondata$System_Size)
    
    # Calculate net social benefits for next 20 years
    functiondata$social_net_ben.lmp.dsire.07     <- -functiondata$sc.dsire.lmp    - pv.annuity(r = .07, n = 20, functiondata$sb)
    functiondata$social_net_ben.lmp.dsire.02     <- -functiondata$sc.dsire.lmp    - pv.annuity(r = .02, n = 20, functiondata$sb)
    functiondata$social_net_ben.ret.dsire.07     <- -functiondata$sc.dsire.ret.07 - pv.annuity(r = .07, n = 20, functiondata$sb)
    functiondata$social_net_ben.ret.dsire.02     <- -functiondata$sc.dsire.ret.02 - pv.annuity(r = .02, n = 20, functiondata$sb)
    
    # Calculate annuity factors and look at annualized net benefits
    annuity.07 <- (1-(1+.07)^(-20))/.07
    annuity.02 <- (1-(1+.02)^(-20))/.02
    
    # Calculate annualized net-benefits
    # Schools
    functiondata$school_net_ben.lmp.dsirerebate.07.annual     <- functiondata$school_net_ben.lmp.dsirerebate.07/annuity.07
    functiondata$school_net_ben.lmp.dsirerebate.02.annual     <- functiondata$school_net_ben.lmp.dsirerebate.02/annuity.02
    functiondata$school_net_ben.ret.dsirerebate.07.annual     <- functiondata$school_net_ben.ret.dsirerebate.07/annuity.07
    functiondata$school_net_ben.ret.dsirerebate.02.annual     <- functiondata$school_net_ben.ret.dsirerebate.02/annuity.02
    
    # Social
    functiondata$social_net_ben.lmp.dsire.07.annual     <- functiondata$social_net_ben.lmp.dsire.07/annuity.07
    functiondata$social_net_ben.lmp.dsire.02.annual     <- functiondata$social_net_ben.lmp.dsire.02/annuity.02
    functiondata$social_net_ben.ret.dsire.07.annual     <- functiondata$social_net_ben.ret.dsire.07/annuity.07
    functiondata$social_net_ben.ret.dsire.02.annual     <- functiondata$social_net_ben.ret.dsire.02/annuity.02
    
    # Output net benefit
    # School net-benefit
    output[i, 1] <- min(functiondata$school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)
    output[i, 2] <- max(functiondata$school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)
    output[i, 3] <- median(functiondata$school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)
    output[i, 4] <- mean(functiondata$school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)
    
    
  }
  
  return(output)
  
}

demand.sensitivity(data = AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, DC = DC, DS = DS) 


# SI
####################################################################
## Summarize results for counties
####################################################################
county.data <- ddply(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, ~COUNTY, summarize, 
                     capacity          <- sum(System_Size, na.rm = TRUE), # kW
                     generation        <- sum(power*System_Size*1000, na.rm = TRUE), # kWh/yr
                     generation.perkW  <- sum(power*System_Size*1000, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), #kWh/kW-yr
                     generation.perkWh <- sum(power*System_Size*1000, na.rm = TRUE)/sum(demand, na.rm = TRUE), # kWh/yr / kWh/yr
                     
                     nox_ben    <- sum(nox_ben, na.rm = TRUE), # $/yr
                     so2_ben    <- sum(so2_ben, na.rm = TRUE), # $/yr
                     pm25_ben   <- sum(pm25_ben, na.rm = TRUE), # $/yr
                     co2_ben    <- sum(co2_ben, na.rm = TRUE), # $/yr
                     
                     school_net_ben.lmp.dsirerebate.07  <- sum(school_net_ben.lmp.dsirerebate.07, na.rm = TRUE), # NPV $
                     school_net_ben.lmp.dsirerebate.02  <- sum(school_net_ben.lmp.dsirerebate.02, na.rm = TRUE), # NPV $
                     school_net_ben.ret.dsirerebate.07  <- sum(school_net_ben.ret.dsirerebate.07, na.rm = TRUE), # NPV $
                     school_net_ben.ret.dsirerebate.02  <- sum(school_net_ben.ret.dsirerebate.02, na.rm = TRUE), # NPV $
                     
                     social_net_ben.lmp.dsire.07        <- sum(social_net_ben.lmp.dsire.07, na.rm = TRUE), # NPV $
                     social_net_ben.lmp.dsire.02        <- sum(social_net_ben.lmp.dsire.02, na.rm = TRUE), # NPV $
                     social_net_ben.ret.dsire.07        <- sum(social_net_ben.ret.dsire.07, na.rm = TRUE), # NPV $
                     social_net_ben.ret.dsire.02        <- sum(social_net_ben.ret.dsire.02, na.rm = TRUE), # NPV $
                     
                     school_net_ben.lmp.dsirerebate.07.annual  <- sum(school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE), # $/yr
                     school_net_ben.lmp.dsirerebate.02.annual  <- sum(school_net_ben.lmp.dsirerebate.02.annual, na.rm = TRUE), # $/yr
                     school_net_ben.ret.dsirerebate.07.annual  <- sum(school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE), # $/yr
                     school_net_ben.ret.dsirerebate.02.annual  <- sum(school_net_ben.lmp.dsirerebate.02.annual, na.rm = TRUE), # $/yr
                     
                     social_net_ben.lmp.dsire.07.annual        <- sum(social_net_ben.lmp.dsire.07.annual, na.rm = TRUE), # $/yr
                     social_net_ben.lmp.dsire.02.annual        <- sum(social_net_ben.lmp.dsire.02.annual, na.rm = TRUE), # $/yr
                     social_net_ben.ret.dsire.07.annual        <- sum(social_net_ben.lmp.dsire.07.annual, na.rm = TRUE), # $/yr
                     social_net_ben.ret.dsire.02.annual        <- sum(social_net_ben.lmp.dsire.02.annual, na.rm = TRUE), # $/yr
                     
                     school_net_ben.lmp.dsirerebate.07.annual.perkW  <- sum(school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                     school_net_ben.lmp.dsirerebate.02.annual.perkW  <- sum(school_net_ben.lmp.dsirerebate.02.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                     school_net_ben.ret.dsirerebate.07.annual.perkW  <- sum(school_net_ben.lmp.dsirerebate.07.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                     school_net_ben.ret.dsirerebate.02.annual.perkW  <- sum(school_net_ben.lmp.dsirerebate.02.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                     
                     social_net_ben.lmp.dsire.07.annual.perkW        <- sum(social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                     social_net_ben.lmp.dsire.02.annual.perkW        <- sum(social_net_ben.lmp.dsire.02.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                     social_net_ben.ret.dsire.07.annual.perkW        <- sum(social_net_ben.lmp.dsire.07.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE), # $/kW-yr
                     social_net_ben.ret.dsire.02.annual.perkW        <- sum(social_net_ben.lmp.dsire.02.annual, na.rm = TRUE)/sum(System_Size, na.rm = TRUE)) # $/kW-yr 


names(county.data)[2] <- "capacity"
names(county.data)[3] <- "generation"
names(county.data)[4] <- "generation.perkW"
names(county.data)[5] <- "generation.perkWh"

names(county.data)[6] <- "nox_ben"
names(county.data)[7] <- "so2_ben"
names(county.data)[8] <- "pm25_ben"
names(county.data)[9] <- "co2_ben"

names(county.data)[10]  <- "school_net_ben.lmp.dsirerebate.07"
names(county.data)[11] <- "school_net_ben.lmp.dsirerebate.02"
names(county.data)[12] <- "school_net_ben.ret.dsirerebate.07"
names(county.data)[13] <- "school_net_ben.ret.dsirerebate.02"

names(county.data)[14] <- "social_net_ben.lmp.dsire.07"
names(county.data)[15] <- "social_net_ben.lmp.dsire.02"
names(county.data)[16] <- "social_net_ben.ret.dsire.07"
names(county.data)[17] <- "social_net_ben.ret.dsire.02"

names(county.data)[18] <- "school_net_ben.lmp.dsirerebate.07.annual"
names(county.data)[19] <- "school_net_ben.lmp.dsirerebate.02.annual"
names(county.data)[20] <- "school_net_ben.ret.dsirerebate.07.annual"
names(county.data)[21] <- "school_net_ben.ret.dsirerebate.02.annual"

names(county.data)[22] <- "social_net_ben.lmp.dsire.07.annual"
names(county.data)[23] <- "social_net_ben.lmp.dsire.02.annual"
names(county.data)[24] <- "social_net_ben.ret.dsire.07.annual"
names(county.data)[25] <- "social_net_ben.ret.dsire.02.annual"

names(county.data)[26] <- "school_net_ben.lmp.dsirerebate.07.annual.perkW"
names(county.data)[27] <- "school_net_ben.lmp.dsirerebate.02.annual.perkW"
names(county.data)[28] <- "school_net_ben.ret.dsirerebate.07.annual.perkW"
names(county.data)[29] <- "school_net_ben.ret.dsirerebate.02.annual.perkW"

names(county.data)[30] <- "social_net_ben.lmp.dsire.07.annual.perkW"
names(county.data)[31] <- "social_net_ben.lmp.dsire.02.annual.perkW"
names(county.data)[32] <- "social_net_ben.ret.dsire.07.annual.perkW"
names(county.data)[33] <- "social_net_ben.ret.dsire.02.annual.perkW"


# loads county map using a slightly better projection for the US
all_states <- map_data("state", projection  = "albers", par = c(30,0))
all_counties <- map_data("county", projection  = "albers", par = c(30,0))

all_counties$polyname <- paste(all_counties$region, all_counties$subregion, sep = ",")
all_counties <- merge(all_counties, county.fips, by="polyname", all.x=T)
names(all_counties)[8] <- "COUNTY"

# Merge the plotting data with the all_counties
county.plot <- merge(all_counties, county.data, by="COUNTY", all.x = TRUE)
nrow(county.plot)

# sort
county.plot <- county.plot[order(county.plot$group, county.plot$order),]



# Map generation

# Essentially treat all missing value as 0...
nrow(county.plot[is.na(county.plot$generation),]) # 1719
nrow(county.plot[which(county.plot$generation == 0),]) # 892
county.plot[is.na(county.plot$generation),]$generation <- 0
nrow(county.plot[is.na(county.plot$generation),]) # 0
nrow(county.plot[which(county.plot$generation == 0),]) # 2611

ggplot() + geom_polygon(data=county.plot, aes(x=long, y=lat, group = group, fill=generation/(10^6)), colour="black", size=0.1) + 
  scale_fill_continuous(low = "#d0d1e6", high = "#045a8d", guide="colorbar") + theme_bw()  + 
  labs(fill = "Total Annual Generation (Mwh/yr)",x = "", y = "") +
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) + 
  geom_path(data = all_states, aes(x=long, y=lat, group = group), colour = "black", size = 1)   # add state borders back in


# Try manually
hist(county.plot$generation/(10^6), breaks = "FD")
summary(county.plot$generation/(10^6))

# Make 6 breaks
my.cols <- c("#fff7fb", "#ece7f2", "#d0d1e6", "#a6bddb","#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858", "#012b44", "#001928")

county.plot$brks <- cut(county.plot$generation/(10^6), 
                        breaks=c(0, 50, 100, 150, 200, 250,  300, 350, 400, 450, 500, 2200), 
                        labels=c("0 - 50", "50 - 100", "100 - 150", "150 - 200", "200 - 250", "250 - 300",
                                 "300 - 250", "350 - 400", "400 - 450", "450 - 500", "> 500"))

ggplot() + geom_polygon(data=county.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Annual Generation (MWh/yr)\n", values = my.cols, guide="legend") + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) +  
  geom_path(data = all_states, aes(x=long, y=lat, group = group), colour = "black", size = 1)   # add state borders back in




# Map generation per kW
# Essentially treat all missing value as 0...
nrow(county.plot[is.na(county.plot$generation.perkW),]) # 2219
nrow(county.plot[which(county.plot$generation.perkW == 0),]) # 392
county.plot[is.na(county.plot$generation.perkW),]$generation.perkW <- 0
nrow(county.plot[is.na(county.plot$generation.perkW),]) # 0
nrow(county.plot[which(county.plot$generation.perkW == 0),]) # 2611


ggplot() + geom_polygon(data=county.plot, aes(x=long, y=lat, group = group, fill=generation.perkW), colour="black", size=0.1) + 
  scale_fill_continuous(low = "#ffffcc", high = "#225ea8", guide="colorbar") + theme_bw()  + 
  labs(fill = "Total Annual Generation per kW (Twh/kW-yr)",x = "", y = "") +
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) + 
  geom_path(data = all_states, aes(x=long, y=lat, group = group), colour = "black", size = 1)   # add state borders back in


# Map annnual school benefits
ggplot() + geom_polygon(data=county.plot, aes(x=long, y=lat, group = group, fill=school_net_ben.lmp.dsirerebate.07/(10^9)), colour="black", size=0.1) + 
  scale_fill_continuous(low = "#ffffcc", high = "#225ea8", guide="colorbar") + theme_bw()  + 
  labs(fill = "Total Annual School Benefits ($B/yr)",x = "", y = "") +
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) + 
  geom_path(data = all_states, aes(x=long, y=lat, group = group), colour = "black", size = 1)   # add state borders back in

# Map annnual social benefits
ggplot() + geom_polygon(data=county.plot, aes(x=long, y=lat, group = group, fill=social_net_ben.lmp.dsire.07/(10^9)), colour="black", size=0.1) + 
  scale_fill_continuous(low = "#d0d1e6", high = "#045a8d", guide="colorbar") + theme_bw()  + 
  labs(fill = "Total Annual Social Benefits ($B/yr)",x = "", y = "") +
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) + 
  geom_path(data = all_states, aes(x=long, y=lat, group = group), colour = "black", size = 1)   # add state borders back in



# Save CBA using the schools with LIDAR and regression predictions
write.table(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, file = "C:/Users/hanusnil/Box Sync/CBA Outputs/AllSchools.FINAL.reduced.area.LIDAR.merge.mar.eas.dsire.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")
write.table(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas, file = "C:/Users/hanusnil/Box Sync/CBA Outputs/AllSchools.FINAL.reduced.area.LIDAR.merge.avg.eas.dsire.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")
write.table(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2, file = "C:/Users/hanusnil/Box Sync/CBA Outputs/AllSchools.FINAL.reduced.area.LIDAR.merge.mar.ap2.dsire.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")
write.table(AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2, file = "C:/Users/hanusnil/Box Sync/CBA Outputs/AllSchools.FINAL.reduced.area.LIDAR.merge.avg.ap2.dsire.csv", sep = ",", quote = FALSE, append = FALSE, row.names = FALSE, na="")

























############################
# Make plots of net benefits
############################

# Read in all data for plotting
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas <- read.csv(file = "C:/Users/hanusnil/Box Sync/CBA Outputs/AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas.dsire.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas <- read.csv(file = "C:/Users/hanusnil/Box Sync/CBA Outputs/AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.eas.dsire.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2 <- read.csv(file = "C:/Users/hanusnil/Box Sync/CBA Outputs/AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.ap2.dsire.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2 <- read.csv(file = "C:/Users/hanusnil/Box Sync/CBA Outputs/AllSchools.LIDAR.reduced.area.LIDAR.merge.avg.ap2.dsire.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)



# Plotting net benefits as CDFs
## EASIUR
# Marginal

# LMP
plot_frame <- data.frame(net_ben = numeric(), price = factor(), pvt_soc = factor(), discount = factor())

# based on mixed, using a 7% discount rate
pf_temp <- data.frame(net_ben = numeric(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      price = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)), 
                      pvt_soc = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      discount = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)))

pf_temp$net_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.07
pf_temp$price <- "Net generation valued at LMP"
pf_temp$pvt_soc <- "Social"
pf_temp$discount <- "7% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)

pf_temp <- data.frame(net_ben = numeric(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      price = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)), 
                      pvt_soc = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      discount = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)))

pf_temp$net_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07
pf_temp$price <- "Net generation valued at LMP"
pf_temp$pvt_soc <- "School"
pf_temp$discount <- "7% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)


pf_temp <- data.frame(net_ben = numeric(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      price = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)), 
                      pvt_soc = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      discount = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)))

pf_temp$net_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.02
pf_temp$price <- "Net generation valued at LMP"
pf_temp$pvt_soc <- "Social"
pf_temp$discount <- "2% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)

pf_temp <- data.frame(net_ben = numeric(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      price = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)), 
                      pvt_soc = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      discount = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)))

pf_temp$net_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.02
pf_temp$price <- "Net generation valued at LMP"
pf_temp$pvt_soc <- "School"
pf_temp$discount <- "2% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)


# RETAIL
pf_temp <- data.frame(net_ben = numeric(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      price = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)), 
                      pvt_soc = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      discount = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)))

pf_temp$net_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.07
pf_temp$price <- "Net generation valued at retail"
pf_temp$pvt_soc <- "Social"
pf_temp$discount <- "7% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)

pf_temp <- data.frame(net_ben = numeric(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      price = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)), 
                      pvt_soc = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      discount = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)))

pf_temp$net_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.07
pf_temp$price <- "Net generation valued at retail"
pf_temp$pvt_soc <- "School"
pf_temp$discount <- "7% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)


pf_temp <- data.frame(net_ben = numeric(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      price = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)), 
                      pvt_soc = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      discount = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)))

pf_temp$net_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.ret.02
pf_temp$price <- "Net generation valued at retail"
pf_temp$pvt_soc <- "Social"
pf_temp$discount <- "2% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)

pf_temp <- data.frame(net_ben = numeric(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      price = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)), 
                      pvt_soc = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      discount = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)))

pf_temp$net_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.ret.02
pf_temp$price <- "Net generation valued at retail"
pf_temp$pvt_soc <- "School"
pf_temp$discount <- "2% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)



ggplot(data = plot_frame, aes(net_ben, colour = pvt_soc)) + stat_ecdf(size=1.5) + 
  scale_color_manual(values=c("darkorange", "dodgerblue4")) +
  labs(x="Net Benefits ($)", y="Frequency", colour = "Perspective") +
  xlim(-20000000, 20000000) + facet_grid(price~discount) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(axis.text=element_text(size = 12))


#########
# Plot the CDFs for selling back at LMP, 2% and 7% discount rate
# Considering te DSIRE rebates

## EASIUR
# Marginal

# LMP
plot_frame <- data.frame(net_ben = numeric(), price = factor(), pvt_soc = factor(), discount = factor())

# based on mixed, using a 7% discount rate
pf_temp <- data.frame(net_ben = numeric(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      price = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)), 
                      pvt_soc = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      discount = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)))

pf_temp$net_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.07
pf_temp$price <- "Net generation valued at LMP"
pf_temp$pvt_soc <- "Social"
pf_temp$discount <- "7% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)

pf_temp <- data.frame(net_ben = numeric(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      price = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)), 
                      pvt_soc = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      discount = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)))

pf_temp$net_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.07
pf_temp$price <- "Net generation valued at LMP"
pf_temp$pvt_soc <- "School"
pf_temp$discount <- "7% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)


pf_temp <- data.frame(net_ben = numeric(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      price = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)), 
                      pvt_soc = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      discount = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)))

pf_temp$net_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$social_net_ben.lmp.dsire.02
pf_temp$price <- "Net generation valued at LMP"
pf_temp$pvt_soc <- "Social"
pf_temp$discount <- "2% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)

pf_temp <- data.frame(net_ben = numeric(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      price = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)), 
                      pvt_soc = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)),
                      discount = factor(length(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.07)))

pf_temp$net_ben <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$school_net_ben.lmp.dsirerebate.02
pf_temp$price <- "Net generation valued at LMP"
pf_temp$pvt_soc <- "School"
pf_temp$discount <- "2% discount rate"

plot_frame <- rbind(pf_temp, plot_frame)

ggplot(data = plot_frame, aes(net_ben, colour = pvt_soc)) + stat_ecdf(size=1.5) + 
  scale_color_manual(values=c("darkorange", "dodgerblue4")) +
  labs(x="Net Benefits ($)", y="Frequency", colour = "Perspective") +
  xlim(-20000000, 20000000) + facet_grid(price~discount) + 
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  theme(axis.text=element_text(size = 12))


#### Chloropleth of counties and the school net benefits ####

# expand for usable area, output, etc.

## EASIUR
# Marginal
# use AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas
# Consider social net benefits, discount rate of 7%, selling back at LMP: social_net_ben.lmp.dsire.07

library(maps)
library(mapproj)
library(ggplot2)

# loads county map using a slightly better projection for the US
all_states <- map_data("state", projection  = "albers", par = c(30,0))
all_counties <- map_data("county", projection  = "albers", par = c(30,0))

all_counties$polyname <- paste(all_counties$region, all_counties$subregion, sep = ",")
all_counties <- merge(all_counties, county.fips, by="polyname", all.x=T)
names(all_counties)[8] <- "COUNTY"

# Need to read in total space and generation data
county.data <- ddply(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, ~COUNTY, summarize, 
                     Total.space <- sum(Total.Space), # in square feet
                     power <- sum(power * System_Size *1000, na.rm = TRUE)/sum(System_Size, na.rm = TRUE)) #in kWh
names(county.data)[2] <- "Total.space"
names(county.data)[3] <- "power"



# Merge the plotting data with the all_counties
county.plot <- merge(all_counties, county.data, by="COUNTY")

# sort
county.plot <- county.plot[order(county.plot$group, county.plot$order),]

library(RColorBrewer)
my.cols <- brewer.pal(5, "Blues")
my.cols2 <- c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", "#253494")

# Plot the available space
county.plot$brks <- cut(county.plot$Total.space, 
                        breaks=c(0, 500000, 1000000, 1500000, 2000000, 72000000), 
                        labels=c("0 - 500,000", "500,000 - 1,000,000", "1,000,000 - 1,500,000", 
                                 "1,500,000 - 2,000,000", "2,000,000 - 72,000,000"))
# don't use this one
# map
ggplot() + geom_polygon(data=county.plot, aes(x=long, y=lat, group = group, fill=Total.space), colour="black", size=0.1) + 
  scale_fill_continuous(low = "cadetblue1", high = "cadetblue4", guide="colorbar", label = comma) + theme_bw()  + 
  labs(fill = "Total Space", title = "Total Space", x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) + 
  geom_path(data = all_states, aes(x=long, y=lat, group = group), colour = "black")   # add state borders back in

# Use this one
# map
ggplot() + geom_polygon(data=county.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black", size=0.1) + 
  scale_fill_manual("Total Space (SF)\n", values = my.cols2, guide="legend") + theme_bw()  + 
  labs(x = "", y = "") +
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) + 
  geom_path(data = all_states, aes(x=long, y=lat, group = group), colour = "black", size = 1)   # add state borders back in

# Find counties with NA
new_DF <- county.plot[which(is.na(county.plot$power)),]


# Plot total output
# Plot the available space
county.plot$brks <- cut(county.plot$power, 
                        breaks=c(0, 500, 1000, 1500, 2000, 2500), 
                        labels=c("0 - 500", "500 - 1,000", "1,000 - 1,500", 
                                 "1,500 - 2,000", "2,000 - 2,500"))

ggplot() + geom_polygon(data=county.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black", size=0.1) + 
  scale_fill_manual("Total Generation (kWh)\n", values = my.cols2, guide="legend") + theme_bw()  + 
  labs(x = "", y = "") +
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank()) + 
  geom_path(data = all_states, aes(x=long, y=lat, group = group), colour = "black", size = 1)   # add state borders back in


#### Chloropleth of states and the school net benefits ####

## EASIUR
# Marginal
# use AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas
# Consider school net benefits, discount rate of 7%, selling back at LMP: school_net_ben.lmp.07

# load map of states dataframe
all_states <- map_data("state")


# Need to sum net benefits for the counties
plot.data <- ddply(AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas, ~State, summarize, 
                   Total.space <- sum(Total.Space, na.rm = TRUE), # in square feet
                   power <- sum(power * System_Size *1000, na.rm = TRUE)/sum(System_Size, na.rm = TRUE)) #in kwh


names(plot.data)[2] <- "Total.space"
names(plot.data)[3] <- "power"


# Merge state names for the plotting data
states.abbrev <- read.csv(file = "C:/Users/hanusnil/Box Sync/states.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
names(states.abbrev)[1] <- "region"
names(states.abbrev)[2] <- "State"
plot.data <- merge(plot.data, states.abbrev, by="State")

# merge with actual data to plot
map.plot <- merge(all_states, plot.data, by="region")

# reorder so lines don't get messed up
map.plot  <- map.plot[order(map.plot$order),]

my.cols2 <- c("#ffffcc", "#c7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494")


# Plot the available space
map.plot$brks <- cut(map.plot$Total.space, 
                     breaks=c(0, 100000000, 200000000, 300000000, 400000000, 500000000, 600000000), 
                     labels=c("0 - 100,000,000", "100,000,000 - 200,000,000", "200,000,000 - 300,000,000", 
                              "300,000,000 - 400,000,000", "400,000,000 - 500,000,000", "500,000,000 - 600,000,000"))


# map of School Net Benefits - using lmp and dsire rebates
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=brks), colour="black") + 
  scale_fill_manual("Total Space (SF)\n", values = my.cols2, guide="legend") + theme_bw()  + 
  labs(x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  


# Plot total generation
hist(map.plot$power)
summary(map.plot$power)




# map of Social Net Benefits - when schools sell back at lmp and dsire
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=social_net_ben.lmp.07), colour="black") + 
  scale_fill_continuous(low = "cadetblue1", high = "cadetblue4", guide="colorbar", label = comma) + theme_bw()  + 
  labs(fill = "Social\nNet Benefits", title = "Social Net Benefits in Millions of Dollars", x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  


# map of total usable area
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=Total.space), colour="black") + 
  scale_fill_continuous(low = "cadetblue1", high = "cadetblue4", guide="colorbar", label = comma) + theme_bw()  + 
  labs(fill = "Total\nArea (SF)", title = "Total usable area", x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  

# map of total anual energy output
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=power), colour="black") + 
  scale_fill_continuous(low = "cadetblue1", high = "cadetblue4", guide="colorbar", label = comma) + theme_bw()  + 
  labs(fill = "Total\nEnergy (kWh)", title = "Total PV annual energy output", x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  


# map of total rebates distributed by states
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=dsire.rebate), colour="black") + 
  scale_fill_continuous(low = "cadetblue1", high = "cadetblue4", guide="colorbar", label = comma) + theme_bw()  + 
  labs(fill = "Total\nRebates", title = "Total Rebates in Millions of Dollars", x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  



# Plot if a state offers a rebate or not
plot.data <- rebates
plot.data$policy <- "No Rebate Offerred"
plot.data[which(plot.data$Rebate...kW. >0),"policy"] <- "$/kW Rebate Offerred"


# Merge state names for the plotting data
states.abbrev <- read.csv(file = "C:/Users/hanusnil/Box Sync/states.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
names(states.abbrev)[1] <- "region"
names(states.abbrev)[2] <- "State"
plot.data <- merge(plot.data, states.abbrev, by="State")

# merge with actual data to plot
map.plot <- merge(all_states, plot.data, by="region")

# reorder so lines don't get messed up
map.plot  <- map.plot[order(map.plot$order),]


# map of School Net Benefits - using lmp and dsire rebates
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=factor(policy)), colour="black") + 
  scale_fill_manual(values = c("lightblue2","steelblue4")) + 
  theme_bw()  + 
  labs(fill = "Rebate for Schools", title = "PV Rebates", x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  



# Plot if a state offers net-metering, and at what rate
all_states <- map_data("state")

plot.data <- net.metering

plot.data$policy <- "No net metering"
plot.data[which(plot.data$NM.Compensation.Rate == "LMP" & plot.data$NM.For.Schools == "1"),"policy"] <- "LMP - schools explicit"
plot.data[which(plot.data$NM.Compensation.Rate == "LMP" & plot.data$NM.For.Schools == "0"),"policy"] <- "LMP - schools not explicit"
plot.data[which(plot.data$NM.Compensation.Rate == "Retail" & plot.data$NM.For.Schools == "1"),"policy"] <- "Retail - schools explicit"
plot.data[which(plot.data$NM.Compensation.Rate == "Retail" & plot.data$NM.For.Schools == "0"),"policy"] <- "Retail - schools not explicit"

# Merge state names for the plotting data
states.abbrev <- read.csv(file = "C:/Users/hanusnil/Box Sync/states.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
names(states.abbrev)[1] <- "region"
names(states.abbrev)[2] <- "State"
plot.data <- merge(plot.data, states.abbrev, by="State")

# merge with actual data to plot
map.plot <- merge(all_states, plot.data, by="region")

# reorder so lines don't get messed up
map.plot  <- map.plot[order(map.plot$order),]


# map of School Net Benefits - using lmp and dsire rebates
ggplot() + geom_polygon(data=map.plot, aes(x=long, y=lat, group = group, fill=factor(policy)), colour="black") + 
  scale_fill_manual(values = c("cadetblue1","cadetblue4", "gray48", "firebrick1", "firebrick4")) + 
  theme_bw()  + 
  labs(fill = "Excess Generation Value", title = "Net Metering Policies", x="", y="") + 
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())  





# DO NOT USE

#### MAP STATES ####
#read the shapes of the states
states <- readOGR(dsn = "C:/Users/hanusnil/Box Sync/states_21basic", layer = "states")

# get states into dataframe
states@data$id <- rownames(states@data)
states.df <- fortify(states, region="id")
statesDF <- merge(states.df, states@data, by = "id")
centroids <- gCentroid(states, byid=TRUE)
n_states <- length(states@data$STATE_ABBR)
centers <- data.frame(long = numeric(n_states), lat = numeric(n_states), STATE_ABBR = character(n_states))
centers$STATE_ABBR <- states@data$STATE_ABBR
centers$long <- coordinates(centroids)[,1]
centers$lat <- coordinates(centroids)[,2]

# need to make annualized benefits for school and society
# Use a discount rate of 7%
# Use retail rates for net generation (school benefits)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb.20.07 <- -pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$sb)
AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.20.07 <- -pv.annuity(r = disc.07, n = life, AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas$schoolb.lmp)

data <- AllSchools.LIDAR.reduced.area.LIDAR.merge.mar.eas[c("System_Size","schoolc", "sc.lmp", "schoolb.20.07","sb.20.07","State", "power")]
nrow(data) #36,219 cases
# drop rows with na
data <- na.omit(data) #35,457 cases


data <- ddply(data, ~State, summarize, 
              System_Size <- sum(System_Size), #in kW
              schoolc = sum(schoolc) / 1e6, #in millions of dollars
              sc.lmp = sum(sc.lmp) / 1e6, #in millions of dollars
              schoolb.20.07 = sum(schoolb.20.07) / 1e6, #in millions of dollars
              sb.20.07 = sum(sb.20.07) / 1e6, #in millions of dollars
              power = sum(power * System_Size *1000)/sum(System_Size)) #in kWh

names(data)[names(data) == "..1"] <- "System_Size"

statesDF <- merge(statesDF, data, by.x = "STATE_ABBR", by.y = "State", match = "all")

data <- merge(data, centers, by.x = "State", by.y = "STATE_ABBR", match = "all")
statesDF <- statesDF[order(statesDF$order),]

# Remove Alaska
data <- data[!data$State == "AK",]
statesDF <- statesDF[!statesDF$STATE_ABBR == "AK",]

lonadj <- 0.3
latadj <- 0.1
scale <- 0.001
size <- 3

# Only keep a few states in the data
keep <- c("IN", "CA", "FL", "ME", "VT", "TX", "CO")
data <- data[data$State %in% keep, ]

m <- ggplot(data=statesDF, aes(x=long, y=lat)) + geom_path(color = "gray48", aes(group = group)) 
m
# total cost benefit
m + geom_errorbar(data = data, size = size, color = "darkorange4", width = 0, aes(x=long - lonadj, ymin=lat - latadj, 
                                                                                  ymax = lat - latadj + data$sc.lmp*scale)) +
  geom_errorbar(data = data, size = size, color = "darkorange", width = 0, aes(x=long - lonadj, ymin=lat - latadj + data$sc.lmp*scale, 
                                                                               ymax = lat - latadj + (data$sc.lmp+data$schoolc)*scale)) +
  geom_errorbar(data = data, size = size, color = "dodgerblue4", width = 0, aes(x=long + lonadj, ymin=lat - latadj, 
                                                                                ymax = lat - latadj + data$sb.20.07*scale)) + 
  geom_errorbar(data = data, size = size, color = "dodgerblue2", width = 0, aes(x=long + lonadj, ymin=lat - latadj + data$sb.20.07*scale, 
                                                                                ymax = lat - latadj + (data$sb.20.07 + data$schoolb.20.07)*scale)) + 
  
  theme(panel.background = element_rect(fill="white")) + 
  theme(axis.ticks = element_blank(), axis.text.y = element_blank(), axis.text.x = element_blank())+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())










