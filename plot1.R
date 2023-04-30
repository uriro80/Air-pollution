# fips: A five-digit number (represented as a string) indicating the U.S. county
# 
# SCC
# SCC: The name of the source as indicated by a digit string (see source code classification table)
# 
# Pollutant
# Pollutant: A string indicating the pollutant
# 
# Emissions
# Emissions: Amount of PM2.5 emitted, in tons
# 
# type
# type: The type of source (point, non-point, on-road, or non-road)
# 
# year
# year: The year of emissions recorded

# set working directory
setwd("C:/Users/uri_r/OneDrive/02-uri/01-PostDoc/101-DataScienceCourse/03-Exploratory Data Analysis/W3/02-Assignments/Air-pollution")

#load relevant library
library(dplyr)

## read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#check for missing data
mean(is.na(NEI$Emissions))
# [1] 0 - No missing data

# calculate descriptive stats by year to get a feel for the data 
mn <- NEI %>% 
        group_by(year) %>%
        summarise(
                mean = mean(Emissions),
                stv = sd(Emissions),
                median = median(Emissions),
                IQR = IQR(Emissions)
        )

#Plot the data for each year
png("plot1.png")
boxplot(log10(NEI$Emissions)~NEI$year)
dev.off()
