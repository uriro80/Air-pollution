# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

# set working directory
setwd("C:/Users/uri_r/OneDrive/02-uri/01-PostDoc/101-DataScienceCourse/03-Exploratory Data Analysis/W3/02-Assignments/Air-pollution")

#load relevant library
library(dplyr)

## read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Subset the Baltimore city data by fips variable
pmBC <- subset(NEI, fips == "24510")

# calculate descriptive stats by year to get a feel for the data 
mn <- pmBC %>% 
        group_by(year) %>%
        summarise(
                mean = mean(Emissions),
                stv = sd(Emissions),
                median = median(Emissions),
                IQR = IQR(Emissions)
        )

#Plot the data for each year
png("plot2.png")
boxplot(log10(pmBC$Emissions)~pmBC$year, main = "Baltimore City PM2.5 emissions")
dev.off()
