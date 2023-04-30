# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# set working directory
setwd("C:/Users/uri_r/OneDrive/02-uri/01-PostDoc/101-DataScienceCourse/03-Exploratory Data Analysis/W3/02-Assignments/Air-pollution")

#load relevant library
library(dplyr)

## read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Subset the Baltimore city data by fips variable
pmBC <- subset(NEI, fips == "24510")

#find the SCC related to motor vehicle sources
IndMV <- grep("^Mobile - .* Gasoline .* Vehicles$", SCC$EI.Sector) # find the indeces
scc.val <- SCC$SCC[IndMV]                            # find the SCC values

#Subset motor vehicle data
pm <- subset(pmBC, SCC %in% scc.val)

# calculate descriptive stats by year to get a feel for the data 
mn <- pm %>% 
        group_by(year) %>%
        summarise(
                mean = mean(Emissions),
                stv = sd(Emissions),
                median = median(Emissions),
                IQR = IQR(Emissions)
        )

# plot the change in emissions across the years - across total US
png("plot5.png")
boxplot(log10(pm$Emissions)~pm$year, main = "Motor vehicle-related PM2.5 emissions")
dev.off()