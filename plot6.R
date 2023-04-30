# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
# in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?

# set working directory
setwd("C:/Users/uri_r/OneDrive/02-uri/01-PostDoc/101-DataScienceCourse/03-Exploratory Data Analysis/W3/02-Assignments/Air-pollution")

#load relevant library
library(dplyr)
library(lattice)

## read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Subset the Baltimore city data by fips variable
pmBC <- subset(NEI, fips == "24510")
#Subset the Baltimore city data by fips variable
pmLA <- subset(NEI, fips == "06037")

#find the SCC related to motor vehicle sources
IndMV <- grep("^Mobile - .* Gasoline .* Vehicles$", SCC$EI.Sector) # find the indeces
scc.val <- SCC$SCC[IndMV]                            # find the SCC values

#Subset motor vehicle data
pmBCsub <- subset(pmBC, SCC %in% scc.val)
pmLAsub <- subset(pmLA, SCC %in% scc.val)

# add a city column for each df, BC = Baltimore and LA=Los Angeles.
pmBCsub$city <- rep("BC", dim(pmBCsub)[1])
pmLAsub$city <- rep("LA", dim(pmLAsub)[1])

# bind rows to form 1 df
pm <- rbind(pmBCsub, pmLAsub)

# calculate descriptive stats by year to get a feel for the data 
mn <- pm %>% 
        group_by(year, city) %>%
        summarise(
                mean = mean(Emissions),
                stv = sd(Emissions),
                median = median(Emissions),
                IQR = IQR(Emissions)
        )

# plot the change in emissions across the years - across total US
png("plot6.png")
xyplot(log10(Emissions)~year|city, data = pm,
       pch = 19,
       alpha = 0.2,
       col = "black",
       xlab = "Year",
       ylab = "log10(Emissions)",
       main = "Motor vehicle-related PM2.5 emissions")
dev.off()
