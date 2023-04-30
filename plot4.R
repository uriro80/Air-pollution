# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

# set working directory
setwd("C:/Users/uri_r/OneDrive/02-uri/01-PostDoc/101-DataScienceCourse/03-Exploratory Data Analysis/W3/02-Assignments/Air-pollution")

#load relevant library
library(dplyr)
library(ggplot2)

## read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#find the SCC related to coal combustion
IndCoal <- grep(".* Comb - .* - Coal$", SCC$EI.Sector) # find the indeces
scc.val <- SCC$SCC[IndCoal]                            # find the SCC values

#Subset coal combustion data
pm <- subset(NEI, SCC %in% scc.val)

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
png("plot4.png")
boxplot(log10(pm$Emissions)~pm$year, main = "Coal combustion-related sources PM2.5 emissions")
dev.off()