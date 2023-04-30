# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.


# set working directory
setwd("C:/Users/uri_r/OneDrive/02-uri/01-PostDoc/101-DataScienceCourse/03-Exploratory Data Analysis/W3/02-Assignments/Air-pollution")

#load relevant library
library(dplyr)
library(ggplot2)

## read data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Subset the Baltimore city data by fips variable
pmBC <- subset(NEI, fips == "24510")

# calculate descriptive stats by year to get a feel for the data 
mn <- pmBC %>% 
        group_by(year, type) %>%
        summarise(
                mean = mean(log10(Emissions)),
                stv = sd(log10(Emissions)),
                median = median(log10(Emissions)),
                IQR = IQR(log10(Emissions))
        )

# form a data frame for every type
type1 <- subset(pmBC, type == "NON-ROAD")
type2 <- subset(pmBC, type == "NONPOINT")
type3 <- subset(pmBC, type == "ON-ROAD")
type4 <- subset(pmBC, type == "POINT")

# plot for NON-ROAD
g <- ggplot(type1, aes(year, log10(Emissions)))
p1 <- g + geom_point() + geom_segment(aes(y = mn$median[1], yend = mn$median[5], x = 1999, xend = 2002)) +
        geom_segment(aes(y = mn$median[5], yend = mn$median[9], x = 2002, xend = 2005)) + 
        geom_segment(aes(y = mn$median[9], yend = mn$median[13], x = 2005, xend = 2008)) + 
        ggtitle("NON-ROAD") 

# plot for NONPOINT
g1 <- ggplot(type2, aes(year, log10(Emissions)))
p2 <- g1 + geom_point() + geom_segment(aes(y = mn$median[2], yend = mn$median[6], x = 1999, xend = 2002)) +
        geom_segment(aes(y = mn$median[6], yend = mn$median[10], x = 2002, xend = 2005)) + 
        geom_segment(aes(y = mn$median[10], yend = mn$median[14], x = 2005, xend = 2008)) + 
        ggtitle("NONPOINT") 

# plot for ON-ROAD
g2 <- ggplot(type3, aes(year, log10(Emissions)))
p3 <- g2 + geom_point() + geom_segment(aes(y = mn$median[3], yend = mn$median[7], x = 1999, xend = 2002)) +
        geom_segment(aes(y = mn$median[7], yend = mn$median[11], x = 2002, xend = 2005)) + 
        geom_segment(aes(y = mn$median[11], yend = mn$median[15], x = 2005, xend = 2008)) + 
        ggtitle("ON-ROAD") 

# plot for POINT
g3 <- ggplot(type3, aes(year, log10(Emissions)))
p4 <- g3 + geom_point() + geom_segment(aes(y = mn$median[4], yend = mn$median[8], x = 1999, xend = 2002)) +
        geom_segment(aes(y = mn$median[8], yend = mn$median[12], x = 2002, xend = 2005)) + 
        geom_segment(aes(y = mn$median[12], yend = mn$median[16], x = 2005, xend = 2008)) + 
        ggtitle("POINT") 

# arrange figures on page and save as png
library(ggpubr)
png("plot3.png", width=4507, height=2480, res=300)
ggarrange(p1, p2+ rremove("ylab") + rremove("xlab"), p3, p4+ rremove("ylab"),
          labels = c("A", "B", "C", "D"),
          nrow = 2, ncol = 2)
dev.off()