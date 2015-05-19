# Load required packages
require(dplyr)
require(ggplot2)

# Make sure data objects are in memory
if (exists("data.codes")) {
  # the data.codes object is in memory
} else if(file.exists("data.codes.RData")) {
  load("data.codes.RData")
} else {
  data.codes <- readRDS("Source_Classification_Code.rds")
  save(data.codes, file="data.codes.RData")
}

if (exists("data.summary")) {
  # the data.summary object is in memory
} else if(file.exists("data.summary.RData")) {
  load("data.summary.RData")
} else {
  data.summary <- readRDS("summarySCC_PM25.rds")
  data.summary$type <- as.factor(data.summary$type)
  #data.summary$year <- as.factor(data.summary$year)
  data.summary$Pollutant <- as.factor(data.summary$Pollutant)
  data.summary <- tbl_df(data.summary) # Convert to dply object
  save(data.summary, file="data.summary.RData")
}


################ Plot-1 ################
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission from 
#all sources for each of the years 1999, 2002, 2005, and 2008.
#
# The total emsissions have decreased 
#
summary.allByYear <- data.summary %>% 
  group_by(year) %>% summarise(total = sum(Emissions))
#x <- as.character(summary.allByYear$year)
x <- summary.allByYear$year
y <- summary.allByYear$total
barplot(y, ylab="Total Emisssion",
        xlab="Year", names.arg=x,
        main="(1): Total PM2.5 Emisssion per year" )
dev.copy(png, file="plot1.png", width=480, height=480)
dev.off()
rm(summary.allByYear, x, y) # Clean up


################ Plot-2 ################
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
#from 1999 to 2008? Use the base plotting system to make a plot answering this question.
#
# The total emsissions have decreased overall, but had some up and down oscillations
#
summary.BaltimoreByYear <- data.summary %>% 
  filter(fips == "24510") %>%
  group_by(year) %>% summarise(total = sum(Emissions))
#x <- as.character(summary.BaltimoreByYear$year)
x <- summary.BaltimoreByYear$year
y <- summary.BaltimoreByYear$total
barplot(y, ylab="Baltimore Total",
        xlab="Year", names.arg=x,
        main="(2): Emisssion per year in Baltimore")
dev.copy(png, file="plot2.png", width=480, height=480)
dev.off()
rm(summary.BaltimoreByYear, x, y) # Clean up


################ Plot-3 ################
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
#Which have seen increases in emissions from 1999-2008? 
#Use the ggplot2 plotting system to make a plot answer this question.
#
# Decreases: "NON-ROAD", "NONPOINT", "ON-ROAD"
# Increases: "POINT", over 1999 to 2005, followed by a decrease from 2005 to 2008
#
summary.BaltimoreEachType <- data.summary %>% 
  filter(fips == "24510") %>%
  group_by(type, year) %>% summarise(total = sum(Emissions))
ggplot(data=summary.BaltimoreEachType, aes(x = as.factor(year),y = total)) +
  geom_bar(stat="identity") +
  facet_wrap(~type, scales="free_y") +
  ggtitle("(3): Baltimore Emissions by Type") +
  xlab("Year") + ylab("Total Emmissions (NOTE: Different Scales)")
dev.copy(png, file="plot3.png", width=480, height=480)
dev.off()
rm(summary.BaltimoreEachType) # Clean up



################ Plot-4 ################
#Across the United States, how have emissions from 
#coal combustion-related sources changed from 1999-2008?
#
# Coal combustion-related sources were fairly consant in 1999, 2002, and 2005
# and then dropped significantly in 2008
#

# USe http://www.epa.gov/air/emissions/basic.htm for definitions
# Search all SCC Codes in EI.Sector that start with "Fuel Comb" and end with "Coal"
selectedRows <- data.codes[grep("^Fuel Comb.*Coal$", data.codes$EI.Sector), ]
selectedRows$SCC <- as.character(selectedRows$SCC)

summary.CombCoal <- data.summary %>% filter(SCC %in% selectedRows$SCC) %>%
  group_by(year) %>% summarise(total = sum(Emissions))

#x <- as.character(summary.CombCoal$year)
x <- summary.CombCoal$year
y <- summary.CombCoal$total
barplot(y, ylab="Total Coal Combustion",
        xlab="Year", names.arg=x,
        main="(4): US Coal Combustion Emisssion per year" )
dev.copy(png, file="plot4.png", width=480, height=480)
dev.off()
rm(selectedRows, summary.CombCoal, x, y) # Clean up


################ Plot-5 ################
#How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
#
# Emissions from motor vehicle sources in Baltimore dropped by more than 50% from 1999 to 2002.
# They were then about the same from 2002 to 2005, and increased some in 2008
#

# USe http://www.epa.gov/air/emissions/basic.htm for definitions
# Search all SCC Codes in EI.Sector that start with "Mobile" 
selectedRows <- data.codes[grep("^Mobile", data.codes$EI.Sector), ]
selectedRows$SCC <- as.character(selectedRows$SCC)

summary.BaltimoreOnly <- data.summary %>% filter(fips == "24510") 

summary.BaltimoreOnlyVehicles <- summary.BaltimoreOnly %>%
  filter(SCC %in% selectedRows$SCC) %>%
  group_by(year) %>% summarise(total = sum(Emissions))

#x <- as.character(summary.BaltimoreOnlyVehicles$year)
x <- summary.BaltimoreOnlyVehicles$year
y <- summary.BaltimoreOnlyVehicles$total
barplot(y, ylab="Total Vehicle Emisssions",
        xlab="Year", names.arg=x,
        main="(5): Baltimore Vehicle Emisssion per year" )
dev.copy(png, file="plot5.png", width=480, height=480)
dev.off()
rm(selectedRows, summary.BaltimoreOnly, summary.BaltimoreOnlyVehicles, x, y) # Clean up


################ Plot-6 ################
#Compare 
#emissions from motor vehicle sources in Baltimore City (fips == "24510") with 
#emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?
#
# Overall, emissions from motor vehicle sources in Baltimore (mean~540) are significantly 
# less than those in LA (mean~9500).
# LA saw an increase from 1999 to 2002, about the same from 2002 to 2005, and then a drop again in
# 2008 (but not to the level of 1999)
# Baltimore was the opposite; a drop from 1999 to 2002, about the same from 2002 to 2005, and then
# an increase in 2008 (but not as high as in 1999).
# LA has seen the largest absolute changes and largest standard deviation over time.
# LA has seen changes over the range of ~3400 (min~7500, max~11000, sd~1600), and 
# Baltimore has seen changes only over the range of ~500 (min~375, max~870, sd~230)

# USe http://www.epa.gov/air/emissions/basic.htm for definitions
# Search all SCC Codes in EI.Sector that start with "Mobile" 
selectedRows <- data.codes[grep("^Mobile", data.codes$EI.Sector), ]
selectedRows$SCC <- as.character(selectedRows$SCC)

summary.BaltLAOnly <- data.summary %>% filter(fips == "24510" | fips == "06037")
summary.BaltLAOnlyVehicles <- summary.BaltLAOnly %>%
  filter(SCC %in% selectedRows$SCC) %>%
  group_by(fips, year) %>% summarise(total = sum(Emissions))
# Change fips to city names
summary.BaltLAOnlyVehicles$fips <- as.factor(summary.BaltLAOnlyVehicles$fips)
levels(summary.BaltLAOnlyVehicles$fips)[levels(summary.BaltLAOnlyVehicles$fips)=="06037"] <- "Los Angeles"
levels(summary.BaltLAOnlyVehicles$fips)[levels(summary.BaltLAOnlyVehicles$fips)=="24510"] <- "Baltimore"

ggplot(data=summary.BaltLAOnlyVehicles, aes(x = as.factor(year),y = total)) +
  geom_bar(stat="identity") +
  facet_wrap(~fips) +
  ggtitle("(6): LA(Left) and Baltimore(Right) Vehicle Emissions showing mean and range") +
  xlab("Year") + ylab("Total Vehicle Emmissions") +
  geom_line(stat="hline", yintercept="mean", aes(group=fips)) +
  geom_line(stat="hline", yintercept="min", aes(group=fips), lty=2) + 
  geom_line(stat="hline", yintercept="max", aes(group=fips), lty=2)
dev.copy(png, file="plot6.png", width=480, height=480)
dev.off()

# Calculate some values related to the data
group_by(summary.BaltLAOnlyVehicles, fips) %>% 
  summarise(mean=mean(total), min=min(total), max=max(total), range=max(total) - min(total), sd=sd(total))
rm(selectedRows, summary.BaltLAOnly, summary.BaltLAOnlyVehicles) # Clean up



