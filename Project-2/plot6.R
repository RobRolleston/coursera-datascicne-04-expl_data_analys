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



