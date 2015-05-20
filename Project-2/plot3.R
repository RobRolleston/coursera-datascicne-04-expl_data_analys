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


