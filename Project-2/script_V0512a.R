#
require(dplyr)

if (file.exists("data.codes.RData")) {
  load("data.codes.RData")
} else {
  data.codes <- readRDS("Source_Classification_Code.rds")
  save(data.codes, file="data.codes.RData")
}
if (file.exists("data.summary.RData")) {
  load("data.summary.RData")
} else {
  data.summary <- readRDS("summarySCC_PM25.rds")
  data.summary$type <- as.factor(data.summary$type)
  data.summary$year <- as.factor(data.summary$year)
  save(data.summary, file="data.summary.RData")
}

data.summary.dplyr <- tbl_df(data.summary)
################-1
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission from 
#all sources for each of the years 1999, 2002, 2005, and 2008.
summary.allByYear <- data.summary.dplyr %>% 
  group_by(year) %>% summarise(total = sum(Emissions))
#totalByYear <- table(as.numeric(levels(summary.allByYear$year)), summary.allByYear$total)
foo <- matrix(c(as.numeric(levels(summary.allByYear$year)), summary.allByYear$total),
  nrow=2, ncol=4, byrow=TRUE)
barplot(foo)
plot(as.numeric(levels(summary.allByYear$year)), summary.allByYear$total, 
     type="l", xlab="Year", ylab="Total Emissions", main="Total Emmisions per Year",
     xlim=c(1999,2008))


################-2
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
#from 1999 to 2008? Use the base plotting system to make a plot answering this question.

################-3
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
#Which have seen increases in emissions from 1999-2008? 
#Use the ggplot2 plotting system to make a plot answer this question.

################-4
#Across the United States, how have emissions from coal combustion-related sources changed 
#from 1999-2008?

################-5
#How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

################-6
#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor 
#vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater 
#changes over time in motor vehicle emissions?
