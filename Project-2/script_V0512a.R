#
require(dplyr)
require(ggplot2)

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
x <- levels(summary.allByYear$year)
y <- summary.allByYear$total
barplot(y, ylab="Total Emisssion",
        xlab="Year", names.arg=x,
        main="Total PM2.5 Emisssion per year" )

################-2
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
#from 1999 to 2008? Use the base plotting system to make a plot answering this question.
summary.BaltimoreByYear <- data.summary.dplyr %>% 
  filter(fips == "24510") %>%
  group_by(year) %>% summarise(total = sum(Emissions))
x <- levels(summary.BaltimoreByYear$year)
y <- summary.BaltimoreByYear$total
barplot(y, ylab="Baltimore Total",
        xlab="Year", names.arg=x,
        main=" Emisssion per year in Baltimore")


################-3
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
#Which have seen increases in emissions from 1999-2008? 
#Use the ggplot2 plotting system to make a plot answer this question.
summary.BaltimoreEachType <- data.summary.dplyr %>% 
  filter(fips == "24510") %>%
  group_by(type, year) %>% summarise(total = sum(Emissions))
p <- ggplot(data=summary.BaltimoreEachType, 
            aes(x = year,y = total)) +
  geom_bar(stat="identity") +
  facet_wrap(~type)
p

################-4
#Across the United States, how have emissions from 
#coal combustion-related sources changed from 1999-2008?

################-5
#How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

################-6
#Compare 
#emissions from motor vehicle sources in Baltimore City (fips == "24510") with 
#emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?
