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





