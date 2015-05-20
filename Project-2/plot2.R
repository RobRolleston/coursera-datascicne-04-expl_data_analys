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



