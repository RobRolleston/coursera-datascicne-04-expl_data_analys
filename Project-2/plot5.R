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




