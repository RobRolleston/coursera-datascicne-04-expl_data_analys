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



