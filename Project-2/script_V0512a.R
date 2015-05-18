#
require(dplyr)
require(ggplot2)

if (!exists("data.codes") && file.exists("data.codes.RData")) {
  load("data.codes.RData")
} else {
  data.codes <- readRDS("Source_Classification_Code.rds")
  save(data.codes, file="data.codes.RData")
}
if (!exists("data.summary") && file.exists("data.summary.RData")) {
  load("data.summary.RData")
} else {
  data.summary <- readRDS("summarySCC_PM25.rds")
  data.summary$type <- as.factor(data.summary$type)
  data.summary$year <- as.factor(data.summary$year)
  data.summary$Pollutant <- as.factor(data.summary$Pollutant)
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
        main="(1): Total PM2.5 Emisssion per year" )
mod.allByYear <- lm(y ~ seq(1:4))
abline(mod.allByYear, lwd=2)
rm(summary.allByYear, x, y, mod.allByYear)

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
        main="(2): Emisssion per year in Baltimore")
mod.allByYear <- lm(y ~ seq(1:4))
abline(mod.allByYear, lwd=2)
rm(summary.BaltimoreByYear, x, y, mod.allByYear)

################-3
#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
#Which have seen increases in emissions from 1999-2008? 
#Use the ggplot2 plotting system to make a plot answer this question.
summary.BaltimoreEachType <- data.summary.dplyr %>% 
  filter(fips == "24510") %>%
  group_by(type, year) %>% summarise(total = sum(Emissions))
ggplot(data=summary.BaltimoreEachType, aes(x = year,y = total)) +
  geom_bar(stat="identity") +
  geom_smooth(method="lm", se=FALSE, aes(group=type)) +
  facet_wrap(~type, scales="free_y") +
  ggtitle("(3): Baltimore Emissions by Type")

rm(summary.BaltimoreEachType, p)

# ggplot(data=summary.BaltimoreByYear,aes(x = levels(year),y = total)) +
#   #geom_point() +
#   geom_bar(stat="identity") +
#   stat_smooth(method="lm", se=FALSE, aes(group=1))

################-4
#Across the United States, how have emissions from 
#coal combustion-related sources changed from 1999-2008?
# USe http://www.epa.gov/air/emissions/basic.htm 
# Search all SCC Codes in EI.Sector that start with "Fuel Comb" and end with "Coal"
selectedRows <- data.codes[grep("^Fuel Comb.*Coal$", data.codes$EI.Sector), ]
selectedRows$SCC <- as.character(selectedRows$SCC)
#summary.BaltimoreOnly <- data.summary.dplyr %>% filter(fips == "24510") 

#summary.CombCoal <- inner_join(data.summary.dplyr, selectedRows, by="SCC") %>%
summary.CombCoal <- data.summary.dplyr %>% filter(SCC %in% selectedRows$SCC) %>%
  group_by(year) %>% summarise(total = sum(Emissions))

x <- levels(summary.CombCoal$year)
y <- summary.CombCoal$total
barplot(y, ylab="Total Coal Combustion",
        xlab="Year", names.arg=x,
        main="(4): US Coal Combustion Emisssion per year" )
mod.allByYear <- lm(y ~ seq(1:4))
abline(mod.allByYear, lwd=2)
rm(selectedRows, summary.CombCoal, x, y, mod.allByYear)

################-5
#How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
# USe http://www.epa.gov/air/emissions/basic.htm 
# Search all SCC Codes in EI.Sector that start with "Mobile" 
selectedRows <- data.codes[grep("^Mobile", data.codes$EI.Sector), ]
selectedRows$SCC <- as.character(selectedRows$SCC)

summary.BaltimoreOnly <- data.summary.dplyr %>% filter(fips == "24510") 

#summary.BaltimoreOnlyVehicles <- inner_join(summary.BaltimoreOnly, selectedRows, by="SCC")
summary.BaltimoreOnlyVehicles <- summary.BaltimoreOnly %>%
  filter(SCC %in% selectedRows$SCC) %>%
  group_by(year) %>% summarise(total = sum(Emissions))

x <- levels(summary.BaltimoreOnlyVehicles$year)
y <- summary.BaltimoreOnlyVehicles$total
barplot(y, ylab="Total Vehicle Emisssions",
        xlab="Year", names.arg=x,
        main="(5): Baltimore Vehicle Emisssion per year" )
mod.allByYear <- lm(y ~ seq(1:4))
abline(mod.allByYear, lwd=2)
rm(selectedRows, summary.BaltimoreOnly, summary.BaltimoreOnlyVehicles, x, y, mod.allByYear)

################-6
#Compare 
#emissions from motor vehicle sources in Baltimore City (fips == "24510") with 
#emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?
selectedRows <- data.codes[grep("^Mobile", data.codes$EI.Sector), ]
selectedRows$SCC <- as.character(selectedRows$SCC)

summary.BaltLAOnly <- data.summary.dplyr %>% filter(fips == "24510" | fips == "06037")
summary.BaltLAOnlyVehicles <- summary.BaltLAOnly %>%
  filter(SCC %in% selectedRows$SCC) %>%
  group_by(fips, year) %>% summarise(total = sum(Emissions))
ggplot(data=summary.BaltLAOnlyVehicles, aes(x = year,y = total)) +
  geom_bar(stat="identity") +
  geom_smooth(method="lm", se=FALSE, aes(group=fips)) +
  facet_wrap(~fips, scales="free_y") +
  ggtitle("(6): Baltimore(24510) and LA(06037) Vehicle Emissions")

rm(selectedRows, summary.BaltLAOnly, summary.BaltLAOnlyVehicles, p)

