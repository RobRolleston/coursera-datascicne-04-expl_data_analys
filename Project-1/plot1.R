# read file
require(sqldf)
colClasses = c("character", "character", rep("numeric", 7))
data.subset <- read.csv.sql("./household_power_consumption.txt", stringsAsFactors=FALSE, header=TRUE,
                       sep=";",  colClasses = colClasses,
                       sql='select * from file where Date = "1/2/2007" or Date="2/2/2007"')
data.subset$Date <- strptime(data.subset$Date, format="%d/%m/%Y")
data.subset$dateTime <- strptime(paste(data.subset$Date, data.subset$Time), format="%Y-%m-%d %H:%M:%S")


#Plot1
png("plot1.png", width=480, height=480)
hist(data.subset$Global_active_power, col="red",
     main="Global Active Power",
     xlab="Global Active Power (kilowatts)",
     ylab="Frequency")
dev.off()

  