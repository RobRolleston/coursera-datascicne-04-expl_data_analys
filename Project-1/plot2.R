# read file
require(sqldf)
colClasses = c("character", "character", rep("numeric", 7))
data.subset <- read.csv.sql("./household_power_consumption.txt", stringsAsFactors=FALSE, header=TRUE,
                       sep=";",  colClasses = colClasses,
                       sql='select * from file where Date = "1/2/2007" or Date="2/2/2007"')
data.subset$Date <- strptime(data.subset$Date, format="%d/%m/%Y")
data.subset$dateTime <- strptime(paste(data.subset$Date, data.subset$Time), format="%Y-%m-%d %H:%M:%S")


#Plot2
png("plot2.png", width=480, height=480)
plot(data.subset$dateTime, data.subset$Global_active_power, type="l",
     xlab="", ylab="Global Active Power (kilowatts)")
dev.off()



  