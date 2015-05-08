# read file
require(sqldf)
colClasses = c("character", "character", rep("numeric", 7))
data.subset <- read.csv.sql("./household_power_consumption.txt", stringsAsFactors=FALSE, header=TRUE,
                       sep=";",  colClasses = colClasses,
                       sql='select * from file where Date = "1/2/2007" or Date="2/2/2007"')
data.subset$Date <- strptime(data.subset$Date, format="%d/%m/%Y")
data.subset$dateTime <- strptime(paste(data.subset$Date, data.subset$Time), format="%Y-%m-%d %H:%M:%S")


#plot3
png("plot3.png", width=480, height=480)
with(data.subset, plot(dateTime, Sub_metering_1, type="n",
                       main="", ylab="Energy sub metering", xlab=""))
with(data.subset, lines(dateTime, Sub_metering_1, col="black"))
with(data.subset, lines(dateTime, Sub_metering_2, col="red"  ))
with(data.subset, lines(dateTime, Sub_metering_3, type="l", col="blue" ))
legend("topright", col = c("black", "red", "blue"),  lwd=2,
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.off()



  