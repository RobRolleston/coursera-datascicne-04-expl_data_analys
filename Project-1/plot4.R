# read file
require(sqldf)
colClasses = c("character", "character", rep("numeric", 7))
data.subset <- read.csv.sql("./household_power_consumption.txt", stringsAsFactors=FALSE, header=TRUE,
                       sep=";",  colClasses = colClasses,
                       sql='select * from file where Date = "1/2/2007" or Date="2/2/2007"')
data.subset$Date <- strptime(data.subset$Date, format="%d/%m/%Y")
data.subset$dateTime <- strptime(paste(data.subset$Date, data.subset$Time), format="%Y-%m-%d %H:%M:%S")

#plot4
png("plot4.png", width=480, height=480)
par.o <- par()
par(mfrow=c(2,2), mar = c(4, 4, 2, 1), oma = c(0, 0, 2, 0))
#UL
plot(data.subset$dateTime, data.subset$Global_active_power, type="l",
     xlab="", ylab="Global Active Power")

#UR
plot(data.subset$dateTime, data.subset$Voltage, type="l",
     xlab="datetime", ylab="Voltage")
#LL
with(data.subset, plot(dateTime, Sub_metering_1, type="n",
                       main="", ylab="Energy sub metering", xlab=""))
with(data.subset, lines(dateTime, Sub_metering_1, col="black"))
with(data.subset, lines(dateTime, Sub_metering_2, col="red"  ))
with(data.subset, lines(dateTime, Sub_metering_3, type="l", col="blue" ))
# use locator(1) to get the x-y coordinates of the legend
#legend(1170357065, 40, col = c("black", "red", "blue"),  lwd=2, bty="n",
legend("topright", col = c("black", "red", "blue"),  lwd=2, bty="n",
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

#LR
plot(data.subset$dateTime, data.subset$Global_reactive_power, type="l",
     xlab="datetime", ylab="Global Active Power")

par(par.o)
dev.off()
rm(par.o)


  