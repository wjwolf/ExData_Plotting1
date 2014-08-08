



readData <- function(path.name, file.name, column.types, missing.types, sep) {
# modelled after a function in
# https://github.com/wehrley/wehrley.github.io/blob/master/SOUPTONUTS.md
    read.csv( paste(path.name, file.name, sep=""), 
              colClasses=column.types,
              na.strings=missing.types,
              sep=sep)
}

# load raw data
path <- "C:\\Users\\wolfw\\Documents\\GitHub\\ExData_Plotting1\\"
fileName <- "household_power_consumption.txt"
missingTypes <- c("?", "")
sep<-";"
colTypes <- c('character',  # Date  (convert to Date/Time later)
              'character',  # Time  (convert to Date/Time later)
              'numeric',    # Global_active_power
              'numeric',    # Global_reactive_power
              'numeric',    # Voltage
              'numeric',    # Global_intensity
              'numeric',    # Sub_metering_1
              'numeric',    # Sub_metering_2
              'numeric'     # Sub_metering_3
)
raw.data <- readData(path.name=path, file.name=fileName, column.types=colTypes,
                     missing.types=missingTypes, sep=sep)

# make a copy of the data to clean
problem.data<-raw.data

# convert Time to ISO Date/Time
problem.data$Time <- strptime(paste(problem.data$Date, " ", problem.data$Time),
                              format="%d/%m/%Y %H:%M:%S")

# convert Date to Date
problem.data$Date <- as.Date(problem.data$Date,format="%d/%m/%Y")

# select subset of data specified, 01/02/2007 to 02/02/2007
problem.data<-subset(problem.data,Date>="2007-02-01" & Date<="2007-02-02")


# open output file
png(file="plot2.png",width=480,height=480)

# plot graph
plot(problem.data$Time,
     problem.data$Global_active_power,
     type="l",  
     ylab="Global Active Power (kilowatts)",
     xlab="")

# close file
dev.off()

