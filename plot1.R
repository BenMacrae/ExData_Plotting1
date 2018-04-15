#This file is used to generate the plots for wk1 of the exploratory data analysis project.

#Load packages needed.
require(dplyr)
require(crayon)
require(lubridate)

#Download and unzip file, if it doesn't already exist.

if(!"household_power_consumption.txt" %in% list.files("ElectricConsumption")){
  download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip","ElectricdataFUCI_HAR.zip")
  unzip("ElectricdataFUCI_HAR.zip", exdir = "ElectricConsumption")        
}


#Load all the data, then use the lubridate package to interperet the dates and times as
#appropriate variables and select only those in the range stored in the variable 'intervaldays'
#This is only executed if the resulting variables are not present in the environment.

if(!(exists("alldata") | exists("shortdata") )){
  alldata <- read.table("ElectricConsumption\\household_power_consumption.txt", sep = ";", header = T, colClasses = "character")
  
  alldata["DateTime"] <- dmy_hms(alldata[["Date"]] %+% alldata[["Time"]])
  
  #Note that interval counts a day as being the midnight starting the day, hence the 3 day interval.
  intervaldays <- interval(ymd("2007/02/01"), ymd("2007/02/03"))
  intervaltest <- function(daytime,interval){daytime %within% interval}
  ininterval <- mapply(intervaltest, alldata["DateTime"], MoreArgs=list(intervaldays))
  
  shortdata <- alldata[ininterval,]
}

#We now select the Global Active Power variable and remove NA values, and plot it as a histogram 
#to 'plot1.png'
GlobalActivePower <- shortdata %>% select(Global_active_power,DateTime)
input <- na.omit(as.numeric(GlobalActivePower$Global_active_power))
png("plot1.png")
hist(input, breaks = 12, col = "Red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
dev.off()

