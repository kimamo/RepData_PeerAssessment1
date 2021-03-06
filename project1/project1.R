## load libraries
## load libraries
library(ggplot2)
library(plyr)
library(knitr)
library(reshape2)
library(lattice)



## Code for reading in the dataset and/or processing the data
## the zip file
activityZipFile <- "activity.zip"
#get current working dir
projDir <- getwd()
# traverse back for zip dir
setwd("..")
#set to zipped dir
zipFileDir  <- getwd()

#reset to back to current project folder
setwd(projDir)

zipFilePath <- paste(zipFileDir,activityZipFile,sep = "/")

activityCSVFullPath <- paste(projDir,"activity.csv", sep = "/")

#zipFilePath
#projDir

#check if unzipped file exists
if (!file.exists(activityCSVFullPath)) {
  ##Unzip downloaded file
  unzip(zipfile = zipFilePath,exdir = projDir)
}

##Read Activity Files
dataActivity <-
  read.csv(
    file.path(activityCSVFullPath),header = TRUE, sep = ',', stringsAsFactors = F, na.strings =
      c("NA","NaN", " ")
    , colClasses = c("numeric", "character","integer")
  )


str(dataActivity)

dataActivity$DateTime  <- as.POSIXct(dataActivity$date, format = "%Y-%m-%d")
dataActivity$day <- weekdays(as.Date(dataActivity$date))
weekends <- weekdays(as.Date(dataActivity$date)) %in% c("Saturday","Sunday")
head(weekends, tail(20))

#make dataType weekday then logically replace swap it with weekend if meets the weekend logic
dataActivity$dateType <- "weekday"
dataActivity$dateType[weekends == TRUE] <-"weekend"

head(dataActivity)

## Compute steps per day
aggrDaySteps <-aggregate(dataActivity$steps ~ dataActivity$date, FUN = sum, na.rm=TRUE)
colnames(aggrDaySteps) <- c("Date","Steps")
head(aggrDaySteps)
 

## Histogram of the total number of steps taken each day
#build histogram of the aggregated Day steps

png(filename = "steps.png")

hist(
  aggrDaySteps$Steps, breaks = seq(from=0, to=25000, by=2500) ,col = "green", xlab = "No. of Steps", main = "Total Steps per a week day"
)


dev.off()
#------------------------------------------------------------------------------------------------------------------------


## Mean and median number of steps taken each day.
meanSteps <- round(as.integer(mean(aggrDaySteps$Steps, na.rm = TRUE)))
medianSteps <- as.integer(median(aggrDaySteps$Steps,na.rm = TRUE))

paste("Mean steps per day: ", meanSteps)
paste("Median steps per day: ", medianSteps)

## Mean and median number of steps taken each day.
meanAggr <-
  tapply(dataActivity$steps, dataActivity$interval,  mean ,na.rm = TRUE)

# ## Time series plot of the average number of steps taken

png("plot-ts.png")
plot(row.names(meanAggr), meanAggr,  type = "l",
     xlab="Interval [5 - Min]", ylab="Average Steps across all days" ,main ="Avg steps taken" )

dev.off()
#---------------------------------------------------------------------------------------------------------
## The 5-minute interval that, on average, contains the maximum number of steps
##get maximum
maxSteps <- which.max(meanAggr)
## print interval with max
paste("interval with max steps is :- ", names(maxSteps))

#find the total NA
totalNA <- sum(is.na(dataActivity$steps))
paste("No of Missing values = ", totalNA)
#rows with missing steps
missingRow <- nrow(dataActivity[is.na(dataActivity$steps),])

## filling the missing data with day average of 5 min interval values
#data  of all NA rows
isNAdata <- dataActivity[is.na(dataActivity$steps),]
# get mean steps
meanSteps <- rep(mean(dataActivity$steps, na.rm = TRUE), times = length(isNAdata))

#find position of NA data to be swapped with mean steps
posOfNA <- which(is.na(dataActivity$steps))

#replace NA's with MeanSteps
dataActivity[posOfNA,"steps"] <- meanSteps
head(dataActivity, tail(15))


aggrByDay <- aggregate(dataActivity$steps, by=list(dataActivity$date),FUN =sum)

#Friendly name the columns
names(aggrByDay) <- c("date", "totals")

#plot hist based on step totals each day
png("histPlot-withMean")
hist(aggrByDay$totals, breaks=seq(from=0,to=25000,by=2500), col = "green", xlab = "Steps Totals", 
     main=expression("Sum total number of steps by Day\n -  NA replaced by the Mean "), ylim=c(0,30))

dev.off()

#--------------------------------------------------------------------------------------------------------
meanSteps1 <- round(as.integer(mean(aggrByDay$ totals, na.rm = TRUE)))
medianSteps1 <- as.integer(median(aggrByDay$totals,na.rm = TRUE))

#Do these values differ from the estimates from the first part of the assignment

paste("Mean steps per day after NA swap: ", meanSteps1)
paste("Median steps per day after NA swap: ", medianSteps1)

#----------------------------------------------------------------------------------------------------------

#Create a new factor variable in the dataset with two levels 
# – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
#factorize dataType
dataActivity$dateType <- as.factor(dataActivity$dateType)

dataAgg <- aggregate(steps ~ interval + dateType, dataActivity, "Mean(Steps)" = mean, FUN = mean)

names(dataAgg)[3] <-"Mean(Steps)"
head(dataAgg)
# diagram  a time-series plot for weekend and weekdays

png("wkend-wkdays-avg-intervals.png")

xyplot(dataAgg$`Mean(Steps)`~ interval | dateType , dataAgg, type = "l", layout =c(1,2), 
       main = "5-Min interval Time-series plot\n vs Average steps taken\n over weekends and weekdays",
       xlab="5-Min Interval", ylab="Avarage Steps")

dev.off()

png("wkend-wkdays-avg-intervals1.png")

ts <- ggplot(dataAgg, aes(x=interval, y=`Mean(Steps)`, color = dateType)) +
  geom_line() + ggtitle("5-Min interval Time-series plot\n vs Average steps taken\n over weekends and weekdays")+
  facet_wrap(~dateType, ncol = 1, nrow=2)
print(ts)


dev.off()

#dev.off()

#-----------------------------------------------------------------------------------------------
