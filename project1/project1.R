## load libraries
library(ggplot2)
library(plyr)
library(knitr)
library(reshape2)

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

zipFilePath
projDir
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

dataActivity$DateTime  <-
  as.POSIXct(dataActivity$date, format = "%Y-%m-%d")
dataActivity$day <- weekdays(as.Date(dataActivity$date))

#cleanup NA
tidyData <- dataActivity[!is.na(dataActivity$steps),]

head(tidyData)

## Compute steps per day
aggrDaySteps <-
  aggregate(tidyData$steps ~ tidyData$date, FUN = sum,)
head(aggrDaySteps)

colnames(aggrDaySteps) <- c("Day","Steps")
##head(aggrDaySteps)

## Histogram of the total number of steps taken each day
#build histogram of the aggregated Day steps

png(filename = "steps.png")

hist(
  aggrDaySteps$Steps, breaks = 5 ,col = "green", ylab = "Frequency" , xlab = "Steps" , main = "Total Steps per Weekday"
)

dev.off()
#------------------------------------------------------------------------------------------------------------------------


## Mean and median number of steps taken each day.
sumByDate <-
  ddply(tidyData, .(interval), summarise, Avg = mean(steps))

meanSteps <- round(as.integer(mean(sumByDate$sum, na.rm = TRUE)))
medianSteps <- as.integer(median(sumByDate$Avg,na.rm = TRUE))

paste("Mean steps per day: ", meanSteps)
paste("Median steps per day: ", medianSteps)

# ## Time series plot of the average number of steps taken

png("plot-ts.png")
ggplot(sumByDate, aes(x = interval, y = Avg), xlab="internal", ylab="Average Steps") +
  geom_line() +  ggtitle("Avg steps per interval")  + xlab("interval")
+ ylab("Average Steps")

dev.off()

## The 5-minute interval that, on average, contains the maximum number of steps
##get maximum
maxSteps <- max(sumByDate$Avg)

## get interval with max
inv <- sumByDate[sumByDate$Avg == maxSteps,1]
paste("interval with max steps is :- ", inv)

##Code to describe and show a strategy for imputing missing data
totalNA <- sum(is.na(dataActivity$steps))
paste("No of Missing values = ", totalNA)
#rows with missing steps
missingRow <- nrow(dataActivity[is.na(dataActivity$steps),])
paste("No. of rows with NA in steps field:- ", missingRow)

## filling the missing data with day average of 5 min interval values
weekdayAvg <- ddply(tidyData, .(interval,day), summarise, Avg = mean(steps))
isNAdata <- dataActivity[is.na(dataActivity$steps),]
mergeDat <- merge(isNAdata,weekdayAvg, by=c("interval", "day")) 
colnames(mergeDat) <- c("steps", "date", "interval","day","DateTime")


mergeData <- rbind(tidyData,mergeDat)


head(mergeData, tail(15))