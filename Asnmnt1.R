q1 <- function(){
        
        require(reshape2)
        
        data <- read.csv("activity.csv")
                
        meltedData <- melt(data, "date", "steps")
        
        # summing melted data
        castedData <- dcast(meltedData, date ~ variable, sum)
        castedData$date <- as.Date(castedData$date, format="%Y-%m-%d")
        
        barplot(castedData$steps, names.arg=format(castedData$date, "%b %d"), main="Total number of steps taken each day", las=2, ylab="Steps")
        
        #mean(castedData$steps, na.rm=TRUE)
        
        #median(castedData$steps, na.rm=TRUE)
        
        #Imputation; replacing NA values
        
        #computing average steps per interval across all days
        #m <- melt(data, "interval", "steps")
        #c <- dcast(m, interval ~ variable, mean, na.rm=TRUE)
        #data[nad, "steps"]<-c[as.factor(data[nad, "interval"]), "steps"]
        
        #g <- ggplot(c, aes(interval, steps))
        #g + geom_point() + geom_line()
        
        #printing max time interval
        #c[c$steps==max(c$steps),]
        
        # total number of NAs
        #nrow(data[is.na(data$steps),])
        
        #padding 0 to interval
        #formatC(c$interval, width = 4, format = "d", flag = "0"),
        

        #d1$steps[is.na(d1$steps)]<-mean(castedData$steps, na.rm=TRUE)
}

q2 <- function(){
        
        require(reshape2)
        
        #Reading from CSV file
        data <- read.csv("activity.csv")
        
        # melting data by "Steps"
        meltedData <- melt(data, "date", "steps")
        
        # Computing mean 
        meanData <- dcast(meltedData, date ~ variable, mean)
        
        #Changing column name
        colnames(meanData)[2] <- "Steps-Mean"
        
        #Rounding mean values to nearest integer
        meanData[2]<-round(meanData[2], 2)
        
        # Computing median
        medianData <- dcast(meltedData, date ~ variable, median, fill=NaN)
        meanData["Steps-Median"] <- medianData$steps
        
        meanData$date <- as.Date(meanData$date, format="%Y-%m-%d")
        
        meanData
        
}

q3 <- function(){
        
        require(reshape2)
        
        data <- read.csv("activity.csv")
        
        #filling missing values
        
        #computing average steps per interval across all days
        meltedData <- melt(data, "interval", "steps")
        castedData <- dcast(meltedData, interval ~ variable, mean, na.rm=TRUE)
        nadata <- is.na(data$steps)
        data[nadata, "steps"] <- castedData[as.factor(data[nadata, "interval"]), "steps"]
        
        
        meltedData <- melt(data, "date", "steps")
        
        # summing melted data
        castedData <- dcast(meltedData, date ~ variable, sum)
        castedData$date <- as.Date(castedData$date, format="%Y-%m-%d")
        
        barplot(castedData$steps, names.arg=format(castedData$date, "%b %d"), main="Total number of steps taken each day", las=2, ylab="Steps")
        
}

q4 <- function(){
        
        require(reshape2)
        
        data <- read.csv("activity.csv")
        
        #filling missing values
        
        #computing average steps per interval across all days
        meltedData <- melt(data, "interval", "steps")
        castedData <- dcast(meltedData, interval ~ variable, mean, na.rm=TRUE)
        nadata <- is.na(data$steps)
        data[nadata, "steps"] <- castedData[as.factor(data[nadata, "interval"]), "steps"]
        
        data$day <- weekdays(as.Date(data$date, format="%Y-%m-%d")) 
        data[data$day=="Sunday" | data$day=="Saturday", "day"] <- "Weekend" 
        data[data$day!="Weekend", "day"] <- "Weekday" 
        
        
        # weekdays(as.Date(d$date, format="%Y-%m-%d"))  
        
        meltedData <- melt(data, c("day","interval"), "steps")
        
        # summing melted data
        castedData <- dcast(meltedData, day + interval ~ variable, mean)
        
        g <- ggplot(castedData, aes(interval, steps))
        
        g <- g + geom_point() + geom_line() + facet_grid(. ~ day) + ggtitle("Differences in activity patterns between weekdays and weekends")
        
        g
        #castedData
        #data
}