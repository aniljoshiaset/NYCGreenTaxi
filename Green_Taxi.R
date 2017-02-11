#Install data.table and curl package
install.packages("data.table")
install.packages("curl")

#Creating a variable file_URL to store the path from where the data needs to be fetched
file_URL<-"https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv"

#Attaching the data.table package so that it can be used in the current session of R
library(data.table)

#Using fread to read the data directly from Internet and save it in a Data Table named NYC_Green_DT
NYC_Green_DT<-fread(file_URL)

#Creating a copy of NYC_Green_DT as NYC_Green_DT1 to retain the original dataset
NYC_Green_DT1<-NYC_Green_DT

#Created a function myStats t
myStats <- function(x) {
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  max <- max(a)
  UC <- m+3*s
  LC <- m-3*s
  outlier_flag<- max>UC | min<LC
  return(c(n=n, outlier_flag=outlier_flag, mean=m, stdev=s,min = min, max=max, UC=UC, LC=LC ))
}

#Fetching the output from myStats function to a new Trip_Stats character vector
Trip_Stats<-myStats(NYC_Green_DT1$Trip_distance)

# Capping the potential outliers with the Upper limit of the distribution
NYC_Green_DT1$Trip_distance[NYC_Green_DT1$Trip_distance>Trip_Stats[["UC"]]]<-Trip_Stats[["UC"]]

# Plotting the histogram for Trip_distance variable
hist(NYC_Green_DT1$Trip_distance, main="Histogram for Trip Distance", xlab="Trip Distance", ylab="Frequency", border="red", col="blue")

#Summary Statistics for Trip_distance variable
summary(NYC_Green_DT1$Trip_distance)

#Converted the data type of lpep_pickup_datetime from character to datetime using strptime function  
NYC_Green_DT1$lpep_pickup_datetime<-strptime(NYC_Green_DT1$lpep_pickup_datetime,format="%Y-%m-%d %H:%M:%S") 

#Extracted the hour from lpep_pickup_datetime using strftime function 
NYC_Green_DT1$ lpep_pickup_hour <-strftime(NYC_Green_DT1$lpep_pickup_datetime,format="%H")

#Created a table StatsbyPickupHour that contains the mean and median of Trip_distance variable grouped by the variable lpep_pickup_hour
StatsbyPickupHour<-NYC_Green_DT1[,.(Trip_distance_Mean=mean(Trip_distance),Trip_distance_Median=median(Trip_distance)), by=lpep_pickup_hour]

#Install and load sqldf package to perform sql operations
install.packages("sqldf")
require(sqldf)

#Query to select the Airport origination and termination points from the dataset
NYC_Green_JFK_P<-sqldf("select * from NYC_Green_DT1 where Pickup_latitude like '%40.64%' and Pickup_longitude like '%-73.78%'")
NYC_Green_JFK_D<-sqldf("select * from NYC_Green_DT1 where Dropoff_latitude like '%40.64%' and Dropoff_longitude like '%-73.78%'")
NYC_Green_lAGU_P<-sqldf("select * from NYC_Green_DT1 where Pickup_latitude like '%40.77%' and Pickup_longitude like '%-73.87%'")
NYC_Green_lAGU_D<-sqldf("select * from NYC_Green_DT1 where Dropoff_latitude like '%40.77%' and Dropoff_longitude like '%-73.87%'")

#Used Row Binding function to merge all the subsets on rows
NYC_Green_Airport1<-rbind(NYC_Green_JFK_P,NYC_Green_JFK_D)
NYC_Green_Airport2<-rbind(NYC_Green_Airport1,NYC_Green_lAGU_D)
NYC_Green_Airport<-rbind(NYC_Green_Airport1,NYC_Green_Airport2)

 #Calculate the average of Fare_amount from the merged subset
AvgFair_Airport<-mean(NYC_Green_Airport$Fare_amount)


#Created a new variable TipPercent and rounded the percentage to two digits
NYC_Green_DT1$TipPercent<-round((NYC_Green_DT1$Tip_amount/NYC_Green_DT1$Total_amount)*100, digits = 2)

#Created a new variable Average_Speed in NYC_Green_DT1 dataset using Trip_distance and Trip_Time_in_secs
NYC_Green_DT1$Average_Speed<-NYC_Green_DT1$Trip_distance/NYC_Green_DT1$Trip_Time_in_secs

#Create a new  variable WeekNumber using strftime function to extract week out of lpep_pickup_datetime variable
NYC_Green_DT1$WeekNumber<-strftime(NYC_Green_DT1$lpep_pickup_datetime,format="%W")

#Remove Infinite labelled values from Average_Speed variable so that ANOVA test can run
NYC_Green_DT1$Average_Speed[which(is.nan(NYC_Green_DT1$Average_Speed))] = NA
NYC_Green_DT1$Average_Speed[which(NYC_Green_DT1$Average_Speed==Inf)] = NA

#Run ANOVA test and get the results into a new variable Hypo_Test
Hypo_Test<-aov(Average_Speed ~ WeekNumber, NYC_Green_DT1)

#Fetch the results using summary function
summary(Hypo_Test)

#Run Anova test on the binned variable lpep_pickup_hour_bin
Hypo_Test_Speed<-aov(Average_Speed ~ lpep_pickup_hour_bin, NYC_Green_DT1)

#Get the results using summary function
summary(Hypo_Test)

