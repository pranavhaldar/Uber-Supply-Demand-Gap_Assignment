# Loading library

library(stringr)
library(ggplot2)

# Reading from the Source Data file

uber<-read.csv("Uber Request Data.csv",stringsAsFactors=F)

# Data Cleaning

#Check for duplicate values

sum(duplicated(uber$Request.id))

#Check for NA values

sum(is.na(uber$Request.id))
sum(is.na(uber$Pickup.point))
sum(is.na(uber$Status))
sum(is.na(uber$Request.timestamp))

# Creating a uniform Timestamp

uber$Request.timestamp<-strtrim(uber$Request.timestamp,16)
uber$Drop.timestamp<-strtrim(uber$Drop.timestamp,16)

uber$Request.timestamp<-gsub("/","-",uber$Request.timestamp)
uber$Drop.timestamp<-gsub("/","-",uber$Drop.timestamp)

uber$Request.timestamp<-paste0(uber$Request.timestamp,":00")
uber$Drop.timestamp<-paste0(uber$Drop.timestamp,":00")

# Removing irregularities in Date

uber$Request.timestamp<-gsub("-1-","-01-",uber$Request.timestamp)
uber$Request.timestamp<-gsub("-2-","-02-",uber$Request.timestamp)
uber$Request.timestamp<-gsub("-3-","-03-",uber$Request.timestamp)
uber$Request.timestamp<-gsub("-4-","-04-",uber$Request.timestamp)
uber$Request.timestamp<-gsub("-5-","-05-",uber$Request.timestamp)
uber$Request.timestamp<-gsub("-6-","-06-",uber$Request.timestamp)
uber$Request.timestamp<-gsub("-7-","-07-",uber$Request.timestamp)
uber$Request.timestamp<-gsub("-8-","-08-",uber$Request.timestamp)
uber$Request.timestamp<-gsub("-9-","-09-",uber$Request.timestamp)

uber$Drop.timestamp<-gsub("-1-","-01-",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub("-2-","-02-",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub("-3-","-03-",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub("-4-","-04-",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub("-5-","-05-",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub("-6-","-06-",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub("-7-","-07-",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub("-8-","-08-",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub("-9-","-09-",uber$Drop.timestamp)

# Removing irregularities in Time

uber$Request.timestamp<-gsub(" 1:"," 01:",uber$Request.timestamp)
uber$Request.timestamp<-gsub(" 2:"," 02:",uber$Request.timestamp)
uber$Request.timestamp<-gsub(" 3:"," 03:",uber$Request.timestamp)
uber$Request.timestamp<-gsub(" 4:"," 04:",uber$Request.timestamp)
uber$Request.timestamp<-gsub(" 5:"," 05:",uber$Request.timestamp)
uber$Request.timestamp<-gsub(" 6:"," 06:",uber$Request.timestamp)
uber$Request.timestamp<-gsub(" 7:"," 07:",uber$Request.timestamp)
uber$Request.timestamp<-gsub(" 8:"," 08:",uber$Request.timestamp)
uber$Request.timestamp<-gsub(" 9:"," 09:",uber$Request.timestamp)

uber$Drop.timestamp<-gsub(" 1:"," 01:",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub(" 2:"," 02:",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub(" 3:"," 03:",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub(" 4:"," 04:",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub(" 5:"," 05:",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub(" 6:"," 06:",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub(" 7:"," 07:",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub(" 8:"," 08:",uber$Drop.timestamp)
uber$Drop.timestamp<-gsub(" 9:"," 09:",uber$Drop.timestamp)

# Converting to datetime format

uber$Request.timestamp <- as.POSIXlt(uber$Request.timestamp, format = "%d-%m-%Y %H:%M:%S")
uber$Drop.timestamp <- as.POSIXlt(uber$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")

uber$Request.hour <- format(uber$Request.timestamp, "%H")
uber$Request.hour<-as.numeric(uber$Request.hour)

colnames(uber)[8]<-"Request.timeslot"

# Identifying the Request Timeslot

for (i in 1:nrow(uber))
{  
if(uber[i,7]>=4 && uber[i,7]<8)
  {
    uber[i,8]<-"Early Morning"
  }
  else if(uber[i,7]>=8 && uber[i,7]<12)
  {
    uber[i,8]<-"Morning"
  }
  else if(uber[i,7]>=12 && uber[i,7]<16)
  {
    uber[i,8]<-"Afternoon"
  }
  else if(uber[i,7]>=16 && uber[i,7]<20)
  {
    uber[i,8]<-"Evening"
  }
  else if(uber[i,7]>=20)
  {
    uber[i,8]<-"Night"
  }
  else if(uber[i,7]>=0 && uber[i,7]<4)
  {
    uber[i,8]<-"Late Night"
  }
}

# Creating Data Subsets for analysis

uber_cancelled<-subset(uber,uber$Status=="Cancelled")
uber_no_car_available<-subset(uber,uber$Status=="No Cars Available")
uber_trip_completed<-subset(uber,uber$Status=="Trip Completed")

uber_airport<-subset(uber,uber$Pickup.point=="Airport")
uber_city<-subset(uber,uber$Pickup.point=="City")

uber_not_completed_trips<-subset(uber,uber$Status!="Trip Completed")
uber_airport_not_completed_trips<-subset(uber_airport,uber_airport$Status!="Trip Completed")
uber_city_not_completed_trips<-subset(uber_city,uber_city$Status!="Trip Completed")


# Analysis

# 1. Higher proportion of Completed Trips originate from City

ggplot(uber_completed_trips, aes(x = Pickup.point)) + 
  geom_bar(position="stack") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) + 
  xlab("Trip Origin") + 
  ylab("Frequency") + 
  ggtitle("Trip Origin Frequency Plot : Completed Trips")

# 2. Cancelled trips huge in trips originating from City

ggplot(uber_cancelled, aes(x = Pickup.point)) + 
  geom_bar(position="stack") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) + 
  xlab("Trip Origin") + 
  ylab("Frequency") + 
  ggtitle("Trip Origin Frequency Plot : Cancelled Trips")

# 3. High proportion of No Cars Available in trips originating from Airport

ggplot(uber_no_car_available, aes(x = Pickup.point)) + 
  geom_bar(position="stack") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) + 
  xlab("Trip Origin") + 
  ylab("Frequency") + 
  ggtitle("Trip Origin Frequency Plot : No Cars Available")

# 4. Early Morning and Morning time slots have very high Cancellation for trips originating from City

ggplot(uber_cancelled, aes(x = Pickup.point, fill = Request.timeslot)) + 
  geom_bar(position="stack") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) + 
  xlab("Trip Origin") + 
  ylab("Frequency") + 
  ggtitle("Trip Origin Frequency Plot : Cancelled Trips")

# 5. Evening and Night time slots have very huge proportions No Cars Available for trips originating from Airport

ggplot(uber_no_car_available, aes(x = Pickup.point, fill = Request.timeslot)) + 
  geom_bar(position="stack") + 
  theme(axis.text.x = element_text(angle=60, hjust=1)) + 
  xlab("Trip Origin") + 
  ylab("Frequency") + 
  ggtitle("Trip Origin Frequency Plot : No Cars Available")

# 6. At an overall level, the Trip Completion Ratio is 42% with highest non completion due to the Non Availability of Cars 

    ggplot(uber, aes(x = Status)) + 
    geom_bar(position="stack") + 
    theme(axis.text.x = element_text(angle=60, hjust=1)) + 
    xlab("Trip Status") + 
    ylab("Request Frequency") + 
    ggtitle("Trip Status Frequency Plot")

# 7. For trips originating from Airport, the major reason is Non Availability of Cars which accounts for Non Completion of abot 53% of Demand at the Airport
  
    ggplot(uber_airport, aes(x = Status)) + 
    geom_bar(position="stack") + 
    theme(axis.text.x = element_text(angle=60, hjust=1)) + 
    xlab("Trip Status") + 
    ylab("Request Frequency") + 
    ggtitle("Trip Status Frequency Plot : Airport Trips")
  
# 8. For trips originating from City, the major reason is trip being Cancelled which accounts for Non Completion of abot 30% of Demand in the City

    ggplot(uber_city, aes(x = Status)) + 
    geom_bar(position="stack") + 
    theme(axis.text.x = element_text(angle=60, hjust=1)) + 
    xlab("Trip Status") + 
    ylab("Request Frequency") + 
    ggtitle("Trip Status Frequency Plot : City Trips")
  
# 9. At an overall level, Evening has the highest Demand followed by Early Morning and Night timeslots with Non Availability of Cars being the major reason for Non Completion of Ride during Evening and Night whereas ride being cancelled is the major reason during Early Morning for the same
      
    ggplot(uber, aes(x = Request.timeslot,fill=Status)) + 
    geom_bar(position="stack") + 
    theme(axis.text.x = element_text(angle=60, hjust=1)) + 
    xlab("Request Timeslot") + 
    ylab("Request Frequency") + 
    ggtitle("Request Timeslot Frequency Plot")
  
# 10. For trips originating from Airport, Evening has the highest Demand followed by the Night timeslot with Non Availability of Cars being the major reason for Non Completion of Ride
  
    ggplot(uber_airport, aes(x = Request.timeslot,fill=Status)) + 
    geom_bar(position="stack") + 
    theme(axis.text.x = element_text(angle=60, hjust=1)) + 
    xlab("Request Timeslot") + 
    ylab("Request Frequency") + 
    ggtitle("Request Timeslot Frequency Plot : Airport Trips")
  
# 11. For trips originating from City, Early Morning has the highest Demand followed by Morning timeslot with ride being Cancelled as the major reason for Non Completion of Ride
  
    ggplot(uber_city, aes(x = Request.timeslot,fill=Status)) + 
    geom_bar(position="stack") + 
    theme(axis.text.x = element_text(angle=60, hjust=1)) + 
    xlab("Request Timeslot") + 
    ylab("Request Frequency") + 
    ggtitle("Request Timeslot Frequency Plot : City Trips")
  
# 12. At an overall level, must improve Trip Completion levels for Evening and Night timeslots which have the highest demand 
    
    ggplot(uber, aes(x = Request.timeslot,fill=Status)) + 
    geom_bar(position="fill") + 
    theme(axis.text.x = element_text(angle=60, hjust=1)) + 
    xlab("Request Timeslot") + 
    ylab("Request Frequency") + 
    ggtitle("Request Timeslot Frequency Plot")
    
# 13. For trips originating from Airport, must improve Trip Completion levels for Evening and Night timeslots which have the highest demand
    
    ggplot(uber_airport, aes(x = Request.timeslot,fill=Status)) + 
    geom_bar(position="fill") + 
    theme(axis.text.x = element_text(angle=60, hjust=1)) + 
    xlab("Request Timeslot") + 
    ylab("Request Frequency") + 
    ggtitle("Request Timeslot Frequency Plot : Airport Trips")
    
# 14. For trips originating from City, must improve Trip Completion levels for Early Morning and Morning timeslots which have the highest demand
    
    ggplot(uber_city, aes(x = Request.timeslot,fill=Status)) + 
    geom_bar(position="fill") + 
    theme(axis.text.x = element_text(angle=60, hjust=1)) + 
    xlab("Request Timeslot") + 
    ylab("Request Frequency") + 
    ggtitle("Request Timeslot Frequency Plot : City Trips")  
    
# 15. Supply-Demand Gap for trips originating from Airport, Huge Supply-Demand Gap exists during Evening and Night timeslots majorly because of Non Availability of Cars at the Airport
    
    ggplot(uber_airport_not_completed_trips, aes(x = Request.timeslot,fill=Status)) + 
    geom_bar(position="stack") + 
    theme(axis.text.x = element_text(angle=60, hjust=1)) + 
    xlab("Request Timeslot") + 
    ylab("Request Frequency") + 
    ggtitle("Supply-Demand Gap : Airport Trips")
    
# 16. Supply-Demand Gap for trips originating from City, Huge Supply-Demand Gap exists during Early Morning and Morning timeslots majorly because the rides are being Cancelled by the drivers
    
    ggplot(uber_city_not_completed_trips, aes(x = Request.timeslot,fill=Status)) + 
    geom_bar(position="stack") + 
    theme(axis.text.x = element_text(angle=60, hjust=1)) + 
    xlab("Request Timeslot") + 
    ylab("Request Frequency") + 
    ggtitle("Supply-Demand Gap : City Trips")   
    
# 17. At an Overall level, Non Availability of Cars during Evening & Night timeslot and Cancellation during Early Morning & Morning timeslots seems to be creating the Supply-Demand Gap for Uber
    
    ggplot(uber_not_completed_trips, aes(x = Request.timeslot,fill=Status,group=Status)) + 
    geom_bar(position="stack") + 
    theme(axis.text.x = element_text(angle=60, hjust=1)) + 
    geom_text(stat="count",aes(label=..count..),position = position_stack(vjust = 0.5),size = 2.5) + 
    xlab("Request Timeslot") + 
    ylab("Request Frequency") + 
    ggtitle("Supply-Demand Gap") + 
    facet_wrap(uber_not_completed_trips$Pickup.point~uber_not_completed_trips$Status)
    
    uber_not_completed_trips$Request.timeslot<-factor(uber_not_completed_trips$Request.timeslot,levels = c("Early Morning","Morning","Afternoon","Evening","Night","Late Night"))
    
    plot<-ggplot(uber,aes(x=uber$Request.timeslot,fill=factor(uber$Status),group=factor(uber$Status))) + 
    geom_bar(position = position_dodge(width=NULL)) + 
    geom_text(stat="count",aes(label=..count..),position = position_dodge(width = .99), size = 3,vjust=-.4)
    
    print(plot)
    
    plot + 
    facet_wrap(uber$Pickup.point~uber$Status) + 
    xlab("Pickup Point") + 
    ylab("Frequency of Request") + 
    ggtitle(" Problem analysis ")
    