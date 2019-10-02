
#reads in driver_ids data
driver_ids <- read.csv("driver_ids.csv")

#reads in ride data
ride_ids <- read.csv("ride_ids.csv")

#reads in timestamps data
ride_timestamps <- read.csv("ride_timestamps.csv")

#separates the event timestamps in ride_timestamps into individual columns
ride_timestamps_mod <-spread(ride_timestamps,event,timestamp)

lyft_data <- full_join(full_join(ride_ids, ride_timestamps_mod),driver_ids)

lyft_data[,c("driver_id")] <- as.factor(lyft_data$driver_id)

#rearranges events to be in order
lyft_data <- lyft_data[c(2,11,1,3:5,10,6,7,9,8)]

#changes the event times columns to POSIX datatype
for(i in 7:11){
  
 lyft_data[,i] <- as.POSIXct(lyft_data[,i],format="%Y-%m-%d %H:%M:%S", tz="PST8PDT")
  
}

#recalculates the duration for rides that are missing that data
no_duration <- with(lyft_data,is.na(ride_duration)==TRUE)
lyft_data[no_duration,c("ride_duration")] <- lyft_data[no_duration,c("dropped_off_at")] - lyft_data[no_duration,c("picked_up_at")]

#shows summary of data
summary(lyft_data)

#calculates a summary of ride distance data omitting invalid data
summary(lyft_data[which(lyft_data[,c("ride_distance")]>0),c("ride_distance")])

#shows summary of onboarding 
daily_onboarding <- fct_count(driver_ids$driver_onboard_date)
mean_daily_onboarding <- mean(daily_onboarding$n)

#calculates the price of ride
#function takes in a list from the dataframe
calclyft <- function(ride){
  
  #changes distance from being in meters to miles
  distance <- as.numeric(ride["ride_distance"])/1609.34
  
  #changes duration of ride from seconds to minutes
  duration <- as.numeric(ride["ride_duration"])/60
  
  #(base + miles*1.15 + duration*0.22 + service fee)*prime time percentage
  price <- (2 + distance*1.15 + duration*0.22 + 1.75)*(1+as.numeric(ride["ride_prime_time"])/100)
    
  #if less than 5, return 5
  if(price<5){
    price=5
  }
  
  #if greater than 400, return 400
  if(price>400){
    price=400
  }
  
  return (price)
}

#defines a vector to hold driver revenues
driver_rev <- numeric()
num_rides <- numeric()
ride_dur <- numeric()
percent_prime <- numeric()

#pulls rides for each driver
for(i in 1:length(levels(lyft_data$driver_id))){
  
  #data frame of each driver's rides
  driver_rides <- lyft_data[with(lyft_data,driver_id==levels(lyft_data$driver_id)[i]),]
  
  #eliminates rides with invalid distances and drops NA data
  driver_rides <- driver_rides[with(driver_rides,ride_distance>0),]
  driver_rides <- driver_rides[is.na(driver_rides$ride_duration)!=TRUE,]
  driver_rides <- driver_rides[is.na(driver_rides$ride_prime_time)!=TRUE,]
  
  rev<-0
  dur<-0
  prime<-0
  
  #defines a total revenue for driver
  if(nrow(driver_rides)>0){
    rev <- sum(apply(driver_rides, 1, calclyft))
    dur <- mean(driver_rides$ride_duration)
    prime <- ((length(which(driver_rides$ride_prime_time!=0)))/nrow(driver_rides))*100
  }
  #adds to vector
  driver_rev[i]<-rev
  num_rides[i]<-nrow(driver_rides)
  ride_dur[i] <- dur
  percent_prime[i] <- prime
  
}


driver_ids_complete <- data.frame(levels(lyft_data$driver_id),"Revenue"=driver_rev,"Total Rides"=as.numeric(num_rides),"Percent Prime"=percent_prime)

#Answer for 2b
# Determine driver_lifetimes
lyft_data2 <- lyft_data[which(is.na(lyft_data$driver_id)==FALSE),]

driver_lifetimes <- lyft_data2 %>%  
   select(driver_id:ride_prime_time, -(requested_at:picked_up_at), dropped_off_at, driver_onboard_date) %>%
   group_by(driver_id) %>%
   arrange(dropped_off_at) %>%
   slice(n()) %>%
   mutate(driver_lifetime = as.Date(dropped_off_at) - as.Date(driver_onboard_date)) %>%
   mutate(driver_lifetime=replace(driver_lifetime, is.na(driver_lifetime), 0)) %>%
   select(driver_id, driver_onboard_date, dropped_off_at, driver_lifetime)

# Calculate average driver lifetime
mean(driver_lifetimes$driver_lifetime)

# Total number of rides in a lifetime
num_rides <- lyft_data2 %>%
    group_by(driver_id) %>%
    summarise(n = n_distinct(ride_id))

# Q2b dataframe for the graph
qtwob_graph <- left_join(driver_lifetimes, num_rides) %>%
   mutate(n=replace(n, is.na(n), 0))

# Q2b scatter plot
qtwob_graph %>%
  filter(driver_lifetime > 0 %in% n) %>%
  ggplot(aes(x=driver_lifetime, y=n)) + 
     geom_point() +
     xlab("Driver Lifetime") + 
     ylab("Number of Rides in a Lifetime") +
     ylim(c(0,800))

# Percent of riders likely to drive frequently past 27 days.
twenty_seven_days <- driver_lifetimes %>%
   filter(driver_lifetime > 27) %>%
   nrow()
freq_drivers <- driver_lifetimes %>%
   filter(driver_lifetime > 27, n>100) %>%
   nrow()
freq_drivers/twenty_seven_days

#Answer for 2c
# scatter plot of wait time vs ride length
ggplot(lyft_data, aes(x = (arrived_at - accepted_at)/60,
                  y = (dropped_off_at - accepted_at)/60)) + 
       geom_point() +
       xlab("Wait Time") +
       ylab("Ride Length") +
       xlim(c(0, 40)) + 
       ylim(c(0,150))

#puts together lifetime and other data
driver_lifetimes$driver_lifetime <- as.numeric(driver_lifetimes$driver_lifetime)

driver_ids_complete <- cbind(driver_ids_complete,"Lifetime"=driver_lifetimes$driver_lifetime)

#drops drivers that are missing dates for drives and thus have an invalid LTV
complete_rides <- driver_ids_complete[which(!(driver_ids_complete$Total.Rides>0 & driver_ids_complete$Lifetime==0)),]

#calculates LTV and rides/day for these 
complete_rides <- mutate(complete_rides, Rides_Day=Total.Rides/Lifetime)

complete_rides <- mutate(complete_rides,"LTV"=Revenue/Lifetime)

averagelv <- mean(complete_rides$LTV)

mean_rides_pd <- mean(driver_ids_complete$Total.Rides)/mean(complete_rides$Lifetime)

#graphs
ggplot(drop_na(complete_rides), aes(LTV, Rides_Day)) + geom_point() + xlab("Lifetime Value") + ylab("Rides Per Day")

ggplot(drop_na(complete_rides), aes(LTV, Rides_Day)) + geom_point() + xlim(0,250) + ylim(0,18) + geom_smooth(model=lm) + xlab("Lifetime Value") + ylab("Rides Per Day")

ggplot(drop_na(complete_rides), aes(Lifetime, Rides_Day)) + geom_point() + xlab("Driver Lifetime") + ylab("Rides Per Day")

ggplot(drop_na(complete_rides), aes(LTV, Percent.Prime)) + geom_point() + xlab("Lifetime Value") + ylab("Percent of Total Rides With Prime")

cor(complete_rides[!is.na(complete_rides$LTV),]$Percent.Prime,complete_rides[!is.na(complete_rides$LTV),]$LTV)

ggplot(drop_na(complete_rides), aes(LTV,Total.Rides)) + geom_point() + xlab("Lifetime Value") + ylab("Total Rides Over Lifetime") + geom_smooth(model=lm)

#examines driver work hours and prime
by_hour <- as.factor(format(lyft_data$requested_at, format="%H"))

hr_freq <- fct_count(by_hour)
colnames(hr_freq) <- c("Hour","Frequency")

by_hour <- data.frame(by_hour,lyft_data$ride_prime_time)
colnames(by_hour) <- c("Hour", "Prime Time")

by_hour <- drop_na(by_hour)

ggplot(hr_freq[1:24,],aes(x=Hour,y=Frequency),na.rm=TRUE)+geom_bar(stat="identity")

count(lyft_data, ride_prime_time)

by_hour[,3]<-with(by_hour,by_hour[,2]>0)

by_hour_prime <- count(by_hour,Hour,V3)

ggplot(by_hour_prime, aes(x = Hour, y = n, fill = V3)) +     geom_bar(stat = "identity") + xlab("\nHour") +    ylab("Number of Rides\n") + guides() + scale_fill_discrete(name = "Primetime Status",
labels = c("Not Prime", "Prime")) + theme_bw()
