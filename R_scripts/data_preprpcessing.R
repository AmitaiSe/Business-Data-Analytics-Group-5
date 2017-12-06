#####------ merge full raw data - trips & station locations ------#######
#read raw data
bike_trip_full = read.csv("C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\raw_data\\bikeshare_trips_full.csv", header=TRUE, as.is=TRUE, na.strings=c("NA", ".", ""))
bike_station = read.csv("C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\raw_data\\bikeshare_stations.csv", header=TRUE, as.is=TRUE, na.strings=c("NA", ".", ""))

#merge bike_trip_full & bike_station
bike_merge_by_startID = merge(bike_trip_full,bike_station,by.x = c("start_station_id"),by.y = c("station_id"))
bike_merge_by_startID$name = NULL
colnames(bike_merge_by_startID)[13] = "start_lat"
colnames(bike_merge_by_startID)[14] = "start_lon"
colnames(bike_merge_by_startID)[15] = "start_dockcount"
colnames(bike_merge_by_startID)[16] = "start_landmark"
colnames(bike_merge_by_startID)[17] = "start_insallDate"

bike_merge = merge(bike_merge_by_startID,bike_station,by.x = c("end_station_id"),by.y = c("station_id"))
bike_merge$name = NULL
colnames(bike_merge)[18] = "end_lat"
colnames(bike_merge)[19] = "end_lon"
colnames(bike_merge)[20] = "end_dockcount"
colnames(bike_merge)[21] = "end_landmark"
colnames(bike_merge)[22] = "end_insallDate"

#rearrange bike data frame
bike_merge_originalOrder = bike_merge[order(bike_merge$X), ]
bikes_trips_and_statsLoc = data.frame(bike_merge_originalOrder[c(3,4,5,6,7,2)])
bikes_trips_and_statsLoc = cbind(bikes_trips_and_statsLoc,bike_merge_originalOrder[13:17])
bikes_trips_and_statsLoc = cbind(bikes_trips_and_statsLoc,bike_merge_originalOrder[c(8,9,1,18,19,20,21,22)])
bikes_trips_and_statsLoc = cbind(bikes_trips_and_statsLoc,bike_merge_originalOrder[c(10,11,12)])
#save merged data
write.csv(bikes_trips_and_statsLoc, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\merged_data\\bikes_trips_and_statsLoc_full.csv")
#####------------------------------------------------------------#######

#####------ select from merged dataset ONLY San-Francisco data ------#######
sanFran = bikes_trips_and_statsLoc[bikes_trips_and_statsLoc$start_landmark == "San Francisco", ]
sanFran = sanFran[sanFran$end_landmark == "San Francisco", ]
write.csv(sanFran, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\san_francisco_data\\sanFran_full.csv")

sanFran_station = bike_station[bike_station$landmark== "San Francisco", ]
write.csv(sanFran_station, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\san_francisco_data\\sanFran_stations.csv")
#####------------------------------------------------------------#######

#####------ split to trainSet & testSet------#######
set.seed(7)
sanFran_random_order <- sanFran[sample(nrow(sanFran)), ]
trainSet_size <- floor((nrow(sanFran)/4)*3)
sanFran_trainSet <- sanFran_random_order[1:trainSet_size, ]
sanFran_testSet <- sanFran_random_order[(trainSet_size+1):nrow(sanFran), ]
write.csv(sanFran_trainSet, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\san_francisco_data\\sanFran_train.csv")
write.csv(sanFran_testSet, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\san_francisco_data\\sanFran_test.csv")
#####------------------------------------------#######

#set dates
sanFran_trainSet$start_date = as.POSIXct(sanFran_trainSet$start_date, format = "%Y-%m-%d %H:%M:%S")
sanFran_trainSet$end_date = as.POSIXct(sanFran_trainSet$end_date, format = "%Y-%m-%d %H:%M:%S")

#set duration
sanFran_trainSet$duration_minutes = sanFran_trainSet$duration_sec / 60
sanFran_trainSet$duration_minutes = round(sanFran_trainSet$duration_minutes, 2)

#add columns of day, month, year, weekend, hour
sanFran_trainSet$year = format(sanFran_trainSet$start_date,"%Y")
sanFran_trainSet$Month = format(sanFran_trainSet$start_date,"%B")
sanFran_trainSet$Weekday = weekdays(sanFran_trainSet$start_date)
sanFran_trainSet$dayStatus = ifelse(sanFran_trainSet$Weekday %in% c("Saturday","Sunday"),"Weekend","weekday")
sanFran_trainSet$Hour = format(sanFran_trainSet$start_date,"%H")
write.csv(sanFran_trainSet, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\san_francisco_data\\sanFran_train_extended.csv")

#stations statistics
hist_startStations_weekend  = sort(table(sanFran_trainSet[sanFran_trainSet$dayStatus == "Weekend", ]$start_station_name),decreasing = TRUE)
top_startStations_weekend = hist_startStations_weekend[1:10]

hist_endStations_weekend  = sort(table(sanFran_trainSet[sanFran_trainSet$dayStatus == "Weekend", ]$end_station_name),decreasing = TRUE)
top_endStations_weekend = hist_endStations_weekend[1:10]

hist_startStations_weekday  = sort(table(sanFran_trainSet[sanFran_trainSet$dayStatus == "weekday", ]$start_station_name),decreasing = TRUE)
top_startStations_weekday = hist_startStations_weekday[1:10]

hist_endStations_weekday  = sort(table(sanFran_trainSet[sanFran_trainSet$dayStatus == "weekday", ]$end_station_name),decreasing = TRUE)
top_endStations_weekday = hist_endStations_weekday[1:10]

count_rentals_Weekend = as.data.frame(table(sanFran_trainSet[sanFran_trainSet$dayStatus == "Weekend", ]$start_station_id))
colnames(count_rentals_Weekend) = c("station_id","count_rentals")
rentalsByStations_weekend = merge(sanFran_station,count_rentals_Weekend,by = c("station_id"))

count_returns_Weekend = as.data.frame(table(sanFran_trainSet[sanFran_trainSet$dayStatus == "Weekend", ]$end_station_id))
colnames(count_returns_Weekend) = c("station_id","count_returns")
rentalsByStations_weekend = merge(rentalsByStations_weekend,count_returns_Weekend,by = c("station_id"),all = TRUE)
write.csv(rentalsByStations_weekend, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\data4TableauMap\\rentalsByStations_weekend.csv")

count_rentals_Weekday = as.data.frame(table(sanFran_trainSet[sanFran_trainSet$dayStatus == "weekday", ]$start_station_id))
colnames(count_rentals_Weekday) = c("station_id","count_rentals")
rentalsByStations_weekday = merge(sanFran_station,count_rentals_Weekday,by = c("station_id"))

count_returns_Weekday = as.data.frame(table(sanFran_trainSet[sanFran_trainSet$dayStatus == "weekday", ]$end_station_id))
colnames(count_returns_Weekday) = c("station_id","count_returns")
rentalsByStations_weekday = merge(rentalsByStations_weekday,count_returns_Weekday,by = c("station_id"),all = TRUE)
write.csv(rentalsByStations_weekday, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\data4TableauMap\\rentalsByStations_weekday.csv")


#START station popularity (for dsuscriber)
start_popStat_sorted_subscriber  = sort(table(sanFran_trainSet[sanFran_trainSet$subscriber_type == "Subscriber", ]$start_station_name),decreasing = TRUE)
start_topTenPopStat_suscriber = start_popStat_sorted_subscriber[1:10]
#START station popularity (for Customer)
start_popStat_sorted_customer  = sort(table(sanFran_trainSet[sanFran_trainSet$subscriber_type == "Customer", ]$start_station_name),decreasing = TRUE)
start_topTenPopStat_customer = start_popStat_sorted_customer[1:10]

#END station popularity (in general)
end_popStat_sorted  = sort(table(sanFran_trainSet$end_station_name),decreasing = TRUE)
end_topTenPopStat = end_popStat_sorted[1:10]
#END station popularity (for dsuscriber)
end_popStat_sorted_suscriber  = sort(table(sanFran_trainSet[sanFran_trainSet$subscriber_type == "Subscriber", ]$end_station_name),decreasing = TRUE)
end_topTenPopStat_suscriber = end_popStat_sorted_suscriber[1:10]
#END station popularity (for Customer)
end_popStat_sorted_customer  = sort(table(sanFran_trainSet[sanFran_trainSet$subscriber_type == "Customer", ]$end_station_name),decreasing = TRUE)
end_topTenPopStat_customer = end_popStat_sorted_customer[1:10]

#day popularity - trips per day
tripsPerDay = table(sanFran_trainSet$Weekday)
barplot(tripsPerDay)
#day popularity - trips per day for Subscriber
tripsPerDay_subscriber = table(sanFran_trainSet[sanFran_trainSet$subscriber_type == "Subscriber", ]$Weekday)
barplot(tripsPerDay_subscriber)
#day popularity - trips per day for Customer
tripsPerDay_subscriber = table(sanFran_trainSet[sanFran_trainSet$subscriber_type == "Customer", ]$Weekday)
barplot(tripsPerDay_subscriber)

#duration per day
avgRidingTime_ForEachDay = tapply(sanFran_trainSet$duration_minutes, sanFran_trainSet$Weekday, mean, na.rm = TRUE)
