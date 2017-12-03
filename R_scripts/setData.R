#read data
bike_trip = read.csv("C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\san_francisco_data\\bikeshare_sanFran.csv", header=TRUE, as.is=TRUE, na.strings=c("NA", ".", ""))
bike_station_sanFran = read.csv("C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\san_francisco_data\\bike_station_freq_sanFran.csv", header=TRUE, as.is=TRUE, na.strings=c("NA", ".", ""))
bike_available = read.csv("C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\san_francisco_data\\bike_available.csv", header=TRUE, as.is=TRUE, na.strings=c("NA", ".", ""))

#set dates
#bike_trip$start_date = as.POSIXct(bike_trip$start_date, format = "%Y-%m-%d %H:%M:%S")
#bike_trip$end_date = as.POSIXct(bike_trip$end_date, format = "%Y-%m-%d %H:%M:%S")
bike_trip$start_date = as.POSIXct(bike_trip$start_date, format = "%d-%m-%y %H:%M")
bike_trip$end_date = as.POSIXct(bike_trip$end_date, format = "%d-%m-%y %H:%M")

#set duration
bike_trip$duration_minutes = bike_trip$duration_sec / 60
bike_trip$duration_minutes = round(bike_trip$duration_minutes, 2)

#add columns of day, month, year, weekend
bike_trip$year = format(bike_trip$start_date,"%Y")
bike_trip$Month = format(bike_trip$start_date,"%B")
bike_trip$Weekday = weekdays(bike_trip$start_date)
bike_trip$dayStatus = ifelse(bike_trip$Weekday %in% c("Saturday","Sunday"),"Weekend","weekday")
bike_trip$Hour = format(bike_trip$start_date,"%H")

#START station popularity (in general)
start_popStat_sorted  = sort(table(bike_trip$start_station_name),decreasing = TRUE)
start_topTenPopStat = start_popStat_sorted[1:10]

#START station popularity (in weekend)
start_popStat_sorted_weekend  = sort(table(bike_trip[bike_trip$dayStatus == "Weekend", ]$start_station_name),decreasing = TRUE)
start_topTenPopStat_weekend = start_popStat_sorted_weekend[1:10]
start_table_byWeekend = as.data.frame(table(bike_trip[bike_trip$dayStatus == "Weekend", ]$start_station_id))
colnames(start_table_byWeekend) = c("station_id","freq_AS_start_station_weekend")
stationFreq_weekend = merge(bike_station_sanFran,start_table_byWeekend,by = c("station_id"))
#END station popularity (in weekend)
end_popStat_sorted_weekend  = sort(table(bike_trip[bike_trip$dayStatus == "Weekend", ]$end_station_name),decreasing = TRUE)
end_topTenPopStat_weekend = end_popStat_sorted_weekend[1:10]
end_table_byWeekend = as.data.frame(table(bike_trip[bike_trip$dayStatus == "Weekend", ]$end_station_id))
colnames(end_table_byWeekend) = c("station_id","freq_AS_end_station_weekend")
stationFreq_weekend = merge(stationFreq_weekend,end_table_byWeekend,by = c("station_id"),all = TRUE)
write.csv(stationFreq_weekend, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\data4Tableau\\stationFreq_weekend.csv")

#START station popularity (in weekday)
start_popStat_sorted_weekday  = sort(table(bike_trip[bike_trip$dayStatus == "weekday", ]$start_station_name),decreasing = TRUE)
start_topTenPopStat_weekday = start_popStat_sorted_weekday[1:10]
start_table_byWeekday = as.data.frame(table(bike_trip[bike_trip$dayStatus == "weekday", ]$start_station_id))
colnames(start_table_byWeekday) = c("station_id","freq_AS_start_station_weekday")
stationFreq_weekday = merge(bike_station_sanFran,start_table_byWeekday,by = c("station_id"))
#END station popularity (in weekday)
end_popStat_sorted_weekday  = sort(table(bike_trip[bike_trip$dayStatus == "weekday", ]$end_station_name),decreasing = TRUE)
end_topTenPopStat_weekday = end_popStat_sorted_weekday[1:10]
end_table_byWeekday = as.data.frame(table(bike_trip[bike_trip$dayStatus == "weekday", ]$end_station_id))
colnames(end_table_byWeekday) = c("station_id","freq_AS_end_station_weekday")
stationFreq_weekday = merge(stationFreq_weekday,end_table_byWeekday,by = c("station_id"),all = TRUE)
write.csv(stationFreq_weekend, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\data4Tableau\\stationFreq_weekday.csv")

#START station popularity (for dsuscriber)
start_popStat_sorted_subscriber  = sort(table(bike_trip[bike_trip$subscriber_type == "Subscriber", ]$start_station_name),decreasing = TRUE)
start_topTenPopStat_suscriber = start_popStat_sorted_subscriber[1:10]
#START station popularity (for Customer)
start_popStat_sorted_customer  = sort(table(bike_trip[bike_trip$subscriber_type == "Customer", ]$start_station_name),decreasing = TRUE)
start_topTenPopStat_customer = start_popStat_sorted_customer[1:10]

#END station popularity (in general)
end_popStat_sorted  = sort(table(bike_trip$end_station_name),decreasing = TRUE)
end_topTenPopStat = end_popStat_sorted[1:10]
#END station popularity (for dsuscriber)
end_popStat_sorted_suscriber  = sort(table(bike_trip[bike_trip$subscriber_type == "Subscriber", ]$end_station_name),decreasing = TRUE)
end_topTenPopStat_suscriber = end_popStat_sorted_suscriber[1:10]
#END station popularity (for Customer)
end_popStat_sorted_customer  = sort(table(bike_trip[bike_trip$subscriber_type == "Customer", ]$end_station_name),decreasing = TRUE)
end_topTenPopStat_customer = end_popStat_sorted_customer[1:10]

#day popularity - trips per day
tripsPerDay = table(bike_trip$Weekday)
barplot(tripsPerDay)
#day popularity - trips per day for Subscriber
tripsPerDay_subscriber = table(bike_trip[bike_trip$subscriber_type == "Subscriber", ]$Weekday)
barplot(tripsPerDay_subscriber)
#day popularity - trips per day for Customer
tripsPerDay_subscriber = table(bike_trip[bike_trip$subscriber_type == "Customer", ]$Weekday)
barplot(tripsPerDay_subscriber)

#duration per day
avgRidingTime_ForEachDay = tapply(bike_trip$duration_minutes, bike_trip$Weekday, mean, na.rm = TRUE)
