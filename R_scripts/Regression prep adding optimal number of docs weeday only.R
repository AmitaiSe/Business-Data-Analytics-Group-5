# pull the data from the data set and add the hour column for later grouping needs
bike_trip_temp = read.csv("C:/Users/amitai/Dropbox (Personal)/MBA/Business analysis/Bikes Project/dataSets Yarden/san_francisco_data/sanFran_train_extended.csv", header=TRUE, as.is=TRUE, na.strings=c("NA", ".", ""))
# set columns for time and date
bike_trip_temp$start_astime = as.POSIXct(bike_trip_temp$start_date, format = "%Y-%m-%d %H:%M:%S")
bike_trip_temp$start_astime = format(bike_trip_temp$start_astime,"%H")
bike_trip_temp$start_asdate = as.POSIXct(bike_trip_temp$start_date, format = "%Y-%m-%d %H:%M:%S")
bike_trip_temp$start_asdate = as.Date(bike_trip_temp$start_asdate, format = "%Y-%m-%d")
# set a colums if weekend
bike_trip_temp$start.weekday = weekdays(as.Date(bike_trip_temp$start_asdate,"%Y-%m-%d"))
bike_trip_temp$start.weekday = as.factor(bike_trip_temp$start.weekday)
bike_trip_temp$start.weekday = factor(bike_trip_temp$start.weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered=TRUE)
bike_trip_temp$start_is_weekend = ifelse(as.integer(bike_trip_temp$start.weekday) == 6 | as.integer(bike_trip_temp$start.weekday) == 7, TRUE, FALSE)
# Subset of what I want to gorup by
bike_trip_temp.SS = subset(bike_trip_temp, select = c(start_station_id, start_asdate, start_astime, trip_id, start_is_weekend))
bike_trip_temp.SS$start_is_weekend = as.character(bike_trip_temp.SS$start_is_weekend)
bike_trip_temp.SS = bike_trip_temp.SS[bike_trip_temp.SS$start_is_weekend=="FALSE",]
groupby1 = list(bike_trip_temp.SS$start_station_id, bike_trip_temp.SS$start_asdate, bike_trip_temp.SS$start_astime)
# first aggrigation to count number of trips per station, date and time
bike_trip_temp.agg = aggregate(bike_trip_temp.SS$trip_id, by=groupby1, FUN=NROW)
bike_trip_temp.agg = bike_trip_temp.agg[order(bike_trip_temp.agg$Group.1, bike_trip_temp.agg$Group.2, bike_trip_temp.agg$Group.3),]
colnames(bike_trip_temp.agg) = c("Station_ID", "Start_date", "Start_time", "Count_of_rides")
# find the mean and sd per station
groupby2 = list(bike_trip_temp.agg$Station_ID)
bike_trip_temp.agg2 = aggregate(bike_trip_temp.agg$Count_of_rides, by=groupby2, FUN=mean)
bike_trip_temp.agg3 = aggregate(bike_trip_temp.agg$Count_of_rides, by=groupby2, FUN=sd)
colnames(bike_trip_temp.agg2) = c("Station_ID", "mean_number_rides")
colnames(bike_trip_temp.agg3) = c("Station_ID", "sd_number_rides")
# merge together two data frames
bike_trip_temp.agg4 = merge(bike_trip_temp.agg2, bike_trip_temp.agg3, by=1)
# The Optimal number of docs is the percentile we define
# bike_trips
percentile = 0.95
bike_trip_temp.agg4$optimal_docs_number = qnorm(percentile, bike_trip_temp.agg4$mean_number_rides, bike_trip_temp.agg4$sd_number_rides)
# Add the sanFran_stations data set in order to unify all
sanFran_stations = read.csv("C:/Users/amitai/Dropbox (Personal)/MBA/Business analysis/Bikes Project/dataSets Yarden/san_francisco_data/sanFran_stations.csv", header=TRUE, as.is=TRUE, na.strings=c("NA", ".", ""))
sanFran_stations_updated = sanFran_stations[,2:8]
sanFran_stations_updated = merge(sanFran_stations_updated, bike_trip_temp.agg4, by=1)
write.csv(sanFran_stations_updated, file="sanFran_stations_updated2.csv", quote=FALSE, na="NA", row.names = FALSE)
