bike_trip = read.csv("C:/Users/amitai/Dropbox (Personal)/MBA/Business analysis/Project/bikeshare_trips_full.csv", header=TRUE, as.is=TRUE, na.strings=c("NA", ".", ""))
# I'm setting the train and test groups with different names than what Gili did so it won't overide in case I have them both
set.seed(111)
group <- floor((nrow(bike_trip)/4)*3)
temp <- bike_trip[sample(nrow(bike_trip)), ]
bike_trip.train <- temp[1:group, ]
bike_trip.test <- temp[(group+1):nrow(bike_trip), ]
# converting the timestamp into dates and to hour of day (separating start and end times)
bike_trip.train$start_asdate = as.POSIXct(bike_trip.train$start_date, format = "%Y-%m-%d %H:%M:%S")
bike_trip.train$start_asdate = as.Date(bike_trip.train$start_asdate, format = "%Y-%m-%d")
bike_trip.train$end_asdate = as.POSIXct(bike_trip.train$end_date, format = "%Y-%m-%d %H:%M:%S")
bike_trip.train$end_asdate = as.Date(bike_trip.train$end_asdate, format = "%Y-%m-%d")
bike_trip.train$start_astime = as.POSIXct(bike_trip.train$start_date, format = "%Y-%m-%d %H:%M:%S")
bike_trip.train$start_astime = format(bike_trip.train$start_astime,"%H")
bike_trip.train$end_astime = as.POSIXct(bike_trip.train$end_date, format = "%Y-%m-%d %H:%M:%S")
bike_trip.train$end_astime = format(bike_trip.train$end_astime,"%H")
# converting seconds to minutes in duration of ride
bike_trip.train$duration_min = bike_trip.train$duration_sec / 60
bike_trip.train$duration_min = round(bike_trip.train$duration_min, 2)
# setting the month of the year per observation
bike_trip.train$month_year = as.POSIXct(bike_trip.train$start_date, format = "%Y-%m-%d %H:%M:%S")
bike_trip.train$month_year = format(bike_trip.train$month_year,"%m-%Y")
# adding column to indicate the day of week of the observation
bike_trip.train$start.weekday = weekdays(as.Date(bike_trip.train$start_asdate,"%Y-%m-%d"))
bike_trip.train$end.weekday = weekdays(as.Date(bike_trip.train$end_asdate,"%Y-%m-%d"))
bike_trip.train$start.weekday = as.factor(bike_trip.train$start.weekday)
bike_trip.train$start.weekday = factor(bike_trip.train$start.weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered=TRUE)
bike_trip.train$end.weekday = as.factor(bike_trip.train$end.weekday)
bike_trip.train$end.weekday = factor(bike_trip.train$end.weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered=TRUE)
# after setting day of week as factor and ordering it I can seperate into weekend / weekday by the number of the day
bike_trip.train$start_is_weekend = ifelse(as.integer(bike_trip.train$start.weekday) == 6 | as.integer(bike_trip.train$start.weekday) == 7, TRUE, FALSE)
bike_trip.train$end_is_weekend = ifelse(as.integer(bike_trip.train$end.weekday) == 6 | as.integer(bike_trip.train$end.weekday) == 7, TRUE, FALSE)
str(bike_trip.train)
summary(bike_trip.train)
colSums(is.na(bike_trip.train))