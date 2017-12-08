library(bigrquery)
project_ID = "coherent-surf-186519"
sql = 'SELECT * FROM[bigquery-public-data:san_francisco.bikeshare_status] where station_id = 70;'
bike_status = query_exec(sql, project = project_ID, max_pages = Inf)
bike_status$date = as.POSIXct(bike_status$time, format = "%Y-%m-%d %H:%M:%S")
bike_status$date = as.Date(bike_status$date, format = "%Y-%m-%d")
bike_status$time2 = as.POSIXct(bike_status$time, format = "%Y-%m-%d %H:%M:%S")
bike_status$time2 = format(bike_status$time2,"%H:%M")
bike_status$time3 = as.POSIXct(bike_status$time, format = "%Y-%m-%d %H:%M:%S")
bike_status$time3 = format(bike_status$time3,"%H")
bike_status = bike_status[order(bike_status$time),]
# I'm creating the time minus 1 hour. The idea is that the amount of availeable bikes at "time -1" is the max of rides that can occor at "time"
bike_status$time3minus1 = as.integer(bike_status$time3)-1
# replaceing -1 (the outcome of 0-1) in 23. 0 is actually midnight and -1 is 11 pm
bike_status$time3minus1 = replace(bike_status$time3minus1, bike_status$time3minus1 == -1, 23)
# create a subset for only the data I need
bike_status.SS = subset(bike_status, select = c(date, time3minus1, bikes_available))
#find what was the minimum number of bikes per time. with this I can know what is the max rides in the next cycle 
bike_status.agg = aggregate(bike_status.SS[,1:2], bike_status.SS[,3], FUN = min)
bike_status.agg = bike_status.agg[order(bike_status.agg$Group.1),]