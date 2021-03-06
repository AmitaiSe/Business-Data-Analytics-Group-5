install.packages("bigrquery")
library(bigrquery)
project_ID = "mybikeproject-186518"

sql = 'SELECT trip_id,duration_sec,start_date,start_station_name,start_station_id,end_date,end_station_name,end_station_id,bike_number,zip_code,subscriber_type FROM[bigquery-public-data:san_francisco.bikeshare_trips] LIMIT 1000000;'
data = query_exec(sql, project = project_ID, max_pages = Inf)
write.csv(data, file = "C:\\Users\\User\\Dropbox\\IDC\\Courses\\BusinessAnalytics\\project\\bike\\dataSets\\raw_data\\bikeshare_trips_full.csv")
