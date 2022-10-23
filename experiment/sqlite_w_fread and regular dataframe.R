
library(RSQLite)
library(DBI)
library(sqldf)
library(data.table)




#### Appending Data Into the Database with fread and sqlite ####

setwd("/Users/dehaay/Desktop/BikeShare Project/raw_data/sqldatabase") ##Sets directory
db <- dbConnect(SQLite(), dbname="bikesharedata_db_fread.sqlite") ## will make a database, if not present


timer = Sys.time()

dbWriteTable(conn=db, name="biketransactions",
             value=na.omit(fread("/Users/dehaay/Desktop/BikeShare Project/raw_data/test/202204-citibike-tripdata.csv"))
             , row.names=FALSE, header=TRUE, append = TRUE)

print(Sys.time() - timer )

dbGetQuery(db,"SELECT 
           COUNT(*)
           FROM biketransactions")

dbWriteTable(conn=db, name="biketransactions",
             value=na.omit(fread("/Users/dehaay/Desktop/BikeShare Project/raw_data/test/202205-citibike-tripdata.csv"))
             , row.names=FALSE, header=TRUE, append = TRUE)

print(Sys.time() - timer )

dbGetQuery(db,"SELECT 
           COUNT(*)
           FROM biketransactions")




#### TEST APPENDING DATA IN A DATAFRAME
timer = Sys.time()
a <- na.omit(fread("/Users/dehaay/Desktop/BikeShare Project/raw_data/test/202204-citibike-tripdata.csv"))
b <- na.omit(fread("/Users/dehaay/Desktop/BikeShare Project/raw_data/test/202205-citibike-tripdata.csv"))
c <- rbind(a,b )
print(Sys.time() - timer )


c$started_at



#### DATE FACTORING ####
dbGetQuery(db,"SELECT 
           *
           FROM biketransactions")
timer = Sys.time()
dbGetQuery(db,"SELECT 
           strftime('%d', started_at),strftime('%m', started_at),strftime('%Y', started_at)
           FROM biketransactions")


timer = Sys.time()


timer = Sys.time()
dbGetQuery(db,"SELECT strftime('%d', datetime(started_at, 'unixepoch', 'utc','-4 hours')),
           strftime('%m', datetime(started_at, 'unixepoch', 'utc','-4 hours')),
           strftime('%Y', datetime(started_at, 'unixepoch', 'utc','-4 hours')),
           strftime('%w', datetime(started_at, 'unixepoch', 'utc','-4 hours')), strftime('%H', datetime(started_at, 'unixepoch', 'utc','-4 hours'))
           FROM biketransactions")
print(Sys.time() - timer )

dbGetQuery(db,"SELECT datetime(started_at, 'unixepoch', 'utc','-4 hours') FROM biketransactions limit 3")
dbGetQuery(db,"SELECT datetime(started_at, 'unixepoch', 'localtime') FROM biketransactions limit 3")





timer = Sys.time()
as.numeric(format(ymd_hms(c$started_at), format = "%d"))
as.numeric(format(ymd_hms(c$started_at), format = "%m"))
as.numeric(format(ymd_hms(c$started_at), format = "%H"))
as.numeric(format(ymd_hms(c$started_at), format = "%Y"))

lubridate::wday(c$started_at, week_start = 1 )
print(Sys.time() - timer )

data$hour_interval <- as.numeric(format(ymd_hms(data$started_at), format = "%H"))
data$month <- as.numeric(format(ymd_hms(data$started_at), format = "%m"))
data$day <- as.numeric(format(ymd_hms(data$started_at), format = "%d"))
data$year <- as.numeric(format(ymd_hms(data$started_at), format = "%Y"))



