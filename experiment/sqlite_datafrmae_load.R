
library(RSQLite)
library(DBI)
library(sqldf)
library(data.table)



#### Appending Data Into the Database ####

setwd("/Users/dehaay/Desktop/BikeShare Project/raw_data/sqldatabase") ##Sets directory
db <- dbConnect(SQLite(), dbname="bikesharedata_db.sqlite") ## will make a database, if not present


timer = Sys.time()

dbWriteTable(conn=db, name="biketransactions",
             value="/Users/dehaay/Desktop/BikeShare Project/raw_data/test/202204-citibike-tripdata.csv"
             , row.names=FALSE, header=TRUE, append = TRUE)

print(Sys.time() - timer )

dbGetQuery(db,"SELECT 
           COUNT(*)
           FROM biketransactions")

dbWriteTable(conn=db, name="biketransactions",
             value="/Users/dehaay/Desktop/BikeShare Project/raw_data/test/202205-citibike-tripdata.csv"
             , row.names=FALSE, header=TRUE, append = TRUE)

print(Sys.time() - timer )

dbGetQuery(db,"SELECT 
           COUNT(*)
           FROM biketransactions")




dbGetQuery(db,"SELECT started_at FROM biketransactions")


timer = Sys.time()
dbGetQuery(db,"SELECT strftime('%d', started_at),
           strftime('%m', started_at),
           strftime('%Y', started_at), strftime('%w', started_at), strftime('%H', started_at) FROM biketransactions")


print(Sys.time() - timer )
