

library(RPostgreSQL)

pg <- list(dbname="postgres",
           host="10.0.0.44",
           port=5432,
           user="postgres",
           password="postgres")


drv <- dbDriver("PostgreSQL")
pg.con <- dbConnect(drv, dbname=pg$dbname, host=pg$host, port=pg$port, user=pg$user, password=pg$password)
dbReadTable(pg.con, "spt")
sts <- dbDisconnect(pg.con)
sts <- dbUnloadDriver(drv)
myTable <- dbReadTable(con, c("tmp","test_tbl"))


con <- dbConnect(drv)
