library(lubridate)
library(RPostgreSQL)

options(stringsAsFactors=FALSE)

is.blank <- function(x, false.triggers=FALSE)
{
  return(is.null(x) || length(x) == 0 || all(is.na(x)) || all(x=="") || (false.triggers && all(!x)))
}

g.sp.name <- c(short="RSF", long="Ratio Spread Forward")

g.ccy <- data.frame()
g.ccy <- rbind(g.ccy, data.frame(base="GBP", quote="USD", bname="£", qname="$", conv=TRUE, depo="USD", slip=0.0010, spread=1.2, digits=5, s.digits=5, b.digits=4, lot=1, low=0.75, high=2.50, quantile=0.01, exp.time="15:00:00"))
g.ccy <- rbind(g.ccy, data.frame(base="GBP", quote="EUR", bname="£", qname="€", conv=FALSE, depo="EUR", slip=0.0010, spread=1.5, digits=5, s.digits=5, b.digits=4, lot=1, low=0.75, high=2.00, quantile=0.01, exp.time="15:00:00"))
g.ccy <- rbind(g.ccy, data.frame(base="EUR", quote="USD", bname="€", qname="$", conv=TRUE, depo="USD", slip=0.0008, spread=1.0, digits=5, s.digits=5, b.digits=4, lot=1, low=0.60, high=1.60, quantile=0.01, exp.time="15:00:00"))
g.ccy <- rbind(g.ccy, data.frame(base="EUR", quote="GBP", bname="€", qname="£", conv=TRUE, depo="EUR", slip=0.0008, spread=1.5, digits=5, s.digits=5, b.digits=4, lot=1, low=0.60, high=1.60, quantile=0.01, exp.time="15:00:00"))
g.ccy$name <- paste(g.ccy$base, g.ccy$quote, sep="/")
g.ccy$sname <- paste(g.ccy$base, g.ccy$quote, sep="")
g.ccy$cname <- g.ccy$sname
g.ccy$inv <- paste(g.ccy$quote, g.ccy$base, sep="")
g.ccy$cname[!g.ccy$conv] <- g.ccy$inv[!g.ccy$conv]
g.ccy$inv <- NULL
g.ccy$step <- 1/(10^g.ccy$digits)
g.ccy$s.step <- 1/(10^g.ccy$s.digits)
g.ccy$b.step <- 1/(10^g.ccy$b.digits)

pg <- list(dbname="postgres",
           host="10.0.0.44",
           port=5432,
           user="postgres",
           password="postgres")

 
res <- try({
  drv <- dbDriver("PostgreSQL")
  pg.con <- dbConnect(drv, dbname=pg$dbname, host=pg$host, port=pg$port, user=pg$user, password=pg$password)
  df <- dbReadTable(pg.con, "spt")
  dbDisconnect(pg.con)
  dbUnloadDriver(drv)
})
spot.ref <- 0
reduction <- NULL
if(class(res) == "try-error") spot.ref <- NULL
if(!is.null(spot.ref) & nrow(df) < 1) spot.ref <- NULL else df <- df[df$ccy == g.ccy$cname[1], ]
if(!is.null(spot.ref) & nrow(df) != 1) spot.ref <- NULL
if(!is.null(spot.ref))
{
  spot.ref <- (df$bid + df$ask)/2
  if(!g.ccy$conv[1]) spot.ref <- 1/spot.ref
  spot.ref <- round(spot.ref, g.ccy$s.digits[1])
  reduction <- round(spot.ref * 1.05, g.ccy$s.digits[1])
}


param <- list()
param$ccy <- g.ccy$name[1]
param$client.buys <- g.ccy$quote[1]
param$notional.ccy <- g.ccy$quote[1]
param$expiry <- Sys.Date() %m+% months(1:6)
param$notional <- rep(100000, 6)
param$ratio <- rep(200000, 6)
param$strike <- rep(spot.ref, 6)
param$reduction <- rep(reduction, 6)
param$spot.ref <- spot.ref
