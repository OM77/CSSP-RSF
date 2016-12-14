

library(RPostgreSQL)

pg <- list(dbname="postgres",
           host="10.0.0.44",
           port=5432,
           user="postgres",
           password="postgres")


drv <- dbDriver("PostgreSQL")
pg.con <- dbConnect(drv, dbname=pg$dbname, host=pg$host, port=pg$port, user=pg$user, password=pg$password)
df <- dbReadTable(pg.con, "frd")
sts <- dbDisconnect(pg.con)
sts <- dbUnloadDriver(drv)

myTable <- dbReadTable(con, c("tmp","test_tbl"))


con <- dbConnect(drv)


frd <- df[df$ccy == "GBPUSD", ]


ccy.pair = "GBPEUR"
client.buy = "EUR"
notional.ccy = "EUR"
spp = data.frame(exp.date=c("2017-03-14", "2017-06-14"), notional=c(100000, 100000), ratio=c(200000, 200000), strike=c(1.2000, 1.2000), reduction=c(1.15, 1.15))
spot.ref = 1.1927
date.time.ref = "2016-12-14 16:36:00"



FwdIntrpl <- function(
  spot = 0,
  t.days = 45,
  quote = c("mid", "bid", "ask")[1],
  frd = data.frame(),
  method = c("linear", "fmm", "natural", "hyman")[1]
)
{
  x <- frd$days_to_mty
  y <- switch(quote, bid=frd$bid, ask=frd$ask, mid=(frd$bid+frd$ask)/2)
  if(is.blank(y))
  {
    msg <- paste(format(Sys.time()), "ERROR FwdIntrpl", "switch", "Check quote", quote, sep=", ")
    ret <- list(fwd=NULL, err=1, msg=msg)
    return(ret)
  }
  if(method == "linear")
  {
    if(class(try({
      fwd <- approx(x=x, y=y, xout=t.days, method="linear")$y
    }, silent=TRUE)) == "try-error")
    {
      msg <- paste(format(Sys.time()), "ERROR FwdIntrpl", "approx", paste(geterrmessage(), collapse=", "), sep=", ")
      ret <- list(fwd=NULL, err=2, msg=msg)
      return(ret)
    } else {
      fwd <- spot + fwd/(10^frd$fwd_scale[1])
      msg <- paste(format(Sys.time()), "OK FwdIntrpl", t.days, fwd, sep=", ")
      ret <- list(fwd=fwd, err=0, msg=msg)
      return(ret)
    }
  } else {
    if(class(try({
      fwd <- spline(x=x, y=y, xout=t.days, method=method)$y
    }, silent=TRUE)) == "try-error")
    {
      msg <- paste(format(Sys.time()), "ERROR FwdIntrpl", "spline", paste(geterrmessage(), collapse=", "), sep=", ")
      ret <- list(fwd=NULL, err=3, msg=msg)
      return(ret)
    } else {
      fwd <- spot + fwd/(10^frd$fwd_scale[1])
      msg <- paste(format(Sys.time()), "OK FwdIntrpl", t.days, fwd, sep=", ")
      ret <- list(fwd=fwd, err=0, msg=msg)
      return(ret)
    }
  }
}
