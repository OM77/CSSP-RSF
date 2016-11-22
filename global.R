library(lubridate)

options(stringsAsFactors=FALSE)

is.blank <- function(x, false.triggers=FALSE)
{
  return(is.null(x) || length(x) == 0 || all(is.na(x)) || all(x=="") || (false.triggers && all(!x)))
}

g.sp.name <- c(short="RSF", long="Ratio Spread Forward")

g.ccy <- data.frame()
g.ccy <- rbind(g.ccy, data.frame(base="GBP", quote="USD", bname="£", qname="$", conv=TRUE, slip=0.0010, digits=5, s.digits=5, b.digits=4, low=0.75, high=2.50))
g.ccy <- rbind(g.ccy, data.frame(base="GBP", quote="EUR", bname="£", qname="€", conv=FALSE, slip=0.0010, digits=5, s.digits=5, b.digits=4, low=0.75, high=2.00))
g.ccy <- rbind(g.ccy, data.frame(base="EUR", quote="USD", bname="€", qname="$", conv=TRUE, slip=0.0008, digits=5, s.digits=5, b.digits=4, low=0.60, high=1.60))
g.ccy <- rbind(g.ccy, data.frame(base="EUR", quote="GBP", bname="€", qname="£", conv=TRUE, slip=0.0007, digits=5, s.digits=5, b.digits=4, low=0.60, high=1.60))
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

 
#g.spot.ref <- 1.2455

param <- list()
param$ccy <- g.ccy$name[1]
param$client.buys <- g.ccy$base[1]
param$notional.ccy <- g.ccy$base[1]
param$expiry <- Sys.Date() %m+% months(1:6)
param$notional <- rep(100000, 6)
param$ratio <- rep(200000, 6)
param$strike <- rep(0.99999, 6)
param$reduction <- rep(1.10001, 6)
param$spot.ref <- 0.99999