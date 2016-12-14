

library(fOptions)


Intrpl <- function(x=NULL, y=NULL, xout=NULL, method=c("linear", "fmm", "natural", "hyman")[1])
{
  if(method == "linear")
  {
    if(class(try({
      yout <- approx(x=x, y=y, xout=xout, method="linear")$y
    }, silent=TRUE)) == "try-error")
    {
      msg <- paste(format(Sys.time()), "ERROR Intrpl", "approx", paste(geterrmessage(), collapse=", "), sep=", ")
      ret <- list(yout=NA, err=1, msg=msg)
      return(ret)
    } else {
      msg <- paste(format(Sys.time()), "OK Intrpl", xout, yout, sep=", ")
      ret <- list(y=yout, err=0, msg=msg)
      return(ret)
    }
  } else {
    if(class(try({
      yout <- spline(x=x, y=y, xout=xout, method=method)$y
    }, silent=TRUE)) == "try-error")
    {
      msg <- paste(format(Sys.time()), "ERROR Intrpl", "spline", paste(geterrmessage(), collapse=", "), sep=", ")
      ret <- list(y=NA, err=2, msg=msg)
      return(ret)
    } else {
      msg <- paste(format(Sys.time()), "OK Intrpl", xout, yout, sep=", ")
      ret <- list(y=yout, err=0, msg=msg)
      return(ret)
    }
  }
}


DeltaStrike <- function(
  delta = 0,
  spot = 0,
  t.year = 0,
  r = 0,
  b = 0,
  sigma = 0,
  tol = 1e-6
)
{
  if(sigma <= 0)
  {
    msg <- paste(format(Sys.time()), "ERROR DeltaStrike", "sigma <= 0", sep=", ")
    ret <- list(strike=NULL, err=1, msg=msg)
    return(ret)
  }
  if(t.year <= 0)
  {
    msg <- paste(format(Sys.time()), "ERROR DeltaStrike", "t.year <= 0", sep=", ")
    ret <- list(strike=NULL, err=2, msg=msg)
    return(ret)
  }
  if(abs(delta) > (1 - tol))
  {
    msg <- paste(format(Sys.time()), "ERROR DeltaStrike", "abs(delta) > (1 - tol)", tol, sep=", ")
    ret <- list(strike=NULL, err=3, msg=msg)
    return(ret)
  }
  if(abs(delta) < tol)
  {
    msg <- paste(format(Sys.time()), "ERROR DeltaStrike", "abs(delta) < tol", tol, sep=", ")
    ret <- list(strike=NULL, err=4, msg=msg)
    return(ret)
  }
  if(class(try({
    q <- r - b
    if(delta > 0) {
      d1 <- qnorm(delta * exp(q * t.year))
    } else {
      d1 <- -qnorm(-delta * exp(q * t.year))
    }
    strike <- spot * exp(-d1 * sigma * sqrt(t.year) + (r - q + sigma*sigma/2) * t.year)
  }, silent=TRUE)) == "try-error")
  {
    msg <- paste(format(Sys.time()), "ERROR DeltaStrike", paste(geterrmessage(), collapse=", "), sep=", ")
    ret <- list(strike=NULL, err=5, msg=msg)
  } else {
    msg <- paste(format(Sys.time()), "OK DeltaStrike", sep=", ")
    ret <- list(strike=strike, err=0, msg=msg)
  }
  return(ret)
}


VannaVolga <- function(
  spot = NULL,
  type = NULL, #c("c", "p")
  strike = NULL,
  t.year = NULL,
  vol0 = NULL,
  ovdv = NULL, #c(atm=10.872, rr25=-0.000, bf25=0.4415, rr10=0.005, bf10=1.322)
  r = NULL,
  b = NULL,
  tol = 1e-5
)
{
  sigma <- c(p10=0, p25=0, atm=0, c25=0, c10=0)
  sigma["atm"] <- ovdv[["atm"]]
  sigma["c25"] <- sigma[["atm"]] + ovdv[["bf25"]] + ovdv[["rr25"]]/2
  sigma["p25"] <- sigma[["c25"]] - ovdv[["rr25"]]
  sigma["c10"] <- sigma[["atm"]] + ovdv[["bf10"]] + ovdv[["rr10"]]/2
  sigma["p10"] <- sigma[["c25"]] - ovdv[["rr10"]]
  sigma <- sigma/100
  
  if(is.null(vol0) || is.na(vol0))
    sigma0 <- sigma["atm"] else sigma0 <- vol0/100
  
  x <- c(p10=0, p25=0, atm=0, c25=0, c10=0)
  x["p10"] <- DeltaStrike(delta=-0.1, spot=spot, t.year=t.year, r=r, b=b, sigma=sigma[["p10"]])$strike
  x["p25"] <- DeltaStrike(delta=-0.25, spot=spot, t.year=t.year, r=r, b=b, sigma=sigma[["p25"]])$strike
  x["atm"] <- DeltaStrike(delta=0.5, spot=spot, t.year=t.year, r=r, b=b, sigma=sigma[["atm"]])$strike
  x["c25"] <- DeltaStrike(delta=0.25, spot=spot, t.year=t.year, r=r, b=b, sigma=sigma[["c25"]])$strike
  x["c10"] <- DeltaStrike(delta=0.1, spot=spot, t.year=t.year, r=r, b=b, sigma=sigma[["c10"]])$strike
  
  if(strike > x["c10"])
  {
    msg <- paste(format(Sys.time()), "VannaVolga", "ERROR", "strike > 10 delta call", strike, x[["c10"]], sep=", ")
    ret <- list(prem=NA, vol=NA, err=1, msg=msg)
    return(ret)
  }
  
  if(strike < x["p10"])
  {
    msg <- paste(format(Sys.time()), "VannaVolga", "ERROR", "strike < 10 delta put", strike, x[["p10"]], sep=", ")
    ret <- list(prem=NA, vol=NA, err=2, msg=msg)
    return(ret)
  }
  
  if(x["p25"] < strike & strike < x["c25"])
  {
    x <- x[c("p25", "atm", "c25")]
    sigma <- sigma[c("p25", "atm", "c25")]
  } else {
    if(strike <= x["p25"])
    {
      x <- x[c("p10", "p25", "atm")]
      sigma <- sigma[c("p10", "p25", "atm")]
    } else {
      x <- x[c("atm", "c25", "c10")]
      sigma <- sigma[c("atm", "c25", "c10")]
    }
  }
  
  if(class(try({
    bsm <- GBSCharacteristics(TypeFlag=type, S=spot, X=strike, Time=t.year, r=r, b=b, sigma=sigma0)
    vega <- sapply(1:3, function(i) GBSCharacteristics(TypeFlag="c", S=spot, X=x[i], Time=t.year, r=r, b=b, sigma=sigma0)$vega)
    w <- NULL
    w[1] <- bsm$vega/vega[1] * log(x[2]/strike)/log(x[2]/x[1]) * log(x[3]/strike)/log(x[3]/x[1])
    w[2] <- bsm$vega/vega[2] * log(strike/x[1])/log(x[2]/x[1]) * log(x[3]/strike)/log(x[3]/x[2])
    w[3] <- bsm$vega/vega[3] * log(strike/x[1])/log(x[3]/x[1]) * log(strike/x[2])/log(x[3]/x[2])
    prem.mkt <- sapply(1:3, function (i) GBSCharacteristics(TypeFlag="c", S=spot, X=x[i], Time=t.year, r=r, b=b, sigma=sigma[i])$premium)
    prem.bsm <- sapply(1:3, function (i) GBSCharacteristics(TypeFlag="c", S=spot, X=x[i], Time=t.year, r=r, b=b, sigma=sigma0)$premium)
    prem <- bsm$premium + sum(w * (prem.mkt - prem.bsm))
    vol <- GBSVolatility(price=prem, TypeFlag=type, S=spot, X=strike, Time=t.year, r=r, b=b, tol=tol) * 100
  }, silent=TRUE)) == "try-error")
  {
    msg <- paste(format(Sys.time()), "VannaVolga", "ERROR", "calculation modul", paste(geterrmessage(), collapse=", "), sep=", ")
    ret <- list(err=3, msg=msg)
    return(ret)
  } else {
    msg <- paste(format(Sys.time()), "VannaVolga", "OK", sep=", ")
    ret <- list(prem=prem, vol=vol, err=0, msg=msg)
    return(ret)
  }
  
}


CalcRSF <- function(
  ccy.pair = "GBPEUR",
  client.buy = "EUR",
  notional.ccy = "EUR",
  spp = data.frame(exp.date=c("2017-03-15", "2017-06-14"), notional=c(100000, 100000), ratio=c(200000, 200000), strike=c(1.2000, 1.2000), reduction=c(1.15, 1.15)),
  spot.ref = 1.1925,
  date.time.ref = "2016-12-14 16:30:00",
  mmd = data.frame(vol.lvl=c(), vol.spd=c(), fwd.pts=c(), r.ccy1=c(), r.ccy2=c()),
  pg = NULL,
)
{
  
  
  
  ##### ccy #####
  if(nrow(ccy <- g.ccy[g.ccy$sname == ccy.pair, ]) != 1)
  {
    msg <- paste(format(Sys.time()), "CalcRSF", "ERROR nrow ccy <> 1", ccy.pair, sep="; ")
    return(list(err=1, msg=msg))
  }
  
  ##### frd dbReadTable #####
  if(class(try({
    drv <- dbDriver("PostgreSQL")
    pg.con <- dbConnect(drv, dbname=pg$dbname, host=pg$host, port=pg$port, user=pg$user, password=pg$password)
    frd <- dbReadTable(pg.con, "frd")
    sts <- dbDisconnect(pg.con)
    sts <- dbUnloadDriver(drv)
  }, silent=TRUE)) == "try-error"){
    msg <- paste(format(Sys.time()), "CalcRSF", "ERROR dbReadTable frd", paste(geterrmessage(), collapse=", "), sep="; ")
    return(list(err=2, msg=msg))
  } else {
    if(nrow(frd <- frd[frd$ccy == ccy$cname, ]) < 1)
    {
      msg <- paste(format(Sys.time()), "CalcRSF", "ERROR nrow frd < 1", nrow(frd), sep="; ")
      return(list(err=3, msg=msg))
    }
  }
  
  ##### swp dbReadTable #####
  if(class(try({
    drv <- dbDriver("PostgreSQL")
    pg.con <- dbConnect(drv, dbname=pg$dbname, host=pg$host, port=pg$port, user=pg$user, password=pg$password)
    swp <- dbReadTable(pg.con, "swp")
    sts <- dbDisconnect(pg.con)
    sts <- dbUnloadDriver(drv)
  }, silent=TRUE)) == "try-error"){
    msg <- paste(format(Sys.time()), "CalcRSF", "ERROR dbReadTable swp", paste(geterrmessage(), collapse=", "), sep="; ")
    return(list(err=4, msg=msg))
  } else {
    if(nrow(swp <- swp[swp$ccy == ccy$depo, ]) < 1)
    {
      msg <- paste(format(Sys.time()), "CalcRSF", "ERROR nrow swp < 1", nrow(swp), sep="; ")
      return(list(err=5, msg=msg))
    }
  }
  
  ##### ovdv dbReadTable #####
  if(class(try({
    drv <- dbDriver("PostgreSQL")
    pg.con <- dbConnect(drv, dbname=pg$dbname, host=pg$host, port=pg$port, user=pg$user, password=pg$password)
    ovdv <- dbReadTable(pg.con, "ovdv")
    sts <- dbDisconnect(pg.con)
    sts <- dbUnloadDriver(drv)
  }, silent=TRUE)) == "try-error"){
    msg <- paste(format(Sys.time()), "CalcRSF", "ERROR dbReadTable ovdv", paste(geterrmessage(), collapse=", "), sep="; ")
    return(list(err=4, msg=msg))
  } else {
    if(nrow(ovdv <- ovdv[ovdv$ccy == ccy$cname, ]) < 1)
    {
      msg <- paste(format(Sys.time()), "CalcRSF", "ERROR nrow ovdv < 1", nrow(ovdv), sep="; ")
      return(list(err=5, msg=msg))
    }
  }
  
  ##### hedge Hedging set #####
  if(class(try({
    hedge <- NULL
    for(i in 1:nrow(spp))
    {
      strip <- data.frame(style=c("Vanilla", "Vanilla", "Vanilla"), buy=c(TRUE, FALSE, FALSE), spread=c(ccy$spread/2, -ccy$spread/2, -ccy$spread/2))
      strip$exp.date <- spp$exp.date[i]
      hedge <- rbind(hedge, strip)
    }
    hedge$ccy <- ccy$cname
    hedge$cp <- NA
    hedge$strike <- NA
    hedge$notional1 <- NA
    hedge$notional2 <- NA
    if(client.buy == ccy$quote)
    {
      if(notional.ccy == ccy$base & ccy$conv)
      {
        for(i in 1:nrow(spp))
        {
          strike <- spp$strike[i] + ccy$slip
          reduction <- spp$reduction[i]
          j <- ((i - 1) * 3 + 1):((i - 1) * 3 + 3)
          hedge$cp[j] <- c("p", "p", "c")
          hedge$strike[j] <- c(strike, reduction, strike)
          hedge$notional1[j] <- round(c(spp$notional[i], spp$notional[i], spp$ratio[i]), 0)
          hedge$notional2[j] <- round(hedge$notional1[j] * hedge$strike[j], 2)
        }
      }
      if(notional.ccy == ccy$base & !ccy$conv)
      {
        for(i in 1:nrow(spp))
        {
          strike <- round(1/(spp$strike[i] + ccy$slip), ccy$s.digits)
          reduction <- ceiling(1/spp$reduction[i] * 10^ccy$s.digits)/(10^ccy$s.digits)
          j <- ((i - 1) * 3 + 1):((i - 1) * 3 + 3)
          hedge$cp[j] <- c("c", "c", "p")
          hedge$strike[j] <- c(strike, reduction, strike)
          hedge$notional1[j] <- round(c(spp$notional[i]/strike, spp$notional[i]/reduction, spp$ratio[i]/strike), 0)
          hedge$notional2[j] <- round(hedge$notional1[j] * hedge$strike[j], 2)
        }
      }
      if(notional.ccy == ccy$quote & ccy$conv)
      {
        spp$low.level <- NA
        for(i in 1:nrow(spp))
        {
          tenor <- as.numeric(difftime(as.POSIXct(paste(spp$exp.date[i], ccy$exp.time), tz="GMT"), as.POSIXct(date.time.ref, tz="GMT"), units="days"))
          fwd <- spot.ref + Intrpl(x=frd$days_to_mty, y=(frd$bid + frd$ask)/2, xout=tenor, method="linear")$y/(10^frd$fwd_scale[1])
          vol <- Intrpl(x=ovdv$days_to_exp[ovdv$par == "ATM"], y=(ovdv$bid[ovdv$par == "ATM"] + ovdv$ask[ovdv$par == "ATM"])/2, xout=tenor, method="linear")$y/100
          spp$low.level[i] <- round(fwd * exp(qnorm(p=ccy$quantile, mean=0, sd=vol*sqrt(tenor/365))), ccy$digits)
          strike <- spp$strike[i] + ccy$slip
          reduction <- spp$reduction[i]
          notional.red <- spp$notional[i]/spp$strike[i] * spp$low.level[i]/(spp$strike[i] - spp$reduction[i] + spp$low.level[i])
          j <- ((i - 1) * 3 + 1):((i - 1) * 3 + 3)
          hedge$cp[j] <- c("p", "p", "c")
          hedge$strike[j] <- c(strike, reduction, strike)
          hedge$notional1[j] <- round(c(spp$notional[i]/strike, notional.red, spp$ratio[i]/strike), 0)
          hedge$notional2[j] <- round(hedge$notional1[j] * hedge$strike[j], 2)
        }
      }
      if(notional.ccy == ccy$quote & !ccy$conv)
      {
        spp$low.level <- NA
        for(i in 1:nrow(spp))
        {
          tenor <- as.numeric(difftime(as.POSIXct(paste(spp$exp.date[i], ccy$exp.time), tz="GMT"), as.POSIXct(date.time.ref, tz="GMT"), units="days"))
          fwd <- 1/spot.ref + Intrpl(x=frd$days_to_mty, y=(frd$bid + frd$ask)/2, xout=tenor, method="linear")$y/(10^frd$fwd_scale[1])
          vol <- Intrpl(x=ovdv$days_to_exp[ovdv$par == "ATM"], y=(ovdv$bid[ovdv$par == "ATM"] + ovdv$ask[ovdv$par == "ATM"])/2, xout=tenor, method="linear")$y/100
          spp$low.level[i] <- round(1/(fwd * exp(qnorm(p=1-ccy$quantile, mean=0, sd=vol*sqrt(tenor/365)))), ccy$digits)
          strike <- round(1/(spp$strike[i] + ccy$slip), ccy$s.digits)
          reduction <- ceiling(1/spp$reduction[i] * 10^ccy$s.digits)/(10^ccy$s.digits)
          notional.red <- spp$notional[i] * spp$reduction[i]/spp$strike[i] * spp$low.level[i]/(spp$strike[i] - spp$reduction[i] + spp$low.level[i])
          j <- ((i - 1) * 3 + 1):((i - 1) * 3 + 3)
          hedge$cp[j] <- c("c", "c", "p")
          hedge$strike[j] <- c(strike, reduction, strike)
          hedge$notional1[j] <- round(c(spp$notional[i], notional.red, spp$ratio[i]), 0)
          hedge$notional2[j] <- round(hedge$notional1[j] * hedge$strike[j], 2)
        }
      }
    }
    if(client.buy == ccy$base)
    {
      if(notional.ccy == ccy$base & ccy$conv)
      {
        for(i in 1:nrow(spp))
        {
          strike <- spp$strike[i] - ccy$slip
          reduction <- spp$reduction[i]
          j <- ((i - 1) * 3 + 1):((i - 1) * 3 + 3)
          hedge$cp[j] <- c("c", "c", "p")
          hedge$strike[j] <- c(strike, reduction, strike)
          hedge$notional1[j] <- round(c(spp$notional[i], spp$notional[i], spp$ratio[i]), 0)
          hedge$notional2[j] <- round(hedge$notional1[j] * hedge$strike[j], 2)
        }
      }
      if(notional.ccy == ccy$base & !ccy$conv)
      {
        for(i in 1:nrow(spp))
        {
          strike <- round(1/(spp$strike[i] - ccy$slip), ccy$s.digits)
          reduction <- floor(1/spp$reduction[i] * 10^ccy$s.digits)/(10^ccy$s.digits)
          j <- ((i - 1) * 3 + 1):((i - 1) * 3 + 3)
          hedge$cp[j] <- c("p", "p", "c")
          hedge$strike[j] <- c(strike, reduction, strike)
          hedge$notional1[j] <- round(c(spp$notional[i]/strike, spp$notional[i]/reduction, spp$ratio[i]/strike), 0)
          hedge$notional2[j] <- round(hedge$notional1[j] * hedge$strike[j], 2)
        }
      }
      if(notional.ccy == ccy$quote & ccy$conv)
      {
        spp$high.level <- NA
        for(i in 1:nrow(spp))
        {
          tenor <- as.numeric(difftime(as.POSIXct(paste(spp$exp.date[i], ccy$exp.time), tz="GMT"), as.POSIXct(date.time.ref, tz="GMT"), units="days"))
          fwd <- spot.ref + Intrpl(x=frd$days_to_mty, y=(frd$bid + frd$ask)/2, xout=tenor, method="linear")$y/(10^frd$fwd_scale[1])
          vol <- Intrpl(x=ovdv$days_to_exp[ovdv$par == "ATM"], y=(ovdv$bid[ovdv$par == "ATM"] + ovdv$ask[ovdv$par == "ATM"])/2, xout=tenor, method="linear")$y/100
          spp$high.level[i] <- round(fwd * exp(qnorm(p=1-ccy$quantile, mean=0, sd=vol*sqrt(tenor/365))), ccy$digits)
          strike <- spp$strike[i] - ccy$slip
          reduction <- spp$reduction[i]
          notional.red <- spp$notional[i]/spp$strike[i] * spp$high.level[i]/(spp$strike[i] - spp$reduction[i] + spp$high.level[i])
          j <- ((i - 1) * 3 + 1):((i - 1) * 3 + 3)
          hedge$cp[j] <- c("c", "c", "p")
          hedge$strike[j] <- c(strike, reduction, strike)
          hedge$notional1[j] <- round(c(spp$notional[i]/strike, notional.red, spp$ratio[i]/strike), 0)
          hedge$notional2[j] <- round(hedge$notional1[j] * hedge$strike[j], 2)
        }
      }
      if(notional.ccy == ccy$quote & !ccy$conv)
      {
        spp$high.level <- NA
        for(i in 1:nrow(spp))
        {
          tenor <- as.numeric(difftime(as.POSIXct(paste(spp$exp.date[i], ccy$exp.time), tz="GMT"), as.POSIXct(date.time.ref, tz="GMT"), units="days"))
          fwd <- 1/spot.ref + Intrpl(x=frd$days_to_mty, y=(frd$bid + frd$ask)/2, xout=tenor, method="linear")$y/(10^frd$fwd_scale[1])
          vol <- Intrpl(x=ovdv$days_to_exp[ovdv$par == "ATM"], y=(ovdv$bid[ovdv$par == "ATM"] + ovdv$ask[ovdv$par == "ATM"])/2, xout=tenor, method="linear")$y/100
          spp$high.level[i] <- round(1/(fwd * exp(qnorm(p=ccy$quantile, mean=0, sd=vol*sqrt(tenor/365)))), ccy$digits)
          strike <- round(1/(spp$strike[i] - ccy$slip), ccy$s.digits)
          reduction <- floor(1/spp$reduction[i] * 10^ccy$s.digits)/(10^ccy$s.digits)
          notional.red <- spp$notional[i] * spp$reduction[i]/spp$strike[i] * spp$high.level[i]/(spp$strike[i] - spp$reduction[i] + spp$high.level[i])
          j <- ((i - 1) * 3 + 1):((i - 1) * 3 + 3)
          hedge$cp[j] <- c("p", "p", "c")
          hedge$strike[j] <- c(strike, reduction, strike)
          hedge$notional1[j] <- round(c(spp$notional[i], notional.red, spp$ratio[i]), 0)
          hedge$notional2[j] <- round(hedge$notional1[j] * hedge$strike[j], 2)
        }
      }
    }
  }, silent=TRUE)) == "try-error"){
    msg <- paste(format(Sys.time()), "CalcRSF", "ERROR Hedging set", paste(geterrmessage(), collapse=", "), sep="; ")
    return(list(err=6, msg=msg))
  } else {
    if(any(is.na(hedge)))
    {
      msg <- paste(format(Sys.time()), "CalcRSF", "ERROR NA in hedge", sep="; ")
      return(list(err=7, msg=msg))
    }
  }
  
  ##### hedge Valuation #####
  if(class(try({
    hedge$fwd <- NA
    hedge$depo1 <- NA
    hedge$depo2 <- NA
    hedge$depo2.imp <- NA
    hedge$vol <- NA
    hedge$prem <- NA
    for(i in 1:nrow(hedge))
    {
      t.days <- as.numeric(difftime(as.POSIXlt(paste(hedge$exp.date[i], ccy$exp.time), tz="GMT"), as.POSIXlt(date.time.ref, tz="GMT"), units="days"))
      if((res <- Intrpl(x=frd$days_to_mty, y=(frd$bid+frd$ask)/2, xout=t.days, method="linear"))$err > 0)
      {
        msg <- paste(format(Sys.time()), "CalcRSF", "ERROR Valuation", res$msg, sep="; ")
        return(list(err=8, msg=msg))
      } else fwd <- res$y/(10^frd$fwd_scale[1])
      if((res <- Intrpl(x=swp$tenor, y=(swp$bid+swp$ask)/2, xout=t.days, method="linear"))$err > 0)
      {
        msg <- paste(format(Sys.time()), "CalcRSF", "ERROR Valuation", res$msg, sep="; ")
        return(list(err=9, msg=msg))
      } else dp <- res$y/100
      depo.dom.imp <- NA
      if(ccy$conv)
      {
        spt <- spot.ref
        if(ccy$base == ccy$depo)
        {
          depo.for <- dp
          depo.dom <- fwd/spt/t.days * 365 + depo.for
          depo.dom.imp <- TRUE
        } else {
          depo.dom <- dp
          depo.for <- depo.dom - fwd/spt/t.days * 365
          depo.dom.imp <- FALSE
        }
      } else {
        spt <- round(1/spot.ref, ccy$digits)
        if(ccy$base == ccy$depo)
        {
          depo.dom <- dp
          depo.for <- depo.dom - fwd/spt/t.days * 365
          depo.dom.imp <- FALSE
        } else {
          depo.for <- dp
          depo.dom <- fwd/spt/t.days * 365 + depo.for
          depo.dom.imp <- TRUE
        }
      }
      hedge$fwd[i] <- fwd
      hedge$depo1[i] <- depo.for
      hedge$depo2[i] <- depo.dom
      hedge$depo2.imp[i] <- depo.dom.imp
      vs <- c(atm=NA, rr25=NA, bf25=NA, rr10=NA, bf10=NA)
      if((res <- Intrpl(x=ovdv$days_to_exp[ovdv$par == "ATM"], y=(ovdv$bid[ovdv$par == "ATM"] + ovdv$ask[ovdv$par == "ATM"])/2, xout=t.days, method="linear"))$err > 0)
      {
        msg <- paste(format(Sys.time()), "CalcRSF", "ERROR Valuation", res$msg, sep="; ")
        return(list(err=10, msg=msg))
      } else vs["atm"] <- res$y
      if((res <- Intrpl(x=ovdv$days_to_exp[ovdv$par == "RR25"], y=(ovdv$bid[ovdv$par == "RR25"] + ovdv$ask[ovdv$par == "RR25"])/2, xout=t.days, method="linear"))$err > 0)
      {
        msg <- paste(format(Sys.time()), "CalcRSF", "ERROR Valuation", res$msg, sep="; ")
        return(list(err=11, msg=msg))
      } else vs["rr25"] <- res$y
      if((res <- Intrpl(x=ovdv$days_to_exp[ovdv$par == "BF25"], y=(ovdv$bid[ovdv$par == "BF25"] + ovdv$ask[ovdv$par == "BF25"])/2, xout=t.days, method="linear"))$err > 0)
      {
        msg <- paste(format(Sys.time()), "CalcRSF", "ERROR Valuation", res$msg, sep="; ")
        return(list(err=11, msg=msg))
      } else vs["bf25"] <- res$y
      if((res <- Intrpl(x=ovdv$days_to_exp[ovdv$par == "RR10"], y=(ovdv$bid[ovdv$par == "RR10"] + ovdv$ask[ovdv$par == "RR10"])/2, xout=t.days, method="linear"))$err > 0)
      {
        msg <- paste(format(Sys.time()), "CalcRSF", "ERROR Valuation", res$msg, sep="; ")
        return(list(err=12, msg=msg))
      } else vs["rr10"] <- res$y
      if((res <- Intrpl(x=ovdv$days_to_exp[ovdv$par == "BF10"], y=(ovdv$bid[ovdv$par == "BF10"] + ovdv$ask[ovdv$par == "BF10"])/2, xout=t.days, method="linear"))$err > 0)
      {
        msg <- paste(format(Sys.time()), "CalcRSF", "ERROR Valuation", res$msg, sep="; ")
        return(list(err=13, msg=msg))
      } else vs["bf10"] <- res$y
      if((res <- VannaVolga(spot=spt, type=hedge$cp[i], strike=hedge$strike[i], t.year=t.days/365, vol0=vs["atm"], ovdv=vs, r=depo.dom, b=depo.dom - depo.for, tol = 1e-5))$err > 0)
      {
        msg <- paste(format(Sys.time()), "CalcRSF", "ERROR Valuation", res$msg, sep="; ")
        return(list(err=15, msg=msg))
      } else hedge$vol[i] <- res$vol
      hedge$prem[i] <- hedge$notional1[i] * GBSOption(TypeFlag=hedge$cp[i], S=spt, X=hedge$strike[i], Time=t.days/365, r=depo.dom, b=depo.dom-depo.for, sigma=(hedge$vol[i]+hedge$spread[i])/100)@price
      if(hedge$buy[i]) hedge$prem[i] <- -1 * hedge$prem[i]
    }
    if(substr(ccy$cname, 4, 6) == "GBP")
    {
      hedge$prem.gbp <- round(hedge$prem, 2)
      margin.rate <- NA
    } else {
      if(substr(ccy$cname, 1, 3) == "GBP")
      {
        hedge$prem.gbp <- round(hedge$prem/spt, 2)
        margin.rate <- spt
      } else {
        if(class(try({
          drv <- dbDriver("PostgreSQL")
          pg.con <- dbConnect(drv, dbname=pg$dbname, host=pg$host, port=pg$port, user=pg$user, password=pg$password)
          df <- dbReadTable(pg.con, "spt")
          sts <- dbDisconnect(pg.con)
          sts <- dbUnloadDriver(drv)
        }, silent=TRUE)) == "try-error"){
          msg <- paste(format(Sys.time()), "CalcRSF", "ERROR Valuation dbReadTable spt", paste(geterrmessage(), collapse=", "), sep="; ")
          return(list(err=16, msg=msg))
        } else {
          if(nrow(df[df$ccy == paste0("GBP", substr(ccy$cname, 4, 6)), ]) == 1)
          {
            df <- df[df$ccy == paste0("GBP", substr(ccy$cname, 4, 6)), ]
            hedge$prem.gbp <- round(hedge$prem/((df$bid + df$ask)/2), 2)
            margin.rate <- (df$bid + df$ask)/2
          } else {
            if(nrow(df[df$ccy == paste0(substr(ccy$cname, 4, 6), "GBP"), ]) == 1)
            {
              df <- df[df$ccy == paste0(substr(ccy$cname, 4, 6), "GBP"), ]
              hedge$prem.gbp <- round(hedge$prem * (df$bid + df$ask)/2, 2)
              margin.rate <- (df$bid + df$ask)/2
            } else {
              msg <- paste(format(Sys.time()), "CalcRSF", "ERROR Valuation dbReadTable spt nrow(df) != 1", sep="; ")
              return(list(err=17, msg=msg))
            }
          }
        }
      }
    }
  }, silent=TRUE)) == "try-error"){
    msg <- paste(format(Sys.time()), "CalcRSF", "ERROR Valuation", paste(geterrmessage(), collapse=", "), sep="; ")
    return(list(err=16, msg=msg))
  } else {
    if(any(is.na(hedge)))
    {
      msg <- paste(format(Sys.time()), "CalcRSF", "ERROR Valuation NA in hedge ", sep="; ")
      return(list(err=20, msg=msg))
    }
  }
  
  ##### ret #####
  if(class(try({
    margin <- sum(hedge$prem)
    margin.ccy <- substr(ccy$cname, 4, 6)
    margin.gbp <- sum(hedge$prem.gbp)
    if(margin.ccy == notional.ccy)
    {
      margin.prc <- round(margin/sum(spp$notional) * 100, 2)
    } else {
      if(margin.ccy == ccy$base)
      {
        margin.prc <- round(margin * spot.ref/sum(spp$notional) * 100, 2)
      } else {
        margin.prc <- round(margin/spot.ref/sum(spp$notional) * 100, 2)
      }
    }
    ret <- list(hedge = hedge,
                low.level = spp$low.level,
                high.level = spp$high.level,
                margin = data.frame(GBP=margin.gbp, Prc=margin.prc, Value=margin, Ccy=margin.ccy, Rate=margin.rate))
  }, silent=TRUE)) == "try-error"){
    msg <- paste(format(Sys.time()), "CalcRSF", "ERROR ret", paste(geterrmessage(), collapse=", "), sep="; ")
    return(list(err=21, msg=msg))
  } else {
    msg <- paste(format(Sys.time()), "CalcRSF", "Ok", sep="; ")
    return(list(ret=ret, err=0, msg=msg))
  }
}



