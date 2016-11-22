
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(lubridate)
library(RPostgreSQL)


shinyServer(function(input, output, session) {
  
  source("global.R", local=TRUE)
  
  param <<- param
  
  upload.file <- format(Sys.time(), "%Y-%m-%d %H%M%S")
  
  rv <- reactiveValues()
  rv$msg.log <- paste(format(Sys.time()), "Run Application", sep=", ")
  full.log <<- NULL
  
  # upload param
  observe({
    z <- input$upload.scheme
    if(!is.null(z))
    {
      df <- read.csv(z$datapath, header=TRUE, sep=",")
      if(df$sp[1] != g.sp.name["short"])
      {
        rv$msg.log <- paste(format(Sys.time()), "ERROR", paste0("SP name <> ", g.sp.name["short"]), sep=", ")
        return(NULL)
      }
      param$ccy <<- as.character(df$ccy[1])
      param$client.buys <<- as.character(df$client.buys[1])
      param$notional.ccy <<- as.character(df$notional.ccy[1])
      param$expiry <<- as.Date(df$expiry, "%Y-%m-%d")
      param$notional <<- as.numeric(df$notional)
      param$ratio <<- as.numeric(df$ratio)
      param$strike <<- as.numeric(df$strike)
      param$reduction <<- as.numeric(df$reduction)
      param$spot.ref <<- as.numeric(df$spot.ref[1])
      updateSelectInput(session, "ccy", selected=param$ccy)
      choices <- as.character(g.ccy[g.ccy$name == param$ccy, c("base", "quote")])
      updateRadioButtons(session, "client.buys", choices=choices, selected=param$client.buys, inline=TRUE)
      updateRadioButtons(session, "notional.ccy", choices=choices, selected=param$notional.ccy, inline=TRUE)
      n <- length(param$expiry)
      updateRadioButtons(session, "strip.occurrences", selected=n, inline=TRUE)
      updateDateInput(session, "expiry1", value=param$expiry[1])
      updateNumericInput(session, "notional1", value=param$notional[1])
      updateNumericInput(session, "ratio1", value=param$ratio[1])
      updateNumericInput(session, "strike1", value=param$strike[1])
      updateNumericInput(session, "reduction1", value=param$reduction[1])
      rv$msg.log <- paste(format(Sys.time()), "upload param", sep=", ")
    }
    
  })
  
  # update client.buys and notional.ccy radio buttons
  observe({
    choices <- as.character(g.ccy[g.ccy$name == input$ccy, c("base", "quote")])
    client.buys <- NULL
    notional.ccy <- NULL
    if(!is.null(param$client.buys))
      if(param$client.buys %in% choices)
        client.buys <- param$client.buys
    if(!is.null(param$notional.ccy))
      if(param$notional.ccy %in% choices)
        notional.ccy <- param$notional.ccy
    rv$msg.log <- paste(format(Sys.time()), "client.buys", param$client.buys, sep=", ")
    updateRadioButtons(session, "client.buys", choices=choices, selected=client.buys, inline=TRUE)
    updateRadioButtons(session, "notional.ccy", choices=choices, selected=notional.ccy, inline=TRUE)
  })
  
  # update param
  observe(if(!is.blank(z <- input$ccy)) param$ccy <<- z)
  observe(if(!is.blank(z <- input$client.buys)) param$client.buys <<- z)
  observe(if(!is.blank(z <- input$notional.ccy)) param$notional.ccy <<- z)
  observe(if(is.Date(z <- input$expiry1)) param$expiry[1] <<- z)
  observe(if(is.Date(z <- input$expiry2)) param$expiry[2] <<- z)
  observe(if(is.Date(z <- input$expiry3)) param$expiry[3] <<- z)
  observe(if(is.Date(z <- input$expiry4)) param$expiry[4] <<- z)
  observe(if(is.Date(z <- input$expiry5)) param$expiry[5] <<- z)
  observe(if(is.Date(z <- input$expiry6)) param$expiry[6] <<- z)
  observe(if(!is.blank(z <- input$notional1)) param$notional[1] <<- z)
  observe(if(!is.blank(z <- input$notional2)) param$notional[2] <<- z)
  observe(if(!is.blank(z <- input$notional3)) param$notional[3] <<- z)
  observe(if(!is.blank(z <- input$notional4)) param$notional[4] <<- z)
  observe(if(!is.blank(z <- input$notional5)) param$notional[5] <<- z)
  observe(if(!is.blank(z <- input$notional6)) param$notional[6] <<- z)
  observe(if(!is.blank(z <- input$ratio1)) param$ratio[1] <<- z)
  observe(if(!is.blank(z <- input$ratio2)) param$ratio[2] <<- z)
  observe(if(!is.blank(z <- input$ratio3)) param$ratio[3] <<- z)
  observe(if(!is.blank(z <- input$ratio4)) param$ratio[4] <<- z)
  observe(if(!is.blank(z <- input$ratio5)) param$ratio[5] <<- z)
  observe(if(!is.blank(z <- input$ratio6)) param$ratio[6] <<- z)
  observe(if(!is.blank(z <- input$strike1)) param$strike[1] <<- z)
  observe(if(!is.blank(z <- input$strike2)) param$strike[2] <<- z)
  observe(if(!is.blank(z <- input$strike3)) param$strike[3] <<- z)
  observe(if(!is.blank(z <- input$strike4)) param$strike[4] <<- z)
  observe(if(!is.blank(z <- input$strike5)) param$strike[5] <<- z)
  observe(if(!is.blank(z <- input$strike6)) param$strike[6] <<- z)
  observe(if(!is.blank(z <- input$reduction1)) param$reduction[1] <<- z)
  observe(if(!is.blank(z <- input$reduction2)) param$reduction[2] <<- z)
  observe(if(!is.blank(z <- input$reduction3)) param$reduction[3] <<- z)
  observe(if(!is.blank(z <- input$reduction4)) param$reduction[4] <<- z)
  observe(if(!is.blank(z <- input$reduction5)) param$reduction[5] <<- z)
  observe(if(!is.blank(z <- input$reduction6)) param$reduction[6] <<- z)
  observe(if(!is.blank(z <- input$spot.fef)) param$spot.ref <<- z)
  
  # apply to all notional
  observe({
    if(!is.blank(z <- input$notional.all) & is.numeric(z))
    {
      updateNumericInput(session, "notional1", value=z)
      updateNumericInput(session, "notional2", value=z)
      updateNumericInput(session, "notional3", value=z)
      updateNumericInput(session, "notional4", value=z)
      updateNumericInput(session, "notional5", value=z)
      updateNumericInput(session, "notional6", value=z)
    }
  })
  
  # apply to all ratio
  observe({
    if(!is.blank(z <- input$ratio.all) & is.numeric(z))
    {
      updateNumericInput(session, "ratio1", value=z)
      updateNumericInput(session, "ratio2", value=z)
      updateNumericInput(session, "ratio3", value=z)
      updateNumericInput(session, "ratio4", value=z)
      updateNumericInput(session, "ratio5", value=z)
      updateNumericInput(session, "ratio6", value=z)
    }
  })
  
  # apply to all strike
  observe({
    if(!is.blank(z <- input$strike.all) & is.numeric(z))
    {
      updateNumericInput(session, "strike1", value=z)
      updateNumericInput(session, "strike2", value=z)
      updateNumericInput(session, "strike3", value=z)
      updateNumericInput(session, "strike4", value=z)
      updateNumericInput(session, "strike5", value=z)
      updateNumericInput(session, "strike6", value=z)
    }
  })
  
  # apply to all reduction
  observe({
    if(!is.blank(z <- input$reduction.all) & is.numeric(z))
    {
      updateNumericInput(session, "reduction1", value=z)
      updateNumericInput(session, "reduction2", value=z)
      updateNumericInput(session, "reduction3", value=z)
      updateNumericInput(session, "reduction4", value=z)
      updateNumericInput(session, "reduction5", value=z)
      updateNumericInput(session, "reduction6", value=z)
    }
  })
  
  
  output$notional.all.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 2)
      return(numericInput(inputId="notional.all", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$ratio.all.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 2)
      return(numericInput(inputId="ratio.all", label=NULL, value=NULL, min=100, max=10^7, step=0.01))
  })
  
  output$strike.all.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 2)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="strike.all", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$reduction.all.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) >= 2)
    {
      z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
      return(numericInput(inputId="reduction.all", label=NULL, value=NULL, min=z$low, max=z$high, step=z$s.step))
    }
  })
  
  output$expiry2.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 2) return(NULL)
    return(dateInput(inputId="expiry2", label=NULL, value=param$expiry[2], format="dd-M-yyyy", weekstart=1, startview="month"))
  })
  
  output$expiry3.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 3) return(NULL)
    return(dateInput(inputId="expiry3", label=NULL, value=param$expiry[3], format="dd-M-yyyy", weekstart=1, startview="month"))
  })
  
  output$expiry4.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 4) return(NULL)
    return(dateInput(inputId="expiry4", label=NULL, value=param$expiry[4], format="dd-M-yyyy", weekstart=1, startview="month"))
  })
  
  output$expiry5.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 5) return(NULL)
    return(dateInput(inputId="expiry5", label=NULL, value=param$expiry[5], format="dd-M-yyyy", weekstart=1, startview="month"))
  })
  
  output$expiry6.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 6) return(NULL)
    return(dateInput(inputId="expiry6", label=NULL, value=param$expiry[6], format="dd-M-yyyy", weekstart=1, startview="month"))
  })
  
  output$notional2.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 2) return(NULL)
    return(numericInput(inputId="notional2", label=NULL, value=param$notional[2], min=0, max=10^7, step=0.01))
  })
  
  output$notional3.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 3) return(NULL)
    return(numericInput(inputId="notional3", label=NULL, value=param$notional[3], min=0, max=10^7, step=0.01))
  })
  
  output$notional4.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 4) return(NULL)
    return(numericInput(inputId="notional4", label=NULL, value=param$notional[4], min=0, max=10^7, step=0.01))
  })
  
  output$notional5.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 5) return(NULL)
    return(numericInput(inputId="notional5", label=NULL, value=param$notional[5], min=0, max=10^7, step=0.01))
  })
  
  output$notional6.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 6) return(NULL)
    return(numericInput(inputId="notional6", label=NULL, value=param$notional[6], min=0, max=10^7, step=0.01))
  })
  
  output$ratio2.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 2) return(NULL)
    return(numericInput(inputId="ratio2", label=NULL, value=param$ratio[2], min=0, max=10^7, step=0.01))
  })
  
  output$ratio3.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 3) return(NULL)
    return(numericInput(inputId="ratio3", label=NULL, value=param$ratio[3], min=0, max=10^7, step=0.01))
  })
  
  output$ratio4.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 4) return(NULL)
    return(numericInput(inputId="ratio4", label=NULL, value=param$ratio[4], min=0, max=10^7, step=0.01))
  })
  
  output$ratio5.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 5) return(NULL)
    return(numericInput(inputId="ratio5", label=NULL, value=param$ratio[5], min=0, max=10^7, step=0.01))
  })
  
  output$ratio6.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 6) return(NULL)
      return(numericInput(inputId="ratio6", label=NULL, value=param$ratio[6], min=0, max=10^7, step=0.01))
  })
  
  output$strike1.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 1) return(NULL)
    z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
    return(numericInput(inputId="strike1", label=NULL, value=param$strike[1], min=z$low, max=z$high, step=z$s.step))
  })
  
  output$strike2.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 2) return(NULL)
    z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
    return(numericInput(inputId="strike2", label=NULL, value=param$strike[2], min=z$low, max=z$high, step=z$s.step))
  })
  
  output$strike3.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 3) return(NULL)
    z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
    return(numericInput(inputId="strike3", label=NULL, value=param$strike[3], min=z$low, max=z$high, step=z$s.step))
  })
  
  output$strike4.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 4) return(NULL)
    z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
    return(numericInput(inputId="strike4", label=NULL, value=param$strike[4], min=z$low, max=z$high, step=z$s.step))
  })
  
  output$strike5.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 5) return(NULL)
    z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
    return(numericInput(inputId="strike5", label=NULL, value=param$strike[5], min=z$low, max=z$high, step=z$s.step))
  })
  
  output$strike6.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 6) return(NULL)
    z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
    return(numericInput(inputId="strike6", label=NULL, value=param$strike[6], min=z$low, max=z$high, step=z$s.step))
  })
  
  output$reduction1.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 1) return(NULL)
    z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
    return(numericInput(inputId="reduction1", label=NULL, value=param$reduction[1], min=z$low, max=z$high, step=z$s.step))
  })
  
  output$reduction2.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 2) return(NULL)
    z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
    return(numericInput(inputId="reduction2", label=NULL, value=param$reduction[2], min=z$low, max=z$high, step=z$s.step))
  })
  
  output$reduction3.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 3) return(NULL)
    z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
    return(numericInput(inputId="reduction3", label=NULL, value=param$reduction[3], min=z$low, max=z$high, step=z$s.step))
  })
  
  output$reduction4.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 4) return(NULL)
    z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
    return(numericInput(inputId="reduction4", label=NULL, value=param$reduction[4], min=z$low, max=z$high, step=z$s.step))
  })
  
  output$reduction5.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 5) return(NULL)
    z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
    return(numericInput(inputId="reduction5", label=NULL, value=param$reduction[5], min=z$low, max=z$high, step=z$s.step))
  })
  
  output$reduction6.rnd <- renderUI({
    if(as.numeric(input$strip.occurrences) < 6) return(NULL)
    z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
    return(numericInput(inputId="reduction6", label=NULL, value=param$reduction[6], min=z$low, max=z$high, step=z$s.step))
  })
  
  
  output$spot.ref.rnd <- renderUI({
    spot.ref <- input$update.spot.ref
    res <- try({
      drv <- dbDriver("PostgreSQL")
      pg.con <- dbConnect(drv, dbname=pg$dbname, host=pg$host, port=pg$port, user=pg$user, password=pg$password)
      df <- dbReadTable(pg.con, "spt")
      dbDisconnect(pg.con)
      dbUnloadDriver(drv)
    })
    spot.ref <- 0
    if(class(res) == "try-error")
    {
      msg <- paste(format(Sys.time()), "ERROR in dbReadTable spt", paste(geterrmessage(), collapse=", "), sep=", ")
      rv$msg.log <- msg
      spot.ref <- NULL
    }
    if(!is.null(spot.ref) & nrow(df) < 1)
    {
      msg <- paste(format(Sys.time()), "ERROR nrow spt < 1", sep=", ")
      rv$msg.log <- msg
      spot.ref <- NULL
    } else {
      df <- df[df$ccy == g.ccy$cname[g.ccy$name == param$ccy], ]
    }
    if(!is.null(spot.ref) & nrow(df) != 1)
    {
      msg <- paste(format(Sys.time()), "ERROR nrow spt <> 1 for ccy =", param$ccy, sep=", ")
      rv$msg.log <- msg
      spot.ref <- NULL
    }
    if(!is.null(spot.ref))
    {
      spot.ref <- (df$bid + df$ask)/2
      if(!g.ccy$conv[g.ccy$name == param$ccy])
        spot.ref <- 1/spot.ref
      spot.ref <- round(spot.ref, g.ccy$s.digits[g.ccy$name == param$ccy])
    }
    z <- g.ccy[g.ccy$name == input$ccy, c("s.step", "low", "high")]
    return(numericInput(inputId="spot.ref", label=NULL, value=spot.ref, min=z$low, max=z$high, step=z$s.step))
  })
  
  
  output$download.scheme <- downloadHandler(
    filename = function() {
      isolate({
        file.name <- input$download.scheme.file.name
        ccy <- gsub("[[:punct:]]", "", input$ccy)
      })
      if(is.null(file.name) | is.na(file.name) | file.name == "")
        file.name <- paste(g.sp.name["short"], ccy, format(Sys.time(), "%Y-%m-%d %H%M%S"))
      file.name <- paste0(file.name, ".csv")
      return(file.name)
    },
    content = function(file) {
      isolate({
        n <- as.numeric(input$strip.occurrences)
        sch <- data.frame(n=1:n)
        sch$sp <- as.character(g.sp.name["short"])
        sch$ccy <- input$ccy
        sch$client.buys <- input$client.buys
        sch$notional.ccy <- input$notional.ccy
        sch$expiry <- NA
        sch$expiry[1] <- format(input$expiry1, "%Y-%m-%d")
        sch$notional <- NA
        sch$notional[1] <- input$notional1
        sch$ratio <- NA
        sch$ratio[1] <- input$ratio1
        sch$strike <- NA
        sch$strike[1] <- input$strike1
        sch$reduction <- NA
        sch$reduction[1] <- input$reduction1
        if(n >= 2)
        {
          sch$expiry[2] <- format(input$expiry2, "%Y-%m-%d")
          sch$notional[2] <- input$notional2
          sch$ratio[2] <- input$ratio2
          sch$strike[2] <- input$strike2
          sch$reduction[2] <- input$reduction2
        }
        if(n >= 3)
        {
          sch$expiry[3] <- format(input$expiry3, "%Y-%m-%d")
          sch$notional[3] <- input$notional3
          sch$ratio[3] <- input$ratio3
          sch$strike[3] <- input$strike3
          sch$reduction[3] <- input$reduction3
        }
        if(n >= 4)
        {
          sch$expiry[4] <- format(input$expiry4, "%Y-%m-%d")
          sch$notional[4] <- input$notional4
          sch$ratio[4] <- input$ratio4
          sch$strike[4] <- input$strike4
          sch$reduction[4] <- input$reduction4
        }
        if(n >= 5)
        {
          sch$expiry[5] <- format(input$expiry5, "%Y-%m-%d")
          sch$notional[5] <- input$notional5
          sch$ratio[5] <- input$ratio5
          sch$strike[5] <- input$strike5
          sch$reduction[5] <- input$reduction5
        }
        if(n >= 6)
        {
          sch$expiry[6] <- format(input$expiry6, "%Y-%m-%d")
          sch$notional[6] <- input$notional6
          sch$ratio[6] <- input$ratio6
          sch$strike[6] <- input$strike6
          sch$reduction[6] <- input$reduction6
        }
        sch$spot.ref[1] <- input$spot.ref
        sch$n <- NULL
      })
      res <- try(write.table(sch, file=file, sep=",", row.names=FALSE, col.names=TRUE))
    }
  )
  
  output$log.msg <- renderText({
    full.log <<- paste(rv$msg.log, full.log, sep="\n")
    full.log
  })

  
  
  
  
  
  output$strike.inv <- renderText(round(1/input$strike, g.ccy$s.digits[g.ccy$name == input$ccy]))
  output$init.strike.inv <- renderText(round(1/input$init.strike, g.ccy$b.digits[g.ccy$name == input$ccy]))
  
  

})
